setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Install packages (only need to install once)
# install.packages('tidyverse')
# install.packages('gridExtra')
# install.packages('MASS')
# install.packages('mlogit')

# Load libraries
library(tidyverse)
library(gridExtra)
library(mlogit)
library(MASS)

# -----------------------------------------------------------------------------
# Settings

# Set option to preview all columns in a data.frame or tibble:
options(dplyr.width = Inf)

# -----------------------------------------------------------------------------
# Functions for preparing the data

dummyCode = function(df, varNames) {
    # Keep the original variables and the order to restore later after merging
    df$order = seq(nrow(df))
    for (i in 1:length(varNames)) {
        varName  = varNames[i]
        colIndex = which(colnames(df) == varName)
        levels   = sort(unique(df[,colIndex]))
        mergeMat = as.data.frame(diag(length(levels)))
        mergeMat = cbind(levels, mergeMat)
        colnames(mergeMat) = c(varName, paste(varName, levels, sep='_'))
        df = merge(df, mergeMat)
    }
    # Restore the original order
    df = df[order(df$order),]
    row.names(df) = df$order
    df$order <- NULL
    return(df)
}

# -----------------------------------------------------------------------------
# Functions for dealing with parameter uncertainty

# Returns multivariate normal draws of the coefficients of a model estimated
# using the mlogit package
getCoefDraws = function(model, numDraws) {
    varcov = abs(solve(as.matrix(model$hessian)))
    draws  = data.frame(mvrnorm(numDraws, model$coef, varcov))
    return(draws)
}

# Returns a confidence interval from a vector of values
getCI = function(values, alpha=0.025) {
    mean   = mean(values, na.rm=T)
    lower  = quantile(values, alpha, na.rm=T)
    upper  = quantile(values, 1-alpha, na.rm=T)
    result = c(mean=mean, lower=lower, upper=upper)
    return(result)
}

# Returns the expected market shares with uncertainty of a market (as a matrix)
# using multivariate normal draws of the coefficients of an estimated mlogit
# model
simulateMarketShares = function(model, market, numDraws, alpha=0.025) {
    coef_draws       = getCoefDraws(model, numDraws)
    v_j_draws        = market %*% t(coef_draws)
    exp_v_j_draws    = exp(v_j_draws)
    share_draws      = apply(exp_v_j_draws, 2, function(x) {x / sum(x)})
    result           = t(apply(share_draws, 1, function(x) {getCI(x, alpha)}))
    result           = as.data.frame(result)
    colnames(result) = c('mean', 'lower', 'upper')
    return(result)
}
