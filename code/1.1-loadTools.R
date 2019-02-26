# Install packages - only need to do this once!
# install.packages('tidyverse')
# install.packages('gridExtra')
# install.packages('MASS')
# install.packages('mlogit')

# Load installed libraries - do this every time!
library(MASS)
library(gridExtra)
library(tidyverse)
library(mlogit)

# -----------------------------------------------------------------------------
# Settings

# Set option to preview all columns in a data.frame or tibble:
options(dplyr.width = Inf)

# -----------------------------------------------------------------------------
# Functions for preparing the data

dummyCode = function(df, varNames) {
    df = as.data.frame(df)
    nonVarNames = colnames(df)[which(! colnames(df) %in% varNames)]
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
    # Restore the original column order
    new = colnames(df)[which(! colnames(df) %in% c(varNames, nonVarNames))]
    df = df[c(nonVarNames, varNames, new)]
    # Restore the original row order
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
getCI = function(df, alpha=0.025) {
    df = data.frame(
        mean  = apply(df, 2, mean, na.rm=T), 
        lower = apply(df, 2, function(x) {quantile(x, alpha, na.rm=T)}), 
        upper = apply(df, 2, function(x) {quantile(x, 1-alpha, na.rm=T)}))
    # df$par = row.names(df)
    # row.names(df) = seq(nrow(df))
    # return(df[c('par', 'mean', 'lower', 'upper')])
    return(df)
}
