# Load libraries and functions
source('./1-loadTools.R')

# Load and run linear model:
source('./3.1-linear_model.R')

# -----------------------------------------------------------------------------
# Compute the market shares of a given market

# Get the model coefficients:
coefs = coef(model_linear)
coefs

# Create a set of alternatives for which to simulate shares:
# Columns are attributes: price, fuelEconomy, accelTime, and powertrain_elec
# Each row is an alternative
market = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Make a function to compute the logit fraction (market shares)
logitProbs = function(market, coefs) {
    v     = as.matrix(market) %*% coefs
    expV  = exp(v)
    denom = sum(expV)
    probs = expV / denom
    return(probs)
}

# Compute the market shares
shares = logitProbs(market, coefs)
shares

# -----------------------------------------------------------------------------
# Compute the market shares with uncertainty

# This function takes draws of the estimated model coefficients using the
# hessian at the solution. It then computes the market shares with each set of
# draws and returns a mean result with a lower and upper bound from a 95%
# confidence interval.
simulateMarketShares = function(model, market, numDraws, alpha=0.025) {
    # See './1-loadTools.R' file for details about the 'getCoefDraws()'
    # and 'getCI()' functions.
    coef_draws  = getCoefDraws(model, numDraws)
    v_draws     = as.matrix(market) %*% t(coef_draws)
    expV_draws  = exp(v_draws)
    share_draws = apply(expV_draws, 2, function(x) {x / sum(x)})
    result      = t(apply(share_draws, 1, function(x) {getCI(x, alpha)}))
    result      = as.data.frame(result)
    colnames(result) = c('mean', 'lower', 'upper')
    return(result)
}

# Compute the market shares with uncertainty
shares = simulateMarketShares(model_linear, market, numDraws=10^4)
shares
