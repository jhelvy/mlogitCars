setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

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
# Attributes are price, fuelEconomy, accelTime, and powertrain_elec
X = matrix(c(
    15,  20, 8, 0, # Each row is an alternative
    30, 90, 6, 1,
    21,  40, 7, 0), ncol=4, byrow=T)

# Compute market shares (this is just the logit fraction):
v      = X %*% coefs
expV   = exp(v)
denom  = sum(expV)
shares = expV / denom
shares

# -----------------------------------------------------------------------------
# Compute the market shares with uncertainty

# The 'simulateMarketShares' function takes draws of the estimated model
# coefficients using the hessian at the solution. It then computes the market
# shares with each set of draws and returns a mean result with a lower and
# upper bound from a 95% confidence interval.
#
# See 'simulateMarketShares' function in '1-loadTools.R' file for details
shares = simulateMarketShares(model_linear, X, numDraws=10^4)
shares
