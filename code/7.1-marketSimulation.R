# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load and run linear model:
source('./code/3.1-linear_model.R')

# -----------------------------------------------------------------------------
# Compute the market shares of a given market

# Create a set of alternatives for which to simulate shares:
# Columns are attributes: price, fuelEconomy, accelTime, and powertrain_elec
# Each row is an alternative
market = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Use the logitProbs() function to compute the market shares
# (see the './1-loadTools.R' file for details about 'logitProbs()'
shares = logitProbs(model_linear, market)
shares

# -----------------------------------------------------------------------------
# Compute the market shares with uncertainty

# Use the logitProbsUnc() function to compute the market shares with uncertainty
# (see the './1-loadTools.R' file for details about 'logitProbsUnc()'
sharesUnc = logitProbsUnc(model_linear, market, numDraws=10^4)
sharesUnc
