# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load the market simulation results:
source('./code/7.1-marketSimulation.R')

# -----------------------------------------------------------------------------
# Measure the sensitivity of the market share outcome for one product to
# the attribute levels of the product. For this case, we will vary the values
# of the four main attributes (price, fuelEconomy, accelTime, and
# powertrain_elec) for the first product alternative in the market, X.

# Define a baseline market:
market = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Get baseline simulation results (see './7.1-marketSimulation.R')
shares = logitProbs(market, coefs)
shares
baselineShare = shares[1]

# Define cases for sensitivity analysis
# (I recommend doing this in a spreadsheet and importing it)
cases = read_csv('./data/sensitivityCases.csv')
# Add a variable to store the market share for each case
cases$share = NA
cases

# Define a function to compute the market shares for each case
# Inputs:
# coefs  = The estimated coefficients from a model
# market = The baseline market
# alt    = The row number of the alternative that will be changes
# var    = The variable (column) name of the value to be changed
# val    = The new value to replace the old one
runCase = function(coefs, market, alt, var, val) {
    market[alt,][var] = val
    shares = logitProbs(market, coefs)
    return(shares[alt])
}

# For each case, run the simulation and store the resulting market share
for (i in 1:nrow(cases)) {
    cases$share[i] = runCase(coefs, market, 1, cases$var[i], cases$val[i])
}
cases
