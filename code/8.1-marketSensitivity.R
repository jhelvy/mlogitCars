# -----------------------------------------------------------------------------
# Sensitivity Analysis:

# Measure the sensitivity of the market share outcome for one product to
# the attribute levels of that product. In these examples, we will examine 
# the effect of changing values of the FIRST alternative in a market on its 
# market share.

# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load the market simulation results:
source('./code/3.1-linear_model.R')

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in price

# Define a baseline market:
marketBaseline = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Define the sensitivity cases:
cases_price = data.frame(
    price = seq(10, 20),
    share = NA)
cases_price

# For each case, define the new market and compute the market share of 
# the first alternative in the market
for (i in 1:nrow(cases_price)) {
    # Define the baseline market
    market = marketBaseline
    # Set the value of price for the first alternative to the case value
    market$price[1] = cases_price$price[i]
    # Compute the market shares using logitProbs()
    # (see the './1-loadTools.R' file for details about 'logitProbs()')
    share = logitProbs(model_linear, market)[1]
    cases_price$share[i] = share
}
cases_price

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in price with uncertainty

# Define the sensitivity cases:
cases_price_unc = data.frame(
    price      = seq(10, 20),
    share_mean = NA, 
    share_low  = NA, 
    share_high = NA)
cases_price_unc

# Run the simulation for each case
for (i in 1:nrow(cases_price_unc)) {
    market = marketBaseline
    market$price[1] = cases_price$price[i]
    shareDf = logitProbsUnc(model_linear, market, numDraws=10^4)
    cases_price_unc[i,2:4] = shareDf[1,]
}
cases_price_unc

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in all attributes

# Define cases for sensitivity analysis
# (I recommend doing this in a spreadsheet and importing it)
cases_all = read_csv('./data/sensitivityCases.csv') %>% 
    mutate(share = NA)
cases_all

# For each case, define the new market and compute the market share of 
# the first alternative in the market
for (i in 1:nrow(cases_all)) {
    market = marketBaseline
    market[cases_all$attribute[i]][1,] = cases_all$value[i]
    share = logitProbs(model_linear, market)[1]
    cases_all$share[i] = as.numeric(share)
}
cases_all
