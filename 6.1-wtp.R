setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# Load and run linear model:
source('./3.1-linear_model.R')

# -----------------------------------------------------------------------------
# Compute the WTP implied from the preference space model

# Get the model coefficients:
coefs = coef(model_linear)
coefs

# Compute WTP:
wtp = -1 * (coefs / coefs['price'])
as.matrix(wtp)

# -----------------------------------------------------------------------------
# Compute the WTP with uncertainty

# Generate draws of coefficients:
coef_draws = getCoefDraws(model_linear, numDraws = 10^4)
head(coef_draws)

# Compute WTP for each coefficient draw:
numCols     = ncol(coef_draws)
price_draws = matrix(rep(coef_draws$price, numCols), ncol=numCols)
wtp_draws   = -1 * (coef_draws / price_draws)
head(wtp_draws)

# For each coefficient, get the mean and 95% confidence interval of WTP:
wtp_fuelEconomy = getCI(wtp_draws$fuelEconomy)
wtp_accelTime   = getCI(wtp_draws$accelTime)
wtp_powertrain  = getCI(wtp_draws$powertrain)

# Combine results
wtp = as.data.frame(rbind(wtp_fuelEconomy, wtp_accelTime, wtp_powertrain))
