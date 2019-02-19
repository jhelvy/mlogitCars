setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# Load and run linear model:
source('./3.1-linear_model.R')

# -----------------------------------------------------------------------------
# Get the model coefficients:
coefs = coef(model_linear)
coefs

# Generate draws of coefficients:
coef_draws = getCoefDraws(model_linear, numDraws = 10^4)
head(coef_draws)

# For each coefficient, get the mean and 95% confidence interval:
coef_price       = getCI(coef_draws$price)
coef_fuelEconomy = getCI(coef_draws$fuelEconomy)
coef_accelTime   = getCI(coef_draws$accelTime)
coef_powertrain  = getCI(coef_draws$powertrain)

# Combine results
coef = as.data.frame(rbind(
    coef_price, coef_fuelEconomy, coef_accelTime, coef_powertrain))
