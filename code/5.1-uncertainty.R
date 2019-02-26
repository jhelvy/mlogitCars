# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load and run linear model:
source('./code/3.1-linear_model.R')

# -----------------------------------------------------------------------------
# Get the model coefficients:
coefs = coef(model_linear)
coefs

# Generate draws of coefficients:
coef_draws = getCoefDraws(model_linear, numDraws = 10^4)
head(coef_draws)

# For each coefficient, get the mean and 95% confidence interval:
coef = getCI(coef_draws)
coef