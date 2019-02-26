# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load and run linear model:
source('./code/3.1-linear_model.R')

# -----------------------------------------------------------------------------
# Compute the WTP implied from the preference space model

# Get the model coefficients:
coefs = coef(model_linear)
coefs

# Compute WTP:
wtp = coefs / (-1*coefs['price'])
wtp

# -----------------------------------------------------------------------------
# Compute the WTP with uncertainty

# Generate draws of coefficients:
coef_draws = getCoefDraws(model_linear, numDraws = 10^4)
head(coef_draws)

# Compute WTP for each coefficient draw:
wtp_draws = -1*(coef_draws[,2:4] / coef_draws[,1])
head(wtp_draws)

# For each coefficient, get the mean and 95% confidence interval of WTP:
wtp = getCI(wtp_draws)
wtp