setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# Load and run linear model:
source('./3-linear.R')

# -----------------------------------------------------------------------------
# Compute the market shares of a given market

# Create a set of alternatives for which to simulate shares:
# Attributes are price, fuelEconomy, accelTime, and powertrain_elec
X = matrix(c(
    15,  20, 8, 0, # Each row is an alternative
    30, 100, 6, 1,
    21,  40, 7, 0), ncol=4, byrow=T)

# Get the model coefficients:
beta = coef(model_linear)
beta

# Compute market shares (this is just the logit fraction):
v_j     = X %*% beta
exp_v_j = exp(v_j)
denom   = sum(exp_v_j)
shares  = exp_v_j / denom

# -----------------------------------------------------------------------------
# Compute the market shares with uncertainty

# See 'simulateMarketShares' function in '1-loadTools.R' file for details
shares = simulateMarketShares(model_linear, X, numDraws=10^4)

# -----------------------------------------------------------------------------
# Plot results

shares$alt = seq(3)
ggplot(data=shares,
    aes(x=as.factor(alt), y=mean)) +
    geom_bar(stat='identity') +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
    scale_y_continuous(limits=c(0, 1)) +
    labs(x='Alternative', y='Market Share') +
    theme_bw()
