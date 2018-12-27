setwd('/Users/jhelvy/sync/00_current/EMSE 6035 - 2019 Spring/R/cars')

# Load libraries and functions
source('./1-loadTools.R')

# Load and run linear model:
source('./3-linear.R')

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
coef_draws = getUncertaintyDraws(model_linear, numDraws = 10^4)
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

# -----------------------------------------------------------------------------
# Plot results

# Create data frames for plotting each attribute
# X is the attribute level
# Y is the utility associated with each level:
levels_fuelEconomy = c(25, 30, 35)
levels_accelTime   = c(6, 7, 8)
levels_powertrain  = c(0, 1)
df_fuelEconomy = data.frame(
    levels = levels_fuelEconomy,
    mean   = (levels_fuelEconomy - 25) * wtp_fuelEconomy[1],
    lower  = (levels_fuelEconomy - 25) * wtp_fuelEconomy[2],
    upper  = (levels_fuelEconomy - 25) * wtp_fuelEconomy[3])
df_accelTime = data.frame(
    levels = levels_accelTime,
    mean   = (levels_accelTime - 6) * wtp_accelTime[1],
    lower  = (levels_accelTime - 6) * wtp_accelTime[2],
    upper  = (levels_accelTime - 6) * wtp_accelTime[3])
df_powertrain = data.frame(
    levels = levels_powertrain,
    mean   = (levels_powertrain - 0) * wtp_powertrain[1],
    lower  = (levels_powertrain - 0) * wtp_powertrain[2],
    upper  = (levels_powertrain - 0) * wtp_powertrain[3])

# Get y-axis upper and lower bounds (plots should have the same y-axis):
df   = rbind(df_fuelEconomy, df_accelTime, df_powertrain)
ymin = min(df$lower)
ymax = max(df$upper)

# Plot the WTP for each attribute with error bars for a 95% confidence interval
plot_fuelEconomy = ggplot(data=df_fuelEconomy,
    aes(x=levels, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='WTP ($1,000)') +
    theme_bw()
plot_accelTime = ggplot(data=df_accelTime,
    aes(x=levels, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='WTP ($1,000)') +
    theme_bw()
plot_powertrain = ggplot(data=df_powertrain,
    aes(x=levels, y=mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='WTP ($1,000)') +
    theme_bw()

# Plot all plots in one figure
ggmultiplot(plot_fuelEconomy, plot_accelTime, plot_powertrain, cols=3)

