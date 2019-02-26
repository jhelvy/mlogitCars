# Load libraries and functions
source('./1-loadTools.R')

# Load and run linear model:
source('./5.1-uncertainty.R')

# -----------------------------------------------------------------------------
# Plot results

# Create data frames for plotting each attribute
# X is the attribute level
# Y is the utility associated with each level:
levels_price       = c(15, 20, 25)
levels_fuelEconomy = c(25, 30, 35)
levels_accelTime   = c(6, 7, 8)
levels_powertrain  = c(0, 1)
df_price = data.frame(
    levels = levels_price,
    mean   = (levels_price - 15) * coef_price[1],
    lower  = (levels_price - 15) * coef_price[2],
    upper  = (levels_price - 15) * coef_price[3])
df_fuelEconomy = data.frame(
    levels = levels_fuelEconomy,
    mean   = (levels_fuelEconomy - 25) * coef_fuelEconomy[1],
    lower  = (levels_fuelEconomy - 25) * coef_fuelEconomy[2],
    upper  = (levels_fuelEconomy - 25) * coef_fuelEconomy[3])
df_accelTime = data.frame(
    levels = levels_accelTime,
    mean   = (levels_accelTime - 6) * coef_accelTime[1],
    lower  = (levels_accelTime - 6) * coef_accelTime[2],
    upper  = (levels_accelTime - 6) * coef_accelTime[3])
df_powertrain = data.frame(
    levels = levels_powertrain,
    mean   = (levels_powertrain - 0) * coef_powertrain[1],
    lower  = (levels_powertrain - 0) * coef_powertrain[2],
    upper  = (levels_powertrain - 0) * coef_powertrain[3])

# Get y-axis upper and lower bounds (plots should have the same y-axis):
df   = rbind(df_price, df_fuelEconomy, df_accelTime, df_powertrain)
ymin = min(df$lower)
ymax = max(df$upper)

# Plot utility for each attribute with error bars for a 95% confidence interval
plot_price = ggplot(data=df_price,
    aes(x=levels, y=mean, ymin=lower, ymax=upper)) +
    geom_line() + # Adds the trend line
    geom_ribbon(alpha=0.2) + # Adds the uncertainty band
    # Set alpha between 0 and 1 for transparency level
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(data=df_fuelEconomy,
    aes(x=levels, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(data=df_accelTime,
    aes(x=levels, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(data=df_powertrain, aes(x=levels, y=mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='Utility') +
    theme_bw()

# Plot all plots in one figure
grid.arrange(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
    nrow=1)

