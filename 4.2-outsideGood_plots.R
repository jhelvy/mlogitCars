setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# Load and run linear model with outside good:
source('./4.1-outsideGood_model.R')

# -----------------------------------------------------------------------------
# Plot results

# Get the model coefficients:
coefs = coef(model_outsideGood)
coefs

# Create data frames for plotting each attribute
# X is the attribute level
# Y is the utility associated with each level:
levels_price       = c(15, 20, 25)
levels_fuelEconomy = c(25, 30, 35)
levels_accelTime   = c(6, 7, 8)
levels_powertrain  = c(0, 1)
levels_outsideGood = c(0, 1)
df_price = data.frame(
    levels  = levels_price,
    utility = (levels_price - 15) * coefs['price'])
df_fuelEconomy = data.frame(
    levels  = levels_fuelEconomy,
    utility = (levels_fuelEconomy - 25) * coefs['fuelEconomy'])
df_accelTime = data.frame(
    levels  = levels_accelTime,
    utility = (levels_accelTime - 6) * coefs['accelTime'])
df_powertrain = data.frame(
    levels  = levels_powertrain,
    utility = (levels_powertrain - 0) * coefs['powertrain_elec'])
df_outsideGood = data.frame(
    levels  = levels_outsideGood,
    utility = (levels_outsideGood - 0) * coefs['outsideGood'])

# Get y-axis upper and lower bounds (plots should have the same y-axis):
df   = rbind(df_price, df_fuelEconomy, df_accelTime, df_powertrain,
    df_outsideGood)
ymin = min(df$utility)
ymax = max(df$utility)

# Plot the utility for each attribute
plot_price = ggplot(data=df_price, aes(x=levels, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(data=df_fuelEconomy, aes(x=levels, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(data=df_accelTime, aes(x=levels, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(data=df_powertrain, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='Utility') +
    theme_bw()
plot_outsideGood = ggplot(data=df_outsideGood, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Outside Good', y='Utility') +
    theme_bw()

# Plot all plots in one figure
ggmultiplot(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
    plot_outsideGood, cols=5)

