setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# Load and run the partworth model:
source('./2.1-partworth_model.R')

# -----------------------------------------------------------------------------
# Plot results

# Get the model coefficients:
coefs = coef(model_partworth)
coefs

# Create data frames for plotting each attribute
# X is the attribute level
# Y is the utility associated with each level:
levels_price       = c(15, 20, 25)
levels_fuelEconomy = c(25, 30, 35)
levels_accelTime   = c(6, 7, 8)
levels_powertrain  = c(0, 1)
df_price = data.frame(
    levels  = levels_price,
    utility = c(0, coefs[c('price_20', 'price_25')]))
df_fuelEconomy = data.frame(
    levels  = levels_fuelEconomy,
    utility = c(0, coefs[c('fuelEconomy_25', 'fuelEconomy_30')]))
df_accelTime = data.frame(
    levels  = levels_accelTime,
    utility = c(0, coefs[c('accelTime_7', 'accelTime_8')]))
df_powertrain = data.frame(
    levels  = levels_powertrain,
    utility = c(0, coefs[c('powertrain_elec')]))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
df   = rbind(df_price, df_fuelEconomy, df_accelTime, df_powertrain)
ymin = min(df$utility)
ymax = max(df$utility)

# Plot the utility for each attribute
plot_price = ggplot(data=df_price, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(data=df_fuelEconomy, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(data=df_accelTime, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(data=df_powertrain, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain Electric', y='Utility') +
    theme_bw()

# Plot all plots in one figure
ggmultiplot(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
    cols=4)
