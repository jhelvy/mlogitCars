# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load and run linear model with outside good:
source('./code/4.1-outsideGood_model.R')

# -----------------------------------------------------------------------------
# Plot results

# Get the model coefficients:
coefs = coef(model_outsideGood)
coefs

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price = data %>%
    distinct(level = price) %>%
    arrange(level) %>%
    filter(level != 0) %>% 
    mutate(utility = (level - min(level)) * coefs['price'])
df_fuelEconomy = data %>%
    distinct(level = fuelEconomy) %>%
    arrange(level) %>%
    filter(level != 0) %>% 
    mutate(utility = (level - min(level)) * coefs['fuelEconomy'])
df_accelTime = data %>%
    distinct(level = accelTime) %>%
    arrange(level) %>%
    filter(level != 0) %>% 
    mutate(utility = (level - min(level)) * coefs['accelTime'])
df_powertrain = data %>%
    distinct(level = powertrain) %>%
    mutate(utility = c(coefs['powertrain_elec'], 0))
df_outsideGood = data %>%
    distinct(level = outsideGood) %>%
    mutate(utility = c(0, coefs['outsideGood']))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
utility = c(df_price$utility, df_fuelEconomy$utility, df_accelTime$utility, 
            df_powertrain$utility, df_outsideGood$utility) 
ymin = min(utility)
ymax = max(utility)

# Plot the utility for each attribute
plot_price = ggplot(df_price, 
    aes(x=level, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(df_fuelEconomy, 
    aes(x=level, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(df_accelTime, 
    aes(x=level, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(df_powertrain, 
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='Utility') +
    theme_bw()
plot_outsideGood = ggplot(df_outsideGood,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Outside Good', y='Utility') +
    theme_bw()

# Plot all plots in one figure
grid.arrange(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
    plot_outsideGood, nrow=1)
