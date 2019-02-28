# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load and run the partworth model:
source('./code/2.1-partworth_model.R')

# -----------------------------------------------------------------------------
# Plot results

# Get the model coefficients:
coefs = coef(model_partworth)
coefs

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
df_price = data %>%
    distinct(level = price) %>%
    arrange(level) %>%
    mutate(utility = c(0, coefs[c('price_20', 'price_25')]))
df_fuelEconomy = data %>%
    distinct(level = fuelEconomy) %>%
    arrange(level) %>%
    mutate(utility = c(0, coefs[c('fuelEconomy_25', 'fuelEconomy_30')]))
df_accelTime = data %>%
    distinct(level = accelTime) %>%
    arrange(level) %>%
    mutate(utility = c(0, coefs[c('accelTime_7', 'accelTime_8')]))
df_powertrain = data %>%
    distinct(level = powertrain) %>%
    arrange(level) %>%
    mutate(utility = c(coefs[c('powertrain_elec')], 0))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
utility = c(df_price$utility, df_fuelEconomy$utility, df_accelTime$utility,
            df_powertrain$utility)
ymin = min(utility)
ymax = max(utility)

# Plot the utility for each attribute
plot_price = ggplot(df_price,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(df_fuelEconomy,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(df_accelTime,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(df_powertrain,
    aes(x=level, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='Utility') +
    theme_bw()

# Plot all plots in one figure
multiplot = grid.arrange(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain, nrow=1)

# Save plots
mainwd = getwd()
setwd('./results/plots/coefficients')
ggsave('./partworth_price.pdf', plot_price, width=4, height=3)
ggsave('./partworth_fuelEconomy.pdf', plot_fuelEconomy, width=4, height=3)
ggsave('./partworth_accelTime.pdf', plot_accelTime, width=4, height=3)
ggsave('./partworth_powertrain.pdf', plot_powertrain, width=4, height=3)
ggsave('./partworth_multiplot.pdf', multiplot, width=10, height=2.5)
setwd(mainwd)
