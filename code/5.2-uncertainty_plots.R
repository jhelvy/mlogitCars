# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load and run linear model:
source('./code/5.1-uncertainty.R')

# -----------------------------------------------------------------------------
# Plot results

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
coef$par         = row.names(coef)
coef_price       = coef %>% filter(par == 'price')
coef_fuelEconomy = coef %>% filter(par == 'fuelEconomy')
coef_accelTime   = coef %>% filter(par == 'accelTime')
coef_powertrain  = coef %>% filter(par == 'powertrain_elec')
df_price = data %>%
    distinct(level = price) %>%
    arrange(level) %>%
    mutate(mean  = (level - min(level)) * coef_price$mean,
           lower = (level - min(level)) * coef_price$lower,
           upper = (level - min(level)) * coef_price$upper)
df_fuelEconomy = data %>%
    distinct(level = fuelEconomy) %>%
    arrange(level) %>%
    mutate(mean  = (level - min(level)) * coef_fuelEconomy$mean,
           lower = (level - min(level)) * coef_fuelEconomy$lower,
           upper = (level - min(level)) * coef_fuelEconomy$upper)
df_accelTime = data %>%
    distinct(level = accelTime) %>%
    arrange(level) %>%
    mutate(mean  = (level - min(level)) * coef_accelTime$mean,
           lower = (level - min(level)) * coef_accelTime$lower,
           upper = (level - min(level)) * coef_accelTime$upper)
df_powertrain = data %>%
    distinct(level = powertrain) %>%
    mutate(mean  = c(coef_powertrain$mean, 0),
           lower = c(coef_powertrain$lower, 0),
           upper = c(coef_powertrain$upper, 0))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
ymin = min(c(df_price$lower, df_fuelEconomy$lower, df_accelTime$lower, 
             df_powertrain$lower))
ymax = max(c(df_price$upper, df_fuelEconomy$upper, df_accelTime$upper, 
             df_powertrain$upper))

# Plot utility for each attribute with error bars for a 95% confidence interval
plot_price = ggplot(df_price,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() + # Adds the trend line
    geom_ribbon(alpha=0.2) + # Adds the uncertainty band
    # Set alpha between 0 and 1 for transparency level
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(df_fuelEconomy,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(df_accelTime,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(df_powertrain, 
    aes(x=level, y=mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='Utility') +
    theme_bw()

# Plot all plots in one figure
multiplot = grid.arrange(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain, nrow=1)

# Save plots 
mainwd = getwd()
setwd('./results/plots/coefficients')
ggsave('./uncertainty_price.pdf', plot_price, width=4, height=3)
ggsave('./uncertainty_fuelEconomy.pdf', plot_fuelEconomy, width=4, height=3)
ggsave('./uncertainty_accelTime.pdf', plot_accelTime, width=4, height=3)
ggsave('./uncertainty_powertrain.pdf', plot_powertrain, width=4, height=3)
ggsave('./uncertainty_multiplot.pdf', multiplot, width=10, height=2.5)
setwd(mainwd)
