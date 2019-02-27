# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load and WTP results:
source('./code/6.1-wtp.R')

# -----------------------------------------------------------------------------
# Plot results (including uncertainty)

# Create data frames for plotting each attribute:
#   level   = The attribute level (x-axis)
#   utility = The utility associated with each level (y-axis)
wtp$par         = row.names(wtp)
wtp_fuelEconomy = wtp %>% filter(par == 'fuelEconomy')
wtp_accelTime   = wtp %>% filter(par == 'accelTime')
wtp_powertrain  = wtp %>% filter(par == 'powertrain_elec')
df_fuelEconomy = data %>%
    distinct(level = fuelEconomy) %>%
    arrange(level) %>%
    mutate(mean  = (level - min(level)) * wtp_fuelEconomy$mean,
           lower = (level - min(level)) * wtp_fuelEconomy$lower,
           upper = (level - min(level)) * wtp_fuelEconomy$upper)
df_accelTime = data %>%
    distinct(level = accelTime) %>%
    arrange(level) %>%
    mutate(mean  = (level - min(level)) * wtp_accelTime$mean,
           lower = (level - min(level)) * wtp_accelTime$lower,
           upper = (level - min(level)) * wtp_accelTime$upper)
df_powertrain = data %>%
    distinct(level = powertrain) %>%
    mutate(mean  = c(wtp_powertrain$mean, 0),
           lower = c(wtp_powertrain$lower, 0),
           upper = c(wtp_powertrain$upper, 0))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
ymin = min(c(df_fuelEconomy$lower, df_accelTime$lower, df_powertrain$lower))
ymax = max(c(df_fuelEconomy$upper, df_accelTime$upper, df_powertrain$upper))

# Plot the WTP for each attribute with error bars for a 95% confidence interval
plot_fuelEconomy = ggplot(df_fuelEconomy,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='WTP ($1,000)') +
    theme_bw()
plot_accelTime = ggplot(df_accelTime,
    aes(x=level, y=mean, ymin=lower, ymax=upper)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='WTP ($1,000)') +
    theme_bw()
plot_powertrain = ggplot(df_powertrain,
    aes(x=level, y=mean)) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain', y='WTP ($1,000)') +
    theme_bw()

# Plot all plots in one figure
multiplot = grid.arrange(plot_fuelEconomy, plot_accelTime, plot_powertrain, nrow=1)

# Save plots 
mainwd = getwd()
setwd('./results/plots/wtp')
ggsave('./wtp_fuelEconomy.pdf', plot_fuelEconomy, width=4, height=3)
ggsave('./wtp_accelTime.pdf', plot_accelTime, width=4, height=3)
ggsave('./wtp_powertrain.pdf', plot_powertrain, width=4, height=3)
ggsave('./wtp_multiplot.pdf', multiplot, width=8, height=2.5)
setwd(mainwd)
