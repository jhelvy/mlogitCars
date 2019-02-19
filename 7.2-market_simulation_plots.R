setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# Load the market simulation results:
source('./7.1-market_simulation.R')

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
