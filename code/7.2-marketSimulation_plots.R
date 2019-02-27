# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load the market simulation results:
source('./code/7.1-marketSimulation.R')

# -----------------------------------------------------------------------------

# Plot results 
shares = as.data.frame(shares) %>%
    mutate(alt = seq(n()))
baseline = ggplot(shares,
    aes(x=as.factor(alt), y=shares)) +
    geom_bar(stat='identity') +
    scale_y_continuous(limits=c(0, 1)) +
    labs(x='Alternative', y='Market Share') +
    theme_bw()

# Plot results with uncertainty (error bars)
sharesUnc$alt = seq(3)
baseline_unc = ggplot(sharesUnc,
    aes(x=as.factor(alt), y=mean)) +
    geom_bar(stat='identity') +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3) +
    scale_y_continuous(limits=c(0, 1)) +
    labs(x='Alternative', y='Market Share') +
    theme_bw()

# Save plots 
mainwd = getwd()
setwd('./results/plots/marketSims')
ggsave('./baseline.pdf', baseline, width=4, height=3)
ggsave('./baseline_unc.pdf', baseline_unc, width=4, height=3)
setwd(mainwd)