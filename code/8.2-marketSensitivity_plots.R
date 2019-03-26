# Load libraries and functions
source('./code/1.1-loadTools.R')

# Load the market simulation sensitivity analysis results:
source('./code/8.1-marketSensitivity.R')

# -----------------------------------------------------------------------------
# Make a line plot of the market sensitivity to price (with uncertainty)

share_price_plot = ggplot(cases_price_unc,
    aes(x=price, y=share_mean, ymin=share_low, ymax=share_high)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_x_continuous(breaks=seq(10, 20, 2)) +
    scale_y_continuous(limits=c(0, 1)) +
    labs(x='Price ($1,000)', y='Market Share') +
    theme_bw()

# -----------------------------------------------------------------------------
# Make a line plot of the revenue sensitivity to price (with uncertainty)

marketSize = 1000
rev_price_plot = cases_price_unc %>%
    mutate(
        rev_mean = price*marketSize*share_mean / 10^3, # Convert to millions
        rev_low  = price*marketSize*share_low / 10^3,
        rev_high = price*marketSize*share_high / 10^3) %>%
    ggplot(
    aes(x=price, y=rev_mean, ymin=rev_low, ymax=rev_high)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_x_continuous(breaks=seq(10, 20, 2)) +
    labs(x='Price ($ Thousand)', y='Revenue ($ Million)') +
    theme_bw()

# -----------------------------------------------------------------------------
# Make a tornado diagram to show market sensitivity to all attributes

# Get baseline simulation results 
# (defined in './8.1-marketSensitivity.R')
shares = logitProbs(model_linear, marketBaseline)
shareBaseline = shares[1]

# Format sensitivity case results for plotting
cases_all = cases_all %>%
    # "Center" the shares around the baseline result 
    mutate(share = share - shareBaseline) %>%
    # Rename variables for prettier plotting
    mutate(attribute = fct_recode(attribute,
        'Price ($1,000)'             = 'price',
        'Fuel Economy (mpg)'         = 'fuelEconomy',
        '0-60 mph Acceleration Time' = 'accelTime',
        'Electric Powertrain'        = 'powertrain_elec')) %>%
    # Compute the range in market share from low to high levels for sorting
    group_by(attribute) %>%
    mutate(shareRange = sum(abs(share)))

# Compute labels for market share
lowerBound = floor(10*min(cases_all$share))/10
upperBound = ceiling(10*max(cases_all$share))/10
breaks     = seq(lowerBound, upperBound, (upperBound - lowerBound) / 5)    
breakLabs  = round(breaks + shareBaseline, 2)

# Make the tornado diagram
tornado_plot = ggplot(cases_all,
    # Use 'fct_reorder' to order the variables according to shareRange
    aes(x=fct_reorder(attribute, shareRange), y=share, fill=level)) +
    geom_bar(stat='identity', width=0.6) +
    # Add labels at end of bars
    geom_text(aes(label=value), vjust=0.5) +
    # Set axis breaks
    scale_y_continuous(limits=c(lowerBound, upperBound), 
                       breaks=breaks, labels=breakLabs) +
    # Modify labels and theme
    labs(x='Attribute', y='Market Share') +
    theme_bw() +
    # Remove legend
    theme(legend.position='none') +
    coord_flip()

# Save plots 
mainwd = getwd()
setwd('./results/plots/marketSims')
ggsave('./sens_share_price.pdf', share_price_plot, width=4, height=3)
ggsave('./sens_rev_price.pdf', rev_price_plot, width=4, height=3)
ggsave('./sens_tornado.pdf', tornado_plot, width=6, height=3)
setwd(mainwd)
