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
share_price_plot

# -----------------------------------------------------------------------------
# Make a line plot of the revenue sensitivity to price (with uncertainty)

marketSize = 10^4
rev_price_plot = cases_price_unc %>%
    mutate(
        rev_mean = price*marketSize*share_mean,
        rev_low  = price*marketSize*share_low,
        rev_high = price*marketSize*share_high) %>%
    ggplot(
    aes(x=price, y=rev_mean, ymin=rev_low, ymax=rev_high)) +
    geom_line() +
    geom_ribbon(alpha=0.2) +
    scale_x_continuous(breaks=seq(10, 20, 2)) +
    labs(x='Price ($1,000)', y='Market Share') +
    theme_bw()
rev_price_plot

# -----------------------------------------------------------------------------
# Make a tornado diagram to show market sensitivity to all attributes

# Get baseline simulation results 
# (defined in './8.1-marketSensitivity.R')
shares = logitProbs(marketBaseline, coefs)
shareBaseline = shares[1]

cases_all = cases_all %>%
    # "Center" the shares around the baseline result (so baseline is at 0)
    mutate(share = share - shareBaseline) %>%
    # Rename variables for prettier plotting
    mutate(var = fct_recode(attribute,
        'Price ($1,000)'             = 'price',
        'Fuel Economy (mpg)'         = 'fuelEconomy',
        '0-60 mph Acceleration Time' = 'accelTime',
        'Electric Powertrain'        = 'powertrain_elec')) %>%
    # Compute the range in change from low to high levels for sorting
    group_by(attribute) %>%
    mutate(shareRange = sum(abs(share)))

# Compute labels for market share
lb        = floor(10*min(cases_all$share))/10
ub        = ceiling(10*max(cases_all$share))/10
breaks    = seq(lb, ub, (ub - lb) / 5)
breakLabs = round(breaks + shareBaseline, 2)

# Make the tornado diagram
tornado_plot = ggplot(cases_all,
    # Use 'fct_reorder' to order the variables according to shareRange
    aes(x=fct_reorder(attribute, shareRange), y=share, fill=level)) +
    geom_bar(stat='identity', width=0.6) +
    # Add labels on bars
    geom_text(aes(label=value), vjust=0.5) +
    scale_y_continuous(limits=c(lb, ub), breaks=breaks, labels=breakLabs) +
    labs(x='Attribute',
         y='Market Share') +
    theme_bw() +
    # Remove legend
    theme(legend.position='none') +
    coord_flip()
tornado_plot