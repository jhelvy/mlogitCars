setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# Load the market simulation sensitivity analysis results:
source('./7.3-marketSimulation_sensitivity.R')

# -----------------------------------------------------------------------------
# Setup data for creating a tornado diagram

cases = cases %>%
    # "Center" the shares around the baseline result (so baseline is at 0)
    mutate(share = share - baselineShare) %>%
    # Rename variables for prettier plotting
    mutate(var = fct_recode(var,
        'Price ($1,000)'             = 'price',
        'Fuel Economy (mpg)'         = 'fuelEconomy',
        '0-60 mph Acceleration Time' = 'accelTime',
        'Electric Powertrain'        = 'powertrain_elec')) %>%
    # Compute the range in change from low to high levels for sorting
    group_by(var) %>%
    mutate(shareRange = sum(abs(share)))

# Compute labels for market share
lb        = floor(10*min(cases$share))/10
ub        = ceiling(10*max(cases$share))/10
breaks    = seq(lb, ub, (ub - lb) / 5)
breakLabs = round(breaks + baselineShare, 2)

# Make the tornado diagram
ggplot(cases,
    # Use 'fct_reorder' to order the variables according to shareRange
    aes(x=fct_reorder(var, shareRange), y=share, fill=level)) +
    geom_bar(stat='identity', width=0.6) +
    # Add labels on bars
    geom_text(aes(label=val), vjust=0.5) +
    scale_y_continuous(limits=c(lb, ub), breaks=breaks, labels=breakLabs) +
    labs(x='Attribute',
         y='Market Share') +
    theme_bw() +
    # Remove legend
    theme(legend.position='none') +
    coord_flip()
