# Load libraries and functions
source('./code/1.1-loadTools.R')

# -----------------------------------------------------------------------------
# In 6.1, a linear model is estimated in the "Preference Space", and then
# WTP is computed using the resulting coefficients. 
#
# An alternative approach is to directly estimate WTP by specifying the 
# utility model in the "WTP Space". To do so, use the "logitr" package:
# https://github.com/jhelvy/logitr

# INSTALLATION

# First, install the "devtools" package:
# install.packages('devtools')

# Install the "logitr" package from github:
# library('devtools')
# install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# -----------------------------------------------------------------------------
# Load the data set:
data = read_csv('./data/data_mnl.csv')
head(data)

# Variables:
# "respID"      = Identifies each unique survey respondent
# "obsID"       = Identifies each unique choice observation
# "alt"         = Identifies the alternative in each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrain"  = Indicates if the car is electric or gas ("gas", "elec")

# Create dummy coded variables
data_dummy = dummyCode(data, varNames = 'powertrain')
head(data_dummy)

# -----------------------------------------------------------------------------
# Use logitr to estimate a MNL linear model in the preference space

# Run a MNL model in the Preference Space:
mnl.pref = logitr(
    data       = data_dummy,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    parNames   = c('price', 'fuelEconomy', 'accelTime', 'powertrain_elec'))

# Print a summary of the results:
summary(mnl.pref)

# Get the coefficients from the model:
coef(mnl.pref)

# Compute the WTP implied from the preference space model
mnl.pref.wtp = wtp(mnl.pref, priceName='price')
mnl.pref.wtp

# -----------------------------------------------------------------------------
# Use logitr to estimate a MNL linear model in the WTP space

# Run a MNL model in the WTP Space using a multistart:
mnl.wtp = logitr(
    data       = data_dummy,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    parNames   = c('fuelEconomy', 'accelTime', 'powertrain_elec'),
    priceName  = 'price',
    modelSpace = 'wtp',
    options = list(
        # Since WTP space models are non-convex, run a multistart:
        numMultiStarts = 10,
        # Use the computed WTP from the preference space model as the starting
        # values for the first run:
        startVals = coef(mnl.pref),
        # Because the computed WTP from the preference space model has values
        # as large as -5, increase the boundaries of the random starting values:
        startParBounds = c(-5,5)))

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl.wtp)

# Get the coefficients from the model:
coef(mnl.wtp)

# Comparing the WTP and log-likelihood values between the equivalent models in
# the preference space and WTP space is a helpful check for whether you have
# reached a global solution in WTP space models, which have non-convex
# log-likelihoods functions. This can be done using the wtpCompare function:
wtpCompare(mnl.pref, mnl.wtp, priceName='price')
