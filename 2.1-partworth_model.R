setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# Load libraries and functions
source('./1-loadTools.R')

# -----------------------------------------------------------------------------
# Load the data set:
data = read.csv('./data/data_mnl.csv')
head(data)

# Variables:
# "obsID"       = Identifies each unique choice observation
# "alt"         = The alternative, denoted by 1 or 2
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in mpg (20, 25, 30)
# "accelTime"   = Zero to 60mph acceleration time in second (6, 7, 8)
# "powertrain"  = Indicates if the car is electric vs. gas ("gas", "elec")

# Create dummy coded variables
data_coded = dummyCode(data,
    varNames = c('price', 'fuelEconomy', 'accelTime', 'powertrain'))
head(data_coded)

# -----------------------------------------------------------------------------
# Estimate MNL partworth model:

# Convert the data to "mlogit" format:
data_mlogit = mlogit.data(
    data    = data_coded,
    shape   = 'long',
    choice  = 'choice',
    alt.var = 'alt')

# Run the model:
model_partworth = mlogit(data_mlogit,
    formula = choice ~                    # Remember: first level "dummied-out"
    price_20 + price_25 +                 # Levels: 15, 20, 25
    fuelEconomy_25 + fuelEconomy_30 +     # Levels: 20, 25, 30
    accelTime_7 + accelTime_8 +           # Levels: 6, 7, 8
    powertrain_elec | 0)                  # Levels: gas, elec

# View summary of results
# Check the 1st order condition: Is the gradient at the solution zero?
summary(model_partworth)

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_partworth$hessian)$values
