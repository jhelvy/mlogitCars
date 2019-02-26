# Load libraries and functions
source('./code/1.1-loadTools.R')

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
data_dummy = dummyCode(data,
    varNames = c('price', 'fuelEconomy', 'accelTime', 'powertrain'))
head(data_dummy)

# -----------------------------------------------------------------------------
# Estimate MNL partworth model:

# Convert the data to "mlogit" format:
data_mlogit = mlogit.data(
    data    = data_dummy,
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
