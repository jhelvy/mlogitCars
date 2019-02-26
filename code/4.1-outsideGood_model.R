# Load libraries and functions
source('./code/1.1-loadTools.R')

# -----------------------------------------------------------------------------
# Load the data set:
data = read_csv('./data/data_outsideGood.csv')

# Variables:
# "respID"      = Identifies each unique survey respondent
# "obsID"       = Identifies each unique choice observation
# "alt"         = Identifies the alternative in each unique choice observation
# "choice"      = 1 if the alternative is chosen, 0 otherwise
# "price"       = Purchase price in thousands of dollars (15, 20, 25)
# "fuelEconomy" = Fuel economy in miles per gallon of gasoline (20, 25, 30)
# "accelTime"   = 0 to 60 mph acceleration time in seconds (6, 7, 8)
# "powertrain"  = Indicates if the car is electric or gas ("gas", "elec")
# "outsideGood" = Identifies the outside good option (0 or 1)

# Create dummy coded variables
data_dummy = dummyCode(data, varNames = 'powertrain')
head(data_dummy)

# -----------------------------------------------------------------------------
# Estimate MNL linear model:

# Convert the data to "mlogit" format:
data_mlogit = mlogit.data(
    data    = data_dummy,
    shape   = 'long',
    choice  = 'choice',
    alt.var = 'alt')

# Run the model:
model_outsideGood = mlogit(data_mlogit,
    formula = choice ~
    price +                # Linear
    fuelEconomy +          # Linear
    accelTime +            # Linear
    powertrain_elec +      # Levels: 0, 1
    outsideGood | 0)       # Levels: 0, 1

# View summary of results
# Check the 1st order condition: Is the gradient at the solution zero?
summary(model_outsideGood)

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_outsideGood$hessian)$values
