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

# -----------------------------------------------------------------------------
# Plot results

# Get the model coefficients:
coefs = coef(model_partworth)
coefs

# Create data frames for plotting each attribute
# X is the attribute level
# Y is the utility associated with each level:
levels_price       = c(15, 20, 25)
levels_fuelEconomy = c(25, 30, 35)
levels_accelTime   = c(6, 7, 8)
levels_powertrain  = c(0, 1)
df_price = data.frame(
    levels  = levels_price,
    utility = c(0, coefs[c('price_20', 'price_25')]))
df_fuelEconomy = data.frame(
    levels  = levels_fuelEconomy,
    utility = c(0, coefs[c('fuelEconomy_25', 'fuelEconomy_30')]))
df_accelTime = data.frame(
    levels  = levels_accelTime,
    utility = c(0, coefs[c('accelTime_7', 'accelTime_8')]))
df_powertrain = data.frame(
    levels  = levels_powertrain,
    utility = c(0, coefs[c('powertrain_elec')]))

# Get y-axis upper and lower bounds (plots should have the same y-axis):
df   = rbind(df_price, df_fuelEconomy, df_accelTime, df_powertrain)
ymin = min(df$utility)
ymax = max(df$utility)

# Plot the utility for each attribute
plot_price = ggplot(data=df_price, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(data=df_fuelEconomy, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(data=df_accelTime, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='0-60mph Accel. Time (sec)', y='Utility') +
    theme_bw()
plot_powertrain = ggplot(data=df_powertrain, aes(x=levels, y=utility)) +
    geom_point() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Powertrain Electric', y='Utility') +
    theme_bw()

# Plot all plots in one figure
ggmultiplot(plot_price, plot_fuelEconomy, plot_accelTime, plot_powertrain,
    cols=4)

