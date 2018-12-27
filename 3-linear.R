setwd('/Users/jhelvy/sync/00_current/EMSE 6035 - 2019 Spring/R/cars')

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
data_coded = dummyCode(data, varNames = c('powertrain'))
head(data_coded)

# -----------------------------------------------------------------------------
# Estimate MNL linear model:

# Convert the data to "mlogit" format:
data_mlogit = mlogit.data(
    data    = data_coded,
    shape   = 'long',
    choice  = 'choice',
    alt.var = 'alt')

# Run the model:
model_linear = mlogit(data_mlogit,
    formula = choice ~
    price +                # Linear
    fuelEconomy +          # Linear
    accelTime +            # Linear
    powertrain_elec | 0)   # Levels: 0, 1

# View summary of results
# Check the 1st order condition: Is the gradient at the solution zero?
summary(model_linear)

# 2nd order condition: Is the hessian negative definite?
# (If all the eigenvalues are negative, the hessian is negative definite)
eigen(model_linear$hessian)$values

# -----------------------------------------------------------------------------
# Plot results

# Get the model coefficients:
coefs = coef(model_linear)
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
    utility = (levels_price - 15) * coefs['price'])
df_fuelEconomy = data.frame(
    levels  = levels_fuelEconomy,
    utility = (levels_fuelEconomy - 25) * coefs['fuelEconomy'])
df_accelTime = data.frame(
    levels  = levels_accelTime,
    utility = (levels_accelTime - 6) * coefs['accelTime'])
df_powertrain = data.frame(
    levels  = levels_powertrain,
    utility = (levels_powertrain - 0) * coefs['powertrain_elec'])

# Get y-axis upper and lower bounds (plots should have the same y-axis):
df   = rbind(df_price, df_fuelEconomy, df_accelTime, df_powertrain)
ymin = min(df$utility)
ymax = max(df$utility)

# Plot the utility for each attribute
plot_price = ggplot(data=df_price, aes(x=levels, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Price ($1000)', y='Utility') +
    theme_bw()
plot_fuelEconomy = ggplot(data=df_fuelEconomy, aes(x=levels, y=utility)) +
    geom_line() +
    scale_y_continuous(limits=c(ymin, ymax)) +
    labs(x='Fuel Economy (mpg)', y='Utility') +
    theme_bw()
plot_accelTime = ggplot(data=df_accelTime, aes(x=levels, y=utility)) +
    geom_line() +
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

