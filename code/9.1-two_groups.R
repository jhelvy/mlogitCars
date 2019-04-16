# Load libraries and functions
source('./code/1.1-loadTools.R')

# -----------------------------------------------------------------------------
# Load the data set:
data = read_csv('./data/data_mnl_2groups.csv')
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
# "group"       = Indicates the respondent group ("A" or "B")

# Create dummy coded variables
data_dummy = dummyCode(data, varNames = c('powertrain', 'group'))
head(data_dummy)

# Create interactions of main effects with each group
data_dummy = data_dummy %>% 
    mutate(
        price_B           = price * group_B, 
        fuelEconomy_B     = fuelEconomy * group_B,
        accelTime_B       = accelTime * group_B, 
        powertrain_elec_B = powertrain_elec * group_B)

# -----------------------------------------------------------------------------
# Estimate MNL linear model:

# Convert the data to "mlogit" format:
data_mlogit = mlogit.data(
    data    = data_dummy,
    shape   = 'long',
    choice  = 'choice',
    alt.var = 'alt')

# Run a model separating out the effects for each group:
model = mlogit(data_mlogit,
    formula = choice ~
    price + price_B +              # Linear price
    fuelEconomy + fuelEconomy_B +  # Linear fuel economy
    accelTime + accelTime_B +      # Linear acceleration time
    powertrain_elec + powertrain_elec_B | 0)  # Partworth: 0, 1

# View summary of results
summary(model)

# -----------------------------------------------------------------------------
# Generate draws of the model coefficients for each group

draws   = getCoefDraws(model)
draws_A = draws %>% select(price, fuelEconomy, accelTime, powertrain_elec)
draws_B = draws %>% 
    mutate(
        price           = price + price_B, 
        fuelEconomy     = fuelEconomy + fuelEconomy_B, 
        accelTime       = accelTime + accelTime_B, 
        powertrain_elec = powertrain_elec + powertrain_elec_B) %>%
    select(price, fuelEconomy, accelTime, powertrain_elec)
        
# -----------------------------------------------------------------------------
# Compute WTP for each group 

wtp_A = draws_A / (-1* draws_A$price)
wtp_B = draws_B / (-1* draws_B$price)
getCI(wtp_A)
getCI(wtp_B)

# -----------------------------------------------------------------------------
# Compute the market shares of a given market for each group

# Define a market:
market = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Use the logitProbsDraws() function to compute the market shares 
# with uncertainty for each group
sharesUnc_A = logitProbsDraws(draws_A, market)
sharesUnc_B = logitProbsDraws(draws_B, market)
sharesUnc_A
sharesUnc_B

# -----------------------------------------------------------------------------
# Sensitivity of market share to changes in price with uncertainty
# for each group

# Define a baseline market:
marketBaseline = data.frame(
    price           = c(15, 30, 21),
    fuelEconomy     = c(20, 90, 40),
    accelTime       = c(8, 6, 7),
    powertrain_elec = c(0, 1, 0))

# Define the sensitivity cases:
cases_price_unc_A = data.frame(
    price      = seq(10, 20),
    share_mean = NA, 
    share_low  = NA, 
    share_high = NA)
cases_price_unc_B = cases_price_unc_A

# Run the simulation for each case
for (i in 1:nrow(cases_price_unc_A)) {
    market = marketBaseline
    market$price[1] = cases_price_unc_A$price[i]
    sharesUnc_A = logitProbsDraws(draws_A, market)
    sharesUnc_B = logitProbsDraws(draws_B, market)
    cases_price_unc_A[i,2:4] = sharesUnc_A[1,]
    cases_price_unc_B[i,2:4] = sharesUnc_B[1,]
}
cases_price_unc_A
cases_price_unc_B

# Plot price sensitivity results for each group 
cases_price_unc_A$group = 'A'
cases_price_unc_B$group = 'B'
cases_price_unc = bind_rows(cases_price_unc_A, cases_price_unc_B)
share_price_plot = ggplot(cases_price_unc,
    aes(x=price, y=share_mean, ymin=share_low, ymax=share_high)) +
    geom_line(aes(color = group)) +
    geom_ribbon(aes(fill=group), alpha=0.2) +
    scale_x_continuous(breaks=seq(10, 20, 2)) +
    scale_y_continuous(limits=c(0, 1)) +
    labs(x='Price ($1,000)', y='Market Share') +
    theme_bw()
share_price_plot