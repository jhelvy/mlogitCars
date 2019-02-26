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

# Look at unique values in data:
data %>% distinct(price)
data %>% distinct(fuelEconomy)
data %>% distinct(accelTime)
data %>% distinct(powertrain)

# Count number of times each alternative was chosen:
data %>% count(choice, alt)

# Check if any respondents made the same choice for more than 7 questions:
data %>% 
    filter(choice == 1) %>%
    count(respID, alt) %>%
    filter(n > 7)

# Visualize how many times each price level was chosen:
data %>% count(price, choice)
ggplot(data, aes(x=price, y=choice)) +
    geom_bar(stat='identity')

# Visualize how many times electric cars were chosen compared to gasoline cars:
ggplot(data, aes(x=powertrain, y=choice)) +
    geom_bar(stat='identity')


