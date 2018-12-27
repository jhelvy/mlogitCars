setwd('/Users/jhelvy/Documents/GitHub/mlogitCars')

# -----------------------------------------------------------------------------
# Functions for simulating SP data

simulateExperiment = function(atts, numObs, numAlts) {
    # Generate the full factorial design
    fullFactorial = expand.grid(atts)
    numIDs        = nrow(fullFactorial)
    # Sample from full factorial design without replacement to avoid
    # repeating the same choice alternative in any one choice set
    ids  = replicate(numObs, sample(seq(numIDs), numAlts, replace=F))
    data = fullFactorial[as.vector(ids), ]
    data = addMetaData(data, numAlts, numObs)
    return(data)
}

addOutsideGood = function(data, numAlts, numObs) {
    data_outsideGood = 0*data[1:numObs,]
    data_outsideGood$outsideGood = 1
    data$outsideGood = 0
    data_outsideGood$order = seq(numAlts, (nrow(data) + (numAlts-1)), numAlts)
    data$order = seq(1, nrow(data), 1)
    data       = rbind(data, data_outsideGood)
    data       = data[order(data$order),]
    numAlts    = numAlts + 1
    data       = addMetaData(data, numAlts, numObs)
    data$order <- NULL
    return(data)
}

addMetaData = function(data, numAlts, numObs) {
    varNames = colnames(data)
    varNames = varNames[which(! varNames %in% c('alt', 'obsID'))]
    data$alt   = rep(seq(1, numAlts), numObs)
    data$obsID = rep(seq(1, numObs), each=numAlts)
    row.names(data) = seq(nrow(data))
    data = data[c('obsID', 'alt', varNames)]
    return(data)
}

simulateChoices = function(data, pars) {
    obsID  = data$obsID
    X      = as.matrix(data[names(pars)])
    V      = X %*% pars
    shares = getMnlLogit(V, obsID)
    choice = tapply(shares, obsID, function(x) rmultinom(n=1, size=1, prob=x))
    return(do.call(rbind, choice))
}

# Returns the logit fraction for mnl (homogeneous) models
getMnlLogit = function(V, obsID) {
    expV       = exp(V)
    sumExpV    = rowsum(expV, group=obsID)
    repTimes   = as.numeric(table(obsID))
    sumExpVMat = matrix(rep(sumExpV, times=repTimes), ncol=1)
    logit      = expV / sumExpVMat
    return(logit)
}

# -----------------------------------------------------------------------------
# Simulate data

# Simulate experiments
numObs  = 3000
numAlts = 3
data_mnl = simulateExperiment(
    atts = list(
        price       = c(15, 20, 25),
        fuelEconomy = c(20, 25, 30),
        accelTime   = c(6, 7, 8),
        elec        = c(0, 1)),
    numObs  = numObs,
    numAlts = numAlts)
data_outsideGood = addOutsideGood(data_mnl, numAlts, numObs)

# Simulate choices
pars_mnl = c(
    price       = -0.7,
    fuelEconomy = 0.1,
    accelTime   = -0.1,
    elec        = -4.0)
pars_outsideGood        = c(pars_mnl, outsideGood = -3.0)
data_mnl$choice         = simulateChoices(data_mnl, pars_mnl)
data_outsideGood$choice = simulateChoices(data_outsideGood, pars_outsideGood)

# Format powertrain variable and save
data_mnl$powertrain         = ifelse(data_mnl$elec == 1, 'elec', 'gas')
data_outsideGood$powertrain = ifelse(data_outsideGood$elec == 1,
    'elec', 'gas')
varNames = c('obsID', 'alt', 'choice', 'price', 'fuelEconomy', 'accelTime',
    'powertrain')
data_mnl         = data_mnl[varNames]
data_outsideGood = data_outsideGood[c(varNames, 'outsideGood')]
write.csv(data_mnl, './data/data_mnl.csv', row.names=F)
write.csv(data_outsideGood, './data/data_outsideGood.csv', row.names=F)




# # -----------------------------------------------------------------------------
# # Preview predicted shares based on data and parameters:
# source('./1-loadTools.R')

# # MNL data:
# data       = read.csv('./data/data_mnl.csv', header=T)
# data       = dummyCode(data, varNames = 'powertrain')
# data$elec  = data$powertrain_elec
# pars       = pars_mnl
# pars_mnl = c(
#     price       = -0.7,
#     fuelEconomy = 0.1,
#     accelTime   = -0.1,
#     elec        = -4.0)
# X          = as.matrix(data[names(pars)])
# data$V     = as.numeric(X %*% pars)
# data$share = as.numeric(round(100*getMnlLogit(data$V, data$obsID), 2))
# head(data, 20)

# data %>% group_by(price) %>% summarise(mean(share))
# data %>% group_by(fuelEconomy) %>% summarise(mean(share))
# data %>% group_by(accelTime) %>% summarise(mean(share))
# data %>% group_by(elec) %>% summarise(mean(share))

# # Outside good data:
# source('./1-loadTools.R')
# data       = read.csv('./data/data_outsideGood.csv', header=T)
# data       = dummyCode(data, varNames = 'powertrain')
# data$elec  = data$powertrain_elec
# pars       = pars_outsideGood
# X          = as.matrix(data[names(pars)])
# data$V     = as.numeric(X %*% pars)
# data$share = as.numeric(round(100*getMnlLogit(data$V, data$obsID), 2))
# head(data, 20)

# data %>% group_by(price) %>% summarise(mean(share))
# data %>% group_by(fuelEconomy) %>% summarise(mean(share))
# data %>% group_by(accelTime) %>% summarise(mean(share))
# data %>% group_by(elec) %>% summarise(mean(share))
# data %>% group_by(outsideGood) %>% summarise(mean(share))

