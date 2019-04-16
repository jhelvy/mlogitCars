# -----------------------------------------------------------------------------
# Functions for simulating SP data

simulateExperiment = function(atts, numObs, numResp, numAlts) {
    # Generate the full factorial design
    fullFactorial = expand.grid(atts)
    numIDs        = nrow(fullFactorial)
    # Sample from full factorial design without replacement to avoid
    # repeating the same choice alternative in any one choice set
    ids  = replicate(numObs, sample(seq(numIDs), numAlts, replace=F))
    data = fullFactorial[as.vector(ids), ]
    data = addMetaData(data, numObs, numResp, numAlts)
    return(data)
}

addMetaData = function(data, numObs, numResp, numAlts) {
    varNames    = colnames(data)
    varNames    = varNames[which(! varNames %in% c('respID', 'obsID', 'alt'))]
    numQ        = numObs / numResp
    data$respID = rep(seq(1, numResp), each=numQ*numAlts)
    data$obsID  = rep(seq(1, numObs), each=numAlts)
    data$alt    = rep(seq(1, numAlts), numObs)
    row.names(data) = seq(nrow(data))
    data = data[c('respID', 'obsID', 'alt', varNames)]
    return(data)
}

addOutsideGood = function(data) {
    numObs  = max(data$obsID)
    numResp = max(data$respID)
    numAlts = max(data$alt)
    data_outsideGood = 0*data[1:numObs,]
    data_outsideGood$outsideGood = 1
    data$outsideGood = 0
    data_outsideGood$order = seq(numAlts, (nrow(data) + (numAlts-1)), numAlts)
    data$order = seq(1, nrow(data), 1)
    data       = rbind(data, data_outsideGood)
    data       = data[order(data$order),]
    numAlts    = numAlts + 1
    data       = addMetaData(data, numObs, numResp, numAlts)
    data$order <- NULL
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
data_mnl = simulateExperiment(
    atts = list(
        price       = c(15, 20, 25),
        fuelEconomy = c(20, 25, 30),
        accelTime   = c(6, 7, 8),
        elec        = c(0, 1)),
    numObs  = 5000,
    numResp = 500,  # 10 questions per respondent
    numAlts = 3)
data_mnl_2groups = data_mnl
data_outsideGood = addOutsideGood(data_mnl)

# Set parameters 
pars_mnl = c(
    price       = -0.7,
    fuelEconomy = 0.1,
    accelTime   = -0.2,
    elec        = -4.0)
pars_mnl_2groups = c(
    price       = -0.6,
    fuelEconomy = 0.15,
    accelTime   = -0.3,
    elec        = -1.0)
pars_outsideGood = c(pars_mnl, outsideGood = -15.0)

# Simulate choices
data_mnl$choice         = simulateChoices(data_mnl, pars_mnl)
data_mnl_2groups$choice = simulateChoices(data_mnl_2groups, pars_mnl_2groups)
data_outsideGood$choice = simulateChoices(data_outsideGood, pars_outsideGood)

# Format powertrain variable
data_mnl$powertrain         = ifelse(data_mnl$elec == 1, 'elec', 'gas')
data_mnl_2groups$powertrain  = ifelse(data_mnl_2groups$elec == 1, 'elec', 'gas')
data_outsideGood$powertrain = ifelse(data_outsideGood$elec == 1, 'elec', 'gas')

# Update names
varNames = c('respID', 'obsID', 'alt', 'choice', 'price', 'fuelEconomy',
             'accelTime', 'powertrain')
data_mnl         = data_mnl[varNames]
data_mnl_2groups  = data_mnl_2groups[varNames]
data_outsideGood = data_outsideGood[c(varNames, 'outsideGood')]

# Add second group for 2-groups data set 
data_mnl_2groups$group = 'B' 
temp = data_mnl
temp$group = 'A'
data_mnl_2groups = rbind(temp, data_mnl_2groups)

# Save data
write.csv(data_mnl, './data/data_mnl.csv', row.names=F)
write.csv(data_mnl_2groups, './data/data_mnl_2groups.csv', row.names=F)
write.csv(data_outsideGood, './data/data_outsideGood.csv', row.names=F)
