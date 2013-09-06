#######################################
### Risk and Statistical Functions  ###
#######################################

#############
# Calculate credit betas for a given CDS ticker
#############
### Load CDS price ticks, join them with Index in R, and calculate credit betas
statBeta_inR <- function(symbol="GLENCR", ts.price.index, otq.file=NULL, field="VALID_MID") {

# Load and join CDS price ticks with Index price ticks (left join)
  ts.price.join <- loadCDSIndex(symbol, ts.price.index, otq.file, field=field)
  nDataMin <- 30
  if (length(ts.price.join)<nDataMin)
    stop(paste("statBeta_inR: Not enough data points for the symbol: ", colnames(ts.price.join[,1])))
# Run regression on returns
  ts.rets.join <- na.locf(diff(ts.price.join))
  lmBetas <- lm(ts.rets.join[,1] ~ ts.rets.join[,3] + ts.rets.join[,3])
  namesMain <- paste(colnames(ts.price.join[,1])," residuals /", date())
  plot(cumsum(lmBetas$residuals), main=namesMain, major.format="%b %y")
#  to.daily(ts.price.join)
  score.stat <- statSummary(lmBetas$residuals, bPlot=TRUE, namesMain=namesMain)
  scores <- c(as.vector(summary.lm(lmBetas)$coefficients), summary.lm(lmBetas)$r.squared, score.stat)
  scores
}
### End statBeta_inR



### Calculate regression credit betas for a time series of CDS price ticks joined with Index
# First column of time series should be CDS price ticks
lm.betas <- function(ts.rets) {
#  nDataMin <- 30
#  if (length(ts.rets)<nDataMin)
#    stop(paste("lm.betas: Not enough data points for the symbol: ", colnames(ts.rets[,1])))
# Calculate returns
#  ts.rets <- na.locf(diff(ts.rets))
### Calculate the statistical scores for CDS alone
#  score.stat <- list()
#  score.stat$CDSstats <- tryCatch(statSummary(ts.rets[,1]), error = function(e) e)
#  score.stat <- unlist(tryCatch(statSummary(ts.rets[,1]), error = function(e) e))
#  cat(unlist(tryCatch(statSummary(ts.rets[,1]), error = function(e) e)), sep=",")
### Run regression on returns - first column is CDS, second is Index
  lmBetas <- lm(ts.rets[,1] ~ ts.rets[,2])
  score.stat <- c(lm.beta=summary(lmBetas)$coefficients[2,1],lm.error=summary(lmBetas)$coefficients[2,2],lm.t.value=summary(lmBetas)$coefficients[2,3],lm.r.squared=summary(lmBetas)$r.squared)
#  score.stat$regCoeff <- summary.lm(lmBetas)$coefficients[2,1]
#  score.stat$regRsq <- summary.lm(lmBetas)$r.squared
#  score.stat <- c(score.stat, unlist(summary.lm(lmBetas)$coefficients))
#  score.stat <- c(score.stat, unlist(summary.lm(lmBetas)$r.squared))
#  cat(unlist(summary.lm(lmBetas)$coefficients), sep=",")
#  cat(",")
#  cat(unlist(summary.lm(lmBetas)$r.squared), sep=",")
#  cat(",")
### Plot regression residuals
#  namesMain <- paste(colnames(ts.rets[,1]),"residuals /", date())
#  plot(cumsum(lmBetas$residuals), main=namesMain, major.format="%b %y")
#  to.daily(ts.rets)
### Calculate the statistical scores for residuals
#  score.stat$residStats <- tryCatch(statSummary(lmBetas$residuals, namesMain=namesMain), error = function(e) e)
#  score.stat <- c(score.stat, unlist(tryCatch(statSummary(lmBetas$residuals, namesMain=namesMain), error = function(e) e)))
#  cat(unlist(tryCatch(statSummary(lmBetas$residuals, namesMain=namesMain), error = function(e) e)), sep=",")
#  write.table(tryCatch(statSummary(lmBetas$residuals, namesMain=namesMain), error = function(e) e), sep=",")
#  as.vector(score.stat)
#  coredata(score.stat)
  score.stat
}
### End lm.betas



#############
# Calculate the Variance Profile of a time series of returns
# The Variance Profile is the variance as a function of aggregation periods or ticks
# Regress the variance as a function of aggregation periods
#############
varProfile <- function(ts.rets, agg.min=NULL, agg.max=NULL, bMean=FALSE, b.intercept=TRUE) {

  if (!is.xts(ts.rets))
    stop("The ts argument is not an xts object!")
# stopifnot(is.xts(ts.rets))
# Check if ts.rets has enough data points - nDataMin is the minimum number of data points
  nDataMin <- 30
  if (length(ts.rets)<nDataMin)
    stop(paste("varProfile: Not enough data points for the symbol: ", colnames(ts.rets[,1])))

# aggCap is the cap on the number of aggregation periods for calculating the variance
  aggCap <- trunc(length(ts.rets) / 10)

# agg.max is the maximum number of aggregation periods for calculating the variance
  if(is.null(agg.max)) {
    agg.max <- aggCap
  }
    else if(agg.max > aggCap) {
      warning("The 'agg.max' parameter is greater than 10% of length(ts.rets)")
      warning("The 'agg.max' parameter was truncated down to: ",aggCap)
      agg.max <- aggCap
    }
  
# agg.min is the minimum number of aggregation periods for calculating the variance
  if(is.null(agg.min)) agg.min <- 1

# vAggregate is a vector of aggregation periods for calculating the variance
  vAggregate <- agg.min : agg.max

# Demean the time series
  if(bMean)
    ts.rets <- ts.rets - mean(ts.rets, na.rm = TRUE)

# Calculate running variance aggregated over agg.max periods - call compiled TTR function "runSum"
  profile.var <- sapply(vAggregate, function(agg) var(.Call("runSum", ts.rets, agg), na.rm = TRUE))

# Perform log-log regression of aggregated variance versus aggregation periods
#  n.ticks <- log(vAggregate)
# Perform regression of aggregated variance versus aggregation periods
  n.ticks <- vAggregate
  if(b.intercept)
# Allow reg line intercept
    lm(profile.var ~ n.ticks)
  else
# Force reg line through the origin
    lm(profile.var ~ n.ticks - 1)
}
### End varProfile



# Returns Variance curve as opposite to regression object - DEPRECATED!!!
varProf <- function(ts.rets, agg.min=2, agg.max=NULL, bMean=FALSE) {
  if (!is.xts(ts.rets))
    stop("The ts argument is not an xts object!")
# stopifnot(is.xts(ts.rets))
# Check if ts.rets has enough data points - nDataMin is the minimum number of data points
  nDataMin <- 30
  if (length(ts.rets)<nDataMin)
    stop(paste("varProfile: Not enough data points for the symbol: ", colnames(ts.rets[,1])))

# aggCap is the cap on the number of aggregation periods for calculating the variance
  aggCap <- trunc(length(ts.rets) / 2)

# agg.max is the maximum number of aggregation periods for calculating the variance
  if(is.null(agg.max)) {
    agg.max <- aggCap
  }
    else if(agg.max > aggCap) {
      warning("The 'agg.max' parameter is greater than 50% of length(ts.rets)")
      warning("The 'agg.max' parameter was truncated down to: ",aggCap)
      agg.max <- aggCap
    }
  
# agg.min is the minimum number of aggregation periods for calculating the variance
  if(is.null(agg.min)) agg.min <- 1

# vAggregate is a vector of aggregation periods for calculating the variance
  vAggregate <- agg.min : agg.max

# Demean the time series
  if(bMean)
    ts.rets <- ts.rets - mean(ts.rets, na.rm = TRUE)

# Calculate running variance aggregated over agg.max periods - call compiled TTR function "runSum"
  varProf <- sapply(vAggregate, function(agg) var(.Call("runSum", ts.rets, agg), na.rm = TRUE))

  varProf
}
### End varProf


### varProfile w big Data
varProfBD <- function(ts.rets, nMax=1000) {

  x <- na.omit(coredata(ts.rets))
  x.l <- length(x)
  if(x.l>nMax) {
    nx <- ceiling(x.l/nMax)
    x.idx <- seq(1, x.l, by=nx)
    x.f <- x[x.idx]
  }
  else x.f <- x
  agg.max <- 0.7*length(x.f)
  vAggregate <- 1:agg.max
  varProf <- sapply(vAggregate, function(agg) var(.Call("runSum", ts.rets, agg), na.rm = TRUE))
  varProf

}
### End varProfBD



#############
# Calculate and plot the Rescaled Variance Profile of a time series of returns
# The Rescaled Variance Profile is the Variance Profile divided by the aggregation periods
#############
varProfileRescaled <- function(symbol="GLENCR", otq.file=NULL, field="VALID_MID", agg.min=NULL, agg.max=NULL, bMean=FALSE) {

# Load tick returns
  ts.rets <- na.omit(diff(loadCDS(symbol, otq.file, field=field)))
#  plot.xts(cumsum(ts.rets), main=paste(symbol," return data"), major.format="%b %y")

# Check if ts.rets has enough data points - nDataMin is the minimum number of data points
  n.ticks <- length(ts.rets)
  nDataMin <- 30
  if (n.ticks<nDataMin)
    stop(paste("varProfileRescaled: Not enough data points for the symbol: ", symbol))

# Calculate the Variance Profile of the time series
  profile.var <- varProfile(ts.rets, agg.min=agg.min, agg.max=agg.max)
#  plot.xts(cbind(profile.var$model[2], profile.var$model[1]), main=paste("varProfile ",symbol," profile.var slope=",round(profile.var$coefficients[[2]],2)), major.format="%b %y")

# Return the slope regression coefficient
#  summary.var <- c(symbol, profile.var$coefficients[[2]])
# Rescale the Variance Profile and calculate its max
  cutoff <- trunc(length(profile.var$model[,1])/10)
  profile.var.resc <- tail(cbind(profile.var$model[,2], (profile.var$model[cutoff,2]/profile.var$model[cutoff,1])*profile.var$model[,1]/profile.var$model[,2]), length(profile.var$model[,1])-cutoff)
  var.max <- max(profile.var.resc[,2])
  tick.max <- which.max(profile.var.resc[,2])
# Plot the Variance Profile
#  plot(profile.var.resc, main=paste("varProfile ", symbol," var.max=",round(var.max,2), " at ", tick.max, " ticks"), major.format="%b %y")
  summary.var <- as.vector(c(symbol,n.ticks,var.max,tick.max))
#  summary.var <- cat(symbol,n.ticks,var.max,tick.max,"",sep=",",append=TRUE)

#  summary.var
#  print(summary.var)
#  write(summary.var,"",append=TRUE,sep="\t")
#  cat(summary.var,"",sep=",",append=TRUE)
#  stop(invisible(cat(symbol,n.ticks,var.max,tick.max,"",sep=",",append=TRUE)))
  cat(symbol, n.ticks, var.max, tick.max, "", sep=",", append=TRUE)

}
### End varProfileRescaled



#############
# Calculate the Rescaled Variance Profile for a portfolio (pair) of returns, and weights
#############
varProfPortf <- function(beta=NULL, ts.price=NULL, agg.min=1, agg.max=1000) {
  ts.rets <- diff((1-beta)*ts.price[,1] - beta*ts.price[,2])
  ts.rets[1] <- 0.0
  profile.var <- varProfile(ts.rets, agg.min=agg.min, agg.max=agg.max)
  cutoff <- trunc(length(profile.var$model[,1])/10)
  profile.var.resc <- tail(cbind(profile.var$model[,2], (profile.var$model[cutoff,2]/profile.var$model[cutoff,1])*profile.var$model[,1]/profile.var$model[,2]), length(profile.var$model[,1])-cutoff)
  n.ticks <- length(ts.rets)
  var.max <- max(profile.var.resc[,2])
  tick.max <- which.max(profile.var.resc[,2])
#  summary.var <- paste("Input xts: ", colnames(ts.rets), "\t", "Output: ", n.ticks, var.max, tick.max, sep="\t")
#  summary.var
#  summary.var <- cat(n.ticks, var.max, tick.max, "\n", "", sep="\t", append=TRUE)
#  summary.var <- write(n.ticks, var.max, tick.max, "\n", file="", sep="\t", append=TRUE)
#  summary.var <- paste(n.ticks, var.max, tick.max, "\n", sep="\t")
# Write to output
#  summary.var <- paste(n.ticks, var.max, tick.max, "\n", sep=",")
#  cat(summary.var)
#  cat("Input xts: ", colnames(ts.rets), "\n", "Output: ", n.ticks, var.max, tick.max, "\n", "", sep="\t", append=TRUE)
#  var.max <- paste((colnames(ts.rets), n.ticks, var.max, tick.max, "", sep=",", append=TRUE))
#  plot(profile.var.resc, type='l')
  summary.var <- c(beta, var.max, tick.max)
  names(summary.var) <- c("beta", "var.max", "tick.max")
  summary.var
}
### End varProfPortf



var.agg <- function(ts.rets, agg.window=NULL, bmean=FALSE) {
  if(is.null(agg.window))
    agg.window <- 1 : trunc(length(ts.rets) / 10)
  if(max(agg.window) > trunc(length(ts.rets) / 10))
    warning("If 'agg.window' parameter is greater than 9% of length(ts.rets), then results may be unreliable.")
  if(bmean)
    ts.rets <- ts.rets - mean(ts.rets, na.rm=TRUE)
  var <- var(.Call("runSum", ts.rets, agg.window), na.rm=TRUE)
  1000*var
}


# Calculate the percentage of the signal's energy spectrum in high frequencies
spectral.frac <- function(ts.rets, cutoff=0.25) {
  specDensity <- spectrum(coredata(ts.rets), plot=FALSE)$spec
  specDensity[1:5] <- 0.0
  specLength <- length(specDensity)
#  plot(spectrum(ts.rets))
  hfreqEnergy <- sum(tail(specDensity, cutoff*specLength))
  hfreqFrac <- 100*hfreqEnergy/sum(specDensity)
  hfreqFrac
}


#############
# Persistence Analysis (Cowles-Jones test?)
#############
# Calculate the persistence intervals of a time series of prices
# Persistence intervals are intervals of time series with monotonically trending prices
# The price time series is averaged over a moving aggregation window
persistenceIntervals <- function(ts.price, agg.window=NULL) {
# Find scale of price range
  range <- max(ts.price)-min(ts.price)

  if (!is.null(agg.window))
# Aggregate return data
    {
      width <- max(1,trunc(agg.window*length(ts.price)))
#  ts.price.mean <- na.omit(apply.rolling(ts.price, width=width, FUN="mean"))
#  ts.price.mean <- na.omit(runMean(ts.price, n=agg.window))
      ts.price.mean <- na.omit(.Call("runSum", ts.price, n=width))/width
    }
# Don't aggregate return data
  else
    ts.price.mean <- ts.price

  ts.retMean <- na.omit(diff(ts.price.mean))

#  plot.xts(ts.price, main=paste("Price ",symbol), major.format="%b %y")
#  plot.xts(ts.price.mean, main=paste("Price mean ",symbol," w/ ",width," tick window"), major.format="%b %y")

# Find turning points
  turningDates <- na.omit(ts.retMean*lag(ts.retMean)>0)
  turningPoints <- ts.price.mean[!turningDates]
# draws are either draw-downs or draw-ups
  draws <- abs(as.vector(na.omit(diff(turningPoints))))/range
#  colnames(draws) <- colnames(ts.price)
# periods are the periods of monotonic trending in number of days
  periods <- as.vector(na.omit(diff(.index(turningPoints))))/86400
  interv <- cbind(draws, periods)
# Sort the draws and periods
  interv <- interv[order(draws, decreasing=TRUE),]
#  draws <- draws[order(draws, decreasing=TRUE)]/range

#  blah <- merge.xts(draws=na.omit(diff(turningPoints)),interv=na.omit(diff(index(turningPoints))))
#  barplot(head(draws,20), names.arg=1:20, ylab="Draw percentage", xlab="Ordinal")
#  cat(interv, sep="\t")
#  write.table(interv, sep=",")
  interv
}
### End persistenceIntervals



#############
# Calculate the persistence profile of a time series of prices
# The Persistence Profile is the sum of the largest persistence intervals vs. the number of aggregation ticks
#############
persistenceProfile <- function(symbol="GLENCR", otq.file=NULL, field="VALID_MID", agg.window) {

# Load tick returns
  ts.price <- loadCDS(symbol, otq.file, field=field)
# Check if ts.price has enough data points - nDataMin is the minimum number of data points
  n.ticks <- length(ts.price)
  nDataMin <- 30
  if (n.ticks<nDataMin)
    stop(paste("persistenceProfile: Not enough data points for the symbol: ", symbol))

# Number of aggregation windows
  nAgg <- length(agg.window)

# Number of biggest draws
  nDraws <- 20

# Calculate the persistence profile by looping over agg.windows
  if (nAgg>1)
# Loop over agg.window range
    aggProfile <- sapply(agg.window, function(aggW) {
                     draws <- persistenceIntervals(ts.price=ts.price, agg.window=aggW)[,1]
                     sDraws <- sum(head(draws, nDraws))
                     sDraws
                   }
                         )
  else
# Calculate the persistence intervals for a single agg.window
    draws <- persistenceIntervals(ts.price=ts.price, agg.window=agg.window)[,1]
# End if

# Calculate the peak of the persistence profile
  drawMax <- max(aggProfile)
  tick.max <- which.max(aggProfile)
  summary.var <- as.vector(c(symbol,n.ticks,drawMax,tick.max))
  cat(symbol, n.ticks, drawMax, tick.max, "", sep=",", append=TRUE)

}
### End persistenceProfile



#############
# Calculate the persistence intervals for a list of symbols and single agg.window
#############
persistenceIntervalsWrapper <- function(symbol="GLENCR", otq.file=NULL, field="VALID_MID", agg.window) {

  if (is.null(otq.file))
    otq.file <- "../../OneTick/Get_scrub_ticks.otq::Get_scrub_ticks"
# Load the time series of prices
  ts.price <- oneTickQueryOTQ(otq.file, SYMBOL=symbol, FIELD=field, context='REMOTE')
  colnames(ts.price) <- symbol
# Remove duplicate ticks
#  ts.price <- ts.price[!duplicated(.index(ts.price))]
  ts.price <- make.index.unique(ts.price)

  agg.window <- agg.window/length(ts.price)
# Calculate the persistence intervals
  draws <- persistenceIntervals(ts.price=ts.price, agg.window=agg.window)[,1]

}
### End persistenceIntervalsWrapper



#############
# Calculate the persistence histogram of a time series
#############
persistenceHistogram <- function(ts.price, time.window, breaks=10) {

  if (!is.xts(ts.price))
    stop("The ts argument is not an xts object!")

  stat.persist <- statPersist(ts.price, time.window)
  hist.persist <- hist(coredata(stat.persist), breaks, plot=FALSE)
  hist.persist$counts <- hist.persist$counts/sum(hist.persist$counts)
# Calculate ratio of histogram tails divided by belly
  breaks <- trunc(breaks/3)
  skew.persist <- sum(hist.persist$counts[c(1:breaks,(time.window-breaks+1):time.window)])/sum(hist.persist$counts[(breaks+1):(time.window-breaks)])
  -skew.persist

}
### End persistenceHistogram



# Function for compiling a variety of correlation statistics
statSummary <- function(ts.rets, ...) {
  nDataMin <- 30
  if (length(ts.rets)<nDataMin)
    stop(paste("statSummary: Not enough data points for the symbol:", colnames(ts.rets)))
# Create vector of betas to calculate stats
  betas <- seq(from=0, to=2.0, by=0.1)
#  namesMain <- colnames(ts.rets)

# Regression
  stat.lm <- lm.betas(ts.rets)

# Ljung-Box test
#  LjBox <- Box.test(ts.rets, type="Ljung")

# Range Analysis
  stat.range <- betaProfile(statRange, ts.rets, betas)
#  write.table(stat.range, sep=",")

# Persistence Analysis
#  stat.persist <- betaProfile(statPersist, ts.rets, betas)
#  write.table(stat.persist, sep=",")

# PACF Analysis
  stat.pacf <- betaProfile(statPACF, ts.rets, betas)
#  write.table(stat.pacf, sep=",")

# VAR Analysis
  stat.var <- betaProfile(statVAR, ts.rets, betas)
#  write.table(stat.var, sep=",")

### Write to output
#  score.stat <- c(score1, score2, score3)
#  score.stat <- as.vector(score.stat)
#  names(score.stat) <- colnames(ts.rets)
#  lapply(stat.pacf, function(sScore) cat(sScore, ","))
#  score.stat
#  write.table(c(colnames(ts.rets[,1]), stat.lm, stat.range, stat.persist, stat.pacf), sep=",", row.names=FALSE, col.names=FALSE)
  out.put <- c(stat.lm, stat.range, stat.pacf, stat.var)
  writeMessage(out.put)
  out.put
}
### End statSummary



#############
# Calculate the aggregation window producing the highest PACF score
#############
aggWrapper <- function(symbol="GLENCR", otq.file=NULL, field="VALID_MID", agg.window) {

# Load tick prices
  ts.price <- loadCDS(symbol, otq.file, field=field)
# Calculate PACF score as a function of aggregation window
aggPacf <- sapply(agg.window, function(aggW, ts.price)
                       {
                         ts.retMean <- na.omit(diff(aggTimeSeries(ts.price, aggW)))
                         statSummary(ts.retMean)
                       },
                     ts.price=ts.price
                     )

# Calculate the aggregation window producing the highest PACF score
  agg.window <- which.max(tail(aggPacf, length(aggPacf)-3))
  agg.window

}
### End aggWrapper



# Compile some aggregation statistics
aggStat <- function(ts.price) {
  cat(colnames(ts.price), "\n")
  nCount <- length(ts.price)
  nCount
}
### End aggStat



#############################################
### Objective functions for optimization  ###
#############################################

### Objective function for optimization
optimObjective <- function(ts.rets) 100*varProfile(ts.rets, nScale=1:5)$coef[2] / varProfile(ts.rets, nScale=1:10)$coef[2]

### Calculate the variance ratio
var.ratio <- function(ts.rets, agg.min, agg.max) var.agg(ts.rets, agg.window=agg.max)/var.agg(ts.rets, agg.window=agg.min)*(agg.min/agg.max)/2


### Calculate Hurst exponent using range
Hurst <- function(ts.rets) {
  ts.cum <- cumsum(ts.rets)
  log((max(ts.cum)-min(ts.cum))/sqrt(var(ts.rets)))/log(length(ts.rets))
}
### End Hurst


### Calculate Hurst exponent using variance ratios
var.Hurst <- function(beta, ts.rets, agg.min, agg.max) {
  ts.tmp <- ts.rets[,1] - beta*ts.rets[,2]
  ts.agg.min <- na.omit(runSum(ts.tmp, n=agg.min))
#  ts.agg.min[1:agg.min,] <- 0.0
#  ts.agg.max <- na.omit(cbind(runSum(ts.tmp, n=agg.max), runSum(ts.rets[,3]), n=agg.max))
  ts.agg.max <- na.omit(runSum(ts.tmp, n=agg.max))
#  ts.agg.max[1:agg.max,] <- 0.0
#  hurst.exp <- var(ts.agg.max[,1])*cor(x=ts.agg.max[,1], y=ts.agg.max[,2])[[1]]/var(ts.agg.min[,1])*(agg.min/agg.max)/2
  hurst.exp <- var(ts.agg.max[,1])/var(ts.agg.min[,1])*(agg.min/agg.max)/2
  hurst.exp[[1]]
}
### End var.Hurst


### Calculate running Hurst exponent
run.Hurst <- function(ts.rets, look.back=10) {
  ts.cum <- cumsum(ts.rets)
  run.max <- runMax(x=ts.cum, n=look.back)
  run.min <- runMin(x=ts.cum, n=look.back)
  run.range <- run.max-run.min
  run.range[1:look.back,] <- run.range[look.back+1,]
  run.sd <- sqrt(runVar(x=ts.rets, n=look.back))
  run.sd[1:look.back,] <- run.sd[look.back+1,]
  log(run.range/run.sd)/log(nrow(ts.rets))
}
### End run.Hurst


### Different stat functions are defined below

### Calculate the range of a price series
# Return positive value for use in optimization
statRange <- function(ts.price) (max(ts.price)-min(ts.price))


### Calculate a time series of rolling persistence scores
statPersist <- function(ts.price, time.window, type.persist="range") {

  stat.persist <- if (type.persist=="range")
    {
# Below the persistence scores are calculated using the price range
      ts.sd <- runMax(ts.price, n=time.window)-runMin(ts.price, n=time.window)
      ts.sd[1] <- 1.0
      ts.sd <- na.locf(ts.sd)
      stat.persist <- diff(ts.price, time.window-1)/ts.sd
      stat.persist[1] <- 0.0
      stat.persist <- na.locf(stat.persist)
    }
# Below the persistence scores are calculated using the standard deviation (like a VAR profile)
  else
    if (type.persist=="sd")
      {
        sd.prices <- runSD(model$prices, n=time.window)
        sd.prices[1] <- 1.0
        sd.prices <- na.locf(sd.prices)
        ts.rets <- diff(model$prices)
        ts.rets[1] <- 0.0
        sd.ret <- runSD(ts.rets, n=time.window)
        sd.ret[1] <- 1.0
        sd.ret <- na.locf(sd.ret)
        stat.persist <- sd.prices/sd.ret
      }
    else
      print("statPersist: ran out of options")

# Below the persistence scores are calculated using the range
#  stat.persist <- sapply(time.window:length(ts.price), function(t.window) {
#    ts.trunc <- ts.price[(t.window-time.window):t.window]
#    (coredata(last(ts.trunc))-coredata(first(ts.trunc)))/(max(ts.trunc)-min(ts.trunc))
#  }
#         )
    stat.persist
}
### End statPersist


# Calculate the PACF score of a price series
statPACF <- function(ts.rets) {
#  ts.rets <- na.omit(diff(ts.price))
  profile.pacf <- pacf(ts.rets, lag=30, plot=FALSE)
  thresh <- 2/sqrt(length(ts.rets))
# Return negative value for use in optimization
  -sum(profile.pacf$acf[profile.pacf$acf>thresh] - thresh)
}
# End statPACF


# Calculate the peak of the rescaled VAR profile of a price series
statVAR <- function(ts.rets=NULL, agg.min=1, agg.max=1000) {
#  ts.rets <- na.omit(diff(ts.price))
  profile.var <- varProfile(ts.rets, agg.min=agg.min, agg.max=agg.max)
  cutoff <- trunc(length(profile.var$model[,1])/10)
  profile.var.resc <- tail(cbind(profile.var$model[,2], (profile.var$model[cutoff,2]/profile.var$model[cutoff,1])*profile.var$model[,1]/profile.var$model[,2]), length(profile.var$model[,1])-cutoff)
  var.max <- max(profile.var.resc[,2])
# Return negative value for use in optimization
  -var.max
}
### End statVAR



# Calculate the beta between two price series which produces the minimum value of func.stat
# New version, takes column names
beta.profile <- function(func.stat, ts.rets, colname1, colname2, betas, ...) {
# Convert from character string to function name
  func.stat <- match.fun(func.stat)
# Calculate the beta profile - first column of ts.rets is CDS, second is Index
  beta.profile <- sapply(betas, func.stat, ts.rets=cbind(ts.rets[,colname1],ts.rets[,colname2]), ...)
# End sapply
  beta.profile <- cbind(betas, beta.profile)
  colnames(beta.profile) <- c("beta","Var.ratio")
  score.stat <- c(beta.profile[1,2],beta.profile[which.max(beta.profile[,2]),c(1,2)])
  names(score.stat)[1] <- "Var.base"
  score.stat
}
# End beta.profile


# Calculate the beta between two price series which produces the minimum value of func.stat
betaProfile <- function(func.stat, ts.price, betas, ...) {

# Convert from character string to function name
  func.stat <- match.fun(func.stat)
# Calculate the beta profile - first column of ts.price is CDS, second is Index
  profile.beta <- sapply(betas, function(beta) as.numeric(func.stat(ts.price[,1]+beta*ts.price[,2], ...)))
# End sapply
  profile.beta <- cbind(as.vector(betas), as.vector(profile.beta))
# Calculate func.stat for Index and for CDS
#  stat.index <- func.stat(ts.price[,3])
  score.stat <- list()
  score.stat$cds <- as.numeric(func.stat(ts.price[,1], ...))
#  score.stat$index <- stat.index
# score.stat$cds.hedged is the extreme value of func.stat (for example the percentage reduction in the range of the Index)
  extreme.val <- max(profile.beta[,2])
#  score.stat$cds.hedged <- (stat.index-extreme.val)/stat.index
  score.stat$cds.hedged <- extreme.val
#  score.stat$beta <- profile.beta[which.min(profile.beta[,2]),1]
  score.stat$beta <- profile.beta[profile.beta[,2]==extreme.val,1]
#  names(score.stat) <- paste(func.stat, c("index", "stat", "beta"), sep=".")
#  name. <- paste(func.stat, "index", sep=".")
  names(score.stat) <- c("cds", "cds.hedged", "beta")
#  c(score.stat$index, score.stat$cds.hedged, score.stat$beta)
  score.stat

}
# End betaProfile



#############
# Heatmaps
#############

alphaHeatmap <- function(ts.rets, param1=1:2, param2=1:2, ...) {
  vParam1 <- rep(param1, length(param2))
  vParam2 <- rep(param2, each=length(param1))
  alphaData <- alphaModel(ts.rets=ts.rets, param1=vParam1, param2=vParam2, ...)
  dim(alphaData) <- c(length(param1), length(param2))
  rownames(alphaData) <- param1
  colnames(alphaData) <- param2
  names(dimnames(alphaData)) <- c("param1", "param2")
  alphaData
}



