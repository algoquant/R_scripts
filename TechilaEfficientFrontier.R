#############################
# Set Environment Variables #
#############################

setwd("C:/PathToWorkingDirectory")
Sys.setenv("JAVA_HOME"="C:\\Program Files\\Java\\PathToJava")
Sys.setenv("TECHILA_SDKROOT" = "C:/PathToTechila")

##################
# Load Libraries #
##################

library("rportfolios") # Random portfolio generation
library("plyr") # Merging and applying functions
library("quantmod") # Downloading returns
library("PerformanceAnalytics") # Charting and calculating return metrics
library("ggplot2") # Visualizing data
library("reshape2") # Melting objects, passed into ggplot

########################
# Techila Installation #
########################

library("rJava")
library("R.utils")
library("techila")
registerDoTechila()

# Set TRUE to run in parallel with Techila, FALSE to run locally
isParallel <- T

# Verify Techila is working
foreach(b=1:4, .combine="cbind") %:%
  foreach(a=1:3) %dopar% {
    a* b
  }

####################
# Data Preparation #
####################

# Now use sector ETFs as tickers
Tickers <- sort(c("XLU", "XLV", "XLI", "XLY", "XLK", "XLF", "XLP", "XLE", "XLB"))
numAssets <- length(Tickers) # How many assets do we have?

# Assign dates to set range for stock quotes
sDate <- as.Date("2000-01-01")
eDate <- Sys.Date()

# Download the data for each ticker
# library("quantmod")
# library("plyr")
tickerData <- llply(Tickers, function(ticker, sDate, eDate){
  getSymbols(ticker, src = "yahoo", from=sDate, to=eDate, auto.assign = F)
},.parallel = isParallel, sDate=sDate, eDate=eDate, .paropts=list(.options.packages=list("plyr","quantmod"))) 

# Merge and calculate Returns
# library("PerformanceAnalytics")
stockMerge <- do.call(merge, lapply(tickerData, Ad)) # Take adjusted close prices
stockReturns <- ROC(na.omit(stockMerge), type="discrete")[-1,] # Get rid of first NA return
colnames(stockReturns) <- Tickers # Rename the columns

# Chart cumulative returns of our assets
charts.PerformanceSummary(stockReturns, main="Individual Stock Cumulative Performance", geometric = T)

# How many simulations should we run?
numSims <- 1000 # 100 max for non parallel

### One Variable ###

# library("rportfolios")

# Generate a random portfolio return timeseries given parameters
randomSim <- function(seed, numAssets, probLong, maxWeight, stockReturns){
  weight <- random.general.test( n=numAssets, p=probLong, x.u = maxWeight)$x
  portRet <- as.xts(apply(sweep(stockReturns, 2, weight, FUN="*"),1,sum))
  return(portRet)
}

# Dynamically plot the return timeseries as the code is runinng
plotPrices <- function(preresult){
  invisible(lines(as.zoo(cumprod(1+preresult[[1]])-1), col=adjustcolor("black", alpha.f=0.1))) # Realtime Charting
  return(preresult)
}

# Verify function works correctly
test <- randomSim(1, numAssets, 1, 0.5, stockReturns)
head(test)

# Generate base plot
plot(as.zoo(cumprod(1+test)-1), ylim=c(-1.0,10), ylab="Cumulative Return", col=adjustcolor("black", alpha.f=0.0))

# Now generate portfolios and dynamically generate the chart, capture the pre-results
result <- maply(.data=1:numSims,
                .fun=randomSim,
                .parallel=isParallel, 
                probLong=1, 
                numAssets=numAssets, 
                maxWeight=0.50,
                stockReturns=stockReturns,
                .paropts=list(.options.callback=plotPrices,
                              .options.packages=list("rportfolios","xts"),
                              .options.steps=10#,.options.projectid=206
                ))

# Combine results into a zoo object and re-index
result.zoo <- as.zoo(t(result))
index(result.zoo) <- index(stockReturns)

# Visualize results some more
charts.PerformanceSummary(result.zoo[,1:10], main="Cumulative Performance of Some Simulations", geometric = F)

### Two Variables / No Live Charting ###

# Vector of maxweight parameters to iterate through
maxWeights <- c(1/numAssets, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50)

# All possible combinations of parameters  
paramGrid <- expand.grid(seed=1:numSims, maxWeight=maxWeights)
dim(paramGrid)

# Run simulation in Techila
result2d <- maply(.data=paramGrid,
                  .fun=randomSim,
                  .parallel=isParallel, 
                  probLong=1, 
                  numAssets=numAssets,
                  stockReturns=stockReturns,
                  .paropts=list(.options.packages=list("rportfolios","xts")
                                #,.options.steps=10
                  ))

# Melt objects before charting with ggplot
#library(ggplot2)
#library(reshape2)

# Pre-allocate
meltedPath <- NA

# Loop through each weight parameter
for(i in 1:length(maxWeights)){
  
  # Extract the simulation returns
  pathrets <- data.frame(t(result2d[,i,]))
  
  # Calculate cumulative returns and reindex the path
  path <- as.zoo(apply(pathrets+1,2,cumprod)-1)
  index(path) <- index(stockReturns)
  
  # Change frequency to monthly and re-index
  path.monthly <- to.monthly(path, OHLC=F)
  path.monthly.df <- data.frame(path.monthly)
  path.monthly.df$Date <- index(path.monthly) # Add date column for indexing
  path.monthly.df$MaxWeight <- maxWeights[i] # Fill in weight parameter 
  
  # Calculate the average path across each simulation for this weight parameter
  path.monthly.df$Average <- apply(path.monthly.df[,1:numSims], 1, mean)
  
  # Return all values to the melted object
  meltedPath <- rbind(meltedPath, melt(path.monthly.df, id.vars = c("Date","MaxWeight","Average"), variable.name = "Seed", value.name="Return"))
}
meltedPath <- meltedPath[-1,]
dim(meltedPath)

# Chart Averages using GGPlot
p <- ggplot(meltedPath, aes(x=Date, y=Average, group=MaxWeight))
p + geom_line(aes(colour = MaxWeight)) + scale_colour_gradient(low="red") + scale_x_yearmon()

# Chart all results in ggplot2
p <- ggplot(meltedPath, aes(x=Date, y=Return, group=Seed))
p + geom_line(aes(colour = MaxWeight)) + scale_colour_gradient(low="red") + scale_x_yearmon()

# Melt for Performance Metrics
perfMetrics <- NA # Pre-allocate
for(i in 1:length(maxWeights)){
  # Gather and re-index simulation results
  pathrets <- data.frame(t(result2d[,i,]))
  path <- as.zoo(apply(pathrets+1,2,cumprod)-1)
  index(path) <- index(stockReturns)
  
  # Calculate annualized return, risk, and sharpe ratios
  Return <- as.numeric(tail(path+1,1)^(252/nrow(path))-1)
  Risk <- as.numeric(apply(pathrets, 2, StdDev.annualized, scale=252))
  Sharpe <- as.numeric(Return/Risk)
  
  # Gather results into a data frame and return
  metricsDF <- data.frame(Return=Return, Risk=Risk, Sharpe=Sharpe, MaxWeight=maxWeights[i])
  perfMetrics <- rbind(perfMetrics, metricsDF)
}
perfMetrics <- perfMetrics[-1,]

# Analyze performance metrics
p <- ggplot(perfMetrics, aes(x=Risk, y=Return, color=MaxWeight))
p + geom_point(alpha=0.5)+ scale_colour_gradient(low="red")

p <- ggplot(perfMetrics, aes(x=Risk, y=Return, color=Sharpe))
p + geom_point(alpha=0.5)+ scale_colour_gradient(low="red",high="green")

p <- ggplot(perfMetrics, aes(x=Risk, y=Sharpe, color=MaxWeight))
p + geom_point(alpha=0.5)+ scale_colour_gradient(low="red")