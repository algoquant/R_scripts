################################################
###
### script for: 
###  loading data from CSV file, 
###  calculating rolling (running) volatility
###
################################################



library(quantmod)
library(TTR)

# script for loading data - taken from FRE6871_Lecture5.pdf slide #13
datav <- read.zoo(file="data.csv", 
                  header=TRUE, sep=",", FUN=as.POSIXct, 
                  tz="America/New_York",
                  format="%m/%d/%Y")
# coerce to xts
datav <- as.xts(datav)
tail(datav)

# calculate returns - produces a list of xts
returns <- lapply(datav, dailyReturn)
length(returns)
names(returns)
tail(returns$"Jan.16")
# calculate volatility - produces a named numeric vector
sapply(returns, sd)

rolling_vol <- function(datav=datav, rangev=NULL, tseries=colnames(datav[, 1]), look_back=10) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  stopifnot("package:TTR" %in% search() || require("TTR", quietly=TRUE))
  if (is.null(rangev))
    rangev <- index(datav)
# calculate returns
  returns <- dailyReturn(na.omit(datav[rangev, tseries]))
# calculate rolling volatility
  na.omit(runSD(x=returns, n=look_back))
}  # end rolling_vol

foo_bar <- rolling_vol(datav, rangev="2015-03-01/2015-11-25", tseries="Apr.16", look_back=20)
chart_Series(foo_bar)


