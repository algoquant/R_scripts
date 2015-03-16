################################################
###
###  parsing and aggregating HFREQ data
###
################################################

rm(list=ls())  # remove all objects

library(utils)
library(quantmod)
library(qmao)
library(caTools)
library(lubridate)
library(highfrequency)
library(quantstrat)
library(HighFreq)

Sys.setenv(TZ="America/New_York")  # Set the time-zone to GMT
setwd("C:/Develop/data")
# search()  # get search path
options(digits.secs=6)
options(digits=5)
options(stringsAsFactors=FALSE)
options(max.print=80)


### set data_dir directory
# data_dir <- "/home/storage/settles/"
# data_dir <- "/home/storage/sec/"
data_dir <- "E:/mktdata/sec/"
scrub_dir <- "E:/scrubdata/"
# data_dir <- "/home/storage/tick/"
# print(data_dir)


###########
# code for loading xts data

# load and save data for a single symbol
save_OHLC("IWF")
# load data for list of symbols
sapply(head(sym_bols), save_OHLC)

# load data for a single symbol
load(file="SPY.RData")
chartSeries(SPY["2013"], name=sym_bol, theme=chartTheme("white"))


###########
# code for loading instruments data

# load list of symbols
sym_bols <- read.csv(file="etf_list_hf.csv")
sym_bols <- sym_bols[[1]]

### load list of instrument definitions: creates .instrument environment
loadInstruments(file_name='E:/mktdata/instruments.rda')

# explore the .instrument environment
# list instrument names in the .instrument environment (character vector)
# ls_instruments()  # vary large list!!!
# ls(FinancialInstrument:::.instrument)
# ls_instruments() bigger than ls(FinancialInstrument:::.instrument)
list.instruments <- ls_instruments()
length(list.instruments)
sample(list.instruments, 11)
tail(list.instruments)
write.csv(list.instruments, file="instruments.txt")


# get tickers for all stocks
# ls_instruments_by('type', 'stock')  # very large list!!!
# find.instrument("stock")  # very large list!!!
# get tickers for all computer stocks
find.instrument("computer")
# get tickers for all bond instruments
find.instrument("bond")


# get contract specs for instrument "First Solar stock"
getInstrument("FSLR")
# explore a few instrument objects
an.instrument <- getInstrument("MSFT")
an.instrument <- getInstrument("BAXH3")
an.instrument$type
an.instrument$series_description



###########
# extra code for parsing list.instruments - you can ignore

convert_NULL_NA <- function(in.var) {
  if (is.null(in.var))
    ret.var <- NA
  else
    ret.var <- in.var
  ret.var
}  # end convert_NULL_NA

attr.instrument <- function(name.instrument) {
  my.instrument <- getInstrument(name.instrument)
  c(name=convert_NULL_NA(my.instrument$primary_id[1]), type=convert_NULL_NA(my.instrument$type[1]), longName=convert_NULL_NA(my.instrument$longName[1]), description=convert_NULL_NA(my.instrument$series_description[1]))
}  # end attr.instrument

# table.instruments <- apply(as.matrix(sample(list.instruments, 5)), 1, attr.instrument)
table.instruments <- aperm(sapply(sample(list.instruments, 11), attr.instrument), c(2,1))
table.instruments <- aperm(sapply(list.instruments, attr.instrument), c(2,1))
write.csv(table.instruments, file="table.instruments.txt")
unique(table.instruments[, "type"])
write.csv(unique(table.instruments[, "longName"]), file="unique.instruments.txt")

### end code for parsing list.instruments



###########
# load ts data using getSymbols.FI

### set defaults for getSymbols
# setDefaults(getSymbols, verbose=FALSE, dir=data_dir, src="rda")

# setDefaults for getSymbols: call getSymbols.FI by default
setDefaults(getSymbols, verbose=FALSE, src="FI")
# setDefaults for getSymbols.FI: load data from local drive
setDefaults(getSymbols.FI,
            extension="RData",
            dir=data_dir,
            days_to_omit="Saturday",
            use_identifier="X.RIC")


### load seconds bar data using getSymbols.FI

# run loadInstruments() first
sym_bol <- "SPY"
getSymbols(sym_bol)  # takes very long!!!
dim(SPY)
SPY[10000:10020, ]



###########
# load ts data using custom functions

# create path to directory with *.RData files
file_dir <- file.path(data_dir, sym_bol)
# get list of *.RData files
file_list <- list.files(file_dir)
# create paths to *.RData files
file_names <- file.path(file_dir, file_list)

# load data into list
daily_xts <- sapply(head(file_names), function(file_name) {
  cat("loading", file_name, "\n")
  data_name <- load(file_name)
  get(data_name)
})
length(daily_xts)

# scrub and aggregate the data
daily_xts <- lapply(daily_xts, scrub_agg)

# flatten list into xts - blows up or takes very long!!!
# daily_xts <- do.call(rbind, daily_xts)
# recursively "rbind" the list into a single xts
daily_xts <- do_call_rbind(daily_xts)


# rename the colnames
colnames(daily_xts) <- sapply(strsplit(colnames(daily_xts), split="[.]"), 
                                 function(strng) paste(sym_bol, strng[-1], sep="."))

head(daily_xts)
chartSeries(daily_xts, name=sym_bol, theme=chartTheme("white"))
chartSeries(daily_xts["2008-01-04/2008-01-06"], name=sym_bol, theme=chartTheme("white"))

# install HighFreq
install.packages(pkgs="C:/Develop/R/HighFreq", repos=NULL, type="source")
install.packages(pkgs="C:/Develop/R/HighFreq", repos=NULL, type="source", lib="C:/Users/Jerzy/Downloads")
install.packages(pkgs="C:/Develop/R/HighFreq", repos=NULL, type="source", lib="C:/Users/Jerzy/Documents/R/win-library/3.1")
library(HighFreq)

# package library path
.rs.rpc.get_package_install_context()
.libPaths()
normalizePath(R.home())
normalizePath(R.home("Library"))
normalizePath(.Library)

install.packages("devtools")
library(devtools)
install_github("hadley/devtools")
install_github(repo="hadley/babynames")


###########
# scrubbing functions


### identify suspect bid_offer values in univariate xts time series
extreme_values <- function(bid_offer, vol_window=51, thresh_old=2) {

  stopifnot(
    (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      ("package:caTools" %in% search()) || require("caTools", quietly=TRUE)
  )

# calculate vo_lat as running quantile
  vo_lat <- runquantile(x=abs(as.vector(bid_offer)), k=vol_window, probs=0.9, endrule="constant", align="center")
  vo_lat <- xts(vo_lat, order.by=index(bid_offer))
  colnames(vo_lat) <- "volat"
# carry forward non-zero vo_lat values
  vo_lat[vo_lat==0] <- NA
  vo_lat[1] <- 1
  vo_lat <- na.locf(vo_lat)
#  vo_lat <- na.locf(vo_lat, fromLast=TRUE)
  
# find suspect values
# suspect if bid_offer greater than vo_lat
  sus_pect <- (abs(bid_offer) > 2*thresh_old*vo_lat)
  sus_pect[1] <- FALSE

  cat("date:", format(as.Date(index(first(bid_offer)))), "\tscrubbed", sum(sus_pect), "suspect bid-offer values\n")
  sus_pect
}  # end extreme_values


### identify suspect jump values in univariate xts price time series
jump_values <- function(price_data, vol_window=51, thresh_old=2) {

  stopifnot(
    (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      ("package:caTools" %in% search()) || require("caTools", quietly=TRUE)
  )

# calculate simple returns
  diff_prices <- diff(price_data)
  diff_prices[1, ] <- 0
  colnames(diff_prices) <- "diffs"
  diff_prices_fut <- lag(diff_prices, -1)
  diff_prices_fut[nrow(diff_prices_fut)] <- 0
  colnames(diff_prices_fut) <- "diff_prices_fut"

# calculate vo_lat as running quantile
  vo_lat <- runquantile(x=abs(as.vector(diff_prices)), k=vol_window, probs=0.9, endrule="constant", align="center")
  vo_lat <- xts(vo_lat, order.by=index(diff_prices))
  colnames(vo_lat) <- "volat"
# carry forward non-zero vo_lat values
  vo_lat[vo_lat==0] <- NA
  vo_lat[1] <- 1
  vo_lat <- na.locf(vo_lat)
#  vo_lat <- na.locf(vo_lat, fromLast=TRUE)
  
# find suspect values
# suspect if abs diffs greater than vo_lat, and if abs sum of diffs less than vo_lat
  sus_pect <- (
    (abs(diff_prices) > thresh_old*vo_lat) & 
      (abs(diff_prices_fut) > thresh_old*vo_lat) & 
      (abs(diff_prices+diff_prices_fut) < 2*thresh_old*vo_lat)
  )
  sus_pect[1] <- FALSE
  colnames(sus_pect) <- "suspect"
# cat("Parsing", deparse(substitute(taq_data)), "\n")
# cat("Parsing", strsplit(deparse(substitute(taq_data)), split="[.]")[[1]][4], "on date:", format(to_day), "\tscrubbed", sum(sus_pect), "suspect values\n")
  cat("date:", format(as.Date(index(first(price_data)))), "\tscrubbed", sum(sus_pect), "suspect jump values\n")
  sus_pect
}  # end jump_values



### scrub and aggregate a single day of TAQ data in xts format
# return mid price and volume
scrub_agg <- function(taq_data, vol_window=51, thresh_old=2) {

  stopifnot(
      (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      (("package:lubridate" %in% search()) || require("lubridate", quietly=TRUE))
  )

# convert time index to New_York
  index(taq_data) <- with_tz(index(taq_data), "America/New_York")
# subset data to NYSE trading hours
  taq_data <- taq_data['T09:30:00/T16:00:00', ]
# return NULL if no data
  if (nrow(taq_data)==0)  return(NULL)
  to_day <- as.Date(index(first(taq_data)))

# remove duplicate time stamps using duplicated
  taq_data <- taq_data[!duplicated(index(taq_data)), ]

# scrub quotes with suspect bid-offer spreads
  bid_offer <- taq_data[, 'Ask.Price'] - taq_data[, 'Bid.Price']
#  bid_offer <- na.omit(bid_offer)
  sus_pect <- extreme_values(bid_offer)
# remove suspect values
  taq_data <- taq_data[!sus_pect]
# replace suspect values
# taq_data[sus_pect, "Bid.Price"] <- taq_data[sus_pect, "Trade.Price"]
# taq_data[sus_pect, "Ask.Price"] <- taq_data[sus_pect, "Trade.Price"]

# scrub quotes with suspect price jumps
# calculate mid prices
  mid_prices <- 0.5 * (taq_data[, "Bid.Price"] + taq_data[, "Ask.Price"])
#  mid_prices <- na.omit(mid_prices)
  colnames(mid_prices) <- "Mid.Price"
# replace suspect values with NA
  mid_prices[jump_values(mid_prices)] <- NA
  mid_prices <- na.locf(mid_prices)
#  mid_prices <- na.locf(mid_prices, fromLast=TRUE)
  mid_prices <- cbind(mid_prices, taq_data[index(mid_prices), "Volume"])
  mid_prices[is.na(mid_prices[, "Volume"]), "Volume"] <- 0

# aggregate to OHLC minutes data and cumulative volume
  mid_prices <- to.period(x=mid_prices, period="minutes")
# round up times to next minute
  index(mid_prices) <- align.time(x=index(mid_prices), 60)
  mid_prices
}  # end scrub_agg



### scrub and return a single day of TAQ data
scrub_TAQ <- function(taq_data, vol_window=51, thresh_old=2) {

  stopifnot(
    (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      (("package:lubridate" %in% search()) || require("lubridate", quietly=TRUE))
  )

# convert time index to New_York
  index(taq_data) <- with_tz(index(taq_data), "America/New_York")
# subset data to NYSE trading hours
  taq_data <- taq_data['T09:30:00/T16:00:00', ]
# return NULL if no data
  if (nrow(taq_data)==0)  return(NULL)

# remove duplicate time stamps using duplicated
  taq_data <- taq_data[!duplicated(index(taq_data)), ]

# scrub quotes with suspect bid-offer spreads
  bid_offer <- taq_data[, 'Ask.Price'] - taq_data[, 'Bid.Price']
#  bid_offer <- na.omit(bid_offer)
  sus_pect <- extreme_values(bid_offer)
# remove suspect values
  taq_data <- taq_data[!sus_pect]
# replace suspect values
# taq_data[sus_pect, "Bid.Price"] <- taq_data[sus_pect, "Trade.Price"]
# taq_data[sus_pect, "Ask.Price"] <- taq_data[sus_pect, "Trade.Price"]

# scrub quotes with suspect price jumps
# calculate mid prices
  mid_prices <- 0.5 * (taq_data[, "Bid.Price"] + taq_data[, "Ask.Price"])
#  mid_prices <- na.omit(mid_prices)
  colnames(mid_prices) <- "Mid.Price"
# replace suspect values with NA
  mid_prices[jump_values(mid_prices)] <- NA
  mid_prices <- na.locf(mid_prices)
#  mid_prices <- na.locf(mid_prices, fromLast=TRUE)
  mid_prices <- cbind(mid_prices, taq_data[index(mid_prices), "Volume"])
  mid_prices[is.na(mid_prices[, "Volume"]), "Volume"] <- 0

# aggregate to OHLC minutes data and cumulative volume
  mid_prices <- to.period(x=mid_prices, period="minutes")
# round up times to next minute
  index(mid_prices) <- align.time(x=index(mid_prices), 60)
  mid_prices
}  # end scrub_TAQ


daily_scrub <- scrub_agg(taq_data=daily_xts[[3]])
chartSeries(daily_scrub, name=sym_bol, theme=chartTheme("white"))

# daily_xts <- NULL
# sapply(head(file_names), function(file_name) {
#   cat("loading", file_name, "\n")
#   data_name <- load(file_name)
#   daily_xts <<- rbind(daily_xts, get(data_name))
#   data_name
# })


###########
# subset ts data

# subset time to trading hours using function from highfrequency - takes very long!!!
# system.time(subset.SPY <- exchangeHoursOnly(SPY['2014-05-08/2014-05-16', ]))
# subset time to trading hours
subset.SPY <- SPY['T09:30:00/T16:00:00', ]  # takes very long!!!
# subset time to trading hours + 15 min pre/after market
subset.SPY <- SPY['T09:15:00/T16:15:00', ]  # takes very long!!!
plot(subset.SPY['2014-05-12/2014-05-16', 'Bid.Price'])  # takes very long!!!
# chart_Series is faster without ON gaps
chart_Series(subset.SPY['2014-05-12/2014-05-16', 'Bid.Price'], name=sym_bol)

save(SPY, subset.SPY, file="SPY.RData")

load(file="SPY.RData")  # big file
load(file="SPY_daily.RData")  # small file



###########
# analyze daily ts data

# load one day of TAQ data
load(file.path(data_dir, "SPY/2014.05.02.SPY.RData"))
head(SPY)

# extract one day of TAQ data
daily_prices <- subset.SPY['2010-04-14', ]
daily_prices <- na.omit(daily_prices)  # omits too many bars?

# calculate seconds mid bid-offer prices (evaluated at trade times)
# mid_prices <- 0.5 * (daily_prices[, 'Bid.Price'] + daily_prices[, 'Ask.Price'])
# mid_prices <- daily_prices['T13:00/T17:00', 'Trade.Price']
# calculate seconds traded prices
# mid_prices <- daily_prices[, c('Volume', 'Trade.Price')]
# mid_prices <- daily_prices[, 'Trade.Price']
# merge prices with volume data
# vo_latume <- daily_prices['T13:00/T17:00', 'Volume']
# vo_latume <- daily_prices[, 'Volume']
# vo_latume[is.na(vo_latume)] <- 0
# mid_prices <- cbind(mid_prices, vo_latume)
# colnames(mid_prices) <- c('trade_price', 'volume')

# get one day of data, after loading data into list:
daily_prices <- (daily_xts[[6]])['T09:30:00/T16:00:00', ]
# calculate mid bid-offer prices
mid_prices <- 0.5 * (daily_prices[, 'Bid.Price'] + daily_prices[, 'Ask.Price'])
mid_prices <- na.omit(mid_prices)
colnames(mid_prices) <- "Mid.Price"


### create test data

# create xts time series
x_ts <- xts(x=rnorm(100), order.by=(Sys.time()-3600*(1:100)))
# split time series into daily list
list_xts <- split(x_ts, "days")
# rbind the list back into a time series and compare with the original
identical(x_ts, do_call_rbind(list_xts))

# create time index of one second intervals for a single day
in_dex <- seq(from=as.POSIXct("2015-02-09 09:30:00"), to=as.POSIXct("2015-02-09 16:00:00"), by="1 sec")
# create xts of random prices
x_ts <- xts(cumsum(rnorm(length(in_dex))), order.by=in_dex)
# create vector of random bid-offer prices
bid_offer <- abs(rnorm(length(in_dex)))/10
# create TAQ data using cbind
taq_data <- cbind(x_ts-bid_offer, x_ts+bid_offer)
# add Trade.Price
taq_data <- cbind(taq_data, x_ts+rnorm(length(in_dex))/10)
# add Volume
taq_data <- cbind(taq_data, sample(x=10*(2:18), size=length(in_dex), replace=TRUE))
colnames(taq_data) <- c("Bid.Price", "Ask.Price", "Trade.Price", "Volume")
# aggregate to one minute OHLC data
ohlc_data <- scrub_agg(taq_data)
chartSeries(ohlc_data, name=sym_bol, theme=chartTheme("white"))



# mid_prices <- xts(cumsum(rnorm(nrow(mid_prices))), order.by=index(daily_prices))
# sine function with jumps
mid_prices <- xts(sin(22*(1:nrow(daily_prices))/nrow(daily_prices)), order.by=index(daily_prices)) + 2
# mid_prices <- sin(22*(1:dim(daily_prices)[1])/dim(daily_prices)[1])
# prices_scrub <- mid_prices
colnames(mid_prices) <- "Mid.Price"

# add noise
mid_prices[c(1000,3000,5000,7000)] <- 1.1*mid_prices[c(1000,3000,5000,7000)]
mid_prices[c(2000,4000,6000,8000)] <- 0.8*mid_prices[c(1000,3000,5000,7000)]
# diff_prices <- xts(diff_prices, order.by=index(daily_xts[[6]])[10001:20000])
plot(mid_prices)


# median filter
test.blob <- xts(runmed(x=coredata(mid_prices), 11), order.by=index(mid_prices))
plot(mid_prices, xlab="", ylab="", type='l')
lines(test.blob, col='red', lwd=1)


# calculate simple returns
diff_prices <- diff(mid_prices)
diff_prices[1, ] <- 0
colnames(diff_prices) <- "diffs"
diff_prices_fut <- lag(diff_prices, -1)
diff_prices_fut[nrow(diff_prices_fut)] <- 0
colnames(diff_prices_fut) <- "diff_prices_fut"

# calculate log returns
# daily_returns <- diff(log(mid_prices))/c(1, diff(.index(mid_prices)))
daily_returns <- diff(mid_prices)/c(1, diff(.index(mid_prices)))
daily_returns[1, ] <- 0


### scrub data from single jump outliers, if two consecutive returns exceed threshold

# calculate the (symmetric) running average absolute deviation
vol_window <- 51
# abs_returns <- abs(as.vector(daily_returns))  # vector dispatches faster code
# vo_lat <- runMean(abs_returns, n=vol_window)
# vo_lat[1:(vol_window/2), ] <- vo_lat[(vol_window/2+1), ]
# system.time(vo_lat <- filter(abs_returns, filter=rep(1/vol_window,vol_window), sides=2))
# daily_mean <- runmean(x=as.vector(daily_returns), k=vol_window, alg="fast", endrule="constant", align="center")
# vo_lat <- runmean(x=abs(as.vector(daily_returns)-daily_mean), k=vol_window, alg="fast", endrule="constant", align="center")
# vo_lat <- runmean(x=abs(as.vector(diff_prices)), k=vol_window, alg="fast", endrule="constant", align="center")
# calculate vo_lat as running quantile
# vo_lat <- runmad(x=as.vector(diff_prices), k=vol_window, endrule="constant", align="center")
vo_lat <- runquantile(x=abs(as.vector(diff_prices)), k=vol_window, probs=0.9, endrule="constant", align="center")
vo_lat <- xts(vo_lat, order.by=index(diff_prices))
colnames(vo_lat) <- "volat"
# carry forward non-zero vo_lat values
vo_lat[vo_lat==0] <- NA
vo_lat <- na.locf(vo_lat)
plot(vo_lat, xlab="", ylab="", type='l')


# scrub the data
# lag_daily_returns <- c(daily_returns[-1], tail(daily_returns, 1))
# lag_daily_returns <- lag(daily_returns)
# lag_daily_returns[1,] <- 0.0

# find suspect values
thresh_old <- 1
# suspect if sum of abs diffs greater than abs sum of diffs - doesn't work
# sus_pect <- (abs(diff_prices) + abs(diff_prices_fut)) > thresh_old*abs(diff_prices+diff_prices_fut)
# suspect if abs diffs greater than vo_lat, and if abs sum of diffs less than vo_lat
sus_pect <- (
  (abs(diff_prices) > thresh_old*vo_lat) & 
    (abs(diff_prices_fut) > thresh_old*vo_lat) & 
    (abs(diff_prices+diff_prices_fut) < 2*thresh_old*vo_lat)
)
colnames(sus_pect) <- "suspect"
sum(sus_pect)
plot(sus_pect, xlab="", ylab="", type='l')


# replace suspect values with NA
# prices_scrub <- mid_prices
prices_scrub <- mid_prices
prices_scrub[sus_pect] <- NA
prices_scrub <- na.locf(prices_scrub)

# plot
plot(prices_scrub, xlab="", ylab="", type='l')
lines(prices_scrub, col='red', lwd=1)

# calculate scrubbed returns
returns_scrub <- diff(prices_scrub)/c(1, diff(.index(prices_scrub)))
returns_scrub[1,] <- 0.0

# mid_prices <- xts(raw.data[,2], order.by=as.POSIXlt(raw.data[,1]))

# End scrub


### plot scrub data

# inspect scrubbing
test.blob <- cbind(mid_prices, diff_prices, diff_prices_fut, vo_lat, sus_pect, prices_scrub)
colnames(test.blob) <- c("mid_prices", "diff_prices", "diff_prices_fut", "vo_lat", "sus_pect", "prices_scrub")
test.blob[995:1005]
plot.zoo(test.blob[7990:8010])



### data aggregation using to.period

chartSeries(prices_scrub, theme=chartTheme("white"))

# bind prices_scrub with volume data and remove NA volumes with zeros
prices_scrub <- cbind(prices_scrub, daily_prices[index(prices_scrub), "Volume"])
prices_scrub[is.na(prices_scrub[, "Volume"]), "Volume"] <- 0
head(prices_scrub)

# aggregate to OHLC minutes data and cumulative volume
prices_scrub <- to.period(x=prices_scrub, period="minutes")
head(prices_scrub)
chartSeries(prices_scrub, theme=chartTheme("white"))


### data aggregation using custom code

# calculate minutes traded prices
# end_points <- endpoints(mid_prices, "minutes")
# calculate traded prices every few bars
# mid_prices <- daily_prices[(1:round(dim(daily_prices)[1]/10)), c('Volume', 'Trade.Price')]

# calculate aggregation index
agg_price_window <- 10  # number of periods per aggregation
num_agg <- trunc(dim(daily_prices)[1]/agg_price_window)  # number of aggregations
# min end_points
# end_points <- dim(daily_prices)[1] - (agg_price_window*(num_agg+1) - 1) + agg_price_window*(1:num_agg)
# max end_points
# end_points <- dim(daily_prices)[1] - agg_price_window*num_agg + agg_price_window*(1:num_agg)
# range of end_points
agg_range <- dim(daily_prices)[1] - (agg_price_window*(num_agg+1) - 1):(agg_price_window*num_agg)

# calculate aggregated traded prices
# mid_prices <- daily_prices[end_points, c('Volume', 'Trade.Price')]
# colnames(mid_prices) <- c('trade_price', 'volume')
mid_prices <- daily_prices[end_points, 'Trade.Price']
colnames(mid_prices) <- 'trade_price'

# calculate aggregated returns given aggregation index start point
agg_returns <- function(agg_start) {
  mid_prices <- prices_scrub[(agg_start + agg_price_window*(1:num_agg)), ]
  daily_returns <- diff(mid_prices)/c(1, diff(.index(mid_prices)))
  daily_returns[1, ] <- 0
  daily_agg_returns <<- rbind(daily_agg_returns, daily_returns)
  agg_start
  #  coredata(daily_returns)
}  # end agg_returns


daily_agg_returns <- NULL
agg_out <- sapply(agg_range, agg_returns)
colnames(daily_agg_returns) <- 'returns'
daily_agg_returns <- sqrt(agg_price_window)*daily_agg_returns


# calculate stddev, skewness, and quantiles
sd(x=coredata(daily_agg_returns))
skewness(x=coredata(daily_agg_returns))
quantile(x=daily_agg_returns, probs=c(0.05, 0.95))
quantile(x=daily_agg_returns, probs=c(0.1, 0.9))


# plot histograms of daily returns
hist(daily_agg_returns, breaks=200, main="returns", xlab="", ylab="", freq=FALSE)
lines(density(daily_agg_returns), col='red', lwd=1)  # draw density

hist(returns_scrub, breaks=200, main="returns", xlab="", ylab="", freq=FALSE)
lines(density(returns_scrub), col='red', lwd=1)  # draw density

hist(returns_scrub, breaks=300, main="returns", xlab="", ylab="", xlim=c(-0.05, 0.05), freq=FALSE)
lines(density(returns_scrub), col='red', lwd=1)  # draw density

hist(daily_returns, breaks=100, main="returns", xlim=c(-2.0e-4, 2.0e-4), ylim=c(0, 10000), xlab="", ylab="", freq=FALSE)
lines(density(daily_returns), col='red', lwd=1)  # draw density

# title(main=ch.title, line=-1)  # add title


# get hourly volumes
end_points <- endpoints(mid_prices, "hours")
period.apply(mid_prices[, 'volume'], INDEX=endpoints(mid_prices, "hours"), sum)


pnls <- period.apply(mid_prices[, 'volume'], INDEX=endpoints(mid_prices, "minutes"), sum)

# get index of dates



###########
# load and scrub multiple days of data for a single symbol
save_OHLC <- function(sym_bol) {

# create path to directory with *.RData files
  file_dir <- file.path(data_dir, sym_bol)
# get list of *.RData files
  file_list <- list.files(file_dir)
# create paths to *.RData files
  file_names <- file.path(file_dir, file_list)

# load data into list
  data <- sapply(file_names, function(file_name) {
    cat("loading", sym_bol, "frome file: ", file_name, "\n")
    data_name <- load(file_name)
    get(data_name)
  })

# scrub and aggregate the data
  data <- sapply(data, scrub_agg)

# recursively "rbind" the list into a single xts
  data <- do_call_rbind(data)

  colnames(data) <- sapply(strsplit(colnames(data), split="[.]"), 
                           function(strng) paste(sym_bol, strng[-1], sep="."))

  assign(sym_bol, data)

  save(list=eval(sym_bol), file=paste0(sym_bol, ".RData"))

}  # end save_OHLC


### extra legacy code

# load second data for single day - works
load("E:/mktdata/sec/ESM9/2009.04.02.ESM9.rdata")
dim(ESM9)
len_es <- dim(ESM9)[1]
# inspect
ESM9[(len_es-10010):(len_es-10000), ]
# plot
plot(ESM9[((len_es-20000):(len_es-10000)), "Bid.Price"])


# tics data works
getSymbols("ZSK5")

# doesn't work
getSymbols("MSFT", verbose=FALSE, dir=data_dir, src="rda")
file  MSFT.rda  does not exist  in  E:/mktdata/ ....skipping
[1] "MSFT"

# doesn't work
getSymbols("ESH3")
NULL
Warning message:
  In getSymbols.FI(Symbols="ESH3", env=<environment>, verbose=FALSE,  :
                    No data found.

# doesn't work
getSymbols("ZQZ9")
Error in FUN(1L[[1L]], ...) : 
  must define instrument first to call with 'use_identifier'
In addition: Warning message:
  In getInstrument(Symbols[[i]], silent=FALSE) :
  instrument ZQZ9 not found, please create it first.
> getSymbols("QZ9")

# doesn't work
getSymbols("ZSK7")
NULL
Warning message:
  In getSymbols.FI(Symbols="ZSK7", env=<environment>, verbose=FALSE,  :
                    No data found.

###

# get data from multiple .Rdata files
getSymbols.FI <- function (symbols_list, date_from="2010-01-01", to=Sys.Date(), ..., 
          dir="", return_class="xts", extension="rda", split_method=c("days", "common"), 
          use_identifier=NA, date_format=NULL, verbose=TRUE, 
          days_to_omit=c("Saturday", "Sunday"), indexTZ=NA) {

# looks redundant
  if (is.null(date_format)) 
    date_format <- "%Y.%m.%d"
# what's this?
  if (is.null(days_to_omit)) 
    days_to_omit <- "NULL"

# copy named dots arguments into function environment - not sure why this is needed
  this_env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this_env)
  }

# recursive "rbind" function for list arguments - same as do.call.rbind
  do_call_rbind <- function(list_var) {
# call lapply in a loop to divide list_var by half, binding neighboring elements
    while (length(list_var) > 1) {
# index of odd list elements
      odd_index <- seq(from=1, to=length(list_var), by=2)
# bind neighboring elements and divide list_var by half
      list_var <- lapply(odd_index, function(in_dex) {
        if (in_dex==length(list_var)) {
          return(list_var[[in_dex]])
        }
        return(rbind(list_var[[in_dex]], list_var[[in_dex+1]]))
      })  # end lapply
    }  # end while
# list_var has only one element - return it
  list_var[[1]]
  }  # end do_call_rbind

# assign input argument values to hidden '.*' variables
# the variables hasArg.* are used in pickArg()
if (hasArg.date_from <- hasArg(date_from)) 
    .date_from <- date_from
  if (hasArg.to <- hasArg(to)) 
    .to <- to
  if (hasArg.dir <- hasArg(dir)) 
    .dir <- dir
  if (hasArg.return_class <- hasArg(return_class)) 
    .return_class <- return_class
  if (hasArg.extension <- hasArg(extension)) 
    .extension <- extension
  if (hasArg.split_method <- hasArg(split_method)) 
    .split_method <- split_method
  if (hasArg.use_identifier <- hasArg(use_identifier)) 
    .use_identifier <- use_identifier
  if (hasArg.date_format <- hasArg(date_format)) 
    .date_format <- date_format
  if (hasArg.verbose <- hasArg(verbose)) 
    .verbose <- verbose
  if (hasArg.days_to_omit <- hasArg(days_to_omit)) 
    .days_to_omit <- days_to_omit
  if (hasArg.indexTZ <- hasArg(indexTZ)) 
    .indexTZ <- indexTZ

  importDefaults("getSymbols.FI")

# assign default.* values to those passed through argument list of getSymbols.FI
# the default.* variables are used in pickArg()
# the default.* and '.*' variables are duplicates of the same arguments
  default.date_from <- date_from
  default.to <- to
  default.dir <- dir
  default.return_class <- return_class
  default.extension <- extension
  default.split_method <- split_method[1]
  default.use_identifier <- use_identifier
  default.date_format <- date_format
  default.verbose <- verbose
  default.days_to_omit <- days_to_omit
  default.indexTZ <- indexTZ
# end unused variables

  auto.assign <- if (hasArg(auto.assign)) {
    auto.assign
  } else {
    TRUE
  }

  env <- if (hasArg(env)) {
    env
  }
  else .GlobalEnv

# get default load parameters for all Symbols
  symbol_lookup <- getSymbolLookup()

# get default load parameters for symbol
  pickArg <- function(x, symbol_name) {
# check if symbol_name was passed as argument, and get '.*' value
    if (get(paste("hasArg", x, sep="."))) {
      get(paste(".", x, sep=""))
    }
# get value that was set using setSymbolLookup
    else if (!is.null(symbol_lookup[[symbol_name]][[x]])) {
      symbol_lookup[[symbol_name]][[x]]
    }
# get 'default.*' value
    else get(paste("default", x, sep="."))
  }  # end pickArg

  fr <- NULL  # "fr" is never used

### huge lapply over list of symbols
  datl <- lapply(1:length(symbols_list),  # load data for list of symbols
                  function(symbol_index) {  # load data for single symbol
                    symbol_i <- symbols_list[[symbol_index]]

# get default load parameters for symbol_i
                  from <- pickArg("from", symbol_i)
                  to <- pickArg("to", symbol_i)
                  dir <- pickArg("dir", symbol_i)
                  return_class <- pickArg("return_class", symbol_i)
                  file_extension <- pickArg("extension", symbol_i)
                  split_method <- pickArg("split_method", symbol_i)
                  use_identifier <- pickArg("use_identifier", symbol_i)
                  date_format <- pickArg("date_format", symbol_i)
                  verbose <- pickArg("verbose", symbol_i)
                  days_to_omit <- pickArg("days_to_omit", symbol_i)
                  indexTZ <- pickArg("indexTZ", symbol_i)

# if use_identifier is set, then extract identifier from symbol
                  instr_str <- NA
                  if (!is.na(use_identifier)) {
                    tmp_instr <- try(getInstrument(symbol_i, silent=FALSE))
                    if (inherits(tmp_instr, "try-error") || !is.instrument(tmp_instr)) 
                        stop("must define instrument first to call with 'use_identifier'")
                    if (!use_identifier=="primary_id") {
                        instr_str <- make.names(tmp_instr$identifiers[[use_identifier]])
                    }
                    else instr_str <- make.names(tmp_instr[[use_identifier]])
                    if (length(instr_str)==0L) 
                        stop("Could not find instrument. Try with use_identifier=NA")
                  }  # end if

# assign symbol name from either identifier or symbol
                  symbol_name <- ifelse(is.na(instr_str), make.names(symbol_i), instr_str)

# drop last "/" from dir
                  ndc <- nchar(dir)
                  if (substr(dir, ndc, ndc)=="/")
                    dir <- substr(dir, 1, ndc - 1)
# add symbol_name to dir
                  ssd <- strsplit(dir, "/")[[1]]
                  if (identical(character(0), ssd) || 
                         (!identical(character(0), ssd) && ssd[length(ssd)] != symbol_name))
                    dir <- paste(dir, symbol_name, sep="/")

# load data for single symbol from its directory
                  if (!dir=="" && !file.exists(dir)) {
                    if (verbose)
                       cat("\ndirectory ", dir, " does not exist, skipping\n")
                  } else {
                    if (verbose)
                       cat("loading ", symbol_i, ".....\n")

### start switch - either "days" or "common" 
                    switch(split_method[1],
                      days={  # load from daily files
# create vector of dates and file names
                            StartDate <- as.Date(from)
                            EndDate <- as.Date(to)
                            vec_dates <- as.Date(StartDate:EndDate)
                            vec_dates <- vec_dates[!weekdays(vec_dates) %in% days_to_omit]
                            vec_dates <- format(vec_dates, format=date_format)
                            vec_file_names <- paste(vec_dates, symbol_name, file_extension, sep=".")
                            if (dir!="") vec_file_names <- file.path(dir, vec_file_names)

# loop over file names and load data
                            data_list <- lapply(vec_file_names, function(file_name_full) {
                              file_name <- strsplit(file_name_full, "/")[[1]]
                              file_name <- file_name[length(file_name)]
                              if (verbose) cat("Reading ", file_name, "...")
                              if (!file.exists(file_name_full)) {
                                if (verbose) cat(" failed. File not found in ", dir, " ... skipping\n")
                              } else {
                                data_name <- load(file_name_full)  # load invisibly and get character string of object names
                                data_object <- get(data_name)  # get value of named object
                                if (!is.na(indexTZ) && !is.null(data_object)) indexTZ(data_object) <- indexTZ
                                if (verbose) cat(" done.\n")
                                data_object  # return data from loop
                              }  # end if
                            }  # end anon function
                            )  # end lapply

                            if (verbose) cat("rbinding data ... ")
                            data_complete <- do_call_rbind(data_list)
                            },  # end days

                            common={
                              file_name <- paste(symbol_name, file_extension, sep=".")
                              if (dir != "") file_name <- file.path(dir, file_name)
                              if (!file.exists(file_name)) {
                                if (verbose) cat("file ", paste(symbol_name, file_extension, sep="."), " does not exist in ", dir, "....skipping\n")
                              } else {
                                data_name <- load(file_name)
                                data_object <- get(data_name)
                                if (!is.na(indexTZ) && !is.null(data_object)) indexTZ(data_object) <- indexTZ
                                assign("data_complete", data_object)
                                if (verbose) cat("done.\n")
                              }
                            }  # end common

                        )  # end switch

                        data_complete <- quantmod:::convert.time.series(data_complete=data_complete, return_class=return_class)
                        symbol_i <- make.names(symbol_i)
                        data_out <- list()
                        data_out[[symbol_i]] <- data_complete
                        if (verbose) 
                           cat("done.\n")
                        data_out
                       }  # end load data for single symbol
                 }  # end anon function for loading single symbol

  )  # end lapply over list of symbols


  if (length(Filter("+", lapply(datl, length)))==0) {
    warning("No data found.")
    return(NULL)
  }

  datl.names <- do.call(c, lapply(datl, names))
  missing <- symbols_list[!symbols_list %in% datl.names]
  if (length(missing) > 0) 
    warning("No data found for ", paste(missing, collapse=" "))
  if (auto.assign) {
    out <- Filter(function(x) length(x) > 0, datl)
    invisible(lapply(out, function(x) assign(names(x), x[[1]], pos=env)))
    return(datl.names)
  }
  else {
    out <- lapply(datl, function(x) {
      if (length(x) > 0) 
        x[[1]]
    })
    if (length(out)==1) 
      return(out[[1]])
    else {
      names(out) <- symbols_list
      return(out)
    }
  }
}

<environment: namespace:FinancialInstrument>


