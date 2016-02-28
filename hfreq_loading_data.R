################################################
###  file contains scripts for high frequency data tasks:
###
###  1. loading, scrubbing, and aggregating data using package HighFreq, 
###
###  2. estimating variance, skewness, and kurtosis,
###
###  3. running simple trading strategies,
###
################################################

rm(list=ls())  # remove all objects

# load packages
library(quantmod)
library(qmao)
library(caTools)
library(highfrequency)
library(TTR)
library(HighFreq)

# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))

# set options
options(max.print=80)
options(digits=5)
options(digits.secs=6)
options(stringsAsFactors=FALSE)
# Set the time-zone to NYC
Sys.setenv(TZ="America/New_York")
# suppress spurious timezone warning messages
options(xts_check_TZ=FALSE)

# set data directories to data on external drive
setwd("C:/Develop/data")
# search()  # get search path
data_dir <- "E:/mktdata/sec/"
output_dir <- "E:/output/data/"

# set data directories to data on local drive
data_dir <- "C:/Develop/data/hfreq/src"
output_dir <- "C:/Develop/data/hfreq/scrub"

# define variable named "sym_bol" containing string with xts variable name
sym_bol <- "SPY"
# remove xts variable
rm(SPY)
# or
rm(list=sym_bol)


#################################
### loading and scrubbing data using package HighFreq

### load minutely OHLC data

sym_bol <- load("C:/Develop/data/SPY.RData")
# or
sym_bol <- load(
  file.path(output_dir, 
            paste0(sym_bol, ".RData")))

# explore "SPY" data
head(SPY, 11)
head(SPY[, c("SPY.High", "SPY.Low")], 11)

# plot single year
chart_Series(SPY["2012", ], name="SPY")
# plot single month
chart_Series(SPY["2013-04", ], name="SPY")
# plot single day
chart_Series(SPY["2013-07-08", ], name="SPY")
chart_Series(SPY["2012-05-08", "SPY.Open"], name="SPY")
# plot single hour
chart_Series(SPY["2013-07-08 12", ], name="SPY")
# plot range of days
chart_Series(SPY["2012-02-15/2012-02-16", ], name="SPY")


### load a single day of seconds TAQ data

sym_bol <- load("C:/Develop/data/hfreq/src/SPY/2012.02.16.SPY.RData")
sym_bol <- load("C:/Develop/data/hfreq/src/SPY/2013.04.16.SPY.RData")

# convert timezone of index to New_York
library(lubridate)
indexTZ(SPY)
index(SPY) <- with_tz(index(SPY), "America/New_York")

# subset TAQ data to NYSE trading hours using "T notation"
SPY <- SPY["T09:15:00/T16:15:00", ]
# explore "SPY" data
head(SPY[, c("Bid.Price", "Ask.Price")], 22)
head(SPY[, "Trade.Price"], 22)
SPY[11:44, "Bid.Price"]
SPY[11:44, "Ask.Price"]


### create single day of synthetic TAQ data with seconds time index

in_dex <- seq(from=as.POSIXct("2015-02-09 09:30:00"), to=as.POSIXct("2015-02-09 16:00:00"), by="1 sec")
len_index <- length(in_dex)
# create xts of random prices
taq_data <- cumsum(rnorm(len_index))
# create vector of random bid-offer spreads
bid_offer <- abs(rnorm(len_index))/10
# create TAQ data with "Bid.Price", "Ask.Price", "Trade.Price" using cbind
taq_data <- cbind(taq_data-bid_offer, 
                  taq_data+bid_offer, 
                  taq_data+bid_offer*runif(len_index, min=-1, max=1))
# add Volume
taq_data <- cbind(taq_data, sample(x=10*(2:18), size=len_index, replace=TRUE))
taq_data <- xts(taq_data, order.by=in_dex)
colnames(taq_data) <- c("Bid.Price", "Ask.Price", "Trade.Price", "Volume")
chart_Series(taq_data[, "Trade.Price"], name="TAQ data")
# aggregate to one minute OHLC data
ohlc_data <- scrub_agg(taq_data)
chart_Series(ohlc_data, name="OHLC data")


### create synthetic data with price jumps and scrub it

# create synthetic data as sine function
jump_prices <- xts(sin(22*(1:nrow(ohlc_data))/nrow(ohlc_data)), order.by=index(ohlc_data))
len_index <- length(index(ohlc_data))
# add price jumps
jump_index <- sample(x=len_index, size=6)
jump_prices[jump_index[1:3], ] <- jump_prices[jump_index[1:3], ] + 0.2
jump_prices[jump_index[4:6], ] <- jump_prices[jump_index[4:6], ] - 0.2
chart_Series(jump_prices, name="prices with jumps")
# remove jumps from prices using price_jumps()
jump_prices <- jump_prices[!price_jumps(jump_prices), ]
# or instead: replace suspect values with NA, and perform 'locf'
jump_prices[price_jumps(jump_prices), ] <- NA
jump_prices <- na.locf(jump_prices)
# or instead: replace suspect values with average
jump_indices <- which(price_jumps(jump_prices))
jump_before <- jump_indices - 1
jump_after <- jump_indices + 1
jump_before <- ifelse(jump_before<1, jump_after, jump_before)
jump_after <- ifelse(jump_after>len_index, jump_before, jump_after)
jump_prices[jump_indices, ] <- 0.5*(coredata(jump_prices)[jump_before] + coredata(jump_prices)[jump_after])
chart_Series(jump_prices, name="prices with scrubbed jumps")



### scrub and aggregate a single day of TAQ data to OHLC "by hand"




### scrub and aggregate a single day of TAQ data to OHLC using package HighFreq

library(HighFreq)
SPY <- scrub_agg(taq_data="SPY")
SPY <- scrub_agg(taq_data=get(sym_bol))
# or assign it to name contained in "sym_bol"
assign(x=sym_bol, value=scrub_agg(taq_data=get(sym_bol)))

# save single day of OHLC data
save(SPY, file="SPY.RData")


### load, scrub and aggregate a list of TAQ data to OHLC

# create list of *.RData files
file_list <- list("2012.02.13.SPY.RData", "2012.02.14.SPY.RData", "2012.02.15.SPY.RData", "2012.02.16.SPY.RData", "2012.02.17.SPY.RData")
# create paths to *.RData files
file_names <- file.path(data_dir, sym_bol, file_list)

# load TAQ data into list and assign it to name contained in "sym_bol"
assign(x=sym_bol, 
       value=lapply(file_names, 
                    function(file_name) {
                      cat("loading", sym_bol, "from file: ", file_name, "\n")
                      data_name <- load(file_name)
                      get(data_name)
                    })  # end lapply
       )  # end assign
# or
SPY <- lapply(file_names, 
              function(file_name) {
                cat("loading", sym_bol, "from file: ", file_name, "\n")
                data_name <- load(file_name)
                get(data_name)
              })  # end lapply

# scrub and aggregate the TAQ data
assign(x=sym_bol, value=lapply(get(sym_bol), scrub_agg))
# or
SPY <- lapply(get(sym_bol), scrub_agg)

# recursively "rbind" the list into a single xts
assign(x=sym_bol, value=do_call_rbind(get(sym_bol)))
# or
SPY <- do_call_rbind(get(sym_bol))
# using do.call() blows up or takes very long!!!
SPY <- do.call(rbind, get(sym_bol))

# assign column names, i.e. "symbol.rets"
colnames(SPY) <- sapply(strsplit(colnames(get(sym_bol)), split="[.]"), 
                        function(strng) paste(sym_bol, strng[-1], sep="."))
# or using get() - but doesn't work
colnames(get(sym_bol)) <- sapply(strsplit(colnames(get(sym_bol)), split="[.]"), 
                                 function(strng) paste(sym_bol, strng[-1], sep="."))
# or using assign() - but doesn't work
assign(x=colnames(SPY), 
       value=sapply(strsplit(colnames(get(sym_bol)), split="[.]"), 
                    function(strng) paste(sym_bol, strng[-1], sep=".")))
assign(x=colnames(get(sym_bol)), 
       value=sapply(strsplit(colnames(get(sym_bol)), split="[.]"), 
                    function(strng) paste(sym_bol, strng[-1], sep=".")))



