################################################
###
###  parsing and aggregating HFREQ data
###
################################################

rm(list=ls())  # remove all objects

library(quantstrat)
library(utils)
library(quantmod)
library(caTools)


Sys.setenv(TZ="UTC")  # Set the time-zone to GMT (UTC)
setwd("C:/Develop/data")
# search()  # get search path


### set data_source directory
# data_source <- "/home/storage/settles/"
# data_source <- "/home/storage/sec/"
data_source <- "E:/mktdata/sec/"
# data_source <- "/home/storage/tick/"
# print(data_source)

options(digits.secs=6)
options(stringsAsFactors=FALSE)



##################
### code for loading instruments data

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



##################
### extra code for parsing list.instruments - you can ignore

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



##################
### code for loading ts data

### set defaults for getSymbols
# setDefaults(getSymbols, verbose=FALSE, dir=data_source, src="rda")

# setDefaults for getSymbols: call getSymbols.FI by default
setDefaults(getSymbols, verbose=FALSE, src="FI")
# setDefaults for getSymbols.FI: load data from local drive
setDefaults(getSymbols.FI,
            extension="RData",
            dir=data_source,
            days_to_omit="Saturday",
            use_identifier="X.RIC")


### load seconds bar data

# run loadInstruments() first
getSymbols("AAPL")  # takes very long!!!
dim(AAPL)
AAPL[10000:10020, ]


# subset time to trading hours using function from highfrequency - takes very long!!!
# system.time(subset.AAPL <- exchangeHoursOnly(AAPL['2014-05-08/2014-05-16', ]))
# subset time to trading hours
subset.AAPL <- AAPL['T09:30:00/T16:00:00', ]  # takes very long!!!
# subset time to trading hours + 15 min pre/after market
subset.AAPL <- AAPL['T09:15:00/T16:15:00', ]  # takes very long!!!
plot(subset.AAPL['2014-05-12/2014-05-16', 'Bid.Price'])  # takes very long!!!
# chart_Series is faster without ON gaps
chart_Series(subset.AAPL['2014-05-12/2014-05-16', 'Bid.Price'], name="AAPL")

save(AAPL, subset.AAPL, file="AAPL.RData")

load(file="SPY.RData")  # big file
load(file="SPY_daily.RData")  # small file



##################
### code for analyzing daily ts data


# extract one day of ts data
daily_prices <- subset.SPY['2010-04-14', ]
daily_prices <- na.omit(daily_prices)  # omits too many bars?


# calculate seconds mid bid-offer prices (evaluated at trade times)
# prices_mid <- 0.5 * (daily_prices[, 'Bid.Price'] + daily_prices[, 'Ask.Price'])
# prices_mid <- daily_prices['T13:00/T17:00', 'Trade.Price']
# calculate seconds traded prices
# prices_mid <- daily_prices[, c('Volume', 'Trade.Price')]
prices_mid <- daily_prices[, 'Trade.Price']
# merge prices with volume data
# daily_volume <- daily_prices['T13:00/T17:00', 'Volume']
# daily_volume <- daily_prices[, 'Volume']
# daily_volume[is.na(daily_volume)] <- 0
# prices_mid <- cbind(prices_mid, daily_volume)
# colnames(prices_mid) <- c('trade_price', 'volume')
colnames(prices_mid) <- 'trade_price'


### create test data

# sine function with jumps
prices_mid <- xts(sin(22*(1:dim(daily_prices)[1])/dim(daily_prices)[1]), order.by=index(daily_prices))
# prices_mid <- sin(22*(1:dim(daily_prices)[1])/dim(daily_prices)[1])
prices_mid[c(1000,3000,5000,7000)] <- 1
prices_mid[c(2000,4000,6000,8000)] <- -1
colnames(prices_mid) <- 'trade_price'
# prices_scrub <- prices_mid
plot(prices_mid, xlab="", ylab="", type='l')

# median filter
test.blob <- xts(runmed(x=coredata(prices_mid), 11), order.by=index(prices_mid))
plot(prices_mid, xlab="", ylab="", type='l')
lines(test.blob, col='red', lwd=1)


# calculate log returns
# daily_returns <- diff(log(prices_mid))/c(1, diff(.index(prices_mid)))
daily_returns <- diff(prices_mid)/c(1, diff(.index(prices_mid)))
# daily_returns <- diff(prices_mid)
# daily_returns <- diff(prices_mid)
daily_returns[1, ] <- 0


### scrub data from single jump outliers, if two consecutive returns exceed threshold

# calculate the (symmetric) running average absolute deviation
agg_vol_window <- 51
daily_diffs <- diff(prices_mid)
daily_diffs[1, ] <- 0
# abs_returns <- abs(as.vector(daily_returns))  # vector dispatches faster code
# daily_vol <- runMean(abs_returns, n=agg_vol_window)
# daily_vol[1:(agg_vol_window/2), ] <- daily_vol[(agg_vol_window/2+1), ]
# system.time(daily_vol <- filter(abs_returns, filter=rep(1/agg_vol_window,agg_vol_window), sides=2))
# daily_mean <- runmean(x=as.vector(daily_returns), k=agg_vol_window, alg="fast", endrule="constant", align="center")
# daily_vol <- runmad(x=as.vector(daily_returns), k=agg_vol_window, endrule="constant", align="center")
# daily_vol <- runmad(x=as.vector(daily_diffs), k=agg_vol_window, endrule="constant", align="center")
# daily_vol <- runmad(x=as.vector(prices_mid), k=agg_vol_window, endrule="constant", align="center")
# daily_vol <- runmean(x=abs(as.vector(daily_returns)-daily_mean), k=agg_vol_window, alg="fast", endrule="constant", align="center")
daily_vol <- runmean(x=abs(as.vector(daily_diffs)), k=agg_vol_window, alg="fast", endrule="constant", align="center")
plot(daily_vol, xlab="", ylab="", type='l')
plot.zoo(cbind(abs(daily_diffs), daily_vol), xlab="", ylab="", type='l')


# scrub the data
# lag_daily_returns <- c(daily_returns[-1], tail(daily_returns, 1))
# lag_daily_returns <- lag(daily_returns)
# lag_daily_returns[1,] <- 0.0

# find suspect values
suspect_window <- 3
# daily_suspect <- ((abs(daily_returns)>suspect_window*daily_vol) & (abs(daily_returns+lag(daily_returns, -1))<suspect_window*daily_vol))
daily_suspect <- ((abs(daily_diffs)>suspect_window*daily_vol) & (abs(daily_diffs+lag(daily_diffs, -1))<suspect_window*daily_vol))
# daily_suspect <- ((abs(daily_returns)>0.5*daily_vol) & (abs(daily_returns+lag(daily_returns, -1))<0.5*daily_vol))
daily_suspect[dim(daily_suspect)[1]] <- FALSE
plot(daily_suspect, xlab="", ylab="", type='l')
sum(daily_suspect)

# replace suspect values with NA
# prices_scrub <- prices_mid
prices_scrub <- prices_mid
prices_scrub[daily_suspect] <- NA
prices_scrub <- na.locf(prices_scrub)

# plot
plot(prices_scrub, xlab="", ylab="", type='l')
plot(prices_mid, xlab="", ylab="", type='l')
lines(prices_scrub, col='red', lwd=1)

# calculate scrubbed returns
returns_scrub <- diff(prices_scrub)/c(1, diff(.index(prices_scrub)))
returns_scrub[1,] <- 0.0

# prices_mid <- xts(raw.data[,2], order.by=as.POSIXlt(raw.data[,1]))

# End scrub


### plot scrub data

test.blob <- cbind(prices_mid, daily_returns, daily_vol, abs(daily_diffs), abs(daily_diffs+lag(daily_diffs, -1)), daily_suspect)
colnames(test.blob) <- c('prices_mid', 'rets', 'vol', 'abs rets', 'rets + lag rets', 'suspect')
test.blob[7990:8010]
plot.zoo(test.blob[7990:8010])



### code for data aggregation

# calculate minutes traded prices
# end_points <- endpoints(prices_mid, "minutes")
# calculate traded prices every few bars
# prices_mid <- daily_prices[(1:round(dim(daily_prices)[1]/10)), c('Volume', 'Trade.Price')]

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
# prices_mid <- daily_prices[end_points, c('Volume', 'Trade.Price')]
# colnames(prices_mid) <- c('trade_price', 'volume')
prices_mid <- daily_prices[end_points, 'Trade.Price']
colnames(prices_mid) <- 'trade_price'

# calculate aggregated returns given aggregation index start point
agg_returns <- function(agg_start) {
  prices_mid <- prices_scrub[(agg_start + agg_price_window*(1:num_agg)), ]
  daily_returns <- diff(prices_mid)/c(1, diff(.index(prices_mid)))
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
end_points <- endpoints(prices_mid, "hours")
period.apply(prices_mid[, 'volume'], INDEX=endpoints(prices_mid, "hours"), sum)


pnls <- period.apply(prices_mid[, 'volume'], INDEX=endpoints(prices_mid, "minutes"), sum)

# get index of dates


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
getSymbols("MSFT", verbose=FALSE, dir=data_source, src="rda")
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

# not sure why this is needed
  this_env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this_env)
  }

# define 'rbind' function for list arguments
  rbind_list <- function(list_var) {
    while (length(list_var) > 1) {
      idx_list_var <- seq(from=1, to=length(list_var), by=2)
      list_var <- lapply(idx_list_var, function(i) {
        if (i==length(list_var)) {
          return(list_var[[i]])
        }
        return(rbind(list_var[[i]], list_var[[i+1]]))
      })  # end lapply
    }  # end while
    list_var[[1]]
  }  # end rbind_list

# assign input argument values to hidden '.*' variables
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

# the variables below aren't used anywhere ?
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

# get default load parameters for symbol
  pickArg <- function(x, symbol_name) {
    if (get(paste("hasArg", x, sep="."))) {
      get(paste(".", x, sep=""))
    }
    else if (!is.null(symbol_lookup[[symbol_name]][[x]])) {
      symbol_lookup[[symbol_name]][[x]]
    }
    else get(paste("default", x, sep="."))
  }  # end pickArg

# get default load parameters for all Symbols
  symbol_lookup <- getSymbolLookup()

  fr <- NULL  # this is never used
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
                   instr_str <- NA

                   # if use_identifier is set, then extract identifier from symbol
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
                   }

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
                     switch(split_method[1],
                            ### load from daily files
                            days={
                              # create vector of dates and file names
                              StartDate <- as.Date(from)
                              EndDate <- as.Date(to)
                              vec_dates <- as.Date(StartDate:EndDate)
                              vec_dates <- vec_dates[!weekdays(vec_dates) %in% days_to_omit]
                              vec_dates <- format(vec_dates, format=date_format)
                              vec_file_names <- paste(vec_dates, symbol_name, file_extension, sep=".")
                              if (dir != "") vec_file_names <- file.path(dir, vec_file_names)

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
                              data_complete <- rbind_list(data_list)
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


