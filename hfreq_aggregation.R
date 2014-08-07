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

prices_mid <- xts(sin(22*(1:dim(daily_prices)[1])/dim(daily_prices)[1]), order.by=index(daily_prices))
# prices_mid <- sin(22*(1:dim(daily_prices)[1])/dim(daily_prices)[1])
prices_mid[c(1000,3000,5000,7000)] <- 1
prices_mid[c(2000,4000,6000,8000)] <- -1
colnames(prices_mid) <- 'trade_price'
# prices_scrub <- prices_mid
plot(prices_mid, xlab="", ylab="", type='l')


# calculate log returns
# daily_returns <- diff(log(prices_mid[, 'trade_price']))/c(1, diff(.index(prices_mid)))
daily_returns <- diff(prices_mid[, 'trade_price'])/c(1, diff(.index(prices_mid)))
# daily_returns <- diff(prices_mid[, 'trade_price'])
# daily_returns <- diff(prices_mid)
daily_returns[1, ] <- 0


### scrub data from single jump outliers, if two consecutive returns exceed threshold

# calculate the (symmetric) running average absolute deviation
agg_vol_window <- 51
daily_diffs <- diff(prices_mid[, 'trade_price'])
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


# scrub the data
# lag_daily_returns <- c(daily_returns[-1], tail(daily_returns, 1))
# lag_daily_returns <- lag(daily_returns)
# lag_daily_returns[1,] <- 0.0

# find suspect values
# daily_suspect <- ((abs(daily_returns)>2*daily_vol) & (abs(daily_returns+lag(daily_returns, -1))<2*daily_vol))
daily_suspect <- ((abs(daily_diffs)>daily_vol) & (abs(daily_diffs+lag(daily_diffs, -1))<daily_vol))
# daily_suspect <- ((abs(daily_returns)>0.5*daily_vol) & (abs(daily_returns+lag(daily_returns, -1))<0.5*daily_vol))
daily_suspect[dim(daily_suspect)[1]] <- FALSE
plot(daily_suspect, xlab="", ylab="", type='l')
sum(daily_suspect)

# replace suspect values with NA
# prices_scrub <- prices_mid
prices_scrub <- prices_mid[, 'trade_price']
prices_scrub[daily_suspect] <- NA
prices_scrub <- na.locf(prices_scrub)

# plot
plot(prices_scrub, xlab="", ylab="", type='l')
plot(prices_mid[, 'trade_price'], xlab="", ylab="", type='l')
lines(prices_scrub, col='red', lwd=1)

# calculate scrubbed returns
returns_scrub <- diff(prices_scrub[, 'trade_price'])/c(1, diff(.index(prices_scrub)))
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
end_points <- dim(daily_prices)[1] - (agg_price_window*(num_agg+1) - 1) + agg_price_window*(1:num_agg)
# max end_points
end_points <- dim(daily_prices)[1] - agg_price_window*num_agg + agg_price_window*(1:num_agg)
# range of end_points
agg_range <- dim(daily_prices)[1] - (agg_price_window*(num_agg+1) - 1):(agg_price_window*num_agg)

# calculate aggregated traded prices
# prices_mid <- daily_prices[end_points, c('Volume', 'Trade.Price')]
# colnames(prices_mid) <- c('trade_price', 'volume')
prices_mid <- daily_prices[end_points, 'Trade.Price']
colnames(prices_mid) <- 'trade_price'

daily_agg_returns <- NULL
# calculate aggregated returns given aggregation index start point
agg_returns <- function(agg_start) {
  prices_mid <- prices_scrub[(agg_start + agg_price_window*(1:num_agg)), ]
  daily_returns <- diff(prices_mid)/c(1, diff(.index(prices_mid)))
  daily_returns[1, ] <- 0
  daily_agg_returns <<- rbind(daily_agg_returns, daily_returns)
  agg_start
  #  coredata(daily_returns)
}  # end agg_returns

agg_out <- sapply(agg_range, agg_returns)
colnames(daily_agg_returns) <- 'returns'
daily_agg_returns <- sqrt(agg_price_window)*daily_agg_returns


# plot histograms of daily returns
hist(daily_agg_returns, breaks=200, main="returns", xlab="", ylab="", freq=FALSE)
lines(density(daily_returns), col='red', lwd=1)  # draw density

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
getSymbols.FI <- function (Symbols, from="2010-01-01", to=Sys.Date(), ..., 
          dir="", return.class="xts", extension="rda", split_method=c("days", 
                                                                              "common"), use_identifier=NA, date_format=NULL, verbose=TRUE, 
          days_to_omit=c("Saturday", "Sunday"), indexTZ=NA)
{
  if (is.null(date_format)) 
    date_format <- "%Y.%m.%d"
  if (is.null(days_to_omit)) 
    days_to_omit <- "NULL"
  this.env <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], this.env)
  }
  do.call.rbind <- function(lst) {
    while (length(lst) > 1) {
      idxlst <- seq(from=1, to=length(lst), by=2)
      lst <- lapply(idxlst, function(i) {
        if (i == length(lst)) {
          return(lst[[i]])
        }
        return(rbind(lst[[i]], lst[[i + 1]]))
      })
    }
    lst[[1]]
  }
  if (hasArg.from <- hasArg(from)) 
    .from <- from
  if (hasArg.to <- hasArg(to)) 
    .to <- to
  if (hasArg.dir <- hasArg(dir)) 
    .dir <- dir
  if (hasArg.return.class <- hasArg(return.class)) 
    .return.class <- return.class
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
  default.from <- from
  default.to <- to
  default.dir <- dir
  default.return.class <- return.class
  default.extension <- extension
  default.split_method <- split_method[1]
  default.use_identifier <- use_identifier
  default.date_format <- date_format
  default.verbose <- verbose
  default.days_to_omit <- days_to_omit
  default.indexTZ <- indexTZ
  auto.assign <- if (hasArg(auto.assign)) {
    auto.assign
  }
  else TRUE
  env <- if (hasArg(env)) {
    env
  }
  else .GlobalEnv
  pickArg <- function(x, Symbol) {
    if (get(paste("hasArg", x, sep="."))) {
      get(paste(".", x, sep=""))
    }
    else if (!is.null(SymbolLookup[[Symbol]][[x]])) {
      SymbolLookup[[Symbol]][[x]]
    }
    else get(paste("default", x, sep="."))
  }
  SymbolLookup <- getSymbolLookup()
  fr <- NULL
  datl <- lapply(1:length(Symbols), function(i) {
    from <- pickArg("from", Symbols[[i]])
    to <- pickArg("to", Symbols[[i]])
    dir <- pickArg("dir", Symbols[[i]])
    return.class <- pickArg("return.class", Symbols[[i]])
    extension <- pickArg("extension", Symbols[[i]])
    split_method <- pickArg("split_method", Symbols[[i]])
    use_identifier <- pickArg("use_identifier", Symbols[[i]])
    date_format <- pickArg("date_format", Symbols[[i]])
    verbose <- pickArg("verbose", Symbols[[i]])
    days_to_omit <- pickArg("days_to_omit", Symbols[[i]])
    indexTZ <- pickArg("indexTZ", Symbols[[i]])
    instr_str <- NA
    if (!is.na(use_identifier)) {
      tmp_instr <- try(getInstrument(Symbols[[i]], silent=FALSE))
      if (inherits(tmp_instr, "try-error") || !is.instrument(tmp_instr)) 
        stop("must define instrument first to call with 'use_identifier'")
      if (!use_identifier == "primary_id") {
        instr_str <- make.names(tmp_instr$identifiers[[use_identifier]])
      }
      else instr_str <- make.names(tmp_instr[[use_identifier]])
      if (length(instr_str) == 0L) 
        stop("Could not find instrument. Try with use_identifier=NA")
    }
    Symbol <- ifelse(is.na(instr_str), make.names(Symbols[[i]]), 
                     instr_str)
    ndc <- nchar(dir)
    if (substr(dir, ndc, ndc) == "/") 
      dir <- substr(dir, 1, ndc - 1)
    ssd <- strsplit(dir, "/")[[1]]
    if (identical(character(0), ssd) || (!identical(character(0), 
                                                    ssd) && ssd[length(ssd)] != Symbol)) 
      dir <- paste(dir, Symbol, sep="/")
    if (!dir == "" && !file.exists(dir)) {
      if (verbose) 
        cat("\ndirectory ", dir, " does not exist, skipping\n")
    }
    else {
      if (verbose) 
        cat("loading ", Symbols[[i]], ".....\n")
      switch(split_method[1], days={
        StartDate <- as.Date(from)
        EndDate <- as.Date(to)
        date.vec <- as.Date(StartDate:EndDate)
        date.vec <- date.vec[!weekdays(date.vec) %in% 
                               days_to_omit]
        date.vec <- format(date.vec, format=date_format)
        sym.files <- paste(date.vec, Symbol, extension, 
                           sep=".")
        if (dir != "") sym.files <- file.path(dir, sym.files)
        dl <- lapply(sym.files, function(fp) {
          sf <- strsplit(fp, "/")[[1]]
          sf <- sf[length(sf)]
          if (verbose) cat("Reading ", sf, "...")
          if (!file.exists(fp)) {
            if (verbose) cat(" failed. File not found in ", 
                             dir, " ... skipping\n")
          } else {
            if (verbose) cat(" done.\n")
            local.name <- load(fp)
            dat <- get(local.name)
            if (!is.na(indexTZ) && !is.null(dat)) indexTZ(dat) <- indexTZ
            dat
          }
        })
        if (verbose) cat("rbinding data ... ")
        fr <- do.call.rbind(dl)
      }, common=, {
        sym.file <- paste(Symbol, extension, sep=".")
        if (dir != "") sym.file <- file.path(dir, sym.file)
        if (!file.exists(sym.file)) {
          if (verbose) cat("file ", paste(Symbol, extension, 
                                          sep="."), " does not exist in ", dir, "....skipping\n")
        } else {
          local.name <- load(sym.file)
          dat <- get(local.name)
          if (!is.na(indexTZ) && !is.null(dat)) indexTZ(dat) <- indexTZ
          assign("fr", dat)
          if (verbose) cat("done.\n")
        }
      })
      fr <- quantmod:::convert.time.series(fr=fr, return.class=return.class)
      Symbols[[i]] <- make.names(Symbols[[i]])
      tmp <- list()
      tmp[[Symbols[[i]]]] <- fr
      if (verbose) 
        cat("done.\n")
      tmp
    }
  })
  if (length(Filter("+", lapply(datl, length))) == 0) {
    warning("No data found.")
    return(NULL)
  }
  datl.names <- do.call(c, lapply(datl, names))
  missing <- Symbols[!Symbols %in% datl.names]
  if (length(missing) > 0) 
    warning("No data found for ", paste(missing, collapse=" "))
  if (auto.assign) {
    out <- Filter(function(x) length(x) > 0, datl)
    invisible(lapply(out, function(x) assign(names(x), x[[1]], 
                                             pos=env)))
    return(datl.names)
  }
  else {
    out <- lapply(datl, function(x) {
      if (length(x) > 0) 
        x[[1]]
    })
    if (length(out) == 1) 
      return(out[[1]])
    else {
      names(out) <- Symbols
      return(out)
    }
  }
}
<environment: namespace:FinancialInstrument>


