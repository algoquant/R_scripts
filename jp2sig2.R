rm(list = ls())
# Load packages
library(HighFreq)
# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")
# Load the OHLC prices of S&P500 stocks
load(file="C:/Develop/lecture_slides/data/sp500.RData")
data_env <- rutils::etfenv
symbolv <- get("symbolv", data_env)
lagg <- 2
threshold <- 0
coeff <- 1

symbol <- "VXX"
ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
# perfstats <- lapply(3:5, backtest_ewma_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
# save(perfstats, file="C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
load("C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
posit <- lapply(perfstats, function(xtes) {
  xtes[, "positions"]
})  # end lapply
posit <- do.call(cbind, posit)
# indeks <- paste(format(index(posit)), "21:00:00")
# indeks <- as.POSIXct(index(posit), tz="UTC")
# Calculate timestamps
indeks <- index(posit)
# Get date from previous business day
indeks <- rutils::lagit(indeks)
# Add time stamp to date equal to last minute before closing
indeks <- paste(indeks, "15:59:00")
# Coerce to date-time
indeks <- as.POSIXct(indeks, tz="America/New_York")
# Coenvert to UTC time zone
indeks <- lubridate::with_tz(indeks, "UTC")
indeks <- format(indeks)
# posit <- rowSums(posit)
posit <- xts::xts(cbind(indeks, rep(symbol, NROW(posit)), rowSums(posit)), index(posit))
colnames(posit) <- c("time", "TICKER", "position_dollars")
posit <- posit["2018-01-01/2020-04-22", ]
posit <- posit[-1, ]  # because VXX starts after 2018-01-01, so lag adds extra day up front
write.table(posit, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

symbol <- "SVXY"
ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
# perfstats <- lapply(4:15, backtest_ewma_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
load("C:/Develop/jp2sig/data/perf_ewma_trend_svxy.RData")
posit <- lapply(perfstats, function(xtes) {
  xtes[, "positions"]
})  # end lapply
posit <- do.call(cbind, posit)
# indeks <- paste(format(index(posit)), "21:00:00")
# indeks <- as.POSIXct(index(posit), tz="UTC")
# Calculate timestamps
indeks <- index(posit)
# Get date from previous business day
indeks <- rutils::lagit(indeks)
# Add time stamp to date equal to last minute before closing
indeks <- paste(indeks, "15:59:00")
# Coerce to date-time
indeks <- as.POSIXct(indeks, tz="America/New_York")
# Coenvert to UTC time zone
indeks <- lubridate::with_tz(indeks, "UTC")
indeks <- format(indeks)
# posit <- rowSums(posit)
posit <- xts::xts(cbind(indeks, rep(symbol, NROW(posit)), rowSums(posit)), index(posit))
colnames(posit) <- c("time", "TICKER", "position_dollars")
posit <- posit["2018-01-01/2020-04-22", ]
write.table(posit, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

symbol <- "DBC"
ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
# perfstats <- lapply(14:17, backtest_ewma_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
load("C:/Develop/jp2sig/data/perf_ewma_trend_dbc.RData")
posit <- lapply(perfstats, function(xtes) {
  xtes[, "positions"]
})  # end lapply
posit <- do.call(cbind, posit)
# indeks <- paste(format(index(posit)), "21:00:00")
# indeks <- as.POSIXct(index(posit), tz="UTC")
# Calculate timestamps
indeks <- index(posit)
# Get date from previous business day
indeks <- rutils::lagit(indeks)
# Add time stamp to date equal to last minute before closing
indeks <- paste(indeks, "15:59:00")
# Coerce to date-time
indeks <- as.POSIXct(indeks, tz="America/New_York")
# Coenvert to UTC time zone
indeks <- lubridate::with_tz(indeks, "UTC")
indeks <- format(indeks)
# posit <- rowSums(posit)
posit <- xts::xts(cbind(indeks, rep(symbol, NROW(posit)), rowSums(posit)), index(posit))
colnames(posit) <- c("time", "TICKER", "position_dollars")
posit <- posit["2018-01-01/2020-04-22", ]
write.table(posit, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

symbol <- "USO"
ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
# perfstats <- lapply(2:10, backtest_ewma_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
load("C:/Develop/jp2sig/data/perf_ewma_trend_uso.RData")
posit <- lapply(perfstats, function(xtes) {
  xtes[, "positions"]
})  # end lapply
posit <- do.call(cbind, posit)
# indeks <- paste(format(index(posit)), "21:00:00")
# indeks <- as.POSIXct(index(posit), tz="UTC")
# Calculate timestamps
indeks <- index(posit)
# Get date from previous business day
indeks <- rutils::lagit(indeks)
# Add time stamp to date equal to last minute before closing
indeks <- paste(indeks, "15:59:00")
# Coerce to date-time
indeks <- as.POSIXct(indeks, tz="America/New_York")
# Coenvert to UTC time zone
indeks <- lubridate::with_tz(indeks, "UTC")
indeks <- format(indeks)
# posit <- rowSums(posit)
posit <- xts::xts(cbind(indeks, rep(symbol, NROW(posit)), rowSums(posit)), index(posit))
colnames(posit) <- c("time", "TICKER", "position_dollars")
posit <- posit["2018-01-01/2020-04-22", ]
write.table(posit, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

symbol <- "XLK"
lagg <- 1
coeff <- (-1)
ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
# threshold <- 1.5
# perfstats <- lapply(13:16, backtest_zscores_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
# threshold <- 1.0
# perfstats <- c(perfstats,
#                 lapply(7:15, backtest_zscores_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff))
load("C:/Develop/jp2sig/data/perf_zscores_revert_xlk.RData")
posit <- lapply(perfstats, function(xtes) {
  xtes[, "positions"]
})  # end lapply
posit <- do.call(cbind, posit)
# indeks <- paste(format(index(posit)), "21:00:00")
# indeks <- as.POSIXct(index(posit), tz="UTC")
# Calculate timestamps
indeks <- index(posit)
# Get date from previous business day
indeks <- rutils::lagit(indeks)
# Add time stamp to date equal to last minute before closing
indeks <- paste(indeks, "15:59:00")
# Coerce to date-time
indeks <- as.POSIXct(indeks, tz="America/New_York")
# Coenvert to UTC time zone
indeks <- lubridate::with_tz(indeks, "UTC")
indeks <- format(indeks)
# posit <- rowSums(posit)
posit <- xts::xts(cbind(indeks, rep(symbol, NROW(posit)), rowSums(posit)), index(posit))
colnames(posit) <- c("time", "TICKER", "position_dollars")
posit <- posit["2018-01-01/2020-04-22", ]
write.table(posit, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

# S&P500 scripts
symbolv <- get("symbolv", sp500env)
symbol <- "ICE"
look_back <- 5
lagg <- 1
coeff <- 1
threshold <- 0.0
ohlc <- get(symbol, sp500env)
closep <- log(quantmod::Cl(ohlc))
load("C:/Develop/jp2sig/data/perf_ewma_trend_lback5.RData")

# perf_env <- new.env()
# process_ed <- eapply(sp500env, function(ohlc) {
#   symbol <- rutils::get_name(colnames(ohlc)[1])
#   assign(x=symbol,
#          value=backtest_ewma_ts(ohlc, look_back=look_back, lagg=lagg, threshold=threshold, coeff=coeff),
#          envir=perf_env)
#   symbol
# })  # end eapply

perfstats <- eapply(perf_env, function(xtes) {
  if (start(xtes) < "2010-01-01") {
    pnls <- xtes["2010/2017" ,"pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply
perfstats <- unlist(perfstats)
perfstats <- sort(perfstats, decreasing=TRUE)
symbolv <- names(perfstats)
not_penny <- eapply(sp500env, function(ohlc) {
  ohlc[NROW(ohlc), 4] > 1
})  # end eapply
not_penny <- unlist(not_penny)
not_penny <- not_penny[not_penny]
not_penny <- names(not_penny)
symbolv <- symbolv[symbolv %in% not_penny]

# Calculate the positions of the best performing stocks
# Triple the stock positions to balance risk with ETFs
nums <- 80
be_st <- lapply(symbolv[1:nums], function(symbol) {
  posit <- get(symbol, perf_env)[ ,"positions"]
  posit <- 3*posit
  # Calculate timestamps
  indeks <- index(posit)
  # Get date from previous business day
  indeks <- rutils::lagit(indeks)
  # Add time stamp to date equal to last minute before closing
  indeks <- paste(indeks, "15:59:00")
  # Coerce to date-time
  indeks <- as.POSIXct(indeks, tz="America/New_York")
  # Coenvert to UTC time zone
  indeks <- lubridate::with_tz(indeks, "UTC")
  indeks <- format(indeks)
  posit <- xts::xts(cbind(indeks, rep(symbol, NROW(posit)), rowSums(posit)), index(posit))
  colnames(posit) <- c("time", "TICKER", "position_dollars")
  posit <- posit["2018-01-01/2020-04-22", ]
  write.table(posit, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply
# Calculate the positions of the worst performing stocks
wo_rst <- lapply(symbolv[(NROW(symbolv)-nums+1):NROW(symbolv)], function(symbol) {
  posit <- get(symbol, perf_env)["2018-01-01/2020-04-22" ,"positions"]
  posit <- (-3*posit)
  # Calculate timestamps
  indeks <- index(posit)
  # Get date from previous business day
  indeks <- rutils::lagit(indeks)
  # Add time stamp to date equal to last minute before closing
  indeks <- paste(indeks, "15:59:00")
  # Coerce to date-time
  indeks <- as.POSIXct(indeks, tz="America/New_York")
  # Coenvert to UTC time zone
  indeks <- lubridate::with_tz(indeks, "UTC")
  indeks <- format(indeks)
  posit <- xts::xts(cbind(indeks, rep(symbol, NROW(posit)), rowSums(posit)), index(posit))
  colnames(posit) <- c("time", "TICKER", "position_dollars")
  posit <- posit["2018-01-01/2020-04-22", ]
  write.table(posit, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply


## Read the strategy positions back

posit <- read.csv(file="C:/Develop/jp2sig/data/positions.csv", stringsAsFactors=FALSE)
posit <- split(posit, posit$TICKER)
posit <- lapply(posit, function(datav) {
  # Extract dates index
  symbol <- datav[1, "TICKER"]
  # Calculate timestamps
  indeks <- datav[, "time"]
  # Coerce strings to date-times
  indeks <- lubridate::ymd_hms(indeks)
  # Coerce to date
  indeks <- as.Date(indeks)
  # Get date for next business day
  indeks <- rutils::lagit(indeks, -1)
  # Add to last date
  indeks[NROW(indeks)] <- indeks[NROW(indeks)] + 1
  # Format into xts series
  datav <- xts::xts(datav[, "position_dollars"], indeks)
  colnames(datav) <- symbol
  datav
})  # end lapply
posit <- rutils::do_call(cbind, posit)
posit$VXX[is.na(posit$VXX)] <- 0
sum(is.na(posit))
indeks <- index(posit)
symbols_strategy <- colnames(posit)
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[symbolv %in% symbols_strategy]
returns <- rutils::etfenv$returns[, symbolv]
returns <- returns[indeks]
returns[is.na(returns)] <- 0
sum(is.na(returns))
symbolv <- names(sp500env)
symbolv <- symbolv[symbolv %in% symbols_strategy]
sp500_returns <- lapply(symbolv, function(symbol) {
  ohlc <- get(symbol, sp500env)
  closep <- log(quantmod::Cl(ohlc))
  rutils::diffit(closep)
})  # end lapply
sp500_returns <- rutils::do_call(cbind, sp500_returns)
sp500_returns <- sp500_returns[indeks]
sum(is.na(sp500_returns))
symbols_strategy <- colnames(posit)
returns <- cbind(returns, sp500_returns)
colnames(returns) <- rutils::get_name(colnames(returns))
sum(is.na(returns))
returns <- returns[, symbols_strategy]
returns_strategy <- returns*posit
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, indeks)
# Plot it
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))


# Plot subsets
returns_strategy <- returns[, c("DBC", "USO", "VXX", "XLK")]*posit[, c("DBC", "USO", "VXX", "XLK")]
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, indeks)
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))
returns_strategy <- returns[, c("DBC", "USO", "SVXY", "VXX", "XLK")]*posit[, c("DBC", "USO", "SVXY", "VXX", "XLK")]
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, indeks)
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))
match(c("DBC", "USO", "SVXY", "VXX", "XLK"), colnames(posit))
foo <- match(c("DBC", "USO", "SVXY", "VXX", "XLK"), colnames(posit))
foo
returns_strategy <- returns[, -foo]*posit[, -foo]
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, indeks)
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))
