##############
# Backtests for fund in July 2020

# Load packages
library(HighFreq)

# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")


# Load the OHLC prices of S&P500 stocks
load(file="C:/Develop/lecture_slides/data/sp500.RData")

# Define variables
symbolv <- get("symbolv", sp500env)
symbol <- "AAPL"
look_back <- 5
lagg <- 1
coeff <- 1
threshold <- 0.0

ohlc <- get(symbol, sp500env)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)



## Calculate the strategy performance for a single stock
pnls <- backtest_ewma_ts(get(symbol, sp500env), look_back=look_back, lagg=lagg, threshold=threshold, coeff=coeff)


## Calculate the strategy performance for a vector of look_back parameters
perfstats <- lapply(3:5, backtest_ewma_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
pnls <- lapply(perfstats, function(x) x[, "pnls"])
pnls <- do.call(cbind, pnls)
pnls <- rowMeans(pnls)
mean(pnls)/sd(pnls)
pnls <- xts::xts(pnls, index(ohlc))
# Plot it
dygraphs::dygraph(cumsum(pnls), main=paste("Back-test of", symbol, "Strategies"))
# Plot it with closep
datav <- cbind(closep, cumsum(pnls))
colnamev <- c(symbol, "Strategy")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main=paste(colnamev[1], "Strategy")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")



## Rank the S&P500 stocks based on strategy returns for a vector of look_back parameters
perfstats <- eapply(sp500env, function(ohlc) {
  if (start(ohlc) < "2017-01-01") {
    pnls <- lapply(3:5, backtest_ewma_ts, ohlc=ohlc["2010/"], lagg=lagg, threshold=threshold, coeff=coeff)
    pnls <- lapply(pnls, function(x) x[, "pnls"])
    pnls <- do.call(cbind, pnls)
    pnls <- rowMeans(pnls)
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply

perfstats <- unlist(perfstats)
# names(perfstats) <- names(sp500env)
perfstats <- sort(perfstats, decreasing=TRUE)
write.csv(perfstats, file="C:/Develop/jp2sig/data/backtest_ewma2.csv", row.names=TRUE)



## Calculate the strategy performance for all S&P500 stocks and copy into environment
perf_env <- new.env()
process_ed <- eapply(sp500env, function(ohlc) {
  symbol <- rutils::get_name(colnames(ohlc)[1])
  # cat(symbol, "\n")
  assign(x=symbol, 
         value=backtest_ewma_ts(ohlc, look_back=look_back, lagg=lagg, threshold=threshold, coeff=coeff), 
         envir=perf_env)
  symbol
})  # end eapply

save(perf_env, file="C:/Develop/jp2sig/data/perf_ewma_trend_lback5.RData")

load("C:/Develop/jp2sig/data/perf_ewma_trend_lback5.RData")


## Rank the S&P500 stocks based on strategy returns
perfstats <- sapply(perf_env, function(xtes) {
  if (start(xtes) < "2017-01-01") {
    pnls <- xtes["2010/" ,"pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply
perfstats <- unlist(perfstats)
# names(perfstats) <- names(sp500env)
perfstats <- sort(perfstats, decreasing=TRUE)
write.csv(perfstats, file="C:/Develop/jp2sig/data/backtest_ewma.csv", row.names=TRUE)



## Rank the S&P500 stocks based on strategy returns in-sample
# perf_env is an environment with time series of performance
perfstats <- eapply(perf_env, function(xtes) {
  if (start(xtes) < "2010-01-01") {
    pnls <- xtes["2010/2017" ,"pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply
perfstats <- unlist(perfstats)
perfstats <- sort(perfstats, decreasing=TRUE)
symbolv <- names(perfstats)

# Select symbolv with final stock price above $1
not_penny <- eapply(sp500env, function(ohlc) {
  ohlc[NROW(ohlc), 4] > 1
})  # end eapply
not_penny <- unlist(not_penny)
not_penny <- not_penny[not_penny]
not_penny <- names(not_penny)
symbolv <- symbolv[symbolv %in% not_penny]

# Calculate the strategy returns out-of-sample
# as a function of the number of stocks selected
calc_perf <- function(nums) {
  # Calculate the returns of the best performing stocks
  be_st <- lapply(symbolv[1:nums], function(symbol) {
    get(symbol, perf_env)[, "pnls"]
  })  # end lapply
  be_st <- rutils::do_call(cbind, be_st)
  # Calculate the returns of the worst performing stocks
  wo_rst <- lapply(symbolv[(NROW(symbolv)-nums+1):NROW(symbolv)], function(symbol) {
    get(symbol, perf_env)[, "pnls"]
  })  # end lapply
  wo_rst <- rutils::do_call(cbind, wo_rst)
  wo_rst <- (-wo_rst)
  pnls <- cbind(be_st, wo_rst)
  pnls[1, is.na(pnls[1, ])] <- 0
  pnls <- zoo::na.locf(pnls, na.rm=FALSE)
  pnls <- pnls["2017/"]
  mean(pnls)/sd(pnls)
}  # end calc_perf

# Calculate the profile of strategy returns as a function of the number of stocks selected
profilev <- sapply(10*(1:10), calc_perf)
plot(x=10*(1:10), y=profilev, t="l", main="Sharpe of Back-test EWMA Strategies", 
     xlab="Select number", ylab="Sharpe")


## Calculate the out-of-sample strategy returns for the best and worst performing stocks
# Calculate the returns of the best performing stocks
nums <- 80
be_st <- lapply(symbolv[1:nums], function(symbol) {
  get(symbol, perf_env)[, "pnls"]
})  # end lapply
be_st <- rutils::do_call(cbind, be_st)
# Calculate the returns of the worst performing stocks
wo_rst <- lapply(symbolv[(NROW(symbolv)-nums+1):NROW(symbolv)], function(symbol) {
  get(symbol, perf_env)[, "pnls"]
})  # end lapply
wo_rst <- rutils::do_call(cbind, wo_rst)
wo_rst <- (-wo_rst)
pnls <- cbind(be_st, wo_rst)
pnls[1, is.na(pnls[1, ])] <- 0
pnls <- zoo::na.locf(pnls, na.rm=FALSE)
pnls <- xts::xts(rowMeans(pnls), index(pnls))
pnls <- cumsum(pnls)
dygraphs::dygraph(pnls, main="Back-test of EWMA strategies")



## Save the strategy positions

nums <- 80
# Calculate the positions of the best performing stocks
be_st <- lapply(symbolv[1:nums], function(symbol) {
  position_s <- get(symbol, perf_env)["2018-01-01/2020-04-22" ,"positions"]
  indeks <- paste(format(index(position_s)), "21:00:00")
  position_s <- cbind(indeks, rep(symbol, NROW(position_s)), as.character(position_s))
  colnames(position_s) <- c("time", "TICKER", "position_dollars")
  write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply
# Calculate the positions of the worst performing stocks
wo_rst <- lapply(symbolv[(NROW(symbolv)-nums+1):NROW(symbolv)], function(symbol) {
  position_s <- get(symbol, perf_env)["2018-01-01/2020-04-22" ,"positions"]
  indeks <- paste(format(index(position_s)), "21:00:00")
  position_s <- (-position_s)
  position_s <- cbind(indeks, rep(symbol, NROW(position_s)), as.character(position_s))
  colnames(position_s) <- c("time", "TICKER", "position_dollars")
  write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply




## Z-scores strategies using backtest_zscores_ts()

# Define variables
data_env <- rutils::etfenv
symbolv <- get("symbolv", data_env)
symbol <- "XLK"
look_back <- 5
lagg <- 1
coeff <- (-1)

ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)

## Calculate the strategy performance for two vectors of look_back parameters
# This model works well more recently
threshold <- 1.5
perfstats <- lapply(13:16, backtest_zscores_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
# This model also worked well in 2008
threshold <- 1.0
perfstats <- c(perfstats,
                lapply(7:15, backtest_zscores_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff))
save(perfstats, file="C:/Develop/jp2sig/data/perf_zscores_revert_xlk.RData")
load("C:/Develop/jp2sig/data/perf_zscores_revert_xlk.RData")
# Calculate the strategy returns
pnls <- lapply(perfstats, function(x) x[, "pnls"])
pnls <- do.call(cbind, pnls)
pnls <- rowMeans(pnls)
mean(pnls)/sd(pnls)
pnls <- xts::xts(pnls, index(ohlc))
# Plot it
dygraphs::dygraph(cumsum(pnls), main=paste("Back-test of", symbol, "Strategies"))
plot(cumsum(pnls), main=paste("Back-test of", symbol, "Strategies"))
# Plot it with closep
datav <- cbind(closep, cumsum(pnls))
colnamev <- c(symbol, "Strategy")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main=paste(colnamev[1], "Strategy")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")



## Save the strategy positions

# Calculate holding periods number of trades
peri_od <- sapply(perfstats, function(xtes) {
  position_s <- xtes[, "positions"]
  2*NROW(position_s) / sum(abs(rutils::diffit(position_s)))
})  # end sapply
mean(peri_od)


# Save the strategy positions
position_s <- lapply(perfstats, function(xtes) {
  xtes["2018-01-01/2020-04-22", "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
indeks <- paste(format(index(position_s)), "21:00:00")
position_s <- rowSums(position_s)
position_s <- cbind(indeks, rep(symbol, NROW(position_s)), as.character(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)


## Trending strategy using backtest_ewma_ts()

# Define variables
symbol <- "VXX"
lagg <- 2
threshold <- 0
coeff <- 1

ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)

## Calculate the strategy performance for two vectors of look_back parameters
perfstats <- lapply(3:5, backtest_ewma_ts, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
save(perfstats, file="C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
load("C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
# Calculate the strategy returns
pnls <- lapply(perfstats, function(x) x[, "pnls"])
pnls <- do.call(cbind, pnls)
pnls <- rowMeans(pnls)
mean(pnls)/sd(pnls)
pnls <- xts::xts(pnls, index(ohlc))
# Plot it
dygraphs::dygraph(cumsum(pnls), main=paste("Back-test of", symbol, "Strategies"))
plot(cumsum(pnls), main=paste("Back-test of", symbol, "Strategies"))
# Plot it with closep
datav <- cbind(closep, cumsum(pnls))
colnamev <- c(symbol, "Strategy")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main=paste(colnamev[1], "Strategy")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")


## Save the strategy positions

# Calculate holding periods number of trades
peri_od <- sapply(perfstats, function(xtes) {
  position_s <- xtes[, "positions"]
  2*NROW(position_s) / sum(abs(rutils::diffit(position_s)))
})  # end sapply
mean(peri_od)


# Save the strategy positions
position_s <- lapply(perfstats, function(xtes) {
  xtes["2018-01-01/2020-04-22", "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
indeks <- paste(format(index(position_s)), "21:00:00")
position_s <- rowSums(position_s)
position_s <- cbind(indeks, rep(symbol, NROW(position_s)), as.character(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)



## Read the strategy positions back

# Read the strategy positions back
position_s <- read.csv(file="C:/Develop/jp2sig/data/positions.csv", stringsAsFactors=FALSE)
# Split time series into daily list
position_s <- split(position_s, position_s$TICKER)
# cbind the list back into a time series and compare with the original
position_s <- lapply(position_s, function(datav) {
  # Extract dates index
  symbol <- datav[1, "TICKER"]
  indeks <- datav[, "time"]
  indeks <- lubridate::ymd_hms(indeks)
  indeks <- as.Date(indeks)
  # Format into xts series
  datav <- xts::xts(datav[, "position_dollars"], indeks)
  colnames(datav) <- symbol
  datav
})  # end lapply

position_s <- rutils::do_call(cbind, position_s)
position_s$VXX[is.na(position_s$VXX)] <- 0
sum(is.na(position_s))
indeks <- index(position_s)
symbols_strategy <- colnames(position_s)

# Select ETF returns
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[symbolv %in% symbols_strategy]
returns <- rutils::etfenv$returns[, symbolv]
returns <- returns[indeks]
returns[is.na(returns)] <- 0
sum(is.na(returns))

# Select S&P500 returns
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

# Combine ETF returns with S&P500 returns
returns <- cbind(returns, sp500_returns)
colnames(returns) <- rutils::get_name(colnames(returns))
sum(is.na(returns))
returns <- returns[, symbols_strategy]

# Calculate the strategy returns
returns_strategy <- returns*position_s
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, indeks)
# Plot it
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))




##############
## Old stuff

## Loop over selected sp500 stocks
sp500env <- sp500env
symbolv <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")

pnls <- lapply(symbolv, function(symbol) {
  backtest_ewma_ts(get(symbol, sp500env), look_back=look_back, lagg=lagg, coeff=coeff)[, "pnls"]
})  # end lapply

pnls <- rutils::do_call(cbind, pnls)
# Copy over NA values with zeros
pnls[1, is.na(pnls[1, ])] <- 0
pnls <- zoo::na.locf(pnls, na.rm=FALSE)
sum(is.na(pnls))
pnls <- xts::xts(rowMeans(pnls), index(pnls))
pnls <- cumsum(pnls)
x11()
chart_Series(x=pnls, name="Back-test of EWMA strategies")
dygraphs::dygraph(pnls, main="Back-test of EWMA strategies")


# Loop over sp500 stocks and get position_s
sp500env <- sp500env
symbolv <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")

position_s <- lapply(symbolv, function(symbol) {
  backtest_ewma_ts(get(symbol, sp500env), look_back=look_back, lagg=lagg, coeff=coeff)[, "positions"]
})  # end lapply

position_s <- rutils::do_call(cbind, position_s)
names(position_s) <- symbolv
position_s[1, ] <- 0
position_s <- na.locf(position_s, na.rm=FALSE)
zoo::write.zoo(position_s, file="C:/Develop/jp2sig/data/positions_ewma.csv", sep=",", col.names=TRUE)


load(file="C:/Develop/lecture_slides/data/sp500_returns.RData")
returns <- returns[, symbolv]
returns[1, is.na(returns[1, ])] <- 0
returns <- na.locf(returns, na.rm=FALSE)
pnls <- xts::xts(cumsum(rowMeans(position_s*returns)), index(returns))



## Backtest of Z-Score crossover strategies using the code in app_zscore.R

# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")

# Load S&P500 prices
load(file="C:/Develop/lecture_slides/data/sp500.RData")
look_back <- 10
lagg <- 1
coeff <- (-1)
threshold <- 1

pnls <- backtest_zscores_ts(sp500env$YUM["2010/"], look_back=look_back, lagg=lagg, threshold=threshold, coeff=coeff)
pnls <- pnls[, "pnls"]
position_s <-  pnls[, "positions"]
plot(cumsum(pnls))
mean(pnls)/sd(pnls)


# Calculate past pnls from performance environment
pnls <- eapply(perfstats, function(xtes) {
  if (start(xtes) < "2017-01-01") {
    pnls <- xtes["2010/2017" ,"pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply

pnls <- unlist(pnls)
pnls <- sort(pnls, decreasing=TRUE)
symbolv <- names(pnls)

# Calculate pnls of best stocks from environment
be_st <- lapply(symbolv[1:50], function(symbol) {
  get(symbol, perfstats)[, "pnls"]
})  # end lapply
names(be_st) <- symbolv[1:50]
be_st <- rutils::do_call(cbind, be_st)

# Calculate reverse of pnls of worst stocks
wo_rst <- lapply(symbolv[(NROW(symbolv)-49):NROW(symbolv)], function(symbol) {
  get(symbol, perfstats)[, "pnls"]
})  # end lapply
wo_rst <- rutils::do_call(cbind, wo_rst)
wo_rst <- (-wo_rst)

# Combine everything and plot
pnls <- cbind(be_st, wo_rst)
pnls[1, is.na(pnls[1, ])] <- 0
pnls <- zoo::na.locf(pnls, na.rm=FALSE)
sum(is.na(pnls))
pnls <- xts::xts(rowMeans(pnls), index(pnls))
pnls <- cumsum(pnls)
dygraphs::dygraph(pnls, main="Back-test of Reverting Strategies")



# Combine everything and plot
pnls <- cbind(pnls, wo_rst)
pnls[1, is.na(pnls[1, ])] <- 0
pnls <- zoo::na.locf(pnls, na.rm=FALSE)
sum(is.na(pnls))
pnls <- xts::xts(rowMeans(pnls), index(pnls))
pnls <- cumsum(pnls)
dygraphs::dygraph(pnls, main="Back-test of Reverting Strategies")



## Calculate pnls directly
pnls <- eapply(sp500env, function(ohlc) {
  if (start(ohlc) < "2017-01-01") {
    pnls <- backtest_zscores_ts(ohlc["2010/"], look_back=look_back, lagg=lagg, threshold=threshold, coeff=coeff)
    pnls <- pnls[, "pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply

pnls <- unlist(pnls)
pnls <- sort(pnls, decreasing=TRUE)
write.csv(pnls, file="C:/Develop/jp2sig/data/backtest_zscores.csv", row.names=TRUE)


## Loop over all sp500 stocks using several parameters
load(file="C:/Develop/lecture_slides/data/sp500.RData")
symbolv <- names(sp500env)
symbol <- "AAPL"
look_back <- 5
lagg <- 1
coeff <- (-1)
threshold <- 1

pnls <- eapply(sp500env, function(ohlc) {
  if (start(ohlc) < "2017-01-01") {
    pnls <- lapply(2*(5:15), backtest_zscores_ts, ohlc=ohlc["2010/"], lagg=lagg, threshold=threshold, coeff=coeff)
    pnls <- lapply(pnls, function(x) x[, "pnls"])
    pnls <- do.call(cbind, pnls)
    pnls <- rowMeans(pnls)
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply

pnls <- unlist(pnls)
# names(pnls) <- names(sp500env)
pnls <- sort(pnls, decreasing=TRUE)
write.csv(pnls, file="C:/Develop/jp2sig/data/backtest_zscores_revert.csv", row.names=TRUE)



## Combine S&P500 symbols

symbols_zscores_revert <- read.csv(file="C:/Develop/jp2sig/data/backtest_zscores_revert.csv", stringsAsFactors=FALSE)
symbols_ewma_revert <- read.csv(file="C:/Develop/jp2sig/data/backtest_ewma_revert.csv", stringsAsFactors=FALSE)
symbols_ewma_revertr <- read.csv(file="C:/Develop/jp2sig/data/backtest_ewma_revertr.csv", stringsAsFactors=FALSE)

foo <- (symbols_zscores_revert$ticker %in% symbols_ewma_revert$ticker)

