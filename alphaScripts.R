################################################
###
### Experimental alpha scripts
### Load data
### explore skew, Hurst
### simple trading strategies
###
###
################################################

rm(list=ls())
options(max.print=80)

# Suppress spurious timezone warning messages
options(xts_check_TZ=FALSE)

library(HighFreq)
library(TTR)


########### start temp scripts ###########

# Summary: Create an AR(p) model for forecasting returns 
# from a "kitchen sink" design matrix.  Apply shrinkage to
# the design matrix and perform a backtest to find the 
# optimal shrinkage parameter.

# Strategy using rolling zscores over OHLC technical indicators

# Find the optimal meta-parameters (the length of look-back interval 
# look_back and the order of the AR(p) process p) by minimizing the 
# out-of-sample MSE.

## Calculate a "kitchen sink" design matrix called design

# Set up data from OHLC prices

library(HighFreq)

look_back <- 252

ohlc <- rutils::etfenv$VTI
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
returns <- rutils::diffit(log(closep))
colnames(returns) <- "returns"
volumes <- Vo(ohlc)
colnames(volume) <- "volume"


# Calculate various indicators

# wippp

variance <- HighFreq::roll_variance(ohlc=log(ohlc), look_back=look_back, scalit=FALSE)
colnames(variance) <- "variance"

score <- HighFreq::roll_zscores(response=close_num, design=design, look_back=look_back)
colnames(score) <- "score"
score[1:look_back] <- 0

roll_sharpe

# Residuals of the regression of the time series of closep prices
dates <- xts::.index(ohlc)
# foo <- unique(dates)
design <- matrix(dates, nc=1)
# foo <- MASS::ginv(design)
look_back <- 11
zscores <- HighFreq::roll_zscores(response=closep, 
                                   design=design, 
                                   look_back=look_back)
colnames(zscores) <- "zscores"
zscores[1:3] <- 0

moment_um <- ((closep-openp) - (highp-lowp)) + 1.0
colnames(moment_um) <- "moment_um"


lambda_s <- 0:31/30

weightv <- sapply(lambda_s, function(lambda) {
  optimd <- optim(par=rep(1.0, nweights),
                  fn=object_ive,
                  method="L-BFGS-B",
                  upper=rep(10, nweights),
                  lower=rep(-10, nweights),
                  excess=excess,
                  covmat=covmat,
                  lambda=lambda,
                  alpha=1)
  optimd$par/sum(optimd$par)
})  # end sapply





# Calculate the coefficients of the AR(11) model for volumes.
# You must perform a regression on lagged volumes using matrix 
# algebra, not arima().

# Define design matrix
design <- sapply(1:11, function(lagg) {
  rutils::lagit(volumes, lagg=lagg)
})  # end sapply
# generalized inverse of design matrix
design_inv <- MASS::ginv(design)
# Regression coefficients with response equal to arimav
coeff <- drop(design_inv %*% volumes)

# You should get the following output:
coeff
# [1] -0.544335000 -0.346797195 -0.393230034 -0.218591436
# [5] -0.199455627 -0.232981341 -0.130858453 -0.010743125
# [9]  0.099890590  0.032009602  0.003775291


# 2. (20pts) Create a function called back_test() which 
# backtests an AR(p) forecasting model on a vector of data.
# back_test() should calculate the out-of-sample forecasts 
# of the AR(p) forecasting model, and return their MSE.
# The function back_test() should accept the following 
# arguments:
#  tseries - a vector of data,
#  look_back - lookback interval,
#  ordern - order of the AR(p) model, 
#  endpoints = the end points: seq_along(tseries),

back_test <- function(tseries, look_back=100, ordern=5, endpoints=endpoints, nrows=nrows) {
  # Calculate aggregations.
  design <- sapply(1:ordern, function(lagg) {
    rutils::lagit(tseries, lagg=lagg)
  })  # end sapply
  design <- cbind(tseries, design)
  startpoints <- c(rep_len(1, look_back-1), 
                    endpoints[1:(NROW(endpoints)-look_back+1)])
  # Perform rolling forecasting
  forecastvs <- sapply(endpoints[-(1:(ordern-1))], function(it) {
    design <- design[startpoints[it]:endpoints[it], ]
    # Calculate AR(p) coefficients
    design_inv <- MASS::ginv(design[, -1])
    coeff <- drop(design_inv %*% design[, 1])
    design[(NROW(design)-ordern+1):NROW(design), 1] %*% coeff
  })  # end sapply
  forecastvs <- c(rep(forecastvs[1], ordern-1), forecastvs)
  # Lag the forecasts to push them out-of-sample
  forecastvs <- rutils::lagit(forecastvs)
  mean((forecastvs-tseries)^2)
}  # end back_test

# Run back_test() as follows:

endpoints <- seq_along(volumes)
nrows <- NROW(endpoints)

back_test(tseries=volumes, look_back=300, ordern=11, endpoints=endpoints)

# You should get the following output:
# [1] 1.73671


# 3. (20pts) Perform two separate sapply() loops over 
# back_test(), to determine the optimal values of ordern 
# and look_back.

# Create vector of moment orders for performing the sapply() loop
orderns <- 9:25

# First perform an sapply() loop over ordern=orderns, 
# and with look_back=200.

mse_s <- sapply(orderns, back_test, tseries=volumes, look_back=200, endpoints=endpoints)

# You should get the following output:
round(mse_s, 3)
#  [1] 1.725 1.629 1.777 1.592 1.787 1.772 1.847 1.586 1.689 1.753
# [11] 2.210 2.077 1.966 2.354 2.078 2.943 2.409

# Notice that the MSE increases with higher AR(p) order 
# because of model overfitting.

# Create a plot similar to backtest_ar_order.png

x11()
plot(x=orderns, y=mse_s, t="l", lwd=2, xlab="AR(p) Order", ylab="",
     main="MSE as Function of AR(p) Order")


# Second perform an sapply() loop over look_back=(50*(2:10)), 
# and with ordern=11.

# Create vector of look_backs for performing the sapply() loop
look_backs <- 50*(2:10)

mse_s <- sapply(look_backs, back_test, tseries=volumes, ordern=11, endpoints=endpoints)

# You should get the following output:
round(mse_s, 3)
# [1] 1.871 1.805 1.777 1.762 1.737 1.730 1.730 1.726 1.721

# Create a plot similar to backtest_ar_lookback.png

plot(x=look_backs, y=mse_s, t="l", lwd=2, xlab="Lookback length", ylab="",
     main="MSE as Function of AR(p) Lookback")




###########
# Perform aggregations by applying a function over a vector of endpoints

## Load minutely price data
symbol <- load("C:/Develop/data/SPY.RData")

# Plot average hourly trading volumes
prices <- Vo(SPY["2010-05-05/2010-05-07"])
var_running <- period.apply(
  x=prices,
  INDEX=xts::endpoints(prices, "hours"),
  sum)
chart_Series(volumes, name="hourly volumes")
indeks <- format(index(volumes), "%H:%M")
volumes <- tapply(X=volumes, INDEX=indeks, FUN=mean)
volumes <- xts(as.vector(volumes), order.by=as.POSIXct(names(volumes), format="%H:%M"))
# Normalize and plot volumes
indeks <- c(30, diff(index(volumes)))
chart_Series(volumes/indeks, name="hourly volumes")


## agg_regate() calculates an aggregation of an xts series
# and returns an xts series with a single row
agg_regate <- function(datav) {
  agg_regation <- c(max=max(datav), min=min(datav))
  xts(t(agg_regation), order.by=end(datav))
}  # end agg_regate
agg_regate(prices)


## Perform aggregations using period.apply(), apply_rolling() and apply_xts()
# apply_rolling() and apply_xts() are legacy functions from utilLib.R

# Extract closing prices for a single day of data
prices <- Cl(SPY["2010-05-06"])

endpoints <- xts::endpoints(prices, "hours")
agg_regations <- period.apply(x=prices,
                            INDEX=endpoints,
                            FUN=agg_regate)
foo_bar <- apply_rolling(xtes=prices,
                         endpoints=endpoints,
                         func=agg_regate)
agg_regations <- apply_xts(xtes=prices,
                         endpoints=endpoints,
                         func=agg_regate)

# Verify that apply_rolling() and apply_xts() produce identical output
all.equal(agg_regations, foo_bar)

# Compare speed of apply_rolling() versus apply_xts()
library(microbenchmark)
summary(microbenchmark(
  agg_sapply=apply_rolling(xtes=prices,
                           endpoints=endpoints,
                           func=agg_regate),
  agg_lapply=apply_xts(xtes=prices,
                                   endpoints=endpoints,
                                   func=agg_regate),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

agg_regations <- apply_rolling(xtes=prices,
                               endpoints=endpoints,
                               look_back=3,
                               func=agg_regate)
# Plot aggregations with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("red", "green")
chart_Series(agg_regations, theme=plot_theme,
             name="price aggregations")
legend("bottomright", legend=colnames(agg_regations),
       bg="white", lty=c(1, 1), lwd=c(2, 2),
       col=plot_theme$col$line.col, bty="n")



###########
### Plot histograms of returns data

## Calculate stddev, skewness, and quantiles of returns data

returns <- 86400*diff(Cl(SPY))/c(1, diff(.index(SPY)))
returns[1, ] <- 0
sum(is.na(returns))
returns <- na.locf(returns, na.rm=FALSE)

sd(x=coredata(returns))
# skewness() from package "moments"
skewness(x=coredata(returns))
quantile(x=returns, probs=c(0.05, 0.95))
quantile(x=returns, probs=c(0.1, 0.9))


# Plot histograms of daily returns
hist(returns, breaks=200, main="returns", xlab="", ylab="", freq=FALSE)
lines(density(returns), col="red", lwd=1)  # draw density

hist(returns, breaks=200, main="returns", xlab="", ylab="", freq=FALSE)
lines(density(returns), col="red", lwd=1)  # draw density

hist(returns, breaks=300, main="returns", xlab="", ylab="", xlim=c(-0.05, 0.05), freq=FALSE)
lines(density(returns), col="red", lwd=1)  # draw density

hist(daily_returns, breaks=100, main="returns", xlim=c(-2.0e-4, 2.0e-4), ylim=c(0, 10000), xlab="", ylab="", freq=FALSE)
lines(density(daily_returns), col="red", lwd=1)  # draw density

# title(main=ch.title, line=-1)  # Add title



hist(returns, breaks=400, main="", xlab="", ylab="", xlim=c(-0.006, 0.006), freq=FALSE)
lines(density(returns), col="red", lwd=1)

library(PerformanceAnalytics)
chart.CumReturns(returns, lwd=2,
                 ylab="", legend.loc="topleft", main="")
chart.Histogram(returns, main="",
                xlim=c(-0.003, 0.003),
                methods=c("add.density", "add.normal"))
chart.Histogram(returns, main="",
                xlim=c(-0.003, 0.003), breaks=300,
                methods=c("add.normal"))


# copy the xts data to a variable with the name "symbol"
symbol_rets <- paste(symbol, "rets", sep=".")
assign(symbol_rets, datav)

########### end temp ###########



##############
# TAQ high frequency trading strategies

# Load packages
library(HighFreq)
library(TAQMNGR)

file_name <- "aapl_tick_trades2020_0720"
# taq <- TAQMNGR::TAQ.Read(file="C:/Develop/data/aapl_tick_trades2020_0720.csv")
# taq <- read.csv(file="C:/Develop/data/aapl_tick_trades2020_0720.csv", stringsAsFactors=FALSE)
# taq <- data.table::fread(file=paste0("C:/Develop/data/", file_name, ".csv"), stringsAsFactors=FALSE)
load(file=paste0("C:/Develop/data/", file_name, ".RData"))
# taq$TIME_M[(NROW(taq)-3e4):(NROW(taq)-3e4+10)]
# plot(taq$PRICE[3e4:(NROW(taq)-3e4)], t="l")
# datav <- taq[3e4:(NROW(taq)-3e4), c("SIZE", "PRICE")]

# Select only the large lots greater than 100
taq <- taq[taq$SIZE > 100]

symbol <- taq$SYM_ROOT[1]
look_back <- 252
lagg <- 2
threshold <- 0.3
coeff <- (-1)
closep <- matrix(taq$PRICE, nc=1)
volumes <- matrix(taq$SIZE, nc=1)
returns <- rutils::diffit(closep)

# Calculate VWAP indicator
vwapv <- HighFreq::roll_sum(tseries=closep*volumes, look_back=look_back)
volume_rolling <- HighFreq::roll_sum(tseries=volumes, look_back=look_back)
vwapv <- vwapv/volume_rolling
vwapv[is.na(vwapv)] <- 0
# hist((closep - vwapv), breaks=100, xlim=c(-0.3, 0.3))
# Simulate strategy
posit <- rep(NA_integer_, NROW(closep))
posit[1] <- 0
# Long positions
indic <- ((closep - vwapv) > threshold*rangev)
indic <- HighFreq::roll_count(indic)
posit <- ifelse(indic >= lagg, 1, posit)
# Short positions
indic <- ((closep - vwapv) < (-threshold*rangev))
indic <- HighFreq::roll_count(indic)
posit <- ifelse(indic >= lagg, -1, posit)
posit <- zoo::na.locf(posit, na.rm=FALSE)
# Lag the positions to trade in next period
posit <- rutils::lagit(posit, lagg=1)
pnls <- cumsum(coeff*returns*posit)
# plot(pnls, t="l")

# Create date-time index
indeks <- paste(taq$DATE, taq$TIME_M)
indeks <- strptime(indeks, "%Y%m%d %H:%M:%OS")
indeks <- as.POSIXct(indeks)
# Coerce trade ticks to xts series
taq <- xts::xts(cbind(taq$PRICE, taq$SIZE), indeks)
colnames(taq) <- paste(symbol, c("Close", "Volume"), sep=".")
save(taq, file=paste0("C:/Develop/data/", file_name, "_biglots.RData"))

returns <- xts::xts(returns, indeks)
colnames(returns) <- paste(symbol, c("returns"), sep=".")

# Subset recurring time interval using "T notation"
returns_am <- returns["T09:30:00/T11:00:00"]
returns_noon <- returns["T11:00:00/T15:00:00"]
returns_pm <- returns["T15:00:00/T16:00:00"]
pacf(as.numeric(returns_am))

# Plot pnls
pnls <- cbind(closep, pnls, vwapv)
pnls <- xts::xts(pnls, indeks)
colnamev <- c(symbol, "strategy", "vwap")
colnames(pnls) <- colnamev
datav <- pnls[rutils::calc_endpoints()]


dygraphs::dygraph(pnls, main=paste(colnamev[1], "Strategy")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[3], axis="y", label=colnamev[3], strokeWidth=2, col="green") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")




##############
# Backtest of ETF trading strategies using the code 
# in jp2sig.R and jp2sig2.R

# Load packages
library(HighFreq)

# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")


## Define data
look_back <- 5
lagg <- 2
coeff <- (-1)
threshold <- 0.0
data_env <- rutils::etfenv
symbols <- get("symbols", data_env)
symbols <- symbols[!(symbols %in% c("VXX", "SVXY", "MTUM"))]
symbol <- "VTI"
ohlc <- get(symbol, data_env)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)

pnls <- backtest_ewma(get(symbol, data_env), look_back=look_back, lagg=lagg, coeff=coeff)

## Loop over look_backs
perfstats <- lapply(3:5, backtest_ewma, ohlc=ohlc, lagg=lagg, threshold=threshold, coeff=coeff)
pnls <- lapply(perfstats, function(x) x[, "pnls"])
pnls <- do.call(cbind, pnls)
pnls <- rowMeans(pnls)
mean(pnls)/sd(pnls)
pnls <- xts::xts(pnls, index(ohlc))
dygraphs::dygraph(cumsum(pnls), main=paste("Back-test of", symbol, "Strategies"))


## Calculate performance of all ETFs and save to environment
symbols <- rutils::etfenv$symbols
perfstats <- lapply(symbols, function(symbol) {
  ohlc <- get(symbol, data_env)
  backtest_ewma(ohlc, look_back=20, lagg=1, threshold=0, coeff=1)
  # backtest_zscores(ohlc, look_back=15, lagg=1, threshold=1.5, coeff=(-1))
})  # end lapply
names(perfstats) <- symbols
save(perfstats, file="C:/Develop/data/perf_ewma_trend_lback20.RData")
load("C:/Develop/data/perf_ewma_trend_lback20.RData")

# Calculate out-of-sample pnls using performance environment
pnls <- lapply(perfstats, function(xtes) {
  if (start(xtes) < "2017-01-01") {
    # Out-of-sample pnls
    pnls <- xtes["2010/2017" ,"pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end lapply

# Calculate symbols of best out-of-sample pnls
# pnls <- rutils::do_call(c, pnls)
pnls <- unlist(pnls)
# names(pnls) <- rutils::etfenv$symbols
pnls <- sort(pnls, decreasing=TRUE)
symbols <- names(pnls)


# Calculate time series of pnls
pnls <- lapply(perfstats, function(xtes) {
  if (start(xtes) < "2017-01-01") {
    xtes[ ,"pnls"]
  } else NULL
})  # end lapply
pnls <- rutils::do_call(cbind, pnls)
pnls[1, is.na(pnls[1, ])] <- 0
pnls <- zoo::na.locf(pnls, na.rm=FALSE)
sum(is.na(pnls))
pnls <- xts::xts(rowMeans(pnls), index(pnls))
pnls <- cumsum(pnls)
dygraphs::dygraph(pnls, main="Back-test of Reverting Strategies")



## Run model for single symbol and plot it

# Get data
symbol <- "MSFT"
ohlc <- get(symbol, sp500env)["2010/"]
closep <- log(quantmod::Cl(ohlc))
# Run model
pnls <- lapply(2*(5:15), backtest_zscores, ohlc=ohlc, lagg=1, threshold=0, coeff=(-1))
pnls <- lapply(pnls, function(x) x[, "pnls"])
pnls <- do.call(cbind, pnls)
pnls <- rowMeans(pnls)
pnls <- xts::xts(cumsum(pnls), index(ohlc))
# Plot it
datav <- cbind(closep, pnls)
colnamev <- c(symbol, "Strategy")
colnames(datav) <- colnamev
dygraphs::dygraph(datav, main=paste(colnamev[1], "Strategy")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")



## Run many models and plot them

data_env <- sp500env
pnls <- eapply(data_env, function(ohlc) {
  if ((start(ohlc) < "2015-01-01") & (end(ohlc) > "2020-06-25")) {
    ohlc <- ohlc["2010/2017"]
    pnls <- lapply(2*(5:15), backtest_zscores, ohlc=ohlc, lagg=1, threshold=0, coeff=(-1))
    pnls <- lapply(pnls, function(x) x[, "pnls"])
    pnls <- do.call(cbind, pnls)
    pnls <- rowMeans(pnls)
    pnls2 <- lapply(4:8, backtest_ewmar, ohlc=ohlc, lagg=1, coeff=(-1))
    pnls2 <- lapply(pnls2, function(x) x[, "pnls"])
    pnls2 <- do.call(cbind, pnls2)
    pnls2 <- rowMeans(pnls2)
    pnls <- rowMeans(cbind(pnls, pnls2))
    mean(pnls)/sd(pnls)
  } else NULL
  
})  # end eapply

pnls <- unlist(pnls)
pnls <- sort(pnls, decreasing=TRUE)
write.csv(pnls, file="C:/Develop/data/backtest_revert_combo_is.csv", row.names=TRUE)


## Calculate performance of all sp500 stocks and save to environment
perf_env <- new.env()
process_ed <- eapply(sp500env, function(ohlc) {
  symbol <- rutils::get_name(colnames(ohlc)[1])
  cat(symbol, "\n")
  assign(x=symbol, 
         value=backtest_zscores(ohlc, look_back=look_back, lagg=lagg, threshold=threshold, coeff=coeff), 
         envir=perf_env)
  symbol
})  # end eapply

save(perf_env, file="C:/Develop/data/perf_sp500_vwap_lback20.RData")
load("C:/Develop/data/perf_sp500_zscores_lback5.RData")


# Calculate in-sample pnls using performance environment
pnls <- eapply(perf_env, function(xtes) {
  if (start(xtes) < "2017-01-01") {
    pnls <- xtes["2010/2017" ,"pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply
pnls <- unlist(pnls)
pnls <- sort(pnls, decreasing=TRUE)
symbols <- names(pnls)

# Calculate out-of-sample pnls of best stocks from environment
be_st <- lapply(symbols[1:20], function(symbol) {
  get(symbol, perf_env)[, "pnls"]
})  # end lapply
names(be_st) <- symbols[1:20]
be_st <- rutils::do_call(cbind, be_st)

# Calculate reverse of pnls of worst stocks
wo_rst <- lapply(symbols[(NROW(symbols)-19):NROW(symbols)], function(symbol) {
  get(symbol, perf_env)[, "pnls"]
})  # end lapply
names(be_st) <- symbols[(NROW(symbols)-19):NROW(symbols)]
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


# Calculate holding period number of trades for best stocks from environment
period_s <- sapply(symbols[1:20], function(symbol) {
  posit <- get(symbol, perf_env)[, "positions"]
  2*NROW(posit) / sum(abs(rutils::diffit(posit)))
})  # end sapply
names(period_s) <- symbols[1:20]

# Calculate holding period number of trades for worst stocks
period_s <- sapply(symbols[(NROW(symbols)-19):NROW(symbols)], function(symbol) {
  posit <- get(symbol, perf_env)[, "positions"]
  2*NROW(posit) / sum(abs(rutils::diffit(posit)))
})  # end sapply
names(period_s) <- symbols[(NROW(symbols)-19):NROW(symbols)]



## Loop over all sp500 stocks using several parameters
perf_env <- new.env()
process_ed <- eapply(sp500env, function(ohlc) {
  symbol <- rutils::get_name(colnames(ohlc)[1])
  # cat(symbol, "\n")
  pnls <- lapply(5:10, backtest_ewma, ohlc=ohlc, lagg=lagg, coeff=coeff)
  pnls <- lapply(pnls, function(x) x[, "pnls"])
  pnls <- do.call(cbind, pnls)
  assign(x=symbol, 
         value=xts::xts(rowMeans(pnls), index(ohlc)), 
         envir=perf_env)
  symbol
})  # end eapply

save(perf_env, file="C:/Develop/data/perf_sp500_vwap_lback510.RData")
load("C:/Develop/data/perf_sp500_vwap_lback510.RData")

# Calculate in-sample pnls using performance environment
pnls <- eapply(perf_env, function(xtes) {
  if (start(xtes) < "2017-01-01") {
    pnls <- xtes["2010/2017"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply
pnls <- unlist(pnls)
pnls <- sort(pnls, decreasing=TRUE)
symbols <- names(pnls)
write.csv(symbols, file="C:/Develop/data/backtest_ewma2.csv", row.names=TRUE)

# Calculate out-of-sample pnls of best stocks from environment
be_st <- lapply(symbols[1:20], function(symbol) {
  get(symbol, perf_env)
})  # end lapply
names(be_st) <- symbols[1:20]
be_st <- rutils::do_call(cbind, be_st)

# Calculate reverse of pnls of worst stocks
wo_rst <- lapply(symbols[(NROW(symbols)-19):NROW(symbols)], function(symbol) {
  get(symbol, perf_env)
})  # end lapply
names(be_st) <- symbols[(NROW(symbols)-19):NROW(symbols)]
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


#############
## Rank the S&P500 stock strategies in-sample
# perf_env is an environment with time series of performance
perfstats <- eapply(perf_env, function(xtes) {
  if (start(xtes) < "2010-01-01") {
    pnls <- xtes["2010/2017" ,"pnls"]
    mean(pnls)/sd(pnls)
  } else NULL
})  # end eapply
perfstats <- unlist(perfstats)
perfstats <- sort(perfstats, decreasing=TRUE)
symbols <- names(perfstats)

# Calculate the returns of S&P500 stock strategies out-of-sample
# as a function of the number of stocks selected
calc_perf <- function(nstocks) {
  # Calculate the returns of the best performing stocks
  be_st <- lapply(symbols[1:nstocks], function(symbol) {
    get(symbol, perf_env)[, "pnls"]
  })  # end lapply
  be_st <- rutils::do_call(cbind, be_st)
  # Calculate the returns of the worst performing stocks
  wo_rst <- lapply(symbols[(NROW(symbols)-nstocks+1):NROW(symbols)], function(symbol) {
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

profilev <- sapply(10*(1:10), calc_perf)
plot(x=10*(1:10), y=profilev, t="l", main="Sharpe of Back-test EWMA Strategies", 
     xlab="Select number", ylab="Sharpe")



##############
# Rolling portfolio optimization strategies
# Code from app_roll_trend_vol_scaled.R

# Load packages
library(HighFreq)

# Best parameters
# typev max_sharpe  min_var
# interval weeks  weeks
# look_back	4 4
# max_eigen	9 9
# alpha	0.01 0.01

# symbols <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU", "SVXY", "VXX")
symbols <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU")

nweights <- NROW(symbols)
returns <- rutils::etfenv$returns[, symbols]
# Calculate the first non-NA values and their positions.
first_non_na <- sapply(returns, function(xtes) {
  match(TRUE, !is.na(xtes))
})  # end sapply
# Select rows containing at least 3 non-NA values.
returns <- returns[-(1:(sort(first_non_na)[7]-1))]
# Copy over NA values with zeros
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
# sum(is.na(returns))

# Define model parameters
typev <- "max_sharpe"
interval <- "weeks"
look_back <- 6
max_eigen <- 5
alpha <- 0.01
probv <- 0.25
coeff <- 1


# Define end points
endp <- rutils::calc_endpoints(returns, interval=interval)
# endp <- ifelse(endp<(nweights+1), nweights+1, endp)
endp <- endp[endp > 2*nweights]
nrows <- NROW(endp)
# Define start points
startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])

# Run the model
pnls <- HighFreq::back_test(excess=returns, 
                             returns=returns,
                             startpoints=startp-1,
                             endpoints=endp-1,
                             probv=probv,
                             max_eigen=max_eigen, 
                             alpha=alpha, 
                             typev=typev,
                             coeff=coeff)

pnls[which(is.na(pnls)), ] <- 0
pnls <- cumsum(pnls)
pnls <- xts::xts(pnls, zoo::index(returns))
dygraphs::dygraph(pnls)
# plot(pnls, t="l")
# pnls <- cumprod(1 + pnls)
pnls <- cbind(pnls, indeks)
colnames(pnls) <- c("Strategy", "Index")


weightv <- HighFreq::calc_weights(returns=returns[startp[1432]:endp[1432], ],
                                   max_eigen=max_eigen, 
                                   probv=probv,
                                   alpha=alpha, 
                                   typev=typev)

weightv <- sapply(1:NROW(endp), function(it) 
  HighFreq::calc_weights(returns=returns[startp[it]:endp[it], ],
                         max_eigen=max_eigen, 
                         probv=probv,
                         alpha=alpha, 
                         typev=typev))
weightv <- t(weightv)
weightv <- xts::xts(weightv, index(returns)[endp])
weightv <- cbind(returns[, 1], weightv)
weightv <- weightv[, -1]
colnames(weightv) <- colnames(returns)
weightv[1, is.na(weightv[1, ])] <- 0
weightv <- zoo::na.locf(weightv, na.rm=FALSE)
weightv <- rutils::lagit(weightv)

zoo::plot.zoo(weightv)

bar <- rowSums(returns * weightv)
bar <- cumsum(bar)
plot(bar, t="l")





##############
# Rolling portfolio optimization strategy in app_roll_trend.R.

symbols <- c("VYM", "VEU", "DBC", "IEF", "IVW")
symbols <- c("VYM", "VEU", "DBC", "IEF", "VTI", "IWF", "IWD", "IWB")
symbols <- c("XLU", "XLE", "XLK", "IWD", "VYM", "IWF", "XLI", "IEF", "VNQ", "DBC")
# ETFs with largest Hurst using calc_hurst_hilo()

# old version
symbols <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU")
symbols <- c("IVW", "VTI", "IWF", "IWD", "IWB", "VYM", "DBC", "IEF", "VEU", "SVXY", "VXX")



##############
# Returns scaled by volume

# Extract volumes
volume_s <- lapply(symbols, function(symbol) {
  quantmod::Vo(get(x=symbol, envir=rutils::etfenv))
})  # end lapply
volume_s <- rutils::do_call(cbind, volume_s)
colnames(volume_s) <- symbols
volume_s <- volume_s[index(returns)]
volume_s[volume_s == 0] <- NA
volume_s <- zoo::na.locf(volume_s, na.rm=FALSE)
volume_s <- zoo::na.locf(volume_s, fromLast=TRUE)
excess <- returns/volume_s
sum(is.na(excess))
sum(is.infinite(excess))

interval <- "months"
look_back <- 6
alpha <- 0.7
max_eigen <- 5
endp <- rutils::calc_endpoints(returns, interval="months")
endp <- endp[endp > 2*nweights]
nrows <- NROW(endp)
startp <- c(rep_len(1, look_back-1), endp[1:(nrows-look_back+1)])

retsp <- drop(HighFreq::back_test(excess=excess, 
                                       returns=returns, 
                                       startpoints=startp-1, 
                                       endpoints=endp-1, 
                                       alpha=alpha, 
                                       max_eigen=max_eigen))
# Benchmark index
indeks <- xts(rowMeans(returns), index(returns))
wealth <- cbind(cumprod(1 + indeks),
                 cumprod(1 + retsp))
colnames(wealth) <- c("equal_weight", "roll_portf")
colnamev <- colnames(wealth)
dygraphs::dygraph(wealth, main="Max Hurst vs Max Hurst DEoptim") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red")




################
### Managing high frequency data using package HighFreq

# Load a single day of seconds TAQ data
symbol <- load("C:/Develop/data/hfreq/src/SPY/2012.02.16.SPY.RData")
# Extract one day of TAQ data from list, and subset to NYSE trading hours using "T notation"
prices <- (get(symbol)[[6]])["T09:30:00/T16:00:00", ]
# Extract trade price and volume
prices <- prices[, c("Trade.Price", "Volume")]

# Calculate mid bid-offer prices and remove NAs
mid_prices <- 0.5 * (prices[, "Bid.Price"] + prices[, "Ask.Price"])
mid_prices <- na.omit(mid_prices)
colnames(mid_prices) <- "Mid.Price"

# Calculate log returns
returns <- diff(log(mid_prices))/c(1, diff(.index(mid_prices)))
returns[1, ] <- 0
chart_Series(exp(cumsum(returns)), name=symbol)

# Load minutely OHLC data
symbol <- load("C:/Develop/data/SPY.RData")
# or
symbol <- load(file.path(output_dir, paste0(symbol, ".RData")))
# Calculate log returns
returns <- diff(log(Cl(SPY)))/c(1, diff(.index(SPY)))
returns[1, ] <- 0
chart_Series(exp(cumsum(returns)), name=symbol)

# Plot histograms of returns
hist(returns, breaks=30, main="returns", xlab="", ylab="", freq=FALSE)
hist(returns, breaks=100, main="returns", xlim=c(-2.0e-4, 2.0e-4), ylim=c(0, 10000), xlab="", ylab="", freq=FALSE)
lines(density(returns), col='red', lwd=1)  # draw density


## Calculate daily Open and Close prices from daily OHLC data
# quantmod::getSymbols("SPY", adjust=TRUE)
openp <- SPY[, 1]
tail(openp)
closep <- SPY[, 4]
tail(closep)


## Calculate daily price profiles after overnight gaps

# Calculate daily Open and Close prices from high frequency data
end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
start_days <- rutils::lagit(end_days)+1
closep <- HighFreq::SPY[end_days, 4]
index(closep) <- lubridate::floor_date(index(closep), "day")
# index(closep) <- as.POSIXct(trunc(index(closep), "day"))
# index(closep) <- as.POSIXct(format(index(closep), format="%Y-%m-%d"))
tail(closep)
openp <- HighFreq::SPY[start_days, 4]
index(openp) <- lubridate::floor_date(index(openp), "day")
tail(openp)
# Calculate daily overnight and intraday returns
close_rets <- (closep - rutils::lagit(openp)) / rutils::lagit(openp)
open_rets <- (openp - rutils::lagit(openp)) / rutils::lagit(openp)
over_night <- (openp - rutils::lagit(closep)) / rutils::lagit(closep)
intra_day <- (closep - openp) / openp
plot.zoo(x=over_night, y=intra_day, main="intra_day versus over_night")
plot.zoo(x=over_night[over_night<(-0.02)], y=intra_day[over_night<(-0.02)], main="intra_day versus over_night")
mo_del <- lm(intra_day[over_night<(-0.02)] ~ over_night[over_night<(-0.02)])
summary(mo_del)
plot.zoo(exp(cumsum(intra_day)), t="l", main="intra_day", ylab="")
plot.zoo(exp(cumsum(over_night)), t="l", main="over_night", ylab="")
plot.zoo(exp(cumsum(over_night-intra_day)), t="l")
hist(over_night, breaks=30, main="over_night", xlab="", ylab="", xlim=c(-0.02, 0.02), freq=TRUE)

# Calculate daily intraday volatilities
var_daily <- (6.5*60*60^2)*period.apply(x=HighFreq::SPY, INDEX=end_days, HighFreq::calc_variance)
index(var_daily) <- lubridate::floor_date(index(var_daily), "day")
chart_Series(var_daily["2013/"], name="daily intraday volatilities")

# Calculate ratio of overnight return divided by yesterday's intraday volatily
over_night <- over_night/rutils::lagit(var_daily)

# Calculate a list of daily endpoints
endpoints <- lapply(seq_along(end_days), function(da_y) {
  start_days[da_y]:end_days[da_y]
})  # end lapply
names(endpoints) <- index(closep)
# Calculate daily price profiles
price_profiles <- lapply(endpoints, function(endpoint) {
  datav <- as.numeric(HighFreq::SPY[endpoint, 4])
  datav / datav[1]
})  # end lapply
# price_profiles <- lapply(seq_along(end_days), function(da_y) {
#   datav <- as.numeric(HighFreq::SPY[start_days[da_y]:end_days[da_y], 4])
#   datav / datav[1]
# })  # end lapply
plot(price_profiles[["2010-05-05/2010-05-07"]], t="l", ylab="")

# run simple contrarian strategies
foo <- -as.numeric(over_night<(-0.01))* intra_day
foo <- -as.numeric(rutils::lagit(over_night)<(-0.01))* open_rets
foo <- -sign(rutils::lagit(over_night))* open_rets
foo <- -sign(rutils::lagit(over_night))* close_rets
foo <- exp(cumsum(foo))
plot.zoo(foo, t="l", main="simple contrarian strategy", ylab="")


# price_profiles <- rowMeans(price_profiles)
# plot(price_profiles, t="l")


# Calculate daily price profiles after overnight gaps



## Calculate intraday seasonality of returns
returns <- diff(log(Cl(SPY)))/c(1, diff(.index(SPY)))
returns <- diff(Cl(SPY))/c(1, diff(.index(SPY)))
returns[1, ] <- 0
returns <- na.locf(returns, na.rm=FALSE)
sum(is.na(returns))
# Remove overnight return spikes at "09:31"
indeks <- format(index(returns), "%H:%M")
returns <- returns[!indeks=="09:31", ]
# Calculate intraday seasonality of returns
season_rets <- HighFreq::season_ality(xtes=returns)
chart_Series(x=season_rets,
             name=paste(colnames(season_rets), "intraday seasonality"))


## spikes over running scaled variance
ohlc <- HighFreq::SPY["2009"]
indeks <- index(ohlc)
var_running <- 6.5*60^3*HighFreq::run_variance(ohlc=ohlc[, 1:4])
# rolling vwav volatility
var_rolling <- roll_vwap(ohlc=ohlc, xtes=var_running, look_back=21)
# colnames(var_rolling) <- colnames(var_running)

indeks <- index(var_rolling)
dim(var_rolling)
head(var_rolling)
tail(var_rolling)
plot(coredata(var_rolling), t="l")


# Plot histogram of volatility - similar to Chi-squared distribution
library(PerformanceAnalytics)
PerformanceAnalytics::chart.Histogram(var_running,
  main="Distribution of running variance",
  xlab=colnames(var_running), xlim=range(var_running)/2,
  methods=c("add.density"))
# Add title
# title(main=paste(symbol, "vol"), line=-1)
# Add Chi-squared fit
x_var <- seq(from=0, to=range(var_rolling)[2]/5, length.out=100)
lines(x=x_var, y=50*NROW(var_rolling)*dchisq(11*x_var/mean(var_rolling), df=11),
      xlab="", ylab="", lwd=1, col="blue")

# identify periods around volatility spikes
var_running <- roll::roll_scale(data=var_running, width=30, min_obs=1)
var_running[1, ] <- 0
var_running <- var_running[indeks, ]
# var_running <- SPY_design[indeks, "variance"]
hist(var_running, breaks=200, main="var_running", xlab="", ylab="", freq=FALSE)
quantile(var_running, probs=c(0.9, 0.99))
vol_spikes <- var_running[var_running>quantile(var_running, probs=0.99), ]
ma_tch <- match(index(vol_spikes), indeks)
# class(vol_spikes)
# dim(vol_spikes)
# head(vol_spikes)
# index(tail(vol_spikes, 22))
# foo <- unique(format(index(vol_spikes), format="%Y-%m-%d"))
chart_Series(SPY[indeks, 4], name=paste("SPY", "vol spikes"))
abline(v=ma_tch, col="red", lwd=1)
# chart_Series(SPY[foo[36]], name=paste("SPY", "vol spike"))
# chart_Series(SPY["2010-05-05/2010-05-07"], name=paste("SPY", "vol spike"))

# foo <- c(1, as.numeric(diff(.index(vol_spikes))))
# foo <- c(1, which(foo>60), NROW(vol_spikes))
# NROW(foo)
# head(foo)
# tail(foo)
# plot(coredata(vol_spikes[(foo[3]-10):(foo[3]+40), ]), t="l")
# which.max(vol_spikes[foo[3]:(foo[4]-1), ])
# vol_spike <- vol_spikes[foo[3]:(foo[4]-1), ]
# max(vol_spike)
# vol_spike[which.max(vol_spike)]
# blah <- which(indeks==index(vol_spike[which.max(vol_spike)]))
# plot(coredata(var_rolling[(blah-10):(blah+30), ])/max(vol_spike), t="l")

# legacy scripts - which I don't understand
# Aggregate over periods around volatility spikes
# vol_peaks <- lapply(1:(NROW(foo)-1), function(i_ter) {
#   vol_spike <- vol_spikes[foo[i_ter]:(foo[i_ter+1]-1), ]
#   vol_spike[which.max(vol_spike)]
# })  # end lapply
# vol_peaks <- rutils::do_call(rbind, vol_peaks)
# class(vol_peaks)
# NROW(vol_peaks)
# head(vol_peaks)
# which(indeks==index(first(vol_peaks)))

# legacy scripts - which I don't understand
# get_vol_peak_data <- function(vol_peak) {
#   which_peak <- which(indeks==index(vol_peak))
#   coredata(var_rolling[(which_peak-10):(which_peak+30), ])/as.numeric(vol_peak)
# }  # end get_vol_peak_data
# get_vol_peak_data(first(vol_peaks))
# get_vol_peak_data(vol_peaks[3])
# get_vol_peak_data(last(vol_peaks))
# debug(get_vol_peak_data)
# vol_profiles <- sapply(1:3, function(i_ter) get_vol_peak_data(vol_peaks[i_ter, ]))
# end legacy scripts

# Calculate volatility profiles around volatility peaks
# old version
# vol_profiles <- sapply(seq_along(vol_peaks), function(i_ter) {
#   which_peak <- which(indeks==index(vol_peaks[i_ter, ]))
#   coredata(var_rolling[(which_peak-200):(which_peak+300), ])/as.numeric(vol_peaks[i_ter, ])
# })  # end sapply
vol_profiles <- sapply(ma_tch[-((NROW(ma_tch)-40):NROW(ma_tch))], function(pea_k) {
  coredata(var_running[(pea_k-2):(pea_k+10), ])#/as.numeric(var_running[pea_k, ])
})  # end sapply
# class(vol_profiles)
# dim(vol_profiles)
vol_profiles <- rowMeans(vol_profiles)
plot(vol_profiles, t="l")

# Calculate price profiles around peak volatility
# old version
# price_profiles <- sapply(seq_along(vol_peaks), function(i_ter) {
#   which_peak <- which(indeks==index(vol_peaks[i_ter, ]))
#   core_data <- coredata(SPY["2009", 4][(which_peak-200):(which_peak+300)])
#   core_data/max(core_data)
# })  # end sapply

price_profiles <- sapply(ma_tch[-((NROW(ma_tch)-250):NROW(ma_tch))], function(pea_k) {
  coredata(ohlc[(pea_k-2):(pea_k+200), 4])/as.numeric(ohlc[pea_k, 4])
})  # end sapply
price_profiles <- rowMeans(price_profiles)
plot(price_profiles, t="l")


foo_bar <- apply(X=price_profiles, MARGIN=2, hurst_exp)
class(foo_bar)
dim(foo_bar)
head(foo_bar, 33)


## Calculate Hurst exponent using range for xts
hurst_exp <- function(returns) {
  cumsumv <- cumsum(returns)
  (max(cumsumv) - min(cumsumv))/sd(returns)/sqrt(NROW(returns))
}  # end hurst_exp
hurst_exp <- function(datav) {
  (max(datav) - min(datav))/sd(diff(datav)[-1])/sqrt(NROW(datav))
}  # end hurst_exp
# Calculate Hurst exponent using range for xts ohlc
hurst_exp <- function(datav) {
  (max(Hi(datav)) - min(Lo(datav)))/(max(Hi(datav)) + min(Lo(datav)))/sum(6.5*60^3*HighFreq::run_variance(ohlc=datav[, 1:4]))/sqrt(NROW(datav))/2
}  # end hurst_exp
# Calculate Hurst exponent using range for non-xts
hurst_exp <- function(datav) {
  log((max(datav) - min(datav))/sd(datav[-1]-datav[-NROW(datav)]))/log(NROW(datav))
}  # end hurst_exp
# Calculate Hurst exponent using variance ratios for non-xts
hurst_exp <- function(datav, l_ag=4) {
  nrows <- NROW(datav)
  var(datav[-(1:l_ag)]-datav[-((nrows-l_ag+1):nrows)])/var(datav[-1]-datav[-nrows])/l_ag
}  # end hurst_exp
hurst_exp(coredata(SPY["2009", 4]))
hurst_exp(coredata(SPY["2009", 4]), l_ag=10)
blah <- rnorm(NROW(SPY["2009", 4]))
head(blah)
hurst_exp(cumsum(blah))
hurst_exp(cumsum(blah+c(0, 0.5*blah[-NROW(blah)])))


## yearly aggregations of volume, skew, and volat

# Extract vector of ye_ars
ye_ars <- format(
  index(skew[xts::endpoints(skew, on="years"), ]),
  format="%Y")
# Sum up volumes for each year
volumes_yearly <- sapply(ye_ars, function(ye_ar) sum(Vo(SPY)[ye_ar]))
# first plot without "x" axis
plot(volumes_yearly, t="l", xaxt="n", xlab=NA, ylab=NA)
# Add "x" axis with monthly ticks
axis(side=1, at=seq_along(volumes_yearly),
     labels=names(volumes_yearly))
# Sum up skew and volat for each year
sapply(ye_ars, function(ye_ar) sum(var_running[ye_ar]))
sapply(ye_ars, function(ye_ar) sum(skew[ye_ar]))
foo <- sapply(ye_ars, function(ye_ar) sum(Vo(SPY)[ye_ar]))
foo <- format(index(daily_skew[which.max(daily_skew)]), "%Y-%m-%d")

foo <- which.max(daily_skew)
foo <- which.min(daily_skew)
foo <- format(index(daily_skew[(foo-1):(foo+1), ]), "%Y-%m-%d")

chart_Series(SPY[foo], name=paste(symbol, "skew"))


# daily returns
daily_rets <- Cl(SPY[index(daily_skew), ])
daily_rets <- diff(log(daily_rets))
daily_rets[1, ] <- daily_rets[2, ]
colnames(daily_rets) <- paste(symbol, "rets", sep=".")
head(daily_rets)
tail(daily_rets)

dates <- "2010-05-05/2010-05-07"
# daily_rets and skew
bar <- cbind(coredata(daily_rets), coredata(daily_skew))
# daily_rets and lagged skew
bar <- cbind(coredata(daily_rets), c(0, coredata(daily_skew)[-NROW(daily_skew)]))

head(bar)
dim(bar)
apply(bar, 2, mad)
madv <- mad(bar[, 2])
blah <- (abs(bar[, 2]-mean(bar[, 2])) > 5*madv)
NROW(blah)
sum(blah)
bar <- bar[!blah, ]


## returns

# lag_rets equals returns lagged by -1
returns <- 6.5*60^2*HighFreq::run_returns(xtes=HighFreq::SPY, scalit=FALSE)
lag_rets <- rutils::lagit(returns_running)
# lag_rets <- c(lag_rets[-1, ], lag_rets[NROW(lag_rets)])
tail(lag_rets)
returns_advanced <- rutils::lagit(returns_running, k=-1)

skew <- 6.5*60^4*HighFreq::run_skew(ohlc=SPY)
colnames(skew) <- paste(symbol, "skew", sep=".")
lag_skew <- lag(skew)


look_back <- 2*60*6.5 + 101

## variance and skew estimators using MAD
# calc var_mad
var_mad <- runmad(coredata(variance), k=look_back)
# lag var_mad
var_mad <- c(rep(0, (look_back-1)/2), var_mad[-((NROW(var_mad)-(look_back-1)/2+1):(NROW(var_mad)))])
NROW(var_mad)
head(var_mad)
tail(var_mad)
# calc skew_mad
skew_mad <- runmad(coredata(skew), k=look_back)
# lag skew_mad
skew_mad <- c(rep(0, (look_back-1)/2), skew_mad[-((NROW(skew_mad)-(look_back-1)/2+1):(NROW(skew_mad)))])
plot(skew_mad[(NROW(skew_mad)-100*look_back):NROW(skew_mad)], t="l", xlab="", ylab="", main="skew_mad")
# calc mad_volu
quantilevs <- c("0.5"=0.5, "0.75"=0.75, "0.85"=0.85, "0.95"=0.95)
mad_volu <- runquantile(coredata(Vo(SPY)), probs=quantilevs, k=look_back)
mad_volu <- mad_volu[, 1, ]
# lag mad_volu
mad_volu <- rbind(
  matrix(numeric(ncol(mad_volu)*(look_back-1)/2), ncol=ncol(mad_volu)),
  mad_volu[-((NROW(mad_volu)-(look_back-1)/2+1):(NROW(mad_volu))), ])
colnames(mad_volu) <- names(quantilevs)
mad_volu <- xts(mad_volu, order.by=index(SPY))
# plot(mad_volu[(NROW(mad_volu)-100*look_back):NROW(mad_volu[,]), 4], t="l", xlab="", ylab="", main="mad_volu")
chart_Series(mad_volu[(NROW(mad_volu)-100*look_back):NROW(mad_volu[,]), 4], name=paste(symbol, "mad_volu"))
# Plot volume spikes above 85% quantile
dates <- (NROW(mad_volu)-4*look_back):NROW(mad_volu[,])
chart_Series(mad_volu[dates, 3], name=paste(symbol, "mad_volu"))
chart_Series(Vo(SPY[dates]) - mad_volu[dates, 4], name=paste(symbol, "volume spikes"))
chart_Series(Cl(SPY[dates]), name=paste(symbol, "prices"))


# signal threshold trading level
pos_skew <- coredata(ifelse(skew > 5*skew_mad, 1, 0))
colnames(pos_skew) <- paste(symbol, "p_skew", sep=".")
neg_skew <- coredata(ifelse(skew < -5*skew_mad, -1, 0))
colnames(neg_skew) <- paste(symbol, "n_skew", sep=".")
c(pos_skew=sum(pos_skew)/NROW(pos_skew), neg_skew=-sum(neg_skew)/NROW(neg_skew))
plot(pos_skew)

spike_skew <- coredata(Vo(SPY) - mad_volu[, 4] > 0, sign(skew), 0)
colnames(spike_skew) <- paste(symbol, "spike_skew", sep=".")

var_rolling <- runSum(variance, n=look_back)
var_rolling[1:(look_back-1)] <- 0
colnames(var_rolling) <- colnames(variance)
head(var_rolling)

chart_Series(var_rolling[dates],
             name=paste(symbol, "volatility"))

roll_skew <- runSum(skew, n=look_back)
roll_skew[1:(look_back-1)] <- 0
colnames(roll_skew) <- colnames(skew)
head(roll_skew)

chart_Series(roll_skew[dates],
             name=paste(symbol, "skew"))

win_short <- 70
win_long <- 225
vwap_short <- roll_vwap(ohlc=SPY, look_back=win_short)
vwap_long <- roll_vwap(ohlc=SPY, look_back=win_long)
head(vwap_short)
head(vwap_long)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste(symbol, "vwap", sep=".")
vwap_diff <- na.locf(vwap_diff, na.rm=FALSE)


## data: lagged returns plus explanatory variables

# for lm reg
# design <- cbind(lag_rets, coredata(returns), pos_skew, neg_skew)
# design <- cbind(lag_rets, coredata(vwap_diff), pos_skew, neg_skew)
design <- cbind(returns, lag_skew)
design <- cbind(lag_rets, sign(coredata(vwap_diff)), pos_skew, neg_skew)
# design <- cbind(sign(lag_rets), sign(coredata(vwap_diff)), pos_skew, neg_skew)
# for logistic reg
design <- cbind((sign(coredata(lag_rets))+1)/2, sign(coredata(vwap_diff)), pos_skew, neg_skew)
# for lda qda
design <- cbind(sign(lag_rets), coredata(vwap_diff), pos_skew, neg_skew)
# colnames(design) <- c("SPY.lagrets", "SPY.rets", "SPY.poskew", "SPY.negskew")
class(design)
tail(design)


## lm

# lm formula with zero intercept
formulav <- as.formula(paste(colnames(design)[1], paste(paste(colnames(design)[-1], collapse=" + "), "- 1"), sep="~"))
formulav <- as.formula(paste(colnames(design)[1], paste(colnames(design)[2], "- 1"), sep="~"))

lmod <- lm(formulav, data=as.data.frame(design))
# Perform regressions over different calendar periods
lmod <- lm(formulav, data=as.data.frame(design["2011-01-01/"]))
lmod <- lm(formulav, data=as.data.frame(design["/2011-01-01"]))
lm_summ <- summary(lmod)
lmod <- lm(formulav, data=as.data.frame(design["2010-05-05/2010-05-07"]))
lm_summ <- summary(lmod)
lm_predict <- predict(lmod, newdata=as.data.frame(design["2010-05-06"]))
foo <- data.frame(sign(lm_predict), coredata(design["2010-05-06", 1]))
colnames(foo) <- c("lm_pred", "realized")
table(foo)
cumu_pnl <- cumsum(sign(lm_predict)*returns["2010-05-06", 1])
last(cumu_pnl)
chart_Series(cumu_pnl, name=paste(symbol, "optim_rets"))

# loop over thresholds and return regression t-values
foo <- sapply(structure(2:10, paste0("thresh", names=2:10)), function(threshold) {
  pos_skew <- coredata(ifelse(skew > threshold*skew_mad, 1, 0))
  colnames(pos_skew) <- paste(symbol, "p_skew", sep=".")
  neg_skew <- coredata(ifelse(skew < -threshold*skew_mad, -1, 0))
  colnames(neg_skew) <- paste(symbol, "n_skew", sep=".")
  design <- cbind(sign(lag_rets), sign(coredata(vwap_diff)), pos_skew, neg_skew)
  lmod <- lm(formulav, data=as.data.frame(design))
  lm_summ <- summary(lmod)
  lm_summ$coefficients[, "t value"]
}, USE.NAMES=TRUE)  # end sapply


# loop over periods
dates <- "2013-06-01/"
dates <- "2008-06-01/2009-06-01"
endpoints <- xts::endpoints(SPY[dates], on="days")
endpoints <- format(index((SPY[dates])[endpoints[-1], ]), "%Y-%m-%d")
look_back <- 10

posit <-
  lapply(look_back:NROW(endpoints),
         function(endpoint) {
           dates <- paste0(endpoints[endpoint-look_back+1], "/", endpoints[endpoint-1])
           lmod <- lm(formulav, data=as.data.frame(design[dates]))
           datav <- design[endpoints[endpoint]]
           xts(x=predict(lmod, newdata=as.data.frame(datav)), order.by=index(datav))
         }  # end anon function
  )  # end lapply
posit <- rutils::do_call(rbind, posit)
chart_Series(posit, name=paste(symbol, "optim_rets"))

cumu_pnl <- cumsum(sign(posit)*returns[index(posit), 1])
last(cumu_pnl)
chart_Series(cumu_pnl, name=paste(symbol, "optim_rets"))


## logistic reg
library(MASS)
library(ISLR)
library(glmnet)
glmod <- glm(formulav, data=as.data.frame(design), family=binomial)
summary(glmod)


## lda
l_da <- lda(formulav, data=as.data.frame(design))
summary(l_da)
l_da <- lda(formulav, data=as.data.frame(design["2010-05-05/2010-05-07"]))
lda_predict <- predict(l_da, newdata=as.data.frame(design["2010-05-06"]))
foo <- data.frame(lda_predict$class, coredata(design["2010-05-06", 1]))
colnames(foo) <- c("lda_pred", "realized")
table(foo)


## qda
q_da <- qda(formulav, data=as.data.frame(design))
summary(q_da)
dates <- "2010-05-05/2010-05-07"
q_da <- qda(formulav, data=as.data.frame(design["2010-05-05/2010-05-07"]))
dates <- "2013-02-07"
qda_predict <- predict(q_da, newdata=as.data.frame(design["2010-05-06"]))
str(qda_predict)
head(qda_predict$class)
tail(qda_predict$class)
NROW(qda_predict$class)
sum(qda_predict$class!=1)
sum(design["2013-02-07", 1]!=1)
foo <- data.frame(qda_predict$class, coredata(design["2010-05-06", 1]))
colnames(foo) <- c("qda_pred", "realized")
table(foo)

# scatterplot of skew and daily_rets
plot(formulav, data=design, xlab="skew", ylab="rets")
abline(lmod, col="blue")

cor.test(formula=as.formula(paste("~", paste(colnames(design), collapse=" + "))), data=as.data.frame(design))


dates <- "2013-06-01/"
design <- cbind(
  coredata(returns[dates, 1]),
  c(0, coredata(roll_skew[dates])[-NROW(roll_skew[dates])]))


# multiply matrix columns
foo <- t(t(coredata(design[, -1]))*coef(lmod)[-1])
dim(foo)
tail(foo)
apply(foo, MARGIN=2, sum)


## run simple strategy

# threshold <- 2*mad(roll_skew)  # signal threshold trading level
# posit <- NA*numeric(NROW(skew))
posit <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, sign(coredata(vwap_diff)))
posit <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, -coredata(returns))
posit <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, sign(coredata(vwap_diff)))
posit <- pos_skew + neg_skew + sign(coredata(vwap_diff))
posit <- -sign(skew) + sign(coredata(vwap_diff))
posit <- coredata(design[, -1]) %*% coef(lmod)
sum(is.na(posit))
NROW(posit)
head(posit)
plot(posit[(NROW(posit)-100*look_back):NROW(posit)], t="l", xlab="", ylab="", main="posit")
plot(posit, t="l", ylim=c(0, 0.001))

posit <- ifelse(roll_skew>threshold, -1, posit)
posit <- ifelse(roll_skew<(-threshold), 1, posit)
posit <- ifelse((roll_skew*lag(roll_skew))<0, 0, posit)
# lag the posit
lag_positions <- c(0, posit[-NROW(posit)])
lag_positions <- na.locf(lag_positions, na.rm=FALSE)
lag_positions <- merge(roll_skew, lag_positions)
colnames(lag_positions)[2] <-
  paste0(symbol, ".Position")
# cumulative PnL
cumu_pnl <- cumsum(lag_positions*returns)
last(cumu_pnl)
# cumu_pnl <- cumsum(lag_positions[, 2]*returns)
plot.zoo(cumu_pnl)
chart_Series(cumu_pnl, name=paste(symbol, "pnl"))

foo <- rutils::roll_sum(abs(sign(skew)-sign(lag_skew)), look_back=1000)
chart_Series(
  foo[xts::endpoints(foo, on="days"), ],
  name=paste(symbol, "contrarian skew strategy frequency of trades"))
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
costs <- bid_offer*abs(posit-lag_positions)/2
pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - costs


## optimize vwap

roll_vwap <- function(win_short=10, win_long=100, prices, returns) {
  vwap_short <- coredata(roll_vwap(ohlc=prices, look_back=win_short))
  vwap_long <- coredata(roll_vwap(ohlc=prices, look_back=win_long))
# lag the posit
  posit <- sign(vwap_short - vwap_long)
  posit <- c(0, posit[-NROW(posit)])
  sum(posit*returns)
}  # end roll_vwap

roll_vwap(prices=SPY, returns=returns)


short_windows <- seq(from=30, to=100, by=10)
names(short_windows) <- paste0("sh", short_windows)
long_windows <- seq(from=200, to=400, by=25)
names(long_windows) <- paste0("lo", long_windows)

matrixv <- sapply(short_windows,
                  function(win_short, ...)
                    sapply(long_windows,
                           roll_vwap,
                           win_short=win_short, ...),
                  prices=SPY, returns=returns)

# Load rgl
library(rgl)
persp3d(z=matrixv, col="green", x=short_windows, y=long_windows)


###


# seconds index
indeks <- as.POSIXct("2015-01-01 00:00:00") + 0:1000
indeks <- seq(from=as.POSIXct("2015-01-01 00:00:00"),
              to=as.POSIXct("2015-01-03 00:00:00"), by="sec")
head(indeks)
tail(indeks)
NROW(indeks)

# Simulate lognormal prices
foo <- xts(exp(cumsum(rnorm(NROW(indeks)))/100), order.by=indeks)
dim(foo)

# Aggregate minutes OHLC bars
ohlc <- to.period(x=foo, period="minutes", name="synth")
tail(ohlc)
# OHLC candlechart
chart_Series(x=ohlc["2015-01-01 01:00:00/2015-01-01 05:00:00"],
             name="OHLC candlechart")

# rolling volatility
var_running <- roll_vwap(ohlc=ohlc, xtes=6.5*60^3*HighFreq::run_variance(ohlc=ohlc), look_back=1000)
head(var_running)
tail(var_running)
# rolling skew
skew <- roll_vwap(ohlc=ohlc, xtes=6.5*60^4*HighFreq::run_skew(ohlc=ohlc), look_back=1000)
skew <- skew/(var_running)^(1.5)
skew[1, ] <- 0
skew <- na.locf(skew, na.rm=FALSE)
chart_Series(x=var_running, name="volatility")
chart_Series(x=skew, name="skew")


###

matrixv <- matrix(1:6, ncol=2)

foo <- etf_rets[, symbols]
head(foo)
NROW(etf_rets)

foo <- xts(matrix(rnorm(3*NROW(etf_rets)), ncol=3), order.by=index(etf_rets))

colnames(foo) <- colnames(etf_rets[, symbols])
head(foo)

annweights <- sapply(2:NROW(endpoints),
                      function(indeks) {
                        optim_portf(
                          retsp=foo,
                          startpoint=endpoints[indeks-1],
                          endpoint=endpoints[indeks])
                      }  # end anon function
)  # end sapply


colnames(annweights) <- format(index(foo[endpoints[-1]]), "%Y")

annweights <- t(annweights)


bar <- lapply(3:NROW(endpoints),
              function(indeks) {
                foo[endpoints[indeks-1]:endpoints[indeks], ] %*%
                  c(1, annweights[indeks-2, ])
              }  # end anon function
)  # end lapply

bar <- rutils::do_call(rbind, bar)

plot(cumsum(bar), t="l")


### sprintf() example scripts
# A wrapper for the C function sprintf, that returns a character vector containing a formatted combination of text and variable values.
# sprintf {base}	R Documentation
# Use C-style String Formatting Commands

sprintf(fmt="%f", foo[1])

# Use a literal % :
sprintf("%.0f%% said yes (out of a sample of size %.0f)", 66.666, 3)

# various formats of pi :
# re-use one argument three times, show difference between %x and %X
xx <- sprintf("%1$d %1$x %1$X", 0:15)
xx <- matrix(xx, dimnames=list(rep("", 16), "%d%x%X"))
noquote(format(xx, justify="right"))

# More sophisticated:

sprintf("min 10-char string '%10s'",
        c("a", "ABC", "and an even longer one"))

# Platform-dependent bad example from qdapTools 1.0.0:
# may pad with spaces or zeroes.
sprintf("%09s", month.name)

n <- 1:18
sprintf(paste0("e with %2d digits = %.", n, "g"), n, exp(1))

# Using arguments out of order
sprintf("second %2$1.0f, first %1$5.2f, third %3$1.0f", pi, 2, 3)

# Using asterisk for width or precision
sprintf("precision %.*f, width '%*.3f'", 3, pi, 8, pi)

# Asterisk and argument re-use, 'e' example reiterated:
sprintf("e with %1$2d digits = %2$.*1$g", n, exp(1))

# Recycle arguments
sprintf("%s %d", "test", 1:3)

# binary output showing rounding/representation errors
x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
cbind(x, sprintf("%a", x), sprintf("%a", y))



###

# measure of dispersion
dis_persion <- function(datav,
                        method=c("mean", "mean_narm", "median")) {
  # validate "method" argument
  method <- match.arg(method)
  switch(method,
         mean=mean(datav),
         mean_narm=mean(datav, na.rm=TRUE),
         median=median(datav))
}  # end dis_persion

# sd
# range
# Interquartile range
# Median absolute deviation (MAD)


### rolling regressions using package roll

library(HighFreq)
library(roll)

# example of rolling beta regressions
# specify regression formula
reg_formula <- XLP ~ VTI
# Perform rolling beta regressions every month
betas <- rollapply(etfenv$returns, width=252,
                    FUN=function(design_matrix)
                      coef(lm(reg_formula, data=design_matrix))[2],
                    by=22, by.column=FALSE, align="right")
betas <- na.omit(betas)
# Plot betas in x11() window
x11()
chart_Series(x=betas, name=paste("rolling betas", format(reg_formula)))

# Perform daily rolling beta regressions in parallel
betas <- roll::roll_lm(x=etfenv$returns[, "VTI"],
                  y=etfenv$returns[, "XLP"],
                  width=252)$coefficients
chart_Series(x=betas, name=paste("rolling betas", format(reg_formula)))

# Compare speed of rollapply() versus roll_lm()
library(microbenchmark)
datav <- etfenv$returns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(datav, width=22,
                      FUN=function(design_matrix)
                        coef(lm(reg_formula, data=design_matrix))[2],
                      by.column=FALSE, align="right"),
  roll_lm=roll::roll_lm(x=datav[, "VTI"],
                  y=datav[, "XLP"],
                  width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### Rolling Portfolio Optimization Strategy for S&P500 Constituents
# in slides already

library(rutils)
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")

n_col <- NCOL(prices)
endpoints <- rutils::calc_endpoints(prices, interval="months")
endpoints <- endpoints[endpoints>50]
nrows <- NROW(endpoints)
look_back <- 12
startpoints <- c(rep_len(1, look_back-1), endpoints[1:(nrows-look_back+1)])
# dates <- index(prices)
# prices <- t(t(prices) / as.numeric(prices[1, ]))
# sum(is.na(prices))
# indeks <- xts(rowSums(prices)/n_col, dates)
# returns <- diffit(prices)

Rcpp::sourceCpp(file="C:/Develop/R/scripts/roll_portf.cpp")

alpha <- 0.01
max_eigen <- 2

library(microbenchmark)
summary(microbenchmark(
  old=roll_portf(foo, foo, startpoints-1, endpoints-1, alpha=alpha, max_eigen=max_eigen),
  new=roll_portfx(foo, foo, startpoints-1, endpoints-1, alpha=alpha, max_eigen=max_eigen),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


strat_rets_arma <- roll_portf(returns,
                              returns,
                              startpoints-1,
                              endpoints-1,
                              alpha=alpha,
                              max_eigen=max_eigen)
x11()
strat_rets_arma <- cumsum(strat_rets_arma)
strat_rets_arma <- xts(strat_rets_arma, dates)

library(dygraphs)
dygraphs::dygraph(strat_rets_arma,
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")

strat_rets_arma <- cbind(strat_rets_arma, indeks)
colnamev <- c("Strategy", "Index")
colnames(strat_rets_arma) <- colnamev

dygraphs::dygraph(strat_rets_arma, main=paste(colnamev, collapse=" and ")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(colnamev[2], axis="y2", col=c("red", "blue"))


foo <- diffit(log(strat_rets_arma-min(strat_rets_arma)+1))
sqrt(260)*mean(foo)/sd(foo)
bar <- diffit(strat_rets_arma2)
sqrt(260)*mean(bar)/sd(bar)
bar <- diffit(log(indeks-min(indeks)+1))
sqrt(260)*mean(bar)/sd(bar)
cor(foo, bar)

foobar <- 0.1*foo+0.9*bar
foobar <- exp(cumsum(foobar))

sapply(seq(0, 1, 0.1), function(x) {
  foobar <- x*foo+(1-x)*bar
  sqrt(260)*mean(foobar)/sd(foobar)
})



###############
### Rolling Portfolio Optimization Strategy for ETFs

library(rutils)
# Load data
# symbols contains all the symbols in rutils::etfenv$returns except for "VXX"
symbols <- colnames(rutils::etfenv$returns)
symbols <- symbols[!(symbols=="VXX")]
# Extract columns of rutils::etfenv$returns and remove NA values
returns <- rutils::etfenv$returns[, symbols]
returns <- zoo::na.locf(returns, na.rm=FALSE)
returns <- na.omit(returns)

# Rolling Portfolio Optimization Strategy
look_back <- 12
endpoints <- rutils::calc_endpoints(returns, interval="months")
# Remove initial endpoints for warmpup
endpoints <- endpoints[endpoints>50]
nrows <- NROW(endpoints)
# sliding window
startpoints <- c(rep_len(1, look_back-1), endpoints[1:(nrows-look_back+1)])
# expanding window
startpoints <- rep_len(1, NROW(endpoints))


# coredata(returns) <- matrix(rnorm(prod(dim(returns)), sd=0.01), nc=NCOL(returns))
# tail(returns)
# riskf is the daily risk-free rate
riskf <- 0.03/260
excess <- returns - riskf
# weightv <- sapply(1:(NROW(endpoints)-1),
#                    function(i) {
#                      # subset the excess returns
#                      excess <- excess[startpoints[i]:endpoints[i], ]
#                      inverse <- solve(cov(excess))
#                      # Calculate the maximum Sharpe ratio portfolio weights
#                      weightv <- inverse %*% colMeans(excess)
#                      weightv <- drop(weightv/sum(weightv))
#                    }  # end anonymous function
# )  # end sapply
# round(weightv, 2)
# dim(weightv)

# Regular inverse
retsp <- lapply(2:NROW(endpoints),
                     function(i) {
                       # subset the excess returns
                       excess <- excess[startpoints[i-1]:endpoints[i-1], ]
                       inverse <- solve(cov(excess))
                       # Calculate the maximum Sharpe ratio portfolio weights
                       weightv <- inverse %*% colMeans(excess)
                       weightv <- drop(weightv/sqrt(sum(weightv^2)))
                       # subset the returns
                       returns <- returns[(endpoints[i-1]+1):endpoints[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(returns %*% weightv, index(returns))
                     }  # end anonymous function
)  # end lapply

# with regularization
max_eigen <- 2
retsp <- lapply(2:NROW(endpoints),
                     function(i) {
                       # subset the excess returns
                       excess <- excess[startpoints[i-1]:endpoints[i-1], ]
                       covmat <- cov(excess)
                       # Perform eigen decomposition and calculate eigenvectors and eigenvalues
                       eigend <- eigen(covmat)
                       eigen_vec <- eigend$vectors
                       # Calculate regularized inverse
                       inverse <- eigen_vec[, 1:max_eigen] %*% (t(eigen_vec[, 1:max_eigen]) / eigend$values[1:max_eigen])
                       # Calculate the maximum Sharpe ratio portfolio weights
                       # weightv <- inverse %*% colMeans(excess)
                       # weightv <- rep(mean(colMeans(excess)), NCOL(excess))
                       weightv <- colMeans(excess)
                       # shrink weightv to the mean of weightv
                       weightv <- ((1-alpha)*weightv + alpha*mean(weightv))
                       weightv <- inverse %*% weightv
                       weightv <- drop(weightv/sqrt(sum(weightv^2)))
                       # subset the returns
                       returns <- returns[(endpoints[i-1]+1):endpoints[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(returns %*% weightv, index(returns))
                     }  # end anonymous function
)  # end lapply


# with shrinkage intensity
alpha <- 0.9
retsp <- lapply(2:NROW(endpoints),
                     function(i) {
                       # subset the excess returns
                       excess <- excess[startpoints[i-1]:endpoints[i-1], ]
                       covmat <- cov(excess)
                       cormat <- cor(excess)
                       stdev <- sqrt(diag(covmat))
                       # Calculate target matrix
                       cor_mean <- mean(cormat[upper.tri(cormat)])
                       targetr <- matrix(cor_mean, nr=NROW(covmat), nc=NCOL(covmat))
                       diag(targetr) <- 1
                       targetr <- t(t(targetr * stdev) * stdev)
                       # Calculate shrinkage covariance matrix
                       covmat <- (1-alpha)*covmat + alpha*targetr
                       inverse <- solve(covmat)
                       # Calculate the maximum Sharpe ratio portfolio weights
                       weightv <- inverse %*% colMeans(excess)
                       weightv <- drop(weightv/sqrt(sum(weightv^2)))
                       # subset the returns
                       returns <- returns[(endpoints[i-1]+1):endpoints[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(returns %*% weightv, index(returns))
                     }  # end anonymous function
)  # end lapply


retsp <- rutils::do_call(rbind, retsp)
colnames(retsp) <- "retsp"
indeks <- index(returns)[index(returns) < start(retsp)]
retsp <- rbind(xts(numeric(NROW(indeks)), indeks), retsp)
all.equal(as.numeric(retsp), drop(foo))


# tail(retsp, 11)
# dim(retsp)
# sqrt(260)*(mean(retsp)-riskf) / sd(retsp)
retsp <- cumsum(retsp)
# tail(retsp, 11)
quantmod::chart_Series(retsp,
                       name="Cumulative Returns of Max Sharpe Portfolio Strategy")
dygraphs::dygraph(retsp, 
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test6.cpp")
foo <- roll_portf(excess, returns, startpoints-1, endpoints-1, alpha=alpha, max_eigen)
coredata(retsp) <- cumsum(foo)



###############
### Rolling S&P500 portfolio strategies

# Load HighFreq
library(rutils)
library(roll)

# sharp_e <- function(x) (as.numeric(x[NROW(x)])-as.numeric(x[1]))/sd(rutils::diffit(x))/sqrt(252)

# Load S&P500 constituent stock prices
load("C:/Develop/R/lecture_slides/data/sp500.RData")
prices <- eapply(sp500env, quantmod::Cl)
prices <- rutils::do_call(cbind, prices)
# Remove NA values
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- zoo::na.locf(prices, na.rm=FALSE, fromLast=TRUE)
# Normalize the prices
colnames(prices) <- sapply(colnames(prices),
                            function(colname) strsplit(colname, split="[.]")[[1]][1])

# Calculate the time series of the static, equal 
# dollar-weighted price of the index components, 
n_col <- NCOL(prices)
indeks <- rowSums(zoo::coredata(prices))/n_col
indeks <- indeks/indeks[1]
sd_index <- sd(rutils::diffit(log(indeks)))
# sqrt(252)*(indeks[NROW(indeks)]-1)/sd_index/NROW(indeks)
indeks <- xts::xts(indeks, order.by=index(prices))
colnames(indeks) <- "index"


# Calculate the percentage (log) returns of the S&P500 
# constituent stocks, and call them returns.
# You can use functions rutils::diffit() and log().

returns <- rutils::diffit(log(prices))
wid_th <- 220
returns_width <- rutils::diffit(log(prices), lagg=wid_th)


## Calculate rolling variance of S&P500 constituents
variance <- roll::roll_var(returns, width=wid_th)
variance <- zoo::na.locf(variance, na.rm=FALSE)
variance[is.na(variance)] <- 0
# variance[variance==0] <- 1
# variance <- zoo::na.locf(variance, na.rm=FALSE, fromLast=TRUE)
# sum(is.na(variance))
# sum(variance==0)
# head(variance[, 100:106])
# tail(variance[, 100:106])


## Calculate rolling Sharpe of S&P500 constituents

weightv <- returns_width / sqrt(wid_th*variance)
weightv[variance==0] <- 0
weightv[1:wid_th, ] <- 1
weightv[is.na(weightv)] <- 0

# long-short variance
# weightv <- 1/sqrt(wid_th*variance)
# weightv[variance==0] <- 0
# weightv[1:wid_th, ] <- 1
# weightv <- weightv/rowSums(abs(weightv))
# weightv[1:wid_th, ] <- 1/NCOL(weightv)
# weightv <- (weightv - rowMeans(weightv))
# sum(is.na(weightv))

# weightv <- 1 / sqrt(wid_th*variance)
# weightv <- zoo::na.locf(weightv, na.rm=FALSE)
# scale weightv so that their sum of squares is equal to 1
# weightv <- weightv/sqrt(rowSums(weightv^2))
# sum(is.na(weightv))
# sum(rowSums(abs(weightv))==0)
weightv <- weightv/rowSums(abs(weightv))
# sum(is.na(weightv))
# sum(rowSums(weightv^2)==0)

# Calculate portfolio profits and losses
pnls <- rowSums(rutils::lagit(weightv)*returns)
sdmoment <- sd(pnls)
pnls <- sd_index*pnls/sdmoment
pnls <- exp(cumsum(pnls))
pnls <- xts(pnls, order.by=index(returns))
colnames(pnls) <- "momentum"
pnls <- cbind(indeks, pnls)
# sharp_e(pnls[, 1])
sapply(pnls, function(x) sd(rutils::diffit(log(x))))

# foo <- pnls[, 1] + pnls[, 2]
# dygraphs::dygraph(foo, main="Momentum PnL")

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnls, theme=plot_theme, lwd=c(2, 2), 
             name="Momentum PnL")
legend("topleft", legend=colnames(pnls), 
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
       col=plot_theme$col$line.col, bty="n")


## rolling eigenvalues and eigenvectors
nrows <- NROW(returns)
foo <- roll::roll_eigen(returns[(nrows-100):nrows, 1:5], width=10)
foo$values[1:9, ] <- 0
# sum(is.na(foo$values))
plot(foo$values[, 1], t="l")
plot(as.numeric(foo$values[101, ]), t="l")
as.numeric(foo$values[101, ])
foo$vectors[, , 101]
ret_sub <- returns[(nrows-9):nrows, 1:5]
ret_mean <- apply(ret_sub, 2, mean)
# ret_sub <- apply(ret_sub, 2, function(x) x-mean(x))
# covmat <- crossprod(ret_sub)

## maximum Sharpe portfolio weights from inverse matrix
covmat <- cov(ret_sub)
eigend <- eigen(covmat)
inverse <- eigend$vectors %*% (t(eigend$vectors)/eigend$values)
trunc(inverse %*% covmat, 4)
weightv <- inverse %*% ret_mean
weightv <- weightv/sum(abs(weightv))

# maximum Sharpe portfolio weights
calc_weights <- function(returns) {
  eigend <- eigen(cov(returns))
  # set tolerance for determining zero eigenvalues
  precision <- sqrt(.Machine$double.eps)
  # check for zero eigenvalues
  not_zero <- (eigend$values > (precision * eigend$values[1]))
  inverse <- eigend$vectors[, not_zero] %*% (t(eigend$vectors[, not_zero])/eigend$values[not_zero])
  weightv <- inverse %*% apply(returns, 2, mean)
  weightv/sum(abs(weightv))
} # end calc_weights
calc_weights(ret_sub)

## maximum Sharpe portfolio weights from optimization
object_ive <- function(weightv, returns) {
  retsp <- returns %*% weightv
  if (sd(retsp) == 0)
    return(0)
  else
    return(-mean(retsp)/sd(retsp))
} # end object_ive

optimd <- optim(par=numeric(NCOL(ret_sub)),
                fn=object_ive,
                returns=ret_sub,
                method="L-BFGS-B",
                upper=(100+numeric(NCOL(ret_sub))),
                lower=(-100+numeric(NCOL(ret_sub))))
optimd$par/sum(abs(optimd$par))
as.numeric(weightv)


## backtest of maximum Sharpe portfolio weights strategy
# this strategy has similar characteristics to momentum, and also suffers crashes.
ret_sub <- returns[, 1:5]
endpoints <- rutils::calc_endpoints(returns, interval="months")
# endpoints <- endpoints[endpoints>look_back]
nrows <- NROW(endpoints)
look_back <- 10
startpoints <- c(rep_len(1, look_back-1), endpoints[1:(nrows-look_back+1)])
# Perform loop over endpoints and calculate aggregations
weightv <- sapply(1:nrows, function(indeks) {
  calc_weights(ret_sub[startpoints[indeks]:endpoints[indeks]])
})  # end sapply
weightv <- t(weightv)
weightv <- xts::xts(weightv, index(returns[endpoints, ]))
weightv <- cbind(returns[, 1], weightv)[, -1]
weightv[1, ] <- rep(1/NCOL(ret_sub), NCOL(ret_sub))
weightv <- zoo::na.locf(weightv, na.rm=FALSE)
weightv <- rutils::lagit(weightv)
sum(is.na(weightv))

pnls <- rowSums(weightv*ret_sub)
sdmoment <- sd(pnls)
pnls <- sd_index*pnls/sdmoment
pnls <- exp(cumsum(pnls))
pnls <- xts(pnls, order.by=index(returns))
colnames(pnls) <- "momentum"
pnls <- cbind(indeks, pnls)
# sharp_e(pnls[, 1])
sapply(pnls, function(x) sd(rutils::diffit(log(x))))

# foo <- pnls[, 1] + pnls[, 2]
# dygraphs::dygraph(foo, main="Momentum PnL")

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnls, theme=plot_theme, lwd=c(2, 2), 
             name="Momentum PnL")
legend("topleft", legend=colnames(pnls), 
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
       col=plot_theme$col$line.col, bty="n")


## GARCH variance
garch_fit <- fGarch::garchFit(data=rutils::diffit(log(indeks)))
garch_fit@fit$par
# Plot GARCH fitted standard deviation
fGarch::plot(garch_fit)
garch_sdev <- xts::xts(sqrt(garch_fit@fit$series$h), order.by=index(returns))
chart_Series(garch_sdev, name="GARCH fitted standard deviation")
dygraphs::dygraph(garch_sdev, main="GARCH fitted standard deviation")



## scatterplot returns versus volatility

endpoints <- rutils::calc_endpoints(returns, interval=20)
ret_vol <- lapply(returns, function(x) {
  returns <- roll::roll_mean(data=x, width=20)[endpoints]
  s_d <- sqrt(roll::roll_var(data=x, width=20)[endpoints])
  cbind(returns, s_d)
})  # end lapply
plot(x=as.numeric(ret_vol[[1]][, 2]), y=as.numeric(ret_vol[[1]][, 1]))
reg_models <- sapply(ret_vol, function(x) {
  reg_model <- lm(as.numeric(x[, 1]) ~ as.numeric(x[, 2]))
  summary(reg_model)$coefficients[2, ]
})  # end sapply
reg_models <- t(reg_models)
plot(x=lambdas, y=reg_models[, 3], t="l")


garch_sdev <- sqrt(garch_fit@fit$series$h)
pnls <- rowSums(weightv*ret_sub) / garch_sdev
plot(x=garch_sdev, y=pnls)
# orde_r <- order(garch_sdev)
# foo <- garch_sdev[orde_r]
# bar <- pnls[orde_r]
break_s <- hist(garch_sdev, breaks="Freedman-Diaconis")
break_s <- c(break_s$breaks, 100)

bar <- sapply(2:NROW(break_s), function(x) {
  ge_t <- (garch_sdev > (break_s[x-1]) & (garch_sdev < break_s[x]))
  sum(pnls[ge_t])
})  # end sapply
plot(x=break_s[-NROW(break_s)], y=bar, t="l")

# run EWMA strategies
lambdas <- seq(0.001, 0.15, 0.01)
rets_ewma <- lapply(lambdas, function(lambda) {
  simu_ewma(ohlc=ohlc, lambda=lambda, wid_th=151)[, 2]
})  # end lapply
rets_ewma <- rutils::do_call(cbind, rets_ewma)
colnames(rets_ewma) <- paste0("rets_", lambdas)
sapply(rets_ewma, function(x) sqrt(260)*sum(x)/sd(x)/NROW(x))
chart_Series(cumsum(rets_ewma[, 1]), name="EWMA trend-following strategy")



## Volatility switching weights strategy
# Adjust weights depending on volatility

ohlc <- rutils::etfenv$VTI
returns <- rutils::diffit(log(Cl(ohlc)))
rets_mean <- roll::roll_mean(data=returns, width=20)
rets_mean[1:19] <- 0
# garch_fit <- fGarch::garchFit(data=returns)
# garch_sdev <- xts::xts(sqrt(garch_fit@fit$series$h), order.by=index(returns))
s_d <- sqrt(roll::roll_var(data=returns, width=20))
# first and second derivatives of volatility
sd_diff <- rutils::diffit(s_d, lagg=20)
sd_diff2 <- rutils::diffit(sd_diff)
# chart_Series(sd_diff2, name="GARCH fitted standard deviation")
rets_mean <- rutils::lagit(rets_mean, lagg=(-20))

# Regression of future returns versus past volatility
reg_model <- lm(rets_mean ~ s_d + sd_diff + sd_diff2)
summary(reg_model)
plot(as.numeric(rets_mean) ~ as.numeric(sd_diff))
abline(lm(rets_mean ~ sd_diff))

# Use first derivative of volatility as predictor
foo <- NA*returns
foo[1] <- 0
foo[sd_diff > 0.001] <- (-1)
foo[sd_diff < (-0.001)] <- 1
foo <- na.locf(foo, na.rm=FALSE)
foo <- exp(cumsum(returns*rutils::lagit(foo)))
chart_Series(foo, name="GARCH fitted standard deviation")

levels <- seq(0.001, 0.01, 0.001)
bar <- lapply(levels, function(x) {
  foo <- NA*returns
  foo[1] <- 0
  foo[sd_diff > x] <- (-1)
  foo[sd_diff < (-x)] <- 1
  foo <- na.locf(foo, na.rm=FALSE)
  exp(cumsum(returns*rutils::lagit(foo)))
})  # end lapply
bar <- rutils::do_call(cbind, bar)
dygraphs::dygraph(bar[, 7], main="GARCH fitted standard deviation")
# chart_Series(bar[, 7], name="GARCH fitted standard deviation")
ba_se <- Cl(ohlc)/as.numeric(Cl(ohlc)[1, ])
foo <- (xts::xts(rowMeans(bar), order.by=index(returns)) + ba_se)/2
dygraphs::dygraph(cbind(foo, ba_se), main="GARCH fitted standard deviation")



# fit t-dist

1 - betav^2 - 2*alpha*betav - 3*alpha^2


moments::moment(returns, order = 3)
moments::moment(returns, order = 2)


# forecast GARCH
foo <- fGarch::predict(garch_fit, plot=TRUE)

# fit GARCH

foo <- tseries::garch(x=returns)
foo <- rugarch::ugarchfit(data=returns)

library(microbenchmark)
summary(microbenchmark(
  inn_er=(response %*% explana_tory),
  su_m=sum(response*explana_tory),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


library(rugarch)
require(xts)
data(spyreal)
spec = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(2, 1)))
setbounds(spec)<-list(alpha2=c(-1,1))
fit = ugarchfit(spec, spyreal[, 1] * 100, solver = 'hybrid', realizedVol = spyreal[,2] * 100)


# matrix eigenvalues
matrixv <- cbind(c(1, -0.5, 0.5), c(-0.5, 1, -0.5), c(0.5, -0.5, 1))
matrixv <- cbind(c(1, 0.5, 0.5), c(0.5, 1, -0.5), c(0.5, -0.5, 1))
matrixv <- cbind(c(1, -0.5, -0.5), c(-0.5, 1, -0.5), c(-0.5, -0.5, 1))
eigend <- eigen(matrixv)
eigend$vectors
eigend$values


# benchmarking Rcpp code
library(microbenchmark)
foo <- rep(1, 1e6)
summary(microbenchmark(
  pure_r=cumsum(foo),
  r_cpp=HighFreq::roll_sum(foo, 30),
  r_utils=rutils::roll_sum(foo, look_back=30),
  times=10))[, c(1, 4, 5)]

foo <- matrix(rnorm(1e6), nc=1)
look_back <- 11
weightv <- exp(0.1*1:look_back)
weightv <- weightv/sum(weightv)
foob <- filter(foo, filter=rev(weightv), sides=1)
foob[1:(look_back-1)] <- 0
foobar <- HighFreq::roll_wsum(foo, weightv)
foobar <- roll::roll_sum(foo, weights=weightv, width=NROW(weightv))
foobar <- roll::roll_var(foo, weights=weightv, width=NROW(weightv))


summary(microbenchmark(
  pure_r=filter(foo, filter=weightv, sides=1),
  r_cpp=HighFreq::roll_wsum(foo, weightv),
  r_oll=roll::roll_var(foo, weights=weightv, width=NROW(weightv)),
  times=10))[, c(1, 4, 5)]



###############
### OHLC momentum

# set up data
ohlc <- rutils::etfenv$VTI
closep <- quantmod::Cl(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
returns <- closep - rutils::lagit(closep)
returns_adv <- rutils::lagit(returns, lagg=-1)
returns_high <- highp - rutils::lagit(highp)
returns_low <- lowp - rutils::lagit(lowp)



## run regressions of future returns against different indicators

# single indicator
indicator <- returns + returns_high + returns_low
reg_model <- lm(returns_adv ~ indicator)
summary(reg_model)

# three indicators - lower lows is most significant
reg_model <- lm(returns_adv ~ returns + returns_high + returns_low)
summary(reg_model)

# single indicator
# lower lows indicator works well in bearish periods
indicator <- (-returns - returns_high + returns_low)
indicator <- sign(indicator)
reg_model <- lm(returns_adv ~ indicator)
summary(reg_model)

# Simulate strategy
pnls <- cumsum(rutils::lagit(indicator)*returns)
colnames(pnls) <- "strategy"

# Plot
library(dygraphs)
dygraphs::dygraph(cbind(closep, pnls)) %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("red", "blue"))



###############
### Forecast and trade minutely stock returns, using static betas over design matrix

returns <- 6.5*60*HighFreq::run_returns(xtes=HighFreq::SPY, scalit=FALSE)
look_back <- 5
rets_lag <- 6.5*60*HighFreq::run_returns(xtes=HighFreq::SPY, lag=look_back, scalit=FALSE)
colnames(rets_lag) <- "rets_lag"
rets_lag2 <- 6.5*60*HighFreq::run_returns(xtes=HighFreq::SPY, lag=2*look_back, scalit=FALSE)
colnames(rets_lag2) <- "rets_lag2"
rets_adv <- rutils::lagit(rets_lag, lag=-look_back)
colnames(rets_adv) <- "rets_adv"
rets_adv2 <- rutils::lagit(rets_lag2, lag=-2*look_back)
colnames(rets_adv2) <- "rets_adv2"
variance <- 6.5*60^3*HighFreq::run_variance(ohlc=HighFreq::SPY, scalit=FALSE)
variance <- HighFreq::roll_vwap(ohlc=HighFreq::SPY, xtes=variance, look_back=look_back)
colnames(variance) <- "variance"
# var_lag2 <- HighFreq::roll_vwap(ohlc=HighFreq::SPY, xtes=variance, look_back=2*look_back)
# colnames(var_lag2) <- "var_lag2"

# skew <- 6.5*60^4*HighFreq::run_skew(ohlc=HighFreq::SPY)
# skew <- ifelse(variance==0, 0, skew/(variance)^(1.5))
# skew[1, ] <- 0
# skew <- roll_vwap(ohlc=HighFreq::SPY, xtes=skew, look_back=2*look_back)
# colnames(skew) <- "skew"
# set plot panels
# par(mfrow=c(2,1))
# chart_Series(HighFreq::SPY["2013-11-15"], name="SPY")
# chart_Series(SPY_design["2013-11-15"], name="posit")
# plot.zoo(posit[match(index(HighFreq::SPY["2013-11-15"]), index(HighFreq::SPY))], main="posit")
# bars with zero skew
# bar_s <- HighFreq::SPY["2013-11-15"][(skew["2013-11-15"]==0)]

# sharp_e <- HighFreq::run_sharpe(ohlc=HighFreq::SPY)
# sharpe_rolling <- roll_vwap(ohlc=HighFreq::SPY, xtes=sharp_e, look_back=look_back)
# sharpe_rolling <- as.numeric(stats::filter(sharp_e, filter=weightv, sides=2))
# colnames(sharpe_rolling) <- "sharpe"

hu_rst <- roll_hurst(ohlc=HighFreq::SPY, look_back=look_back)
colnames(hu_rst) <- "hurst"


# rets_lag <- lapply(1:(3*look_back), function(lag) {
#   6.5*60*HighFreq::run_returns(xtes=HighFreq::SPY, lag=lag, scalit=FALSE)
# })  # end lapply
rets_lag <- lapply(1:(3*look_back), HighFreq::run_returns,
                   xtes=HighFreq::SPY, colnum=4, scalit=FALSE)
rets_lag <- 6.5*60*rutils::do_call(cbind, rets_lag)
colnames(rets_lag) <- paste0("rets_lag_", 1:(3*look_back))



## create design matrix
SPY_design <- cbind(rets_lag2, sharpe_rolling)
# SPY_design <- cbind(rets_lag2, zscores[[3]], hu_rst, sharpe_rolling)
# colnames(SPY_design) <- c("returns", "variance", "skew", "hurst")
end_days <- xts::endpoints(SPY_design, "days")

## Apply rolling centering and scaling to the design matrix
# library(roll)
SPY_design <- roll::roll_scale(data=SPY_design, width=100*look_back, min_obs=1)
# Remove NAs
SPY_design[is.na(SPY_design)] <- 0
sum(is.na(SPY_design))


mo_del <- lm(rets_adv2 ~ SPY_design)
summary(mo_del)
coef(summary(mo_del))
betas <- -coef(summary(mo_del))[-1, 1]


## Calculate indicator from static betas and apply its rolling z-scores

indic <- matrix(rowSums(SPY_design %*% betas), ncol=1)
# indic <- roll::roll_scale(data=indic, width=6, min_obs=1)
# indic[is.na(indic)] <- 0
# Regress future returns against z-scores
mo_del <- lm(rets_adv2 ~ indic)
summary(mo_del)
# Calculate rolling range of z-scores
look_back <- 21
rangev <- cbind(min=-runMax(-indic, n=look_back),
                max=runMax(indic, n=look_back))
rangev[1:(look_back-1), ] <- rangev[look_back, ]
rangev <- rutils::lagit(rangev)
# Calculate posit and pnls from z-scores and rangev
posit <- ifelse(indic > 0.96*rangev[, "max"], -1,
                     ifelse(indic < 0.96*rangev[, "min"], 1, NA))
posit[1] <- 0
posit <- na.locf(posit, na.rm=FALSE)
# posit <- rutils::lagit(posit)
posit <- lapply(1:3, rutils::lagit, xtes=posit)
posit <- rutils::do_call(cbind, posit)
posit <- rowSums(posit)/NCOL(posit)
cum_pnls <- cumsum(posit*returns)
x11()
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


## Calculate the strategy success rate as the pnl divided by asset return volatility (to normalize the asset returns)
# result: the plot of the strategy success rate doesn't show any time variation or dependence on volatility
variance <- 6.5*60^3*HighFreq::run_variance(ohlc=HighFreq::SPY, scalit=TRUE)
variance <- sqrt(variance)
variance <- HighFreq::roll_vwap(ohlc=HighFreq::SPY, xtes=variance, look_back=look_back)
bar <- rutils::diffit(cum_pnls, lag=look_back) / variance
bar[1] <- 0
plot.zoo(bar[end_days], main="bar", xlab=NA, ylab=NA)
# the strategy average daily success rate isn't more successful when the volatility is higher
foo <- apply.daily(abs(bar), FUN=sum)
plot.zoo(foo, main="foo", xlab=NA, ylab=NA)


# Calculate correlation between strategy pnls and variance: there is no correlation
variance <- 6.5*60^3*HighFreq::run_variance(ohlc=HighFreq::SPY, scalit=TRUE)
variance <- sqrt(variance)
range(variance)
range(variance[variance > 1e-06])
pnls <- posit*returns
mo_del <- lm(pnls[variance > 1e-03] ~ variance[variance > 1e-03])
summary(mo_del)
plot(x=as.numeric(variance[variance > 1e-03]), y=as.numeric(pnls[variance > 1e-03]))


## Calculate the strategy success rate as the product of the forecast posit times the actual position (return direction)
# result: there is no significant correlation between the daily average success rate and the level of variance
bar <- apply.daily(posit*sign(returns), FUN=sum)
foo <- apply.daily(variance, FUN=sum)
mo_del <- lm(bar ~ foo)
summary(mo_del)
plot(x=as.numeric(foo), y=as.numeric(bar))
plot.zoo(cbind(foo, cumsum(bar)))



## Calculate z-scores and apply them to regression of future returns

# function for calculating z-scores
z_score <- function(width) {
  z_score <- roll::roll_scale(data=HighFreq::SPY[, 4], width=width, min_obs=1)
  z_score[is.na(z_score)] <- 0
  colnames(zscores) <- paste0("z_width_", width)
  z_score
}  # end z_score

# Calculate z-scores for different widths (lookbacks)
width_s <- 4:20
zscores <- lapply(width_s, z_score)
names(zscores) <- paste0("z_width_", width_s)
# zscores <- lapply(names(zscores), function(x) {
#   colnames(zscores[[x]]) <- x
#   zscores[[x]]
# })  # end lapply


# Regress future returns against z-scores
t_val <- function(zscores) {
  mo_del <- lm(rets_adv2 ~ zscores)
  coef(summary(mo_del))[2, 3]
}  # end t_val

t_vals <- sapply(zscores, t_val)
# t_vals <- cbind(width_s, t_val)
t(sapply(zscores, range))


# Calculate rolling range of z-scores

range(zscores[[3]])
rangev <- cbind(min=-runMax(-zscores[[3]], n=look_back),
                max=runMax(zscores[[3]], n=look_back))
rangev[1:(look_back-1), ] <- rangev[look_back, ]
rangev <- rutils::lagit(rangev)
# range(rangev[, 1])
# plot.zoo(rangev[end_days, 1], main="rolling min of z-scores", xlab=NA, ylab=NA)


# Calculate posit and pnls from z-scores and rangev

posit <- ifelse(zscores[[3]] > 0.96*rangev[, "max"], -1,
                     ifelse(zscores[[3]] < 0.96*rangev[, "min"], 1, NA))
posit[1] <- 0
posit <- na.locf(posit, na.rm=FALSE)
# posit <- rutils::lagit(posit)
posit <- lapply(1:3, rutils::lagit, xtes=posit)
posit <- rutils::do_call(cbind, posit)
posit <- -rowSums(posit)/NCOL(posit)
cum_pnls <- cumsum(posit*returns)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)

# Average number of trades per day
sum(abs(rutils::diffit(posit))) / mean(abs(posit)) / 2 / NROW(end_days)
# Average holding period (minutes)
2*NROW(posit) / sum(abs(rutils::diffit(posit))) * mean(abs(posit))


# Calculate total pnls from z-scores (dynamic threshold)
cum_pnl <- function(zscores, threshold=1.0, look_back=21, lag=3) {
  rangev <- cbind(min=-runMax(-zscores, n=look_back),
                  max=runMax(zscores, n=look_back))
  rangev[1:(look_back-1), ] <- rangev[look_back, ]
  rangev <- rutils::lagit(rangev)
  posit <- ifelse(zscores > threshold*rangev[, "max"], -1,
                       ifelse(zscores < threshold*rangev[, "min"], 1, NA))
  posit[1] <- 0
  posit <- na.locf(posit, na.rm=FALSE)
  posit <- lapply(1:lag, rutils::lagit, xtes=posit)
  posit <- rutils::do_call(cbind, posit)
  posit <- rowSums(posit)/NCOL(posit)
  cumsum(posit*returns)
}  # end cum_pnl

bar <- cum_pnl(zscores=zscores[[3]], threshold=0.96, look_back=21, lag=3)
plot.zoo(bar[end_days], main="cum_pnls", xlab=NA, ylab=NA)

# Calculate total pnls for different thresholds
thresholds <- seq(from=0.9, to=1.1, by=0.01)
bar <- lapply(thresholds, cum_pnl,
              zscores=zscores[[3]],
              look_back=21,
              lag=3)  # end lapply
names(bar) <- paste0("threshold_", thresholds)
unlist(lapply(bar, last))

# Calculate total pnls for different look_backs
look_backs <- seq(from=11, to=31, by=2)
bar <- lapply(look_backs, cum_pnl,
              zscores=zscores[[3]],
              threshold=0.96,
              lag=3)  # end lapply
names(bar) <- paste0("look_back_", look_backs)
unlist(lapply(bar, last))



# function for calculating posit from z-scores (static threshold)
z_pos <- function(zscores, threshold=2.0) {
  posit <- ifelse(abs(zscores) > threshold, sign(zscores), NA)
  posit[1] <- 0
  na.locf(posit, na.rm=FALSE)
}  # end z_pos

# Calculate time series of pnls from z-scores
posit <- z_pos(zscores[[3]], threshold=1.4)
posit <- lapply(1:3, rutils::lagit, xtes=posit)
posit <- rutils::do_call(cbind, posit)
posit <- rowSums(posit)/NCOL(posit)
cum_pnls <- -cumsum(posit*returns)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


# Calculate total pnls from z-scores
cum_pnl <- function(zscores, threshold=2.0, lag=3) {
  posit <- z_pos(zscores, threshold=threshold)
  posit <- lapply(1:lag, rutils::lagit, xtes=posit)
  posit <- rutils::do_call(cbind, posit)
  posit <- rowSums(posit)/NCOL(posit)
  -sum(posit*returns)
}  # end cum_pnl

cum_pnl(zscores[[3]], threshold=1.4, lag=3)

# Calculate total pnls for different thresholds
thresholds <- seq(from=1.0, to=3.0, by=0.1)
bar <- sapply(thresholds, cum_pnl,
              zscores=zscores[[3]],
              lag=3)
bar <- cbind(thresholds, bar)


posit <- lapply(zscores, z_pos, threshold=1.0)
posit <- rutils::do_call(cbind, posit)
posit <- rutils::lagit(posit, lag=1)


z_rets <- lapply(zscores, function(zscores) {
  posit <- ifelse(abs(zscores) > 2.0, sign(zscores), NA)
  posit[1] <- 0
  posit <- na.locf(posit, na.rm=FALSE)
  posit <- rutils::lagit(posit)
  -posit*returns
})  # end lapply

cum_pnls <- lapply(z_rets, cumsum)
cum_pnls <- rutils::do_call(cbind, cum_pnls)

cum_pnls <- rowSums(cum_pnls)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


## Simulate weighting the best performing strategies

weightv <- rutils::diffit(cum_pnls, lag=1000)
row_sums <- rowSums(abs(weightv))
row_sums[row_sums==0] <- 1.0
weightv <- weightv/row_sums
weightv <- rutils::lagit(weightv)

barr <- cumsum(rowSums(weightv*rutils::do_call(cbind, z_rets)))
plot.zoo(barr[end_days], main="cum_pnls")



## Perform rolling beta regressions in parallel
lm_roll <- roll::roll_lm(x=SPY_design,
                         y=rets_adv2,
                         width=3000*look_back)
betas <- lm_roll$coefficients[, -1]
betas[!complete.cases(betas), ] <- 0
# sum(is.na(betas))
betas <- rutils::lagit(betas, lag=2*look_back)
# betas <- na.omit(betas[, 2])
chart_Series(x=betas[end_days, "rets_lag2"], name="rolling betas")


## Perform rolling daily beta regressions
# Calculate daily endpoints
end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
# length of lookback window
look_back <- 3000*look_back

# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster, varlist=c("look_back", "rets_adv2", "SPY_design"))

# Perform parallel loop over daily endpoints - strange results independent of look_back
lm_roll <- parLapply(cluster, end_days, function(end_day) {
  indeks <- max(1, end_day-look_back):end_day
  summary(lm(rets_adv2[indeks, ] ~ SPY_design[indeks, ]))
})  # end parLapply

# stop R processes over cluster under Windows
stopCluster(cluster)

# Perform loop over daily endpoints
lm_roll <- lapply(end_days, function(end_day) {
  indeks <- max(1, end_day-look_back):end_day
  summary(lm(rets_adv2[indeks, ] ~ SPY_design[indeks, ]))
})  # end lapply


t_vals <- sapply(lm_roll, function(x) x$coefficients[-1, 3])
t_vals <- t(t_vals)
t_vals <- xts(t_vals, order.by=index(SPY_design[end_days, ]))
colnames(t_vals) <- colnames(SPY_design)
plot.zoo(cbind(t_vals[, 2], HighFreq::SPY[end_days, 4])["2010", ])

co_ef <- sapply(lm_roll, function(x) x$coefficients[-1, 1])
co_ef <- t(co_ef)
colnames(co_ef) <- colnames(SPY_design)
co_ef <- rutils::lagit(co_ef)
betas <- NA*SPY_design
betas[1, ] <- 0
betas[end_days, ] <- co_ef
betas <- na.locf(betas, na.rm=FALSE)

# Calculate posit and pnls
posit <- rowSums(SPY_design * betas)
# static betas work better than rolling regression
# betas <- c(rep(-1.0, 5), 0.00)
posit <- rowSums(SPY_design %*% betas)
# posit <- ifelse(abs(posit)>0.01, sign(posit), NA)
# posit[1] <- 0
# posit <- na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit)
posit <- rutils::roll_sum(posit, look_back=5) / 5
# histo_gram <- hist(posit, breaks=200, xlim=c(-0.05, 0.05))
# Average number of trades per day
sum(abs(rutils::diffit(posit))) / mean(abs(posit)) / 2 / NROW(end_days)
# Average holding period (minutes)
2*NROW(posit) / sum(abs(rutils::diffit(posit))) * mean(abs(posit))
# colnames(posit) <- "posit"
# plot.zoo(cbind(posit[end_days], HighFreq::SPY[end_days, 4])["2010", ])
pnls <- cumsum(posit*returns)
colnames(pnls) <- "SPY contrarian"
chart_Series(x=pnls["2008-01-29/2008-01-31"], name="pnls")
chart_Series(x=pnls[end_days, ], name="pnls")

# Apply moving average crossover strategy to resulting pnls
# Define aggregation window, decay parameter, and calculate VWAP
lambda <- 0.01
# Calculate EWMA prices
weightv <- exp(-lambda*1:(10*look_back+1))
weightv <- weightv/sum(weightv)
ew_ma <- stats::filter(pnls, filter=weightv, sides=1)
ew_ma <- as.numeric(ew_ma)
ew_ma[1:(10*look_back)] <- ew_ma[10*look_back+1]
# Calculate VWAP indicator
indic <- sign(pnls - ew_ma)
# Determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diffit(indic) != 0)
trade_dates <- which(trade_dates) + 1

# Calculate positions, either: -1, 0, or 1
pos_vwap <- rep(NA_integer_, NROW(pnls))
pos_vwap[1] <- 0
pos_vwap[trade_dates] <- indic[trade_dates]
pos_vwap <- na.locf(pos_vwap, na.rm=FALSE)
pos_vwap <- xts(pos_vwap, order.by=index(pnls))

# Calculate daily profits and losses
pnl_vwap <- cumsum(pos_vwap*rutils::diffit(pnls))
colnames(pnl_vwap) <- "SPY contrarian plus vwap"

# Plot
dates <- "2010-05-05/2010-05-07"
back_test <- cbind(HighFreq::SPY[, 4], cum_pnls)[dates, ]
# back_test <- cbind(HighFreq::SPY[, 4], sharpe_rolling)[dates, ]
back_test[, 1] <- back_test[, 1] - as.numeric(back_test[1, 1])
back_test[, 2] <- back_test[, 2] - as.numeric(back_test[1, 2])
back_test[, 2] <- 3*back_test[, 2] / max(back_test[, 2])

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(back_test, theme=plot_theme,
             name="SPY contrarian strategy plus vwap")
add_TA(cbind(HighFreq::SPY[, 4], posit)[dates, 2] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(cbind(HighFreq::SPY[, 4], posit)[dates, 2] < 0, on=-1,
       col="lightgrey", border="lightgrey")
legend("topleft", legend=c("pnls", "pnl_vwap"),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")



## Simulating minutely EWMA strategies


# Define function for simulating minutely EWMA crossover strategy
simu_ewma <- function(xtes, lambda=0.05, look_back=51) {
  # Calculate EWMA prices
  # weightv <- exp(-lambda*(1:look_back))
  # weightv <- weightv/sum(weightv)
  # ew_ma <- as.numeric(stats::filter(xtes, filter=weightv, sides=1))
  # ew_ma[1:(look_back-1)] <- ew_ma[look_back]
  ew_ma <- HighFreq::roll_vwap(xtes, look_back=look_back)
  # Determine dates right after EWMA has crossed prices
  indic <- sign(as.numeric(xtes[, 4] - ew_ma))
  trade_dates <- (rutils::diffit(indic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(xtes)]
  # Calculate positions, either: -1, 0, or 1
  posit <- rep(NA_integer_, NROW(xtes))
  posit[1] <- 0
  posit[trade_dates] <- rutils::lagit(indic)[trade_dates]
  na.locf(posit, na.rm=FALSE)
}  # end simu_ewma

end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
end_hours <- xts::endpoints(HighFreq::SPY, "hours")[-1]
positions_hours <- simu_ewma(xtes=HighFreq::SPY[end_hours], lambda=0.01, look_back=1001)
posit <- rep(NA_integer_, NROW(HighFreq::SPY))
posit[1] <- 0
posit[end_hours] <- positions_hours
posit <- na.locf(posit, na.rm=FALSE)
chart_Series(-cumsum(posit*returns)[end_days], name="SPY minutely vwap strategy")
posit <- xts(posit, order.by=index(returns))
add_TA(posit > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1,
       col="lightgrey", border="lightgrey")



# Perform parallel loop over lambdas
lambdas <- seq(0.001, 0.03, 0.001)
window_s <- seq(500, 1500, 100)

# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster, varlist=c("ohlc", "look_back", "simu_ewma"))
# Perform parallel loop over lambdas under Windows
returns <- parLapply(cluster, lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  simu_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "returns"]
})  # end parLapply


## Simple trend-following strategy

bar <- rutils::etfenv$returns[, "VTI"]
bar <- cumsum(bar*sign(rutils::lagit(bar)))
chart_Series(bar, name="Simple trend-following strategy")


bar <- -cumsum(returns*sign(rutils::lagit(skew, lag=2)))
bar <- rutils::roll_sum(rutils::lagit(skew), look_back=3) / 3
bar <- -cumsum(returns*sign(bar))


posit <- ifelse(abs(SPY_design)>0.052, sign(SPY_design), NA)
posit[1] <- 0
posit <- na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit)
pnls <- -cumsum(posit*returns)
colnames(pnls) <- "SPY skew contrarian"
chart_Series(x=pnls[end_days, ], name="SPY skew contrarian")

cum_pnl <- function(posit=skew, threshold=0.05, returns=returns) {
  posit <- ifelse(abs(posit)>threshold, sign(posit), NA)
  posit[1] <- 0
  posit <- na.locf(posit, na.rm=FALSE)
  posit <- rutils::lagit(posit)
  -sum(posit*returns)
}  # end cum_pnl

cum_pnl(threshold=0.045, returns=returns)

thresholds <- seq(from=0.04, to=0.065, by=0.001)
bar <- sapply(thresholds, cum_pnl,
              posit=as.numeric(rutils::lagit(SPY_design)),
              returns=returns)
bar <- cbind(thresholds, bar)


###############
### simulation of trading strategy

## cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(sharper, returns, endpoints) {
  be_st <- apply(sharper, 1, which.max)
  be_st <- rutils::lagit(be_st)
  be_st[1] <- 1
  returns <- lapply(seq_along(be_st), function(indeks) {
    returns[endpoints[indeks+1, 1]:endpoints[indeks+1, 2], be_st[indeks]]
  })  # end lapply
  sum(rutils::do_call(rbind, returns))
}  # end cum_pnl

cum_pnl(sharper, returns, endpoints)

# Switch to best asset with biggest SR
be_st <- apply(sharper, 1, which.max)
be_st <- rutils::lagit(be_st)
be_st[1] <- 1
bar <- lapply(seq_along(be_st), function(indeks) {
  returns[endpoints[indeks+1, 1]:endpoints[indeks+1, 2], be_st[indeks]]
})  # end lapply
bar <- rutils::do_call(rbind, bar)

chart_Series(x=cumsum(bar), name="Back-test of SR strategies")

## simulation for determining the optimal length of the lookback interval

library(rutils)
options(max.print=40)
ohlc <- HighFreq::SPY["/2008-03"]
indeks <- index(ohlc)
n_row <- NROW(ohlc)

# Calculate close to close percentage returns
closep <- Cl(ohlc)
returns <- 60*HighFreq::run_returns(xtes=HighFreq::SPY)

# Define aggregation window and decay parameter
look_back <- 51
lambda <- 0.05
# Calculate EWMA prices
weightv <- exp(-lambda*1:look_back)
weightv <- weightv/sum(weightv)
ew_ma <- stats::filter(closep, filter=weightv, sides=1)
ew_ma[1:(look_back-1)] <- ew_ma[look_back]
ew_ma <- xts(ew_ma, order.by=index(ohlc))
colnames(ew_ma) <- "VTI EWMA"

# Determine dates right after EWMA has crossed prices
indic <- sign(closep - ew_ma[, 2])
trade_dates <- (rutils::diffit(indic) != 0)
trade_dates <- which(trade_dates) + 1
# Calculate positions, either: -1, 0, or 1
posit <- rep(NA_integer_, NROW(closep))
posit[1] <- 0
posit[trade_dates] <- rutils::lagit(indic)[trade_dates]
posit <- na.locf(posit, na.rm=FALSE)
posit <- xts(posit, order.by=index(ohlc))

prices_lag <- rutils::lagit(closep)
position_lagged <- rutils::lagit(posit)
# Calculate daily profits and losses
returns <- position_lagged*(closep - prices_lag)
returns[trade_dates] <-
  position_lagged[trade_dates] *
  (openp[trade_dates] - prices_lag[trade_dates]) +
  posit[trade_dates] *
  (closep[trade_dates] - openp[trade_dates])
# Calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(returns)/sd(returns)/NROW(returns)
pnls <- cumsum(returns)
pnls <- cbind(closep-as.numeric(closep[1, ]), pnls)
colnames(pnls) <- c("VTI", "EWMA PnL")


# Define function for simulating daily EWMA crossover strategy
simu_ewma <- function(ohlc, lambda=0.05, look_back=51) {
  # Calculate EWMA prices
  weightv <- exp(-lambda*1:look_back)
  weightv <- weightv/sum(weightv)
  closep <- Cl(ohlc)
  ew_ma <- stats::filter(as.numeric(closep), filter=weightv, sides=1)
  ew_ma[1:(look_back-1)] <- ew_ma[look_back]
  # Determine dates right after EWMA has crossed prices
  indic <- xts(sign(as.numeric(closep) - ew_ma), order.by=index(ohlc))
  trade_dates <- (rutils::diffit(indic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(ohlc)]
  # Calculate positions, either: -1, 0, or 1
  posit <- rep(NA_integer_, NROW(closep))
  posit[1] <- 0
  posit[trade_dates] <- rutils::lagit(indic)[trade_dates]
  posit <- xts(na.locf(posit, na.rm=FALSE), order.by=index(ohlc))
  openp <- Op(ohlc)
  prices_lag <- rutils::lagit(closep)
  position_lagged <- rutils::lagit(posit)
  # Calculate daily profits and losses
  returns <- position_lagged*(closep - prices_lag)
  returns[trade_dates] <-
    position_lagged[trade_dates] *
    (openp[trade_dates] - prices_lag[trade_dates]) +
    posit[trade_dates] *
    (closep[trade_dates] - openp[trade_dates])
  output <- cbind(posit, returns)
  colnames(output) <- c("posit", "returns")
  output
}  # end simu_ewma


# Perform parallel loop over lambdas
lambdas <- seq(0.001, 0.03, 0.001)
lambdas <- seq(0.01, 1.0, 0.1)

# Initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(detectCores()-1)
clusterExport(cluster, varlist=c("ohlc", "look_back", "simu_ewma"))
# Perform parallel loop over lambdas under Windows
returns <- parLapply(cluster, lambdas, function(lambda) {
  library(quantmod)
  # Simulate EWMA strategy and calculate returns
  simu_ewma(ohlc=ohlc, lambda=lambda, look_back=look_back)[, "returns"]
})  # end parLapply


# set up loop over lookback windows
# length of lookback window
# look_back <- 11
# Define endpoints at end of every day
endpoints <- xts::endpoints(ohlc, on="days")
# num_agg <- n_row %/% look_back
# endpoints <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
nrows <- NROW(endpoints)
# startpoints are single-period lag of endpoints
startpoints <- endpoints[c(1, 1:(nrows-1))] + 1
# redefine endpoints
endpoints <- cbind(startpoints, endpoints)

# Perform parallel loop over returns
clusterExport(cluster, varlist=c("nrows", "endpoints"))
sharper <- parLapply(cluster, returns, function(returns) {
  sapply(2:nrows, function(indeks) {
    xtes <- returns[endpoints[indeks, 1]:endpoints[indeks, 2]]
    # Calculate annualized Sharpe ratio of returns
    sqrt(260)*sum(xtes)/sd(xtes)/NROW(xtes)
  })  # end sapply
})  # end parLapply

sharper <- rutils::do_call(cbind, sharper)

# Calculate dispersion of SRs of individual strategies over time periods
apply(sharper, 2, sd)
# Calculate dispersion of SRs of strategies in each time period
foo <- apply(sharper, 1, sd)
mean(foo)

# Calculate differences of SRs over periods
foo <- apply(sharper, 2, rutils::diffit)
dim(foo)
dim(sharper)
tail(foo)
tail(sharper)

# Are the sharper autocorrelated?
# yes, about -50%
bar <- foo[-NROW(foo), ]
bar <- rbind(rep(0, NCOL(foo)), bar)
bar <- bar*foo
colSums(bar) / apply(foo, 2, sd) / NROW(bar)

# Switch to best strategy
bar <- apply(sharper, 1, which.max)

# Switch to strategy with biggest differences of SRs over periods
bar <- apply(foo, 1, which.max)
bar <- rutils::lagit(bar)
bar[1] <- 1
bar <- lapply(2:nrows, function(indeks) {
  returns[[bar[indeks-1]]][endpoints[indeks, 1]:endpoints[indeks, 2]]
})  # end lapply
bar <- rutils::do_call(rbind, bar)

# Average over all strategies
bar <- rutils::do_call(cbind, returns)
bar <- xts(rowSums(bar), order.by=index(returns[[1]]))

chart_Series(x=-cumsum(bar), name="Back-test of EWMA strategies")


## Perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 50*(5:30)

foo <- sapply(look_backs, function(look_back) {
  # Define endpoints with beginning stub
  num_agg <- n_row %/% look_back
  endpoints <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  nrows <- NROW(endpoints)
  # startpoints are single-period lag of endpoints
  startpoints <- endpoints[c(1, 1:(nrows-1))] + 1
  # redefine endpoints
  endpoints <- cbind(startpoints, endpoints)
  
  # Perform parallel loop over returns
  clusterExport(cluster, varlist=c("nrows", "endpoints"))
  sharper <- parLapply(cluster, returns, function(returns) {
    sapply(2:nrows, function(indeks) {
      xtes <- returns[endpoints[indeks, 1]:endpoints[indeks, 2]]
      # Calculate annualized Sharpe ratio of returns
      sqrt(260)*sum(xtes)/sd(xtes)/NROW(xtes)
    })  # end sapply
  })  # end parLapply
  
  sharper <- rutils::do_call(cbind, sharper)
  sharper[which(is.na(sharper), arr.ind=TRUE)] <- 1
  
  # Calculate dispersion of SRs
  c(by_strategy=mean(apply(sharper, 2, sd)),
    by_period=mean(apply(sharper, 1, sd)))
})  # end sapply

foo <- t(foo)
dim(foo)
foo
plot(foo[, 1]/foo[, 2], t="l")

## end perform loop over lookback windows


# stop R processes over cluster under Windows
stopCluster(cluster)

