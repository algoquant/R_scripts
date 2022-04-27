##############################
# Functions for simulating trending and mean reverting trading strategies.
##############################

# Source this file in R:
# source("/Users/jerzy/Develop/R/back_test.R")

## Calculate signals using various methods
calc_signal <- function(ohlc, closep, predictor, look_short, look_long=look_short, high_freq=TRUE) {
  # signal from t-value of trailing slope
  if (high_freq) {
    # Calculate the signal as the residual of the rolling time series
    # regressions of the closep prices
    rollreg <- HighFreq::roll_reg(response=closep, predictor=predictor, look_back=look_short)
    zscores <- rollreg[, NCOL(rollreg), drop=TRUE]
  }
  else {
    # signal equal to trailing average returns
    # variance <- HighFreq::roll_variance(ohlc=ohlc, look_back=look_long, scalit=FALSE)
    # variance[variance==0] <- as.numeric(variance[2])
    # returns <- rutils::diffit(closep, lagg=look_short)/look_short/sqrt(variance)
    # returns <- roll::roll_scale(data=returns, width=look_long, min_obs=1, center=FALSE)
    # variance <- roll::roll_scale(data=variance, width=look_long, min_obs=1, center=FALSE)
    # zscores <- returns + variance
    # signal equal to trailing average returns
    zscores <- rutils::diffit(closep, lagg=look_short)/sqrt(look_short)/sqrt(HighFreq::roll_variance(ohlc=ohlc, look_back=look_short, scalit=FALSE))
  }  # end if
  zscores[1:look_short, ] <- 0
  # zscores <- HighFreq::roll_scale(matrixv=zscores, look_back=look_short, use_median=TRUE)
  # zscores[1:look_short, ] <- 0
  zscores[is.infinite(zscores)] <- NA
  zscores <- zoo::na.locf(zscores, na.rm=FALSE)
  # zscores[is.na(zscores), ] <- 0
  # zscores[is.infinite(zscores), ] <- 0
  # rutils::lagit(zscores, lagg=trade_lag)
  zscores
}  # end calc_signal


## Calculate the signal as the difference between the price minus the moving average of the price
calc_ma <- function(ohlc, closep, predictor, look_back, high_freq=TRUE) {
  # signal from t-value of trailing slope
  # sign(closep - HighFreq::roll_vwap(ohlc, look_back=look_back))
  (closep - HighFreq::roll_vec(closep, look_back=look_back)/look_back)
  # zscores[1:look_back, ] <- 0
  # zscores[is.na(zscores), ] <- 0
  # zscores[is.infinite(zscores), ] <- 0
  # zscores
}  # end calc_ma



## Simulate the pnls of mean reverting strategy
sim_revert <- function(zscores, returns, close_high, close_high_count, close_low, close_low_count, enter, exit, trade_lag=1) {
  posit <- rep(NA_integer_, NROW(zscores))
  posit[1] <- 0
  zscores <- drop(zscores)
  # enter long contrarian position only on downtick
  # posit <- -zscores
  enter_long <- rutils::lagit(zscores < (-enter), lagg=trade_lag)
  # enter_long <- (zscores < (-enter))
  posit[enter_long & close_low] <- 1
  # posit[close_low] <- 1
  # posit[enter_long] <- 1
  # posit[(zscores < (-enter)) & (close_low_count==trade_lag)] <- 1
  # posit[(close_low_count == trade_lag)] <- 1
  # posit[(close_low_count) & close_low] <- 1
  # enter short contrarian position only on uptick
  enter_short <- rutils::lagit(zscores > enter, lagg=trade_lag)
  # enter_short <- (zscores > enter)
  posit[enter_short & close_high] <- (-1)
  # posit[close_high] <- (-1)
  # posit[enter_short] <- (-1)
  # posit[(zscores > enter) & (close_high_count==trade_lag)] <- (-1)
  # posit[(close_high_count == trade_lag)] <- (-1)
  # posit[(close_high_count) & close_high] <- (-1)
  posit[abs(zscores) < exit] <- 0
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  # pnls <- xts(zscores, index(returns))
  # pnls <- xts(posit, index(returns))
  # posit <- rutils::lagit(posit, lagg=trade_lag)
  pnls <- cumsum(posit*returns)
  colnames(pnls) <- "strategy"
  pnls
  # drop(pnls[NROW(pnls)])
}  # end sim_revert



## Simulate the pnls of trending strategy
sim_trend <- function(zscores, returns, enter, exit, close_high, close_low, trade_lag=1) {
  posit <- rep(NA_integer_, NROW(zscores))
  posit[1] <- 0
  # enter long trending position only on downtick
  posit[(zscores > 0) & close_low] <- 1
  # enter short trending position only on uptick
  posit[(zscores < 0) & close_high] <- (-1)
  # posit[abs(zscores) < exit] <- 0
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- rutils::lagit(posit, lagg=trade_lag)
  # pnls <- xts(posit, index(returns))
  # pnls <- xts(zscores, index(returns))
  pnls <- cumsum(posit*returns)
  colnames(pnls) <- "strategy"
  pnls
  # drop(pnls[NROW(pnls)])
}  # end sim_trend



## Simulate pnls of a mean reverting strategy biased by a trending strategy
# The mean reverting strategy can enter a long contrarian position only if the
# trending zscores is positive


# biased by a trending strategy
# biasing the reverting strategy, depending on the direction of the trending strategy
sim_revert_trending <- function(signal_short, signal_long, returns, enter, exit, close_high, close_low, trade_lag=1) {
  posit <- rep(NA_integer_, NROW(signal_short))
  posit[1] <- 0
  # enter long contrarian position only on downtick
  # posit[(signal_short < (-enter)) & (signal_long > 0) & close_low] <- 1
  posit[((signal_short-signal_long) < (-enter)) & close_low] <- 1
  # enter short contrarian position only on uptick
  # posit[(signal_short > enter) & (signal_long < 0) & close_high] <- (-1)
  posit[((signal_short-signal_long) > enter) & close_high] <- (-1)
  # posit[abs(signal_short) < exit] <- 0
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- rutils::lagit(posit, lagg=trade_lag)
  # pnls <- (signal_short-signal_long)
  # pnls <- xts(posit, index(zscores))
  pnls <- cumsum(posit*returns)
  colnames(pnls) <- "strategy"
  pnls
  # drop(pnls[NROW(pnls)])
}  # end sim_revert_trending


## Define EWMA backtest function
backtest_ewma <- function(ohlc, look_back=252, lagg=2, threshold=0.0, coeff=1) {
  closep <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(closep)
  rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
  volumes <- quantmod::Vo(ohlc)
  # Calculate VWAP indicator
  vwapv <- HighFreq::roll_sum(tseries=closep*volumes, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(tseries=volumes, look_back=look_back)
  vwapv <- vwapv/volume_rolling
  vwapv[is.na(vwapv)] <- 0
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
  # Return positions, vwap, and log strategy returns
  datav <- cbind(positions=coeff*posit, vwap=vwapv, pnls=coeff*returns*posit)
  colnames(datav) <- c("positions", "vwap", "pnls")
  return(datav)
}  # end backtest_ewma



## Define Z-scores backtest function
backtest_zscores <- function(ohlc, look_back=NULL, lambda=NULL, lagg=2, threshold=0.0, coeff=1) {
  closep <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(closep)
  # rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
  # volumes <- quantmod::Vo(ohlc)
  # Simulate strategy
  indeks <- 1:NROW(ohlc)
  predictor <- matrix(indeks, nc=1)
  if (!is.null(look_back)) {
    # Perform rolling regressions
    zscores <- HighFreq::roll_reg(response=closep, predictor=predictor, look_back=look_back)
    zscores <- zscores[, NCOL(zscores), drop=FALSE]
    zscores[1:look_back, ] <- 0
  }
  else if (!is.null(lambda)) {
    # Perform running regressions
    zscores <- HighFreq::run_reg(response=closep, predictor=predictor, lambda=lambda, method="scale")
    zscores <- zscores[, 1, drop=FALSE]
  }  # end if
  colnames(zscores) <- "zscores"
  # Simulate strategy
  posit <- rep(NA_integer_, NROW(closep))
  posit[1] <- 0
  # Long and short positions
  indic <- (zscores > threshold)
  indic <- HighFreq::roll_count(indic)
  posit <- ifelse(indic >= lagg, coeff, posit)
  indic <- (zscores < (-threshold))
  indic <- HighFreq::roll_count(indic)
  posit <- ifelse(indic >= lagg, -coeff, posit)
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  # Lag the positions to trade in next period
  posit <- rutils::lagit(posit, lagg=1)
  # Return positions and log strategy returns
  datav <- cbind(positions=posit, pnls=returns*posit)
  colnames(datav) <- c("positions", "pnls")
  return(datav)
}  # end backtest_zscores


## Define EWMA backtest function
backtest_ewmar <- function(ohlc, look_back=252, lagg=2, threshold=0.0, coeff=1) {
  closep <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(closep)
  # rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
  volumes <- quantmod::Vo(ohlc)
  # Simulate strategy
  vwapv <- HighFreq::roll_sum(tseries=returns*volumes, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(tseries=volumes, look_back=look_back)
  vwapv <- vwapv/volume_rolling
  vwapv[is.na(vwapv)] <- 0
  # Calculate VWAP indicator
  # indic <- sign(closep - vwapv)
  posit <- sign(vwapv)
  # Lag the positions to trade in next period
  posit <- rutils::lagit(posit, lagg=1)
  # Return positions, vwap, and log strategy returns
  datav <- cbind(positions=coeff*posit, vwap=vwapv, pnls=coeff*returns*posit)
  colnames(datav) <- c("positions", "vwap", "pnls")
  return(datav)
}  # end backtest_ewmar



###############
# Legacy functions for 2020 backtest for TS fund


## Define EWMA backtest function
backtest_ewma_ts <- function(ohlc, look_back=252, lagg=2, threshold=0.0, coeff=1) {
  closep <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(closep)
  rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
  volumes <- quantmod::Vo(ohlc)
  # Simulate strategy
  vwapv <- HighFreq::roll_sum(tseries=closep*volumes, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(tseries=volumes, look_back=look_back)
  vwapv <- vwapv/volume_rolling
  vwapv[is.na(vwapv)] <- 0
  # Calculate VWAP indicator
  # indic <- sign(closep - vwapv)
  indic <- integer(NROW(ohlc))
  indic <- ifelse((closep - vwapv) > threshold*rangev, 1, indic)
  indic <- ifelse((closep - vwapv) < (-threshold*rangev), -1, indic)
  indic_sum <- HighFreq::roll_sum(tseries=indic, look_back=lagg)
  indic_sum[1:lagg] <- 0
  posit <- rep(NA_integer_, NROW(closep))
  posit[1] <- 0
  posit <- ifelse(indic_sum == lagg, 1, posit)
  posit <- ifelse(indic_sum == (-lagg), -1, posit)
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  # Lag the positions to trade in next period
  posit <- rutils::lagit(posit, lagg=1)
  # Return positions, vwap, and log strategy returns
  datav <- cbind(positions=coeff*posit, vwap=vwapv, pnls=coeff*returns*posit)
  colnames(datav) <- c("positions", "vwap", "pnls")
  return(datav)
}  # end backtest_ewma_ts



## Define Z-scores backtest function
backtest_zscores_ts <- function(ohlc, look_back=252, lagg=2, threshold=0.0, coeff=1) {
  closep <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(closep)
  # rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
  # volumes <- quantmod::Vo(ohlc)
  # Simulate strategy
  indeks <- 1:NROW(ohlc)
  predictor <- matrix(indeks, nc=1)
  rollreg <- HighFreq::roll_reg(response=closep, predictor=predictor, look_back=look_back)
  zscores <- rollreg[, NCOL(rollreg)]
  colnames(zscores) <- "zscores"
  zscores[1:look_back] <- 0
  
  indic <- integer(NROW(ohlc))
  indic <- ifelse(zscores > threshold, 1, indic)
  indic <- ifelse(zscores < (-threshold), -1, indic)
  indic_sum <- HighFreq::roll_sum(tseries=indic, look_back=lagg)
  indic_sum[1:lagg] <- 0
  posit <- rep(NA_integer_, NROW(closep))
  posit[1] <- 0
  posit <- ifelse(indic_sum == lagg, 1, posit)
  posit <- ifelse(indic_sum == (-lagg), -1, posit)
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  # Lag the positions to trade in next period
  posit <- rutils::lagit(posit, lagg=1)
  # Return positions and log strategy returns
  datav <- cbind(positions=coeff*posit, pnls=coeff*returns*posit)
  colnames(datav) <- c("positions", "pnls")
  return(datav)
}  # end backtest_zscores_ts









