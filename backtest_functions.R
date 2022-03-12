##############################
# Functions for simulating trending and mean reverting trading strategies.
##############################


## Calculate signals using various methods
calc_signal <- function(ohlc, closep, design, look_short, look_long=look_short, high_freq=TRUE) {
  # signal from t-value of trailing slope
  if (high_freq)
    # Calculate the signal as the residual of the rolling time series
    # regressions of the closep prices
    sig_nal <- HighFreq::roll_zscores(response=closep, design=design, look_back=look_short)
  else {
    # signal equal to trailing average returns
    # variance <- HighFreq::roll_variance(ohlc=ohlc, look_back=look_long, scalit=FALSE)
    # variance[variance==0] <- as.numeric(variance[2])
    # returns <- rutils::diffit(closep, lagg=look_short)/look_short/sqrt(variance)
    # returns <- roll::roll_scale(data=returns, width=look_long, min_obs=1, center=FALSE)
    # variance <- roll::roll_scale(data=variance, width=look_long, min_obs=1, center=FALSE)
    # sig_nal <- returns + variance
    # signal equal to trailing average returns
    sig_nal <- rutils::diffit(closep, lagg=look_short)/sqrt(look_short)/sqrt(HighFreq::roll_variance(ohlc=ohlc, look_back=look_short, scalit=FALSE))
    # sig_nal <- roll_zscores(response=closep, design=design, look_back=look_short)
  }  # end if
  sig_nal[1:look_short, ] <- 0
  # sig_nal <- HighFreq::roll_scale(matrixv=sig_nal, look_back=look_short, use_median=TRUE)
  # sig_nal[1:look_short, ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
  # sig_nal[is.na(sig_nal), ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  # rutils::lagit(sig_nal, lagg=trade_lag)
  sig_nal
}  # end calc_signal


## Calculate the signal as the difference between the price minus the moving average of the price
calc_ma <- function(ohlc, closep, design, look_back, high_freq=TRUE) {
  # signal from t-value of trailing slope
  # sign(closep - HighFreq::roll_vwap(ohlc, look_back=look_back))
  (closep - HighFreq::roll_vec(closep, look_back=look_back)/look_back)
  # sig_nal[1:look_back, ] <- 0
  # sig_nal[is.na(sig_nal), ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  # sig_nal
}  # end calc_ma



## Simulate the pnls of mean reverting strategy
sim_revert <- function(sig_nal, returns, close_high, close_high_count, close_low, close_low_count, en_ter, ex_it, trade_lag=1) {
  po_sit <- rep(NA_integer_, NROW(sig_nal))
  po_sit[1] <- 0
  sig_nal <- drop(sig_nal)
  # enter long contrarian position only on downtick
  # po_sit <- -sig_nal
  enter_long <- rutils::lagit(sig_nal < (-en_ter), lagg=trade_lag)
  # enter_long <- (sig_nal < (-en_ter))
  po_sit[enter_long & close_low] <- 1
  # po_sit[close_low] <- 1
  # po_sit[enter_long] <- 1
  # po_sit[(sig_nal < (-en_ter)) & (close_low_count==trade_lag)] <- 1
  # po_sit[(close_low_count == trade_lag)] <- 1
  # po_sit[(close_low_count) & close_low] <- 1
  # enter short contrarian position only on uptick
  enter_short <- rutils::lagit(sig_nal > en_ter, lagg=trade_lag)
  # enter_short <- (sig_nal > en_ter)
  po_sit[enter_short & close_high] <- (-1)
  # po_sit[close_high] <- (-1)
  # po_sit[enter_short] <- (-1)
  # po_sit[(sig_nal > en_ter) & (close_high_count==trade_lag)] <- (-1)
  # po_sit[(close_high_count == trade_lag)] <- (-1)
  # po_sit[(close_high_count) & close_high] <- (-1)
  po_sit[abs(sig_nal) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
  # pnls <- xts(sig_nal, index(returns))
  # pnls <- xts(po_sit, index(returns))
  # po_sit <- rutils::lagit(po_sit, lagg=trade_lag)
  pnls <- cumsum(po_sit*returns)
  colnames(pnls) <- "strategy"
  pnls
  # drop(pnls[NROW(pnls)])
}  # end sim_revert



## Simulate the pnls of trending strategy
sim_trend <- function(sig_nal, returns, en_ter, ex_it, close_high, close_low, trade_lag=1) {
  po_sit <- rep(NA_integer_, NROW(sig_nal))
  po_sit[1] <- 0
  # enter long trending position only on downtick
  po_sit[(sig_nal > 0) & close_low] <- 1
  # enter short trending position only on uptick
  po_sit[(sig_nal < 0) & close_high] <- (-1)
  # po_sit[abs(sig_nal) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
  po_sit <- rutils::lagit(po_sit, lagg=trade_lag)
  # pnls <- xts(po_sit, index(returns))
  # pnls <- xts(sig_nal, index(returns))
  pnls <- cumsum(po_sit*returns)
  colnames(pnls) <- "strategy"
  pnls
  # drop(pnls[NROW(pnls)])
}  # end sim_trend



## Simulate pnls of a mean reverting strategy biased by a trending strategy
# The mean reverting strategy can enter a long contrarian position only if the
# trending sig_nal is positive


# biased by a trending strategy
# biasing the reverting strategy, depending on the direction of the trending strategy
sim_revert_trending <- function(signal_short, signal_long, returns, en_ter, ex_it, close_high, close_low, trade_lag=1) {
  po_sit <- rep(NA_integer_, NROW(signal_short))
  po_sit[1] <- 0
  # enter long contrarian position only on downtick
  # po_sit[(signal_short < (-en_ter)) & (signal_long > 0) & close_low] <- 1
  po_sit[((signal_short-signal_long) < (-en_ter)) & close_low] <- 1
  # enter short contrarian position only on uptick
  # po_sit[(signal_short > en_ter) & (signal_long < 0) & close_high] <- (-1)
  po_sit[((signal_short-signal_long) > en_ter) & close_high] <- (-1)
  # po_sit[abs(signal_short) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
  po_sit <- rutils::lagit(po_sit, lagg=trade_lag)
  # pnls <- (signal_short-signal_long)
  # pnls <- xts(po_sit, index(sig_nal))
  pnls <- cumsum(po_sit*returns)
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
  vwapv <- HighFreq::roll_sum(t_series=closep*volumes, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(t_series=volumes, look_back=look_back)
  vwapv <- vwapv/volume_rolling
  vwapv[is.na(vwapv)] <- 0
  # Simulate strategy
  position_s <- rep(NA_integer_, NROW(closep))
  position_s[1] <- 0
  # Long positions
  indica_tor <- ((closep - vwapv) > threshold*rangev)
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, 1, position_s)
  # Short positions
  indica_tor <- ((closep - vwapv) < (-threshold*rangev))
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lagit(position_s, lagg=1)
  # Return positions, vwap, and log strategy returns
  datav <- cbind(positions=coeff*position_s, vwap=vwapv, pnls=coeff*returns*position_s)
  colnames(datav) <- c("positions", "vwap", "pnls")
  return(datav)
}  # end backtest_ewma



## Define Z-Score backtest function
backtest_zscores <- function(ohlc, look_back=252, lagg=2, threshold=0.0, coeff=1) {
  closep <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(closep)
  # rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
  # volumes <- quantmod::Vo(ohlc)
  # Simulate strategy
  indeks <- 1:NROW(ohlc)
  design <- matrix(indeks, nc=1)
  sig_nal <- HighFreq::roll_zscores(response=closep, design=design, look_back=look_back)
  colnames(sig_nal) <- "sig_nal"
  sig_nal[1:look_back] <- 0
  # Simulate strategy
  position_s <- rep(NA_integer_, NROW(closep))
  position_s[1] <- 0
  # Long positions
  indica_tor <- (sig_nal > threshold)
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, 1, position_s)
  # Short positions
  indica_tor <- (sig_nal < (-threshold))
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lagit(position_s, lagg=1)
  # Return positions and log strategy returns
  datav <- cbind(positions=coeff*position_s, pnls=coeff*returns*position_s)
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
  vwapv <- HighFreq::roll_sum(t_series=returns*volumes, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(t_series=volumes, look_back=look_back)
  vwapv <- vwapv/volume_rolling
  vwapv[is.na(vwapv)] <- 0
  # Calculate VWAP indicator
  # indica_tor <- sign(closep - vwapv)
  position_s <- sign(vwapv)
  # Lag the positions to trade in next period
  position_s <- rutils::lagit(position_s, lagg=1)
  # Return positions, vwap, and log strategy returns
  datav <- cbind(positions=coeff*position_s, vwap=vwapv, pnls=coeff*returns*position_s)
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
  vwapv <- HighFreq::roll_sum(t_series=closep*volumes, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(t_series=volumes, look_back=look_back)
  vwapv <- vwapv/volume_rolling
  vwapv[is.na(vwapv)] <- 0
  # Calculate VWAP indicator
  # indica_tor <- sign(closep - vwapv)
  indica_tor <- integer(NROW(ohlc))
  indica_tor <- ifelse((closep - vwapv) > threshold*rangev, 1, indica_tor)
  indica_tor <- ifelse((closep - vwapv) < (-threshold*rangev), -1, indica_tor)
  indic_sum <- HighFreq::roll_sum(t_series=indica_tor, look_back=lagg)
  indic_sum[1:lagg] <- 0
  position_s <- rep(NA_integer_, NROW(closep))
  position_s[1] <- 0
  position_s <- ifelse(indic_sum == lagg, 1, position_s)
  position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lagit(position_s, lagg=1)
  # Return positions, vwap, and log strategy returns
  datav <- cbind(positions=coeff*position_s, vwap=vwapv, pnls=coeff*returns*position_s)
  colnames(datav) <- c("positions", "vwap", "pnls")
  return(datav)
}  # end backtest_ewma_ts



## Define Z-Score backtest function
backtest_zscores_ts <- function(ohlc, look_back=252, lagg=2, threshold=0.0, coeff=1) {
  closep <- log(quantmod::Cl(ohlc))
  returns <- rutils::diffit(closep)
  # rangev <- (log(quantmod::Hi(ohlc)) - log(quantmod::Lo(ohlc)))
  # volumes <- quantmod::Vo(ohlc)
  # Simulate strategy
  indeks <- 1:NROW(ohlc)
  design <- matrix(indeks, nc=1)
  sig_nal <- HighFreq::roll_zscores(response=closep, design=design, look_back=look_back)
  colnames(sig_nal) <- "sig_nal"
  sig_nal[1:look_back] <- 0
  
  indica_tor <- integer(NROW(ohlc))
  indica_tor <- ifelse(sig_nal > threshold, 1, indica_tor)
  indica_tor <- ifelse(sig_nal < (-threshold), -1, indica_tor)
  indic_sum <- HighFreq::roll_sum(t_series=indica_tor, look_back=lagg)
  indic_sum[1:lagg] <- 0
  position_s <- rep(NA_integer_, NROW(closep))
  position_s[1] <- 0
  position_s <- ifelse(indic_sum == lagg, 1, position_s)
  position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lagit(position_s, lagg=1)
  # Return positions and log strategy returns
  datav <- cbind(positions=coeff*position_s, pnls=coeff*returns*position_s)
  colnames(datav) <- c("positions", "pnls")
  return(datav)
}  # end backtest_zscores_ts









