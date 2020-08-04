##############################
# Functions for simulating trending and mean reverting trading strategies.
##############################


## Calculate signals using various methods
calc_signal <- function(oh_lc, clo_se, de_sign, look_short, look_long=look_short, high_freq=TRUE) {
  # signal from t-value of trailing slope
  if (high_freq)
    # Calculate the signal as the residual of the rolling time series
    # regressions of the clo_se prices
    sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_short)
  else {
    # signal equal to trailing average returns
    # vari_ance <- HighFreq::roll_variance(oh_lc=oh_lc, look_back=look_long, scal_e=FALSE)
    # vari_ance[vari_ance==0] <- as.numeric(vari_ance[2])
    # re_turns <- rutils::diff_it(clo_se, lagg=look_short)/look_short/sqrt(vari_ance)
    # re_turns <- roll::roll_scale(data=re_turns, width=look_long, min_obs=1, center=FALSE)
    # vari_ance <- roll::roll_scale(data=vari_ance, width=look_long, min_obs=1, center=FALSE)
    # sig_nal <- re_turns + vari_ance
    # signal equal to trailing average returns
    sig_nal <- rutils::diff_it(clo_se, lagg=look_short)/sqrt(look_short)/sqrt(HighFreq::roll_variance(oh_lc=oh_lc, look_back=look_short, scal_e=FALSE))
    # sig_nal <- roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_short)
  }  # end if
  sig_nal[1:look_short, ] <- 0
  # sig_nal <- HighFreq::roll_scale(mat_rix=sig_nal, look_back=look_short, use_median=TRUE)
  # sig_nal[1:look_short, ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
  # sig_nal[is.na(sig_nal), ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  # rutils::lag_it(sig_nal, lagg=trade_lag)
  sig_nal
}  # end calc_signal


## Calculate the signal as the difference between the price minus the moving average of the price
calc_ma <- function(oh_lc, clo_se, de_sign, look_back, high_freq=TRUE) {
  # signal from t-value of trailing slope
  # sign(clo_se - HighFreq::roll_vwap(oh_lc, look_back=look_back))
  (clo_se - HighFreq::roll_vec(clo_se, look_back=look_back)/look_back)
  # sig_nal[1:look_back, ] <- 0
  # sig_nal[is.na(sig_nal), ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  # sig_nal
}  # end calc_ma



## Simulate the pnls of mean reverting strategy
sim_revert <- function(sig_nal, re_turns, close_high, close_high_count, close_low, close_low_count, en_ter, ex_it, trade_lag=1) {
  po_sit <- rep(NA_integer_, NROW(sig_nal))
  po_sit[1] <- 0
  sig_nal <- drop(sig_nal)
  # enter long contrarian position only on downtick
  # po_sit <- -sig_nal
  enter_long <- rutils::lag_it(sig_nal < (-en_ter), lagg=trade_lag)
  # enter_long <- (sig_nal < (-en_ter))
  po_sit[enter_long & close_low] <- 1
  # po_sit[close_low] <- 1
  # po_sit[enter_long] <- 1
  # po_sit[(sig_nal < (-en_ter)) & (close_low_count==trade_lag)] <- 1
  # po_sit[(close_low_count == trade_lag)] <- 1
  # po_sit[(close_low_count) & close_low] <- 1
  # enter short contrarian position only on uptick
  enter_short <- rutils::lag_it(sig_nal > en_ter, lagg=trade_lag)
  # enter_short <- (sig_nal > en_ter)
  po_sit[enter_short & close_high] <- (-1)
  # po_sit[close_high] <- (-1)
  # po_sit[enter_short] <- (-1)
  # po_sit[(sig_nal > en_ter) & (close_high_count==trade_lag)] <- (-1)
  # po_sit[(close_high_count == trade_lag)] <- (-1)
  # po_sit[(close_high_count) & close_high] <- (-1)
  po_sit[abs(sig_nal) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
  # pnl_s <- xts(sig_nal, index(re_turns))
  # pnl_s <- xts(po_sit, index(re_turns))
  # po_sit <- rutils::lag_it(po_sit, lagg=trade_lag)
  pnl_s <- cumsum(po_sit*re_turns)
  colnames(pnl_s) <- "strategy"
  pnl_s
  # drop(pnl_s[NROW(pnl_s)])
}  # end sim_revert



## Simulate the pnls of trending strategy
sim_trend <- function(sig_nal, re_turns, en_ter, ex_it, close_high, close_low, trade_lag=1) {
  po_sit <- rep(NA_integer_, NROW(sig_nal))
  po_sit[1] <- 0
  # enter long trending position only on downtick
  po_sit[(sig_nal > 0) & close_low] <- 1
  # enter short trending position only on uptick
  po_sit[(sig_nal < 0) & close_high] <- (-1)
  # po_sit[abs(sig_nal) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
  po_sit <- rutils::lag_it(po_sit, lagg=trade_lag)
  # pnl_s <- xts(po_sit, index(re_turns))
  # pnl_s <- xts(sig_nal, index(re_turns))
  pnl_s <- cumsum(po_sit*re_turns)
  colnames(pnl_s) <- "strategy"
  pnl_s
  # drop(pnl_s[NROW(pnl_s)])
}  # end sim_trend



## Simulate pnls of a mean reverting strategy biased by a trending strategy
# The mean reverting strategy can enter a long contrarian position only if the
# trending sig_nal is positive


# biased by a trending strategy
# biasing the reverting strategy, depending on the direction of the trending strategy
sim_revert_trending <- function(signal_short, signal_long, re_turns, en_ter, ex_it, close_high, close_low, trade_lag=1) {
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
  po_sit <- rutils::lag_it(po_sit, lagg=trade_lag)
  # pnl_s <- (signal_short-signal_long)
  # pnl_s <- xts(po_sit, index(sig_nal))
  pnl_s <- cumsum(po_sit*re_turns)
  colnames(pnl_s) <- "strategy"
  pnl_s
  # drop(pnl_s[NROW(pnl_s)])
}  # end sim_revert_trending


## Define EWMA backtest function
backtest_ewma <- function(oh_lc, look_back=252, lagg=2, thresh_old=0.0, co_eff=1) {
  clo_se <- log(quantmod::Cl(oh_lc))
  re_turns <- rutils::diff_it(clo_se)
  rang_e <- (log(quantmod::Hi(oh_lc)) - log(quantmod::Lo(oh_lc)))
  vol_ume <- quantmod::Vo(oh_lc)
  # Calculate VWAP indicator
  v_wap <- HighFreq::roll_sum(t_series=clo_se*vol_ume, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(t_series=vol_ume, look_back=look_back)
  v_wap <- v_wap/volume_rolling
  v_wap[is.na(v_wap)] <- 0
  # Simulate strategy
  position_s <- rep(NA_integer_, NROW(clo_se))
  position_s[1] <- 0
  # Long positions
  indica_tor <- ((clo_se - v_wap) > thresh_old*rang_e)
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, 1, position_s)
  # Short positions
  indica_tor <- ((clo_se - v_wap) < (-thresh_old*rang_e))
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lag_it(position_s, lagg=1)
  # Return positions, vwap, and log strategy returns
  da_ta <- cbind(positions=co_eff*position_s, vwap=v_wap, pnls=co_eff*re_turns*position_s)
  colnames(da_ta) <- c("positions", "vwap", "pnls")
  return(da_ta)
}  # end backtest_ewma



## Define Z-Score backtest function
backtest_zscores <- function(oh_lc, look_back=252, lagg=2, thresh_old=0.0, co_eff=1) {
  clo_se <- log(quantmod::Cl(oh_lc))
  re_turns <- rutils::diff_it(clo_se)
  # rang_e <- (log(quantmod::Hi(oh_lc)) - log(quantmod::Lo(oh_lc)))
  # vol_ume <- quantmod::Vo(oh_lc)
  # Simulate strategy
  in_dex <- 1:NROW(oh_lc)
  de_sign <- matrix(in_dex, nc=1)
  sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_back)
  colnames(sig_nal) <- "sig_nal"
  sig_nal[1:look_back] <- 0
  # Simulate strategy
  position_s <- rep(NA_integer_, NROW(clo_se))
  position_s[1] <- 0
  # Long positions
  indica_tor <- (sig_nal > thresh_old)
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, 1, position_s)
  # Short positions
  indica_tor <- (sig_nal < (-thresh_old))
  indica_tor <- HighFreq::roll_count(indica_tor)
  position_s <- ifelse(indica_tor >= lagg, -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lag_it(position_s, lagg=1)
  # Return positions and log strategy returns
  da_ta <- cbind(positions=co_eff*position_s, pnls=co_eff*re_turns*position_s)
  colnames(da_ta) <- c("positions", "pnls")
  return(da_ta)
}  # end backtest_zscores




## Define EWMA backtest function
backtest_ewmar <- function(oh_lc, look_back=252, lagg=2, thresh_old=0.0, co_eff=1) {
  clo_se <- log(quantmod::Cl(oh_lc))
  re_turns <- rutils::diff_it(clo_se)
  # rang_e <- (log(quantmod::Hi(oh_lc)) - log(quantmod::Lo(oh_lc)))
  vol_ume <- quantmod::Vo(oh_lc)
  # Simulate strategy
  v_wap <- HighFreq::roll_sum(t_series=re_turns*vol_ume, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(t_series=vol_ume, look_back=look_back)
  v_wap <- v_wap/volume_rolling
  v_wap[is.na(v_wap)] <- 0
  # Calculate VWAP indicator
  # indica_tor <- sign(clo_se - v_wap)
  position_s <- sign(v_wap)
  # Lag the positions to trade in next period
  position_s <- rutils::lag_it(position_s, lagg=1)
  # Return positions, vwap, and log strategy returns
  da_ta <- cbind(positions=co_eff*position_s, vwap=v_wap, pnls=co_eff*re_turns*position_s)
  colnames(da_ta) <- c("positions", "vwap", "pnls")
  return(da_ta)
}  # end backtest_ewmar



###############
# Legacy functions for 2020 backtest for TS fund


## Define EWMA backtest function
backtest_ewma_ts <- function(oh_lc, look_back=252, lagg=2, thresh_old=0.0, co_eff=1) {
  clo_se <- log(quantmod::Cl(oh_lc))
  re_turns <- rutils::diff_it(clo_se)
  rang_e <- (log(quantmod::Hi(oh_lc)) - log(quantmod::Lo(oh_lc)))
  vol_ume <- quantmod::Vo(oh_lc)
  # Simulate strategy
  v_wap <- HighFreq::roll_sum(t_series=clo_se*vol_ume, look_back=look_back)
  volume_rolling <- HighFreq::roll_sum(t_series=vol_ume, look_back=look_back)
  v_wap <- v_wap/volume_rolling
  v_wap[is.na(v_wap)] <- 0
  # Calculate VWAP indicator
  # indica_tor <- sign(clo_se - v_wap)
  indica_tor <- integer(NROW(oh_lc))
  indica_tor <- ifelse((clo_se - v_wap) > thresh_old*rang_e, 1, indica_tor)
  indica_tor <- ifelse((clo_se - v_wap) < (-thresh_old*rang_e), -1, indica_tor)
  indic_sum <- HighFreq::roll_sum(t_series=indica_tor, look_back=lagg)
  indic_sum[1:lagg] <- 0
  position_s <- rep(NA_integer_, NROW(clo_se))
  position_s[1] <- 0
  position_s <- ifelse(indic_sum == lagg, 1, position_s)
  position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lag_it(position_s, lagg=1)
  # Return positions, vwap, and log strategy returns
  da_ta <- cbind(positions=co_eff*position_s, vwap=v_wap, pnls=co_eff*re_turns*position_s)
  colnames(da_ta) <- c("positions", "vwap", "pnls")
  return(da_ta)
}  # end backtest_ewma_ts



## Define Z-Score backtest function
backtest_zscores_ts <- function(oh_lc, look_back=252, lagg=2, thresh_old=0.0, co_eff=1) {
  clo_se <- log(quantmod::Cl(oh_lc))
  re_turns <- rutils::diff_it(clo_se)
  # rang_e <- (log(quantmod::Hi(oh_lc)) - log(quantmod::Lo(oh_lc)))
  # vol_ume <- quantmod::Vo(oh_lc)
  # Simulate strategy
  in_dex <- 1:NROW(oh_lc)
  de_sign <- matrix(in_dex, nc=1)
  sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_back)
  colnames(sig_nal) <- "sig_nal"
  sig_nal[1:look_back] <- 0
  
  indica_tor <- integer(NROW(oh_lc))
  indica_tor <- ifelse(sig_nal > thresh_old, 1, indica_tor)
  indica_tor <- ifelse(sig_nal < (-thresh_old), -1, indica_tor)
  indic_sum <- HighFreq::roll_sum(t_series=indica_tor, look_back=lagg)
  indic_sum[1:lagg] <- 0
  position_s <- rep(NA_integer_, NROW(clo_se))
  position_s[1] <- 0
  position_s <- ifelse(indic_sum == lagg, 1, position_s)
  position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  # Lag the positions to trade in next period
  position_s <- rutils::lag_it(position_s, lagg=1)
  # Return positions and log strategy returns
  da_ta <- cbind(positions=co_eff*position_s, pnls=co_eff*re_turns*position_s)
  colnames(da_ta) <- c("positions", "pnls")
  return(da_ta)
}  # end backtest_zscores_ts









