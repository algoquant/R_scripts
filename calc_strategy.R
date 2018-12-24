##############################
# Functions for simulating trending and mean reverting trading strategies.
##############################


# Calculate signals using various methods
calc_signal <- function(oh_lc, clo_se, de_sign, look_short, look_long=look_short, high_freq=TRUE) {
  # signal from t-value of trailing slope
  if (high_freq)
    # Calculate the signal as the residual of the rolling time series
    # regressions of the clo_se prices
    sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_short)
  else {
    # signal equal to trailing average returns
    # vari_ance <- HighFreq::roll_variance(oh_lc=oh_lc, look_back=look_long, sca_le=FALSE)
    # vari_ance[vari_ance==0] <- as.numeric(vari_ance[2])
    # re_turns <- rutils::diff_it(clo_se, lagg=look_short)/look_short/sqrt(vari_ance)
    # re_turns <- roll::roll_scale(data=re_turns, width=look_long, min_obs=1, center=FALSE)
    # vari_ance <- roll::roll_scale(data=vari_ance, width=look_long, min_obs=1, center=FALSE)
    # sig_nal <- re_turns + vari_ance
    # signal equal to trailing average returns
    sig_nal <- rutils::diff_it(clo_se, lagg=look_short)/sqrt(look_short)/sqrt(HighFreq::roll_variance(oh_lc=oh_lc, look_back=look_short, sca_le=FALSE))
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


# Calculate the signal as the difference between the price minus the moving average of the price
calc_ma <- function(oh_lc, clo_se, de_sign, look_back, high_freq=TRUE) {
  # signal from t-value of trailing slope
  # sign(clo_se - HighFreq::roll_vwap(oh_lc, look_back=look_back))
  (clo_se - HighFreq::roll_sum(clo_se, look_back=look_back)/look_back)
  # sig_nal[1:look_back, ] <- 0
  # sig_nal[is.na(sig_nal), ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  # sig_nal
}  # end calc_ma



# Simulate the pnls of mean reverting strategy
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



# Simulate the pnls of trending strategy
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



# Simulate pnls of a mean reverting strategy biased by a trending strategy
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


