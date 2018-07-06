###############
### Calculate matrix of OHLC technical indicators

library(rutils)

# Define OHLC data
oh_lc <- log(rutils::env_etf$VTI)
op_en <- Op(oh_lc)
hi_gh <- Hi(oh_lc)
lo_w <- Lo(oh_lc)
clo_se <- Cl(oh_lc)
vari_ance <- (hi_gh - lo_w)^2
colnames(vari_ance) <- "variance"
vol_at <- sqrt(vari_ance)
colnames(vol_at) <- "volat"
vol_ume <- Vo(oh_lc)
colnames(vol_ume) <- "volume"

# Define current and future returns
re_turns <- rutils::diff_it(clo_se)
colnames(re_turns) <- "returns"
# returns_adv <- rutils::lag_it(re_turns, lagg=-1)
# or
# returns_adv <- 0.5*(returns_adv + rutils::lag_it(returns_adv, lagg=-1))
look_back <- 2
returns_adv <- rutils::lag_it(HighFreq::roll_sum(re_turns, look_back=look_back), lagg=-look_back)/look_back
returns_adv <- xts(returns_adv, index(oh_lc))
colnames(returns_adv) <- "returns_adv"
# scale returns using sigmoid
# returns_adv <- plogis(returns_adv, scale=-quantile(returns_adv, 0.01))
# returns_adv <- (returns_adv - median(returns_adv))
# colnames(returns_adv) <- "returns_adv"

# Define OHLC technical indicators
# residuals of the regression of the time series of clo_se prices
date_s <- xts::.index(oh_lc)
look_back <- 11
z_scores <- HighFreq::roll_zscores(res_ponse=clo_se, 
                                   de_sign=matrix(as.numeric(date_s), nc=1), 
                                   look_back=look_back)
colnames(z_scores) <- "z_scores"
z_scores[1:3] <- 0
close_open <- (clo_se-op_en)
colnames(close_open) <- "close_open"
close_high <- (clo_se-hi_gh)
colnames(close_high) <- "close_high"
close_low <- (clo_se-lo_w)
colnames(close_low) <- "close_low"
# sk_ew <- ((hi_gh+lo_w) - (op_en+clo_se))
sk_ew <- ((hi_gh+lo_w) - (op_en+clo_se))
colnames(sk_ew) <- "sk_ew"
# moment_um <- ((clo_se-op_en) - (hi_gh-lo_w))
moment_um <- ((clo_se-op_en) - (hi_gh-lo_w)) + 1.0
colnames(moment_um) <- "moment_um"

# close_high <- (hi_gh - rutils::lag_it(hi_gh))
# close_low <- (lo_w - rutils::lag_it(lo_w))
# Select only independent indicators

indicator_s <- cbind(re_turns, vol_at, sk_ew)
# scale indicator_s using roll_scale()
look_back <- 60
indicator_s <- roll::roll_scale(data=indicator_s, width=look_back, min_obs=1)
indicator_s[1, ] <- 0
indicator_s <- cbind(indicator_s, z_scores)
indicator_s[1:3, ] <- 0
col_names <- colnames(indicator_s)

