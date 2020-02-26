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

## Calculate a "kitchen sink" design matrix called de_sign

# Set up data from OHLC prices

library(HighFreq)

look_back <- 252

oh_lc <- rutils::etf_env$VTI
op_en <- quantmod::Op(oh_lc)
hi_gh <- quantmod::Hi(oh_lc)
lo_w <- quantmod::Lo(oh_lc)
clo_se <- quantmod::Cl(oh_lc)
re_turns <- rutils::diff_it(log(clo_se))
colnames(re_turns) <- "returns"
vol_ume <- Vo(oh_lc)
colnames(volume) <- "volume"


# Calculate various indicators

# wippp

vari_ance <- HighFreq::roll_variance(oh_lc=log(oh_lc), look_back=look_back, scal_e=FALSE)
colnames(vari_ance) <- "variance"

sig_nal <- HighFreq::roll_zscores(res_ponse=close_num, de_sign=de_sign, look_back=look_back)
colnames(sig_nal) <- "sig_nal"
sig_nal[1:look_back] <- 0

roll_sharpe

# Residuals of the regression of the time series of clo_se prices
date_s <- xts::.index(oh_lc)
# foo <- unique(date_s)
de_sign <- matrix(date_s, nc=1)
# foo <- MASS::ginv(de_sign)
look_back <- 11
z_scores <- HighFreq::roll_zscores(res_ponse=clo_se, 
                                   de_sign=de_sign, 
                                   look_back=look_back)
colnames(z_scores) <- "z_scores"
z_scores[1:3] <- 0

moment_um <- ((clo_se-op_en) - (hi_gh-lo_w)) + 1.0
colnames(moment_um) <- "moment_um"


lambda_s <- 0:31/30

weight_s <- sapply(lambda_s, function(lamb_da) {
  op_tim <- optim(par=rep(1.0, n_weights),
                  fn=object_ive,
                  method="L-BFGS-B",
                  upper=rep(10, n_weights),
                  lower=rep(-10, n_weights),
                  ex_cess=ex_cess,
                  cov_mat=cov_mat,
                  lamb_da=lamb_da,
                  al_pha=1)
  op_tim$par/sum(op_tim$par)
})  # end sapply





# Calculate the coefficients of the AR(11) model for vol_ume.
# You must perform a regression on lagged vol_ume using matrix 
# algebra, not arima().

# Define design matrix
de_sign <- sapply(1:11, function(lagg) {
  rutils::lag_it(vol_ume, lagg=lagg)
})  # end sapply
# generalized inverse of design matrix
design_inv <- MASS::ginv(de_sign)
# Regression coefficients with response equal to ari_ma
co_eff <- drop(design_inv %*% vol_ume)

# You should get the following output:
co_eff
# [1] -0.544335000 -0.346797195 -0.393230034 -0.218591436
# [5] -0.199455627 -0.232981341 -0.130858453 -0.010743125
# [9]  0.099890590  0.032009602  0.003775291


# 2. (20pts) Create a function called back_test() which 
# backtests an AR(p) forecasting model on a vector of data.
# back_test() should calculate the out-of-sample forecasts 
# of the AR(p) forecasting model, and return their MSE.
# The function back_test() should accept the following 
# arguments:
#  se_ries - a vector of data,
#  look_back - lookback interval,
#  or_der - order of the AR(p) model, 
#  end_points = the end points: seq_along(se_ries),

back_test <- function(se_ries, look_back=100, or_der=5, end_points=end_points, n_rows=n_rows) {
  # Calculate aggregations.
  de_sign <- sapply(1:or_der, function(lagg) {
    rutils::lag_it(se_ries, lagg=lagg)
  })  # end sapply
  de_sign <- cbind(se_ries, de_sign)
  start_points <- c(rep_len(1, look_back-1), 
                    end_points[1:(NROW(end_points)-look_back+1)])
  # Perform rolling forecasting
  fore_casts <- sapply(end_points[-(1:(or_der-1))], function(it) {
    de_sign <- de_sign[start_points[it]:end_points[it], ]
    # Calculate AR(p) coefficients
    design_inv <- MASS::ginv(de_sign[, -1])
    co_eff <- drop(design_inv %*% de_sign[, 1])
    de_sign[(NROW(de_sign)-or_der+1):NROW(de_sign), 1] %*% co_eff
  })  # end sapply
  fore_casts <- c(rep(fore_casts[1], or_der-1), fore_casts)
  # Lag the forecasts to push them out-of-sample
  fore_casts <- rutils::lag_it(fore_casts)
  mean((fore_casts-se_ries)^2)
}  # end back_test

# Run back_test() as follows:

end_points <- seq_along(vol_ume)
n_rows <- NROW(end_points)

back_test(se_ries=vol_ume, look_back=300, or_der=11, end_points=end_points)

# You should get the following output:
# [1] 1.73671


# 3. (20pts) Perform two separate sapply() loops over 
# back_test(), to determine the optimal values of or_der 
# and look_back.

# Create vector of moment orders for performing the sapply() loop
or_ders <- 9:25

# First perform an sapply() loop over or_der=or_ders, 
# and with look_back=200.

mse_s <- sapply(or_ders, back_test, se_ries=vol_ume, look_back=200, end_points=end_points)

# You should get the following output:
round(mse_s, 3)
#  [1] 1.725 1.629 1.777 1.592 1.787 1.772 1.847 1.586 1.689 1.753
# [11] 2.210 2.077 1.966 2.354 2.078 2.943 2.409

# Notice that the MSE increases with higher AR(p) order 
# because of model overfitting.

# Create a plot similar to backtest_ar_order.png

x11()
plot(x=or_ders, y=mse_s, t="l", lwd=2, xlab="AR(p) Order", ylab="",
     main="MSE as Function of AR(p) Order")


# Second perform an sapply() loop over look_back=(50*(2:10)), 
# and with or_der=11.

# Create vector of look_backs for performing the sapply() loop
look_backs <- 50*(2:10)

mse_s <- sapply(look_backs, back_test, se_ries=vol_ume, or_der=11, end_points=end_points)

# You should get the following output:
round(mse_s, 3)
# [1] 1.871 1.805 1.777 1.762 1.737 1.730 1.730 1.726 1.721

# Create a plot similar to backtest_ar_lookback.png

plot(x=look_backs, y=mse_s, t="l", lwd=2, xlab="Lookback length", ylab="",
     main="MSE as Function of AR(p) Lookback")




###########
# Perform aggregations by applying a function over a vector of endpoints

## Load minutely price data
sym_bol <- load("C:/Develop/data/SPY.RData")

# Plot average hourly trading volumes
price_s <- Vo(SPY["2010-05-05/2010-05-07"])
var_running <- period.apply(
  x=price_s,
  INDEX=xts::endpoints(price_s, "hours"),
  sum)
chart_Series(vol_ume, name="hourly volumes")
in_dex <- format(index(vol_ume), "%H:%M")
vol_ume <- tapply(X=vol_ume, INDEX=in_dex, FUN=mean)
vol_ume <- xts(as.vector(vol_ume), order.by=as.POSIXct(names(vol_ume), format="%H:%M"))
# Normalize and plot vol_ume
in_dex <- c(30, diff(index(vol_ume)))
chart_Series(vol_ume/in_dex, name="hourly volumes")


## agg_regate() calculates an aggregation of an xts series
# and returns an xts series with a single row
agg_regate <- function(da_ta) {
  agg_regation <- c(max=max(da_ta), min=min(da_ta))
  xts(t(agg_regation), order.by=end(da_ta))
}  # end agg_regate
agg_regate(price_s)


## Perform aggregations using period.apply(), apply_rolling() and apply_xts()
# apply_rolling() and apply_xts() are legacy functions from utilLib.R

# Extract closing prices for a single day of data
price_s <- Cl(SPY["2010-05-06"])

end_points <- xts::endpoints(price_s, "hours")
agg_regations <- period.apply(x=price_s,
                            INDEX=end_points,
                            FUN=agg_regate)
foo_bar <- apply_rolling(x_ts=price_s,
                         end_points=end_points,
                         func_tion=agg_regate)
agg_regations <- apply_xts(x_ts=price_s,
                         end_points=end_points,
                         func_tion=agg_regate)

# Verify that apply_rolling() and apply_xts() produce identical output
all.equal(agg_regations, foo_bar)

# Compare speed of apply_rolling() versus apply_xts()
library(microbenchmark)
summary(microbenchmark(
  agg_sapply=apply_rolling(x_ts=price_s,
                           end_points=end_points,
                           func_tion=agg_regate),
  agg_lapply=apply_xts(x_ts=price_s,
                                   end_points=end_points,
                                   func_tion=agg_regate),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

agg_regations <- apply_rolling(x_ts=price_s,
                               end_points=end_points,
                               look_back=3,
                               func_tion=agg_regate)
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

re_turns <- 86400*diff(Cl(SPY))/c(1, diff(.index(SPY)))
re_turns[1, ] <- 0
sum(is.na(re_turns))
re_turns <- na.locf(re_turns, na.rm=FALSE)

sd(x=coredata(re_turns))
# skewness() from package "moments"
skewness(x=coredata(re_turns))
quantile(x=re_turns, probs=c(0.05, 0.95))
quantile(x=re_turns, probs=c(0.1, 0.9))


# Plot histograms of daily returns
hist(re_turns, breaks=200, main="returns", xlab="", ylab="", freq=FALSE)
lines(density(re_turns), col="red", lwd=1)  # draw density

hist(re_turns, breaks=200, main="returns", xlab="", ylab="", freq=FALSE)
lines(density(re_turns), col="red", lwd=1)  # draw density

hist(re_turns, breaks=300, main="returns", xlab="", ylab="", xlim=c(-0.05, 0.05), freq=FALSE)
lines(density(re_turns), col="red", lwd=1)  # draw density

hist(daily_returns, breaks=100, main="returns", xlim=c(-2.0e-4, 2.0e-4), ylim=c(0, 10000), xlab="", ylab="", freq=FALSE)
lines(density(daily_returns), col="red", lwd=1)  # draw density

# title(main=ch.title, line=-1)  # Add title



hist(re_turns, breaks=400, main="", xlab="", ylab="", xlim=c(-0.006, 0.006), freq=FALSE)
lines(density(re_turns), col="red", lwd=1)

library(PerformanceAnalytics)
chart.CumReturns(re_turns, lwd=2,
                 ylab="", legend.loc="topleft", main="")
chart.Histogram(re_turns, main="",
                xlim=c(-0.003, 0.003),
                methods=c("add.density", "add.normal"))
chart.Histogram(re_turns, main="",
                xlim=c(-0.003, 0.003), breaks=300,
                methods=c("add.normal"))


# copy the xts data to a variable with the name "sym_bol"
sym_bol_rets <- paste(sym_bol, "rets", sep=".")
assign(sym_bol_rets, da_ta)

########### end temp ###########


################
### managing high frequency data using package HighFreq

# Load a single day of seconds TAQ data
sym_bol <- load("C:/Develop/data/hfreq/src/SPY/2012.02.16.SPY.RData")
# Extract one day of TAQ data from list, and subset to NYSE trading hours using "T notation"
price_s <- (get(sym_bol)[[6]])["T09:30:00/T16:00:00", ]
# Extract trade price and volume
price_s <- price_s[, c("Trade.Price", "Volume")]

# Calculate mid bid-offer prices and remove NAs
mid_prices <- 0.5 * (price_s[, "Bid.Price"] + price_s[, "Ask.Price"])
mid_prices <- na.omit(mid_prices)
colnames(mid_prices) <- "Mid.Price"

# Calculate log returns
re_turns <- diff(log(mid_prices))/c(1, diff(.index(mid_prices)))
re_turns[1, ] <- 0
chart_Series(exp(cumsum(re_turns)), name=sym_bol)

# Load minutely OHLC data
sym_bol <- load("C:/Develop/data/SPY.RData")
# or
sym_bol <- load(file.path(output_dir, paste0(sym_bol, ".RData")))
# Calculate log returns
re_turns <- diff(log(Cl(SPY)))/c(1, diff(.index(SPY)))
re_turns[1, ] <- 0
chart_Series(exp(cumsum(re_turns)), name=sym_bol)

# Plot histograms of returns
hist(re_turns, breaks=30, main="returns", xlab="", ylab="", freq=FALSE)
hist(re_turns, breaks=100, main="returns", xlim=c(-2.0e-4, 2.0e-4), ylim=c(0, 10000), xlab="", ylab="", freq=FALSE)
lines(density(re_turns), col='red', lwd=1)  # draw density


## Calculate daily Open and Close prices from daily OHLC data
# quantmod::getSymbols("SPY", adjust=TRUE)
op_en <- SPY[, 1]
tail(op_en)
cl_ose <- SPY[, 4]
tail(cl_ose)


## Calculate daily price profiles after overnight gaps

# Calculate daily Open and Close prices from high frequency data
end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
start_days <- rutils::lag_it(end_days)+1
cl_ose <- HighFreq::SPY[end_days, 4]
index(cl_ose) <- lubridate::floor_date(index(cl_ose), "day")
# index(cl_ose) <- as.POSIXct(trunc(index(cl_ose), "day"))
# index(cl_ose) <- as.POSIXct(format(index(cl_ose), format="%Y-%m-%d"))
tail(cl_ose)
op_en <- HighFreq::SPY[start_days, 4]
index(op_en) <- lubridate::floor_date(index(op_en), "day")
tail(op_en)
# Calculate daily overnight and intraday returns
close_rets <- (cl_ose - rutils::lag_it(op_en)) / rutils::lag_it(op_en)
open_rets <- (op_en - rutils::lag_it(op_en)) / rutils::lag_it(op_en)
over_night <- (op_en - rutils::lag_it(cl_ose)) / rutils::lag_it(cl_ose)
intra_day <- (cl_ose - op_en) / op_en
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
over_night <- over_night/rutils::lag_it(var_daily)

# Calculate a list of daily endpoints
end_points <- lapply(seq_along(end_days), function(da_y) {
  start_days[da_y]:end_days[da_y]
})  # end lapply
names(end_points) <- index(cl_ose)
# Calculate daily price profiles
price_profiles <- lapply(end_points, function(end_point) {
  da_ta <- as.numeric(HighFreq::SPY[end_point, 4])
  da_ta / da_ta[1]
})  # end lapply
# price_profiles <- lapply(seq_along(end_days), function(da_y) {
#   da_ta <- as.numeric(HighFreq::SPY[start_days[da_y]:end_days[da_y], 4])
#   da_ta / da_ta[1]
# })  # end lapply
plot(price_profiles[["2010-05-05/2010-05-07"]], t="l", ylab="")

# run simple contrarian strategies
foo <- -as.numeric(over_night<(-0.01))* intra_day
foo <- -as.numeric(rutils::lag_it(over_night)<(-0.01))* open_rets
foo <- -sign(rutils::lag_it(over_night))* open_rets
foo <- -sign(rutils::lag_it(over_night))* close_rets
foo <- exp(cumsum(foo))
plot.zoo(foo, t="l", main="simple contrarian strategy", ylab="")


# price_profiles <- rowMeans(price_profiles)
# plot(price_profiles, t="l")


# Calculate daily price profiles after overnight gaps



## Calculate intraday seasonality of returns
re_turns <- diff(log(Cl(SPY)))/c(1, diff(.index(SPY)))
re_turns <- diff(Cl(SPY))/c(1, diff(.index(SPY)))
re_turns[1, ] <- 0
re_turns <- na.locf(re_turns, na.rm=FALSE)
sum(is.na(re_turns))
# Remove overnight return spikes at "09:31"
in_dex <- format(index(re_turns), "%H:%M")
re_turns <- re_turns[!in_dex=="09:31", ]
# Calculate intraday seasonality of returns
season_rets <- HighFreq::season_ality(x_ts=re_turns)
chart_Series(x=season_rets,
             name=paste(colnames(season_rets), "intraday seasonality"))


## spikes over running scaled variance
oh_lc <- HighFreq::SPY["2009"]
in_dex <- index(oh_lc)
var_running <- 6.5*60^3*HighFreq::run_variance(oh_lc=oh_lc[, 1:4])
# rolling vwav volatility
var_rolling <- roll_vwap(oh_lc=oh_lc, x_ts=var_running, win_dow=21)
# colnames(var_rolling) <- colnames(var_running)

in_dex <- index(var_rolling)
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
# title(main=paste(sym_bol, "vol"), line=-1)
# Add Chi-squared fit
x_var <- seq(from=0, to=range(var_rolling)[2]/5, length.out=100)
lines(x=x_var, y=50*NROW(var_rolling)*dchisq(11*x_var/mean(var_rolling), df=11),
      xlab="", ylab="", lwd=1, col="blue")

# identify periods around volatility spikes
var_running <- roll::roll_scale(data=var_running, width=30, min_obs=1)
var_running[1, ] <- 0
var_running <- var_running[in_dex, ]
# var_running <- SPY_design[in_dex, "variance"]
hist(var_running, breaks=200, main="var_running", xlab="", ylab="", freq=FALSE)
quantile(var_running, probs=c(0.9, 0.99))
vol_spikes <- var_running[var_running>quantile(var_running, probs=0.99), ]
ma_tch <- match(index(vol_spikes), in_dex)
# class(vol_spikes)
# dim(vol_spikes)
# head(vol_spikes)
# index(tail(vol_spikes, 22))
# foo <- unique(format(index(vol_spikes), format="%Y-%m-%d"))
chart_Series(SPY[in_dex, 4], name=paste("SPY", "vol spikes"))
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
# blah <- which(in_dex==index(vol_spike[which.max(vol_spike)]))
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
# which(in_dex==index(first(vol_peaks)))

# legacy scripts - which I don't understand
# get_vol_peak_data <- function(vol_peak) {
#   which_peak <- which(in_dex==index(vol_peak))
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
#   which_peak <- which(in_dex==index(vol_peaks[i_ter, ]))
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
#   which_peak <- which(in_dex==index(vol_peaks[i_ter, ]))
#   core_data <- coredata(SPY["2009", 4][(which_peak-200):(which_peak+300)])
#   core_data/max(core_data)
# })  # end sapply

price_profiles <- sapply(ma_tch[-((NROW(ma_tch)-250):NROW(ma_tch))], function(pea_k) {
  coredata(oh_lc[(pea_k-2):(pea_k+200), 4])/as.numeric(oh_lc[pea_k, 4])
})  # end sapply
price_profiles <- rowMeans(price_profiles)
plot(price_profiles, t="l")


foo_bar <- apply(X=price_profiles, MARGIN=2, hurst_exp)
class(foo_bar)
dim(foo_bar)
head(foo_bar, 33)


## Calculate Hurst exponent using range for xts
hurst_exp <- function(re_turns) {
  cum_sum <- cumsum(re_turns)
  (max(cum_sum) - min(cum_sum))/sd(re_turns)/sqrt(NROW(re_turns))
}  # end hurst_exp
hurst_exp <- function(da_ta) {
  (max(da_ta) - min(da_ta))/sd(diff(da_ta)[-1])/sqrt(NROW(da_ta))
}  # end hurst_exp
# Calculate Hurst exponent using range for xts ohlc
hurst_exp <- function(da_ta) {
  (max(Hi(da_ta)) - min(Lo(da_ta)))/(max(Hi(da_ta)) + min(Lo(da_ta)))/sum(6.5*60^3*HighFreq::run_variance(oh_lc=da_ta[, 1:4]))/sqrt(NROW(da_ta))/2
}  # end hurst_exp
# Calculate Hurst exponent using range for non-xts
hurst_exp <- function(da_ta) {
  log((max(da_ta) - min(da_ta))/sd(da_ta[-1]-da_ta[-NROW(da_ta)]))/log(NROW(da_ta))
}  # end hurst_exp
# Calculate Hurst exponent using variance ratios for non-xts
hurst_exp <- function(da_ta, l_ag=4) {
  len_gth <- NROW(da_ta)
  var(da_ta[-(1:l_ag)]-da_ta[-((len_gth-l_ag+1):len_gth)])/var(da_ta[-1]-da_ta[-len_gth])/l_ag
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
  index(sk_ew[xts::endpoints(sk_ew, on="years"), ]),
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
sapply(ye_ars, function(ye_ar) sum(sk_ew[ye_ar]))
foo <- sapply(ye_ars, function(ye_ar) sum(Vo(SPY)[ye_ar]))
foo <- format(index(daily_skew[which.max(daily_skew)]), "%Y-%m-%d")

foo <- which.max(daily_skew)
foo <- which.min(daily_skew)
foo <- format(index(daily_skew[(foo-1):(foo+1), ]), "%Y-%m-%d")

chart_Series(SPY[foo], name=paste(sym_bol, "skew"))


# daily returns
daily_rets <- Cl(SPY[index(daily_skew), ])
daily_rets <- diff(log(daily_rets))
daily_rets[1, ] <- daily_rets[2, ]
colnames(daily_rets) <- paste(sym_bol, "rets", sep=".")
head(daily_rets)
tail(daily_rets)

date_s <- "2010-05-05/2010-05-07"
# daily_rets and sk_ew
bar <- cbind(coredata(daily_rets), coredata(daily_skew))
# daily_rets and lagged sk_ew
bar <- cbind(coredata(daily_rets), c(0, coredata(daily_skew)[-NROW(daily_skew)]))

head(bar)
dim(bar)
apply(bar, 2, mad)
ma_d <- mad(bar[, 2])
blah <- (abs(bar[, 2]-mean(bar[, 2])) > 5*ma_d)
NROW(blah)
sum(blah)
bar <- bar[!blah, ]


## returns

# lag_rets equals returns lagged by -1
re_turns <- 6.5*60^2*HighFreq::run_returns(x_ts=HighFreq::SPY, scal_e=FALSE)
lag_rets <- rutils::lag_it(returns_running)
# lag_rets <- c(lag_rets[-1, ], lag_rets[NROW(lag_rets)])
tail(lag_rets)
returns_advanced <- rutils::lag_it(returns_running, k=-1)

sk_ew <- 6.5*60^4*HighFreq::run_skew(oh_lc=SPY)
colnames(sk_ew) <- paste(sym_bol, "skew", sep=".")
lag_skew <- lag(sk_ew)


win_dow <- 2*60*6.5 + 101

## variance and skew estimators using MAD
# calc var_mad
var_mad <- runmad(coredata(vari_ance), k=win_dow)
# lag var_mad
var_mad <- c(rep(0, (win_dow-1)/2), var_mad[-((NROW(var_mad)-(win_dow-1)/2+1):(NROW(var_mad)))])
NROW(var_mad)
head(var_mad)
tail(var_mad)
# calc skew_mad
skew_mad <- runmad(coredata(sk_ew), k=win_dow)
# lag skew_mad
skew_mad <- c(rep(0, (win_dow-1)/2), skew_mad[-((NROW(skew_mad)-(win_dow-1)/2+1):(NROW(skew_mad)))])
plot(skew_mad[(NROW(skew_mad)-100*win_dow):NROW(skew_mad)], t="l", xlab="", ylab="", main="skew_mad")
# calc mad_volu
quan_tiles <- c("0.5"=0.5, "0.75"=0.75, "0.85"=0.85, "0.95"=0.95)
mad_volu <- runquantile(coredata(Vo(SPY)), probs=quan_tiles, k=win_dow)
mad_volu <- mad_volu[, 1, ]
# lag mad_volu
mad_volu <- rbind(
  matrix(numeric(ncol(mad_volu)*(win_dow-1)/2), ncol=ncol(mad_volu)),
  mad_volu[-((NROW(mad_volu)-(win_dow-1)/2+1):(NROW(mad_volu))), ])
colnames(mad_volu) <- names(quan_tiles)
mad_volu <- xts(mad_volu, order.by=index(SPY))
# plot(mad_volu[(NROW(mad_volu)-100*win_dow):NROW(mad_volu[,]), 4], t="l", xlab="", ylab="", main="mad_volu")
chart_Series(mad_volu[(NROW(mad_volu)-100*win_dow):NROW(mad_volu[,]), 4], name=paste(sym_bol, "mad_volu"))
# Plot volume spikes above 85% quantile
date_s <- (NROW(mad_volu)-4*win_dow):NROW(mad_volu[,])
chart_Series(mad_volu[date_s, 3], name=paste(sym_bol, "mad_volu"))
chart_Series(Vo(SPY[date_s]) - mad_volu[date_s, 4], name=paste(sym_bol, "volume spikes"))
chart_Series(Cl(SPY[date_s]), name=paste(sym_bol, "prices"))


# signal threshold trading level
pos_skew <- coredata(ifelse(sk_ew > 5*skew_mad, 1, 0))
colnames(pos_skew) <- paste(sym_bol, "p_skew", sep=".")
neg_skew <- coredata(ifelse(sk_ew < -5*skew_mad, -1, 0))
colnames(neg_skew) <- paste(sym_bol, "n_skew", sep=".")
c(pos_skew=sum(pos_skew)/NROW(pos_skew), neg_skew=-sum(neg_skew)/NROW(neg_skew))
plot(pos_skew)

spike_skew <- coredata(Vo(SPY) - mad_volu[, 4] > 0, sign(sk_ew), 0)
colnames(spike_skew) <- paste(sym_bol, "spike_skew", sep=".")

var_rolling <- runSum(vari_ance, n=win_dow)
var_rolling[1:(win_dow-1)] <- 0
colnames(var_rolling) <- colnames(vari_ance)
head(var_rolling)

chart_Series(var_rolling[date_s],
             name=paste(sym_bol, "volatility"))

roll_skew <- runSum(sk_ew, n=win_dow)
roll_skew[1:(win_dow-1)] <- 0
colnames(roll_skew) <- colnames(sk_ew)
head(roll_skew)

chart_Series(roll_skew[date_s],
             name=paste(sym_bol, "skew"))

win_short <- 70
win_long <- 225
vwap_short <- roll_vwap(oh_lc=SPY, win_dow=win_short)
vwap_long <- roll_vwap(oh_lc=SPY, win_dow=win_long)
head(vwap_short)
head(vwap_long)
vwap_diff <- vwap_short - vwap_long
colnames(vwap_diff) <- paste(sym_bol, "vwap", sep=".")
vwap_diff <- na.locf(vwap_diff, na.rm=FALSE)


## data: lagged returns plus explanatory variables

# for lm reg
# de_sign <- cbind(lag_rets, coredata(re_turns), pos_skew, neg_skew)
# de_sign <- cbind(lag_rets, coredata(vwap_diff), pos_skew, neg_skew)
de_sign <- cbind(re_turns, lag_skew)
de_sign <- cbind(lag_rets, sign(coredata(vwap_diff)), pos_skew, neg_skew)
# de_sign <- cbind(sign(lag_rets), sign(coredata(vwap_diff)), pos_skew, neg_skew)
# for logistic reg
de_sign <- cbind((sign(coredata(lag_rets))+1)/2, sign(coredata(vwap_diff)), pos_skew, neg_skew)
# for lda qda
de_sign <- cbind(sign(lag_rets), coredata(vwap_diff), pos_skew, neg_skew)
# colnames(de_sign) <- c("SPY.lagrets", "SPY.rets", "SPY.poskew", "SPY.negskew")
class(de_sign)
tail(de_sign)


## lm

# lm formula with zero intercept
for_mula <- as.formula(paste(colnames(de_sign)[1], paste(paste(colnames(de_sign)[-1], collapse=" + "), "- 1"), sep="~"))
for_mula <- as.formula(paste(colnames(de_sign)[1], paste(colnames(de_sign)[2], "- 1"), sep="~"))

l_m <- lm(for_mula, data=as.data.frame(de_sign))
# Perform regressions over different calendar periods
l_m <- lm(for_mula, data=as.data.frame(de_sign["2011-01-01/"]))
l_m <- lm(for_mula, data=as.data.frame(de_sign["/2011-01-01"]))
lm_summ <- summary(l_m)
l_m <- lm(for_mula, data=as.data.frame(de_sign["2010-05-05/2010-05-07"]))
lm_summ <- summary(l_m)
lm_predict <- predict(l_m, newdata=as.data.frame(de_sign["2010-05-06"]))
foo <- data.frame(sign(lm_predict), coredata(de_sign["2010-05-06", 1]))
colnames(foo) <- c("lm_pred", "realized")
table(foo)
cumu_pnl <- cumsum(sign(lm_predict)*re_turns["2010-05-06", 1])
last(cumu_pnl)
chart_Series(cumu_pnl, name=paste(sym_bol, "optim_rets"))

# loop over thresholds and return regression t-values
foo <- sapply(structure(2:10, paste0("thresh", names=2:10)), function(thresh_old) {
  pos_skew <- coredata(ifelse(sk_ew > thresh_old*skew_mad, 1, 0))
  colnames(pos_skew) <- paste(sym_bol, "p_skew", sep=".")
  neg_skew <- coredata(ifelse(sk_ew < -thresh_old*skew_mad, -1, 0))
  colnames(neg_skew) <- paste(sym_bol, "n_skew", sep=".")
  de_sign <- cbind(sign(lag_rets), sign(coredata(vwap_diff)), pos_skew, neg_skew)
  l_m <- lm(for_mula, data=as.data.frame(de_sign))
  lm_summ <- summary(l_m)
  lm_summ$coefficients[, "t value"]
}, USE.NAMES=TRUE)  # end sapply


# loop over periods
date_s <- "2013-06-01/"
date_s <- "2008-06-01/2009-06-01"
end_points <- xts::endpoints(SPY[date_s], on="days")
end_points <- format(index((SPY[date_s])[end_points[-1], ]), "%Y-%m-%d")
win_dow <- 10

position_s <-
  lapply(win_dow:NROW(end_points),
         function(end_point) {
           date_s <- paste0(end_points[end_point-win_dow+1], "/", end_points[end_point-1])
           l_m <- lm(for_mula, data=as.data.frame(de_sign[date_s]))
           da_ta <- de_sign[end_points[end_point]]
           xts(x=predict(l_m, newdata=as.data.frame(da_ta)), order.by=index(da_ta))
         }  # end anon function
  )  # end lapply
position_s <- rutils::do_call(rbind, position_s)
chart_Series(position_s, name=paste(sym_bol, "optim_rets"))

cumu_pnl <- cumsum(sign(position_s)*re_turns[index(position_s), 1])
last(cumu_pnl)
chart_Series(cumu_pnl, name=paste(sym_bol, "optim_rets"))


## logistic reg
library(MASS)
library(ISLR)
library(glmnet)
g_lm <- glm(for_mula, data=as.data.frame(de_sign), family=binomial)
summary(g_lm)


## lda
l_da <- lda(for_mula, data=as.data.frame(de_sign))
summary(l_da)
l_da <- lda(for_mula, data=as.data.frame(de_sign["2010-05-05/2010-05-07"]))
lda_predict <- predict(l_da, newdata=as.data.frame(de_sign["2010-05-06"]))
foo <- data.frame(lda_predict$class, coredata(de_sign["2010-05-06", 1]))
colnames(foo) <- c("lda_pred", "realized")
table(foo)


## qda
q_da <- qda(for_mula, data=as.data.frame(de_sign))
summary(q_da)
date_s <- "2010-05-05/2010-05-07"
q_da <- qda(for_mula, data=as.data.frame(de_sign["2010-05-05/2010-05-07"]))
date_s <- "2013-02-07"
qda_predict <- predict(q_da, newdata=as.data.frame(de_sign["2010-05-06"]))
str(qda_predict)
head(qda_predict$class)
tail(qda_predict$class)
NROW(qda_predict$class)
sum(qda_predict$class!=1)
sum(de_sign["2013-02-07", 1]!=1)
foo <- data.frame(qda_predict$class, coredata(de_sign["2010-05-06", 1]))
colnames(foo) <- c("qda_pred", "realized")
table(foo)

# scatterplot of sk_ew and daily_rets
plot(for_mula, data=de_sign, xlab="skew", ylab="rets")
abline(l_m, col="blue")

cor.test(formula=as.formula(paste("~", paste(colnames(de_sign), collapse=" + "))), data=as.data.frame(de_sign))


date_s <- "2013-06-01/"
de_sign <- cbind(
  coredata(re_turns[date_s, 1]),
  c(0, coredata(roll_skew[date_s])[-NROW(roll_skew[date_s])]))


# multiply matrix columns
foo <- t(t(coredata(de_sign[, -1]))*coef(l_m)[-1])
dim(foo)
tail(foo)
apply(foo, MARGIN=2, sum)


## run simple strategy

# thresh_old <- 2*mad(roll_skew)  # signal threshold trading level
# position_s <- NA*numeric(NROW(sk_ew))
position_s <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, sign(coredata(vwap_diff)))
position_s <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, -coredata(re_turns))
position_s <- ifelse((pos_skew!=0) | (neg_skew!=0), 1, sign(coredata(vwap_diff)))
position_s <- pos_skew + neg_skew + sign(coredata(vwap_diff))
position_s <- -sign(sk_ew) + sign(coredata(vwap_diff))
position_s <- coredata(de_sign[, -1]) %*% coef(l_m)
sum(is.na(position_s))
NROW(position_s)
head(position_s)
plot(position_s[(NROW(position_s)-100*win_dow):NROW(position_s)], t="l", xlab="", ylab="", main="position_s")
plot(position_s, t="l", ylim=c(0, 0.001))

position_s <- ifelse(roll_skew>thresh_old, -1, position_s)
position_s <- ifelse(roll_skew<(-thresh_old), 1, position_s)
position_s <- ifelse((roll_skew*lag(roll_skew))<0, 0, position_s)
# lag the position_s
lag_positions <- c(0, position_s[-NROW(position_s)])
lag_positions <- na.locf(lag_positions, na.rm=FALSE)
lag_positions <- merge(roll_skew, lag_positions)
colnames(lag_positions)[2] <-
  paste0(sym_bol, ".Position")
# cumulative PnL
cumu_pnl <- cumsum(lag_positions*re_turns)
last(cumu_pnl)
# cumu_pnl <- cumsum(lag_positions[, 2]*re_turns)
plot.zoo(cumu_pnl)
chart_Series(cumu_pnl, name=paste(sym_bol, "pnl"))

foo <- rutils::roll_sum(abs(sign(sk_ew)-sign(lag_skew)), win_dow=1000)
chart_Series(
  foo[xts::endpoints(foo, on="days"), ],
  name=paste(sym_bol, "contrarian skew strategy frequency of trades"))
# Calculate transaction costs
bid_offer <- 0.001  # 10 bps for liquid ETFs
cost_s <- bid_offer*abs(position_s-lag_positions)/2
pnl_xts[, "pnl"] <- pnl_xts[, "pnl"] - cost_s


## optimize vwap

roll_vwap <- function(win_short=10, win_long=100, price_s, re_turns) {
  vwap_short <- coredata(roll_vwap(oh_lc=price_s, win_dow=win_short))
  vwap_long <- coredata(roll_vwap(oh_lc=price_s, win_dow=win_long))
# lag the position_s
  position_s <- sign(vwap_short - vwap_long)
  position_s <- c(0, position_s[-NROW(position_s)])
  sum(position_s*re_turns)
}  # end roll_vwap

roll_vwap(price_s=SPY, re_turns=re_turns)


short_windows <- seq(from=30, to=100, by=10)
names(short_windows) <- paste0("sh", short_windows)
long_windows <- seq(from=200, to=400, by=25)
names(long_windows) <- paste0("lo", long_windows)

mat_rix <- sapply(short_windows,
                  function(win_short, ...)
                    sapply(long_windows,
                           roll_vwap,
                           win_short=win_short, ...),
                  price_s=SPY, re_turns=re_turns)

# Load rgl
library(rgl)
persp3d(z=mat_rix, col="green", x=short_windows, y=long_windows)


###


# seconds index
in_dex <- as.POSIXct("2015-01-01 00:00:00") + 0:1000
in_dex <- seq(from=as.POSIXct("2015-01-01 00:00:00"),
              to=as.POSIXct("2015-01-03 00:00:00"), by="sec")
head(in_dex)
tail(in_dex)
NROW(in_dex)

# Simulate lognormal prices
foo <- xts(exp(cumsum(rnorm(NROW(in_dex)))/100), order.by=in_dex)
dim(foo)

# Aggregate minutes OHLC bars
oh_lc <- to.period(x=foo, period="minutes", name="synth")
tail(oh_lc)
# OHLC candlechart
chart_Series(x=oh_lc["2015-01-01 01:00:00/2015-01-01 05:00:00"],
             name="OHLC candlechart")

# rolling volatility
var_running <- roll_vwap(oh_lc=oh_lc, x_ts=6.5*60^3*HighFreq::run_variance(oh_lc=oh_lc), win_dow=1000)
head(var_running)
tail(var_running)
# rolling skew
sk_ew <- roll_vwap(oh_lc=oh_lc, x_ts=6.5*60^4*HighFreq::run_skew(oh_lc=oh_lc), win_dow=1000)
sk_ew <- sk_ew/(var_running)^(1.5)
sk_ew[1, ] <- 0
sk_ew <- na.locf(sk_ew, na.rm=FALSE)
chart_Series(x=var_running, name="volatility")
chart_Series(x=sk_ew, name="skew")


###

mat_rix <- matrix(1:6, ncol=2)

foo <- etf_rets[, sym_bols]
head(foo)
NROW(etf_rets)

foo <- xts(matrix(rnorm(3*NROW(etf_rets)), ncol=3), order.by=index(etf_rets))

colnames(foo) <- colnames(etf_rets[, sym_bols])
head(foo)

ann_weights <- sapply(2:NROW(end_points),
                      function(in_dex) {
                        optim_portf(
                          portf_rets=foo,
                          start_point=end_points[in_dex-1],
                          end_point=end_points[in_dex])
                      }  # end anon function
)  # end sapply


colnames(ann_weights) <- format(index(foo[end_points[-1]]), "%Y")

ann_weights <- t(ann_weights)


bar <- lapply(3:NROW(end_points),
              function(in_dex) {
                foo[end_points[in_dex-1]:end_points[in_dex], ] %*%
                  c(1, ann_weights[in_dex-2, ])
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
dis_persion <- function(da_ta,
                        meth_od=c("mean", "mean_narm", "median")) {
  # validate "meth_od" argument
  meth_od <- match.arg(meth_od)
  switch(meth_od,
         mean=mean(da_ta),
         mean_narm=mean(da_ta, na.rm=TRUE),
         median=median(da_ta))
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
beta_s <- rollapply(etf_env$re_turns, width=252,
                    FUN=function(design_matrix)
                      coef(lm(reg_formula, data=design_matrix))[2],
                    by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# Plot beta_s in x11() window
x11()
chart_Series(x=beta_s, name=paste("rolling betas", format(reg_formula)))

# Perform daily rolling beta regressions in parallel
beta_s <- roll::roll_lm(x=etf_env$re_turns[, "VTI"],
                  y=etf_env$re_turns[, "XLP"],
                  width=252)$coefficients
chart_Series(x=beta_s, name=paste("rolling betas", format(reg_formula)))

# Compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- etf_env$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
                      FUN=function(design_matrix)
                        coef(lm(reg_formula, data=design_matrix))[2],
                      by.column=FALSE, align="right"),
  roll_lm=roll::roll_lm(x=da_ta[, "VTI"],
                  y=da_ta[, "XLP"],
                  width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### Rolling Portfolio Optimization Strategy for S&P500 Constituents
# in slides already

library(rutils)
load("C:/Develop/R/lecture_slides/data/sp500_prices.RData")

n_col <- NCOL(price_s)
end_points <- rutils::calc_endpoints(price_s, inter_val="months")
end_points <- end_points[end_points>50]
len_gth <- NROW(end_points)
look_back <- 12
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# date_s <- index(price_s)
# price_s <- t(t(price_s) / as.numeric(price_s[1, ]))
# sum(is.na(price_s))
# in_dex <- xts(rowSums(price_s)/n_col, date_s)
# re_turns <- diff_it(price_s)

Rcpp::sourceCpp(file="C:/Develop/R/scripts/roll_portf.cpp")

al_pha <- 0.01
max_eigen <- 2

library(microbenchmark)
summary(microbenchmark(
  old=roll_portf(foo, foo, start_points-1, end_points-1, al_pha=al_pha, max_eigen=max_eigen),
  new=roll_portfx(foo, foo, start_points-1, end_points-1, al_pha=al_pha, max_eigen=max_eigen),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


strat_rets_arma <- roll_portf(re_turns,
                              re_turns,
                              start_points-1,
                              end_points-1,
                              al_pha=al_pha,
                              max_eigen=max_eigen)
x11()
strat_rets_arma <- cumsum(strat_rets_arma)
strat_rets_arma <- xts(strat_rets_arma, date_s)

library(dygraphs)
dygraphs::dygraph(strat_rets_arma,
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")

strat_rets_arma <- cbind(strat_rets_arma, in_dex)
col_names <- c("Strategy", "Index")
colnames(strat_rets_arma) <- col_names

dygraphs::dygraph(strat_rets_arma, main=paste(col_names, collapse=" and ")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(col_names[2], axis="y2", col=c("red", "blue"))


foo <- diff_it(log(strat_rets_arma-min(strat_rets_arma)+1))
sqrt(260)*mean(foo)/sd(foo)
bar <- diff_it(strat_rets_arma2)
sqrt(260)*mean(bar)/sd(bar)
bar <- diff_it(log(in_dex-min(in_dex)+1))
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
# sym_bols contains all the symbols in rutils::etf_env$re_turns except for "VXX"
sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
# Extract columns of rutils::etf_env$re_turns and remove NA values
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
re_turns <- na.omit(re_turns)

# Rolling Portfolio Optimization Strategy
look_back <- 12
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
# Remove initial end_points for warmpup
end_points <- end_points[end_points>50]
len_gth <- NROW(end_points)
# sliding window
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# expanding window
start_points <- rep_len(1, NROW(end_points))


# coredata(re_turns) <- matrix(rnorm(prod(dim(re_turns)), sd=0.01), nc=NCOL(re_turns))
# tail(re_turns)
# risk_free is the daily risk-free rate
risk_free <- 0.03/260
ex_cess <- re_turns - risk_free
# weight_s <- sapply(1:(NROW(end_points)-1),
#                    function(i) {
#                      # subset the ex_cess returns
#                      ex_cess <- ex_cess[start_points[i]:end_points[i], ]
#                      in_verse <- solve(cov(ex_cess))
#                      # Calculate the maximum Sharpe ratio portfolio weights
#                      weight_s <- in_verse %*% colMeans(ex_cess)
#                      weight_s <- drop(weight_s/sum(weight_s))
#                    }  # end anonymous function
# )  # end sapply
# round(weight_s, 2)
# dim(weight_s)

# Regular inverse
portf_rets <- lapply(2:NROW(end_points),
                     function(i) {
                       # subset the ex_cess returns
                       ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
                       in_verse <- solve(cov(ex_cess))
                       # Calculate the maximum Sharpe ratio portfolio weights
                       weight_s <- in_verse %*% colMeans(ex_cess)
                       weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
                       # subset the re_turns
                       re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply

# with regularization
max_eigen <- 2
portf_rets <- lapply(2:NROW(end_points),
                     function(i) {
                       # subset the ex_cess returns
                       ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
                       cov_mat <- cov(ex_cess)
                       # Perform eigen decomposition and calculate eigenvectors and eigenvalues
                       ei_gen <- eigen(cov_mat)
                       eigen_vec <- ei_gen$vectors
                       # Calculate regularized inverse
                       in_verse <- eigen_vec[, 1:max_eigen] %*% (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
                       # Calculate the maximum Sharpe ratio portfolio weights
                       # weight_s <- in_verse %*% colMeans(ex_cess)
                       # weight_s <- rep(mean(colMeans(ex_cess)), NCOL(ex_cess))
                       weight_s <- colMeans(ex_cess)
                       # shrink weight_s to the mean of weight_s
                       weight_s <- ((1-al_pha)*weight_s + al_pha*mean(weight_s))
                       weight_s <- in_verse %*% weight_s
                       weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
                       # subset the re_turns
                       re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply


# with shrinkage intensity
al_pha <- 0.9
portf_rets <- lapply(2:NROW(end_points),
                     function(i) {
                       # subset the ex_cess returns
                       ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
                       cov_mat <- cov(ex_cess)
                       cor_mat <- cor(ex_cess)
                       std_dev <- sqrt(diag(cov_mat))
                       # Calculate target matrix
                       cor_mean <- mean(cor_mat[upper.tri(cor_mat)])
                       tar_get <- matrix(cor_mean, nr=NROW(cov_mat), nc=NCOL(cov_mat))
                       diag(tar_get) <- 1
                       tar_get <- t(t(tar_get * std_dev) * std_dev)
                       # Calculate shrinkage covariance matrix
                       cov_mat <- (1-al_pha)*cov_mat + al_pha*tar_get
                       in_verse <- solve(cov_mat)
                       # Calculate the maximum Sharpe ratio portfolio weights
                       weight_s <- in_verse %*% colMeans(ex_cess)
                       weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))
                       # subset the re_turns
                       re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply


portf_rets <- rutils::do_call(rbind, portf_rets)
colnames(portf_rets) <- "portf_rets"
in_dex <- index(re_turns)[index(re_turns) < start(portf_rets)]
portf_rets <- rbind(xts(numeric(NROW(in_dex)), in_dex), portf_rets)
all.equal(as.numeric(portf_rets), drop(foo))


# tail(portf_rets, 11)
# dim(portf_rets)
# sqrt(260)*(mean(portf_rets)-risk_free) / sd(portf_rets)
portf_rets <- cumsum(portf_rets)
# tail(portf_rets, 11)
quantmod::chart_Series(portf_rets,
                       name="Cumulative Returns of Max Sharpe Portfolio Strategy")
dygraphs::dygraph(portf_rets, 
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")

Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/scripts/rcpp_test6.cpp")
foo <- roll_portf(ex_cess, re_turns, start_points-1, end_points-1, al_pha=al_pha, max_eigen)
coredata(portf_rets) <- cumsum(foo)



###############
### Rolling S&P500 portfolio strategies

# Load HighFreq
library(rutils)
library(roll)

# sharp_e <- function(x) (as.numeric(x[NROW(x)])-as.numeric(x[1]))/sd(rutils::diff_it(x))/sqrt(252)

# Load S&P500 constituent stock prices
load("C:/Develop/R/lecture_slides/data/sp500.RData")
price_s <- eapply(env_sp500, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# Remove NA values
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, na.rm=FALSE, fromLast=TRUE)
# Normalize the price_s
colnames(price_s) <- sapply(colnames(price_s),
                            function(col_name) strsplit(col_name, split="[.]")[[1]][1])

# Calculate the time series of the static, equal 
# dollar-weighted price of the index components, 
n_col <- NCOL(price_s)
in_dex <- rowSums(zoo::coredata(price_s))/n_col
in_dex <- in_dex/in_dex[1]
sd_index <- sd(rutils::diff_it(log(in_dex)))
# sqrt(252)*(in_dex[NROW(in_dex)]-1)/sd_index/NROW(in_dex)
in_dex <- xts::xts(in_dex, order.by=index(price_s))
colnames(in_dex) <- "index"


# Calculate the percentage (log) returns of the S&P500 
# constituent stocks, and call them re_turns.
# You can use functions rutils::diff_it() and log().

re_turns <- rutils::diff_it(log(price_s))
wid_th <- 220
returns_width <- rutils::diff_it(log(price_s), lagg=wid_th)


## Calculate rolling variance of S&P500 constituents
vari_ance <- roll::roll_var(re_turns, width=wid_th)
vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
vari_ance[is.na(vari_ance)] <- 0
# vari_ance[vari_ance==0] <- 1
# vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE, fromLast=TRUE)
# sum(is.na(vari_ance))
# sum(vari_ance==0)
# head(vari_ance[, 100:106])
# tail(vari_ance[, 100:106])


## Calculate rolling Sharpe of S&P500 constituents

weight_s <- returns_width / sqrt(wid_th*vari_ance)
weight_s[vari_ance==0] <- 0
weight_s[1:wid_th, ] <- 1
weight_s[is.na(weight_s)] <- 0

# long-short vari_ance
# weight_s <- 1/sqrt(wid_th*vari_ance)
# weight_s[vari_ance==0] <- 0
# weight_s[1:wid_th, ] <- 1
# weight_s <- weight_s/rowSums(abs(weight_s))
# weight_s[1:wid_th, ] <- 1/NCOL(weight_s)
# weight_s <- (weight_s - rowMeans(weight_s))
# sum(is.na(weight_s))

# weight_s <- 1 / sqrt(wid_th*vari_ance)
# weight_s <- zoo::na.locf(weight_s, na.rm=FALSE)
# scale weight_s so that their sum of squares is equal to 1
# weight_s <- weight_s/sqrt(rowSums(weight_s^2))
# sum(is.na(weight_s))
# sum(rowSums(abs(weight_s))==0)
weight_s <- weight_s/rowSums(abs(weight_s))
# sum(is.na(weight_s))
# sum(rowSums(weight_s^2)==0)

# Calculate portfolio profits and losses
pnl_s <- rowSums(rutils::lag_it(weight_s)*re_turns)
sd_moment <- sd(pnl_s)
pnl_s <- sd_index*pnl_s/sd_moment
pnl_s <- exp(cumsum(pnl_s))
pnl_s <- xts(pnl_s, order.by=index(re_turns))
colnames(pnl_s) <- "momentum"
pnl_s <- cbind(in_dex, pnl_s)
# sharp_e(pnl_s[, 1])
sapply(pnl_s, function(x) sd(rutils::diff_it(log(x))))

# foo <- pnl_s[, 1] + pnl_s[, 2]
# dygraphs::dygraph(foo, main="Momentum PnL")

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme, lwd=c(2, 2), 
             name="Momentum PnL")
legend("topleft", legend=colnames(pnl_s), 
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
       col=plot_theme$col$line.col, bty="n")


## rolling eigenvalues and eigenvectors
n_rows <- NROW(re_turns)
foo <- roll::roll_eigen(re_turns[(n_rows-100):n_rows, 1:5], width=10)
foo$values[1:9, ] <- 0
# sum(is.na(foo$values))
plot(foo$values[, 1], t="l")
plot(as.numeric(foo$values[101, ]), t="l")
as.numeric(foo$values[101, ])
foo$vectors[, , 101]
ret_sub <- re_turns[(n_rows-9):n_rows, 1:5]
ret_mean <- apply(ret_sub, 2, mean)
# ret_sub <- apply(ret_sub, 2, function(x) x-mean(x))
# cov_mat <- crossprod(ret_sub)

## maximum Sharpe portfolio weights from inverse matrix
cov_mat <- cov(ret_sub)
ei_gen <- eigen(cov_mat)
in_verse <- ei_gen$vectors %*% (t(ei_gen$vectors)/ei_gen$values)
trunc(in_verse %*% cov_mat, 4)
weight_s <- in_verse %*% ret_mean
weight_s <- weight_s/sum(abs(weight_s))

# maximum Sharpe portfolio weights
calc_weights <- function(re_turns) {
  ei_gen <- eigen(cov(re_turns))
  # set tolerance for determining zero eigenvalues
  to_l <- sqrt(.Machine$double.eps)
  # check for zero eigenvalues
  not_zero <- (ei_gen$values > (to_l * ei_gen$values[1]))
  in_verse <- ei_gen$vectors[, not_zero] %*% (t(ei_gen$vectors[, not_zero])/ei_gen$values[not_zero])
  weight_s <- in_verse %*% apply(re_turns, 2, mean)
  weight_s/sum(abs(weight_s))
} # end calc_weights
calc_weights(ret_sub)

## maximum Sharpe portfolio weights from optimization
object_ive <- function(weight_s, re_turns) {
  portf_rets <- re_turns %*% weight_s
  if (sd(portf_rets) == 0)
    return(0)
  else
    return(-mean(portf_rets)/sd(portf_rets))
} # end object_ive

op_tim <- optim(par=numeric(NCOL(ret_sub)),
                fn=object_ive,
                re_turns=ret_sub,
                method="L-BFGS-B",
                upper=(100+numeric(NCOL(ret_sub))),
                lower=(-100+numeric(NCOL(ret_sub))))
op_tim$par/sum(abs(op_tim$par))
as.numeric(weight_s)


## backtest of maximum Sharpe portfolio weights strategy
# this strategy has similar characteristics to momentum, and also suffers crashes.
ret_sub <- re_turns[, 1:5]
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
# end_points <- end_points[end_points>look_back]
len_gth <- NROW(end_points)
look_back <- 10
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
# Perform loop over end_points and calculate aggregations
weight_s <- sapply(1:len_gth, function(in_dex) {
  calc_weights(ret_sub[start_points[in_dex]:end_points[in_dex]])
})  # end sapply
weight_s <- t(weight_s)
weight_s <- xts::xts(weight_s, index(re_turns[end_points, ]))
weight_s <- cbind(re_turns[, 1], weight_s)[, -1]
weight_s[1, ] <- rep(1/NCOL(ret_sub), NCOL(ret_sub))
weight_s <- zoo::na.locf(weight_s, na.rm=FALSE)
weight_s <- rutils::lag_it(weight_s)
sum(is.na(weight_s))

pnl_s <- rowSums(weight_s*ret_sub)
sd_moment <- sd(pnl_s)
pnl_s <- sd_index*pnl_s/sd_moment
pnl_s <- exp(cumsum(pnl_s))
pnl_s <- xts(pnl_s, order.by=index(re_turns))
colnames(pnl_s) <- "momentum"
pnl_s <- cbind(in_dex, pnl_s)
# sharp_e(pnl_s[, 1])
sapply(pnl_s, function(x) sd(rutils::diff_it(log(x))))

# foo <- pnl_s[, 1] + pnl_s[, 2]
# dygraphs::dygraph(foo, main="Momentum PnL")

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(pnl_s, theme=plot_theme, lwd=c(2, 2), 
             name="Momentum PnL")
legend("topleft", legend=colnames(pnl_s), 
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
       col=plot_theme$col$line.col, bty="n")


## GARCH variance
garch_fit <- fGarch::garchFit(data=rutils::diff_it(log(in_dex)))
garch_fit@fit$par
# Plot GARCH fitted standard deviation
fGarch::plot(garch_fit)
garch_sdev <- xts::xts(sqrt(garch_fit@fit$series$h), order.by=index(re_turns))
chart_Series(garch_sdev, name="GARCH fitted standard deviation")
dygraphs::dygraph(garch_sdev, main="GARCH fitted standard deviation")



## scatterplot returns versus volatility

end_points <- rutils::calc_endpoints(re_turns, inter_val=20)
ret_vol <- lapply(re_turns, function(x) {
  re_turns <- roll::roll_mean(data=x, width=20)[end_points]
  s_d <- sqrt(roll::roll_var(data=x, width=20)[end_points])
  cbind(re_turns, s_d)
})  # end lapply
plot(x=as.numeric(ret_vol[[1]][, 2]), y=as.numeric(ret_vol[[1]][, 1]))
reg_models <- sapply(ret_vol, function(x) {
  reg_model <- lm(as.numeric(x[, 1]) ~ as.numeric(x[, 2]))
  summary(reg_model)$coefficients[2, ]
})  # end sapply
reg_models <- t(reg_models)
plot(x=lamb_das, y=reg_models[, 3], t="l")


garch_sdev <- sqrt(garch_fit@fit$series$h)
pnl_s <- rowSums(weight_s*ret_sub) / garch_sdev
plot(x=garch_sdev, y=pnl_s)
# orde_r <- order(garch_sdev)
# foo <- garch_sdev[orde_r]
# bar <- pnl_s[orde_r]
break_s <- hist(garch_sdev, breaks="Freedman-Diaconis")
break_s <- c(break_s$breaks, 100)

bar <- sapply(2:NROW(break_s), function(x) {
  ge_t <- (garch_sdev > (break_s[x-1]) & (garch_sdev < break_s[x]))
  sum(pnl_s[ge_t])
})  # end sapply
plot(x=break_s[-NROW(break_s)], y=bar, t="l")

# run EWMA strategies
lamb_das <- seq(0.001, 0.15, 0.01)
rets_ewma <- lapply(lamb_das, function(lamb_da) {
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, wid_th=151)[, 2]
})  # end lapply
rets_ewma <- rutils::do_call(cbind, rets_ewma)
colnames(rets_ewma) <- paste0("rets_", lamb_das)
sapply(rets_ewma, function(x) sqrt(260)*sum(x)/sd(x)/NROW(x))
chart_Series(cumsum(rets_ewma[, 1]), name="EWMA trend-following strategy")



## Volatility switching weights strategy
# Adjust weights depending on volatility

oh_lc <- rutils::etf_env$VTI
re_turns <- rutils::diff_it(log(Cl(oh_lc)))
rets_mean <- roll::roll_mean(data=re_turns, width=20)
rets_mean[1:19] <- 0
# garch_fit <- fGarch::garchFit(data=re_turns)
# garch_sdev <- xts::xts(sqrt(garch_fit@fit$series$h), order.by=index(re_turns))
s_d <- sqrt(roll::roll_var(data=re_turns, width=20))
# first and second derivatives of volatility
sd_diff <- rutils::diff_it(s_d, lagg=20)
sd_diff2 <- rutils::diff_it(sd_diff)
# chart_Series(sd_diff2, name="GARCH fitted standard deviation")
rets_mean <- rutils::lag_it(rets_mean, lagg=(-20))

# Regression of future returns versus past volatility
reg_model <- lm(rets_mean ~ s_d + sd_diff + sd_diff2)
summary(reg_model)
plot(as.numeric(rets_mean) ~ as.numeric(sd_diff))
abline(lm(rets_mean ~ sd_diff))

# Use first derivative of volatility as predictor
foo <- NA*re_turns
foo[1] <- 0
foo[sd_diff > 0.001] <- (-1)
foo[sd_diff < (-0.001)] <- 1
foo <- na.locf(foo, na.rm=FALSE)
foo <- exp(cumsum(re_turns*rutils::lag_it(foo)))
chart_Series(foo, name="GARCH fitted standard deviation")

level_s <- seq(0.001, 0.01, 0.001)
bar <- lapply(level_s, function(x) {
  foo <- NA*re_turns
  foo[1] <- 0
  foo[sd_diff > x] <- (-1)
  foo[sd_diff < (-x)] <- 1
  foo <- na.locf(foo, na.rm=FALSE)
  exp(cumsum(re_turns*rutils::lag_it(foo)))
})  # end lapply
bar <- rutils::do_call(cbind, bar)
dygraphs::dygraph(bar[, 7], main="GARCH fitted standard deviation")
# chart_Series(bar[, 7], name="GARCH fitted standard deviation")
ba_se <- Cl(oh_lc)/as.numeric(Cl(oh_lc)[1, ])
foo <- (xts::xts(rowMeans(bar), order.by=index(re_turns)) + ba_se)/2
dygraphs::dygraph(cbind(foo, ba_se), main="GARCH fitted standard deviation")



# fit t-dist

1 - be_ta^2 - 2*al_pha*be_ta - 3*al_pha^2


moments::moment(re_turns, order = 3)
moments::moment(re_turns, order = 2)


# forecast GARCH
foo <- fGarch::predict(garch_fit, plot=TRUE)

# fit GARCH

foo <- tseries::garch(x=re_turns)
foo <- rugarch::ugarchfit(data=re_turns)

library(microbenchmark)
summary(microbenchmark(
  inn_er=(res_ponse %*% explana_tory),
  su_m=sum(res_ponse*explana_tory),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


library(rugarch)
require(xts)
data(spyreal)
spec = ugarchspec(mean.model = list(armaOrder = c(0, 0), include.mean = FALSE), variance.model = list(model = 'realGARCH', garchOrder = c(2, 1)))
setbounds(spec)<-list(alpha2=c(-1,1))
fit = ugarchfit(spec, spyreal[, 1] * 100, solver = 'hybrid', realizedVol = spyreal[,2] * 100)


# matrix eigenvalues
mat_rix <- cbind(c(1, -0.5, 0.5), c(-0.5, 1, -0.5), c(0.5, -0.5, 1))
mat_rix <- cbind(c(1, 0.5, 0.5), c(0.5, 1, -0.5), c(0.5, -0.5, 1))
mat_rix <- cbind(c(1, -0.5, -0.5), c(-0.5, 1, -0.5), c(-0.5, -0.5, 1))
ei_gen <- eigen(mat_rix)
ei_gen$vectors
ei_gen$values


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
weight_s <- exp(0.1*1:look_back)
weight_s <- weight_s/sum(weight_s)
foob <- filter(foo, filter=rev(weight_s), sides=1)
foob[1:(look_back-1)] <- 0
foobar <- HighFreq::roll_wsum(foo, weight_s)
foobar <- roll::roll_sum(foo, weights=weight_s, width=NROW(weight_s))
foobar <- roll::roll_var(foo, weights=weight_s, width=NROW(weight_s))


summary(microbenchmark(
  pure_r=filter(foo, filter=weight_s, sides=1),
  r_cpp=HighFreq::roll_wsum(foo, weight_s),
  r_oll=roll::roll_var(foo, weights=weight_s, width=NROW(weight_s)),
  times=10))[, c(1, 4, 5)]



###############
### OHLC momentum

# set up data
oh_lc <- rutils::etf_env$VTI
clo_se <- quantmod::Cl(oh_lc)
hi_gh <- quantmod::Hi(oh_lc)
lo_w <- quantmod::Lo(oh_lc)
re_turns <- clo_se - rutils::lag_it(clo_se)
returns_adv <- rutils::lag_it(re_turns, lagg=-1)
returns_high <- hi_gh - rutils::lag_it(hi_gh)
returns_low <- lo_w - rutils::lag_it(lo_w)



## run regressions of future returns against different indicators

# single indicator
in_dicator <- re_turns + returns_high + returns_low
reg_model <- lm(returns_adv ~ in_dicator)
summary(reg_model)

# three indicators - lower lows is most significant
reg_model <- lm(returns_adv ~ re_turns + returns_high + returns_low)
summary(reg_model)

# single indicator
# lower lows indicator works well in bearish periods
in_dicator <- (-re_turns - returns_high + returns_low)
in_dicator <- sign(in_dicator)
reg_model <- lm(returns_adv ~ in_dicator)
summary(reg_model)

# Simulate strategy
pnl_s <- cumsum(rutils::lag_it(in_dicator)*re_turns)
colnames(pnl_s) <- "strategy"

# Plot
library(dygraphs)
dygraphs::dygraph(cbind(clo_se, pnl_s)) %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("red", "blue"))



###############
### Forecast and trade minutely stock returns, using static betas over design matrix

re_turns <- 6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, scal_e=FALSE)
look_back <- 5
rets_lag <- 6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, lag=look_back, scal_e=FALSE)
colnames(rets_lag) <- "rets_lag"
rets_lag2 <- 6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, lag=2*look_back, scal_e=FALSE)
colnames(rets_lag2) <- "rets_lag2"
rets_adv <- rutils::lag_it(rets_lag, lag=-look_back)
colnames(rets_adv) <- "rets_adv"
rets_adv2 <- rutils::lag_it(rets_lag2, lag=-2*look_back)
colnames(rets_adv2) <- "rets_adv2"
vari_ance <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY, scal_e=FALSE)
vari_ance <- HighFreq::roll_vwap(oh_lc=HighFreq::SPY, x_ts=vari_ance, look_back=look_back)
colnames(vari_ance) <- "variance"
# var_lag2 <- HighFreq::roll_vwap(oh_lc=HighFreq::SPY, x_ts=vari_ance, look_back=2*look_back)
# colnames(var_lag2) <- "var_lag2"

# sk_ew <- 6.5*60^4*HighFreq::run_skew(oh_lc=HighFreq::SPY)
# sk_ew <- ifelse(vari_ance==0, 0, sk_ew/(vari_ance)^(1.5))
# sk_ew[1, ] <- 0
# sk_ew <- roll_vwap(oh_lc=HighFreq::SPY, x_ts=sk_ew, look_back=2*look_back)
# colnames(sk_ew) <- "skew"
# set plot panels
# par(mfrow=c(2,1))
# chart_Series(HighFreq::SPY["2013-11-15"], name="SPY")
# chart_Series(SPY_design["2013-11-15"], name="position_s")
# plot.zoo(position_s[match(index(HighFreq::SPY["2013-11-15"]), index(HighFreq::SPY))], main="position_s")
# bars with zero skew
# bar_s <- HighFreq::SPY["2013-11-15"][(sk_ew["2013-11-15"]==0)]

# sharp_e <- HighFreq::run_sharpe(oh_lc=HighFreq::SPY)
# sharpe_rolling <- roll_vwap(oh_lc=HighFreq::SPY, x_ts=sharp_e, look_back=look_back)
# sharpe_rolling <- as.numeric(stats::filter(sharp_e, filter=weight_s, sides=2))
# colnames(sharpe_rolling) <- "sharpe"

hu_rst <- roll_hurst(oh_lc=HighFreq::SPY, look_back=look_back)
colnames(hu_rst) <- "hurst"


# rets_lag <- lapply(1:(3*look_back), function(lag) {
#   6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, lag=lag, scal_e=FALSE)
# })  # end lapply
rets_lag <- lapply(1:(3*look_back), HighFreq::run_returns,
                   x_ts=HighFreq::SPY, col_umn=4, scal_e=FALSE)
rets_lag <- 6.5*60*rutils::do_call(cbind, rets_lag)
colnames(rets_lag) <- paste0("rets_lag_", 1:(3*look_back))



## create design matrix
SPY_design <- cbind(rets_lag2, sharpe_rolling)
# SPY_design <- cbind(rets_lag2, z_scores[[3]], hu_rst, sharpe_rolling)
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
beta_s <- -coef(summary(mo_del))[-1, 1]


## Calculate indicator from static betas and apply its rolling z-scores

in_dic <- matrix(rowSums(SPY_design %*% beta_s), ncol=1)
# in_dic <- roll::roll_scale(data=in_dic, width=6, min_obs=1)
# in_dic[is.na(in_dic)] <- 0
# Regress future returns against z-scores
mo_del <- lm(rets_adv2 ~ in_dic)
summary(mo_del)
# Calculate rolling range of z-scores
look_back <- 21
ran_ge <- cbind(min=-runMax(-in_dic, n=look_back),
                max=runMax(in_dic, n=look_back))
ran_ge[1:(look_back-1), ] <- ran_ge[look_back, ]
ran_ge <- rutils::lag_it(ran_ge)
# Calculate position_s and pnls from z-scores and ran_ge
position_s <- ifelse(in_dic > 0.96*ran_ge[, "max"], -1,
                     ifelse(in_dic < 0.96*ran_ge[, "min"], 1, NA))
position_s[1] <- 0
position_s <- na.locf(position_s, na.rm=FALSE)
# position_s <- rutils::lag_it(position_s)
position_s <- lapply(1:3, rutils::lag_it, x_ts=position_s)
position_s <- rutils::do_call(cbind, position_s)
position_s <- rowSums(position_s)/NCOL(position_s)
cum_pnls <- cumsum(position_s*re_turns)
x11()
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


## Calculate the strategy success rate as the pnl divided by asset return volatility (to normalize the asset returns)
# result: the plot of the strategy success rate doesn't show any time variation or dependence on volatility
vari_ance <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY, scal_e=TRUE)
vari_ance <- sqrt(vari_ance)
vari_ance <- HighFreq::roll_vwap(oh_lc=HighFreq::SPY, x_ts=vari_ance, look_back=look_back)
bar <- rutils::diff_it(cum_pnls, lag=look_back) / vari_ance
bar[1] <- 0
plot.zoo(bar[end_days], main="bar", xlab=NA, ylab=NA)
# the strategy average daily success rate isn't more successful when the volatility is higher
foo <- apply.daily(abs(bar), FUN=sum)
plot.zoo(foo, main="foo", xlab=NA, ylab=NA)


# Calculate correlation between strategy pnl_s and vari_ance: there is no correlation
vari_ance <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY, scal_e=TRUE)
vari_ance <- sqrt(vari_ance)
range(vari_ance)
range(vari_ance[vari_ance > 1e-06])
pnl_s <- position_s*re_turns
mo_del <- lm(pnl_s[vari_ance > 1e-03] ~ vari_ance[vari_ance > 1e-03])
summary(mo_del)
plot(x=as.numeric(vari_ance[vari_ance > 1e-03]), y=as.numeric(pnl_s[vari_ance > 1e-03]))


## Calculate the strategy success rate as the product of the forecast position_s times the actual position (return direction)
# result: there is no significant correlation between the daily average success rate and the level of vari_ance
bar <- apply.daily(position_s*sign(re_turns), FUN=sum)
foo <- apply.daily(vari_ance, FUN=sum)
mo_del <- lm(bar ~ foo)
summary(mo_del)
plot(x=as.numeric(foo), y=as.numeric(bar))
plot.zoo(cbind(foo, cumsum(bar)))



## Calculate z-scores and apply them to regression of future returns

# function for calculating z-scores
z_score <- function(width) {
  z_score <- roll::roll_scale(data=HighFreq::SPY[, 4], width=width, min_obs=1)
  z_score[is.na(z_score)] <- 0
  colnames(z_scores) <- paste0("z_width_", width)
  z_score
}  # end z_score

# Calculate z-scores for different widths (lookbacks)
width_s <- 4:20
z_scores <- lapply(width_s, z_score)
names(z_scores) <- paste0("z_width_", width_s)
# z_scores <- lapply(names(z_scores), function(x) {
#   colnames(z_scores[[x]]) <- x
#   z_scores[[x]]
# })  # end lapply


# Regress future returns against z-scores
t_val <- function(z_scores) {
  mo_del <- lm(rets_adv2 ~ z_scores)
  coef(summary(mo_del))[2, 3]
}  # end t_val

t_vals <- sapply(z_scores, t_val)
# t_vals <- cbind(width_s, t_val)
t(sapply(z_scores, range))


# Calculate rolling range of z-scores

range(z_scores[[3]])
ran_ge <- cbind(min=-runMax(-z_scores[[3]], n=look_back),
                max=runMax(z_scores[[3]], n=look_back))
ran_ge[1:(look_back-1), ] <- ran_ge[look_back, ]
ran_ge <- rutils::lag_it(ran_ge)
# range(ran_ge[, 1])
# plot.zoo(ran_ge[end_days, 1], main="rolling min of z-scores", xlab=NA, ylab=NA)


# Calculate position_s and pnls from z-scores and ran_ge

position_s <- ifelse(z_scores[[3]] > 0.96*ran_ge[, "max"], -1,
                     ifelse(z_scores[[3]] < 0.96*ran_ge[, "min"], 1, NA))
position_s[1] <- 0
position_s <- na.locf(position_s, na.rm=FALSE)
# position_s <- rutils::lag_it(position_s)
position_s <- lapply(1:3, rutils::lag_it, x_ts=position_s)
position_s <- rutils::do_call(cbind, position_s)
position_s <- -rowSums(position_s)/NCOL(position_s)
cum_pnls <- cumsum(position_s*re_turns)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)

# Average number of trades per day
sum(abs(rutils::diff_it(position_s))) / mean(abs(position_s)) / 2 / NROW(end_days)
# Average holding period (minutes)
2*NROW(position_s) / sum(abs(rutils::diff_it(position_s))) * mean(abs(position_s))


# Calculate total pnls from z-scores (dynamic thresh_old)
cum_pnl <- function(z_scores, thresh_old=1.0, look_back=21, lag=3) {
  ran_ge <- cbind(min=-runMax(-z_scores, n=look_back),
                  max=runMax(z_scores, n=look_back))
  ran_ge[1:(look_back-1), ] <- ran_ge[look_back, ]
  ran_ge <- rutils::lag_it(ran_ge)
  position_s <- ifelse(z_scores > thresh_old*ran_ge[, "max"], -1,
                       ifelse(z_scores < thresh_old*ran_ge[, "min"], 1, NA))
  position_s[1] <- 0
  position_s <- na.locf(position_s, na.rm=FALSE)
  position_s <- lapply(1:lag, rutils::lag_it, x_ts=position_s)
  position_s <- rutils::do_call(cbind, position_s)
  position_s <- rowSums(position_s)/NCOL(position_s)
  cumsum(position_s*re_turns)
}  # end cum_pnl

bar <- cum_pnl(z_scores=z_scores[[3]], thresh_old=0.96, look_back=21, lag=3)
plot.zoo(bar[end_days], main="cum_pnls", xlab=NA, ylab=NA)

# Calculate total pnls for different thresh_olds
thresh_olds <- seq(from=0.9, to=1.1, by=0.01)
bar <- lapply(thresh_olds, cum_pnl,
              z_scores=z_scores[[3]],
              look_back=21,
              lag=3)  # end lapply
names(bar) <- paste0("threshold_", thresh_olds)
unlist(lapply(bar, last))

# Calculate total pnls for different look_backs
look_backs <- seq(from=11, to=31, by=2)
bar <- lapply(look_backs, cum_pnl,
              z_scores=z_scores[[3]],
              thresh_old=0.96,
              lag=3)  # end lapply
names(bar) <- paste0("look_back_", look_backs)
unlist(lapply(bar, last))



# function for calculating position_s from z-scores (static thresh_old)
z_pos <- function(z_scores, thresh_old=2.0) {
  position_s <- ifelse(abs(z_scores) > thresh_old, sign(z_scores), NA)
  position_s[1] <- 0
  na.locf(position_s, na.rm=FALSE)
}  # end z_pos

# Calculate time series of pnls from z-scores
position_s <- z_pos(z_scores[[3]], thresh_old=1.4)
position_s <- lapply(1:3, rutils::lag_it, x_ts=position_s)
position_s <- rutils::do_call(cbind, position_s)
position_s <- rowSums(position_s)/NCOL(position_s)
cum_pnls <- -cumsum(position_s*re_turns)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


# Calculate total pnls from z-scores
cum_pnl <- function(z_scores, thresh_old=2.0, lag=3) {
  position_s <- z_pos(z_scores, thresh_old=thresh_old)
  position_s <- lapply(1:lag, rutils::lag_it, x_ts=position_s)
  position_s <- rutils::do_call(cbind, position_s)
  position_s <- rowSums(position_s)/NCOL(position_s)
  -sum(position_s*re_turns)
}  # end cum_pnl

cum_pnl(z_scores[[3]], thresh_old=1.4, lag=3)

# Calculate total pnls for different thresh_olds
thresh_olds <- seq(from=1.0, to=3.0, by=0.1)
bar <- sapply(thresh_olds, cum_pnl,
              z_scores=z_scores[[3]],
              lag=3)
bar <- cbind(thresh_olds, bar)


position_s <- lapply(z_scores, z_pos, thresh_old=1.0)
position_s <- rutils::do_call(cbind, position_s)
position_s <- rutils::lag_it(position_s, lag=1)


z_rets <- lapply(z_scores, function(z_scores) {
  position_s <- ifelse(abs(z_scores) > 2.0, sign(z_scores), NA)
  position_s[1] <- 0
  position_s <- na.locf(position_s, na.rm=FALSE)
  position_s <- rutils::lag_it(position_s)
  -position_s*re_turns
})  # end lapply

cum_pnls <- lapply(z_rets, cumsum)
cum_pnls <- rutils::do_call(cbind, cum_pnls)

cum_pnls <- rowSums(cum_pnls)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


## Simulate weighting the best performing strategies

weight_s <- rutils::diff_it(cum_pnls, lag=1000)
row_sums <- rowSums(abs(weight_s))
row_sums[row_sums==0] <- 1.0
weight_s <- weight_s/row_sums
weight_s <- rutils::lag_it(weight_s)

barr <- cumsum(rowSums(weight_s*rutils::do_call(cbind, z_rets)))
plot.zoo(barr[end_days], main="cum_pnls")



## Perform rolling beta regressions in parallel
lm_roll <- roll::roll_lm(x=SPY_design,
                         y=rets_adv2,
                         width=3000*look_back)
beta_s <- lm_roll$coefficients[, -1]
beta_s[!complete.cases(beta_s), ] <- 0
# sum(is.na(beta_s))
beta_s <- rutils::lag_it(beta_s, lag=2*look_back)
# beta_s <- na.omit(beta_s[, 2])
chart_Series(x=beta_s[end_days, "rets_lag2"], name="rolling betas")


## Perform rolling daily beta regressions
# Calculate daily endpoints
end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
# length of lookback window
look_back <- 3000*look_back

# Initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, varlist=c("look_back", "rets_adv2", "SPY_design"))

# Perform parallel loop over daily endpoints - strange results independent of look_back
lm_roll <- parLapply(clus_ter, end_days, function(end_day) {
  in_dex <- max(1, end_day-look_back):end_day
  summary(lm(rets_adv2[in_dex, ] ~ SPY_design[in_dex, ]))
})  # end parLapply

# stop R processes over cluster under Windows
stopCluster(clus_ter)

# Perform loop over daily endpoints
lm_roll <- lapply(end_days, function(end_day) {
  in_dex <- max(1, end_day-look_back):end_day
  summary(lm(rets_adv2[in_dex, ] ~ SPY_design[in_dex, ]))
})  # end lapply


t_vals <- sapply(lm_roll, function(x) x$coefficients[-1, 3])
t_vals <- t(t_vals)
t_vals <- xts(t_vals, order.by=index(SPY_design[end_days, ]))
colnames(t_vals) <- colnames(SPY_design)
plot.zoo(cbind(t_vals[, 2], HighFreq::SPY[end_days, 4])["2010", ])

co_ef <- sapply(lm_roll, function(x) x$coefficients[-1, 1])
co_ef <- t(co_ef)
colnames(co_ef) <- colnames(SPY_design)
co_ef <- rutils::lag_it(co_ef)
beta_s <- NA*SPY_design
beta_s[1, ] <- 0
beta_s[end_days, ] <- co_ef
beta_s <- na.locf(beta_s, na.rm=FALSE)

# Calculate position_s and pnl_s
position_s <- rowSums(SPY_design * beta_s)
# static beta_s work better than rolling regression
# beta_s <- c(rep(-1.0, 5), 0.00)
position_s <- rowSums(SPY_design %*% beta_s)
# position_s <- ifelse(abs(position_s)>0.01, sign(position_s), NA)
# position_s[1] <- 0
# position_s <- na.locf(position_s, na.rm=FALSE)
position_s <- rutils::lag_it(position_s)
position_s <- rutils::roll_sum(position_s, look_back=5) / 5
# histo_gram <- hist(position_s, breaks=200, xlim=c(-0.05, 0.05))
# Average number of trades per day
sum(abs(rutils::diff_it(position_s))) / mean(abs(position_s)) / 2 / NROW(end_days)
# Average holding period (minutes)
2*NROW(position_s) / sum(abs(rutils::diff_it(position_s))) * mean(abs(position_s))
# colnames(position_s) <- "position_s"
# plot.zoo(cbind(position_s[end_days], HighFreq::SPY[end_days, 4])["2010", ])
pnl_s <- cumsum(position_s*re_turns)
colnames(pnl_s) <- "SPY contrarian"
chart_Series(x=pnl_s["2008-01-29/2008-01-31"], name="pnl_s")
chart_Series(x=pnl_s[end_days, ], name="pnl_s")

# Apply moving average crossover strategy to resulting pnl_s
# Define aggregation window, decay parameter, and calculate VWAP
lamb_da <- 0.01
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:(10*look_back+1))
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(pnl_s, filter=weight_s, sides=1)
ew_ma <- as.numeric(ew_ma)
ew_ma[1:(10*look_back)] <- ew_ma[10*look_back+1]
# Calculate VWAP indicator
in_dic <- sign(pnl_s - ew_ma)
# Determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1

# Calculate positions, either: -1, 0, or 1
pos_vwap <- rep(NA_integer_, NROW(pnl_s))
pos_vwap[1] <- 0
pos_vwap[trade_dates] <- in_dic[trade_dates]
pos_vwap <- na.locf(pos_vwap, na.rm=FALSE)
pos_vwap <- xts(pos_vwap, order.by=index(pnl_s))

# Calculate daily profits and losses
pnl_vwap <- cumsum(pos_vwap*rutils::diff_it(pnl_s))
colnames(pnl_vwap) <- "SPY contrarian plus vwap"

# Plot
date_s <- "2010-05-05/2010-05-07"
back_test <- cbind(HighFreq::SPY[, 4], cum_pnls)[date_s, ]
# back_test <- cbind(HighFreq::SPY[, 4], sharpe_rolling)[date_s, ]
back_test[, 1] <- back_test[, 1] - as.numeric(back_test[1, 1])
back_test[, 2] <- back_test[, 2] - as.numeric(back_test[1, 2])
back_test[, 2] <- 3*back_test[, 2] / max(back_test[, 2])

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(back_test, theme=plot_theme,
             name="SPY contrarian strategy plus vwap")
add_TA(cbind(HighFreq::SPY[, 4], position_s)[date_s, 2] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(cbind(HighFreq::SPY[, 4], position_s)[date_s, 2] < 0, on=-1,
       col="lightgrey", border="lightgrey")
legend("topleft", legend=c("pnl_s", "pnl_vwap"),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")



## Simulating minutely EWMA strategies


# Define function for simulating minutely EWMA crossover strategy
simu_ewma <- function(x_ts, lamb_da=0.05, look_back=51) {
  # Calculate EWMA prices
  # weight_s <- exp(-lamb_da*(1:look_back))
  # weight_s <- weight_s/sum(weight_s)
  # ew_ma <- as.numeric(stats::filter(x_ts, filter=weight_s, sides=1))
  # ew_ma[1:(look_back-1)] <- ew_ma[look_back]
  ew_ma <- HighFreq::roll_vwap(x_ts, look_back=look_back)
  # Determine dates right after EWMA has crossed prices
  in_dic <- sign(as.numeric(x_ts[, 4] - ew_ma))
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(x_ts)]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(x_ts))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
  na.locf(position_s, na.rm=FALSE)
}  # end simu_ewma

end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
end_hours <- xts::endpoints(HighFreq::SPY, "hours")[-1]
positions_hours <- simu_ewma(x_ts=HighFreq::SPY[end_hours], lamb_da=0.01, look_back=1001)
position_s <- rep(NA_integer_, NROW(HighFreq::SPY))
position_s[1] <- 0
position_s[end_hours] <- positions_hours
position_s <- na.locf(position_s, na.rm=FALSE)
chart_Series(-cumsum(position_s*re_turns)[end_days], name="SPY minutely vwap strategy")
position_s <- xts(position_s, order.by=index(re_turns))
add_TA(position_s > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
       col="lightgrey", border="lightgrey")



# Perform parallel loop over lamb_das
lamb_das <- seq(0.001, 0.03, 0.001)
window_s <- seq(500, 1500, 100)

# Initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, varlist=c("oh_lc", "look_back", "simu_ewma"))
# Perform parallel loop over lamb_das under Windows
re_turns <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, look_back=look_back)[, "re_turns"]
})  # end parLapply


## Simple trend-following strategy

bar <- rutils::etf_env$re_turns[, "VTI"]
bar <- cumsum(bar*sign(rutils::lag_it(bar)))
chart_Series(bar, name="Simple trend-following strategy")


bar <- -cumsum(re_turns*sign(rutils::lag_it(sk_ew, lag=2)))
bar <- rutils::roll_sum(rutils::lag_it(sk_ew), look_back=3) / 3
bar <- -cumsum(re_turns*sign(bar))


position_s <- ifelse(abs(SPY_design)>0.052, sign(SPY_design), NA)
position_s[1] <- 0
position_s <- na.locf(position_s, na.rm=FALSE)
position_s <- rutils::lag_it(position_s)
pnl_s <- -cumsum(position_s*re_turns)
colnames(pnl_s) <- "SPY skew contrarian"
chart_Series(x=pnl_s[end_days, ], name="SPY skew contrarian")

cum_pnl <- function(position_s=sk_ew, thresh_old=0.05, re_turns=re_turns) {
  position_s <- ifelse(abs(position_s)>thresh_old, sign(position_s), NA)
  position_s[1] <- 0
  position_s <- na.locf(position_s, na.rm=FALSE)
  position_s <- rutils::lag_it(position_s)
  -sum(position_s*re_turns)
}  # end cum_pnl

cum_pnl(thresh_old=0.045, re_turns=re_turns)

thresh_olds <- seq(from=0.04, to=0.065, by=0.001)
bar <- sapply(thresh_olds, cum_pnl,
              position_s=as.numeric(rutils::lag_it(SPY_design)),
              re_turns=re_turns)
bar <- cbind(thresh_olds, bar)


###############
### simulation of trading strategy

## cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(sharpe_ratios, re_turns, end_points) {
  be_st <- apply(sharpe_ratios, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  re_turns <- lapply(seq_along(be_st), function(in_dex) {
    re_turns[end_points[in_dex+1, 1]:end_points[in_dex+1, 2], be_st[in_dex]]
  })  # end lapply
  sum(rutils::do_call(rbind, re_turns))
}  # end cum_pnl

cum_pnl(sharpe_ratios, re_turns, end_points)

# Switch to best asset with biggest SR
be_st <- apply(sharpe_ratios, 1, which.max)
be_st <- rutils::lag_it(be_st)
be_st[1] <- 1
bar <- lapply(seq_along(be_st), function(in_dex) {
  re_turns[end_points[in_dex+1, 1]:end_points[in_dex+1, 2], be_st[in_dex]]
})  # end lapply
bar <- rutils::do_call(rbind, bar)

chart_Series(x=cumsum(bar), name="Back-test of SR strategies")

## simulation for determining the optimal length of the lookback interval

library(rutils)
options(max.print=40)
oh_lc <- HighFreq::SPY["/2008-03"]
in_dex <- index(oh_lc)
n_row <- NROW(oh_lc)

# Calculate close to close percentage returns
cl_ose <- Cl(oh_lc)
re_turns <- 60*HighFreq::run_returns(x_ts=HighFreq::SPY)

# Define aggregation window and decay parameter
look_back <- 51
lamb_da <- 0.05
# Calculate EWMA prices
weight_s <- exp(-lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(look_back-1)] <- ew_ma[look_back]
ew_ma <- xts(ew_ma, order.by=index(oh_lc))
colnames(ew_ma) <- "VTI EWMA"

# Determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_it(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# Calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
position_s <- na.locf(position_s, na.rm=FALSE)
position_s <- xts(position_s, order.by=index(oh_lc))

prices_lag <- rutils::lag_it(cl_ose)
position_lagged <- rutils::lag_it(position_s)
# Calculate daily profits and losses
re_turns <- position_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# Calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]), pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")


# Define function for simulating daily EWMA crossover strategy
simu_ewma <- function(oh_lc, lamb_da=0.05, look_back=51) {
  # Calculate EWMA prices
  weight_s <- exp(-lamb_da*1:look_back)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(look_back-1)] <- ew_ma[look_back]
  # Determine dates right after EWMA has crossed prices
  in_dic <- xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # Calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(cl_ose))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
  position_s <- xts(na.locf(position_s, na.rm=FALSE), order.by=index(oh_lc))
  op_en <- Op(oh_lc)
  prices_lag <- rutils::lag_it(cl_ose)
  position_lagged <- rutils::lag_it(position_s)
  # Calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <-
    position_lagged[trade_dates] *
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    position_s[trade_dates] *
    (cl_ose[trade_dates] - op_en[trade_dates])
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("position_s", "re_turns")
  out_put
}  # end simu_ewma


# Perform parallel loop over lamb_das
lamb_das <- seq(0.001, 0.03, 0.001)
lamb_das <- seq(0.01, 1.0, 0.1)

# Initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, varlist=c("oh_lc", "look_back", "simu_ewma"))
# Perform parallel loop over lamb_das under Windows
re_turns <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # Simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, look_back=look_back)[, "re_turns"]
})  # end parLapply


# set up loop over lookback windows
# length of lookback window
# look_back <- 11
# Define end_points at end of every day
end_points <- xts::endpoints(oh_lc, on="days")
# num_agg <- n_row %/% look_back
# end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
start_points <- end_points[c(1, 1:(len_gth-1))] + 1
# redefine end_points
end_points <- cbind(start_points, end_points)

# Perform parallel loop over re_turns
clusterExport(clus_ter, varlist=c("len_gth", "end_points"))
sharpe_ratios <- parLapply(clus_ter, re_turns, function(re_turns) {
  sapply(2:len_gth, function(in_dex) {
    x_ts <- re_turns[end_points[in_dex, 1]:end_points[in_dex, 2]]
    # Calculate annualized Sharpe ratio of returns
    sqrt(260)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
  })  # end sapply
})  # end parLapply

sharpe_ratios <- rutils::do_call(cbind, sharpe_ratios)

# Calculate dispersion of SRs of individual strategies over time periods
apply(sharpe_ratios, 2, sd)
# Calculate dispersion of SRs of strategies in each time period
foo <- apply(sharpe_ratios, 1, sd)
mean(foo)

# Calculate differences of SRs over periods
foo <- apply(sharpe_ratios, 2, rutils::diff_it)
dim(foo)
dim(sharpe_ratios)
tail(foo)
tail(sharpe_ratios)

# Are the sharpe_ratios autocorrelated?
# yes, about -50%
bar <- foo[-NROW(foo), ]
bar <- rbind(rep(0, NCOL(foo)), bar)
bar <- bar*foo
colSums(bar) / apply(foo, 2, sd) / NROW(bar)

# Switch to best strategy
bar <- apply(sharpe_ratios, 1, which.max)

# Switch to strategy with biggest differences of SRs over periods
bar <- apply(foo, 1, which.max)
bar <- rutils::lag_it(bar)
bar[1] <- 1
bar <- lapply(2:len_gth, function(in_dex) {
  re_turns[[bar[in_dex-1]]][end_points[in_dex, 1]:end_points[in_dex, 2]]
})  # end lapply
bar <- rutils::do_call(rbind, bar)

# Average over all strategies
bar <- rutils::do_call(cbind, re_turns)
bar <- xts(rowSums(bar), order.by=index(re_turns[[1]]))

chart_Series(x=-cumsum(bar), name="Back-test of EWMA strategies")


## Perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 50*(5:30)

foo <- sapply(look_backs, function(look_back) {
  # Define end_points with beginning stub
  num_agg <- n_row %/% look_back
  end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  len_gth <- NROW(end_points)
  # start_points are single-period lag of end_points
  start_points <- end_points[c(1, 1:(len_gth-1))] + 1
  # redefine end_points
  end_points <- cbind(start_points, end_points)
  
  # Perform parallel loop over re_turns
  clusterExport(clus_ter, varlist=c("len_gth", "end_points"))
  sharpe_ratios <- parLapply(clus_ter, re_turns, function(re_turns) {
    sapply(2:len_gth, function(in_dex) {
      x_ts <- re_turns[end_points[in_dex, 1]:end_points[in_dex, 2]]
      # Calculate annualized Sharpe ratio of returns
      sqrt(260)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
    })  # end sapply
  })  # end parLapply
  
  sharpe_ratios <- rutils::do_call(cbind, sharpe_ratios)
  sharpe_ratios[which(is.na(sharpe_ratios), arr.ind=TRUE)] <- 1
  
  # Calculate dispersion of SRs
  c(by_strategy=mean(apply(sharpe_ratios, 2, sd)),
    by_period=mean(apply(sharpe_ratios, 1, sd)))
})  # end sapply

foo <- t(foo)
dim(foo)
foo
plot(foo[, 1]/foo[, 2], t="l")

## end perform loop over lookback windows


# stop R processes over cluster under Windows
stopCluster(clus_ter)

