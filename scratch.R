###############
### Create trending portfolios of similar ETFs.

# Load packages
library(rutils)

# Calculate ETF returns and volumes
# VXX with SVXY

# Calculate ETF returns and volumes
sym_bols <- c("VTI", "SVXY", "VXX")
re_turns <- na.omit(rutils::etf_env$re_turns[, sym_bols])
date_s <- zoo::index(re_turns)

look_back <- 11
# Scale the volume by the rolling average volume
rets_scaled <- lapply(sym_bols, function(sym_bol) {
  oh_lc <- get(sym_bol, rutils::etf_env)
  re_turns <- rutils::diff_it(log(quantmod::Cl(oh_lc)))
  vol_ume <- quantmod::Vo(oh_lc)
  volume_rolling <- roll::roll_mean(vol_ume, width=look_back)
  volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
  vol_ume <- vol_ume/volume_rolling
  # Divide  the returns by the volume - use trading time (volume clock)
  re_turns/vol_ume
})  # end lapply

# Scale by the High minus Low range
rets_scaled <- lapply(sym_bols, function(sym_bol) {
  oh_lc <- log(get(sym_bol, rutils::etf_env))
  re_turns <- rutils::diff_it(quantmod::Cl(oh_lc))
  rang_e <- (quantmod::Hi(oh_lc) - quantmod::Lo(oh_lc))
  re_turns/rang_e
})  # end lapply

# Scale by the rolling volatility
rets_scaled <- lapply(sym_bols, function(sym_bol) {
  oh_lc <- log(get(sym_bol, rutils::etf_env))
  re_turns <- rutils::diff_it(quantmod::Cl(oh_lc))
  vari_ance <- HighFreq::roll_var_ohlc(oh_lc=oh_lc, look_back=look_back, scal_e=FALSE)
  colnames(vari_ance) <- "variance"
  vol_at <- sqrt(vari_ance)
  re_turns/vol_at
})  # end lapply

rets_scaled <- do.call(cbind, rets_scaled)
colnames(rets_scaled) <- do.call(rbind,(strsplit(colnames(rets_scaled), split="[.]")))[, 1]
rets_scaled <- na.omit(rets_scaled)
rets_scaled <- rets_scaled[date_s]

weight_s <- 1/sapply(rets_scaled, sd)/100
port_f <- rets_scaled %*% weight_s

# PACF of AR(1) process
x11(width=6, height=5)
pac_f <- pacf(port_f, lag=10, xlab="", ylab="", main="")
abs(sum(pac_f$acf))

sum_pacf <- function(weight_s) {
  port_f <- rets_scaled %*% weight_s
  pac_f <- pacf(port_f, lag=10, plot=FALSE)
  -sum(pac_f$acf) - sum(port_f) + (1-sum(weight_s^2))^2
}  # end sum_pacf


# Calculate end points
inter_val <- 21
n_rows <- NROW(rets_scaled)
num_agg <- n_rows %/% inter_val
end_p <- c(0, n_rows - num_agg*inter_val + (0:num_agg)*inter_val)

# Calculate Hurst
hurs_t <- function(weight_s) {
  port_f <- rets_scaled %*% weight_s
  price_s <- cumsum(port_f)
  r_s <- sapply(2:NROW(end_p), function(ep) {
    in_dex <- end_p[ep-1]:end_p[ep]
    diff(range(price_s[in_dex]))/sd(port_f[in_dex])
  })  # end sapply
  # Calculate Hurst from single data point
  -log(mean(r_s))/log(inter_val) - sum(port_f) + (1-sum(weight_s^2))^2
}  # end sum_pacf

op_tim <- optim(par=c(0.1, 0.1, 0.1), 
                fn=hurs_t,
                method="L-BFGS-B",
                upper=c(10, 10, 10),
                lower=c(0, 0, 0))
# Optimal parameters and value
weight_s <- op_tim$par
port_f <- rets_scaled %*% weight_s
pacf(port_f, lag=10, xlab="", ylab="", main="")
port_f <- xts::xts(port_f, date_s)

da_ta <- cbind(re_turns$VTI, port_f)
sharp_e <- sqrt(252)*sapply(da_ta, function(x) mean(x)/sd(x[x<0]))

colnames(pnl_s) <- c(paste(input$sym_bol, "Returns"), "Strategy", "Buy", "Sell")

cap_tion <- paste("Strategy for", input$sym_bol, "Returns Scaled by the Trading Volumes / \n", 
                  paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                  "Number of trades=", n_trades)

col_names <- colnames(da_ta)
dygraphs::dygraph(cumsum(da_ta), main="Autoregressive Portfolio") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="red")

dygraphs::dygraph(da_ta, main="Autoregressive Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)



###############
### Autoregressive strategy using the principal components  
# of average returns as predictors.
# Mostly in app_ar_pca_strat.R

# Load packages
library(rutils)

# Calculate ETF returns and volumes
sym_bol <- "VTI"
oh_lc <- get(sym_bol, rutils::etf_env)
date_s <- zoo::index(oh_lc)
n_rows <- NROW(oh_lc)
clos_e <- log(quantmod::Cl(oh_lc))
re_turns <- rutils::diff_it(clos_e)
cum_rets <- cumsum(re_turns)
vol_ume <- quantmod::Vo(oh_lc)

in_sample <- 1:(n_rows %/% 4)
out_sample <- (n_rows %/% 4 + 1):n_rows

# Scale the volume by the rolling average volume
look_back <- 11
volume_rolling <- roll::roll_mean(vol_ume, width=look_back)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
vol_ume <- vol_ume/volume_rolling

# Divide  the returns by the volume - use trading time (volume clock)
# rets_scaled <- ifelse(vol_ume > 0, re_turns/vol_ume, 0)
rets_scaled <- re_turns/vol_ume

## Aersion using only recent returns

# First version
look_backs <- 2:25
de_sign <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x))
de_sign <- do.call(cbind, de_sign)
de_sign[1, ] <- 0
de_sign <- zoo::na.locf(de_sign)
# sum(is.na(de_sign))
de_sign <- cbind(rets_scaled, de_sign)
max_back <- last(look_backs)
# res_ponse <- rutils::lag_it(de_sign[, max_back], lagg=(-max_back))
res_ponse <- sqrt(max_back)*roll::roll_mean(re_turns, max_back)
res_ponse[1:(max_back-1)] <- 0
res_ponse <- rutils::lag_it(res_ponse, lagg=(-max_back))

# Second version
# Define predictor matrix for forecasting
order_max <- 10
de_sign <- lapply(1:order_max, rutils::lag_it, in_put=re_turns)
de_sign <- do.call(cbind, de_sign)
colnames(de_sign) <- paste0("pred_", 1:NCOL(de_sign))
# res_ponse <- re_turns
res_ponse <- rutils::lag_it(de_sign[, order_max], lagg=(-order_max))


###########
## Old version
# Calculate de_sign equal to the rolling means
look_backs <- c(5, 20, 80, 250)
# de_sign <- lapply(look_backs, roll::roll_mean, x=re_turns)
# Scale the rolling means so they have similar volatities
de_sign <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(re_turns, x))
# de_sign <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(re_turns, x)/sqrt(roll::roll_var(re_turns, x)))
de_sign <- do.call(cbind, de_sign)
de_sign[1, ] <- 0
de_sign <- zoo::na.locf(de_sign)
# sum(is.na(de_sign))
colnames(de_sign) <- paste0("back_", look_backs)
sapply(de_sign, sd)
# Standardize (de-mean and scale) the de_sign
# de_sign <- lapply(de_sign, function(x) {(x - mean(x))/sd(x)})
# de_sign <- rutils::do_call(cbind, de_sign)

# Define response as a rolling sum and shift it forward out-of-sample
res_ponse <- rutils::lag_it(de_sign[, 1], lagg=(-look_backs[1]))

###########
## End Old version

# Calculate covariance matrix of de_sign
cov_mat <- cov(de_sign)
# Calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)

# Define predictors as the principal components of de_sign
# eigen_vec <- ei_gen$vectors
predic_tor <- xts::xts(de_sign %*% ei_gen$vectors, order.by=date_s)
colnames(predic_tor) <- paste0("pc", 1:NCOL(predic_tor))
# round(cov(predic_tor), 3)
predic_tor <- rutils::lag_it(predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor)[1] <- "unit"

# Calculate in-sample fitted coefficients
max_eigen <- 4
in_verse <- MASS::ginv(predic_tor[in_sample, 1:max_eigen])
coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
# Calculate out-sample forecasts of re_turns
# forecast_s <- drop(predic_tor[out_sample, 1:3] %*% coeff_fit[1:3])
forecast_s <- drop(predic_tor[out_sample, 1:max_eigen] %*% coeff_fit)
mean((re_turns[out_sample, ] - forecast_s)^2)
drop(cor(re_turns[out_sample, ], forecast_s))

# Lag the positions to trade in next period
position_s <- sign(rutils::lag_it(forecast_s))

# Calculate strategy pnl_s
pnl_s <- cumsum(position_s*re_turns[out_sample])
pnl_s <- cbind(cumsum(re_turns[out_sample]), pnl_s)
colnames(pnl_s) <- c(sym_bol, "Strategy")
dygraphs::dygraph(pnl_s, main="Autoregressive Strategy Performance") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)


###############
# Case for look_backs = 2

# Calculate ETF returns and volumes
sym_bol <- "VTI"
oh_lc <- get(sym_bol, rutils::etf_env)
date_s <- zoo::index(oh_lc)
n_rows <- NROW(oh_lc)
clos_e <- log(quantmod::Cl(oh_lc))
re_turns <- rutils::diff_it(clos_e)
cum_rets <- cumsum(re_turns)
vol_ume <- quantmod::Vo(oh_lc)

in_sample <- 1:(n_rows %/% 2)
out_sample <- (n_rows %/% 2 + 1):n_rows

# Scale the volume by the rolling average volume
look_back <- 11
volume_rolling <- roll::roll_mean(vol_ume, width=look_back)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
vol_ume <- vol_ume/volume_rolling
# Divide  the returns by the volume - use trading time (volume clock)
rets_scaled <- re_turns/vol_ume

look_backs <- 2:5
de_sign <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x))
de_sign <- do.call(cbind, de_sign)
# look_backs <- 2
# de_sign <- sqrt(2)*roll::roll_mean(rets_scaled, 2)
de_sign[1, ] <- 0
de_sign <- zoo::na.locf(de_sign)
de_sign <- cbind(rets_scaled, de_sign)
max_back <- last(look_backs)

res_ponse <- sqrt(max_back)*roll::roll_mean(re_turns, max_back)
res_ponse[1:(max_back-1)] <- 0
res_ponse <- rutils::lag_it(res_ponse, lagg=(-max_back))
cov_mat <- cov(de_sign)
ei_gen <- eigen(cov_mat)
predic_tor <- xts::xts(de_sign %*% ei_gen$vectors, order.by=date_s)
predic_tor <- rutils::lag_it(predic_tor)
# predic_tor <- cbind(rep(1, n_rows), predic_tor)
in_verse <- MASS::ginv(predic_tor[in_sample, ])
coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
forecast_s <- drop(predic_tor[out_sample, ] %*% coeff_fit)
forecast_s <- roll::roll_mean(sign(forecast_s), width=max_back)
forecast_s[1:(max_back-1)] <- 1
pnl_s <- cumsum(forecast_s*re_turns[out_sample])
pnl_s <- cbind(cumsum(re_turns[out_sample]), pnl_s)
colnames(pnl_s) <- c(sym_bol, "Strategy")
dygraphs::dygraph(pnl_s, main="Autoregressive Strategy Performance") %>%
dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
dyLegend(width=500)



###############
### PTS Sentiment SPY trading strategy
# Load packages

# Load packages
library(rutils)
library(data.table)

# Load data with SPY sentiment from csv file
da_ta <- data.table::fread(file="C:/Develop/predictive/data/correlation_news_to_price_change.csv", stringsAsFactors=FALSE)
da_ta <- da_ta[, c("date", "sentiment", "close")]
colnames(da_ta) <- c("date", "sentiment", "SPY")
sapply(da_ta, class)
da_ta <- xts::xts(da_ta[, 2:3], as.Date.IDate(da_ta[, date]))


re_turns <- lapply(da_ta[, -1], rutils::diff_it)
re_turns <- rutils::do_call(cbind, re_turns)
colnames(re_turns) <- c("sentiment", "SPY")
cor(rutils::lag_it(re_turns[, 1]), re_turns[, 2])

x11(width=6, height=5)
plot(SPY ~ sentiment, data=re_turns)

col_names <- colnames(da_ta)
cap_tion <- paste(col_names, collapse=" vs ")
dygraphs::dygraph(da_ta, main=cap_tion) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")




###############
### Benchmark of rolling functions.

# Benchmark the speed of TTR::runMedian()
oh_lc <- HighFreq::SPY["2011"]
clos_e <- log(drop(coredata(Cl(oh_lc))))
ro_ll <- roll::roll_median(clos_e, width=look_back)[-(1:(look_back-1))]
rcpp_roll <- RcppRoll::roll_median(clos_e, n=look_back)
tt_r <- TTR::runMedian(clos_e, n=look_back)[-(1:(look_back-1))]
all.equal(rcpp_roll, coredata(tt_r), check.attributes=FALSE)
all.equal(ro_ll, tt_r, check.attributes=FALSE)
library(microbenchmark)
# roll::roll_median() is several times faster than the other two
summary(microbenchmark(
  ro_ll=roll::roll_median(clos_e, width=look_back),
  rcpp_roll=RcppRoll::roll_median(clos_e, n=look_back),
  tt_r=TTR::runMedian(clos_e, n=look_back),
  times=10))[, c(1, 4, 5)]


# Benchmark the speed of HighFreq::roll_scale()
# Use roll and TTR
re_turns <- rutils::diff_it(clos_e)
med_rets <- roll::roll_median(re_turns, width=look_back)
mad_rets <- TTR::runMAD(re_turns, n=look_back)
re_scaled <- (re_turns - med_rets)/mad_rets
# Use HighFreq::roll_scale()
re_scaledh <- HighFreq::roll_scale(matrix(re_turns, ncol=1), look_back=look_back, use_median=TRUE)
re_scaledh <- drop(re_scaledh)
# Same result up to factor of qnorm(0.75)
tail(re_scaled)/tail(re_scaledh)
library(microbenchmark)
# HighFreq::roll_scale() is over twice as fast
summary(microbenchmark(
  h_freq=HighFreq::roll_scale(matrix(re_turns, ncol=1), look_back=look_back, use_median=TRUE),
  rcpp_roll={
    (re_turns - roll::roll_median(re_turns, width=look_back))/TTR::runMAD(re_turns, n=look_back)
  },
  times=10))[, c(1, 4, 5)]





###############
### Prototype of function get_data() for rutils

get_data <- function(sym_bols,
                     data_dir = NULL, # the directory containing csv files
                     data_env = NULL, # the environment for writing xts into
                     start_date = "2000-01-01",
                     end_date = Sys.Date(),
                     date_fun = match.fun("as.Date"),
                     for_mat = "%Y-%m-%d",
                     header = TRUE,
                     e_cho = TRUE,
                     scrub = TRUE,
                     api.key = NULL) {
  if (is.null(data_dir)) {
    # download prices from Tiingo
    out_put <- quantmod::getSymbols.tiingo(sym_bols,
                                           env = data_env,
                                           from = start_date,
                                           to = end_date,
                                           adjust = TRUE,
                                           auto.assign = TRUE,
                                           api.key = api.key)
    # Adjust the OHLC prices and save back to data_env
    # out_put <- lapply(sym_bols,
    #                   function(sym_bol) {
    #                     assign(sym_bol,
    #                            value = adjust_ohlc(get(sym_bol, envir = data_env)),
    #                            envir = data_env)
    #                     sym_bol
    #                   }
    # )  # end lapply
    invisible(out_put)
  } else {
    # load from csv files
    file_names <- file.path(data_dir, paste0(sym_bols, ".csv"))
    invisible(sapply(file_names, function(file_name) {
      if (e_cho)
        cat("Loading instrument: \t", file_name, "\n")
      da_ta <- xts::as.xts(zoo::read.zoo(file = file_name,
                                         header = header, sep = ",",
                                         drop = FALSE,
                                         FUN = date_fun,
                                         format = for_mat))
      if (scrub) {
        # overwrite NA values
        da_ta <- rutils::na_locf(da_ta)
        da_ta <- rutils::na_locf(da_ta, from_last = TRUE)
      }  # end if
      assign(rutils::get_name(colnames(da_ta)[1]),
             da_ta,
             envir = data_env)
      file_name
    }))  # end sapply
  }  # end if
}  # end get_data



###############
### PTS AAPL tick data trading strategy

# Load packages
library(rutils)
library(data.table)

## Load data with AAPL stock features from csv file
raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl_20201021.csv", sep="\t")
# tail(raw_ticks)
# class(raw_ticks)
# sapply(raw_ticks, class)
# unlist(sapply(raw_ticks, function(x) if (is.numeric(x)) sum(x))) == 0
# Remove empty columns
raw_ticks <- raw_ticks[, .(timestamp=V10, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 10)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Or more recent data
raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl_20201102.csv", sep=",")
raw_ticks <- raw_ticks[, .(timestamp=V8, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 8)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Bind additional pieces of data together
foo <- data.table::fread(file="C:/Develop/predictive/data/aapl_20201030.csv", sep="\t")
foo <- foo[, c(1:3, 8)]
colnames(foo) <- c("price", "volume", "seconds", "timestamp")
foo <- foo[, .(timestamp, seconds, price, volume)]
bar <- (last(raw_ticks)$price - first(foo)$price)
foo[, price := (price + bar)]
raw_ticks <- rbind(raw_ticks, foo)



## Apply Hampel filter to remove price jumps

win_dow <- 111
half_window <- win_dow %/% 2
medi_an <- TTR::runMedian(raw_ticks$price, n=win_dow)
medi_an <- rutils::lag_it(medi_an, lagg=-half_window, pad_zeros=FALSE)
ma_d <- TTR::runMAD(raw_ticks$price, n=win_dow)
ma_d <- rutils::lag_it(ma_d, lagg=-half_window, pad_zeros=FALSE)
ma_d[1:half_window] <- 1
ma_d[ma_d == 0] <- 1

z_scores <- (raw_ticks$price - medi_an)/ma_d
z_scores[is.na(z_scores)] <- 0
z_scores[!is.finite(z_scores)] <- 0
sum(is.na(z_scores))
sum(!is.finite(z_scores))
range(z_scores)
mad(z_scores)
foo <- hist(z_scores, breaks=1000, xlim=c(-5*mad(z_scores), 5*mad(z_scores)))

thresh_old <- 3
bad_ticks <- (abs(z_scores) > thresh_old)
good_ticks <- raw_ticks[!bad_ticks]
good_ticks <- raw_ticks[volume > 10]

## Calculate a vector of returns
re_turns <- rutils::diff_it(good_ticks$price)
n_rows <- NROW(re_turns)


## Simple big tick contrarian strategy - trade on next tick after large volume tick

# Trade on large volume and non-zero return
big_ticks <- (good_ticks$volume >= 2000) & (abs(re_turns) > 0)
# Or: Trade on large volume and go flat if zero return
# big_ticks <- (good_ticks$volume >= 100)
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
position_s[big_ticks] <- -sign(re_turns[big_ticks])
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s, 3)
pnl_s <- cumsum(re_turns*position_s)
x11(width=6, height=5)
plot(pnl_s, t="l")
# Number of trades
sum(abs(rutils::diff_it(position_s))) / NROW(position_s)
# Plot dygraph
date_s <- as.POSIXct(good_ticks$seconds, origin="1970-01-01")
# There are many duplicate dates:
NROW(good_ticks$seconds) == NROW(unique(good_ticks$seconds))
# Make dates unique:
date_s <- xts::make.index.unique(date_s)
pnl_s <- xts::xts(pnl_s, date_s)
# dygraphs::dygraph(pnl_s)
# Combine index with AAPL
pnl_s <- cbind(pnl_s, good_ticks$price)
col_names <- c("Strategy", "AAPL")
colnames(pnl_s) <- col_names
dygraphs::dygraph(pnl_s, main="AAPL Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)

da_ta <- cbind(good_ticks, position_s, pnl_s$Strategy)
data.table::fwrite(da_ta, file="C:/Develop/predictive/data/aapl_strategy.csv")


## Simple big tick contrarian strategy - trade on large volume ticks only

big_ticks <- raw_ticks[volume >= 400]
re_turns <- rutils::diff_it(big_ticks$price)
# Flip position or flatten if re_turns == 0
position_s <- (-rutils::lag_it(sign(re_turns)))
pnl_s <- cumsum(re_turns*position_s)
plot(pnl_s, t="l")
da_ta <- cbind(big_ticks, position_s, pnl_s$Strategy)
data.table::fwrite(da_ta, file="C:/Develop/predictive/data/aapl_strategy.csv")
# Number of trades
sum(abs(rutils::diff_it(position_s))) / NROW(position_s)
# Plot dygraph
date_s <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnl_s <- xts::xts(pnl_s, date_s)
dygraphs::dygraph(pnl_s)
# Combine index with AAPL
pnl_s <- cbind(pnl_s, big_ticks$price)
col_names <- c("Strategy", "AAPL")
colnames(pnl_s) <- col_names
dygraphs::dygraph(pnl_s, main="AAPL Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)


# Or always carry a position - doesn't work so well
position_s <- rep(NA_integer_, NROW(re_turns))
position_s[1] <- 0
position_s <- ifelse(re_turns > 0, -1, position_s)
position_s <- ifelse(re_turns < (-1), 1, position_s)
position_s <- zoo::na.locf(position_s)
# position_s <- (-rutils::lag_it(sign(re_turns)))
position_s <- rutils::lag_it(position_s)


# temp stuff
foo <- tail(raw_ticks, 33)
date_s <- as.POSIXct(foo$V3, origin="1970-01-01")
oh_lc <- foo[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=seconds]
sum(foo[foo$seconds == foo[33]$seconds]$volume)


# Aggregate to OHLC
# tail(raw_ticks)
oh_lc <- raw_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=seconds]
# all.equal(oh_lc$seconds, unique(raw_ticks$seconds))
star_t <- as.numeric(as.POSIXct("2020-10-21 09:30:00"))
en_d <- as.numeric(as.POSIXct("2020-10-21 16:00:00"))
oh_lc <- oh_lc[seconds >= star_t & seconds <= en_d]
foo <- hist(oh_lc$volume[-which(oh_lc$volume > max(oh_lc$volume)/1000)], breaks=1e3, xlim=c(0, 1000))


## Simple OHLC contrarian strategy - trade on large volume only

foo <- oh_lc[volume >= 200]
re_turns <- rutils::diff_it(foo$close)
n_rows <- NROW(re_turns)
# foo <- hist(re_turns, breaks=500, xlim=c(-0.1, 0.1))
position_s <- (-rutils::lag_it(sign(re_turns)))
pnl_s <- cumsum(re_turns*position_s)
plot(pnl_s, t="l")

foo <- lapply(100*(2:10), function(x) {
  re_turns <- rutils::diff_it(oh_lc[volume >= x]$close)
  position_s <- (-rutils::lag_it(sign(re_turns)))
  cumsum(re_turns*position_s)
})  # end lapply
sapply(foo, NROW)
sapply(foo, last)


## AR strategy for OHLC - too complicated?

# Calculate a vector of returns
re_turns <- rutils::diff_it(oh_lc$close)
n_rows <- NROW(re_turns)
hist(re_turns, breaks=500, xlim=c(-0.1, 0.1))

in_sample <- 1:(n_rows %/% 2)
out_of_sample <- (n_rows %/% 2 + 1):n_rows

order_max <- 5  # Define maximum order parameter
look_back <- 5

predic_tor <- rutils::roll_sum(re_turns, look_back=look_back)
# Shift the res_ponse forward into out-of-sample
res_ponse <- rutils::lag_it(predic_tor, lagg=(-look_back))
# Define predictor matrix for forecasting
predic_tor <- sapply(1+look_back*(0:order_max), rutils::lag_it,
                     in_put=predic_tor)
predic_tor <- cbind(rep(1, n_rows), predic_tor)
colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
# Calculate forecasts as function of or_der
forecast_s <- lapply(2:NCOL(predic_tor), function(or_der) {
  # Calculate fitted coefficients
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
  # Calculate out-of-sample forecasts of re_turns
  drop(predic_tor[out_of_sample, 1:or_der] %*% coeff_fit)
})  # end lapply
names(forecast_s) <- paste0("p=", 2:NCOL(predic_tor))

# Calculate out-of-sample PnLs
pnl_s <- sapply(forecast_s, function(x) {
  cumsum(sign(x)*re_turns[out_of_sample])
})  # end sapply
colnames(pnl_s) <- names(forecast_s)
pnl_s <- xts::xts(pnl_s, index(price_s[out_of_sample]))
plot(pnl_s[, 1], t="l")
plot.zoo(pnl_s)



## Simple contrarian strategy using Hampel filter - doesn't work too well

# Calculate a time series of rolling z-scores
win_dow <- 5
# price_s <- big_ticks$price
re_turns <- rutils::diff_it(big_ticks$price)
medi_an <- TTR::runMedian(re_turns, n=win_dow)
medi_an[1:win_dow] <- 1
# sum(is.na(medi_an))
ma_d <- TTR::runMAD(re_turns, n=win_dow)
ma_d[1:win_dow] <- 1
ma_d[ma_d < 1e-6] <- 1
# sum(is.na(ma_d))
z_scores <- ifelse(ma_d!=0, (re_turns-medi_an)/ma_d, 0)
z_scores[1:win_dow] <- 0
# sum(is.na(z_scores))
# ma_d <- zoo::na.locf(z_scores)
# mad_zscores <- TTR::runMAD(z_scores, n=win_dow)
# mad_zscores[1:win_dow, ] <- 0

tail(z_scores)
mad(z_scores)
range(z_scores)
x11(width=6, height=5)
hist(z_scores, breaks=200, xlim=c(-5, 5), freq=FALSE)

# Calculate position_s and pnls from z-scores
position_s <- rep(NA_integer_, NROW(re_turns))
position_s[1] <- 0
# thresh_old <- 3*mad(z_scores)
thresh_old <- 1
position_s <- ifelse(z_scores > thresh_old, -1, position_s)
position_s <- ifelse(z_scores < (-thresh_old), 1, position_s)
# position_s <- ifelse(z_scores > 2*mad_zscores, -1, position_s)
# position_s <- ifelse(z_scores < (-2*mad_zscores), 1, position_s)
position_s <- zoo::na.locf(position_s)
# Number of trades
# sum(abs(rutils::diff_it(position_s))) / NROW(position_s)
positions_lag <- rutils::lag_it(position_s, lagg=2)
pnl_s <- cumsum(positions_lag*re_turns)

plot(pnl_s, t="l")
# Number of trades
sum(abs(rutils::diff_it(position_s))) / NROW(position_s)
# Plot dygraph
date_s <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnl_s <- xts::xts(pnl_s, date_s)
dygraphs::dygraph(pnl_s)
# Combine index with AAPL
pnl_s <- cbind(pnl_s, big_ticks$price)
col_names <- c("Strategy", "AAPL")
colnames(pnl_s) <- col_names
dygraphs::dygraph(pnl_s, main="AAPL Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)




############### temp

in_dic <- rutils::diff_it(position_s)
indic_buy <- (in_dic > 0)
indic_sell <- (in_dic < 0)
cum_sum <- cumsum(re_turns)


pnl_s <- cbind(pnl_s, cum_sum[indic_buy], cum_sum[indic_sell])
colnames(pnl_s)[3:4] <- c("Buy", "Sell")

col_names <- colnames(pnl_s)
dygraphs::dygraph(pnl_s, main=cap_tion) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue") %>%
  dySeries(name=col_names[3], axis="y2", label=col_names[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
  dySeries(name=col_names[4], axis="y2", label=col_names[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")




############### homework
# Summary: Study how the dispersion of the Hampel z-scores 
# depends on the level of volatility in the interval.
# Yes, z_scores have higher dispersion on more volatile days.
# But so what?

oh_lc <- HighFreq::SPY["T09:31:00/T15:59:00"]
# oh_lc <- rutils::etf_env$VTI
n_rows <- NROW(oh_lc)
oh_lc <- log(oh_lc[, 1:4])
clos_e <- Cl(oh_lc)
# Calculate the z_scores
look_back <- 11
medi_an <- TTR::runMedian(clos_e, n=look_back)
medi_an[1:look_back, ] <- 1
z_scores <- (clos_e-medi_an)
z_scores[1:look_back, ] <- 0
mad_zscores <- TTR::runMAD(z_scores, n=10*look_back)
mad_zscores[1:(10*look_back), ] <- 0
z_scores <- ifelse(mad_zscores != 0, z_scores/mad_zscores, 0)

# Calculate the log variance for SPY
vari_ance <- xts::apply.daily(oh_lc, HighFreq::calc_var_ohlc)
# For VTI
# vari_ance <- sapply((2*look_back):n_rows, function(ro_w) {
#   HighFreq::calc_var_ohlc(oh_lc[(ro_w-look_back+1):ro_w, ], scal_e=FALSE)
# })  # end sapply
# vari_ance <- c(vari_ance[1]+numeric(2*look_back-1), vari_ance)
# x11(width=6, height=5)
# plot(vari_ance)
# Plot the VTI volatility
vo_l <- sqrt(vari_ance)
# vo_l <- xts::xts(sqrt(vari_ance), index(oh_lc))
dygraphs::dygraph(vo_l, main="VTI Volatility")


# Plot z_scores versus volatility for VTI
plot(as.numeric(z_scores) ~ as.numeric(vo_l))


# Calculate dates with high volatility
is_high <- (vo_l > max(vo_l)/10)
is_high <- is_high[is_high]
high_days <- index(is_high)
high_days <- as.Date(high_days)
# low_days <- high_days[!high_days]

date_s <- index(oh_lc)
date_s <- as.Date(date_s)

is_high <- date_s %in% high_days
high_scores <- z_scores[is_high]
low_scores <- z_scores[!is_high]

# Plot histogram of z_scores
range(low_scores)
low_scores <- low_scores[low_scores > quantile(low_scores, 0.05)]
low_scores <- low_scores[low_scores < quantile(low_scores, 0.95)]
x11(width=6, height=5)
hist(low_scores, xlim=c(quantile(low_scores, 0.05), quantile(low_scores, 0.95)), breaks=50, main=paste("low_scores", "look_back =", look_back))

range(high_scores)
high_scores <- high_scores[high_scores > quantile(high_scores, 0.05)]
high_scores <- high_scores[high_scores < quantile(high_scores, 0.95)]
x11(width=6, height=5)
hist(high_scores, xlim=c(quantile(high_scores, 0.05), quantile(high_scores, 0.95)), breaks=50, main=paste("high_scores", "look_back =", look_back))



###############
### PTS AAPL features PCA dimension reduction trading strategy

# Load packages
library(rutils)

# Load data with AAPL stock features from csv file
da_ta <- data.table::fread(file="C:/Develop/predictive/data/jerzy_aapl_20200720.csv", stringsAsFactors=FALSE)
re_turns <- da_ta$price_change_plus_5min
da_ta <- da_ta[, -"price_change_plus_5min"]
da_ta <- as.matrix(da_ta)
cor_vec <- drop(cor(re_turns, da_ta))
barplot(cor_vec, main="Correlations of Features to the AAPL Returns")
data_scaled <- scale(da_ta, center=TRUE, scale=TRUE)
sd_data <- apply(da_ta, MARGIN=2, sd)
mean_data <- apply(da_ta, MARGIN=2, mean)

# Calculate correlation matrix
cor_mat <- cor(data_scaled)
# Reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat,
                       order="hclust",
                       hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# Plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
x11()
corrplot(cor_mat, title="AAPL Features Correlation Matrix",
         tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
         method="square", col=col_ors(8),
         cl.offset=0.75, cl.cex=0.7,
         cl.align.text="l", cl.ratio=0.25)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2,
                method="complete", col="red")


# Perform PCA
pc_a <- prcomp(data_scaled, scale=FALSE)
# Plot barplots with PCA vectors weights in multiple panels
x11()
n_weights <- 6
par(mfrow=c(n_weights/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:n_weights) {
  barplot(pc_a$rotation[, or_der], las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0, col.main="red")
} # end for

# Inspect principal component time series
round(cor(pc_a$x), 4)
plot(pc_a$x[, 1], t="l")

# Calculate correlations of principal component time series and re_turns
returns_std <- (re_turns - mean(re_turns))/sd(re_turns)
s_d <- apply(pc_a$x, MARGIN=2, sd)
# pca_ts <- scale(pc_a$x, center=TRUE, scale=TRUE)
cor_vec <- cor(re_turns, pc_a$x)
# apply(returns_std*pca_ts, MARGIN=2, sum)/NROW(returns_std)
# Calculate weight_s equal to correlations
weight_s <- cor_vec/s_d
x11()
barplot(weight_s)

# Invert all the principal component time series
inv_rotation <- solve(pc_a$rotation)
weights_solved <- drop(weight_s %*% inv_rotation)
weights_solved <- weights_solved/sd_data
foo <- drop(da_ta %*% weights_solved)
cor(re_turns, foo)
barplot(weights_solved)
barplot(weights_solved, main="Weights of Features in New Feature")


# Simulate trading strategy
position_s <- rep(NA_integer_, NROW(re_turns))
position_s[1] <- 0
# Long positions
# indica_tor <- (da_ta[, feature4] + da_ta[, feature5])
indica_tor <- (da_ta[, feature4] + da_ta[, feature5])
position_s <- indica_tor
# position_s <- ifelse(indica_tor >= lagg, 1, position_s)
# Short positions
# indica_tor <- ((clos_e - v_wap) < (-thresh_old*rang_e))
# indica_tor <- HighFreq::roll_count(indica_tor)
# position_s <- ifelse(indica_tor >= lagg, -1, position_s)
# position_s <- zoo::na.locf(position_s, na.rm=FALSE)
# Lag the positions to trade in next period
position_s <- rutils::lag_it(position_s, lagg=1)
pnl_s <- cumsum(co_eff*re_turns*position_s)
plot(pnl_s[(1e3*(1:(NROW(re_turns) %/% 1e3)))], t="l")




###############
### Backtest rescaled range strategy

# Load packages
library(HighFreq)

# Calculate rolling rescaled cumulative returns from OHLC data
roll_range <- function(oh_lc, look_back=11) {
  re_turns <- rutils::diff_it(oh_lc[, 4])
  ran_ge <- HighFreq::roll_sum(re_turns, look_back=look_back)
  var_rolling <- sqrt(HighFreq::roll_var_ohlc(oh_lc, look_back=look_back, scal_e=FALSE))
  look_back <- sqrt(look_back)
  hurst_rolling <- ifelse((var_rolling==0) | (ran_ge==0),
                          0.0,
                          ran_ge/var_rolling/look_back)
  # Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(oh_lc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  # end roll_range


# Calculate rolling rescaled cumulative returns from returns data
roll_range <- function(re_turns, cum_returns, look_back=11) {
  ran_ge <- HighFreq::roll_sum(re_turns, look_back=look_back)
  var_rolling <- sqrt(HighFreq::roll_var(re_turns, look_back=look_back))
  look_back <- sqrt(look_back)
  hurst_rolling <- ifelse((var_rolling==0) | (ran_ge==0),
                          0.0,
                          ran_ge/var_rolling/look_back)
  # Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(oh_lc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  # end roll_range


re_turns <- rutils::diff_it(drop(zoo::coredata(quantmod::Cl(HighFreq::SPY))))
n_rows <- NROW(re_turns)
dim(re_turns) <- c(n_rows, 1)
# cum_returns <- cumsum(re_turns)



# Calculate rolling Hurst for SPY
hurst_rolling <- HighFreq::roll_hurst(oh_lc=HighFreq::SPY, look_back=7)
# Calculate rolling rescaled range
hurst_rolling <- roll_range(oh_lc=HighFreq::SPY, look_back=5)
hurst_rolling <- roll_range(re_turns=re_turns, look_back=5)
# chart_Series(hurst_rolling["2009-03-10/2009-03-12"], name="SPY hurst_rolling")

# thresh_old <- 0.5

x11(width=6, height=5)
quantile_s <- quantile(hurst_rolling, c(0.01, 0.99))
hist(hurst_rolling, xlim=quantile_s, breaks=1e2)


# Trade on rescaled range

quantile_s <- quantile(hurst_rolling, c(0.2, 0.8))
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
# Flip only if two consecutive signals in same direction
position_s <- ifelse((re_scaled > quantile_s[2]) & (rescaled_lag > quantile_s[2]), -1, position_s)
position_s <- ifelse((re_scaled < quantile_s[1]) & (rescaled_lag < quantile_s[1]), 1, position_s)
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s, 2)
pnl_s <- cumsum(re_turns*position_s)
x11(width=6, height=5)
plot(pnl_s, t="l")
# Number of trades
sum(abs(rutils::diff_it(position_s))) / NROW(position_s)



###############
### Backtest momentum strategy

# Load packages
library(rutils)

load("C:/Develop/lecture_slides/data/sp500_returns.RData")
returns_100[1, ] <- 0
returns_100 <- zoo::na.locf(returns_100, na.rm=FALSE)
in_dex <- cumsum(rowMeans(returns_100))

lagg <- 5
sum_roll <- rutils::roll_sum(returns_100, look_back=lagg)
weight_s <- matrixStats::rowRanks(sum_roll)
weight_s <- (weight_s - 50)
weight_s <- rutils::lag_it(weight_s, lagg=1)

weal_th <- -weight_s*returns_100
weal_th <- -weight_s*rutils::lag_it(sum_roll, lagg=(-lagg))
weal_th <- cumsum(rowMeans(weal_th))
x11()
plot(weal_th, t="l")
weal_th <- xts::xts(weal_th, index(returns_100))
dygraphs::dygraph(weal_th)

# Combine index with AAPL
weal_th <- cbind(weal_th, in_dex)
# weal_th <- xts(weal_th, index(returns_100))
col_names <- c("Strategy", "Index")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)




# Define backtest functional for daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(re_turns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- roll::roll_var(re_turns, width=look_back)
  vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
  # vari_ance[is.na(vari_ance)] <- 1
  vari_ance[vari_ance <= 0] <- 1
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(re_turns, width=look_back)
  pas_t[1:look_back, ] <- 1
  weight_s <- pas_t/sqrt(vari_ance)
  # weight_s <- ifelse(vari_ance > 0, pas_t/sqrt(vari_ance), 0)
  # weight_s[vari_ance == 0] <- 0
  weight_s[1:look_back, ] <- 1
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s, 2)
  # Calculate momentum profits and losses
  fu_ture <- rutils::lag_it(pas_t, (-look_back))
  pnl_s <- tre_nd*rowMeans(weight_s*fu_ture)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  cumsum(pnl_s - cost_s)
}  # end momentum_daily


weal_th <- momentum_daily(re_turns=returns_100, look_back=5, bid_offer=0, tre_nd=(-1))

# Combine index with AAPL
weal_th <- cbind(weal_th, in_dex)
weal_th <- xts(weal_th, index(returns_100))
col_names <- c("Strategy", "Index")
colnames(weal_th) <- col_names
dygraphs::dygraph(weal_th, main="Momentum S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)



###############
### Relationship between liquidity (turnover) and linear dependence (correlation).

# Do illiquid stocks with a lower dollar turnover have significant autocorrelation?

# The answer is that there's no significant relationship between liquidity (turnover) 
# and linear dependence (correlation).

# Load packages
library(rutils)

load("C:/Develop/lecture_slides/data/sp500.RData")

star_t <- "2000-01-01"
lagg <- 25
da_ta <- lapply(sp500_env, function(oh_lc) {
  if (start(oh_lc) < star_t) {
    price_s <- quantmod::Cl(oh_lc)
    vol_ume <- quantmod::Vo(oh_lc)
    re_turns <- rutils::diff_it(log(price_s))
    # Calculate autocorrelations from PACF
    p_acf <- pacf(na.omit(re_turns), plot=FALSE)
    c(turnover=sum(vol_ume*price_s), dependence=sum(p_acf$acf))
    # Calculate autocorrelations from Hurst
    # end_p <- rutils::calc_endpoints(oh_lc, lagg)
    # c(turnover=sum(vol_ume*price_s), dependence=calc_hurst_hilo(Hi(oh_lc), Lo(oh_lc), end_p))
  } else NULL
})  # end lapply

# Bind and sort
da_ta <- do.call(rbind, da_ta)
da_ta <- da_ta[order(da_ta[, "dependence"], decreasing=FALSE), ]

# Plot
x11()
hist(da_ta)
plot(da_ta)
chart_Series(quantmod::Cl(sp500_env$TYL))
dygraphs::dygraph(quantmod::Cl(sp500_env$TYL))



###############
### Test autoregressive strategy for all ETFs in rutils::etf_env

library(rutils)
or_der <- 3
look_back <- 5

back_test <- function(sym_bol) {
  price_s <- log(quantmod::Cl(get(sym_bol, rutils::etf_env)))
  re_turns <- rutils::diff_it(price_s)
  re_turns <- as.numeric(re_turns)
  n_rows <- NROW(re_turns)
  in_sample <- 1:(n_rows %/% 2)
  out_of_sample <- (n_rows %/% 2 + 1):n_rows
  predic_tor <- rutils::roll_sum(re_turns, look_back=look_back)
  res_ponse <- rutils::lag_it(predic_tor, lagg=(-look_back))
  predic_tor <- sapply(1+look_back*(0:or_der), rutils::lag_it, in_put=predic_tor)
  predic_tor <- cbind(rep(1, n_rows), predic_tor)
  colnames(predic_tor) <- paste0("pred_", 1:NCOL(predic_tor))
  in_verse <- MASS::ginv(predic_tor[in_sample, 1:or_der])
  coeff_fit <- drop(in_verse %*% res_ponse[in_sample])
  forecast_s <- drop(predic_tor[out_of_sample, 1:or_der] %*% coeff_fit)
  sign(forecast_s)*re_turns[out_of_sample]
}  # end back_test


test_s <- lapply(rutils::etf_env$sym_bols, back_test)
names(test_s) <- rutils::etf_env$sym_bols
sort(sapply(test_s, sum))

forecast_s <- test_s[[1]]
pnl_s <- xts(cumsum(sign(forecast_s)*re_turns[out_of_sample]), index(price_s[out_of_sample]))



###############
### Variance ratios

# Find stocks with largest variance ratios
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
sym_bols <- colnames(re_turns)
n_weights <- NROW(sym_bols)
lagg <- 25
vr_s <- sapply(re_turns, function(re_turn) {
  re_turn <- na.omit(re_turn)
  if (NROW(re_turn) > 500)
    calc_var(re_turn, lagg)/calc_var(re_turn)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)

# Find ETFs with largest variance ratios
re_turns <- rutils::etf_env$re_turns
sym_bols <- colnames(re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM", "IEF"))]
re_turns <- re_turns[, sym_bols]
vr_s <- sapply(re_turns, function(re_turn) {
  re_turn <- na.omit(re_turn)
  if (NROW(re_turn) > 100)
    calc_var(re_turn, lagg)/calc_var(re_turn)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)
# sym_bols <- names(vr_s)

# Find PCAs with largest variance ratios
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
pc_a <- prcomp(re_turns, scale=TRUE)
pca_rets <- xts(pc_a$x/100, order.by=index(re_turns))
vr_s <- sapply(pca_rets, function(re_turn) {
  re_turn <- na.omit(re_turn)
  if (NROW(re_turn) > 100)
    calc_var(re_turn, lagg)/calc_var(re_turn)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)
pca_rets <- pca_rets[, names(vr_s)]
save(pca_rets, file="C:/Develop/data/pca_rets.RData")
x11()
dygraphs::dygraph(cumsum(pca_rets[, "PC2"]))
barplot(sort(pc_a$rotation[, "PC2"]))

# Second PCA
cum_sum <- cumsum(pca_rets)
end_p <- rutils::calc_endpoints(pca_rets, inter_val=5)
cum_sum <- cum_sum[end_p, ]
cum_sum <- rutils::diff_it(cum_sum)
pc_a <- prcomp(cum_sum, scale=TRUE)
pca_rets <- xts(pc_a$x/100, order.by=index(cum_sum))
vr_s <- sapply(pca_rets, function(re_turn) {
  re_turn <- na.omit(re_turn)
  if (NROW(re_turn) > 100)
    calc_var(re_turn, lagg)/calc_var(re_turn)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)


## optim
object_ive <- function(weight_s, re_turns, lagg) {
  re_turns <- (re_turns %*% weight_s)
  -calc_var(re_turns, lagg)/calc_var(re_turns)/lagg
}  # end object_ive

sym_bols <- colnames(re_turns)
n_weights <- NROW(sym_bols)
op_tim <- optim(par=rep(1/n_weights, n_weights),
                fn=object_ive,
                re_turns=re_turns,
                lagg=lagg,
                method="L-BFGS-B",
                upper=rep(10, n_weights),
                lower=rep(-10, n_weights))
# Optimal parameters
weight_s <- op_tim$par
# weight_s <- weight_s*sd(rowMeans(rets_pca))/sd(rets_pca %*% weight_s)
names(weight_s) <- colnames(re_turns)
object_ive(weight_s, re_turns, lagg)
op_tim$value
pnl_s <- cumsum(re_turns %*% weight_s)
pnl_s <- xts::xts(pnl_s, zoo::index(re_turns))
dygraphs::dygraph(pnl_s)


# DEoptim
op_tim <- DEoptim::DEoptim(object_ive,
                           re_turns=re_turns,
                           lagg=lagg,
                           upper=rep(10, n_weights),
                           lower=rep(-10, n_weights),
                           control=list(trace=FALSE, itermax=500))

# Extract optimal parameters into weight_s vector
weight_s <- op_tim$optim$bestmem
# weight_s <- weight_s*sd(rowMeans(rets_pca))/sd(rets_pca %*% weight_s)
names(weight_s) <- colnames(re_turns)
object_ive(weight_s, re_turns, lagg)
pnl_s <- cumsum(re_turns %*% weight_s)
pnl_s <- xts::xts(pnl_s, zoo::index(re_turns))
dygraphs::dygraph(pnl_s)



###############
### Hurst stuff

lagg <- 25
oh_lc <- rutils::etf_env$VTI
end_p <- rutils::calc_endpoints(oh_lc, lagg)
hi_gh <- Hi(oh_lc)
lo_w <- Lo(oh_lc)
calc_hurst_hilo(hi_gh, lo_w, end_p)

# Find ETFs with largest Hurst
hurst_s <- sapply(rutils::etf_env$sym_bols, function(sym_bol) {
  oh_lc <- get(sym_bol, rutils::etf_env)
  end_p <- rutils::calc_endpoints(oh_lc, lagg)
  hi_gh <- Hi(oh_lc)
  lo_w <- Lo(oh_lc)
  calc_hurst_hilo(hi_gh, lo_w, end_p)
})  # end eapply
hurst_s <- sort(hurst_s, decreasing=TRUE)

plot(hurst_s, vr_s)
text(x=hurst_s, y=vr_s, labels=names(vr_s))


# Calculate Hurst from returns
end_p <- rutils::calc_endpoints(re_turns, lagg)
hurst_s <- sapply(re_turns, calc_hurst_rets, end_p)
hurst_s <- sort(hurst_s, decreasing=TRUE)

# Find stocks with largest Hurst
load("C:/Develop/lecture_slides/data/sp500.RData")
star_t <- "2000-01-01"
hurst_s <- eapply(sp500_env, function(oh_lc) {
  # oh_lc <- get(sym_bol, rutils::etf_env)
  # Check if data starts before 2000
  if (start(oh_lc) < star_t) {
    oh_lc <- oh_lc[paste0("/", star_t)]
    end_p <- rutils::calc_endpoints(oh_lc, lagg)
    hi_gh <- Hi(oh_lc)
    lo_w <- Lo(oh_lc)
    calc_hurst_hilo(hi_gh, lo_w, end_p)
  } else NULL
})  # end eapply
hurst_s <- sort(unlist(hurst_s), decreasing=TRUE)
sym_bols <- names(hurst_s[1:100])
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
re_turns <- re_turns[, sym_bols]
save(re_turns, file="C:/Develop/lecture_slides/data/sp100_rets_hurst.RData")



## Find portfolio with largest Hurst

# Vector of initial portfolio weights
weight_s <- rep(1/n_weights, n_weights)

# object_ive with shrinkage
object_ive <- function(weight_s, re_turns, end_p) {
  -calc_hurst_rets(re_turns %*% weight_s, end_p)
}  # end object_ive


# Portfolio optimization using principal components
# Perform PCA
pc_a <- prcomp(re_turns, center=TRUE, scale=TRUE)
# ei_gen <- eigen(cor(re_turns))
# all.equal(abs(pc_a$rotation), abs(ei_gen$vectors), check.attributes=FALSE)
# Calculate principal component time series
rets_pca <- scale(re_turns) %*% pc_a$rotation
# all.equal(pc_a$x, rets_pca, check.attributes=FALSE)
round(cor(rets_pca), 4)
# Calculate the re_turns from the principal component
# time series rets_pca:
rot_inv <- solve(pc_a$rotation)
sol_ved <- rets_pca %*% rot_inv

hurst_pca <- apply(rets_pca, 2, calc_hurst_rets, end_p=end_p)
sort(hurst_pca, decreasing=TRUE)

op_tim <- optim(par=rep(1/n_weights, n_weights),
                fn=object_ive,
                re_turns=rets_pca,
                end_p=end_p,
                method="L-BFGS-B",
                upper=rep(10, n_weights),
                lower=rep(-10, n_weights))
# Optimal parameters
weight_s <- op_tim$par
weight_s <- 0.01*weight_s/sd(rets_pca %*% weight_s)
# weight_s <- weight_s*sd(rowMeans(rets_pca))/sd(rets_pca %*% weight_s)
names(weight_s) <- colnames(rets_pca)
object_ive(weight_s, rets_pca, end_p)
op_tim$value


# Find portfolio with largest Hurst
object_ive <- function(weight_s, re_turns, end_p) {
  -calc_hurst_rets(re_turns %*% weight_s, end_p)
  + (sum(weight_s^2) - 1)^2
}  # end object_ive
# library(parallel)
# num_cores <- detectCores()
# clus_ter <- makeCluster(num_cores-1)
# clusterExport(clus_ter, varlist=c("calc_hurst_rets"))
op_tim <- DEoptim::DEoptim(object_ive,
                           re_turns=rets_pca,
                           end_p=end_p,
                           upper=rep(10, n_weights),
                           lower=rep(-10, n_weights),
                           # cluster=clus_ter,
                           # parVar=c("calc_hurst_rets"),
                           control=list(trace=FALSE, itermax=500, parVar=c("calc_hurst_rets"), parallelType=1))
# Stop R processes over cluster under Windows
# stopCluster(clus_ter)

# Extract optimal parameters into weight_s vector
weight_s <- op_tim$optim$bestmem
weight_s <- 0.01*weight_s/sd(rets_pca %*% weight_s)
# weight_s <- weight_s*sd(rowMeans(rets_pca))/sd(rets_pca %*% weight_s)
names(weight_s) <- colnames(rets_pca)
object_ive(weight_s, rets_pca, end_p)


# Plot wealth
portf_hurst <- drop(rets_pca %*% weight_s)
calc_hurst_rets(portf_hurst, end_p)
portf_hurst <- xts::xts(portf_hurst, index(re_turns))
colnames(portf_hurst) <- "max_hurst_deopt"
weal_th <- cumsum(portf_hurst)
# weal_th <- cumprod(1 + portf_hurst)
da_ta <- cbind(Cl(rutils::etf_env$VEU)[index(re_turns)], weal_th)
colnames(da_ta)[1] <- "VEU"
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")



## Calculate autocorrelations
re_turns <- rutils::etf_env$re_turns
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
returns_lag <- rutils::lag_it(re_turns)
auto_cor <- sapply(colnames(re_turns), function(sym_bol) {
  re_turns <- re_turns[, sym_bol]
  mean(re_turns*returns_lag[, sym_bol])/var(re_turns)
})  # end sapply
auto_cor <- sort(auto_cor, decreasing=TRUE)


## Find portfolio with largest autocorrelations

# Vector of initial portfolio weights
re_turns <- rutils::etf_env$re_turns
sym_bols <- colnames(re_turns)
sym_bols <- sym_bols[!(sym_bols %in% c("VXX", "SVXY", "MTUM", "IEF"))]
re_turns <- re_turns[, sym_bols]
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- zoo::na.locf(re_turns, na.rm=FALSE)
returns_lag <- rutils::lag_it(re_turns)

n_weights <- NCOL(re_turns)
weight_s <- rep(1/n_weights, n_weights)

# object_ive with shrinkage
object_ive <- function(weight_s, re_turns, returns_lag) {
  re_turns <- re_turns %*% weight_s
  -drop(mean(re_turns*(returns_lag %*% weight_s))/var(re_turns))
}  # end object_ive
object_ive(weight_s, re_turns, returns_lag)


op_tim <- optim(par=rep(1/n_weights, n_weights),
                fn=object_ive,
                re_turns=re_turns,
                returns_lag=returns_lag,
                method="L-BFGS-B",
                upper=rep(10, n_weights),
                lower=rep(-10, n_weights))
# Optimal parameters
weight_s <- op_tim$par
names(weight_s) <- colnames(re_turns)
weight_s <- sort(weight_s, decreasing=TRUE)
object_ive(weight_s, re_turns, returns_lag)
op_tim$value
# wippp
pnl_s <- xts::xts(cumsum(re_turns %*% weight_s), zoo::index(re_turns))
dygraphs::dygraph(pnl_s)

# DEoptim
op_tim <- DEoptim::DEoptim(object_ive,
                           re_turns=re_turns,
                           returns_lag=returns_lag,
                           upper=rep(10, n_weights),
                           lower=rep(-10, n_weights),
                           # cluster=clus_ter,
                           # parVar=c("calc_hurst_rets"),
                           control=list(trace=FALSE, itermax=500, parallelType=1))
# Extract optimal parameters into weight_s vector
weight_s <- op_tim$optim$bestmem
weight_s <- 0.01*weight_s/sd(rets_pca %*% weight_s)
# weight_s <- weight_s*sd(rowMeans(rets_pca))/sd(rets_pca %*% weight_s)
names(weight_s) <- colnames(re_turns)
sort(weight_s, decreasing=TRUE)
object_ive(weight_s, re_turns, returns_lag)
pnl_s <- xts::xts(cumsum(re_turns %*% weight_s), zoo::index(re_turns))
dygraphs::dygraph(pnl_s)


# Plot wealth
portf_hurst <- drop(rets_pca %*% weight_s)
calc_hurst_rets(portf_hurst, end_p)
portf_hurst <- xts::xts(portf_hurst, index(re_turns))
colnames(portf_hurst) <- "max_hurst_deopt"
weal_th <- cumsum(portf_hurst)
# weal_th <- cumprod(1 + portf_hurst)
da_ta <- cbind(Cl(rutils::etf_env$VEU)[index(re_turns)], weal_th)
colnames(da_ta)[1] <- "VEU"
col_names <- colnames(da_ta)
dygraphs::dygraph(da_ta, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="blue") %>%
  dySeries(name=col_names[2], axis="y2", col="red")




# More stuff below

da_ta <- exp(cumsum(rnorm(1e7)/100))
date_s <- seq.POSIXt(from=Sys.time(), by="sec", length.out=NROW(da_ta))
da_ta <- xts(da_ta, date_s)

interval_s <- seq.int(from=1e2, to=1e3, by=1e2)
vol_s <- sapply(interval_s, function(inter_val) {
  # spy_agg <- rutils::to_period(oh_lc=da_ta, k=inter_val)
  # HighFreq::calc_var_ohlc(spy_agg)
  end_p <- rutils::calc_endpoints(da_ta, inter_val=inter_val)
  sd(rutils::diff_it(log(da_ta[end_p])))
})  # end sapply

interval_s <- c("seconds", "minutes", "hours", "days")
inter_log <- log(c(1, 60, 3600, 6.5*3600))
inter_log <- log(c(1, 60, 3600, 24*3600))
vol_s <- sapply(interval_s, function(inter_val) {
  spy_agg <- rutils::to_period(oh_lc=HighFreq::SPY, k=inter_val)
  # spy_agg <- rutils::to_period(oh_lc=HighFreq::SPY, period=inter_val)
  # HighFreq::calc_var_ohlc(spy_agg)
  re_turns <- rutils::diff_it(spy_agg[, 4])
  sd(re_turns)
})  # end sapply


names(vol_s) <- paste0("agg_", interval_s)
vol_log <- log(vol_s)
inter_log <- log(interval_s)
inter_log <- inter_log - mean(inter_log)
vol_log <- vol_log - mean(vol_log)
mod_el <- lm(vol_log ~ inter_log)
hurst_lm <- summary(mod_el)$coeff[2, 1]
hurs_t <- sum(vol_log*inter_log)/sum(inter_log^2)
all.equal(hurst_lm, hurs_t)



###############
### Backtests

# Define backtest functional
backtest_rolling <- function(re_turns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- HighFreq::roll_var(returns_weighted, look_back=look_back)
  vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
  vari_ance[is.na(vari_ance)] <- 0
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(re_turns, width=look_back)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s[vari_ance == 0] <- 0
  weight_s[1:look_back, ] <- 1
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- tre_nd*rowMeans(weight_s*re_turns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  cumprod(1 + pnl_s - cost_s)
}  # end backtest_rolling


# Define backtest functional
backtest_weighted <- function(re_turns, returns_weighted,
                              look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  vari_ance <- HighFreq::roll_var(returns_weighted, look_back=look_back)
  vari_ance <- zoo::na.locf(vari_ance, na.rm=FALSE)
  vari_ance[is.na(vari_ance)] <- 0
  # Calculate rolling Sharpe
  pas_t <- roll::roll_mean(returns_weighted, width=look_back)
  weight_s <- pas_t/sqrt(vari_ance)
  weight_s[vari_ance == 0] <- 0
  weight_s[1:look_back, ] <- 1
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0
  weight_s <- rutils::lag_it(weight_s)
  # Calculate momentum profits and losses
  pnl_s <- tre_nd*rowMeans(weight_s*re_turns)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*rowSums(abs(rutils::diff_it(weight_s)))
  cumprod(1 + pnl_s - cost_s)
}  # end backtest_weighted


# Define backtest functional
backtest_momentum <- function(re_turns, returns_weighted,
                              perform_ance=function(re_turns) (sum(re_turns)/sd(re_turns)),
                              look_back=12, re_balance="months", bid_offer=0.001,
                              end_p=rutils::calc_endpoints(re_turns, inter_val=re_balance),
                              with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  n_rows <- NROW(end_p)
  start_p <- c(rep_len(1, look_back-1), end_p[1:(n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(start_p, end_p)
  # Calculate look-forward intervals
  look_fwds <- cbind(end_p + 1, rutils::lag_it(end_p, -1))
  look_fwds[n_rows, 1] <- end_p[n_rows]
  # Calculate past performance over look-back intervals
  pas_t <- t(apply(look_backs, 1, function(ep) sapply(returns_weighted[ep[1]:ep[2]], perform_ance)))
  pas_t[is.na(pas_t)] <- 0
  # Calculate future performance
  fu_ture <- t(apply(look_fwds, 1, function(ep) sapply(re_turns[ep[1]:ep[2]], sum)))
  fu_ture[is.na(fu_ture)] <- 0
  # Scale weight_s so sum of squares is equal to 1
  weight_s <- pas_t
  weight_s <- weight_s/sqrt(rowSums(weight_s^2))
  weight_s[is.na(weight_s)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnl_s <- rowSums(weight_s*fu_ture)
  # Calculate transaction costs
  cost_s <- 0.5*bid_offer*cumprod(1 + pnl_s)*rowSums(abs(rutils::diff_it(weight_s)))
  pnl_s <- (pnl_s - cost_s)
  if (with_weights)
    rutils::lag_it(cbind(pnl_s, weight_s))
  else
    rutils::lag_it(pnl_s)
}  # end backtest_momentum


# Perform sapply loop over look_backs
end_p <- rutils::calc_endpoints(re_turns, inter_val="weeks")
look_backs <- seq(3, 15, by=1)
perform_ance <- function(re_turns) sum(re_turns)/sd(re_turns)
pro_files <- sapply(look_backs, function(look_back) {
  pnl_s <- backtest_momentum(re_turns=re_turns, returns_weighted=returns_weighted,
                             end_p=end_p,
                             look_back=look_back, perform_ance=perform_ance)
  last(cumprod(1 + pnl_s))
})  # end sapply
x11(width=6, height=4)
plot(x=look_backs, y=pro_files, t="l",
     main="Strategy PnL as function of look_back",
     xlab="look_back (months)", ylab="pnl")

date_s <- index(re_turns[end_p])
weights_aw <- c(0.30, 0.55, 0.15)
ret_aw <- re_turns %*% weights_aw
wealth_aw <- cumprod(1 + ret_aw)
wealth_aw <- xts::xts(wealth_aw[end_p], date_s)

look_back <- look_backs[which.max(pro_files)]
pnl_s <- backtest_momentum(re_turns=re_turns, returns_weighted=returns_weighted,
                           look_back=look_back, end_p=end_p,
                           perform_ance=perform_ance, with_weights=TRUE)
tail(pnl_s)
ret_mom <- as.numeric(pnl_s[, 1])
weal_th <- cumprod(1 + ret_mom)

da_ta <- cbind(weal_th, wealth_aw)
colnames(da_ta) <- c("Momentum Strategy", "All_weather")

dygraphs::dygraph(da_ta, main="Momentum Strategy") %>%
  dyAxis("y", label="All_weather", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="All_weather", axis="y", label="All_weather", strokeWidth=2, col="blue")


# Plot multiple wealth curves
# Perform sapply loop over look_backs
weal_th <- sapply(look_backs, backtest_momentum,
                  re_turns=re_turns,
                  returns_weighted=returns_weighted,
                  end_p=end_p,
                  perform_ance=perform_ance)
weal_th <- apply(weal_th, 2, function(x) cumprod(1 + x))
colnames(weal_th) <- paste0("look_back=", look_backs)
weal_th <- xts(weal_th, date_s)
tail(weal_th)

plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(weal_th))
chart_Series(weal_th,
             theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(weal_th),
       inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(re_turns)),
       col=plot_theme$col$line.col, bty="n")



###############
### VXX and SVXY

# re_turns <- -etf_env$re_turns$VXX
re_turns <- na.omit(etf_env$re_turns$SVXY)
re_turns <- cbind(re_turns, -na.omit(etf_env$re_turns$VXX))
which_na <- which(is.na(re_turns$VXX))
re_turns$VXX[which_na] <- re_turns$SVXY[which_na]
re_turns <- cumprod(1+rowMeans(re_turns))
sum(is.na(re_turns))
head(re_turns)
vol_ume <- cbind(quantmod::Vo(etf_env$SVXY), quantmod::Vo(etf_env$VXX))
vol_ume$VXX.Volume[which_na] <- vol_ume$SVXY.Volume[which_na]
vol_ume <- rowMeans(vol_ume)

roll_vwap <- rutils::roll_sum(x_ts=re_turns*vol_ume, look_back=look_back)
volume_rolling <- rutils::roll_sum(x_ts=vol_ume, look_back=look_back)
roll_vwap <- roll_vwap/volume_rolling
roll_vwap[is.na(roll_vwap)] <- 0
roll_vwap

plot(re_turns, t="l", lwd=2)
lines(roll_vwap, col="red", lwd=2)



###############
### Wilcoxon tests

# Data
sample1 <- rnorm(200)
sample2 <- rnorm(100, mean=0.1)
# Mann-Whitney-Wilcoxon rank sum test
wil_cox <- wilcox.test(sample1, sample2, paired=FALSE)
wil_cox$statistic
# Calculate U statistic of Mann-Whitney-Wilcoxon test
da_ta <- c(sample1, sample2)
rank_s <- rank(da_ta)
sum(rank_s[1:200]) - 100*201
sum(rank_s[201:300]) - 50*101

# Data
sample1 <- rnorm(100)
sample2 <- rnorm(100, mean=0.1)
# Wilcoxon signed rank test
wil_cox <- wilcox.test(sample1, sample2, paired=TRUE)
wil_cox$statistic
# Calculate V statistic of Wilcoxon test
da_ta <- (sample1 - sample2)
sum(rank(abs(da_ta))[da_ta>0])


###############
### C:/Develop/predictive/data

library(rutils)
da_ta <- read.zoo(file="C:/Develop/predictive/data/predictions_long_account.csv", header=TRUE, sep=",")
da_ta <- as.xts(da_ta)
date_s <- index(da_ta)
col_names <- colnames(da_ta)
da_ta <- lapply(da_ta, as.numeric)
da_ta <- rutils::do_call(cbind, da_ta)
da_ta <- xts(da_ta, date_s)
colnames(da_ta) <- col_names

core_data <- da_ta[, 8:9]
colnames(core_data) <- c("actual", "predicted")
core_data <- core_data["2020-01-10/"]
core_data <- na.omit(core_data)
date_s <- index(core_data)
date_s <- as.Date(date_s)
date_s <- unique(date_s)
date_s <- as.Date(date_s)
foo <- sapply(date_s, function(dat_e) {
  foo <- core_data[(date_s == dat_e), ]
  sum(foo[foo[, 2] > 0.15, 1])
})  # end sapply
names(foo) <- date_s


tail(core_data, 33)
dim(core_data)
head(core_data, 33)
core_data <- coredata(core_data)
core_data <- cbind(core_data[, 2, drop=FALSE], core_data[, 1, drop=FALSE])
plot(core_data)

ma_x <- max(core_data[, 1])
sapply((1:8)*ma_x/10, function(x) {
  sum(core_data[core_data[, 1]>x, 2])
})  # end sapply
sum(core_data[core_data[, 1]>0.15, 2])


foo <- da_ta["2020-01-10/"]
foo <- na.omit(foo)
sum(foo[, 8])
sum(foo[foo[, 9]>0.15, 8])




###############
### Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/test.cpp")


in_dex <- rutils::etf_env$re_turns[, "VTI", drop=FALSE]
in_dex <- na.omit(in_dex)
date_s <- index(in_dex)

re_turns <- rutils::etf_env$re_turns[, "XLF", drop=FALSE]
re_turns <- na.omit(re_turns)
re_turns <- re_turns[date_s]
calc_alpha(re_turns, in_dex, typ_e="wilcoxon")

sym_bols <- colnames(rutils::etf_env$re_turns)
sym_bols <- sym_bols[-grep("VTI", sym_bols, ignore.case=TRUE)]

foo <- sapply(sym_bols, function(sym_bol) {
  cat(sym_bol, "\n")
  re_turns <- rutils::etf_env$re_turns[, sym_bol, drop=FALSE]
  re_turns <- na.omit(re_turns)
  re_turns <- re_turns[date_s]
  date_s <- date_s[index(re_turns)]
  calc_alpha(re_turns, date_s, typ_e="wilcoxon")
})  # end sapply




###############
### Scale minutely returns by the volume to make them closer to normal or stationary.
# Use trading time (volume clock)

library(HighFreq)
oh_lc <- HighFreq::SPY
price_s <- drop(coredata(quantmod::Cl(oh_lc)))
vol_ume <- drop(coredata(quantmod::Vo(oh_lc)))
re_turns <- rutils::diff_it(log(price_s))
re_turns <- re_turns/sd(re_turns)
price_s <- cumsum(re_turns)

# Scale the volume by the rolling average volume
look_back <- 111
volume_rolling <- rutils::roll_sum(x_ts=vol_ume, look_back=look_back)
vol_ume <- look_back*vol_ume/volume_rolling

## wippp
## Add to homeworks
## Statistics for different volume scaling exponents.
# Dividing by the square root of the volume works better
# than dividing by the volume itself.
statis_tic <- sapply((1:20)/20, function(ex_po) {
  rets_scaled <- ifelse(vol_ume > 0, re_turns/(vol_ume^ex_po), 0)
  rets_scaled <- rets_scaled/sd(rets_scaled)
  # Calculate moments and perform JB normality tests
  # tseries::jarque.bera.test(rets_scaled)$statistic
  # moments::moment(rets_scaled, order=4)
  # Calculate autocorrelations from PACF
  pa_cf <- pacf(as.numeric(rets_scaled), lag=10, plot=FALSE)
  sum(pa_cf$acf)
  # Standard deviation of square returns is proxy for kurtosis and stationarity
  # sd(rets_scaled^2)
  # calc_hurst(rets_scaled, end_p)
})  # end sapply
x11(width=6, height=5)
plot(statis_tic, t="l")


# Divide returns by the volume (volume clock).
rets_scaled <- ifelse(vol_ume > 0, re_turns/vol_ume, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
ma_d <- mad(rets_scaled)

# Plot densities of the returns
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(density(re_turns), xlim=5*c(-ma_d, ma_d), 
     lwd=3, mgp=c(2, 1, 0), col="blue", 
     xlab="returns (standardized)", ylab="frequency", 
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(rets_scaled, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
       leg=c("minutely", "scaled", "normal"),
       lwd=6, lty=1, col=c("blue", "red", "green"))

# Plot the cumulative scaled returns
prices_scaled <- cumsum(rets_scaled)
prices_scaled <- xts(prices_scaled, index(oh_lc))
dygraphs::dygraph(prices_scaled[60*(1:(NROW(prices_scaled) %/% 60))], main="SPY Prices")


# Plot the cumulative scaled returns with close prices
da_ta <- cbind(price_s, prices_scaled)
col_names <- c("SPY Prices", "Scaled by Volume")
# da_ta <- xts::to.hourly(da_ta)
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta[60*(1:(NROW(da_ta) %/% 60))], main="SPY Prices") %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=col_names[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)


## Calculate Hurst

# Define a single aggregation interval.
inter_val <- 35^2
inter_log <- log(inter_val)
# Calculate index of end points spaced apart by inter_val.
n_rows <- NROW(price_s)
num_agg <- n_rows %/% inter_val
end_p <- c(0, n_rows - num_agg*inter_val + (0:num_agg)*inter_val)

# Calculate Hurst from single data point
calc_hurst <- function(re_turns, end_p) {
  price_s <- cumsum(re_turns)
  r_s <- sapply(2:NROW(end_p), function(ep) {
    in_dex <- end_p[ep-1]:end_p[ep]
    diff(range(price_s[in_dex]))/sd(re_turns[in_dex])
  })  # end sapply
  log(mean(r_s))/inter_log
}  # end calc_hurst

calc_hurst(re_turns, end_p)



###############
### Scale minutely returns by the price range to make them closer to normal or stationary

library(HighFreq)

# sym_bol <- "SPY"
oh_lc <- HighFreq::SPY

## Calculate price range
rang_e <- log(drop(coredata(Hi(oh_lc)/Lo(oh_lc))))
# Around 1.8% of bars have zero range
sum(rang_e==0)/NROW(rang_e)
# Remove bars with zero range
# oh_lc <- oh_lc[!(rang_e==0)]
# Remove bars with zero returns
re_turns <- rutils::diff_it(log(drop(coredata(Cl(oh_lc)))))
# zero_rets <- (re_turns==0)
# oh_lc <- oh_lc[!zero_rets]

date_s <- index(oh_lc)
n_rows <- NROW(oh_lc)
end_p <- xts::endpoints(oh_lc, on="hours")
clos_e <- Cl(oh_lc)[end_p]
rang_e <- log(drop(coredata(Hi(oh_lc)/Lo(oh_lc))))
rang_e <- (rang_e + c(0, rang_e[-NROW(rang_e)]))/2
re_turns <- rutils::diff_it(log(drop(coredata(Cl(oh_lc)))))
# re_turns <- c(0, re_turns)
sum(is.na(re_turns))
sum(is.infinite(re_turns))

## Distribution of raw returns is bimodal
x11()
# Standardize raw returns to make later comparisons
re_turns <- re_turns/sd(re_turns)
ma_d <- mad(re_turns)
range(re_turns)
# Standard deviation of square returns is proxy for kurtosis and stationarity
sd((re_turns/sd(re_turns))^2)
hist(re_turns, breaks=1111, xlim=c(-3, 3), freq=FALSE)
lines(density(re_turns, bw=0.2), col='red', lwd=2)

## Calculate moments and perform JB normality tests
sapply(1:4, moments::moment, x=re_turns)
tseries::jarque.bera.test(re_turns)
shapiro.test(re_turns)


# Regress volume on range
da_ta <- cbind(volume=log(vol_ume), range=log(rang_e))
# da_ta[!is.finite(da_ta)] <- NA
da_ta <- na.omit(is.finite(da_ta)*da_ta)
# foo <- lm(paste(colnames(da_ta), collapse=" ~ "), data=as.data.frame(da_ta))
foo <- lm(volume ~ range, as.data.frame(da_ta))
plot(volume ~ range, as.data.frame(da_ta[sampl_e, ]))
abline(foo, lwd=3, col="red")


## Apply Manly transformation to make returns more normal
lamb_da <- 1.5

# Modulus
# Divide returns by square root of volume (volume clock)


## Calculate moments and perform JB normality test
sum(is.na(rets_scaled))
sum(is.infinite(rets_scaled))
range(rets_scaled)
sapply(1:4, moments::moment, x=rets_scaled)
tseries::jarque.bera.test(rets_scaled)
x11()
ma_d <- mad(rets_scaled)
hist(rets_scaled, breaks=11111, xlim=10*c(-ma_d, ma_d), freq=FALSE)
pacf(rets_scaled)


## Scale returns using price range
rets_scaled <- ifelse(rang_e>0, re_turns/rang_e, 0)
# rets_scaled <- re_turns/rang_e
rets_scaled <- rets_scaled/sd(rets_scaled)
ma_d <- mad(rets_scaled)
hist(rets_scaled, breaks=1111, xlim=c(-3, 3), freq=FALSE)
lines(density(rets_scaled, bw=0.2), col='blue', lwd=2)

# JB statistic for different range scaling exponents
statis_tic <- sapply((1:6)/2, function(ex_po) {
  rets_scaled <- ifelse(rang_e>0, re_turns/(rang_e^ex_po), 0)
  rets_scaled <- rets_scaled/sd(rets_scaled)
  tseries::jarque.bera.test(rets_scaled)$statistic
})  # end sapply


# Stationarity statistic for different scaling exponents
statis_tic <- sapply((1:20)/10, function(ex_po) {
  # rets_scaled <- ifelse(vol_ume>0, re_turns/(vol_ume^x), 0)
  rets_scaled <- ifelse(rang_e>0, re_turns/(rang_e^ex_po), 0)
  # rets_scaled <- re_turns/(rang_e^ex_po)
  # Remove zero returns
  # rets_scaled <- rets_scaled[!(rets_scaled==0)]
  # rets_scaled <- rets_scaled/sd(rets_scaled)
  # Standard deviation of square returns is proxy for kurtosis and stationarity
  sd((rets_scaled/sd(rets_scaled))^2)
})  # end sapply



###############
### Fractional differencing




############### homework
# Summary: Strategy using weekly and monthly stock returns.
# It's implemented in app_roll_portf9.R

library(HighFreq)

# Aggregate the VTI returns to monthly and run pacf()

clos_e <- Cl(rutils::etf_env$VTI)
re_turns <- rutils::diff_it(log(clos_e))
returns_adv <- rutils::lag_it(re_turns, lagg=(-1))
returns_adv <- as.numeric(returns_adv)

# end_p <- rutils::calc_endpoints(clos_e, inter_val="weeks")
# week_ly <- clos_e[end_p]
# week_ly <- rutils::diff_it(log(week_ly))
week_ly <- rutils::diff_it(log(clos_e), lagg=5)
week_ly <- as.numeric(week_ly)
returns_adv <- rutils::lag_it(week_ly, lagg=(-1))
returns_adv <- as.numeric(returns_adv)
# end_p <- rutils::calc_endpoints(clos_e, inter_val="months")
# month_ly <- clos_e[end_p]
# month_ly <- rutils::diff_it(log(month_ly))
month_ly <- rutils::diff_it(log(clos_e), lagg=25)
month_ly <- as.numeric(month_ly)
returns_adv <- rutils::lag_it(month_ly, lagg=(-1))
returns_adv <- as.numeric(returns_adv)

# Objective function for simple optimization
object_ive <- function(wei_ght) {
  # weight_s <- c(wei_ght, 1-wei_ght)
  sum((returns_adv - (wei_ght*week_ly + (1-wei_ght)*month_ly))^2)
}  # end object_ive
object_ive(0.5)
foo <- optimize(f=object_ive, interval=c(-10, 10))
unlist(foo)
wei_ght <- unlist(foo)[1]
position_s <- sign(wei_ght*week_ly + (1-wei_ght)*month_ly)
positions_lag <- rutils::lag_it(position_s, lagg=2)
pnl_s <- cumsum(positions_lag*re_turns)
x11()
end_days <- rutils::calc_endpoints(re_turns, "days")
plot.zoo(-pnl_s[end_days], main="pnl_s", xlab=NA, ylab=NA)




############### homework
# Summary: Create a contrarian strategy using
# the Hampel filter.
# It's implemented in app_roll_portf10.R

library(rutils)


# 1. (10pts)
# Define a rolling look-back window and a half window:
# Use the %/% operator.

win_dow <- 11
half_window <- win_dow %/% 2

# Calculate a time series of rolling z-scores
# (called z_scores), using the Hampel filter code
# from the lecture slides.

price_s <- Cl(HighFreq::SPY)["T09:31:00/T15:59:00"]
re_turns <- rutils::diff_it(log(price_s))

medi_an <- TTR::runMedian(price_s, n=win_dow)
medi_an[1:win_dow, ] <- 1
sum(is.na(medi_an))
ma_d <- TTR::runMAD(price_s, n=win_dow)
ma_d[1:win_dow, ] <- 1
sum(is.na(ma_d))
z_scores <- ifelse(ma_d!=0, (price_s-medi_an)/ma_d, 0)
z_scores[1:win_dow, ] <- 0
ma_d <- zoo::na.locf(z_scores)
sum(is.na(z_scores))
mad_zscores <- TTR::runMAD(z_scores, n=win_dow)
mad_zscores[1:win_dow, ] <- 0

# You should get the following output:
tail(z_scores)
mad(z_scores)
range(z_scores)
hist(z_scores, breaks=30, xlim=c(-10, 10), freq=FALSE)

# Calculate position_s and pnls from z-scores
position_s <- rep(NA_integer_, NROW(price_s))
position_s[1] <- 0
# thresh_old <- 3*mad(z_scores)
# position_s <- ifelse(z_scores > thresh_old, -1, position_s)
# position_s <- ifelse(z_scores < (-thresh_old), 1, position_s)
position_s <- ifelse(z_scores > 2*mad_zscores, -1, position_s)
position_s <- ifelse(z_scores < (-2*mad_zscores), 1, position_s)
position_s <- zoo::na.locf(position_s)
positions_lag <- rutils::lag_it(position_s, lagg=2)
pnl_s <- cumsum(positions_lag*re_turns)
x11()
end_days <- rutils::calc_endpoints(price_s, "days")
plot.zoo(pnl_s[end_days], main="pnl_s", xlab=NA, ylab=NA)



############### homework - Hurst exponents almost random
# Summary: Calculate a time series of monthly Hurst
# exponents and the volatility for the SPY series.
# Demonstrate that the changes of the Hurst exponent
# have negative autocorrelations, that Hurst exponent
# is anti-persistent over time.
# Calculate the correlation between the SPY Hurst
# exponent and the level of volatility.
#
# Regress the Hurst exponent versus the standard
# deviation, and create plots and perform a regression
# of the two.

# Observation:
# The Hurst exponent is low when the volatility is high.
# When the volatility is low then the Hurst exponent
# can be both high and low.


library(HighFreq)

# This is best version
# Calculate Hurst exponent using median of range ratios
calc_hurst_hilo <- function(hi_gh, lo_w, end_p) {
  range_ratios <- sapply(seq_along(end_p)[-1], function(it) {
    start_point <- end_p[it-1]
    end_point <- end_p[it]
    hi_gh <- hi_gh[start_point:end_point]
    lo_w <- lo_w[start_point:end_point]
    log((max(hi_gh) - min(lo_w))/mean(hi_gh - lo_w))/log(end_point-start_point)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_hilo

# Calculate Hurst exponent using median of range ratios
calc_hursto <- function(hi_gh, lo_w, end_p) {
  range_ratios <- sapply(seq_along(end_p)[-1], function(it) {
    hi_gh <- hi_gh[end_p[it-1]:end_p[it]]
    lo_w <- lo_w[end_p[it-1]:end_p[it]]
    (max(hi_gh) - min(lo_w))/mean(hi_gh - lo_w)
  })  # end sapply
  log(median(na.omit(range_ratios)))/log(median(rutils::diff_it(end_p)))
}  # end calc_hursto

# Calculate Hurst exponent from returns
calc_hurst_rets <- function(rets, end_p) {
  cum_sum <- cumsum(rets)
  range_ratios <- sapply(seq_along(end_p)[-1], function(it) {
    start_point <- end_p[it-1]
    end_point <- end_p[it]
    rets <- rets[start_point:end_point]
    cum_sum <- cum_sum[start_point:end_point]
    log((max(cum_sum) - min(cum_sum))/sd(rets))/log(end_point-start_point)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_rets


oh_lc <- log(HighFreq::SPY)
hi_gh <- quantmod::Hi(oh_lc)
lo_w <- quantmod::Lo(oh_lc)
calc_hurst(hi_gh, lo_w, rutils::calc_endpoints(oh_lc, inter_val="days"))

library(microbenchmark)
summary(microbenchmark(
  calc_hurst=calc_hurst(hi_gh, lo_w, rutils::calc_endpoints(oh_lc, inter_val="days")),
  calc_hursto=calc_hursto(hi_gh, lo_w, rutils::calc_endpoints(oh_lc, inter_val="days")),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



# 2. (20pts)
# Calculate a vector of monthly end points from
# the oh_lc, and call it end_p.
# use the function rutils::calc_endpoints().

end_p <- rutils::calc_endpoints(oh_lc, inter_val="months")

# Perform an sapply() loop over the length of end_p.
# Inside the loop calculate the standard deviation of
# returns and the cumulative trading volumes.
# The output should be a matrix called volat_hurst.

volat_hurst <- sapply(seq_along(end_p)[-1],
  function(it) {
    oh_lc <- oh_lc[end_p[it-1]:end_p[it]]
    hi_gh <- quantmod::Hi(oh_lc)
    lo_w <- quantmod::Lo(oh_lc)
    c(volatility=HighFreq::calc_var_ohlc(oh_lc),
      hurst=calc_hurst(hi_gh, lo_w, rutils::calc_endpoints(oh_lc, inter_val="days")))
  })  # end sapply
# rbind list into single xts or matrix
volat_hurst <- t(volat_hurst)

x11()
plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diff_it(volat_hurst)
plot(foo)
pacf(foo[, 1])


# wippp
############### homework
# Summary: Calculate a time series of annual Hurst
# exponents for S&P500 stocks.
# Plot a scatterplot of Hurst for the years 2008 and 2009.

# Set up the data as follows:
library(HighFreq)

# Load S&P500 constituent stock prices
load("C:/Develop/lecture_slides/data/sp500.RData")

oh_lc <- log(sp500_env$SIG)
quantmod::chart_Series(Cl(oh_lc))
hi_gh <- quantmod::Hi(oh_lc)
lo_w <- quantmod::Lo(oh_lc)
calc_hurst(hi_gh, lo_w, rutils::calc_endpoints(oh_lc, inter_val="months"))

end_p <- rutils::calc_endpoints(oh_lc, inter_val="years")
volat_hurst <- sapply(seq_along(end_p)[-1],
                      function(it) {
                        oh_lc <- oh_lc[end_p[it-1]:end_p[it]]
                        hi_gh <- quantmod::Hi(oh_lc)
                        lo_w <- quantmod::Lo(oh_lc)
                        c(volatility=HighFreq::calc_var_ohlc(oh_lc),
                          hurst=calc_hurst(hi_gh, lo_w, rutils::calc_endpoints(oh_lc, inter_val="months")))
                      })  # end sapply
# Transpose the matrix
volat_hurst <- t(volat_hurst)

plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diff_it(volat_hurst)
plot(foo)
plot(cbind(foo[, 1], volat_hurst[, 2]))
pacf(foo[, 1])

mod_el <- lm(volat_hurst[, 2] ~ volat_hurst[, 1])
model_sum <- summary(mod_el)
model_sum$coefficients[2, 3]
plot(volat_hurst[, 2] ~ volat_hurst[, 1])
abline(mod_el)

###

# Scrub data in sp500_env

x[is.infinite(x)] <- NA
x <- zoo::na.locf(x)
x <- zoo::na.locf(x, fromLast=TRUE)


###


hurst_prof <- eapply(sp500_env, function(oh_lc) {
  end_p <- rutils::calc_endpoints(oh_lc, inter_val="years")
  if (NROW(end_p) > 3) {
    oh_lc <- log(oh_lc)
    volat_hurst <- sapply(seq_along(end_p)[-1],
                          function(it) {
                            oh_lc <- oh_lc[end_p[it-1]:end_p[it]]
                            hi_gh <- quantmod::Hi(oh_lc)
                            lo_w <- quantmod::Lo(oh_lc)
                            c(volatility=HighFreq::calc_var_ohlc(oh_lc),
                              hurst=calc_hurst(hi_gh, lo_w, rutils::calc_endpoints(oh_lc, inter_val="months")))
                          })  # end sapply
    # Transpose the matrix
    volat_hurst <- t(volat_hurst)
    # Scrub the data
    volat_hurst <- zoo::na.locf(volat_hurst)
    zoo::na.locf(volat_hurst, fromLast=TRUE)
  } else {
    c("Not enough years for Hurst\n")
    cbind(volatility=rep(1, NROW(end_p)), hurst=rep(0.5, NROW(end_p)))
  }
})  # end eapply

hurst_prof <- lapply(hurst_prof, function(volat_hurst) {
  # Scrub the data
  volat_hurst[is.infinite(volat_hurst)] <- NA
  volat_hurst <- zoo::na.locf(volat_hurst)
  zoo::na.locf(volat_hurst, fromLast=TRUE)
})  # end lapply

save(hurst_prof, file="C:/Develop/lecture_slides/data/sp500_perf.RData")

bar <- sapply(hurst_prof, function(x) sum(is.na(x) | is.infinite(x)))
max(bar)
which.max(bar)
hurst_prof[[names(which.max(bar))]]

get_tval <- function(x) {
  cat("NROW(x) = ", NROW(x), "\n")
  x <- na.omit(x)
  if ((NROW(x) > 3) & (sum(is.na(x))==0)) {
    mod_el <- lm(x[, 2] ~ x[, 1])
    summary(mod_el)$coefficients[2, 3]
  } else 1
}  # end get_tval
bar <- sapply(hurst_prof, get_tval)

bar <- sapply(hurst_prof, function(x) {
  # cat("dim(x) = ", dim(x), "\n")
  if (NROW(x) > 3) {
    mod_el <- lm(x[, 2] ~ x[, 1])
    summary(mod_el)$coefficients[2, 3]
  } else 0
})  # end sapply

bar <- sort(bar)
hist(bar, freq=FALSE)
which.max(bar)
bar[[names(which.max(bar))]]
hurst_prof[[names(which.max(bar))]]
which.min(bar)
bar[[names(which.min(bar))]]
hurst_prof[[names(which.min(bar))]]
plot(hurst_prof[[names(which.min(bar))]])

bar <- sapply(hurst_prof, function(x) {
  max(x[, 2])
})  # end sapply
re_turns <- price_s[, names(tail(bar, 100))]
re_turns <- rutils::diff_it(log(re_turns))
save(re_turns, file="C:/Develop/lecture_slides/data/sp100_rets.RData")

col_names <- colnames(hurst_prof$AAPL)
bar <- lapply(hurst_prof, function(x) {
  x <- cbind((x[, 1]-min(x[, 1]))/(max(x[, 1]-min(x[, 1]))), (x[, 2]-min(x[, 2]))/(max(x[, 2])-min(x[, 2])))
  colnames(x) <- col_names
  x
})  # end lapply
foo <- NULL
unlist(sapply(hurst_prof, function(x) {
  foo <<- rbind(foo, x)
  NULL
}))  # end sapply
plot(foo)



###############
### Forecasting for univariate regression

library(HighFreq)
source("C:/Develop/R/scripts/market_making.R")

data_dir <- "C:/Develop/data/ib_data/"
sym_bol <- "ES"
load(paste0(data_dir, sym_bol, "_ohlc.RData"))
n_rows <- NROW(oh_lc)
oh_lc <- coredata(oh_lc)

look_back <- 500
de_sign <- cbind(rep(1, look_back), 1:look_back)
deg_free <- (look_back - NCOL(de_sign))
design_inv <- MASS::ginv(de_sign)
design_2 <- MASS::ginv(crossprod(de_sign))
lagg <- 1
oo_s <- cbind(1, look_back + lagg)
oos_t <- t(oo_s)

# influ_ence <- de_sign %*% design_inv
# fit_ted <- drop(influ_ence %*% se_ries)

# se_ries <- oh_lc[(look_back+1):(look_back+100), 4]
# val_ue <- oh_lc[201, 4]
# calc_zscore(val_ue, se_ries, de_sign, design_inv, design_2, oo_s, oos_t, deg_free)

z_scores <- sapply((look_back+1):n_rows, function(x) {
  se_ries <- oh_lc[(x-look_back):(x-1), 4]
  val_ue <- oh_lc[x, 4]
  calc_zscore(val_ue, se_ries, de_sign, design_inv, design_2, oo_s, oos_t, deg_free)
})  # end sapply
z_scores <- c(rep(0, look_back), z_scores)

hist(z_scores, breaks=100, freq=FALSE)
quantile(z_scores, 0.9)

foo <- which((z_scores < quantile(z_scores, 0.1)) & (z_scores > quantile(z_scores, 0.01)))
foo <- which((z_scores > quantile(z_scores, 0.9)) & (z_scores < quantile(z_scores, 0.99)))
foo <- foo[(foo>(30)) & (foo<(n_rows-100))]
foo <- lapply(foo, function(x) {
  oh_lc[(x-30):(x+100), 4]/oh_lc[x, 4]
})
foo <- rutils::do_call(cbind, foo)
foo <- rowMeans(foo)
plot(foo, t="l")




###############
### Tests for HighFreq functions

library(rutils)
# detach("package:HighFreq")
library(dygraphs)

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/calc_weights.cpp")


## Tests for sorting and ranking

da_ta <- round(runif(7), 2)
all.equal(da_ta, drop(sort_back(da_ta)))
all.equal(rank(da_ta), drop(calc_ranks(da_ta)))
drop(calc_ranks_m(da_ta))
sum(calc_ranks_m(da_ta))
da_ta <- xts::xts(runif(7), seq.Date(Sys.Date(), by=1, length.out=7))
all.equal(rank(drop(coredata(da_ta))), drop(calc_ranks(da_ta)))



## Load S&P500 stock returns
# re_turns <- na.omit(rutils::etf_env$re_turns[, 1:9])
load(file="C:/Develop/lecture_slides/data/sp500_returns.RData")
ret_s <- returns_100["2000/"]
ret_s[1, is.na(ret_s[1, ])] <- 0
ret_s <- zoo::na.locf(ret_s, na.rm=FALSE)
n_rows <- NROW(ret_s)
n_cols <- NCOL(ret_s)


## Tests for HighFreq::calc_weights()

## Calculate rank_sharpe weights using R
weights_r <- sapply(ret_s, function(x) mean(x)/sd(x))
weights_r <- rank(weights_r)
weights_r <- (weights_r - mean(weights_r))

# weights_r <- 0.01*weight_s/arma::stddev(re_turns*weight_s)

## Calculate weights using RcppArmadillo
weight_s <- drop(calc_weights(ret_s, model_type="rank_sharpe", scal_e=FALSE))
all.equal(weight_s, weights_r, check.attributes=FALSE)


## Calculate max_sharpe weights using R

# Calculate covariance matrix of ETF returns
# ret_s <- na.omit(rutils::etf_env$re_turns[, 1:16])
ei_gen <- eigen(cov(ret_s))
# Calculate regularized inverse of covariance matrix
max_eigen <- 3
eigen_vec <- ei_gen$vectors[, 1:max_eigen]
eigen_val <- ei_gen$values[1:max_eigen]
in_verse <- eigen_vec %*% (t(eigen_vec) / eigen_val)
# Define shrinkage intensity and apply shrinkage to the mean returns
al_pha <- 0.5
col_means <- colMeans(ret_s)
col_means <- ((1-al_pha)*col_means + al_pha*mean(col_means))

weights_r <- in_verse %*% col_means
n_col <- NCOL(ret_s)
weights_r <- weights_r*sd(ret_s %*% rep(1/n_col, n_col))/sd(ret_s %*% weights_r)

## Calculate weights using RcppArmadillo
weight_s <- drop(calc_weights(ret_s, model_type="max_sharpe", al_pha=al_pha, max_eigen=3, scal_e=FALSE))
all.equal(weight_s, drop(weights_r), check.attributes=FALSE)



## Calculate returns on equal weight portfolio
in_dex <- rowMeans(ret_s)
std_dev <- sd(in_dex[in_dex<0])
in_dex <- xts(in_dex, index(ret_s))

foo <- weight_returns(ret_s, weight_s)
bar <- ret_s %*% weight_s
all.equal(foo, bar)


# Define maximum Sharpe portfolio weights
calc_weights <- function(re_turns) {
  # ei_gen <- eigen(cov(re_turns))
  # # set tolerance for determining zero eigenvalues
  # to_l <- sqrt(.Machine$double.eps)
  # # check for zero eigenvalues
  # not_zero <- (ei_gen$values > (to_l * ei_gen$values[1]))
  # in_verse <- ei_gen$vectors[, not_zero] %*% (t(ei_gen$vectors[, not_zero])/ei_gen$values[not_zero])
  # weight_s <- in_verse %*% apply(re_turns, 2, mean)
  # weight_s/sum(abs(weight_s))

  weight_s <- sapply(re_turns[((n_rows-500):n_rows)], median)
  # weight_s <- (order(order(weight_s)-1)-1)
  # weight_s <- order(order(weight_s))
  # weight_s <- order(weight_s)
  weight_s <- (order(weight_s)-1)
  # weight_s <- order(order(weight_s, decreasing=TRUE), decreasing=TRUE)
  # weight_s <- or_der[or_der]

} # end calc_weights
weight_s <- calc_weights(ret_sub)


foo <- drop(calc_weights(re_turns[((n_rows-500):n_rows)], typ_e="rankrob", al_pha=0, scal_e=FALSE))


all.equal(weight_s, foo)
foo <- cbind(weight_s, foo)
head(foo, 11)
tail(foo, 11)


weight_s <- colMeans(re_turns)
weight_s <- sapply(re_turns, function(x) mean(x)/sd(x))
weight_s <- sapply(re_turns, moments::skewness)
weight_s <- drop(HighFreq::calc_ranks(weight_s))
weight_s <- (weight_s - mean(weight_s))
names(weight_s) <- colnames(re_turns)


weight_s <- drop(calc_weights(re_turns, typ_e="max_sharpe", al_pha=0))
pnl_s <- (re_turns %*% weight_s)
pnl_s <- xts(cumsum(pnl_s), order.by=index(re_turns))
price_s <- cumsum(rowMeans(re_turns))
pnl_s <- cbind(pnl_s, price_s)
colnames(pnl_s) <- c("Strategy", "Index")

col_names <- colnames(pnl_s)
cap_tion <- paste("Momentum Strategy for S&P500 Stocks")
dygraphs::dygraph(pnl_s, main=cap_tion) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")




###############
### Strategy for market making using limit orders

data_dir <- "C:/Develop/data/ib_data"
setwd(dir=data_dir)
load("oh_lc.RData")
ohlc_data <- coredata(oh_lc)
n_rows <- NROW(ohlc_data)

ohlc_lag <- rutils::lag_it(ohlc_data)
price_s <- ohlc_data[, 4]

buy_spread <- 0.25
sell_spread <- 0.25

# Vectorized version

buy_price <- (ohlc_lag[, 3] - buy_spread)
sell_price <- (ohlc_lag[, 2] + sell_spread)

buy_ind <- (ohlc_data[, 3] < buy_price)
n_buy <- cumsum(buy_ind)
sell_ind <- (ohlc_data[, 2] > sell_price)
n_sell <- cumsum(sell_ind)

buy_s <- numeric(n_rows)
buy_s[buy_ind] <- buy_price[buy_ind]
buy_s <- cumsum(buy_s)
sell_s <- numeric(n_rows)
sell_s[sell_ind] <- sell_price[sell_ind]
sell_s <- cumsum(sell_s)

pnl_s <- ((sell_s-buy_s) - price_s*(n_sell-n_buy))


# Loop version

buy_price <- numeric(n_rows)
sell_price <- numeric(n_rows)
n_buy <- numeric(n_rows)
n_sell <- numeric(n_rows)
buy_s <- numeric(n_rows)
sell_s <- numeric(n_rows)
pnl_s <- numeric(n_rows)

for (it in 2:n_rows) {
  buy_price[it] <- (ohlc_lag[it, 3] - buy_spread)
  sell_price[it] <- (ohlc_lag[it, 2] + sell_spread)

  buy_ind <- (ohlc_data[it, 3] < buy_price[it])
  sell_ind <- (ohlc_data[it, 2] > sell_price[it])
  n_buy[it] <- n_buy[it-1] + buy_ind
  n_sell[it] <- n_sell[it-1] + sell_ind
  buy_s[it] <- buy_s[it-1] + buy_ind*buy_price[it]
  sell_s[it] <- sell_s[it-1] + sell_ind*sell_price[it]
  pnl_s[it] <- ((sell_s[it] - buy_s[it]) - price_s[it]*(n_sell[it] - n_buy[it]))
}  # end for


plot(pnl_s[c(1, rutils::calc_endpoints(oh_lc, inter_val="minutes"))], t="l", main="Market Making Strategy")




###############
### Strategy using OHLC technical indicators

library(HighFreq)

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/lm_arma.cpp")


# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")


# load OHLC data
# oh_lc <- HighFreq::SPY
# oh_lc <- HighFreq::SPY["2010-10/2010-11"]
# oh_lc <- rutils::etf_env$VTI
# load recent ES1 futures data
# load(file="C:/Develop/data/ES1.RData")
# or
# oh_lc <- read.zoo(file="C:/Develop/data/new_bar/ES1.csv", header=TRUE, sep=",",
#                   drop=FALSE, format="%Y-%m-%d %H:%M",
#                   FUN=as.POSIXct, tz="America/New_York")
# oh_lc <- as.xts(oh_lc)
# oh_lc <- oh_lc["T09:00:00/T16:30:00"]
# save(oh_lc, file="C:/Develop/data/ES1.RData")
# load recent combined futures data
load(file="C:/Develop/data/combined.RData")

# set up data for signal
sym_bol <- "UX1"
oh_lc <- com_bo[, paste(sym_bol, c("Open", "High", "Low", "Close"), sep=".")]

ohlc_log <- log(oh_lc)
# sum(is.na(oh_lc))
# sapply(oh_lc, class)
# tail(oh_lc, 11)
clos_e <- Cl(ohlc_log)
close_num <- drop(coredata(clos_e))
re_turns <- rutils::diff_it(clos_e)
# regression with clos_e prices as response requires clos_e to be a vector
# clos_e <- drop(coredata(clos_e)
# plot dygraph
dygraphs::dygraph(xts::to.hourly(clos_e), main=sym_bol)
# random data
# re_turns <- xts(rnorm(NROW(oh_lc), sd=0.01), index(oh_lc))
# clos_e <- drop(coredata(cumsum(re_turns))

# Define OHLC data
op_en <- Op(ohlc_log)
hi_gh <- Hi(ohlc_log)
high_num <- as.numeric(hi_gh)
lo_w <- Lo(ohlc_log)
low_num <- as.numeric(lo_w)
close_high <- (close_num == high_num)
close_high_count <- drop(HighFreq::roll_count(close_high))
close_low <- (clos_e == lo_w)
close_low_count <- drop(HighFreq::roll_count(close_low))
open_high <- (op_en == hi_gh)
open_high_count <- drop(HighFreq::roll_count(open_high))
open_low <- (op_en == lo_w)
open_low_count <- drop(HighFreq::roll_count(open_low))


# Set up data for trading
sym_bol <- "ES1"
re_turns <- rutils::diff_it(log(com_bo[, paste(sym_bol, "Close", sep=".")]))



# vari_ance <- (hi_gh - lo_w)^2
look_back <- 11
vari_ance <- HighFreq::roll_var_ohlc(oh_lc=oh_lc, look_back=look_back, scal_e=FALSE)
colnames(vari_ance) <- "variance"
vol_at <- sqrt(vari_ance)
colnames(vol_at) <- "volat"
vol_ume <- Vo(oh_lc)
colnames(vol_ume) <- "volume"

# Define current and future returns
# re_turns <- rutils::diff_it(clos_e)
# trailing average returns
re_turns <- rutils::diff_it(clos_e, lagg=look_back)/sqrt(look_back)
colnames(re_turns) <- "returns"
# returns_adv <- rutils::lag_it(re_turns, lagg=-1)
# or
# returns_adv <- 0.5*(returns_adv + rutils::lag_it(returns_adv, lagg=-1))
returns_adv <- rutils::lag_it(rutils::diff_it(clos_e, lagg=look_back), lagg=-look_back)/sqrt(look_back)
# returns_adv <- rutils::lag_it(HighFreq::roll_sum(re_turns, look_back=look_back), lagg=-look_back)/look_back
# returns_adv <- xts(returns_adv, index(oh_lc))
colnames(returns_adv) <- "returns_adv"
# Scale returns using sigmoid
# returns_adv <- plogis(returns_adv, scale=-quantile(returns_adv, 0.01))
# returns_adv <- (returns_adv - median(returns_adv))
# colnames(returns_adv) <- "returns_adv"



# Begin old stuff

###############
### Strategy using rolling z-scores over OHLC technical indicators
# with regression and dimension reduction


# colnames(re_turns) <- "returns"
# create design matrix
# date_s <- xts::.index(oh_lc)
in_dex <- 1:NROW(oh_lc)
de_sign <- matrix(in_dex, nc=1)

mod_el <- HighFreq::calc_lm(res_ponse=as.numeric(returns_adv), de_sign=cbind(re_turns, vari_ance))
mod_el$coefficients

# old: calculate sig_nal as the residual of the regression of the time series of clos_e prices
look_back <- 11
sig_nal <- HighFreq::roll_zscores(res_ponse=close_num, de_sign=de_sign, look_back=look_back)
colnames(sig_nal) <- "sig_nal"
sig_nal[1:look_back] <- 0
# or
sig_nal <- calc_signal(look_back, close_num, de_sign)
hist(sig_nal, freq=FALSE)
# hist(sig_nal, xlim=c(-10, 10), freq=FALSE)

# old: perform parallel loop over look_backs
look_backs <- 15:35
library(parallel)
num_cores <- detectCores()
clus_ter <- makeCluster(num_cores-1)
# clusterExport(clus_ter, varlist=c("clos_e", "de_sign"))
signal_s <- parLapply(clus_ter, X=look_backs, fun=calc_signal, clos_e=close_num, de_sign=de_sign)


# trade entry and exit levels
en_ter <- 1.0
ex_it <- 0.5
pnl_s <- calc_revert(signal_s[[1]], re_turns, en_ter, ex_it)
quantmod::chart_Series(pnl_s[endpoints(pnl_s, on="days")])

# old uses calc_revert(): run strategies over a vector of trade entry levels
run_strategies <- function(sig_nal, re_turns, en_ters, ex_it, return_series=TRUE) {
  # sapply(en_ters, calc_revert, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it)
  pnl_s <- lapply(en_ters, calc_revert, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it)
  pnl_s <- rutils::do_call(cbind, pnl_s)
  if (return_series) {
    pnl_s <- rowSums(pnl_s)
  } else {
    pnl_s <- as.numeric(pnl_s[NROW(pnl_s)])
  }  # end if
  return(pnl_s)
}  # end run_strategies

# define vector of trade entry levels
en_ters <- (5:30)/10
# pnl_s <- run_strategies(signal_s[[1]], re_turns, en_ters, ex_it=ex_it)
# pnl_s <- xts(pnl_s, index(oh_lc))
# quantmod::chart_Series(pnl_s)
clusterExport(clus_ter, varlist=c("calc_revert"))


## old uses run_strategies(): simulate ensemble of strategies and return heatmap of final pnls
pnl_s <- parLapply(clus_ter, X=signal_s, fun=run_strategies, re_turns=re_turns, en_ters=en_ters, ex_it=ex_it, return_series=FALSE)
pnl_s <- rutils::do_call(rbind, pnl_s)
colnames(pnl_s) <- paste0("en_ter=", en_ters)
rownames(pnl_s) <- paste0("look_back=", look_backs)
heatmap(pnl_s, Colv=NA, Rowv=NA, col=c("red", "blue"))
rgl::persp3d(z=pnl_s, col="green")
plot(colSums(pnl_s), t="l", xlab="")


## Simulate ensemble of strategies and return the average pnls
pnl_s <- parLapply(clus_ter, X=signal_s[1:10], fun=run_strategies, re_turns=re_turns, en_ters=en_ters, ex_it=ex_it, return_series=TRUE)
pnl_s <- rutils::do_call(cbind, pnl_s)
pnl_s <- xts(pnl_s, index(oh_lc))
colnames(pnl_s) <- paste0("look_back=", look_backs[1:10])
# plot matrix using plot.zoo()
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(pnl_s))
plot.zoo(pnl_s[endpoints(pnl_s, on="days")], main="pnls", lwd=2,
         plot.type="single", xlab="", ylab="pnls", col=col_ors)
# Add legend
legend("bottomright", legend=colnames(pnl_s), col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
# plot single dygraph
pnl_s <- rowSums(pnl_s)
pnl_s <- xts(pnl_s, index(oh_lc))
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clos_e, pnl_s)[endpoints(pnl_s, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


stopCluster(clus_ter)  # Stop R processes over cluster under Windows

# Count the number of consecutive TRUE elements, and reset to zero after every FALSE element
roll_countr <- function(sig_nal) {
  count_true <- integer(NROW(sig_nal))
  count_true[1] <- sig_nal[1]
  for (it in 2:NROW(sig_nal)) {
    if (sig_nal[it])
      count_true[it] <- count_true[it-1] + sig_nal[it]
    else
      count_true[it] <- sig_nal[it]
  }  # end for
  return(count_true)
}  # end roll_countr
foo <- logical(21)
foo[sample(NROW(foo), 12)] <- TRUE
barr <- roll_countr(foo)
foo <- roll_countr(close_high)
bar <- hist(foo, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar <- hist(close_high_count, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar <- hist(close_low_count, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar$counts
all.equal(roll_countr(close_high), drop(HighFreq::roll_count(close_high)), check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_countr(close_high),
  rcpp=HighFreq::roll_count(close_high),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# Contrarian strategy
n_rows <- NROW(oh_lc)
po_sit <- rep(NA_integer_, n_rows)
po_sit[1] <- 0
po_sit[close_high] <- (-1)
po_sit[close_low] <- 1
po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
po_sit <- rutils::lag_it(po_sit, lagg=1)

# Contrarian strategy using HighFreq::roll_count()
po_sit <- rep(NA_integer_, n_rows)
po_sit[1] <- 0
po_sit[close_high_count>2] <- (-1)
po_sit[close_low_count>2] <- 1
po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
po_sit <- rutils::lag_it(po_sit, lagg=1)

# Contrarian strategy using roll_cum()
po_sit <- rep(0, n_rows)
po_sit[close_high] <- (-1)
po_sit[close_low] <- 1
po_sit <- roll_cum(po_sit, 2)
po_sit <- rutils::lag_it(po_sit, lagg=1)


# wipp
# Contrarian strategy using roll_maxmin()
look_back <- 11
max_min <- roll_maxmin(close_num, look_back)
close_max <- (close_num == max_min[, 1])
close_min <- (close_num == max_min[, 2])
vol_at <- HighFreq::roll_var_ohlc(oh_lc=ohlc_log, look_back=5*look_back, scal_e=FALSE)
vol_at <- sqrt(vol_at)
vol_at[1] <- vol_at[2]
colnames(vol_at) <- "volat"
dra_w <- rutils::diff_it(close_num, lagg=look_back)
dra_w <- as.numeric(dra_w/vol_at)
max_min <- roll_maxmin(dra_w, look_back)
draw_max <- (dra_w == max_min[, 1])
draw_min <- (dra_w == max_min[, 2])

po_sit <- rep(NA_integer_, n_rows)
po_sit[1] <- 0
# po_sit[close_max] <- (-1)
# po_sit[close_min] <- 1
po_sit[(dra_w>4) & draw_max & close_max] <- (-1)
po_sit[(dra_w<(-4)) & draw_min & close_min] <- 1
po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
po_sit <- rutils::lag_it(po_sit, lagg=1)

# Number of trades
sum(abs(rutils::diff_it(po_sit))) / NROW(po_sit)

# Calculate strategy pnl_s
pnl_s <- cumsum(po_sit*re_turns)
colnames(pnl_s) <- "strategy"


# dygraphs plot
end_p <- xts::endpoints(pnl_s, on="days")
dygraphs::dygraph(pnl_s[end_p], main="ES1 strategy")
# data for plot
da_ta <- cbind(clos_e, pnl_s)[end_p]
colnames(da_ta) <- c(sym_bol, "pnls")
# or
da_ta <- cbind(clos_e, po_sit)
colnames(da_ta) <- c(sym_bol, "position")
# dygraphs plot with two "y" axes
second_series <- colnames(da_ta)[2]
dygraphs::dygraph(da_ta, main=paste(sym_bol, "Strategy Using OHLC Technical Indicators")) %>%
  dyAxis("y", label=sym_bol, independentTicks=TRUE) %>%
  dyAxis("y2", label=second_series, independentTicks=TRUE) %>%
  dySeries(second_series, axis="y2", col=c("blue", "red"))


x11()
# date_s <- index(oh_lc)
po_sit <- xts::xts(po_sit, index(oh_lc))
# rang_e <- "2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"
rang_e <- "2018-02-05/2018-02-07"
dygraphs::dygraph(pnl_s[rang_e], main="ES1 strategy")
# Calculate integer index of date range
# rang_e <- index(oh_lc["2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"])
# rang_e <- index(oh_lc[rang_e])
# rang_e <- (which(date_s==min(rang_e)):which(date_s==max(rang_e)))
# plot prices
chart_Series(x=Cl(oh_lc[rang_e]))
# Add background shading of areas
add_TA(po_sit[rang_e] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(po_sit[rang_e] < 0, on=-1,
       col="lightgrey", border="lightgrey")

# Calculate integer index of date range
date_s <- xts::.index(oh_lc)
rang_e <- xts::.index(oh_lc[rang_e])
rang_e <- (which(date_s==min(rang_e)):which(date_s==max(rang_e)))
# Add vertical lines
# close_high_count <- xts::xts(close_high_count, index(oh_lc))
# close_low_count <- xts::xts(close_low_count, index(oh_lc))
close_high_count <- drop(close_high_count)
close_low_count <- drop(close_low_count)
abline(v=which(close_high_count[rang_e]>0), col='red')
abline(v=which(close_low_count[rang_e]>0), col='blue')

draw_max <- xts::xts(draw_max, index(oh_lc))
draw_min <- xts::xts(draw_min, index(oh_lc))
abline(v=draw_max[rang_e], col='blue')
abline(v=draw_min[rang_e], col='red')
# Add background shading of areas
chart_Series(x=Cl(oh_lc[rang_e]))
add_TA(draw_max[rang_e], on=-1,
       col="blue", border="blue")
add_TA(draw_min[rang_e], on=-1,
       col="red", border="red")

# wipp
# dygraphs plot with max_min lines
da_ta <- xts::xts(cbind(close_num, max_min), index(oh_lc))[rang_e]
colnames(da_ta) <- c(sym_bol, "max", "min")
col_ors <- c("blue", "red", "green")
dygraphs::dygraph(da_ta, main=paste(sym_bol, "max and min lines")) %>%
  dyOptions(colors=col_ors)

# Standard plot with max_min lines
# plot(as.numeric(da_ta[, 1]), type="l", col="blue",
#      main=paste(sym_bol, "max and min lines"),
#      xlab="", ylab="")
# lines(da_ta[, 2], col="red")
# lines(da_ta[, 3], col="green")
plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
quantmod::chart_Series(da_ta, theme=plot_theme, name=paste(sym_bol, "max and min lines"))
legend(x="left", title=NULL, legend=colnames(da_ta),
       inset=0.1, cex=1.2, bg="white", bty="n",
       lwd=6, lty=1, col=col_ors)


# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vec_tor, look_back) {
  len_gth <- NROW(vec_tor)
  max_min <- matrix(numeric(2*len_gth), nc=2)
  # Startup periods
  max_min[1, 1] <- vec_tor[1]
  max_min[1, 2] <- vec_tor[1]
  for (it in 2:(look_back-1)) {
    sub_vec <- vec_tor[1:it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  # remaining periods
  for (it in look_back:len_gth) {
    sub_vec <- vec_tor[(it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr

max_min <- roll_maxmin(close_num, look_back)
max_minr <- roll_maxminr(close_num, look_back)
all.equal(max_min, max_minr)
bar <- TTR::runMax(x=close_num, n=look_back)
all.equal(max_min[-(1:look_back), 1], bar[-(1:look_back)])
bar <- TTR::runMin(x=close_num, n=look_back)
all.equal(max_min[-(1:look_back), 2], bar[-(1:look_back)])
max_min <- xts(max_min, index(clos_e["2014-05"]))
dygraphs::dygraph(max_min[, 1]-clos_e["2014-05"])

library(microbenchmark)
summary(microbenchmark(
  tt_r=TTR::runMax(x=clos_e["2014-05"], n=look_back),
  rcpp=roll_maxmin(as.numeric(clos_e["2014-05"]), look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# end of old stuff


###############
### Strategy using static betas over OHLC technical indicators
# with regression and dimension reduction

# Load OHLC futures data
load(file="C:/Develop/data/combined.RData")

# Define OHLC technical indicators
# residuals of the regression of the time series of clos_e prices
date_s <- xts::.index(oh_lc)
# foo <- unique(date_s)
de_sign <- matrix(date_s, nc=1)
# foo <- MASS::ginv(de_sign)
look_back <- 11
z_scores <- HighFreq::roll_zscores(res_ponse=clos_e,
                                   de_sign=de_sign,
                                   look_back=look_back)
colnames(z_scores) <- "z_scores"
z_scores[1:3] <- 0
close_open <- (clos_e-op_en)
colnames(close_open) <- "close_open"
close_high <- (clos_e-hi_gh)
colnames(close_high) <- "close_high"
close_low <- (clos_e-lo_w)
colnames(close_low) <- "close_low"
# sk_ew <- ((hi_gh+lo_w) - (op_en+clos_e))
sk_ew <- ((hi_gh+lo_w) - (op_en+clos_e))
colnames(sk_ew) <- "sk_ew"
# moment_um <- ((clos_e-op_en) - (hi_gh-lo_w))
moment_um <- ((clos_e-op_en) - (hi_gh-lo_w)) + 1.0
colnames(moment_um) <- "moment_um"

# close_high <- (hi_gh - rutils::lag_it(hi_gh))
# close_low <- (lo_w - rutils::lag_it(lo_w))
# Select only independent indicators

indicator_s <- cbind(re_turns, vol_at, sk_ew)

# indicator_s <- cbind(re_turns, close_open, close_high, close_low, vol_at, sk_ew, moment_um, z_scores)
# colnames(indicator_s) <- c("close_high", "op_en_hi_gh", "clos_e_hi_gh")
# indicator_s <- cbind(re_turns, vol_at, sk_ew, moment_um, indicator_s)
# indicator_s <- cbind(op_en-hi_gh, op_en-lo_w, op_en-clos_e, clos_e-hi_gh, clos_e-lo_w, hi_gh-lo_w)
# colnames(indicator_s) <- c("open_high", "open_low", "open_close", "close_high", "close_low", "high_low")
# indicator_s <- cbind(sk_ew, moment_um, indicator_s)
# Select only independent indicators
# indicator_s <- cbind(op_en-hi_gh, clos_e-hi_gh)
# colnames(indicator_s) <- c("op_en_hi_gh", "clos_e_hi_gh")
# indicator_s <- cbind(re_turns, sk_ew, indicator_s)
col_names <- colnames(indicator_s)

# Scale indicator_s using roll_scale()
look_back <- 11
indicator_s <- roll::roll_scale(data=indicator_s, width=look_back, min_obs=1)
indicator_s[1, ] <- 0
round(cor(indicator_s), 3)
indicator_s <- cbind(indicator_s, z_scores)
indicator_s[1:3, ] <- 0
col_names <- colnames(indicator_s)


# Scale indicator_s using sigmoid
indicator_s <- lapply(1:NCOL(indicator_s), function(col_umn) {
  x <- plogis(indicator_s[, col_umn], scale=-quantile(indicator_s[, col_umn], 0.01))
  (x - median(x))
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)

round(cor(indicator_s), 3)

# Calculate PCA of technical indicators
pc_a <- prcomp(indicator_s)
pc_a$sdev
pc_a$rotation


## Create design matrix for SPY or ES1
# de_sign <- cbind(re_turns, close_high, close_low, rutils::diff_it(vari_ance), rutils::diff_it(vol_ume))
# de_sign from pc_a
# rolling average
indicator_s <- lapply(1:NCOL(indicator_s), function(col_umn) {
  HighFreq::roll_sum(indicator_s[, col_umn], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- col_names
# de_sign <- as.data.frame(cbind(returns_adv, re_turns, vari_ance))
# colnames(de_sign) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
de_sign <- as.data.frame(cbind(returns_adv, indicator_s))
colnames(de_sign)[1] <- "returns_adv"
# or
de_sign <- cbind(HighFreq::roll_sum(re_turns, look_back=look_back),
                 HighFreq::roll_sum(moment_um, look_back=look_back),
                 HighFreq::roll_sum(sk_ew, look_back=look_back))
de_sign <- as.data.frame(cbind(returns_adv, de_sign))
# de_sign <- cbind(returns_adv>0, de_sign)
colnames(de_sign) <- c("indic", "returns", "momentum", "skew")

# de_sign <- cbind(de_sign, rutils::lag_it(de_sign, lagg=1), rutils::lag_it(de_sign, lagg=2), rutils::lag_it(de_sign, lagg=3), rutils::lag_it(de_sign, lagg=4))
# de_sign <- cbind(re_turns, close_high, close_low, re_turns/sqrt(vari_ance), close_high/sqrt(vari_ance), vari_ance, vol_ume)
# colnames(de_sign)[4:5] <- c("re_turns_s", "close_high_s")
## Apply rolling centering and scaling to the design matrix
de_sign <- lapply(de_sign, function(x) (x-mean(x))/sd(x))
de_sign <- rutils::do_call(cbind, de_sign)
sum(is.na(de_sign))



## Create design matrix for ES1, TY1, UX1
# de_sign <- cbind(re_turns, close_high, close_low, rutils::diff_it(vari_ance), rutils::diff_it(vol_ume))
# de_sign from pc_a
# define indicators
look_back <- 5
indicator_s <- c("ES1.Close", "TY1.Close", "TU1.Close", "UX1.Close", "UX2.Close")
dygraphs::dygraph(oh_lc[, indicator_s[2]]-oh_lc[, indicator_s[3]])
indicator_s <- lapply(indicator_s, function(col_umn) {
  col_umn <- oh_lc[, col_umn]
  sig_nal <- rutils::diff_it(clos_e, lagg=look_back)/sqrt(look_back)/sqrt(HighFreq::roll_var_ohlc(oh_lc=oh_lc, look_back=look_back, scal_e=FALSE))
  HighFreq::roll_sum(indicator_s[, col_umn], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- col_names
de_sign <- as.data.frame(cbind(returns_adv, de_sign))
# colnames(de_sign) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
de_sign <- as.data.frame(cbind(returns_adv, indicator_s))
colnames(de_sign)[1] <- "returns_adv"
# or
de_sign <- cbind(HighFreq::roll_sum(re_turns, look_back=look_back),
                 HighFreq::roll_sum(moment_um, look_back=look_back),
                 HighFreq::roll_sum(sk_ew, look_back=look_back))
de_sign <- as.data.frame(cbind(returns_adv, de_sign))
# de_sign <- cbind(returns_adv>0, de_sign)
colnames(de_sign) <- c("indic", "returns", "momentum", "skew")

# de_sign <- cbind(de_sign, rutils::lag_it(de_sign, lagg=1), rutils::lag_it(de_sign, lagg=2), rutils::lag_it(de_sign, lagg=3), rutils::lag_it(de_sign, lagg=4))
# de_sign <- cbind(re_turns, close_high, close_low, re_turns/sqrt(vari_ance), close_high/sqrt(vari_ance), vari_ance, vol_ume)
# colnames(de_sign)[4:5] <- c("re_turns_s", "close_high_s")
## Apply rolling centering and scaling to the design matrix
de_sign <- lapply(de_sign, function(x) (x-mean(x))/sd(x))
de_sign <- rutils::do_call(cbind, de_sign)
sum(is.na(de_sign))





## run regressions of future returns against different indicators

# lm formula
col_names <- colnames(de_sign)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse=" + "), sep="~"))
for_mula <- as.formula(paste(col_names[1], paste(paste(col_names[-1], collapse=" + "), "- 1"), sep="~"))


# find extreme returns
ex_treme <- which(returns_adv > quantile(returns_adv, 0.99) | returns_adv < quantile(returns_adv, 0.01))
ex_treme <- sort(unique(c(ex_treme, ex_treme+1, ex_treme-1)))

# perform regression
mod_el <- lm(for_mula, data=de_sign)
# mod_el <- lm(returns_adv[-ex_treme] ~ de_sign[-ex_treme, ])
model_sum <- summary(mod_el)
model_sum$coefficients
weight_s <- model_sum$coefficients[, 1]
weight_s <- model_sum$coefficients[, 1][-1]
sig_nal <- xts(as.matrix(de_sign)[, -1] %*% weight_s, order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# Signal from z-scores (t-values) of trailing slope
de_sign <- matrix(xts::.index(oh_lc), nc=1)
look_back <- 3
sig_nal <- HighFreq::roll_zscores(res_ponse=clos_e, de_sign=de_sign, look_back=look_back)
sig_nal <- roll::roll_scale(data=sig_nal, width=look_back, min_obs=1)
sig_nal[1:look_back, ] <- 0
sig_nal[is.infinite(sig_nal)] <- NA
sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
sum(is.infinite(sig_nal))
sum(is.na(sig_nal))
sd(sig_nal)
hist(sig_nal, freq=FALSE)
plot(sig_nal, t="l")


# wipp
# Simulate ensemble of strategies using slope as technical indicator
# mean-reverting strategies
# par_am <- cbind(6:10, rep((3:12)/10, each=NROW(6:10)))
posit_mat <- sapply(4:8, function(look_short) {
  # mean reverting signal
  sig_nal_short <- calc_signal(oh_lc=ohlc_log,
                               clos_e=close_num,
                               de_sign=de_sign,
                               look_short=look_short, high_freq=FALSE)
  # Simulate the positions of mean reverting strategy
  sim_revert(sig_nal_short, re_turns, close_high, close_low, en_ter, ex_it, trade_lag=1)
})  # end sapply
par_am <- cbind(8:12, rep((3:12)/10, each=NROW(8:12)))
posit_mat <- sapply(1:NROW(par_am), function(it) {
  look_short <- par_am[it, 1]
  en_ter <- par_am[it, 2]
  sig_nal <- HighFreq::roll_zscores(res_ponse=clos_e, de_sign=de_sign, look_back=look_short)
  sig_nal[1:look_short, ] <- 0
  # Scale sig_nal using roll_scale()
  sig_nal <- roll::roll_scale(data=sig_nal, width=look_short, min_obs=1)
  sig_nal[1:look_short, ] <- 0
  # sig_nal <- rutils::lag_it(sig_nal, lagg=1)
  # Calculate positions, either: -1, 0, or 1
  po_sit <- rep(NA_integer_, n_rows)
  po_sit[1] <- 0
  po_sit[sig_nal < (-en_ter)] <- 1
  po_sit[sig_nal > en_ter] <- (-1)
  zoo::na.locf(po_sit, na.rm=FALSE)
})  # end sapply
po_sit <- rowMeans(posit_mat)
po_sit[is.na(po_sit)] <- 0
po_sit <- rutils::lag_it(po_sit, lagg=1)
# plot(po_sit, t="l")
pnl_s <- cumsum(po_sit*re_turns)
pnl_s <- clos_e + 2*pnl_s
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clos_e, pnl_s)[endpoints(clos_e, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# Scale returns using MAD median
num_returns <- as.numeric(re_turns)
foo <- sapply((look_back+1):NROW(num_returns), function(it) {
  sub_vec <- num_returns[(it-look_back+1):it]
  (num_returns[it]-median(sub_vec))/mad(sub_vec, constant=1.0)
})  # end sapply
tail(foo)
bar <- HighFreq::roll_scale(mat_rix=re_turns, look_back=look_back, use_median=TRUE)
bar[is.infinite(bar), ] <- 0
tail(drop(bar))
summary(microbenchmark(
  roll=roll::roll_scale(data=num_returns, width=look_back, min_obs=1),
  rcpp=roll_scale(mat_rix=num_returns, look_back=look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

foo <- (num_returns-median(num_returns))/mad(num_returns, constant=1.0)
bar <- HighFreq::calc_scaled(mat_rix=num_returns)
all.equal(foo, drop(bar))

library(microbenchmark)
summary(microbenchmark(
  pure_r=(num_returns-median(num_returns))/mad(num_returns, constant=1.0),
  rcpp=HighFreq::calc_scaled(mat_rix=num_returns),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# wipp
# Newer code: optimize strategies using slope as technical indicator

# OHLC data setup
oh_lc <- log(HighFreq::SPY["2010-10/2010-11"])
ohlc_log <- log(oh_lc)
op_en <- Op(ohlc_log)
hi_gh <- Hi(ohlc_log)
lo_w <- Lo(ohlc_log)
clos_e <- Cl(ohlc_log)
re_turns <- rutils::diff_it(clos_e)
# colnames(re_turns) <- "returns"
close_num <- as.numeric(clos_e)
close_high <- (close_num == high_num)
close_low <- (close_num == low_num)
in_dex <- 1:NROW(oh_lc)
de_sign <- matrix(in_dex, nc=1)

look_back <- 15
run_signal <- function(look_back, re_turns) {
  sig_nal <- HighFreq::roll_scale(mat_rix=re_turns, look_back=look_back, use_median=TRUE)
  sig_nal[1:look_back, ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
  rutils::lag_it(sig_nal, lagg=1)
}  # end run_signal
sig_nal <- run_signal(look_back, re_turns)
run_signal <- function(look_back, clos_e, de_sign) {
  sig_nal <- HighFreq::roll_zscores(res_ponse=clos_e, de_sign=de_sign, look_back=look_back)
  sig_nal[1:look_back, ] <- 0
  # sig_nal <- HighFreq::roll_scale(mat_rix=sig_nal, look_back=look_back, use_median=TRUE)
  # sig_nal[1:look_back, ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
  rutils::lag_it(sig_nal, lagg=1)
}  # end run_signal
sig_nal <- run_signal(look_back, clos_e, de_sign)
hist(sig_nal, freq=FALSE)
hist(sig_nal, xlim=c(-10, 10), freq=FALSE)

# perform parallel loop over look_backs under Windows
look_backs <- 15:35
library(parallel)
clus_ter <- makeCluster(num_cores-1)
clusterExport(clus_ter, varlist=c("clos_e", "de_sign"))
signal_s <- parLapply(clus_ter, X=look_backs, fun=run_signal, clos_e=clos_e, de_sign=de_sign)


# close_high and close_low are Boolean vectors which are TRUE if the close price is at the high or low price
run_strategy <- function(sig_nal, re_turns, en_ter, ex_it, close_high=TRUE, close_low=TRUE) {
  po_sit <- rep(NA_integer_, NROW(sig_nal))
  po_sit[1] <- 0
  # po_sit[sig_nal < (-en_ter)] <- 1
  po_sit[(sig_nal < (-en_ter)) & close_low] <- 1
  # po_sit[sig_nal > en_ter] <- (-1)
  po_sit[(sig_nal > en_ter) & close_high] <- (-1)
  po_sit[abs(sig_nal) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
  po_sit <- po_sit + rutils::lag_it(po_sit, lagg=1)
  pnl_s <- cumsum(po_sit*re_turns)
  pnl_s[NROW(pnl_s)]
  # colnames(pnl_s) <- "strategy"
}  # end run_strategy
# trade entry and exit levels
en_ter <- 2.0
ex_it <- 0.5
pnl_s <- run_strategy(signal_s[[1]], re_turns, en_ter, ex_it, close_high, close_low)


run_strategies <- function(sig_nal, re_turns, en_ters, ex_it, close_high=TRUE, close_low=TRUE) {
  sapply(en_ters, run_strategy, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it, close_high=close_high, close_low=close_low)
  # pnl_s <- lapply(en_ters, run_strategy, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it)
  # pnl_s <- rutils::do_call(cbind, pnl_s)
  # rowSums(pnl_s)
}  # end run_strategies
# trade entry levels
en_ters <- (5:40)/10
foo <- run_strategies(signal_s[[1]], re_turns, en_ters, ex_it=ex_it, close_high, close_low)
clusterExport(clus_ter, varlist=c("run_strategy"))
pnl_s <- parLapply(clus_ter, X=signal_s, fun=run_strategies, re_turns=re_turns, en_ters=en_ters, ex_it=ex_it, close_high=close_high, close_low=close_low)

stopCluster(clus_ter)  # Stop R processes over cluster under Windows
pnl_s <- rutils::do_call(cbind, pnl_s)
rownames(pnl_s) <- paste0("en_ter=", en_ters)
colnames(pnl_s) <- paste0("look_back=", look_backs)
heatmap(pnl_s, Colv=NA, Rowv=NA, col=c("red", "blue"))
pnl_s <- rowSums(pnl_s)
pnl_s <- xts(pnl_s, index(oh_lc))
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clos_e, pnl_s)[endpoints(pnl_s, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# trade ensemble of strategies using slope as technical indicator
# mean-reverting strategies
foo <- sapply(2:15, function(look_back) {
  sig_nal <- HighFreq::roll_zscores(res_ponse=clos_e,
                          de_sign=de_sign,
                          look_back=look_back)
  sig_nal[1:3, ] <- 0
  # sig_nal <- rutils::lag_it(sig_nal)
  -sign(sig_nal)
})  # end sapply
# trending strategies
bar <- sapply(10*(10:15), function(look_back) {
  sig_nal <- HighFreq::roll_zscores(res_ponse=clos_e,
                          de_sign=de_sign,
                          look_back=look_back)
  sig_nal[1:3, ] <- 0
  # sig_nal <- rutils::lag_it(sig_nal)
  sign(sig_nal)
})  # end sapply
po_sit <- cbind(foo, bar)
po_sit <- rowMeans(po_sit)
po_sit[is.na(po_sit)] <- 0
po_sit <- rutils::lag_it(po_sit)
plot(po_sit, t="l")
pnl_s <- cumsum(po_sit*re_turns)
pnl_s <- clos_e + 3*pnl_s
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clos_e, pnl_s), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# regress returns_adv versus moment_um indicator
# moment_um <- moment_um[abs(moment_um)>0.9]
foo <- sapply(1:10, function(look_back) {
  if (look_back>1)
    returns_adv <- rutils::lag_it(HighFreq::roll_sum(re_turns, look_back=look_back), lagg=-look_back)/look_back
  else
    returns_adv <- rutils::lag_it(re_turns, lagg=-look_back)
  mod_el <- lm(returns_adv ~ moment_um)
  model_sum <- summary(mod_el)
  model_sum$coefficients[2, 3]
})  # end sapply

# use average re_turns as predictor
foo <- sapply(1:11, function(look_back) {
  if (look_back>1)
    re_turns <- HighFreq::roll_sum(re_turns, look_back=look_back)/look_back
  else
    re_turns <- re_turns
  mod_el <- lm(returns_adv ~ re_turns)
  model_sum <- summary(mod_el)
  model_sum$coefficients[2, 3]
})  # end sapply

foo <- cbind(returns_adv, re_turns)
col_names <- colnames(foo)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse=" + "), sep="~"))
# perform regression
# returns_adv <- plogis(returns_adv, scale=-quantile(foo, 0.1))
ex_treme <- ((foo[, 2]>quantile(foo[, 2], 0.05)) & (foo[, 2]<quantile(foo[, 2], 0.95)))
mod_el <- lm(for_mula, data=foo[ex_treme])
model_sum <- summary(mod_el)
model_sum
plot(for_mula, data=foo[ex_treme])
abline(mod_el, lwd=2, col="red")

# perform optimization
# objective function equal to the strategy Sharpe ratio
# plus a penalty term for the weight constraint:
# sum(weight_s) == 1.
object_ive <- function(weight_s, indicator_s, re_turns) {
  sig_nal <- rutils::lag_it(indicator_s %*% weight_s)
  pnl_s <- sig_nal*re_turns
  se_lect <- ((pnl_s>quantile(pnl_s, 0.05)) & (pnl_s<quantile(pnl_s, 0.95)))
  pnl_s <- pnl_s[se_lect]
  -mean(pnl_s)/sd(pnl_s) + (sum(weight_s) - 1)^2
}  # end object_ive

# perform parameter optimization using function optim()
op_tim <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s,
                re_turns=re_turns)
weight_s <- op_tim$par
names(weight_s) <- colnames(indicator_s)
sig_nal <- xts(indicator_s %*% weight_s, order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# perform logistic regression
g_lm <- glm(for_mula, data=de_sign, family=binomial)
summary(g_lm)
glm_predict <- predict(g_lm, newdata=de_sign, type="response")
en_ter <- 0.58
fore_casts <- data.frame((glm_predict>en_ter), coredata(de_sign[, 1]))
colnames(fore_casts) <- c("lm_pred", "realized")
table(fore_casts)
sig_nal <- xts(de_sign %*% rota_tion, order.by=index(oh_lc))


# perform lda
l_da <- MASS::lda(for_mula, data=de_sign)
summary(l_da)
lda_predict <- predict(l_da, newdata=de_sign)
fore_casts <- data.frame(lda_predict$class, coredata(de_sign[, 1]))
colnames(fore_casts) <- c("lda_pred", "realized")
table(fore_casts)

# perform qda
q_da <- MASS::qda(for_mula, data=de_sign)
summary(q_da)
qda_predict <- predict(q_da, newdata=de_sign)
fore_casts <- data.frame(qda_predict$class, coredata(de_sign[, 1]))
colnames(fore_casts) <- c("qda_pred", "realized")
table(fore_casts)


# Calculate PCA of de_sign
pc_a <- prcomp(de_sign)
pc_a$sdev
pc_a$rotation
# lm
mod_el <- lm(returns_adv ~ pc_a$x - 1)
model_sum <- summary(mod_el)
model_sum$coefficients


# Curated PCs
rota_tion <- cbind(PC1=rep(0.2, 5),
                   PC2=c(-2, -1, 0, 1, 2),
                   PC3=c(-1, 0.5, 1, 0.5, -1))
pca_ts <- xts(de_sign %*% rota_tion, order.by=index(de_sign))

mod_el <- lm(returns_adv ~ pca_ts - 1)
model_sum <- summary(mod_el)
model_sum$coefficients


## Perform in-sample
in_sample <- 1:2000
# Define OHLC technical indicators
indic_in <- indicator_s[in_sample]
# Scale indic_in using sigmoid
indic_in <- lapply(1:NCOL(indic_in), function(col_umn) {
  x <- plogis(indic_in[, col_umn], scale=-quantile(indic_in[, col_umn], 0.01))
  (x - median(x))
})  # end lapply
indic_in <- rutils::do_call(cbind, indic_in)
design_in <- as.data.frame(cbind(returns_adv[in_sample], indic_in))
colnames(design_in)[1] <- "returns_adv"

# perform optimization
op_tim <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s[in_sample],
                re_turns=re_turns[in_sample])
weight_s <- op_tim$par
names(weight_s) <- colnames(indicator_s)
sig_nal <- xts(indicator_s %*% weight_s, order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# perform regression
mod_el <- lm(for_mula, data=design_in)
# mod_el <- lm(returns_adv[-ex_treme] ~ de_sign[-ex_treme, ])
model_sum <- summary(mod_el)
weight_s <- model_sum$coefficients[, 1][-1]

# or
pc_a <- prcomp(de_sign[in_sample, ])
pc_a$sdev
pc_a$rotation
mod_el <- lm(returns_adv[in_sample] ~ pc_a$x - 1)
model_sum <- summary(mod_el)
model_sum$coefficients

# or
mod_el <- lm(returns_adv[in_sample] ~ pca_ts[in_sample] - 1)
model_sum <- summary(mod_el)

# or
mod_el <- lm(returns_adv[in_sample] ~ de_sign[in_sample, ] - 1)
model_sum <- summary(mod_el)


weight_s <- model_sum$coefficients[, 1]
# weight_s <- weight_s[-1]
t_vals <- rep(TRUE, NROW(weight_s))
t_vals <- (abs(model_sum$coefficients[, 3]) > 2)
weight_s[!t_vals] <- 0


## Perform out-of-sample
out_sample <- 2001:NROW(oh_lc)
# Define OHLC technical indicators
indic_out <- indicator_s[out_sample, ]
# Scale indic_in using sigmoid
indic_out <- lapply(1:NCOL(indic_out), function(col_umn) {
  x <- plogis(indic_out[, col_umn], scale=-quantile(indicator_s[in_sample, col_umn], 0.01))
  (x - median(indicator_s[in_sample, col_umn]))
})  # end lapply
indic_out <- rutils::do_call(cbind, indic_out)
sig_nal <- xts(indic_out %*% weight_s, order.by=index(oh_lc[out_sample]))
# Simulate strategy
pnl_s <- cumsum(sig_nal*re_turns[out_sample])
colnames(pnl_s) <- "strategy"


# or
sig_nal <- xts(as.matrix(de_sign)[, t_vals] %*% weight_s[t_vals], order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)

# or
sig_nal <- xts(de_sign %*% rota_tion, order.by=index(oh_lc))
# sig_nal <- xts(de_sign %*% pc_a$rotation[, t_vals], order.by=index(oh_lc))
sig_nal <- xts(as.matrix(de_sign)[, -1] %*% weight_s, order.by=index(oh_lc))
sig_nal <- xts(sig_nal[, t_vals] %*% weight_s[t_vals], order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# Simulate strategy
pnl_s <- cumsum(sig_nal*re_turns)
colnames(pnl_s) <- "strategy"

# plot
library(dygraphs)
dygraphs::dygraph(cbind(clos_e, pnl_s), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))



# de_sign <- cbind(rets_lag2, z_scores[[3]], hu_rst, sharpe_rolling)
# colnames(de_sign) <- c("returns", "variance", "skew", "hurst")
end_p <- xts::endpoints(de_sign, "years")

## Apply rolling centering and scaling to the design matrix
# library(roll)
de_sign <- roll::roll_scale(data=de_sign, width=100*look_back, min_obs=1)
# remove NAs
de_sign[is.na(de_sign)] <- 0
sum(is.na(de_sign))

## perform regressions of future returns against different indicators

# Single indicator
returns_adv <- re_turns + close_high + close_low
mod_el <- lm(returns_adv ~ returns_adv)
summary(mod_el)

# three indicators - lower lows is most significant
mod_el <- lm(returns_adv ~ re_turns + close_high + close_low)
summary(mod_el)

# Single indicator
# lower lows indicator works well in bearish periods
returns_adv <- -re_turns - close_high + close_low
returns_adv <- sign(returns_adv)
mod_el <- lm(returns_adv ~ returns_adv)
summary(mod_el)

mo_del <- lm(rets_adv2 ~ de_sign)
summary(mo_del)
coef(summary(mo_del))
beta_s <- -coef(summary(mo_del))[-1, 1]

max_eigen <- 2
cov_mat <- cov(ex_cess)

# Calculate eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_values <- ei_gen$values
eigen_vec <- ei_gen$vectors

# Check for zero singular values
# Set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
not_zero <- (eigen_values > (to_l*eigen_values[1]))

# Calculate generalized inverse from eigen decomposition
eigen_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_values[not_zero])

# perform eigen decomposition and calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
# Calculate regularized inverse
in_verse <- eigen_vec[, 1:max_eigen] %*% (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# Calculate the maximum Sharpe ratio portfolio weights
# weight_s <- in_verse %*% colMeans(ex_cess)
# weight_s <- rep(mean(colMeans(ex_cess)), NCOL(ex_cess))
weight_s <- colMeans(ex_cess)
weight_s <- in_verse %*% weight_s
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))


# Simulate strategy
pnl_s <- cumsum(sig_nal*re_turns)
colnames(pnl_s) <- "strategy"




###############
### State space model and Kalman filter

## Simulate state space model

# Length of data
# len_gth <- NROW(end_p)
len_gth <- 100  # Number of time points
# n <- 5    # Number of observations at each time point
# p <- 2    # Number of covariates


# True parameter values

rho_v <- 2.0
rho_w <- 1.0
gg <- 1.0
hh <- 1.0

# Allocate state vector xx
xx <- numeric(len_gth)
# Transition equation for state vector under AR(1) process
set.seed(1121)
# xx[1] <- rnorm(1, sd=rho_w)
for (it in 2:len_gth) {
  xx[it] <- gg*xx[it-1] + rnorm(1, sd=rho_w)
}  # end for

# Measurement equation for measured vector
yy <- hh*xx + rnorm(len_gth, sd=rho_v)

# Plot
x11()
matplot(cbind(xx, yy), type="l", xlab="", ylab="", lty=1, col=1:2, lwd=2,
        main="State space model \nand Kalman filter")
legend("top", leg=c("state", "observed"), lty=1, lwd=6, col=1:2, inset=0.05)


## Apply Kalman filter

# process matrix
aa <- 1.0
# process variance
qq <- 1.0
# measurement variance
rr <- 1.0

# Allocate predicted vector zz
zz <- numeric(len_gth)
zz[1] <- aa*yy[1]
# Allocate process variance vector pp
pp <- numeric(len_gth)
pp[1] <- 1
# Allocate predicted variance vector ppp
ppp <- numeric(len_gth)
ppp[1] <- aa^2*pp[1] + qq
# Allocate Kalman gain vector kk
kk <- numeric(len_gth)
kk[1] <- ppp[1]*hh/(ppp[1]*hh^2+rr)

# Apply Kalman filter recursivelly
for (it in 2:len_gth) {
  # Prediction equations for predicted vector zz and predicted variance pp
  zz[it] <- aa*yy[it-1]
  ppp[it] <- aa^2 + qq
  # Correction (measurement) equations
  kk[it] <- ppp[it]*hh/(ppp[it]*hh^2+rr)
  zz[it] <- zz[it] + kk[it]*(yy[it] - hh*zz[it])
  pp[it] <- (1-kk[it]*hh)*ppp[it]
}  # end for


# Plot
x11()
matplot(cbind(xx, yy, zz), type="l", xlab="", ylab="", lty=1, col=1:3, lwd=2,
        main="State space model \nand Kalman filter")
legend("top", leg=c("state", "observed", "Kalman filter"), lty=1, lwd=6, col=1:3, inset=0.05)



set.seed(1121)
d_lm <- dlm::dlm(FF=hh, V=rho_v, GG=gg, W=rho_w, m0=0, C0=100)

a_r <- dlm::dlmModARMA(ar=gg, ma=0.0, sigma2=rho_w)

# Generate data
X <- array(rnorm(len_gth*n*p), c(n, p, len_gth))
X[, 1, ] <- 1


Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/kalman_filter.cpp")




# Calculate ETF prices
sym_bols <- colnames(rutils::etf_env$price_s)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
price_s <- rutils::etf_env$price_s[, sym_bols]
# Carry forward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- na.omit(price_s)
# Calculate simple ETF returns
re_turns <- rutils::diff_it(price_s)
# Calculate the daily excess returns
# risk_free is the daily risk-free rate
risk_free <- 0.03/260
ex_cess <- re_turns - risk_free


# Define monthly end_p without initial warmpup period
end_p <- rutils::calc_endpoints(re_turns, inter_val="months")
end_p <- end_p[end_p>50]
len_gth <- NROW(end_p)
# Define 12-month look_back interval and start_p over sliding window
look_back <- 12
start_p <- c(rep_len(1, look_back-1), end_p[1:(len_gth-look_back+1)])

# Define the shrinkage intensity
al_pha <- 0.5
max_eigen <- 3

# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(end_p),
                     function(i) {
                       # Subset the ex_cess returns
                       ex_cess <- ex_cess[start_p[i-1]:end_p[i-1], ]
                       ei_gen <- eigen(cov(ex_cess))
                       # Calculate regularized inverse of covariance matrix
                       max_eigen <- 3
                       eigen_vec <- ei_gen$vectors[, 1:max_eigen]
                       eigen_val <- ei_gen$values[1:max_eigen]
                       in_verse <- eigen_vec %*% (t(eigen_vec) / eigen_val)
                       # Apply shrinkage to the mean returns
                       col_means <- colMeans(ex_cess)
                       col_means <- ((1-al_pha)*col_means + al_pha*mean(col_means))
                       # Calculate weights using R
                       weight_s <- in_verse %*% col_means
                       weight_s <- weight_s/sum(weight_s)
                       # Subset the re_turns to out-of-sample returns
                       re_turns <- re_turns[(end_p[i-1]+1):end_p[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply

# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"


# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(end_p),
                     function(i) {
                       # Subset the ex_cess returns
                       ex_cess <- ex_cess[start_p[i-1]:end_p[i-1], ]
                       # Apply regularized inverse to mean of ex_cess
                       weight_s <- HighFreq::calc_weights(ex_cess, max_eigen, al_pha)
                       # Subset the re_turns to out-of-sample returns
                       re_turns <- re_turns[(end_p[i-1]+1):end_p[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply
# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"

indicator_s <- HighFreq::roll_portf(ex_cess, re_turns, start_p-1, end_p-1, max_eigen, al_pha)
indicator_s <- xts(indicator_s, index(re_turns))
colnames(indicator_s) <- "strat_rets"

# Compare RcppArmadillo with R
all.equal(strat_rets, indicator_s[index(strat_rets)])

# Plot dygraph
dygraphs::dygraph(cumsum(indicator_s),
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")



###############
### Benchmark eigen decomposition function in RcppArmadillo

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/calc_eigen.cpp")

ei_gen <- calc_eigen(scale(prices_ts, scale=FALSE))
mod_el <- prcomp(prices_ts)
all.equal(mod_el$sdev^2, drop(ei_gen$values))
all.equal(unname(mod_el$rotation), -ei_gen$vectors)

library(microbenchmark)
summary(microbenchmark(
  rcpp=calc_eigen(prices_ts),
  pure_r=prcomp(prices_ts),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### ARIMA simulation

library(rutils)

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/sim_arima.cpp")

co_eff <- -0.8
in_nov <- rnorm(100)

ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
for (i in 2:NROW(in_nov)) ari_ma[i] <- co_eff*ari_ma[i-1] + in_nov[i]
foo <- ari_ma

ari_ma <- filter(c(in_nov), filter=co_eff, method="recursive")
all.equal(as.numeric(ari_ma), foo)


# two co_eff
co_eff <- c(-0.8, 0.2)

# old
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
ari_ma[2] <- co_eff[1]*ari_ma[1] + in_nov[2]
for (i in 3:NROW(in_nov)) {
  ari_ma[i] <- co_eff[1]*ari_ma[i-1] + co_eff[2]*ari_ma[i-2] + in_nov[i]
}  # end for
foo <- ari_ma

# vectorized
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
for (i in 2:NROW(co_eff)) {
  ari_ma[i] <- co_eff[1:(i-1)] %*% ari_ma[(i-1):1] + in_nov[i]
}  # end for

for (i in (NROW(co_eff)+1):NROW(in_nov)) {
  ari_ma[i] <- co_eff %*% ari_ma[(i-1):(i-NROW(co_eff))] + in_nov[i]
}  # end for
foo <- ari_ma

ari_ma <- filter(in_nov, filter=co_eff, method="recursive")
all.equal(as.numeric(ari_ma), foo)


# vectorized vector of co_eff
co_eff <- c(-0.8, 0.2)
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
for (i in 2:NROW(co_eff)) {
  ari_ma[i] <- co_eff[1:(i-1)] %*% ari_ma[(i-1):1] + in_nov[i]
}  # end for

for (i in (NROW(co_eff)+1):NROW(in_nov)) {
  ari_ma[i] <- co_eff %*% ari_ma[(i-1):(i-NROW(co_eff))] + in_nov[i]
}  # end for
foo <- ari_ma

ari_ma <- filter(in_nov, filter=co_eff, method="recursive")
all.equal(as.numeric(ari_ma), foo)

ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(as.numeric(ari_ma), foo)

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::sim_arima(in_nov, rev(co_eff)),
  pure_r=filter(in_nov, filter=co_eff, method="recursive"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### Convolutions and filtering

# Rcpp::sourceCpp(file="C:/Develop/lecture_slides/assignments/roll_wsum.cpp")
library(HighFreq)

weight_s <- c(1, rep(1e-5, 10))

weight_s <- exp(-0.2*1:11)
weight_s <- weight_s/sum(weight_s)
vec_tor <- as.numeric(rutils::etf_env$VTI[, 6])
weight_ed <- HighFreq::roll_wsum(vec_tor=vec_tor, weight_s=rev(weight_s))
filter_ed <- filter(x=vec_tor, filter=weight_s, method="convolution", sides=1)

all.equal(as.numeric(vec_tor), as.numeric(weight_ed))

all.equal(as.numeric(filter_ed[-(1:10)]), as.numeric(weight_ed))


library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vec_tor=vec_tor, weight_s=weight_s),
  pure_r=filter(x=vec_tor, filter=weight_s, method="convolution", sides=1, circular=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


filter_ed <- roll_wsum_arma(vec_tor, rev(weight_s))
all.equal(as.numeric(filter_ed[-(1:10)]), as.numeric(weight_ed))

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vec_tor=vec_tor, weight_s=weight_s),
  arma=roll_wsum_arma(vec_tor, rev(weight_s)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### Stitching and Updating Data for S&P500 Constituents
# in tests already

library(rutils)

# load new data
load("C:/Develop/lecture_slides/data/sp500_2018.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500_env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_new are the new symbols
symbols_new <- ls(sp500_env)
# sp500_env_new is the new data
sp500_env_new <- sp500_env


# load old data
load("C:/Develop/lecture_slides/data/sp500_2017.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500_env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_old are the old symbols
symbols_old <- ls(sp500_env)
# sp500_env_old is the old data
sp500_env_old <- sp500_env
rm(sp500_env)

# find the new symbols that are not in the old symbols
is_in <- symbols_new %in% symbols_old
symbols_new[!is_in]

# find the old symbols that are in the new symbols
is_in <- symbols_old %in% symbols_new
sym_bols <- symbols_old[is_in]
# find the old symbols that are not in the new symbols
symbols_old[!is_in]

# Create a new environment to store the updated data
sp500_env <- new.env()
# Copy the old symbols that are also in the new symbols from sp500_env_old to sp500_env
# for (sym_bol in sym_bols) {
#   assign(sym_bol, get(sym_bol, envir=sp500_env_old), envir=sp500_env)
#   # sp500_env$sym_bol <- sp500_env_old$sym_bol
# }  # end for

# Stitch the old and new data and copy it into sp500_env
for (sym_bol in sym_bols) {
  # get old data
  old_data <- get(sym_bol, envir=sp500_env_old)
  end_date <- end(old_data)
  # get new data
  new_data <- get(sym_bol, envir=sp500_env_new)
  # Stitch the old and new data only if old is older
  if (start(old_data) < start(new_data)) {
    clos_e <- new_data[, 4]
    # diff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] - as.numeric(clos_e))
    # diff the Close prices
    clos_e <- rutils::diff_it(log(clos_e))
    # Calculate new extended Close prices
    new_close <- as.numeric(old_data[end_date, 4])*exp(cumsum(clos_e[index(new_data)>end_date]))
    # foo <- as.numeric(new_data[end_date, 4])*exp(cumsum(clos_e[index(new_data)>end_date]))
    # all.equal(new_data[index(new_data)>end_date, 4], foo)
    # Stitch the Close prices
    new_close <- rbind(old_data[, 4], new_close)
    # all.equal(NROW(index(new_close)), NROW(unique(index(new_close))))
    # new_data <- (new_data[, 1:3] + as.numeric(new_close))
    # undiff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] + as.numeric(new_close[index(new_data)]))
    new_data[, 4] <- new_close[index(new_data)]
    new_data[, 6] <- new_close[index(new_data)]
    # Stitch all the prices
    new_data <- rbind(old_data, new_data[index(new_data)>end_date])
  }  # end if
  # Copy the data
  assign(sym_bol, new_data, envir=sp500_env)
}  # end for

# verify that all symbols were stitched
all.equal(sym_bols, ls(sp500_env))
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500_env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))

save(sp500_env, file="C:/Develop/lecture_slides/data/sp500.RData")



###############
### sweep() for matrix multiplication

mat_rix1 <- matrix(rnorm(1e6), ncol=100)
vec_tor <- rnorm(1e2)
mat_rix2 <- diag(vec_tor)
pro_duct <- mat_rix1 %*% mat_rix2
pro_duct2 <- sweep(mat_rix1, 2, vec_tor, FUN="*")
all.equal(pro_duct, pro_duct2)

# sweep() is about 5 times faster
library(microbenchmark)
summary(microbenchmark(
  matrix_mult=(mat_rix1 %*% mat_rix2),
  sweep=sweep(mat_rix1, 2, vec_tor, FUN="*"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### Portfolio optimization with constraints

# This is an objective function equal to the portfolio
# variance plus a penalty term for the weight constraint:
# sum(weight_s) == 1.

object_ive <- function(weight_s, re_turns) {
  var(re_turns %*% weight_s) +
    (sum(weight_s) - 1)^2
}  # end object_ive

# Perform portfolio optimization with the same two weight
# constraints as in p.1
# You must use function optim().

op_tim <- optim(par=rep(1.0, NCOL(re_turns)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(re_turns)),
                lower=rep(-1, NCOL(re_turns)),
                re_turns=re_turns)

weight_s <- op_tim$par

var(re_turns %*% weight_s)


# You should get output similar to the following:
# > op_tim$par


object_ive <- function(weight_s, re_turns, conf_level, portfolio_sub) {
  # pnl_s <- re_turns %*% weight_s
  # var(pnl_s) +
  t(weight_s) %*% cov_mat %*% weight_s +
    1000*(sum(weight_s) - 1)^2 +
    1000*(sum(weight_s*portfolio_sub[-1]) - portfolio_sub[1])^2
}  # end object_ive



###############
### plot multiple dygraphs in the same RStudio window

# Create the time series
temperature <- ts(frequency = 12, start = c(1980, 1),
                  data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5,
                           25.2, 26.5, 23.3, 18.3, 13.9, 9.6))
rainfall <- ts(frequency = 12, start = c(1980, 1),
               data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0,
                        135.6, 148.5, 216.4, 194.1, 95.6, 54.4))

# Create a list of dygraphs objects
library(dygraphs)
dy_graph <- list(
  dygraphs::dygraph(temperature, group="temp_rain", main="temperature", width=400, height=200),
  dygraphs::dygraph(rainfall, group="temp_rain", main="rainfall", width=400, height=200)
)  # end list

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dy_graph))



###############
### plot multiple dygraphs in the same RStudio window

# load packages
library(quantmod)
library(dygraphs)

# download time series into an environment
sym_bols <- c("VTI", "EEM")
data_env <- new.env()
quantmod::getSymbols(sym_bols, from="2017-01-01", env=data_env)

dygraphs::dygraph(data_env$EEM[, 1:4]) %>% dygraphs::dyCandlestick()

# Create a list of dygraphs objects in a loop
dy_graph <- eapply(data_env, function(x_ts) {
  dygraphs::dygraph(x_ts[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(x_ts)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
})  # end eapply

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dy_graph))

## perform same plotting as above using pipes syntax
# Create a list of dygraphs objects in a loop
eapply(data_env, function(x_ts) {
  dygraphs::dygraph(x_ts[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(x_ts)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
}) %>% # end eapply
  # render the dygraphs objects using htmltools
  htmltools::tagList() %>% htmltools::browsable()




###############
### dygraph plot with highlighting of specific points

library(xts)
library(dygraphs)
# Convert numeric time index of ldeaths into class 'Date' (approximately)
date_s <- as.Date(365*(zoo::index(ldeaths)-1970))
# Convert time index from class 'Date' to 'POSIXct'
date_s <- as.POSIXct.Date(date_s)

# Convert ldeaths into xts time series
l_deaths <- xts::xts(as.numeric(ldeaths), order.by=date_s)
# Calculate number of years
n_years <- NROW(date_s)/12
# Calculate the January dates
jan_dates <- date_s[1 + 12*(0:(n_years - 1))]
# or
# jan_dates <- date_s[which(months(date_s)=="January")]
# or
# jan_dates <- date_s[grep("Jan", months(date_s), ignore.case=TRUE)]
# Calculate the July dates
jul_dates <- date_s[7 + 12*(0:(n_years - 1))]
# or
# jul_dates <- date_s[which(months(date_s)=="July")]

# Create dygraph object
dy_graph <- dygraphs::dygraph(l_deaths, main="Dygraph of ldeaths with Annotations") %>%
  dyHighlight(highlightCircleSize=5)

# Add annotations for the January and July dates to dygraph object
for (i in 1:NROW(jan_dates)) {
  dy_graph <- dygraphs::dyAnnotation(dy_graph, x=jan_dates[i], text="Jan")
}  # end for
for (i in 1:NROW(jul_dates)) {
  dy_graph <- dygraphs::dyAnnotation(dy_graph, x=jul_dates[i], text="Jul")
}  # end for

# plot dygraph object
dy_graph



###############
### exponentiation operator is a function:

'^'(3, 2)
"^"(3, 2)



###############
### Split random xts time series into daily list and rbind it back into the original xts

x_ts <- xts(x=rnorm(100), order.by=(Sys.time()-3600*(1:100)))
# Split time series into daily list
li_st <- xts::split.xts(x_ts, "days")
# rbind the list back into a time series and compare with the original
all.equal(x_ts, rutils::do_call(rbind, li_st))



###############
### Code to check for duplicate dates

# Create xts with duplicate dates
x <- xts(rnorm(5), order.by=c(Sys.Date() + 1:4, Sys.Date() + 2))
diff(index(x))

rutils::diff_it(index(x))

as.POSIXct(make.index.unique(.index(x)), origin="1970-01-01")


###############
### Example of an apply() error.
# When apply() parses a data frame with a Boolean column by rows,
# then it adds a leading space to TRUE.
# This may be because FALSE has 5 letters, while TRUE has only 4 letters.
# Below is example of how it works.

# Create data frame with two columns: Boolean column and character column
data_frame <- data.frame(
  new_instr=(rnorm(10)>0),
  instr_name=paste0("instrument", 1:10),
  stringsAsFactors=FALSE)
# perform apply() loop - requires gsub() to remove leading space in TRUE
apply(data_frame, MARGIN=1, function(instru_ment) {
  cat("processing instrument:", instru_ment["instr_name"], "\n")
  cat(instru_ment["new_instr"], "\n")
  if (as.logical(gsub(" ", "", instru_ment["new_instr"])))
    # below doesn't work
    # if (as.logical(instru_ment["new_instr"]))
    1
  else
    0
})  # end apply


# sapply() loop doesn't introduce leading space to TRUE
sapply(1:NROW(data_frame), function(i_ter) {
  instru_ment <- unlist(data_frame[i_ter, ])
  cat("processing instrument:", instru_ment["instr_name"], "\n")
  cat(instru_ment["new_instr"], "\n")
  # if (as.logical(gsub(" ", "", instru_ment["new_instr"])))
    if (as.logical(instru_ment["new_instr"]))
    1
  else
    0
})  # end sapply



###############
### Brownian bridge puzzle: given deck of 52 cards, every time you randomly choose a red card you're account increases by $1, but if you choose black card you're account decreases by -$1
# At any point you can choose to continue playing, or to stop and keep your net wins.
# The optimal strategy is to stop playing if the current net wins are greater than the expected value of wins from continuing to play.
# Calculate the expected value of the optimal strategy, assuming you start with zero in your account.

# stra_tegy <- matrix(nrow=4, ncol=4)
n_pos <- 26
stra_tegy <- outer(n_pos:0, n_pos:0, function(positive, negative)
  (negative - positive))
stra_tegy[, n_pos+1] <- 0
stra_tegy[n_pos+1, ] <- n_pos:0

prob_s <- outer(n_pos:0, n_pos:0, function(positive, negative)
  positive/(positive + negative))
prob_s[, n_pos+1] <- 0

for (i in n_pos:1) {
  for (j in n_pos:1)
    stra_tegy[i, j] <- max(stra_tegy[i, j],
                           prob_s[i, j]*stra_tegy[i+1, j] + (1-prob_s[i, j])*stra_tegy[i, j+1])
  for (j in n_pos:1)
    stra_tegy[j, i] <- max(stra_tegy[j, i],
                           prob_s[j, i]*stra_tegy[j+1, i] + (1-prob_s[j, i])*stra_tegy[j, i+1])
}  # end for

stra_tegy[1, 1]

stra_tegy <- function(cash, positive, negative) {
  # cat(paste("args=", cash, positive, negative, "\n"))
  pro_b <- positive/(positive + negative)
  if (positive==0)
    max(cash, 0)
  else if (negative==0)
    max(cash + positive, 0)
  else
    max(cash,
        pro_b*stra_tegy(cash+1, positive-1, negative) +
          (1-pro_b)*stra_tegy(cash-1, positive, negative-1))
}  #end stra_tegy

# stra_tegy(0, 26, 26)
stra_tegy(0, 3, 3)
stra_tegy(3, 0, 3)
stra_tegy(2, 1, 0)
stra_tegy(-3, 3, 0)
stra_tegy(0, 3, 3)

sapply(3:1, function(positive, negative)
  stra_tegy(negative-positive, positive, negative),
  negative=3:1)



###############
### Simulating several managers, with only one manager with skill,
# and try to determine how much data is needed to determine which manager has skill

library(rutils)
num_managers <- 11
# Daily probability as function of Sharpe ratio
sharpe_ratio <- 0.4
pro_b <- (sharpe_ratio/sqrt(250)+1)/2
# Adjust probability to account for multiple managers
p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
mean_s <- c(2*p1-1, rep(2*p2-1, num_managers-1))

# length of Brownian motion
n_row <- 5000

# Simulate Brownian motion
vol_at <- 0.01
set.seed(1121)  # reset random number generator
re_turns <- sapply(mean_s, rnorm, n=n_row, sd=vol_at)
re_turns <- apply(re_turns, 2, cumsum)
# apply(re_turns, 2, mean)
# plot.zoo(re_turns, plot.type="single")

# length of lookback window
look_back <- 100
# define end_p with beginning stub
num_agg <- n_row %/% look_back
end_p <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
len_gth <- NROW(end_p)
# start_p are single-period lag of end_p
start_p <- end_p[c(1, 1:(len_gth-1))] + 1
fix_points <- (start_p > end_p)
start_p[fix_points] <- end_p[fix_points]

# total re_turns aggregated over non-overlapping windows
agg_rets <- apply(re_turns, 2, function(x) (x[end_p]-x[start_p]))


# Switch to best manager with biggest total re_turns
be_st <- apply(agg_rets, 1, which.max)
be_st <- rutils::lag_it(be_st)
be_st[1] <- 1
# be_st <- c(rep(1, NROW(end_p)-NROW(be_st)), be_st)
pnl_s <- agg_rets[cbind(1:NROW(agg_rets), be_st)]
# pnl_s <- lapply(seq_along(end_p), function(date_s) {
#   re_turns[start_p[date_s]:end_p[date_s], be_st[date_s]]
# })  # end lapply
# pnl_s <- rutils::do_call(c, pnl_s)
plot.zoo(cumsum(pnl_s))


## cum_pnl for multi-manager strategy
cum_pnl <- function(sharpe_ratio, re_turns=NULL, mean_s=NULL, num_managers, n_row, look_back, vol_at=0.01) {
  # Simulate Brownian motion
  if(is.null(re_turns)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- c(2*p1-1, rep(2*p2-1, num_managers-1))
    set.seed(1121)  # reset random number generator
    re_turns <- sapply(mean_s, rnorm, n=n_row, sd=vol_at)
    re_turns <- apply(re_turns, 2, cumsum)
  } else {
    num_managers <- NCOL(re_turns)
    n_row <- NROW(re_turns)
  }  # end if

  # define end_p with beginning stub
  num_agg <- n_row %/% look_back
  end_p <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  len_gth <- NROW(end_p)
  # start_p are single-period lag of end_p
  start_p <- end_p[c(1, 1:(len_gth-1))] + 1
  fix_points <- (start_p > end_p)
  start_p[fix_points] <- end_p[fix_points]

  # total re_turns over non-overlapping windows
  agg_rets <- apply(re_turns, 2, function(x) (x[end_p]-x[start_p]))

  # Switch to best manager with biggest total re_turns
  be_st <- apply(agg_rets, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  be_st <- c(rep(1, NROW(end_p)-NROW(be_st)), be_st)
  pnl_s <- lapply(seq_along(end_p), function(date_s) {
    re_turns[start_p[date_s]:end_p[date_s], be_st[date_s]]
  })  # end lapply
  # return total expected pnl
  pnl_s <- rutils::do_call(c, pnl_s)
  sum(pnl_s)
}  # end cum_pnl

cum_pnl(re_turns=re_turns, look_back=100)

cum_pnl(sharpe_ratio=0.4, num_managers=11, look_back=100, n_row=5000)



## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, n_row=NULL, sharpe_ratio=NULL, re_turns=NULL, mean_s=NULL, num_managers=NULL, vol_at=0.01) {
  # Calculate drifts
  if(is.null(mean_s)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- vol_at*look_back*c(2*p1-1, rep(2*p2-1, num_managers-1))
  } else {
    num_managers <- NROW(mean_s)
  }  # end if
  # Simulate Brownian motion
  if(is.null(re_turns)) {
    # set.seed(1121)  # reset random number generator
    num_agg <- n_row %/% look_back
    re_turns <- sapply(mean_s, rnorm, n=num_agg, sd=sqrt(look_back)*vol_at)
  } else {
    num_managers <- NCOL(re_turns)
    n_row <- NROW(re_turns)
  }  # end if

  # Switch to best manager with biggest total re_turns
  be_st <- apply(re_turns, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnl_s <- re_turns[cbind(1:NROW(re_turns), be_st)]
  sum(re_turns[cbind(1:NROW(re_turns), be_st)])
}  # end cum_pnl



## cum_pnl for multi-manager strategy (simplest version)
cum_pnl <- function(look_back, n_row, sharpe_ratio=NULL, re_turns=NULL, mean_s=NULL, num_managers=NULL, vol_at=0.01) {
  # Calculate drifts
  if(is.null(mean_s)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- vol_at*look_back*c(2*p1-1, rep(2*p2-1, num_managers-1))
  } else {
    num_managers <- NROW(mean_s)
  }  # end if

  # Calculate probability of selecting the best manager
  pro_b <- integrate(function(x, ...)
    dnorm(x, mean=mean_s[1], ...)*pnorm(x, mean=mean_s[2], ...)^(num_managers-1),
            low=-3.0, up=3.0,
            sd=sqrt(look_back)*vol_at)$value
  # return total expected pnl
  num_agg <- n_row %/% look_back
  num_agg*(pro_b*mean_s[1] + (1-pro_b)*mean_s[2])
}  # end cum_pnl

cum_pnl(sharpe_ratio=0.4, num_managers=11, mean_s=mean_s, look_back=100, n_row=5000)
cum_pnl(sharpe_ratio=0.4, num_managers=11, look_back=100, n_row=5000)
cum_pnl(look_back=100, sharpe_ratio=0.4, num_managers=11, n_row=5000)

# Calculate average pnl
foo <- mean(sapply(1:10000, function(x)
  cum_pnl(mean_s=mean_s, look_back=100, n_row=5000)))

foo <- mean(sapply(1:10000, function(x)
  cum_pnl(look_back=100, sharpe_ratio=0.4, num_managers=11, n_row=500000)))

# perform loop over lookback windows
look_backs <- 100*(1:20)
foo <- sapply(look_backs, cum_pnl,
              sharpe_ratio=0.4, num_managers=11, n_row=50000)
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(cumsum(pnl_s), t="l")

# perform loop over number of managers
num_managers <- 2*(1:50)
foo <- sapply(num_managers, cum_pnl,
              re_turns=NULL, sharpe_ratio=0.4, look_back=100, n_row=50000, mean_s=NULL, vol_at=0.01)
foo <- cbind(num_managers, foo)
plot(foo, t="l")



###############
### Simulation of asset returns, with a time-dependent drift (skill) plus a random noise.

# define daily volatility: daily prices change by vol_at units
vol_at <- 0.01
n_row <- 50000
num_managers <- 3
# rate of drift (skill) change
ra_te <- 2*pi
# Daily probability as function of Sharpe ratio
sharpe_ratio <- 0.4
pro_b <- (sharpe_ratio/sqrt(250)+1)/2
# Adjust probability to account for two managers
pro_b <- 0.5 + (pro_b-0.5)/2
# define growth rate
mea_n <- vol_at*(2*pro_b-1)
# time-dependent drift (skill)
# dri_ft <- 0.01*sin(ra_te*(1:n_row)/n_row)
# dri_ft <- rutils::do_call(c, lapply(1:num_managers, function(x) (dri_ft + 2*pi*x/num_managers)))
dri_ft <- sapply(1:num_managers, function(x)
  mea_n*sin(ra_te*(1:n_row)/n_row + 2*pi*x/num_managers))

# Simulate multiple price paths

# re_turns <- xts(vol_at*rnorm(n_row) + dri_ft - vol_at^2/2,
#                 order.by=seq.Date(Sys.Date()-n_row+1, Sys.Date(), by=1))
# chart_Series(x=re_turns, name="Multiple price paths")

set.seed(1121)  # reset random number generator
re_turns <- matrix(vol_at*rnorm(num_managers*n_row) - vol_at^2/2, nc=num_managers) + dri_ft
# re_turns <- exp(matrixStats::colCumsums(re_turns))
# Create zoo time series
# re_turns <- xts(re_turns, order.by=seq.Date(Sys.Date()-NROW(re_turns)+1, Sys.Date(), by=1))
# plot zoo time series
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(re_turns))
# col_ors <- col_ors[order(order(re_turns[NROW(re_turns), ]))]
par(mfrow=c(2, 2))
par(mar=c(3, 1, 1, 1), oma=c(1, 1, 1, 1))
plot.zoo(dri_ft, main="time-dependent growth rates", lwd=3, xlab=NA, ylab=NA, plot.type="single", col=col_ors)
plot.zoo(re_turns, main="simulated returns", xlab=NA, ylab=NA, plot.type="single", col=col_ors)
plot.zoo(apply(re_turns, 2, cumsum),
         main="simulated prices", xlab=NA, ylab=NA, plot.type="single", col=col_ors)
# plot_theme <- chart_theme()
# plot_theme$col$line.col <- col_ors
# chart_Series(re_turns, theme=plot_theme, name="Multiple price paths")


## Calculate pnl over lookback window
# Calculate cumulative returns
pnl_s <- apply(re_turns, 2, cumsum)
# length of lookback window
look_back <- 100
# define end_p with beginning stub
num_agg <- n_row %/% look_back
end_p <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
len_gth <- NROW(end_p)
# start_p are single-period lag of end_p
start_p <- end_p[c(1, 1:(len_gth-1))] + 1
fix_points <- (start_p > end_p)
start_p[fix_points] <- end_p[fix_points]

# total re_turns aggregated over non-overlapping windows
agg_rets <- apply(pnl_s, 2, function(x) (x[end_p]-x[start_p]))

# Switch to best manager with biggest total re_turns
be_st <- apply(agg_rets, 1, which.max)
be_st <- rutils::lag_it(be_st)
be_st[1] <- 1
# be_st <- c(rep(1, NROW(end_p)-NROW(be_st)), be_st)
pnl_s <- agg_rets[cbind(1:NROW(agg_rets), be_st)]
plot.zoo(cumsum(pnl_s))


## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, re_turns) {
  n_row <- NROW(re_turns)
  # define end_p with beginning stub
  num_agg <- n_row %/% look_back
  end_p <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  len_gth <- NROW(end_p)
  # start_p are single-period lag of end_p
  start_p <- end_p[c(1, 1:(len_gth-1))] + 1
  fix_points <- (start_p > end_p)
  start_p[fix_points] <- end_p[fix_points]
  # total re_turns aggregated over non-overlapping windows
  re_turns <- apply(re_turns, 2, function(x) (x[end_p]-x[start_p]))
  # Switch to best manager with biggest total re_turns
  be_st <- apply(re_turns, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnl_s <- re_turns[cbind(1:NROW(re_turns), be_st)]
  sum(re_turns[cbind(1:NROW(re_turns), be_st)])
}  # end cum_pnl

cum_pnl(look_back=100, re_turns=pnl_s)


## cum_pnl for trend-following multi-manager strategy (without end_p)
cum_pnl <- function(look_back, re_turns, pnl_s) {
  # n_row <- NROW(re_turns)
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(pnl_s, 2, rutils::diff_it, lag=look_back)
  # Switch to best manager with biggest total re_turns
  be_st <- apply(agg_rets, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnl_s <- re_turns[cbind(1:NROW(re_turns), be_st)]
  sum(re_turns[cbind(1:NROW(re_turns), be_st)])
}  # end cum_pnl

# Calculate cumulative returns
pnl_s <- apply(re_turns, 2, cumsum)
cum_pnl(look_back=100, re_turns=re_turns, pnl_s=pnl_s)


## perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
pnl_s <- sapply(look_backs, cum_pnl, se_lect=1, re_turns=re_turns, pnl_s=pnl_s)
pnl_s <- cbind(look_backs, pnl_s)
plot(pnl_s, t="l")
# plot(cumsum(pnl_s), t="l")


## pre-calculate row order indices for a vector of look_backs
# perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
order_stats <- lapply(look_backs, function(look_back) {
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(pnl_s, 2, rutils::diff_it, lag=look_back)
  or_der <- t(apply(agg_rets, 1, order))
  or_der <- rutils::lag_it(or_der)
  or_der[1, ] <- 1
  or_der
})  # end lapply
names(order_stats) <- look_backs


## cum_pnl for long-short multi-manager strategy (without end_p)
cum_pnl <- function(select_best=NULL, select_worst=NULL, re_turns, or_der) {
  n_row <- NROW(re_turns)
  if(!is.null(select_best)) {
    n_col <- NCOL(re_turns)
    be_st <- or_der[, (n_col-select_best+1):n_col]
    be_st <- cbind(1:n_row, be_st)
  } else {
    be_st <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    wor_st <- or_der[, 1:select_worst]
    wor_st <- cbind(1:n_row, wor_st)
  } else {
    wor_st <- NULL
  }  # end if
  # return total expected pnl
  # pnl_s <- re_turns[be_st]-re_turns[wor_st]
  sum(re_turns[be_st])/select_best-sum(re_turns[wor_st])/(if(is.null(select_worst)) 1)
}  # end cum_pnl

# Calculate pnl for long-short multi-manager strategy
cum_pnl(select_best=1, select_worst=1, re_turns=re_turns, or_der=order_stats[[5]])


## perform loop over lookback windows
pnl_s <- sapply(order_stats, cum_pnl, select_best=1, select_worst=1, re_turns=re_turns)
pnl_s <- cbind(look_backs, pnl_s)
plot(pnl_s, t="l")
# plot(cumsum(pnl_s), t="l")


num_managers <- 5
dri_ft <- sapply(1:num_managers, function(x)
  mea_n*sin(ra_te*(1:n_row)/n_row + 2*pi*x/num_managers))
set.seed(1121)  # reset random number generator
re_turns <- matrix(vol_at*rnorm(num_managers*n_row) - vol_at^2/2, nc=num_managers) + dri_ft
# Calculate cumulative returns
pnl_s <- apply(re_turns, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
look_backs <- 20*(1:50)
order_stats <- lapply(look_backs, function(look_back) {
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(pnl_s, 2, rutils::diff_it, lag=look_back)
  or_der <- t(apply(agg_rets, 1, order))
  or_der <- rutils::lag_it(or_der)
  or_der[1, ] <- 1
  or_der
})  # end lapply
names(order_stats) <- look_backs

## cum_pnl for long-short multi-manager strategy (without end_p)
cum_pnl <- function(select_best=NULL, select_worst=NULL, re_turns, or_der) {
  n_row <- NROW(re_turns)
  if(!is.null(select_best)) {
    n_col <- NCOL(re_turns)
    be_st <- or_der[, (n_col-select_best+1):n_col]
    be_st <- cbind(1:n_row, be_st)
  } else {
    be_st <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    wor_st <- or_der[, 1:select_worst]
    wor_st <- cbind(1:n_row, wor_st)
  } else {
    wor_st <- NULL
  }  # end if
  # return total expected pnl
  # pnl_s <- re_turns[be_st]-re_turns[wor_st]
  sum(re_turns[be_st])-sum(re_turns[wor_st])
}  # end cum_pnl

# Calculate pnl for long-short multi-manager strategy
# cum_pnl(select_best=1, select_worst=1, re_turns=re_turns, or_der=order_stats[[5]])

# perform loop over lookback windows
pnl_s <- sapply(order_stats, cum_pnl, select_best=1, select_worst=NULL, re_turns=re_turns)
pnl_s <- cbind(look_backs, pnl_s)
# par(mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1))
# plot(pnl_s, t="l", main="Trend-following PnL, as function of lookback window")


## double the dri_ft
set.seed(1121)  # reset random number generator
re_turns <- matrix(vol_at*rnorm(num_managers*n_row) - vol_at^2/2, nc=num_managers) + 2*dri_ft
# Calculate cumulative returns
pnl_s <- apply(re_turns, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
order_stats_2x <- lapply(look_backs, function(look_back) {
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(pnl_s, 2, rutils::diff_it, lag=look_back)
  or_der <- t(apply(agg_rets, 1, order))
  or_der <- rutils::lag_it(or_der)
  or_der[1, ] <- 1
  or_der
})  # end lapply
names(order_stats_2x) <- look_backs

plot.zoo(cbind(pnl_s[, 2], pnls_2x), main="Long-short Ensemble PnL, as function of lookback window",
         lwd=2, xaxt="n", xlab="lookback windows", ylab="PnL", plot.type="single", col=c("black", "red"))
# Add x-axis
axis(1, seq_along(look_backs), look_backs)
# Add legend
legend(x="top", legend=paste0("SR=", c(0.4, 0.8)),
       inset=0.0, cex=0.8, bg="white",
       lwd=6, lty=c(1, 1), col=c("black", "red"))




## parallel version with loops - much slower and more complicated
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(num_cores-1)

foo <- sapply(look_backs, function(look_back) {
  # define end_p with beginning stub
  num_agg <- n_row %/% look_back
  end_p <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  len_gth <- NROW(end_p)
  # start_p are single-period lag of end_p
  start_p <- end_p[c(1, 1:(len_gth-1))] + 1
  # redefine end_p
  end_p <- cbind(start_p, end_p)

  # perform parallel loop over re_turns
  clusterExport(clus_ter, varlist=c("len_gth", "end_p"))
  sharpe_ratios <- parApply(clus_ter, MARGIN=2, re_turns, function(re_turns) {
    sapply(2:len_gth, function(date_s) {
      x_ts <- re_turns[end_p[date_s, 1]:end_p[date_s, 2]]
      # Calculate annualized Sharpe ratio of returns
      sum(x_ts)/sd(x_ts)
    })  # end sapply
  })  # end parApply

  # sharpe_ratios <- rutils::do_call(cbind, sharpe_ratios)
  sharpe_ratios[which(is.na(sharpe_ratios), arr.ind=TRUE)] <- 1

  # Calculate dispersion of SRs
  # c(by_strategy=mean(apply(sharpe_ratios, 1, sd)),
  #   by_period=mean(apply(sharpe_ratios, 2, sd)))
  # diff_sr <- apply(sharpe_ratios, 2, rutils::diff_it) / sharpe_ratios
  # mean(abs(diff_sr))
  # c(by_strategy=mean(apply(sharpe_ratios, 1, sd)),
  #   by_period=mean(apply(diff_sr, 1, sd)))
  cum_pnl(sharpe_ratios, re_turns, end_p)
})  # end sapply

foo <- t(foo)
dim(foo)
foo
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(foo[, 1]/foo[, 2], t="l")

## end perform loop over lookback windows




###############
### portfolio optimization using quadratic solver

load("C:/Develop/data/etf_data.RData")
ls(etf_env)
dim(etf_env$re_turns)
colnames(etf_env$re_turns)



## perform standard calibration over oh_lc interval
op_tim <- optim(par=rep(0.5, 2*NCOL(de_sign)),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=rep(2, 2*NCOL(de_sign)),
                lower=rep(-2, 2*NCOL(de_sign)),
                de_sign=de_sign[date_s],
                re_turns=returns_running[date_s],
                lamb_da=lamb_da)

beta_s <- op_tim$par
names(beta_s) <- c(paste0(colnames(de_sign), "_long"), paste0(colnames(de_sign), "_short"))


## cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(beta_s, la_g=15, de_sign=de_sign, re_turns=returns_running, lamb_da=0) {
  n_col <- NCOL(de_sign)
  position_s <- rep.int(NA, NROW(de_sign))
  position_s[1] <- 0
  # buy signal
  bu_y <- (de_sign %*% beta_s[1:n_col] < -1)
  position_s[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lag_it(bu_y, lag=la_g))
  # Sell signal
  position_s[se_ll] <- -1.0
  position_s[bu_y] <- 1.0
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s <- c(0, position_s[-NROW(position_s)])
  # pnl_s <- position_s*re_turns
  # be_ta <- (sum(pnl_s*re_turns) - sum(pnl_s)*sum(re_turns)) / (sum(pnl_s*pnl_s) - sum(pnl_s)^2 )
  # -(exp(sum(pnl_s) - be_ta*sum(re_turns)) - 1)
  # -(exp(sum(position_s*re_turns))-1) # / (sum(abs(rutils::diff_it(position_s))) / 2/ 1e5) / abs(sum(position_s>0) - sum(position_s<0))
  -((exp(sum(position_s*re_turns))-1) - lamb_da*sum(abs(beta_s)))
}  # end cum_pnl

cum_pnl(beta_s=beta_s, de_sign=de_sign[date_s], re_turns=returns_running[date_s])

# perform calibration over oh_lc interval
op_tim <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, NCOL(de_sign)),
                           lower=rep(-2, NCOL(de_sign)),
                           de_sign=de_sign[date_s],
                           re_turns=returns_running[date_s],
                           lamb_da=lamb_da,
                           control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))


beta_s <- op_tim$optim$bestmem
names(beta_s) <- colnames(de_sign)
# names(beta_s) <- colnames(de_sign)
op_tim$optim$bestval
cum_pnl(beta_s, de_sign=de_sign[date_s])


bu_y <- (de_sign %*% beta_s[1:n_col] < -1)

cum_pnl <- function(inter_val) {
  position_s <- rep.int(NA, NROW(de_sign))
  position_s[1] <- 0
  position_s[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lag_it(bu_y, lag=inter_val))
  position_s[se_ll] <- -1.0
  position_s[bu_y] <- 1.0
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s <- c(0, position_s[-NROW(position_s)])
  exp(sum((position_s*returns_running)))-1
}  # end cum_pnl

cum_pnl(200)
sapply(20*(1:30), cum_pnl)



###############
### Convert correlation matrix into distance object
dis_tance <- xts::.index(vol_spikes)
dis_tance <- abs(outer(X=dis_tance, Y=dis_tance, FUN="-"))
# dis_tance <- rutils::diff_it(xts::.index(vol_spikes))
dis_tance <- as.dist(dis_tance)
# Perform hierarchical clustering analysis
clus_ter <- hclust(dis_tance)
plot(clus_ter, ann=FALSE, xlab="", ylab="")
title("clustering of vol_spikes", line=0.0)
foo <- cutree(clus_ter, h=2000)
# foo <- cutree(clus_ter, k=100)
NROW(vol_spikes)
NROW(foo)
NROW(unique(foo))
tail(foo)
tail(vol_spikes)
bar <- match(index(vol_spikes), index(vari_ance))
tail(bar)



hc <- hclust(dist(USArrests))
plot(hc)
cutree(hc, k=5)
cutree(hc, h=50)

returns_future <- rutils::roll_sum(returns_running, look_back=5)
returns_future <- rutils::lag_xts(returns_running, lag=-5)
colnames(returns_future) <- "returns_future"
foo <- lm(returns_future["2008"] ~ de_sign["2008"] - 1)
summary(foo)


##

16*sd(rutils::etf_env$re_turns[, "VTI"])
sqrt(250)
250/5

# Summary: Create a functional which aggregates
# Asset returns over lookback and look-forward
# intervals.


# define functional

# 1. (20pts) Create a functional called roll_agg(),

# Should perform only a single


###############
###
# 4. (20pts) Create a scatterplot of returns and forward returns
# Create a scatterplot of alphas for "2008" and "2009",
# and add labels with ETF names,
# use columns of "alphas_capm" and functions plot() and text(),

dim(fwd_rets)
dim(pnl_s)

foo <- na.omit(merge(fwd_rets[, 5], pnl_s[, 5]))
colnames(foo) <- c("forward_returns", "past_returns")
foo <- as.data.frame(foo)
head(foo)
dim(foo)

x11()
# perform regression
reg_formula <- paste(colnames(foo), collapse=" ~ ")
mo_del <- lm(reg_formula, data=foo)
summary(mo_del)
# plot scatterplot using formula
plot(foo[, 2], foo[, 1], xlab="past returns", ylab="forward returns")
# plot(foo)
title(main="Simple Regression", line=-1)
# Add regression line
abline(mo_del, lwd=2, col="red")


# Select weight_s proportional to pnl_s
dim(pnl_s)
weight_s <- coredata(pnl_s[index(fwd_rets)])
weight_s <- weight_s/sqrt(rowSums(weight_s^2))

# bar <- matrixStats::rowMaxs(weight_s)
bar <- coredata(fwd_rets)
dim(bar)

# Select best and worst models in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, function(x) {which.min(x)})
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)

back_test <- rowSums(weight_s*bar)
x11()
plot(cumsum(back_test), t="l")

# back_test <- t(weight_s) %*% bar
back_test <- rowSums(weight_s*bar)
back_test <- xts(back_test, order.by=index(fwd_rets))
x11()
chart_Series(x=cumsum(back_test), name="Back-test of EWMA strategies")

plot(cumsum(back_test), t="l")
NROW(back_test)


#########

# define lookback windows


# Create a functional for performing rolling
# aggregations over overlapping intervals.
# Apply the functional to roll the function simu_ewma()
# over overlapping 12-month intervals in the past.

# 1. (20pts) Create a functional called roll_agg(),
# which should accept four arguments:
#  x_ts - an xts series containing one or more columns of data,
#  end_p - integer vector of end points,
#  look_back - number of intervals in the lookback window,
#  FUN - name of of an aggregation function,
#  "..." - optional dots arguments to FUN.

# The functional roll_agg() should perform an lapply()
# loop over end_p, subset the x_ts series, and pass
# it to FUN, together with the dots "..." argument.
# roll_agg() should return an xts series, with each
# row equal to the vector returned by FUN.
# hint: You can adapt code from the slide:
# Performing Aggregations Over Overlapping Intervals.

roll_agg <- function(x_ts, end_p, look_back, FUN, ...) {
  len_gth <- NROW(end_p)
  # start_p are multi-period lag of end_p
  start_p <-  end_p[c(rep_len(1, look_back-1), 1:(len_gth-look_back+1))]
  # perform lapply() loop over length of end_p
  agg_s <- lapply(2:len_gth,
                  function(in_dex) {
                    FUN(x_ts[start_p[in_dex]:end_p[in_dex]], ...)
                  })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=index(x_ts[end_p]))
  agg_s
}  # end roll_agg


# 2. (20pts) Create an aggregation function called
# agg_regate(), which calls the function simu_ewma()
# and calculates the Sharpe ratios of the EWMA strategy,
# for a given vector of lambdas.
# agg_regate() should accept three arguments:
#  oh_lc - an OHLC series containing four columns of data,
#  lamb_das - integer vector of lambda parameters,
#  "..." - additional dots arguments to be passed to simu_ewma().
# hint: You can adapt code from the slide:
# Simulating Multiple EWMA Strategies

agg_regate <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # Simulate EWMA strategy and calculate Sharpe ratio
    re_turns <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, ...)[, "re_turns"]
    sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
  })  # end sapply
}  # end agg_regate

# Source the function simu_ewma() from the file
# ewma_model.R, using function source().
# Download the latest version from NYU Classes.

source("C:/Develop/R/scripts/ewma_model.R")

# Define oh_lc series, the EWMA look_back, and lamb_das.

library(HighFreq)
oh_lc <- rutils::etf_env$VTI["/2011"]
look_back <- 51
lamb_das <- seq(0.001, 0.01, 0.001)

# Call agg_regate() as follows:
agg_regate(oh_lc, lamb_das, look_back=look_back)

# You should get the following output:
#  [1] 0.1220623 0.1620571 0.1887122 0.2399056 0.2308350 0.1594881 0.1702486 0.1539695 0.1136539
# [10] 0.1180002


# 3. (20pts) Apply the functional roll_agg() to roll
# the function simu_ewma() over overlapping 12-month
# intervals in the past.

# Define end points at the end of each month.
# Use function endpoints() from package xts.

end_p <- xts::endpoints(oh_lc, on="months")
len_gth <- NROW(end_p)

# Define number of monthly intervals per lookback interval:
look_back <- 12

# Note that there are two different windows in this simulation.
# The first window is the EWMA window, called look_back and equal
# to 51 by default.
# The second window is the lookback interval, called look_back.
# To avoid an error, the end_p should be greater than
# the EWMA look_back, except for the first end_p, which
# should be equal to zero.
# Adjust the end_p so that they are greater than the
# EWMA look_back.

end_p[(end_p > 0) & (end_p <= look_back)] <- look_back+1

# Run roll_agg() as follows:

sharpe_ratios <- roll_agg(x_ts=oh_lc,
                          end_p=end_p,
                          look_back=look_back,
                          FUN=agg_regate,
                          lamb_das=lamb_das,
                          look_back=look_back)

# You should get the following output:
# > sharpe_ratios[1:6, 1:5]
#                  [,1]       [,2]       [,3]       [,4]       [,5]
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-30 -2.2223479 -2.2223479 -2.2223479 -2.2223479 -2.2223479
# 2007-04-30 -0.9446608 -0.9446608 -0.9446608 -0.9446608 -0.9446608
# 2007-05-31  0.0550219  0.0550219  0.0550219  0.0550219  0.0550219
# 2007-06-29 -0.3290286 -0.3290286 -0.3290286 -0.3290286 -0.3290286


#########

bar <- "foo"
bar <- 10

bar <- "foo"
assign(bar, 10)



###

var_1 <- sum(pc_1*pc_1)
# make re_turns orthogonal to pc1
date_s <- index(re_turns)
re_turns <- apply(re_turns, MARGIN=2,
                  function(x) {x - sum(pc_1*x)*pc_1/var_1})
# apply(re_turns, MARGIN=2, function(x) sum(pc_1*x)) # verify orthogonality

###

x11()
foo <- seq(0, 2*pi, length.out=24)
plot(x=cos(foo), y=sin(foo), asp=1)
abline(a=0, b=-0.1, col="red")
abline(a=0, b=10, col="blue")


###

heatmap(sharpe_ratios, col=colorRampPalette(c("blue", "red"))(22))

summary(microbenchmark(
  tee=-t(pnl_s) %*% pnl_s,
  s_um=-sum(pnl_s*pnl_s),
  times=10))[, c(1, 4, 5)]


###

w_1 <- sqrt(0.5); w_2 <- w_1
foo <- matrix(c(w_1, w_2, -w_2, w_1), nc=2)
t(foo) %*% foo
# bar <- re_turns %*% t(solve(foo))
(t(bar) %*% bar) / NROW(bar)

cov_mat <- function(re_turns, an_gle=0) {
  w_1 <- cos(an_gle)
  w_2 <- sin(an_gle)
  mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
  compo_nents <- re_turns %*% t(mat_rix)
  (t(compo_nents) %*% compo_nents) / NROW(compo_nents)
}  # end cov_mat

bar <- cov_mat(re_turns, an_gle=pi/4)
(t(re_turns) %*% re_turns) / NROW(re_turns)
(t(bar) %*% bar) / NROW(bar)

angle_s <- seq(0, pi/2, by=pi/24)
cov_mat <- sapply(angle_s, function(an_gle)
  cov_mat(re_turns, an_gle=an_gle)[1, 1])
plot(x=angle_s, y=cov_mat, t="l")

op_tim <- optimize(
  f=function(an_gle)
    -cov_mat(re_turns, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- op_tim$minimum
bar <- cov_mat(re_turns, an_gle=an_gle)
tan(an_gle)

w_1 <- cos(an_gle)
w_2 <- sin(an_gle)
mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
compo_nents <- re_turns %*% t(mat_rix)
(t(compo_nents) %*% compo_nents) / NROW(compo_nents)

plot(x=compo_nents[, 1], y=compo_nents[, 2],
     xlim=c(-10, 10), ylim=c(-10, 10))

mo_del <- lm(reg_formula, data=re_turns)
# get regression coefficients
coef(summary(mo_del))

foo <- cbind(rnorm(1000, sd=0.2), rnorm(1000)) %*% t(mat_rix)
(t(foo) %*% foo) / NROW(foo)
plot(x=foo[, 1], y=foo[, 2])
summary(lm(foo[, 1] ~ foo[, 2]))

op_tim <- optimize(
  f=function(an_gle)
    -cov_mat(foo, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- op_tim$minimum
tan(an_gle)

###

library(plotly)

df <- data.frame(Date=seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by="days"),
                 Value=sample(100:200, size=244, replace=T))

plot_ly(data=df, x=df$Date, y=df$Value, type="scatter", mode="lines") %>%
  add_trace(x=~df$Date, y=~df$Value, name="20yr Treasury rate") %>%
  layout(xaxis=list(range=c( as.numeric(max(df$Date)-30)*86400000,
                                 as.numeric(max(df$Date))*86400000   ),
                      rangeslider=list(type="date")  ))

###



