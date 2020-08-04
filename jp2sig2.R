rm(list = ls())
# Load packages
library(HighFreq)
# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")
# Load the OHLC prices of S&P500 stocks
load(file="C:/Develop/lecture_slides/data/sp500.RData")
data_env <- rutils::etf_env
sym_bols <- get("sym_bols", data_env)
lagg <- 2
thresh_old <- 0
co_eff <- 1

sym_bol <- "VXX"
oh_lc <- get(sym_bol, data_env)
clo_se <- log(quantmod::Cl(oh_lc))
# perf_stats <- lapply(3:5, backtest_ewma_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
# save(perf_stats, file="C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
load("C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
position_s <- lapply(perf_stats, function(x_ts) {
  x_ts[, "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
# in_dex <- paste(format(index(position_s)), "21:00:00")
# in_dex <- as.POSIXct(index(position_s), tz="UTC")
# Calculate timestamps
in_dex <- index(position_s)
# Get date from previous business day
in_dex <- rutils::lag_it(in_dex)
# Add time stamp to date equal to last minute before closing
in_dex <- paste(in_dex, "15:59:00")
# Coerce to date time
in_dex <- as.POSIXct(in_dex, tz="America/New_York")
# Coenvert to UTC time zone
in_dex <- lubridate::with_tz(in_dex, "UTC")
in_dex <- format(in_dex)
# position_s <- rowSums(position_s)
position_s <- xts::xts(cbind(in_dex, rep(sym_bol, NROW(position_s)), rowSums(position_s)), index(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
position_s <- position_s["2018-01-01/2020-04-22", ]
position_s <- position_s[-1, ]  # because VXX starts after 2018-01-01, so lag adds extra day up front
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

sym_bol <- "SVXY"
oh_lc <- get(sym_bol, data_env)
clo_se <- log(quantmod::Cl(oh_lc))
# perf_stats <- lapply(4:15, backtest_ewma_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
load("C:/Develop/jp2sig/data/perf_ewma_trend_svxy.RData")
position_s <- lapply(perf_stats, function(x_ts) {
  x_ts[, "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
# in_dex <- paste(format(index(position_s)), "21:00:00")
# in_dex <- as.POSIXct(index(position_s), tz="UTC")
# Calculate timestamps
in_dex <- index(position_s)
# Get date from previous business day
in_dex <- rutils::lag_it(in_dex)
# Add time stamp to date equal to last minute before closing
in_dex <- paste(in_dex, "15:59:00")
# Coerce to date time
in_dex <- as.POSIXct(in_dex, tz="America/New_York")
# Coenvert to UTC time zone
in_dex <- lubridate::with_tz(in_dex, "UTC")
in_dex <- format(in_dex)
# position_s <- rowSums(position_s)
position_s <- xts::xts(cbind(in_dex, rep(sym_bol, NROW(position_s)), rowSums(position_s)), index(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
position_s <- position_s["2018-01-01/2020-04-22", ]
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

sym_bol <- "DBC"
oh_lc <- get(sym_bol, data_env)
clo_se <- log(quantmod::Cl(oh_lc))
# perf_stats <- lapply(14:17, backtest_ewma_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
load("C:/Develop/jp2sig/data/perf_ewma_trend_dbc.RData")
position_s <- lapply(perf_stats, function(x_ts) {
  x_ts[, "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
# in_dex <- paste(format(index(position_s)), "21:00:00")
# in_dex <- as.POSIXct(index(position_s), tz="UTC")
# Calculate timestamps
in_dex <- index(position_s)
# Get date from previous business day
in_dex <- rutils::lag_it(in_dex)
# Add time stamp to date equal to last minute before closing
in_dex <- paste(in_dex, "15:59:00")
# Coerce to date time
in_dex <- as.POSIXct(in_dex, tz="America/New_York")
# Coenvert to UTC time zone
in_dex <- lubridate::with_tz(in_dex, "UTC")
in_dex <- format(in_dex)
# position_s <- rowSums(position_s)
position_s <- xts::xts(cbind(in_dex, rep(sym_bol, NROW(position_s)), rowSums(position_s)), index(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
position_s <- position_s["2018-01-01/2020-04-22", ]
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

sym_bol <- "USO"
oh_lc <- get(sym_bol, data_env)
clo_se <- log(quantmod::Cl(oh_lc))
# perf_stats <- lapply(2:10, backtest_ewma_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
load("C:/Develop/jp2sig/data/perf_ewma_trend_uso.RData")
position_s <- lapply(perf_stats, function(x_ts) {
  x_ts[, "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
# in_dex <- paste(format(index(position_s)), "21:00:00")
# in_dex <- as.POSIXct(index(position_s), tz="UTC")
# Calculate timestamps
in_dex <- index(position_s)
# Get date from previous business day
in_dex <- rutils::lag_it(in_dex)
# Add time stamp to date equal to last minute before closing
in_dex <- paste(in_dex, "15:59:00")
# Coerce to date time
in_dex <- as.POSIXct(in_dex, tz="America/New_York")
# Coenvert to UTC time zone
in_dex <- lubridate::with_tz(in_dex, "UTC")
in_dex <- format(in_dex)
# position_s <- rowSums(position_s)
position_s <- xts::xts(cbind(in_dex, rep(sym_bol, NROW(position_s)), rowSums(position_s)), index(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
position_s <- position_s["2018-01-01/2020-04-22", ]
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

sym_bol <- "XLK"
lagg <- 1
co_eff <- (-1)
oh_lc <- get(sym_bol, data_env)
clo_se <- log(quantmod::Cl(oh_lc))
# thresh_old <- 1.5
# perf_stats <- lapply(13:16, backtest_zscores_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
# thresh_old <- 1.0
# perf_stats <- c(perf_stats,
#                 lapply(7:15, backtest_zscores_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff))
load("C:/Develop/jp2sig/data/perf_zscores_revert_xlk.RData")
position_s <- lapply(perf_stats, function(x_ts) {
  x_ts[, "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
# in_dex <- paste(format(index(position_s)), "21:00:00")
# in_dex <- as.POSIXct(index(position_s), tz="UTC")
# Calculate timestamps
in_dex <- index(position_s)
# Get date from previous business day
in_dex <- rutils::lag_it(in_dex)
# Add time stamp to date equal to last minute before closing
in_dex <- paste(in_dex, "15:59:00")
# Coerce to date time
in_dex <- as.POSIXct(in_dex, tz="America/New_York")
# Coenvert to UTC time zone
in_dex <- lubridate::with_tz(in_dex, "UTC")
in_dex <- format(in_dex)
# position_s <- rowSums(position_s)
position_s <- xts::xts(cbind(in_dex, rep(sym_bol, NROW(position_s)), rowSums(position_s)), index(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
position_s <- position_s["2018-01-01/2020-04-22", ]
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)

# S&P500 scripts
sym_bols <- get("sym_bols", sp500_env)
sym_bol <- "ICE"
look_back <- 5
lagg <- 1
co_eff <- 1
thresh_old <- 0.0
oh_lc <- get(sym_bol, sp500_env)
clo_se <- log(quantmod::Cl(oh_lc))
load("C:/Develop/jp2sig/data/perf_ewma_trend_lback5.RData")

# perf_env <- new.env()
# process_ed <- eapply(sp500_env, function(oh_lc) {
#   sym_bol <- rutils::get_name(colnames(oh_lc)[1])
#   assign(x=sym_bol,
#          value=backtest_ewma_ts(oh_lc, look_back=look_back, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff),
#          envir=perf_env)
#   sym_bol
# })  # end eapply

perf_stats <- eapply(perf_env, function(x_ts) {
  if (start(x_ts) < "2010-01-01") {
    pnl_s <- x_ts["2010/2017" ,"pnls"]
    mean(pnl_s)/sd(pnl_s)
  } else NULL
})  # end eapply
perf_stats <- unlist(perf_stats)
perf_stats <- sort(perf_stats, decreasing=TRUE)
sym_bols <- names(perf_stats)
not_penny <- eapply(sp500_env, function(oh_lc) {
  oh_lc[NROW(oh_lc), 4] > 1
})  # end eapply
not_penny <- unlist(not_penny)
not_penny <- not_penny[not_penny]
not_penny <- names(not_penny)
sym_bols <- sym_bols[sym_bols %in% not_penny]

# Calculate the positions of the best performing stocks
# Triple the stock positions to balance risk with ETFs
n_stocks <- 80
be_st <- lapply(sym_bols[1:n_stocks], function(sym_bol) {
  position_s <- get(sym_bol, perf_env)[ ,"positions"]
  position_s <- 3*position_s
  # Calculate timestamps
  in_dex <- index(position_s)
  # Get date from previous business day
  in_dex <- rutils::lag_it(in_dex)
  # Add time stamp to date equal to last minute before closing
  in_dex <- paste(in_dex, "15:59:00")
  # Coerce to date time
  in_dex <- as.POSIXct(in_dex, tz="America/New_York")
  # Coenvert to UTC time zone
  in_dex <- lubridate::with_tz(in_dex, "UTC")
  in_dex <- format(in_dex)
  position_s <- xts::xts(cbind(in_dex, rep(sym_bol, NROW(position_s)), rowSums(position_s)), index(position_s))
  colnames(position_s) <- c("time", "TICKER", "position_dollars")
  position_s <- position_s["2018-01-01/2020-04-22", ]
  write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply
# Calculate the positions of the worst performing stocks
wo_rst <- lapply(sym_bols[(NROW(sym_bols)-n_stocks+1):NROW(sym_bols)], function(sym_bol) {
  position_s <- get(sym_bol, perf_env)["2018-01-01/2020-04-22" ,"positions"]
  position_s <- (-3*position_s)
  # Calculate timestamps
  in_dex <- index(position_s)
  # Get date from previous business day
  in_dex <- rutils::lag_it(in_dex)
  # Add time stamp to date equal to last minute before closing
  in_dex <- paste(in_dex, "15:59:00")
  # Coerce to date time
  in_dex <- as.POSIXct(in_dex, tz="America/New_York")
  # Coenvert to UTC time zone
  in_dex <- lubridate::with_tz(in_dex, "UTC")
  in_dex <- format(in_dex)
  position_s <- xts::xts(cbind(in_dex, rep(sym_bol, NROW(position_s)), rowSums(position_s)), index(position_s))
  colnames(position_s) <- c("time", "TICKER", "position_dollars")
  position_s <- position_s["2018-01-01/2020-04-22", ]
  write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply


## Read the strategy positions back

position_s <- read.csv(file="C:/Develop/jp2sig/data/positions.csv", stringsAsFactors=FALSE)
position_s <- split(position_s, position_s$TICKER)
position_s <- lapply(position_s, function(da_ta) {
  # Extract dates index
  sym_bol <- da_ta[1, "TICKER"]
  # Calculate timestamps
  in_dex <- da_ta[, "time"]
  # Coerce strings to date times
  in_dex <- lubridate::ymd_hms(in_dex)
  # Coerce to date
  in_dex <- as.Date(in_dex)
  # Get date for next business day
  in_dex <- rutils::lag_it(in_dex, -1)
  # Add to last date
  in_dex[NROW(in_dex)] <- in_dex[NROW(in_dex)] + 1
  # Format into xts series
  da_ta <- xts::xts(da_ta[, "position_dollars"], in_dex)
  colnames(da_ta) <- sym_bol
  da_ta
})  # end lapply
position_s <- rutils::do_call(cbind, position_s)
position_s$VXX[is.na(position_s$VXX)] <- 0
sum(is.na(position_s))
in_dex <- index(position_s)
symbols_strategy <- colnames(position_s)
sym_bols <- rutils::etf_env$sym_bols
sym_bols <- sym_bols[sym_bols %in% symbols_strategy]
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- re_turns[in_dex]
re_turns[is.na(re_turns)] <- 0
sum(is.na(re_turns))
sym_bols <- names(sp500_env)
sym_bols <- sym_bols[sym_bols %in% symbols_strategy]
sp500_returns <- lapply(sym_bols, function(sym_bol) {
  oh_lc <- get(sym_bol, sp500_env)
  clo_se <- log(quantmod::Cl(oh_lc))
  rutils::diff_it(clo_se)
})  # end lapply
sp500_returns <- rutils::do_call(cbind, sp500_returns)
sp500_returns <- sp500_returns[in_dex]
sum(is.na(sp500_returns))
symbols_strategy <- colnames(position_s)
re_turns <- cbind(re_turns, sp500_returns)
colnames(re_turns) <- rutils::get_name(colnames(re_turns))
sum(is.na(re_turns))
re_turns <- re_turns[, symbols_strategy]
returns_strategy <- re_turns*position_s
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, in_dex)
# Plot it
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))


# Plot subsets
returns_strategy <- re_turns[, c("DBC", "USO", "VXX", "XLK")]*position_s[, c("DBC", "USO", "VXX", "XLK")]
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, in_dex)
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))
returns_strategy <- re_turns[, c("DBC", "USO", "SVXY", "VXX", "XLK")]*position_s[, c("DBC", "USO", "SVXY", "VXX", "XLK")]
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, in_dex)
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))
match(c("DBC", "USO", "SVXY", "VXX", "XLK"), colnames(position_s))
foo <- match(c("DBC", "USO", "SVXY", "VXX", "XLK"), colnames(position_s))
foo
returns_strategy <- re_turns[, -foo]*position_s[, -foo]
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, in_dex)
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))
