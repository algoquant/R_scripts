##############
# Backtests for fund in July 2020

# Load packages
library(HighFreq)

# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")


# Load the OHLC prices of S&P500 stocks
load(file="C:/Develop/lecture_slides/data/sp500.RData")

# Define variables
sym_bols <- get("sym_bols", sp500_env)
sym_bol <- "AAPL"
look_back <- 5
lagg <- 1
co_eff <- 1
thresh_old <- 0.0

oh_lc <- get(sym_bol, sp500_env)
clos_e <- log(quantmod::Cl(oh_lc))
re_turns <- rutils::diff_it(clos_e)



## Calculate the strategy performance for a single stock
pnl_s <- backtest_ewma_ts(get(sym_bol, sp500_env), look_back=look_back, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)


## Calculate the strategy performance for a vector of look_back parameters
perf_stats <- lapply(3:5, backtest_ewma_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
pnl_s <- lapply(perf_stats, function(x) x[, "pnls"])
pnl_s <- do.call(cbind, pnl_s)
pnl_s <- rowMeans(pnl_s)
mean(pnl_s)/sd(pnl_s)
pnl_s <- xts::xts(pnl_s, index(oh_lc))
# Plot it
dygraphs::dygraph(cumsum(pnl_s), main=paste("Back-test of", sym_bol, "Strategies"))
# Plot it with clos_e
da_ta <- cbind(clos_e, cumsum(pnl_s))
col_names <- c(sym_bol, "Strategy")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main=paste(col_names[1], "Strategy")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")



## Rank the S&P500 stocks based on strategy returns for a vector of look_back parameters
perf_stats <- eapply(sp500_env, function(oh_lc) {
  if (start(oh_lc) < "2017-01-01") {
    pnl_s <- lapply(3:5, backtest_ewma_ts, oh_lc=oh_lc["2010/"], lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
    pnl_s <- lapply(pnl_s, function(x) x[, "pnls"])
    pnl_s <- do.call(cbind, pnl_s)
    pnl_s <- rowMeans(pnl_s)
    mean(pnl_s)/sd(pnl_s)
  } else NULL
})  # end eapply

perf_stats <- unlist(perf_stats)
# names(perf_stats) <- names(sp500_env)
perf_stats <- sort(perf_stats, decreasing=TRUE)
write.csv(perf_stats, file="C:/Develop/jp2sig/data/backtest_ewma2.csv", row.names=TRUE)



## Calculate the strategy performance for all S&P500 stocks and copy into environment
perf_env <- new.env()
process_ed <- eapply(sp500_env, function(oh_lc) {
  sym_bol <- rutils::get_name(colnames(oh_lc)[1])
  # cat(sym_bol, "\n")
  assign(x=sym_bol, 
         value=backtest_ewma_ts(oh_lc, look_back=look_back, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff), 
         envir=perf_env)
  sym_bol
})  # end eapply

save(perf_env, file="C:/Develop/jp2sig/data/perf_ewma_trend_lback5.RData")

load("C:/Develop/jp2sig/data/perf_ewma_trend_lback5.RData")


## Rank the S&P500 stocks based on strategy returns
perf_stats <- sapply(perf_env, function(x_ts) {
  if (start(x_ts) < "2017-01-01") {
    pnl_s <- x_ts["2010/" ,"pnls"]
    mean(pnl_s)/sd(pnl_s)
  } else NULL
})  # end eapply
perf_stats <- unlist(perf_stats)
# names(perf_stats) <- names(sp500_env)
perf_stats <- sort(perf_stats, decreasing=TRUE)
write.csv(perf_stats, file="C:/Develop/jp2sig/data/backtest_ewma.csv", row.names=TRUE)



## Rank the S&P500 stocks based on strategy returns in-sample
# perf_env is an environment with time series of performance
perf_stats <- eapply(perf_env, function(x_ts) {
  if (start(x_ts) < "2010-01-01") {
    pnl_s <- x_ts["2010/2017" ,"pnls"]
    mean(pnl_s)/sd(pnl_s)
  } else NULL
})  # end eapply
perf_stats <- unlist(perf_stats)
perf_stats <- sort(perf_stats, decreasing=TRUE)
sym_bols <- names(perf_stats)

# Select sym_bols with final stock price above $1
not_penny <- eapply(sp500_env, function(oh_lc) {
  oh_lc[NROW(oh_lc), 4] > 1
})  # end eapply
not_penny <- unlist(not_penny)
not_penny <- not_penny[not_penny]
not_penny <- names(not_penny)
sym_bols <- sym_bols[sym_bols %in% not_penny]

# Calculate the strategy returns out-of-sample
# as a function of the number of stocks selected
calc_perf <- function(n_stocks) {
  # Calculate the returns of the best performing stocks
  be_st <- lapply(sym_bols[1:n_stocks], function(sym_bol) {
    get(sym_bol, perf_env)[, "pnls"]
  })  # end lapply
  be_st <- rutils::do_call(cbind, be_st)
  # Calculate the returns of the worst performing stocks
  wo_rst <- lapply(sym_bols[(NROW(sym_bols)-n_stocks+1):NROW(sym_bols)], function(sym_bol) {
    get(sym_bol, perf_env)[, "pnls"]
  })  # end lapply
  wo_rst <- rutils::do_call(cbind, wo_rst)
  wo_rst <- (-wo_rst)
  pnl_s <- cbind(be_st, wo_rst)
  pnl_s[1, is.na(pnl_s[1, ])] <- 0
  pnl_s <- zoo::na.locf(pnl_s, na.rm=FALSE)
  pnl_s <- pnl_s["2017/"]
  mean(pnl_s)/sd(pnl_s)
}  # end calc_perf

# Calculate the profile of strategy returns as a function of the number of stocks selected
pro_file <- sapply(10*(1:10), calc_perf)
plot(x=10*(1:10), y=pro_file, t="l", main="Sharpe of Back-test EWMA Strategies", 
     xlab="Select number", ylab="Sharpe")


## Calculate the out-of-sample strategy returns for the best and worst performing stocks
# Calculate the returns of the best performing stocks
n_stocks <- 80
be_st <- lapply(sym_bols[1:n_stocks], function(sym_bol) {
  get(sym_bol, perf_env)[, "pnls"]
})  # end lapply
be_st <- rutils::do_call(cbind, be_st)
# Calculate the returns of the worst performing stocks
wo_rst <- lapply(sym_bols[(NROW(sym_bols)-n_stocks+1):NROW(sym_bols)], function(sym_bol) {
  get(sym_bol, perf_env)[, "pnls"]
})  # end lapply
wo_rst <- rutils::do_call(cbind, wo_rst)
wo_rst <- (-wo_rst)
pnl_s <- cbind(be_st, wo_rst)
pnl_s[1, is.na(pnl_s[1, ])] <- 0
pnl_s <- zoo::na.locf(pnl_s, na.rm=FALSE)
pnl_s <- xts::xts(rowMeans(pnl_s), index(pnl_s))
pnl_s <- cumsum(pnl_s)
dygraphs::dygraph(pnl_s, main="Back-test of EWMA strategies")



## Save the strategy positions

n_stocks <- 80
# Calculate the positions of the best performing stocks
be_st <- lapply(sym_bols[1:n_stocks], function(sym_bol) {
  position_s <- get(sym_bol, perf_env)["2018-01-01/2020-04-22" ,"positions"]
  in_dex <- paste(format(index(position_s)), "21:00:00")
  position_s <- cbind(in_dex, rep(sym_bol, NROW(position_s)), as.character(position_s))
  colnames(position_s) <- c("time", "TICKER", "position_dollars")
  write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply
# Calculate the positions of the worst performing stocks
wo_rst <- lapply(sym_bols[(NROW(sym_bols)-n_stocks+1):NROW(sym_bols)], function(sym_bol) {
  position_s <- get(sym_bol, perf_env)["2018-01-01/2020-04-22" ,"positions"]
  in_dex <- paste(format(index(position_s)), "21:00:00")
  position_s <- (-position_s)
  position_s <- cbind(in_dex, rep(sym_bol, NROW(position_s)), as.character(position_s))
  colnames(position_s) <- c("time", "TICKER", "position_dollars")
  write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)
  NULL
})  # end lapply




## Z-scores strategies using backtest_zscores_ts()

# Define variables
data_env <- rutils::etf_env
sym_bols <- get("sym_bols", data_env)
sym_bol <- "XLK"
look_back <- 5
lagg <- 1
co_eff <- (-1)

oh_lc <- get(sym_bol, data_env)
clos_e <- log(quantmod::Cl(oh_lc))
re_turns <- rutils::diff_it(clos_e)

## Calculate the strategy performance for two vectors of look_back parameters
# This model works well more recently
thresh_old <- 1.5
perf_stats <- lapply(13:16, backtest_zscores_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
# This model also worked well in 2008
thresh_old <- 1.0
perf_stats <- c(perf_stats,
                lapply(7:15, backtest_zscores_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff))
save(perf_stats, file="C:/Develop/jp2sig/data/perf_zscores_revert_xlk.RData")
load("C:/Develop/jp2sig/data/perf_zscores_revert_xlk.RData")
# Calculate the strategy returns
pnl_s <- lapply(perf_stats, function(x) x[, "pnls"])
pnl_s <- do.call(cbind, pnl_s)
pnl_s <- rowMeans(pnl_s)
mean(pnl_s)/sd(pnl_s)
pnl_s <- xts::xts(pnl_s, index(oh_lc))
# Plot it
dygraphs::dygraph(cumsum(pnl_s), main=paste("Back-test of", sym_bol, "Strategies"))
plot(cumsum(pnl_s), main=paste("Back-test of", sym_bol, "Strategies"))
# Plot it with clos_e
da_ta <- cbind(clos_e, cumsum(pnl_s))
col_names <- c(sym_bol, "Strategy")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main=paste(col_names[1], "Strategy")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")



## Save the strategy positions

# Calculate holding periods number of trades
peri_od <- sapply(perf_stats, function(x_ts) {
  position_s <- x_ts[, "positions"]
  2*NROW(position_s) / sum(abs(rutils::diff_it(position_s)))
})  # end sapply
mean(peri_od)


# Save the strategy positions
position_s <- lapply(perf_stats, function(x_ts) {
  x_ts["2018-01-01/2020-04-22", "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
in_dex <- paste(format(index(position_s)), "21:00:00")
position_s <- rowSums(position_s)
position_s <- cbind(in_dex, rep(sym_bol, NROW(position_s)), as.character(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)


## Trending strategy using backtest_ewma_ts()

# Define variables
sym_bol <- "VXX"
lagg <- 2
thresh_old <- 0
co_eff <- 1

oh_lc <- get(sym_bol, data_env)
clos_e <- log(quantmod::Cl(oh_lc))
re_turns <- rutils::diff_it(clos_e)

## Calculate the strategy performance for two vectors of look_back parameters
perf_stats <- lapply(3:5, backtest_ewma_ts, oh_lc=oh_lc, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
save(perf_stats, file="C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
load("C:/Develop/jp2sig/data/perf_ewma_trend_vxx.RData")
# Calculate the strategy returns
pnl_s <- lapply(perf_stats, function(x) x[, "pnls"])
pnl_s <- do.call(cbind, pnl_s)
pnl_s <- rowMeans(pnl_s)
mean(pnl_s)/sd(pnl_s)
pnl_s <- xts::xts(pnl_s, index(oh_lc))
# Plot it
dygraphs::dygraph(cumsum(pnl_s), main=paste("Back-test of", sym_bol, "Strategies"))
plot(cumsum(pnl_s), main=paste("Back-test of", sym_bol, "Strategies"))
# Plot it with clos_e
da_ta <- cbind(clos_e, cumsum(pnl_s))
col_names <- c(sym_bol, "Strategy")
colnames(da_ta) <- col_names
dygraphs::dygraph(da_ta, main=paste(col_names[1], "Strategy")) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=2, col="blue") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=2, col="red")


## Save the strategy positions

# Calculate holding periods number of trades
peri_od <- sapply(perf_stats, function(x_ts) {
  position_s <- x_ts[, "positions"]
  2*NROW(position_s) / sum(abs(rutils::diff_it(position_s)))
})  # end sapply
mean(peri_od)


# Save the strategy positions
position_s <- lapply(perf_stats, function(x_ts) {
  x_ts["2018-01-01/2020-04-22", "positions"]
})  # end lapply
position_s <- do.call(cbind, position_s)
in_dex <- paste(format(index(position_s)), "21:00:00")
position_s <- rowSums(position_s)
position_s <- cbind(in_dex, rep(sym_bol, NROW(position_s)), as.character(position_s))
colnames(position_s) <- c("time", "TICKER", "position_dollars")
write.table(position_s, file="C:/Develop/jp2sig/data/positions.csv", sep=",", row.names=FALSE, col.names=FALSE, append=TRUE)



## Read the strategy positions back

# Read the strategy positions back
position_s <- read.csv(file="C:/Develop/jp2sig/data/positions.csv", stringsAsFactors=FALSE)
# Split time series into daily list
position_s <- split(position_s, position_s$TICKER)
# cbind the list back into a time series and compare with the original
position_s <- lapply(position_s, function(da_ta) {
  # Extract dates index
  sym_bol <- da_ta[1, "TICKER"]
  in_dex <- da_ta[, "time"]
  in_dex <- lubridate::ymd_hms(in_dex)
  in_dex <- as.Date(in_dex)
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

# Select ETF returns
sym_bols <- rutils::etf_env$sym_bols
sym_bols <- sym_bols[sym_bols %in% symbols_strategy]
re_turns <- rutils::etf_env$re_turns[, sym_bols]
re_turns <- re_turns[in_dex]
re_turns[is.na(re_turns)] <- 0
sum(is.na(re_turns))

# Select S&P500 returns
sym_bols <- names(sp500_env)
sym_bols <- sym_bols[sym_bols %in% symbols_strategy]
sp500_returns <- lapply(sym_bols, function(sym_bol) {
  oh_lc <- get(sym_bol, sp500_env)
  clos_e <- log(quantmod::Cl(oh_lc))
  rutils::diff_it(clos_e)
})  # end lapply
sp500_returns <- rutils::do_call(cbind, sp500_returns)
sp500_returns <- sp500_returns[in_dex]
sum(is.na(sp500_returns))

# Combine ETF returns with S&P500 returns
re_turns <- cbind(re_turns, sp500_returns)
colnames(re_turns) <- rutils::get_name(colnames(re_turns))
sum(is.na(re_turns))
re_turns <- re_turns[, symbols_strategy]

# Calculate the strategy returns
returns_strategy <- re_turns*position_s
returns_strategy <- rowMeans(returns_strategy)
returns_strategy <- xts::xts(returns_strategy, in_dex)
# Plot it
dygraphs::dygraph(cumsum(returns_strategy), main=paste("Back-test of Strategies"))




##############
## Old stuff

## Loop over selected sp500 stocks
sp500_env <- sp500_env
sym_bols <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")

pnl_s <- lapply(sym_bols, function(sym_bol) {
  backtest_ewma_ts(get(sym_bol, sp500_env), look_back=look_back, lagg=lagg, co_eff=co_eff)[, "pnls"]
})  # end lapply

pnl_s <- rutils::do_call(cbind, pnl_s)
# Copy over NA values with zeros
pnl_s[1, is.na(pnl_s[1, ])] <- 0
pnl_s <- zoo::na.locf(pnl_s, na.rm=FALSE)
sum(is.na(pnl_s))
pnl_s <- xts::xts(rowMeans(pnl_s), index(pnl_s))
pnl_s <- cumsum(pnl_s)
x11()
chart_Series(x=pnl_s, name="Back-test of EWMA strategies")
dygraphs::dygraph(pnl_s, main="Back-test of EWMA strategies")


# Loop over sp500 stocks and get position_s
sp500_env <- sp500_env
sym_bols <- c("PG", "CDNS", "YUM", "YUMC", "KHC", "SNPS", "ODFL", "CHRW", "AWK", "SO", "EA", "FIS", "DG", "BAX", "HRL", "MSFT", "XOM", "BSX", "JNJ", "CLX", "CL", "MCD", "WMT", "SBUX", "LLY", "ADM", "BIO", "XLNX", "ATVI", "DISH", "K", "SHW", "SIG", "CSCO", "INTU", "VRTX", "FB", "ORCL", "DUK", "KSS", "ROP", "AKAM", "MXIM", "TXN", "NEM", "COST", "EL", "JWN", "ACN", "FISV", "KLAC", "PFE", "TYL", "BIIB", "MCHP", "BBBY", "DRE", "PEP", "LIN", "NKE", "TROW", "LEN", "HOLX", "NVR", "UDR", "WEC", "DHI", "NI")

position_s <- lapply(sym_bols, function(sym_bol) {
  backtest_ewma_ts(get(sym_bol, sp500_env), look_back=look_back, lagg=lagg, co_eff=co_eff)[, "positions"]
})  # end lapply

position_s <- rutils::do_call(cbind, position_s)
names(position_s) <- sym_bols
position_s[1, ] <- 0
position_s <- na.locf(position_s, na.rm=FALSE)
zoo::write.zoo(position_s, file="C:/Develop/jp2sig/data/positions_ewma.csv", sep=",", col.names=TRUE)


load(file="C:/Develop/lecture_slides/data/sp500_returns.RData")
re_turns <- re_turns[, sym_bols]
re_turns[1, is.na(re_turns[1, ])] <- 0
re_turns <- na.locf(re_turns, na.rm=FALSE)
pnl_s <- xts::xts(cumsum(rowMeans(position_s*re_turns)), index(re_turns))



## Backtest of Z-Score crossover strategies using the code in app_zscore.R

# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")

# Load S&P500 prices
load(file="C:/Develop/lecture_slides/data/sp500.RData")
look_back <- 10
lagg <- 1
co_eff <- (-1)
thresh_old <- 1

pnl_s <- backtest_zscores_ts(sp500_env$YUM["2010/"], look_back=look_back, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
pnl_s <- pnl_s[, "pnls"]
position_s <-  pnl_s[, "positions"]
plot(cumsum(pnl_s))
mean(pnl_s)/sd(pnl_s)


# Calculate past pnls from performance environment
pnl_s <- eapply(perf_stats, function(x_ts) {
  if (start(x_ts) < "2017-01-01") {
    pnl_s <- x_ts["2010/2017" ,"pnls"]
    mean(pnl_s)/sd(pnl_s)
  } else NULL
})  # end eapply

pnl_s <- unlist(pnl_s)
pnl_s <- sort(pnl_s, decreasing=TRUE)
sym_bols <- names(pnl_s)

# Calculate pnls of best stocks from environment
be_st <- lapply(sym_bols[1:50], function(sym_bol) {
  get(sym_bol, perf_stats)[, "pnls"]
})  # end lapply
names(be_st) <- sym_bols[1:50]
be_st <- rutils::do_call(cbind, be_st)

# Calculate reverse of pnls of worst stocks
wo_rst <- lapply(sym_bols[(NROW(sym_bols)-49):NROW(sym_bols)], function(sym_bol) {
  get(sym_bol, perf_stats)[, "pnls"]
})  # end lapply
wo_rst <- rutils::do_call(cbind, wo_rst)
wo_rst <- (-wo_rst)

# Combine everything and plot
pnl_s <- cbind(be_st, wo_rst)
pnl_s[1, is.na(pnl_s[1, ])] <- 0
pnl_s <- zoo::na.locf(pnl_s, na.rm=FALSE)
sum(is.na(pnl_s))
pnl_s <- xts::xts(rowMeans(pnl_s), index(pnl_s))
pnl_s <- cumsum(pnl_s)
dygraphs::dygraph(pnl_s, main="Back-test of Reverting Strategies")



# Combine everything and plot
pnl_s <- cbind(pnl_s, wo_rst)
pnl_s[1, is.na(pnl_s[1, ])] <- 0
pnl_s <- zoo::na.locf(pnl_s, na.rm=FALSE)
sum(is.na(pnl_s))
pnl_s <- xts::xts(rowMeans(pnl_s), index(pnl_s))
pnl_s <- cumsum(pnl_s)
dygraphs::dygraph(pnl_s, main="Back-test of Reverting Strategies")



## Calculate pnls directly
pnl_s <- eapply(sp500_env, function(oh_lc) {
  if (start(oh_lc) < "2017-01-01") {
    pnl_s <- backtest_zscores_ts(oh_lc["2010/"], look_back=look_back, lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
    pnl_s <- pnl_s[, "pnls"]
    mean(pnl_s)/sd(pnl_s)
  } else NULL
})  # end eapply

pnl_s <- unlist(pnl_s)
pnl_s <- sort(pnl_s, decreasing=TRUE)
write.csv(pnl_s, file="C:/Develop/jp2sig/data/backtest_zscores.csv", row.names=TRUE)


## Loop over all sp500 stocks using several parameters
load(file="C:/Develop/lecture_slides/data/sp500.RData")
sym_bols <- names(sp500_env)
sym_bol <- "AAPL"
look_back <- 5
lagg <- 1
co_eff <- (-1)
thresh_old <- 1

pnl_s <- eapply(sp500_env, function(oh_lc) {
  if (start(oh_lc) < "2017-01-01") {
    pnl_s <- lapply(2*(5:15), backtest_zscores_ts, oh_lc=oh_lc["2010/"], lagg=lagg, thresh_old=thresh_old, co_eff=co_eff)
    pnl_s <- lapply(pnl_s, function(x) x[, "pnls"])
    pnl_s <- do.call(cbind, pnl_s)
    pnl_s <- rowMeans(pnl_s)
    mean(pnl_s)/sd(pnl_s)
  } else NULL
})  # end eapply

pnl_s <- unlist(pnl_s)
# names(pnl_s) <- names(sp500_env)
pnl_s <- sort(pnl_s, decreasing=TRUE)
write.csv(pnl_s, file="C:/Develop/jp2sig/data/backtest_zscores_revert.csv", row.names=TRUE)



## Combine S&P500 symbols

symbols_zscores_revert <- read.csv(file="C:/Develop/jp2sig/data/backtest_zscores_revert.csv", stringsAsFactors=FALSE)
symbols_ewma_revert <- read.csv(file="C:/Develop/jp2sig/data/backtest_ewma_revert.csv", stringsAsFactors=FALSE)
symbols_ewma_revertr <- read.csv(file="C:/Develop/jp2sig/data/backtest_ewma_revertr.csv", stringsAsFactors=FALSE)

foo <- (symbols_zscores_revert$ticker %in% symbols_ewma_revert$ticker)

