####################################
### Scripts for demonstrating the forecasting of stock returns
### using PCR regression from package roll
####################################


### Install packages rutils, HighFreq, and roll from github

install.packages("devtools")
devtools::install_github(repo="algoquant/rutils")
devtools::install_github(repo="algoquant/HighFreq")
devtools::install_github(repo="jjf234/roll")
library(HighFreq)


### Example of rolling beta regressions using package roll (without forecasting)

library(roll)

# example of rolling regressions using standard rollapply() - SLOW!
# specify regression formula
reg_formula <- XLP ~ VTI
# perform rolling beta regressions every month
beta_s <- rollapply(env_etf$re_turns, width=252,
                    FUN=function(de_sign)
                      coef(lm(reg_formula, data=de_sign))[2],
                    by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11()
chart_Series(x=beta_s, name=paste("rolling betas", format(reg_formula)))

# perform daily rolling beta regressions in parallel - FAST!
beta_s <- roll::roll_lm(x=env_etf$re_turns[, "VTI"],
                        y=env_etf$re_turns[, "XLP"],
                        width=252)$coefficients
beta_s <- na.omit(beta_s[, 2])
chart_Series(x=beta_s, name=paste("rolling betas", format(reg_formula)))

# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- env_etf$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
                      FUN=function(de_sign)
                        coef(lm(reg_formula, data=de_sign))[2],
                      by.column=FALSE, align="right"),
  roll_lm=roll::roll_lm(x=da_ta[, "VTI"],
                        y=da_ta[, "XLP"],
                        width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


### More interesting example using SPY_design design matrix
# first either load or calculate design matrix

### Load design matrix called SPY_design containing columns of data aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)


### Calculate design matrix called SPY_design containing columns of data aggregations

win_dow <- 5
returns_running <- HighFreq::run_returns(x_ts=SPY)
returns_rolling <- roll_vwap(oh_lc=SPY, x_ts=returns_running, win_dow=win_dow)
colnames(returns_running) <- "returns"
colnames(returns_rolling) <- "returns.WA5"

var_running <- run_variance(oh_lc=SPY)
var_rolling <- roll_vwap(oh_lc=SPY, x_ts=var_running, win_dow=win_dow)
colnames(var_running) <- "variance"
colnames(var_rolling) <- "variance.WA5"

skew_running <- run_skew(oh_lc=SPY)
skew_rolling <- roll_vwap(oh_lc=SPY, x_ts=skew_running, win_dow=win_dow)
colnames(skew_running) <- "skew"
colnames(skew_rolling) <- "skew.WA5"

# sharpe_running <- run_sharpe(oh_lc=SPY)
# sharpe_rolling <- roll_vwap(oh_lc=SPY, x_ts=sharpe_running, win_dow=win_dow)
# colnames(sharpe_running) <- "sharpe_running"
# colnames(sharpe_rolling) <- "sharpe_running.WA5"
#
# sharpe_rolling <- roll_sharpe(oh_lc=SPY, win_dow=win_dow)
# colnames(sharpe_rolling) <- "sharpe_rolling"

hurst_rolling <- roll_hurst(oh_lc=SPY, win_dow=win_dow)
colnames(hurst_rolling) <- "hurst_rolling"

# select most significant factors plus interaction terms
SPY_design <- cbind(returns_running, returns_rolling, var_running, skew_running,
                    hurst_rolling, returns_running*var_running, returns_running*skew_running)
colnames(SPY_design) <- c(colnames(SPY_design)[1:4], "hurst", "rets_var", "rets_skew")

# apply rolling centering and scaling to the design matrix
library(roll)
SPY_design <- roll::roll_scale(data=SPY_design, width=6.5*60, min_obs=1)
# remove NAs
core_data <- coredata(SPY_design)
core_data[is.na(core_data)] <- 0
SPY_design <- xts(x=core_data, order.by=index(SPY_design))
sum(is.na(SPY_design))


### calculate close to close percentage returns
returns_running <- HighFreq::run_returns(x_ts=SPY, sca_le=FALSE)
# calculate returns advanced in time
returns_advanced <- rutils::lag_xts(returns_running, lag=-1)
colnames(returns_advanced) <- "returns_advanced"


### perform rolling regressions of returns_advanced versus de_sign
beta_s <- sapply(1:((NROW(SPY_design) %/% 10000)-1), function(x) {
  ran_ge <- 1:10000 + 10000*x
  summary(lm(returns_advanced[ran_ge, ] ~ SPY_design[ran_ge, ]))$coefficients[, "t value"]
})  # end sapply
beta_s <- t(beta_s)
colnames(beta_s)[-1] <- sapply(colnames(beta_s)[-1], function(x) strsplit(x, split = "]")[[1]][2])
apply(beta_s, MARGIN=2, sum)
plot.zoo(beta_s)

### perform regression of returns_advanced versus de_sign in first quarter of data
ran_ge <- 1:(NROW(SPY_design) %/% 4)
mod_el <- lm(returns_advanced[ran_ge, ] ~ SPY_design[ran_ge, ])
beta_s <- summary(mod_el)$coefficients[, "t value"]
names(beta_s)[-1] <- sapply(names(beta_s)[-1], function(x) strsplit(x, split = "]")[[1]][2])

### apply beta_s out-of-sample
weight_s <- SPY_design %*% beta_s[-1]
colnames(weight_s) <- "weights"
# lag weight_s by one period into the future
weight_s <- rutils::lag_xts(weight_s)
# weight_s <- rutils::roll_sum(weight_s, win_dow=3) / 3
pnl_s <- (weight_s * returns_running)[-ran_ge, ]
pnl_s <- pnl_s*sd(diff_xts(log(SPY[index(pnl_s), 4])))/sd(pnl_s)
pnl_s <- exp(cumsum(pnl_s))
# pnl_s <- exp(cumsum(sign(weight_s) * returns_running[-ran_ge, ]))


### hit rates
hit_s <- sign(weight_s * returns_running)[-ran_ge, ]
hit_s <- cbind(weight_s[-ran_ge, ], hit_s)
colnames(hit_s) <- c("weights", "hits")
x11()
# histogram and quantiles
foo <- hist(hit_s[, "weights"], breaks=100)
foo$breaks
quantile_s <- quantile(hit_s[, "weights"], probs=seq(0.05, 0.95, 0.1))
# extreme quantiles have higher hit rates
sapply(seq_along(quantile_s)[-1], function(x)
  NROW(hit_s[(hit_s[, "weights"]>=quantile_s[x-1]) & (hit_s[, "weights"]<quantile_s[x]), "hits"]))
sapply(seq_along(quantile_s)[-1], function(x)
  sum(hit_s[(hit_s[, "weights"]>=quantile_s[x-1]) & (hit_s[, "weights"]<quantile_s[x]), "hits"]))


### buy when weight_s exceeds threshold, hold, and sell when weight_s is below (-threshold)
thresh_old <- 1.5
position_s <- NA*numeric(NROW(weight_s))
position_s[weight_s > thresh_old] <- 1.0
position_s[weight_s < (-thresh_old)] <- -1.0
position_s[ran_ge] <- 0.0
position_s <- zoo::na.locf(position_s)
# lag the position_s
position_s <- c(0, position_s[-NROW(position_s)])
# position_s <- xts(position_s, order.by=index(returns_running))
# position_s <- cbind(weight_s, position_s)
# colnames(position_s)[2] <- "positions"
# cumulative PnL
pnl_s <- exp(cumsum((position_s * returns_running)[-ran_ge, ]))
last(pnl_s)
# pnl_s <- cumsum(position_s[, 2]*re_turns)
# chart_Series(pnl_s)

cum_pnls <- function(thresh_old) {
  position_s <- NA*numeric(NROW(weight_s))
  position_s[weight_s > thresh_old] <- 1.0
  position_s[weight_s < (-thresh_old)] <- -1.0
  position_s[ran_ge] <- 0.0
  position_s <- zoo::na.locf(position_s)
  position_s <- c(0, position_s[-NROW(position_s)])
  last(exp(cumsum((position_s * returns_running)[-ran_ge, ])))
}  # end

foo <- sapply(seq(0.1, 3.0, by=0.1), cum_pnls)
names(foo) <- seq(0.1, 3.0, by=0.1)


### perform rolling forecasting PCR regressions in parallel
# use only the first principal component: argument "comps"
betas_running <- roll::roll_pcr(x=SPY_design["2011/2012", ],
                          y=returns_advanced["2011/2012", ],
                          width=1*60, comps=1:1, min_obs=1)
betas_running$coefficients[1, ] <- 0

# calculate mean beta coefficients
sapply(betas_running$coefficients, mean)

# calculate rolling mean beta coefficients over time
betas_rolling <- rutils::roll_sum(x_ts=betas_running$coefficients, win_dow=11)/11
tail(betas_rolling)


### forecast the returns from today's factors times the lagged betas

# lag the betas by a single period
betas_lagged <- rutils::lag_xts(betas_running$coefficients)

# forecast the returns from today's factors in SPY_design times the lagged betas
# note that "weight_s" has minus sign, because betas_running are
# calculated in the opposite sign by roll_pcr()
weight_s <-
  -rowSums(betas_lagged[, -1]*SPY_design[index(betas_running$coefficients)]) -
  betas_lagged[, 1]


### perform backtests: invest proportional to weight_s

### trade immediately at the close
# lag weight_s by one period into the future
weight_s <- rutils::lag_xts(weight_s)
# pnl_s <- exp(cumsum(weight_s * returns_running))
# or, invest fixed notional using sign()
pnl_s <- exp(cumsum(sign(weight_s) * returns_running))

### or, trade at the open in the next period
# lag weight_s two periods into future
weight_s <- rutils::lag_xts(weight_s, lag=2)
# calculate open to open returns
returns_open <- HighFreq::run_returns(x_ts=SPY, col_umn=1, sca_le=FALSE)
colnames(returns_open) <- "returns_advanced"
pnl_s <- exp(cumsum(sign(weight_s) * returns_open))


### plotting

pnl_s <- back_test(de_sign=SPY_design[-ran_ge, ], beta_s=beta_s, re_turns=returns_running[-ran_ge, ], bid_offer=0.0, lag=1)
colnames(pnl_s) <- "backtest"

x11()

bench_mark <- SPY[index(pnl_s), 4]
bench_mark <- bench_mark / as.numeric(bench_mark[1, ])
bench_mark <- merge(pnl_s, bench_mark)[endpoints(bench_mark, on="days"), ]
tail(bench_mark)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(bench_mark, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
legend("topleft", legend=colnames(bench_mark),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

chart_Series(pnl_s, name="strategy cumulative returns")
chart_Series(x=SPY["2011/2012", 4], name="SPY prices")
add_TA(x=pnl_s, col='red', name="strategy cumulative returns")

# chart_Series(x=SPY["2011/2012", ], name="SPY prices")
# add_TA(x=pnl_s, col='red', name="strategy cumulative returns")

chart_Series(x=SPY["2011/2012", 4], name="SPY prices")
add_TA(x=betas_rolling[, "returns"], col='red', name="returns loading")

chart_Series(x=SPY["2011-07/2011-10", 4], name="SPY prices")
add_TA(x=betas_rolling["2011-07/2011-10", "returns"], col='red', name="returns loading")
add_TA(x=betas_rolling["2011-07/2011-10", "hurst"], col='red', name="hurst loading")



### test for data snooping in PCR using random data

# perform one random PCR simulation using function run_pcr() (below)
pnl_s <- run_pcr(SPY, de_sign=SPY_design)
pnl_s <- run_pcr(SPY, de_sign=SPY_design, trade_the_close=FALSE)
chart_Series(x=pnl_s, name="strategy cumulative returns")


# takes very long!!! - perform 100 random PCR simulations - takes very long!!!
pnl_s <- sapply(1:100, function(x) sum(run_pcr(SPY, de_sign=SPY_design, random_ize=TRUE)))
pnl_s <- sapply(1:100, function(x) sum(run_pcr(random_ize=TRUE)))
sum(pnl_s>0)/length(pnl_s)
x11()
hist(pnl_s, breaks="FD", xlim=c(-5e6, 5e6), main="distribution of Pnl's")

# run PCR model and return the Pnl
run_pcr <- function(oh_lc=NULL, de_sign=NULL, trade_the_close=TRUE, random_ize=FALSE) {
  if (random_ize)
    oh_lc <- HighFreq::random_OHLC(oh_lc=oh_lc)
  if (is.null(de_sign))
    de_sign <- get_design(oh_lc, win_dow=60)
  # calculate close to close returns
  returns_running <- HighFreq::run_returns(x_ts=oh_lc)
  # calculate returns advanced in time
  returns_advanced <- rutils::lag_xts(returns_running, lag=-1)
  colnames(returns_advanced) <- "returns_advanced"
  # perform rolling forecasting PCR regressions in parallel
  # use only the first principal component: argument "comps"
  betas_running <- roll_pcr(x=de_sign,
                            y=returns_advanced,
                            width=1*60, comps=1:1, min_obs=1)
  betas_running$coefficients[1, ] <- 0
  # lag the betas by a single period
  betas_lagged <- rutils::lag_xts(betas_running$coefficients)
  # forecast the returns from today's factors in de_sign times the lagged betas
  # note that "weight_s" has minus sign, because betas_running are
  # calculated in the opposite sign by roll_pcr()
  weight_s <-
    -rowSums(betas_lagged[, -1]*de_sign[index(betas_running$coefficients)]) -
    betas_lagged[, 1]
  if (trade_the_close) {
    # trade immediately at the close
    # lag weight_s by one period into the future
    weight_s <- rutils::lag_xts(weight_s)
  }
  else {
    # trade at the open in the next period
    # lag weight_s two periods into future
    weight_s <- rutils::lag_xts(weight_s, lag=2)
    # calculate open to open returns
    returns_running <- HighFreq::run_returns(x_ts=oh_lc, col_umn=1)
  }
  cumsum(sign(weight_s) * returns_running)
}  # end run_pcr


# create a design matrix from OHLC data
get_design <- function(oh_lc, win_dow) {
  returns_running <- HighFreq::run_returns(x_ts=oh_lc)
  returns_rolling <- HighFreq::roll_vwap(oh_lc=oh_lc, x_ts=returns_running, win_dow=win_dow)
  var_running <- run_variance(oh_lc=oh_lc)
  skew_running <- run_skew(oh_lc=oh_lc)
  hurst_rolling <- roll_hurst(oh_lc=oh_lc, win_dow=win_dow)
  de_sign <- cbind(returns_running, returns_rolling, var_running, skew_running, hurst_rolling, returns_running*var_running, returns_running*skew_running)
  de_sign <- roll::roll_scale(data=de_sign, width=60, min_obs=1)
  core_data <- coredata(de_sign)
  core_data[is.na(core_data)] <- 0
  de_sign <- xts(x=core_data, order.by=index(de_sign))
  colnames(de_sign) <- c("returns", "returns.WA5", "variance", "skew", "hurst", "rets_var", "rets_skew")
  de_sign
}  # end get_design

SPY_design <- get_design(SPY, win_dow=60)


back_test <- function(de_sign=NULL, beta_s=NULL, re_turns=NULL, lag=1, bid_offer=0.0) {
  weight_s <- de_sign %*% beta_s[-1]
  weight_s <- rutils::lag_it(weight_s, lag=lag)
  if (lag > 1)
    weight_s <- rutils::roll_sum(weight_s, win_dow=lag) / lag
  # calculate pnl_s and scale them to SPY volatility
  pnl_s <- (weight_s * re_turns)
  fac_tor <- sd(diff_xts(log(SPY[index(pnl_s), 4])))/sd(pnl_s)
  pnl_s <- fac_tor*pnl_s
  # calculate transaction costs
  cost_s <- fac_tor*bid_offer*abs(rutils::diff_it(weight_s))
  pnl_s <- exp(cumsum(pnl_s - cost_s))
  colnames(pnl_s) <- "backtest"
  pnl_s
}  # end back_test

### below are scratch scripts

returns_ratio <- ifelse(returns_running != 0, returns_advanced/returns_running, 0)
colnames(returns_ratio) <- "returns ratio"
sum(is.na(returns_ratio))

bar <- merge(SPY_design["2011", "returns"], returns_ratio["2011"])
bar <- merge(SPY_design["2011", "variance"], returns_ratio["2011"])
bar <- bar[bar[, 2] != 0]
bar <- bar[abs(bar[, 2]) < 10]
tail(coredata(bar), 33)
plot(coredata(bar))

foo <- bar[bar[, 1] > 5, ]
head(foo)
sum(foo[, 2] > 0)/NROW(foo)
sum(foo[, 2])

# simple contrarian strategy works better than PCR
foo <- -sign(rutils::lag_xts(returns_running))
sum(foo*returns_running)
chart_Series(x=cumsum(foo*returns_running), name="strategy cumulative returns")

