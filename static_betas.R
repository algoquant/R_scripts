####################################
### Scripts for forecasting stock returns
### using static betas over design matrix
####################################

## model description:
# sig_nal is calculated as weighted average of static beta_s times design matrix.
# beta_s are calibrated once over a single interval, and then applied out-of-sample to the remaining data.

## models saved in the file static_betas.Rdata:
# optim_static: base model of mean-reverting (contrarian) strategy with threshold: 
#  sell when sig_nal exceeds threshold, and hold position until another trade arrives, 
#  buy when sig_nal is below (-threshold), and hold position until another trade arrives, 
#  use separate beta_s for buy signal and separate for sell signal, 
#  without LASSO penalty.
# optim_multi_lasso: list of contrarian strategies with different LASSO penalties.
#  run the strategies by calculating betas_lasso, positions_lasso, and performance_lasso.


## old models saved in the file static_betas 04-28-17.RData:
# optim_annual: mean-reverting (contrarian) strategy with threshold: 
#  sell when sig_nal exceeds threshold, hold, and buy when sig_nal is below (-threshold).
# optim_stat_arb: apply statistical arbitrage trading rule: go flat if sig_nal is close to zero.
# optim_multi_annual: calibrate separate out-of-sample beta_s for each annual interval.
# optim_pca: perform PCA of design matrix, and create new design matrix from first few principal components.
# optim_multi_annual_pca: use PCA design matrix, and calibrate separate out-of-sample beta_s for each annual interval.
# optim_binom: each column of design matrix produces its own sig_nal,
#  sell when all sig_nals exceed threshold, hold, and buy when all sig_nals are below (-threshold)


## open to-do:

# Study effect of using different window size for centering and scaling of the design matrix.

# Test for leakage by re-sampling SPY and re-calculating design matrix.


## closed to-do:

# Apply LASSO shrinkage.
#  result: works very well, and prevents betas from becoming too large,
#  also reduces number of trades. 
#  Out-of-sample PnL increases with stronger shrinkage, but then collapses, 
#  for lamb_da greater than 1, as signal becomes too small and number of trades collapses.
#  If the trading frequency is less than around one trade every 6 minutes, then the PnL collapses. 

# Plot daily chart with shading.
#  result: works well, and shows that strategy isn't able to time peaks and troughs very well,
#  but gets enough right to make money.

# Create trading rule: buy and hold for a fixed interval, and then revert to short.
#  result: works decent, but doesn't out-perform standard optim_static rule, and trades just as frequently.

# Create shiny app that allows changing betas for a static betas strategy 
#  with buy and hold for a fixed interval, 
#  and displays the buy signals: shiny_static_betas_strategy.Rmd

# Create shiny app that allows changing betas and displays the buy signals: shiny_static_betas.Rmd

# Combine strategies: standard rule optim_static and optim_stat_arb
#  result: works well because introduces diversification

# Create trading rule optim_stat_arb: go flat if sig_nal is close to zero, similar to statistical arbitrage rule.
#  result: works well, but doesn't out-perform standard optim_static rule.

# Calculate annual betas over out-of-sample multi-year intervals, and compare to previous method.
#  result: works well

# Perform principal component regressions over annual intervals.
#  result: doesn't work because the factors: returns, variance, skew, and hurst are not correlated.

# Create double-down strategy: buy (sell) more if another buy (sell) signal arrives.
#  apply cumsum() to buy and sell time series, instead of locf().
#  result: builds up beta and doesn't work

# Create strategies which don't optimize total PnL:
#  objective function: alpha (doesn't out-perform)
#  objective function: penalize by number of trades (doesn't out-perform)

# Create trading rule optim_binom: 
#  compare each factor with 1, and sell if all are above 1, and buy if all are below 1.
#  result: doesn't work out-of-sample because of model over-fitting.

# bug fix: dual beta model performed worse: there was a bug in cum_pnl() - objective function should return cumulative return, not mark-to-market



### 

library(HighFreq)
options(max.print=40)
oh_lc <- HighFreq::SPY["/2008-03"]
oh_lc <- HighFreq::SPY["/2008"]
oh_lc <- HighFreq::SPY["2009-03"]
oh_lc <- HighFreq::SPY["2010-05-06"]
# define calibration range
ran_ge <- match(index(oh_lc), index(SPY_design))
in_dex <- index(oh_lc)
n_row <- NROW(oh_lc)

# load design matrix called SPY_design containing columns of data aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)

### calculate close to close percentage returns
returns_running <- 60*HighFreq::run_returns(x_ts=HighFreq::SPY)
colnames(returns_running) <- "returns"
# calculate returns advanced in time
returns_advanced <- rutils::lag_xts(returns_running, lag=-1)
colnames(returns_advanced) <- "returns_advanced"


### Calculate SPY_design design matrix, containing columns of data aggregations
# scale to daily returns
win_dow <- 5
returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=HighFreq::SPY)
returns_rolling <- roll_vwap(oh_lc=HighFreq::SPY, x_ts=returns_running, win_dow=win_dow)
colnames(returns_running) <- "returns"
colnames(returns_rolling) <- "returns.roll"

var_running <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY)
var_rolling <- HighFreq::roll_vwap(oh_lc=HighFreq::SPY, x_ts=var_running, win_dow=win_dow)
colnames(var_running) <- "variance"
colnames(var_rolling) <- "variance.roll"

skew_running <- 6.5*60^4*HighFreq::run_skew(oh_lc=HighFreq::SPY)
skew_rolling <- roll_vwap(oh_lc=HighFreq::SPY, x_ts=skew_running, win_dow=win_dow)
colnames(skew_running) <- "skew"
colnames(skew_rolling) <- "skew.roll"

# sharpe_running <- HighFreq::run_sharpe(oh_lc=HighFreq::SPY)
# sharpe_rolling <- roll_vwap(oh_lc=HighFreq::SPY, x_ts=sharpe_running, win_dow=win_dow)
# colnames(sharpe_running) <- "sharpe"
# colnames(sharpe_rolling) <- "sharpe.roll"
#
# sharpe_rolling <- HighFreq::roll_sharpe(oh_lc=HighFreq::SPY, win_dow=win_dow)
# colnames(sharpe_rolling) <- "sharpe.roll"

hurst_rolling <- HighFreq::roll_hurst(oh_lc=HighFreq::SPY, win_dow=win_dow)
colnames(hurst_rolling) <- "hurst.roll"

# select most significant factors plus interaction terms
# SPY_design <- cbind(returns_running, returns_rolling, var_running, var_rolling, skew_running, skew_rolling, sharpe_running, sharpe_rolling, hurst_rolling)
# SPY_design <- cbind(returns_running, returns_rolling, var_running, skew_running,
#                     hurst_rolling, returns_running*var_running, returns_running*skew_running)
# colnames(SPY_design) <- c(colnames(SPY_design)[1:4], "hurst", "rets_var", "rets_skew")
SPY_design <- cbind(returns_running, var_running, skew_running, hurst_rolling)
colnames(SPY_design) <- c("returns", "variance", "skew", "hurst")

# apply rolling centering and scaling to the design matrix
library(roll)
SPY_design <- roll::roll_scale(data=SPY_design, width=30, min_obs=1)
# remove NAs
SPY_design[is.na(SPY_design)] <- 0
sum(is.na(SPY_design))

# save(SPY_design, file="C:/Develop/data/SPY_design.RData")

# end calculate SPY_design design matrix


### perform regression of returns_advanced versus de_sign in first quarter of data
# negative coefficients indicate that this is a contrarian strategy
# ran_ge <- 1:(NROW(SPY_design) %/% 4)
# ran_ge <- index(oh_lc)
ran_ge <- match(index(oh_lc), index(SPY_design))
for_mula <- returns_advanced[ran_ge, ] ~ SPY_design[ran_ge, ] - 1
mod_el <- lm(for_mula)
beta_s <- summary(mod_el)$coefficients[, "Estimate"]
names(beta_s) <- sapply(names(beta_s), function(x) strsplit(x, split="]")[[1]][2])


### calculate the forecast sig_nal by applying beta_s out-of-sample to the remaining data
sig_nal <- SPY_design %*% beta_s
colnames(sig_nal) <- "signal"
# lag sig_nal by one period
# sig_nal <- rutils::lag_xts(sig_nal)
# calculate average of sig_nal over past, to improve forecasts
# sig_nal <- rutils::roll_sum(sig_nal, win_dow=3) / 3


### calculate hit rates by signal quantiles
hit_s <- sign(sig_nal * returns_running)[-ran_ge, ]
hit_s <- cbind(sig_nal[-ran_ge, ], hit_s)
colnames(hit_s) <- c("signal", "hits")
x11()
# histogram of sig_nal
histo_gram <- hist(hit_s[, "signal"], breaks=100, xlim=c(-10, 15))
histo_gram$breaks
# quantiles of sig_nal
quantile_s <- quantile(hit_s[, "signal"], probs=seq(0.05, 0.95, 0.1))
# extreme quantiles have higher hit rates
sapply(seq_along(quantile_s)[-1], function(x)
  NROW(hit_s[(hit_s[, "signal"]>=quantile_s[x-1]) & (hit_s[, "signal"]<quantile_s[x]), "hits"]))
sapply(seq_along(quantile_s)[-1], function(x)
  sum(hit_s[(hit_s[, "signal"]>=quantile_s[x-1]) & (hit_s[, "signal"]<quantile_s[x]), "hits"]))


### proportional strategy: invest proportional to sig_nal - but it trades too much and takes too much risk
# calculate out-of-sample pnl_s
position_s <- rutils::lag_xts(sig_nal)
# pnl_s <- exp(cumsum(sign(position_s) * returns_running[-ran_ge, ]))
pnl_s <- (position_s * returns_running)#[-ran_ge, ]
# scale pnl_s to SPY volatility
pnl_s <- pnl_s*sd(diff_xts(log(HighFreq::SPY[index(pnl_s), 4])))/sd(pnl_s)
pnl_s <- exp(cumsum(pnl_s))


###############
### contrarian strategy with threshold:
#  sell when sig_nal exceeds threshold, hold, and buy when sig_nal is below (-threshold)

# thresh_old <- 1.0
position_s <- rep.int(NA, NROW(SPY_design))
position_s[1] <- 0
# position_s[sig_nal < -thresh_old] <- 1.0
# position_s[sig_nal > thresh_old] <- -1.0
sig_nal <- SPY_design %*% beta_s[1:NCOL(SPY_design)]
position_s[sig_nal < -1] <- 1.0
# position_s[abs(sig_nal) < 0.1] <- 0.0
sig_nal <- SPY_design %*% beta_s[(NCOL(SPY_design)+1):(2*NCOL(SPY_design))]
position_s[sig_nal > 1] <- -1.0
# position_s[abs(sig_nal) < 0.1] <- 0.0
# position_s[ran_ge] <- 0.0
position_s <- zoo::na.locf(position_s)
# lag the position_s
position_s <- rutils::lag_it(position_s)
# position_s <- c(0, position_s[-NROW(position_s)])
# position_s <- xts(position_s, order.by=index(returns_running))
# position_s <- cbind(sig_nal, position_s)
# colnames(position_s)[2] <- "positions"
# calculate cumulative PnL
pnl_s <- exp(cumsum((position_s * returns_running)))-1
# pnl_s <- exp(cumsum(((position_s * returns_running)[ran_ge, ])))
colnames(pnl_s) <- "SPY static betas"
last(pnl_s)
# pnl_s <- cumsum(position_s[, 2]*re_turns)
# chart_Series(pnl_s)


###############
### contrarian double down strategy with threshold: 
#  sell when sig_nal exceeds threshold, hold, and buy when sig_nal is below (-threshold)
#  double-down: buy (sell) more if another buy (sell) signal arrives.
#  apply cumsum() to buy and sell time series, instead of locf().

# thresh_old <- 1.0
position_s <- rep.int(0, NROW(SPY_design))
# position_s[1] <- 0
# position_s[sig_nal < -thresh_old] <- 1.0
# position_s[sig_nal > thresh_old] <- -1.0
sig_nal <- SPY_design %*% beta_s[1:NCOL(SPY_design)]
position_s[sig_nal < -1] <- 1.0
position_s <- cumsum(position_s)
# position_s[abs(sig_nal) < 0.1] <- 0.0
sig_nal <- SPY_design %*% beta_s[(NCOL(SPY_design)+1):(2*NCOL(SPY_design))]
position_s[sig_nal > 1] <- -1.0
# position_s[abs(sig_nal) < 0.1] <- 0.0
# position_s[ran_ge] <- 0.0
position_s <- zoo::na.locf(position_s)
# lag the position_s
position_s <- rutils::lag_it(position_s)
# position_s <- c(0, position_s[-NROW(position_s)])
# position_s <- xts(position_s, order.by=index(returns_running))
# position_s <- cbind(sig_nal, position_s)
# colnames(position_s)[2] <- "positions"
# calculate cumulative PnL
pnl_s <- exp(cumsum((position_s * returns_running)))-1
# pnl_s <- exp(cumsum(((position_s * returns_running)[ran_ge, ])))
colnames(pnl_s) <- "SPY static betas"
last(pnl_s)
# pnl_s <- cumsum(position_s[, 2]*re_turns)
# chart_Series(pnl_s)


### cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(beta_s, de_sign=SPY_design, re_turns=returns_running, lamb_da=0) {
  n_col <- NCOL(de_sign)
  position_s <- rep.int(NA, NROW(de_sign))
  position_s[1] <- 0
  # long signal
  sig_nal <- de_sign %*% beta_s[1:n_col]
  position_s[sig_nal < -1] <- 1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  # short signal
  sig_nal <- de_sign %*% beta_s[(n_col+1):(2*n_col)]
  position_s[sig_nal > 1] <- -1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  position_s <- zoo::na.locf(position_s)
  # position_s <- rutils::lag_it(position_s)
  position_s <- c(0, position_s[-NROW(position_s)])
  # total PnL
  # -exp(sum(position_s*re_turns))
  # average PnL per trade
  # pnl_s <- position_s*re_turns
  # be_ta <- (sum(pnl_s * re_turns) - sum(pnl_s) * sum(re_turns)) / (sum(pnl_s * pnl_s) - sum(pnl_s)^2 )
  # -(exp(sum(pnl_s) - be_ta * sum(re_turns)) - 1)
  # -(exp(sum(position_s*re_turns))-1) # / (sum(abs(rutils::diff_it(position_s))) / 2/ 1e5) / abs(sum(position_s>0) - sum(position_s<0))
  -((exp(sum(position_s*re_turns))-1) - lamb_da*sum(abs(beta_s)))
}  # end cum_pnl

cum_pnl(beta_s=beta_s, de_sign=SPY_design[ran_ge], re_turns=returns_running[ran_ge])
cum_pnl(beta_s=beta_s)

# perform calibration over oh_lc interval
op_tim <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, 2*NCOL(SPY_design)),
                           lower=rep(-2, 2*NCOL(SPY_design)), 
                           de_sign=SPY_design[in_dex], 
                           re_turns=returns_running[in_dex],
                           lamb_da=lamb_da,
                           control=list(trace=FALSE, itermax=200, parallelType=1, packages="rutils"))


## LASSO calibration over lambda_s - takes very long!
lambda_s <- 2^(-(-3:3)/3)
optim_lasso <- lapply(lambda_s, function(lamb_da) 
  DEoptim::DEoptim(fn=cum_pnl,
                   upper=rep(2, 2*NCOL(SPY_design)),
                   lower=rep(-2, 2*NCOL(SPY_design)), 
                   de_sign=SPY_design[in_dex], 
                   re_turns=returns_running[in_dex],
                   lamb_da=lamb_da,
                   control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))
)  # end lapply
names(optim_lasso) <- paste0("lambda=", round(lambda_s, 4))


## calculate matrix of betas
betas_lasso <- sapply(optim_lasso, function(op_tim) op_tim$optim$bestmem
)  # end sapply
betas_lasso <- t(betas_lasso)
colnames(betas_lasso) <- c(paste0(colnames(SPY_design), "_long"), paste0(colnames(SPY_design), "_short"))
rownames(betas_lasso) <- round(lambda_s, 3)


## calculate matrix of strategy positions
# cum_pnl should return the vector position_s
positions_lasso <- lapply(seq_along(lambda_s), function(i_ter) 
  cum_pnl(beta_s=betas_lasso[i_ter, ], 
          de_sign=SPY_design, 
          re_turns=returns_running)
)  # end lapply

# flatten list into xts
positions_lasso <- rutils::do_call(cbind, positions_lasso)
colnames(positions_lasso) <- paste0("lamb_da=", rownames(betas_lasso))
position_s <- positions_lasso[, 4]

## calculate matrix of pnl_s and trades_per_day, in and out-of-sample
performance_lasso <- apply(positions_lasso, MARGIN=2, function(position_s) 
  c(pnls=(exp(sum(position_s*returns_running))-1),
    trades_per_day=sum(abs(rutils::diff_it(position_s))) / 2 / NROW(endpoints(SPY_design, on="days")),
    holding_period=2*NROW(position_s) / sum(abs(rutils::diff_it(position_s))))
)  # end apply
performance_lasso <- t(performance_lasso)
beta_s <- betas_lasso[4, ]


## perform calibration over all annual intervals
year_s <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014")
optim_annual <- lapply(year_s, function(year) 
  DEoptim::DEoptim(fn=cum_pnl,
                   upper=rep(4000, 2*NCOL(SPY_design)),
                   lower=rep(-4000, 2*NCOL(SPY_design)), 
                   de_sign=SPY_design[year], 
                   re_turns=returns_running[year],
                   control=list(trace=FALSE, itermax=100, parallelType=1, packages="rutils"))
)  # end lapply


# perform calibration over all multi-annual intervals
optim_multi_annual_pca <- lapply(1:(NROW(year_s)-1), function(year) {
  year_s <- year_s[1:year]
  de_sign <- SPY_design[year_s]
  re_turns <- returns_running[year_s]
  DEoptim::DEoptim(fn=cum_pnl,
                   upper=rep(4000, 2*NCOL(SPY_design)),
                   lower=rep(-4000, 2*NCOL(SPY_design)), 
                   de_sign=de_sign, 
                   re_turns=re_turns,
                   control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))
})  # end lapply


# calculate annual betas
betas_annual <- sapply(optim_annual, function(op_tim) op_tim$optim$bestmem
)  # end sapply
betas_annual <- t(betas_annual)
colnames(betas_annual) <- c(paste0(colnames(SPY_design), "_long"), paste0(colnames(SPY_design), "_short"))
rownames(betas_annual) <- year_s

### calculate annual pnl_s in-sample
# cum_pnl should return the vector (position_s*re_turns)
pnl_s <- lapply(year_s, function(year) 
  cum_pnl(beta_s=betas_annual[year, ], 
          de_sign=SPY_design[year], 
          re_turns=returns_running[year])
)  # end lapply

# flatten list into xts
pnl_s <- rutils::do_call(rbind, pnl_s)
pnl_s <- exp(cumsum(pnl_s)) - 1
colnames(pnl_s) <- "SPY static betas"


### calculate annual pnl_s out-of-sample
# loop over annual intervals
pnl_s <- lapply(seq_along(year_s), function(year) {
  year1 <- year_s[year]
  de_sign <- SPY_design[year1]
  re_turns <- returns_running[year1]
  # loop over out-of-sample betas
  # cum_pnl should return the vector (position_s*re_turns)
  pnl_s <- lapply(year_s[-year], function(year2) 
    cum_pnl(beta_s=betas_annual[year2, ], 
            de_sign=de_sign, 
            re_turns=re_turns)
  )  # end lapply
  # calculate average over out-of-sample pnl_s
  pnl_s <- rutils::do_call(cbind, pnl_s)
  rowSums(pnl_s)/NCOL(pnl_s)
})  # end lapply

# flatten list into xts
pnl_s <- rutils::do_call(c, pnl_s)
pnl_s <- exp(cumsum(pnl_s)) - 1
pnl_s <- xts(pnl_s, order.by=index(SPY_design))
colnames(pnl_s) <- "SPY static betas"


###############
### cum_pnl function version with binomial sig_nal
cum_pnl <- function(beta_s, de_sign=t(SPY_design), re_turns=returns_running) {
  n_col <- NROW(de_sign)
  position_s <- rep.int(NA, NCOL(de_sign))
  position_s[1] <- 0
  # long signal
  # compare signal with 1, and sell if all are above 1, and buy if all are below 1
  sig_nal <- de_sign * beta_s[1:n_col]
  # sig_nal <- de_sign %*% beta_s[1:n_col]
  position_s[colSums(sig_nal < -1) == n_col] <- 1.0
  # position_s[sig_nal < -1] <- 1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  # short signal
  sig_nal <- de_sign * beta_s[(n_col+1):(2*n_col)]
  # sig_nal <- de_sign %*% beta_s[(n_col+1):(2*n_col)]
  position_s[colSums(sig_nal > 1) == n_col] <- -1.0
  # position_s[sig_nal > 1] <- -1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  position_s <- zoo::na.locf(position_s)
  # position_s <- rutils::lag_it(position_s)
  position_s <- c(0, position_s[-NROW(position_s)])
  # total PnL
  # -exp(sum(position_s*re_turns))
  # average PnL per trade
  pnl_s <- position_s*re_turns
  be_ta <- (sum(pnl_s * re_turns) - sum(pnl_s) * sum(re_turns)) / (sum(pnl_s * pnl_s) - sum(pnl_s)^2 )
  -(exp(sum(pnl_s) - be_ta * sum(re_turns)) - 1)
  # -(exp(sum(position_s*re_turns))-1) # /(sum(abs(rutils::diff_it(position_s))) / 2/ 1e5)
}  # end cum_pnl

cum_pnl(beta_s=beta_s, de_sign=t(SPY_design[ran_ge]), 
        re_turns=returns_running[ran_ge])
cum_pnl(beta_s=beta_s)

# calculate PnLs
# sapply(seq(10.0, 30.0, by=2.0), cum_pnl)
# sapply(seq(0.1, 1.0, by=0.1), cum_pnl)

op_tim <- DEoptim::DEoptim(fn=cum_pnl, 
                           upper=rep(4000, 2*NCOL(SPY_design)), 
                           lower=rep(-4000, 2*NCOL(SPY_design)), 
                           de_sign=t(SPY_design[ran_ge]), 
                           re_turns=returns_running[ran_ge], 
                           control=list(trace=FALSE, itermax=100, parallelType=1, packages="rutils"))

# list models
ls(pattern=glob2rx("op*"))
# create list with optimal PCA parameters and model
optim_pca <- list(rotation=p_ca$rotation, model=op_tim)
# load("C:/Develop/data/static_betas.Rdata")
# save(list=ls(pattern=glob2rx("op*")), file="C:/Develop/data/static_betas.Rdata")
summary(op_tim)
x11()
plot(op_tim)
beta_s <- op_tim$optim$bestmem
names(beta_s) <- c(paste0(colnames(SPY_design), "_long"), paste0(colnames(SPY_design), "_short"))
# names(beta_s) <- colnames(SPY_design)
op_tim$optim$bestval
cum_pnl(beta_s, de_sign=SPY_design[ran_ge])


### backtest() function for contrarian strategy with threshold
pnl_s <- back_test(de_sign=SPY_design[-ran_ge, ], beta_s=beta_s, re_turns=returns_running[-ran_ge, ], bid_offer=0.0, lag=1)

pnl_s <- back_test(de_sign=SPY_design[-ran_ge, ], beta_s=beta_s, thresh_old=thresh_old, re_turns=returns_running[-ran_ge, ], bid_offer=0.0, lag=4)

# loop over threshold_s
threshold_s <- seq(0.1, 3.0, by=0.1)
foo <- sapply(threshold_s, function(thresh_old)
  last(back_test(de_sign=SPY_design[-ran_ge, ], beta_s=beta_s, thresh_old=thresh_old, re_turns=returns_running[-ran_ge, ], bid_offer=0.0, lag=4)))
names(foo) <- threshold_s


### plotting

x11()
# colnames(pnl_s) <- "backtest"
# back_test <- cbind(exp(cumsum(returns_running[ran_ge])), pnl_s[ran_ge])[endpoints(pnl_s, on="days")]
back_test <- cbind(exp(cumsum(returns_running[index(pnl_s)]))-1, pnl_s)[endpoints(pnl_s, on="days")]
# tail(back_test)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(back_test, theme=plot_theme,
             name="Backtest of static beta strategy for SPY")
legend("topleft", legend=colnames(back_test),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

## plot with shading

bu_y <- xts(SPY_design %*% beta_s[1:NCOL(SPY_design)], order.by=index(SPY_design))
# position_s[bu_y < -1] <- 1.0
se_ll <- xts(SPY_design %*% beta_s[(NCOL(SPY_design)+1):(2*NCOL(SPY_design))], order.by=index(SPY_design))
# position_s[se_ll > 1] <- -1.0

ran_ge <- "2010-05-05/2010-05-07"
ran_ge <- match(index(SPY_design[ran_ge]), index(SPY_design))
back_test <- cbind(exp(cumsum(returns_running[ran_ge]))-1, 
                   exp(cumsum(position_s[ran_ge]*returns_running[ran_ge]))-1)
colnames(back_test) <- c("SPY", "backtest")

back_test <- cbind(exp(cumsum(returns_running[ran_ge]))-1, 
                   bu_y[ran_ge])
colnames(back_test) <- c("SPY", "buy signal")

back_test <- cbind(bu_y[ran_ge], se_ll[ran_ge])
colnames(back_test) <- c("buy signal", "sell signal")

plot.zoo(back_test)
plot.zoo(cbind(HighFreq::SPY[ran_ge, 4], SPY_design[ran_ge, "variance"]))
plot.zoo(cbind(HighFreq::SPY[ran_ge, 4], SPY_design[ran_ge, 2:3]))

plot(as.numeric(HighFreq::SPY[ran_ge, 4]), t="l")
abline(v=which(bu_y[ran_ge] < -1), col="red", lwd=1)
abline(v=which(se_ll[ran_ge] > 1), col="blue", lwd=1)
abline(v=which(SPY_design[ran_ge, "variance"] > 1), col="red", lwd=1)
abline(v=which(SPY_design[ran_ge, 3] > 1), col="red", lwd=1)
abline(v=which((SPY_design[ran_ge, 2] > 2) | (SPY_design[ran_ge, 3] > 1)), col="red", lwd=1)


sha_de <- xts(position_s[ran_ge], order.by=index(returns_running[ran_ge]))

chart_Series(back_test, theme=plot_theme,
             name="Backtest of static beta strategy for SPY")
add_TA(sha_de > 0, on=-1, col="lightgreen", border="lightgreen")
add_TA(sha_de < 0, on=-1, col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(back_test),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")


## plot using dygraphs
library(dygraphs)
dygraph(back_test, main="Backtest of static beta strategy for SPY") %>%
  dyOptions(colors=c("orange", "blue")) %>%
  dyRangeSelector()

## plot using plotly
library(plotly)
data.frame(dates=index(back_test), coredata(back_test)) %>%
  plot_ly(x=~dates, y=~backtest, type="scatter", mode="lines + markers", name="backtest") %>%
  add_trace(x=~dates, y=~SPY.Close, name="SPY") %>%
  layout(title="Backtest of static beta strategy for SPY",
         xaxis=list(title="Time"),
         yaxis=list(title="PnL"),
         legend=list(x=0.1, y=0.9))


### back_test function
back_test <- function(de_sign=NULL, beta_s=NULL, thresh_old=NULL, re_turns=NULL, lag=1, bid_offer=0.0) {
  sig_nal <- de_sign %*% beta_s
  sig_nal <- rutils::lag_it(sig_nal, lag=lag)
  if (lag > 1)
    sig_nal <- rutils::roll_sum(sig_nal, win_dow=lag) / lag
  # calculate returns
  if (is.null(thresh_old)) {
    # calculate returns proportional to sig_nal and scale them to SPY volatility
    position_s <- sig_nal
    pnl_s <- (position_s * re_turns)
    fac_tor <- sd(diff_xts(log(HighFreq::SPY[index(pnl_s), 4])))/sd(pnl_s)
    pnl_s <- fac_tor*pnl_s
  }
  else {
    # calculate returns of contrarian strategy with threshold
    position_s <- rep.int(NA, NROW(sig_nal))
    position_s[1] <- 0.0
    position_s[sig_nal > thresh_old] <- 1.0
    position_s[sig_nal < -thresh_old] <- -1.0
    position_s <- zoo::na.locf(position_s)
    pnl_s <- position_s * re_turns
    fac_tor <- 1
  }
  # calculate transaction costs
  cost_s <- fac_tor*bid_offer*abs(rutils::diff_it(position_s))
  # calculate cumulative PnL
  pnl_s <- exp(cumsum(pnl_s - cost_s))
  colnames(pnl_s) <- "backtest"
  pnl_s
}  # end back_test


### older


colnames(pnl_s) <- "backtest"
back_test <- cbind(exp(cumsum(returns_running[index(pnl_s)])), pnl_s)[endpoints(pnl_s, on="days")]
# back_test <- SPY[index(pnl_s), 4]
# back_test <- back_test / as.numeric(back_test[1, ])
# back_test <- cbind(pnl_s, back_test)[endpoints(back_test, on="days"), ]
chart_Series(back_test, theme=plot_theme,
             name="Backtest of static beta strategy for SPY")
legend("topleft", legend=colnames(back_test),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

dates <- "2010-05-05/2010-05-07"
foo <- back_test[dates]
# foo <- sapply(foo, function(x) {x - as.numeric(x[1])})
foo[, 1] <- foo[, 1] - as.numeric(foo[1, 1])
foo[, 2] <- foo[, 2] - as.numeric(foo[1, 2])
chart_Series(foo, theme=plot_theme,
             name="Backtest of static beta strategy for SPY")
add_TA(position_s[dates] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(position_s[dates] < 0, on=-1,
       col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(foo),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

bar <- xts(sig_nal, order.by=index(returns_running))[dates]
back_test <- SPY[index(bar), 4]
back_test <- back_test - as.numeric(back_test[1, ])
bar <- cbind(back_test, bar)
chart_Series(bar, theme=plot_theme,
             name="Backtest of static beta strategy for SPY")
legend("topleft", legend=colnames(bar),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")
add_TA(bar[dates], lwd=2, on=1, col='blue')


### below are scratch scripts

# simple contrarian strategy works better than static beta?
foo <- -sign(rutils::lag_xts(returns_running))
sum(foo*returns_running)
chart_Series(x=cumsum(foo*returns_running), name="strategy cumulative returns")

