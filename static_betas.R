####################################
### Scripts for forecasting stock returns
### using static betas over design matrix
####################################

## model description:
# sig_nal is calculated as weighted average of static betas times design matrix.
# betas are calibrated once over a single interval, and then applied out-of-sample to the remaining data.

## models saved in the file static_betas.Rdata:
# optim_static: base model of mean-reverting (contrarian) strategy with threshold: 
#  sell when sig_nal exceeds threshold, and hold position until another trade arrives, 
#  buy when sig_nal is below (-threshold), and hold position until another trade arrives, 
#  use separate betas for buy signal and separate for sell signal, 
#  without LASSO penalty.
# optim_multi_lasso: list of contrarian strategies with different LASSO penalties.
#  run the strategies by calculating betas_lasso, positions_lasso, and performance_lasso.


## old models saved in the file static_betas 04-28-17.RData:
# optim_annual: mean-reverting (contrarian) strategy with threshold: 
#  sell when sig_nal exceeds threshold, hold, and buy when sig_nal is below (-threshold).
# optim_stat_arb: apply statistical arbitrage trading rule: go flat if sig_nal is close to zero.
# optim_multi_annual: calibrate separate out-of-sample betas for each annual interval.
# optim_pca: perform PCA of design matrix, and create new design matrix from first few principal components.
# optim_multi_annual_pca: use PCA design matrix, and calibrate separate out-of-sample betas for each annual interval.
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
#  for lambdav greater than 1, as signal becomes too small and number of trades collapses.
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
ohlc <- HighFreq::SPY["/2008-03"]
ohlc <- HighFreq::SPY["/2008"]
ohlc <- HighFreq::SPY["2009-03"]
ohlc <- HighFreq::SPY["2010-05-06"]
# define calibration range
rangev <- match(index(ohlc), index(SPY_design))
indeks <- index(ohlc)
n_row <- NROW(ohlc)

# load design matrix called SPY_design containing columns of data aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)

### calculate close to close percentage returns
returns_running <- 60*HighFreq::run_returns(xtes=HighFreq::SPY)
colnames(returns_running) <- "returns"
# calculate returns advanced in time
returns_advanced <- rutils::lagxts(returns_running, lag=-1)
colnames(returns_advanced) <- "returns_advanced"


### Calculate SPY_design design matrix, containing columns of data aggregations
# scale to daily returns
win_dow <- 5
returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=HighFreq::SPY)
returns_rolling <- roll_vwap(ohlc=HighFreq::SPY, xtes=returns_running, win_dow=win_dow)
colnames(returns_running) <- "returns"
colnames(returns_rolling) <- "returns.roll"

var_running <- 6.5*60^3*HighFreq::run_variance(ohlc=HighFreq::SPY)
var_rolling <- HighFreq::roll_vwap(ohlc=HighFreq::SPY, xtes=var_running, win_dow=win_dow)
colnames(var_running) <- "variance"
colnames(var_rolling) <- "variance.roll"

skew_running <- 6.5*60^4*HighFreq::run_skew(ohlc=HighFreq::SPY)
skew_rolling <- roll_vwap(ohlc=HighFreq::SPY, xtes=skew_running, win_dow=win_dow)
colnames(skew_running) <- "skew"
colnames(skew_rolling) <- "skew.roll"

# sharpe_running <- HighFreq::run_sharpe(ohlc=HighFreq::SPY)
# sharpe_rolling <- roll_vwap(ohlc=HighFreq::SPY, xtes=sharpe_running, win_dow=win_dow)
# colnames(sharpe_running) <- "sharpe"
# colnames(sharpe_rolling) <- "sharpe.roll"
#
# sharpe_rolling <- HighFreq::roll_sharpe(ohlc=HighFreq::SPY, win_dow=win_dow)
# colnames(sharpe_rolling) <- "sharpe.roll"

hurst_rolling <- HighFreq::roll_hurst(ohlc=HighFreq::SPY, win_dow=win_dow)
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


### perform regression of returns_advanced versus design in first quarter of data
# negative coefficients indicate that this is a contrarian strategy
# rangev <- 1:(NROW(SPY_design) %/% 4)
# rangev <- index(ohlc)
rangev <- match(index(ohlc), index(SPY_design))
formulav <- returns_advanced[rangev, ] ~ SPY_design[rangev, ] - 1
model <- lm(formulav)
betas <- summary(model)$coefficients[, "Estimate"]
names(betas) <- sapply(names(betas), function(x) strsplit(x, split="]")[[1]][2])


### calculate the forecast sig_nal by applying betas out-of-sample to the remaining data
sig_nal <- SPY_design %*% betas
colnames(sig_nal) <- "signal"
# lag sig_nal by one period
# sig_nal <- rutils::lagxts(sig_nal)
# calculate average of sig_nal over past, to improve forecasts
# sig_nal <- rutils::roll_sum(sig_nal, win_dow=3) / 3


### calculate hit rates by signal quantiles
hit_s <- sign(sig_nal * returns_running)[-rangev, ]
hit_s <- cbind(sig_nal[-rangev, ], hit_s)
colnames(hit_s) <- c("signal", "hits")
x11()
# histogram of sig_nal
histo_gram <- hist(hit_s[, "signal"], breaks=100, xlim=c(-10, 15))
histo_gram$breaks
# quantiles of sig_nal
quantiles <- quantile(hit_s[, "signal"], probs=seq(0.05, 0.95, 0.1))
# extreme quantiles have higher hit rates
sapply(seq_along(quantiles)[-1], function(x)
  NROW(hit_s[(hit_s[, "signal"]>=quantiles[x-1]) & (hit_s[, "signal"]<quantiles[x]), "hits"]))
sapply(seq_along(quantiles)[-1], function(x)
  sum(hit_s[(hit_s[, "signal"]>=quantiles[x-1]) & (hit_s[, "signal"]<quantiles[x]), "hits"]))


### proportional strategy: invest proportional to sig_nal - but it trades too much and takes too much risk
# calculate out-of-sample pnls
position_s <- rutils::lagxts(sig_nal)
# pnls <- exp(cumsum(sign(position_s) * returns_running[-rangev, ]))
pnls <- (position_s * returns_running)#[-rangev, ]
# scale pnls to SPY volatility
pnls <- pnls*sd(diff_xts(log(HighFreq::SPY[index(pnls), 4])))/sd(pnls)
pnls <- exp(cumsum(pnls))


###############
### contrarian strategy with threshold:
#  sell when sig_nal exceeds threshold, hold, and buy when sig_nal is below (-threshold)

# thresh_old <- 1.0
position_s <- rep.int(NA, NROW(SPY_design))
position_s[1] <- 0
# position_s[sig_nal < -thresh_old] <- 1.0
# position_s[sig_nal > thresh_old] <- -1.0
sig_nal <- SPY_design %*% betas[1:NCOL(SPY_design)]
position_s[sig_nal < -1] <- 1.0
# position_s[abs(sig_nal) < 0.1] <- 0.0
sig_nal <- SPY_design %*% betas[(NCOL(SPY_design)+1):(2*NCOL(SPY_design))]
position_s[sig_nal > 1] <- -1.0
# position_s[abs(sig_nal) < 0.1] <- 0.0
# position_s[rangev] <- 0.0
position_s <- zoo::na.locf(position_s)
# lag the position_s
position_s <- rutils::lagit(position_s)
# position_s <- c(0, position_s[-NROW(position_s)])
# position_s <- xts(position_s, order.by=index(returns_running))
# position_s <- cbind(sig_nal, position_s)
# colnames(position_s)[2] <- "positions"
# calculate cumulative PnL
pnls <- exp(cumsum((position_s * returns_running)))-1
# pnls <- exp(cumsum(((position_s * returns_running)[rangev, ])))
colnames(pnls) <- "SPY static betas"
last(pnls)
# pnls <- cumsum(position_s[, 2]*returns)
# chart_Series(pnls)


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
sig_nal <- SPY_design %*% betas[1:NCOL(SPY_design)]
position_s[sig_nal < -1] <- 1.0
position_s <- cumsum(position_s)
# position_s[abs(sig_nal) < 0.1] <- 0.0
sig_nal <- SPY_design %*% betas[(NCOL(SPY_design)+1):(2*NCOL(SPY_design))]
position_s[sig_nal > 1] <- -1.0
# position_s[abs(sig_nal) < 0.1] <- 0.0
# position_s[rangev] <- 0.0
position_s <- zoo::na.locf(position_s)
# lag the position_s
position_s <- rutils::lagit(position_s)
# position_s <- c(0, position_s[-NROW(position_s)])
# position_s <- xts(position_s, order.by=index(returns_running))
# position_s <- cbind(sig_nal, position_s)
# colnames(position_s)[2] <- "positions"
# calculate cumulative PnL
pnls <- exp(cumsum((position_s * returns_running)))-1
# pnls <- exp(cumsum(((position_s * returns_running)[rangev, ])))
colnames(pnls) <- "SPY static betas"
last(pnls)
# pnls <- cumsum(position_s[, 2]*returns)
# chart_Series(pnls)


### cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(betas, design=SPY_design, returns=returns_running, lambdav=0) {
  n_col <- NCOL(design)
  position_s <- rep.int(NA, NROW(design))
  position_s[1] <- 0
  # long signal
  sig_nal <- design %*% betas[1:n_col]
  position_s[sig_nal < -1] <- 1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  # short signal
  sig_nal <- design %*% betas[(n_col+1):(2*n_col)]
  position_s[sig_nal > 1] <- -1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  position_s <- zoo::na.locf(position_s)
  # position_s <- rutils::lagit(position_s)
  position_s <- c(0, position_s[-NROW(position_s)])
  # total PnL
  # -exp(sum(position_s*returns))
  # average PnL per trade
  # pnls <- position_s*returns
  # betav <- (sum(pnls * returns) - sum(pnls) * sum(returns)) / (sum(pnls * pnls) - sum(pnls)^2 )
  # -(exp(sum(pnls) - betav * sum(returns)) - 1)
  # -(exp(sum(position_s*returns))-1) # / (sum(abs(rutils::diffit(position_s))) / 2/ 1e5) / abs(sum(position_s>0) - sum(position_s<0))
  -((exp(sum(position_s*returns))-1) - lambdav*sum(abs(betas)))
}  # end cum_pnl

cum_pnl(betas=betas, design=SPY_design[rangev], returns=returns_running[rangev])
cum_pnl(betas=betas)

# perform calibration over ohlc interval
optimd <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, 2*NCOL(SPY_design)),
                           lower=rep(-2, 2*NCOL(SPY_design)), 
                           design=SPY_design[indeks], 
                           returns=returns_running[indeks],
                           lambdav=lambdav,
                           control=list(trace=FALSE, itermax=200, parallelType=1, packages="rutils"))


## LASSO calibration over lambda_s - takes very long!
lambda_s <- 2^(-(-3:3)/3)
optim_lasso <- lapply(lambda_s, function(lambdav) 
  DEoptim::DEoptim(fn=cum_pnl,
                   upper=rep(2, 2*NCOL(SPY_design)),
                   lower=rep(-2, 2*NCOL(SPY_design)), 
                   design=SPY_design[indeks], 
                   returns=returns_running[indeks],
                   lambdav=lambdav,
                   control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))
)  # end lapply
names(optim_lasso) <- paste0("lambda=", round(lambda_s, 4))


## calculate matrix of betas
betas_lasso <- sapply(optim_lasso, function(optimd) optimd$optim$bestmem
)  # end sapply
betas_lasso <- t(betas_lasso)
colnames(betas_lasso) <- c(paste0(colnames(SPY_design), "_long"), paste0(colnames(SPY_design), "_short"))
rownames(betas_lasso) <- round(lambda_s, 3)


## calculate matrix of strategy positions
# cum_pnl should return the vector position_s
positions_lasso <- lapply(seq_along(lambda_s), function(i_ter) 
  cum_pnl(betas=betas_lasso[i_ter, ], 
          design=SPY_design, 
          returns=returns_running)
)  # end lapply

# flatten list into xts
positions_lasso <- rutils::do_call(cbind, positions_lasso)
colnames(positions_lasso) <- paste0("lambdav=", rownames(betas_lasso))
position_s <- positions_lasso[, 4]

## calculate matrix of pnls and trades_per_day, in and out-of-sample
performance_lasso <- apply(positions_lasso, MARGIN=2, function(position_s) 
  c(pnls=(exp(sum(position_s*returns_running))-1),
    trades_per_day=sum(abs(rutils::diffit(position_s))) / 2 / NROW(endpoints(SPY_design, on="days")),
    holding_period=2*NROW(position_s) / sum(abs(rutils::diffit(position_s))))
)  # end apply
performance_lasso <- t(performance_lasso)
betas <- betas_lasso[4, ]


## perform calibration over all annual intervals
years <- c("2008", "2009", "2010", "2011", "2012", "2013", "2014")
optim_annual <- lapply(years, function(year) 
  DEoptim::DEoptim(fn=cum_pnl,
                   upper=rep(4000, 2*NCOL(SPY_design)),
                   lower=rep(-4000, 2*NCOL(SPY_design)), 
                   design=SPY_design[year], 
                   returns=returns_running[year],
                   control=list(trace=FALSE, itermax=100, parallelType=1, packages="rutils"))
)  # end lapply


# perform calibration over all multi-annual intervals
optim_multi_annual_pca <- lapply(1:(NROW(years)-1), function(year) {
  years <- years[1:year]
  design <- SPY_design[years]
  returns <- returns_running[years]
  DEoptim::DEoptim(fn=cum_pnl,
                   upper=rep(4000, 2*NCOL(SPY_design)),
                   lower=rep(-4000, 2*NCOL(SPY_design)), 
                   design=design, 
                   returns=returns,
                   control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))
})  # end lapply


# calculate annual betas
betas_annual <- sapply(optim_annual, function(optimd) optimd$optim$bestmem
)  # end sapply
betas_annual <- t(betas_annual)
colnames(betas_annual) <- c(paste0(colnames(SPY_design), "_long"), paste0(colnames(SPY_design), "_short"))
rownames(betas_annual) <- years

### calculate annual pnls in-sample
# cum_pnl should return the vector (position_s*returns)
pnls <- lapply(years, function(year) 
  cum_pnl(betas=betas_annual[year, ], 
          design=SPY_design[year], 
          returns=returns_running[year])
)  # end lapply

# flatten list into xts
pnls <- rutils::do_call(rbind, pnls)
pnls <- exp(cumsum(pnls)) - 1
colnames(pnls) <- "SPY static betas"


### calculate annual pnls out-of-sample
# loop over annual intervals
pnls <- lapply(seq_along(years), function(year) {
  year1 <- years[year]
  design <- SPY_design[year1]
  returns <- returns_running[year1]
  # loop over out-of-sample betas
  # cum_pnl should return the vector (position_s*returns)
  pnls <- lapply(years[-year], function(year2) 
    cum_pnl(betas=betas_annual[year2, ], 
            design=design, 
            returns=returns)
  )  # end lapply
  # calculate average over out-of-sample pnls
  pnls <- rutils::do_call(cbind, pnls)
  rowSums(pnls)/NCOL(pnls)
})  # end lapply

# flatten list into xts
pnls <- rutils::do_call(c, pnls)
pnls <- exp(cumsum(pnls)) - 1
pnls <- xts(pnls, order.by=index(SPY_design))
colnames(pnls) <- "SPY static betas"


###############
### cum_pnl function version with binomial sig_nal
cum_pnl <- function(betas, design=t(SPY_design), returns=returns_running) {
  n_col <- NROW(design)
  position_s <- rep.int(NA, NCOL(design))
  position_s[1] <- 0
  # long signal
  # compare signal with 1, and sell if all are above 1, and buy if all are below 1
  sig_nal <- design * betas[1:n_col]
  # sig_nal <- design %*% betas[1:n_col]
  position_s[colSums(sig_nal < -1) == n_col] <- 1.0
  # position_s[sig_nal < -1] <- 1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  # short signal
  sig_nal <- design * betas[(n_col+1):(2*n_col)]
  # sig_nal <- design %*% betas[(n_col+1):(2*n_col)]
  position_s[colSums(sig_nal > 1) == n_col] <- -1.0
  # position_s[sig_nal > 1] <- -1.0
  # position_s[abs(sig_nal) < 0.1] <- 0.0
  position_s <- zoo::na.locf(position_s)
  # position_s <- rutils::lagit(position_s)
  position_s <- c(0, position_s[-NROW(position_s)])
  # total PnL
  # -exp(sum(position_s*returns))
  # average PnL per trade
  pnls <- position_s*returns
  betav <- (sum(pnls * returns) - sum(pnls) * sum(returns)) / (sum(pnls * pnls) - sum(pnls)^2 )
  -(exp(sum(pnls) - betav * sum(returns)) - 1)
  # -(exp(sum(position_s*returns))-1) # /(sum(abs(rutils::diffit(position_s))) / 2/ 1e5)
}  # end cum_pnl

cum_pnl(betas=betas, design=t(SPY_design[rangev]), 
        returns=returns_running[rangev])
cum_pnl(betas=betas)

# calculate PnLs
# sapply(seq(10.0, 30.0, by=2.0), cum_pnl)
# sapply(seq(0.1, 1.0, by=0.1), cum_pnl)

optimd <- DEoptim::DEoptim(fn=cum_pnl, 
                           upper=rep(4000, 2*NCOL(SPY_design)), 
                           lower=rep(-4000, 2*NCOL(SPY_design)), 
                           design=t(SPY_design[rangev]), 
                           returns=returns_running[rangev], 
                           control=list(trace=FALSE, itermax=100, parallelType=1, packages="rutils"))

# list models
ls(pattern=glob2rx("op*"))
# create list with optimal PCA parameters and model
optim_pca <- list(rotation=p_ca$rotation, model=optimd)
# load("C:/Develop/data/static_betas.Rdata")
# save(list=ls(pattern=glob2rx("op*")), file="C:/Develop/data/static_betas.Rdata")
summary(optimd)
x11()
plot(optimd)
betas <- optimd$optim$bestmem
names(betas) <- c(paste0(colnames(SPY_design), "_long"), paste0(colnames(SPY_design), "_short"))
# names(betas) <- colnames(SPY_design)
optimd$optim$bestval
cum_pnl(betas, design=SPY_design[rangev])


### backtest() function for contrarian strategy with threshold
pnls <- back_test(design=SPY_design[-rangev, ], betas=betas, returns=returns_running[-rangev, ], bid_offer=0.0, lag=1)

pnls <- back_test(design=SPY_design[-rangev, ], betas=betas, thresh_old=thresh_old, returns=returns_running[-rangev, ], bid_offer=0.0, lag=4)

# loop over threshold_s
threshold_s <- seq(0.1, 3.0, by=0.1)
foo <- sapply(threshold_s, function(thresh_old)
  last(back_test(design=SPY_design[-rangev, ], betas=betas, thresh_old=thresh_old, returns=returns_running[-rangev, ], bid_offer=0.0, lag=4)))
names(foo) <- threshold_s


### plotting

x11()
# colnames(pnls) <- "backtest"
# back_test <- cbind(exp(cumsum(returns_running[rangev])), pnls[rangev])[endpoints(pnls, on="days")]
back_test <- cbind(exp(cumsum(returns_running[index(pnls)]))-1, pnls)[endpoints(pnls, on="days")]
# tail(back_test)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(back_test, theme=plot_theme,
             name="Backtest of static beta strategy for SPY")
legend("topleft", legend=colnames(back_test),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

## plot with shading

bu_y <- xts(SPY_design %*% betas[1:NCOL(SPY_design)], order.by=index(SPY_design))
# position_s[bu_y < -1] <- 1.0
se_ll <- xts(SPY_design %*% betas[(NCOL(SPY_design)+1):(2*NCOL(SPY_design))], order.by=index(SPY_design))
# position_s[se_ll > 1] <- -1.0

rangev <- "2010-05-05/2010-05-07"
rangev <- match(index(SPY_design[rangev]), index(SPY_design))
back_test <- cbind(exp(cumsum(returns_running[rangev]))-1, 
                   exp(cumsum(position_s[rangev]*returns_running[rangev]))-1)
colnames(back_test) <- c("SPY", "backtest")

back_test <- cbind(exp(cumsum(returns_running[rangev]))-1, 
                   bu_y[rangev])
colnames(back_test) <- c("SPY", "buy signal")

back_test <- cbind(bu_y[rangev], se_ll[rangev])
colnames(back_test) <- c("buy signal", "sell signal")

plot.zoo(back_test)
plot.zoo(cbind(HighFreq::SPY[rangev, 4], SPY_design[rangev, "variance"]))
plot.zoo(cbind(HighFreq::SPY[rangev, 4], SPY_design[rangev, 2:3]))

plot(as.numeric(HighFreq::SPY[rangev, 4]), t="l")
abline(v=which(bu_y[rangev] < -1), col="red", lwd=1)
abline(v=which(se_ll[rangev] > 1), col="blue", lwd=1)
abline(v=which(SPY_design[rangev, "variance"] > 1), col="red", lwd=1)
abline(v=which(SPY_design[rangev, 3] > 1), col="red", lwd=1)
abline(v=which((SPY_design[rangev, 2] > 2) | (SPY_design[rangev, 3] > 1)), col="red", lwd=1)


sha_de <- xts(position_s[rangev], order.by=index(returns_running[rangev]))

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
back_test <- function(design=NULL, betas=NULL, thresh_old=NULL, returns=NULL, lag=1, bid_offer=0.0) {
  sig_nal <- design %*% betas
  sig_nal <- rutils::lagit(sig_nal, lag=lag)
  if (lag > 1)
    sig_nal <- rutils::roll_sum(sig_nal, win_dow=lag) / lag
  # calculate returns
  if (is.null(thresh_old)) {
    # calculate returns proportional to sig_nal and scale them to SPY volatility
    position_s <- sig_nal
    pnls <- (position_s * returns)
    ratio <- sd(diff_xts(log(HighFreq::SPY[index(pnls), 4])))/sd(pnls)
    pnls <- ratio*pnls
  }
  else {
    # calculate returns of contrarian strategy with threshold
    position_s <- rep.int(NA, NROW(sig_nal))
    position_s[1] <- 0.0
    position_s[sig_nal > thresh_old] <- 1.0
    position_s[sig_nal < -thresh_old] <- -1.0
    position_s <- zoo::na.locf(position_s)
    pnls <- position_s * returns
    ratio <- 1
  }
  # calculate transaction costs
  costs <- ratio*bid_offer*abs(rutils::diffit(position_s))
  # calculate cumulative PnL
  pnls <- exp(cumsum(pnls - costs))
  colnames(pnls) <- "backtest"
  pnls
}  # end back_test


### older


colnames(pnls) <- "backtest"
back_test <- cbind(exp(cumsum(returns_running[index(pnls)])), pnls)[endpoints(pnls, on="days")]
# back_test <- SPY[index(pnls), 4]
# back_test <- back_test / as.numeric(back_test[1, ])
# back_test <- cbind(pnls, back_test)[endpoints(back_test, on="days"), ]
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
foo <- -sign(rutils::lagxts(returns_running))
sum(foo*returns_running)
chart_Series(x=cumsum(foo*returns_running), name="strategy cumulative returns")

