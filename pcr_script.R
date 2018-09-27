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


### load SPY_design design matrix, containing columns of data aggregations

load("C:/Develop/data/SPY_design.RData")
head(SPY_design)


### SPY_design correlation and PCA analysis

# calculate correlation matrix
corr_matrix <- cor(SPY_design[ran_ge])
colnames(corr_matrix) <- colnames(SPY_design)
rownames(corr_matrix) <- colnames(SPY_design)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(corr_matrix, 
                       order="hclust", 
                       hclust.method="complete")
corr_matrix <- corr_matrix[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "blue"))
corrplot(corr_matrix, title="Correlation Matrix", 
         tl.col="black", tl.cex=0.8, mar=c(0,0,1,0), 
         method="square", col=col_ors(NCOL(SPY_design)), 
         cl.offset=0.75, cl.cex=0.7, 
         cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(corr_matrix, k=NCOL(SPY_design) %/% 2, 
                method="complete", col="red")


# draw dendrogram of correlation matrix
# convert correlation matrix into distance object
data_dist <- as.dist(1-corr_matrix_ordered)
# Perform hierarchical clustering analysis
data_cluster <- hclust(data_dist)
plot(data_cluster, ann=FALSE, xlab="", ylab="")
title("Dissimilarity = 1-Correlation", line=-0.5)


# perform principal component analysis PCA
p_ca <- prcomp(SPY_design[ran_ge], center=FALSE, scale=FALSE)
summary(p_ca)

# plot principal component loadings (weights)
p_ca$rotation
# plot loading barplots in multiple panels
n_cols <- NCOL(SPY_design)
par(mfrow=c(n_cols %/% 2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (or_der in 1:(2*(n_cols %/% 2))) {
  barplot(p_ca$rotation[, or_der], 
          las=3, xlab="", ylab="", main="")
  title(paste0("PC", or_der), line=-2.0, 
        col.main="red")
}  # end for

# calculate out-of-sample principal component time series
pca_ts <- xts(SPY_design %*% p_ca$rotation, order.by=index(SPY_design))
# colnames(pca_ts)

# redefine design matrix as the time series of principal components
SPY_design <- pca_ts[, 1:4]



### Example of rolling beta regressions using package roll (without forecasting)
# you can skip this

library(roll)

# example of rolling regressions using standard rollapply() - SLOW!
# specify regression formula
for_mula <- XLP ~ VTI
# perform rolling beta regressions every month
beta_s <- rollapply(etf_env$re_turns, width=252,
                    FUN=function(de_sign)
                      coef(lm(for_mula, data=de_sign))[2],
                    by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11()
chart_Series(x=beta_s, name=paste("rolling betas", format(for_mula)))

# perform daily rolling beta regressions in parallel - FAST!
beta_s <- roll::roll_lm(x=etf_env$re_turns[, "VTI"],
                        y=etf_env$re_turns[, "XLP"],
                        width=252)$coefficients
beta_s <- na.omit(beta_s[, 2])
chart_Series(x=beta_s, name=paste("rolling betas", format(for_mula)))

# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- etf_env$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22,
                      FUN=function(de_sign)
                        coef(lm(for_mula, data=de_sign))[2],
                      by.column=FALSE, align="right"),
  roll_lm=roll::roll_lm(x=da_ta[, "VTI"],
                        y=da_ta[, "XLP"],
                        width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



### Example using design matrix SPY_design
# first either load or calculate design matrix

# calculate returns advanced in time
returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=SPY)
returns_advanced <- rutils::lag_xts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"
tail(cbind(returns_advanced, returns_running))


### perform rolling regressions of returns_advanced versus de_sign
beta_s <- sapply(1:((NROW(SPY_design) %/% 10000)-1), function(x) {
  ran_ge <- 1:10000 + 10000*x
  summary(lm(returns_advanced[ran_ge, ] ~ SPY_design[ran_ge, ]))$coefficients[, "t value"]
})  # end sapply
beta_s <- t(beta_s)
colnames(beta_s)[-1] <- sapply(colnames(beta_s)[-1], function(x) strsplit(x, split = "]")[[1]][2])
apply(beta_s, MARGIN=2, sum)
plot.zoo(beta_s)


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
# note that "sig_nal" has minus sign, because betas_running are
# calculated in the opposite sign by roll_pcr()
sig_nal <-
  -rowSums(betas_lagged[, -1]*SPY_design[index(betas_running$coefficients)]) -
  betas_lagged[, 1]


### perform backtests: invest proportional to sig_nal

### trade immediately at the close
# lag sig_nal by one period into the future
sig_nal <- rutils::lag_xts(sig_nal)
# pnl_s <- exp(cumsum(sig_nal * returns_running))
# or, invest fixed notional using sign()
pnl_s <- exp(cumsum(sign(sig_nal) * returns_running))

### or, trade at the open in the next period
# lag sig_nal two periods into future
sig_nal <- rutils::lag_xts(sig_nal, lag=2)
# calculate open to open returns
returns_open <- HighFreq::run_returns(x_ts=HighFreq::SPY, col_umn=1, sca_le=FALSE)
colnames(returns_open) <- "returns_advanced"
pnl_s <- exp(cumsum(sign(sig_nal) * returns_open))


### plotting

bench_mark <- HighFreq::SPY[index(pnl_s), 4]
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
pnl_s <- run_pcr(HighFreq::SPY, de_sign=SPY_design)
pnl_s <- run_pcr(HighFreq::SPY, de_sign=SPY_design, trade_the_close=FALSE)
chart_Series(x=pnl_s, name="strategy cumulative returns")


# takes very long!!! - perform 100 random PCR simulations - takes very long!!!
pnl_s <- sapply(1:100, function(x) sum(run_pcr(HighFreq::SPY, de_sign=SPY_design, random_ize=TRUE)))
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
  returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=oh_lc)
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
  # note that "sig_nal" has minus sign, because betas_running are
  # calculated in the opposite sign by roll_pcr()
  sig_nal <-
    -rowSums(betas_lagged[, -1]*de_sign[index(betas_running$coefficients)]) -
    betas_lagged[, 1]
  if (trade_the_close) {
    # trade immediately at the close
    # lag sig_nal by one period into the future
    sig_nal <- rutils::lag_xts(sig_nal)
  }
  else {
    # trade at the open in the next period
    # lag sig_nal two periods into future
    sig_nal <- rutils::lag_xts(sig_nal, lag=2)
    # calculate open to open returns
    returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=oh_lc, col_umn=1)
  }
  cumsum(sign(sig_nal) * returns_running)
}  # end run_pcr


# create a design matrix from OHLC data
get_design <- function(oh_lc, win_dow) {
  returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=oh_lc)
  returns_rolling <- HighFreq::roll_vwap(oh_lc=oh_lc, x_ts=returns_running, win_dow=win_dow)
  var_running <- run_variance(oh_lc=oh_lc)
  skew_running <- run_skew(oh_lc=oh_lc)
  hurst_rolling <- roll_hurst(oh_lc=oh_lc, win_dow=win_dow)
  de_sign <- cbind(returns_running, returns_rolling, var_running, skew_running, hurst_rolling, returns_running*var_running, returns_running*skew_running)
  de_sign <- roll::roll_scale(data=de_sign, width=60, min_obs=1)
  core_data <- coredata(de_sign)
  core_data[is.na(core_data)] <- 0
  de_sign <- xts(x=core_data, order.by=index(de_sign))
  colnames(de_sign) <- c("returns", "returns.roll", "variance", "skew", "hurst", "rets_var", "rets_skew")
  de_sign
}  # end get_design

SPY_design <- get_design(HighFreq::SPY, win_dow=60)


### rolling regressions over SPY_design using package roll

# perform rolling forecasting regressions in parallel
rolling_betas <- roll::roll_lm(x=SPY_design["2011/2012", ], 
                               y=returns_advanced["2011/2012", ],
                               width=6.5*60, min_obs=1)
rolling_betas$coefficients[1, ] <- 0
sum(is.na(rolling_betas$coefficients))
head(rolling_betas$coefficients)
tail(rolling_betas$coefficients["2012-11-12"])
tail(rolling_betas$r.squared["2012-11-12"])
chart_Series(x=rolling_betas$r.squared["2012-11-12"], name="R-squared for rolling betas")

# calculate intraday seasonality of R2
# remove first day containing warmup
in_dex <- "2011-01-03" == format(index(rolling_betas$r.squared), "%Y-%m-%d")
r2_seasonal <- season_ality(rolling_betas$r.squared[!in_dex])
colnames(r2_seasonal) <- "R-squared seasonality"
chart_Series(x=r2_seasonal, name="R-squared seasonality")

# plot coefficients with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- rainbow(NCOL(rolling_betas$coefficients))
chart_Series(x=rolling_betas$coefficients["2012-11-12"], 
             theme=plot_theme, 
             name="coefficients for rolling betas")
legend("bottom", legend=colnames(rolling_betas$coefficients["2012-11-12"]), 
       bg="white", lty=c(1, 1), lwd=c(2, 2), 
       col=plot_theme$col$line.col, bty="n")


### rolling principal component regressions (PCR) over SPY_design using package roll

# perform rolling forecasting PCR regressions in parallel
# use only the first principal component argument "comps"
rolling_betas <- roll::roll_pcr(x=SPY_design["2011/2012", ], 
                                y=returns_advanced["2011/2012", ],
                                width=1*60, comps=1:1, min_obs=1)
rolling_betas$coefficients[1, ] <- 0
sum(is.na(rolling_betas$coefficients))
head(rolling_betas$coefficients)
tail(rolling_betas$coefficients["2012-11-12"])
tail(rolling_betas$r.squared["2012-11-12"])
chart_Series(x=rolling_betas$r.squared["2012-11-12"], name="R-squared for rolling betas")

# calculate intraday seasonality of R2
# remove first day containing warmup
in_dex <- "2011-01-03" == format(index(rolling_betas$r.squared), "%Y-%m-%d")
r2_seasonal <- season_ality(rolling_betas$r.squared[!in_dex])
colnames(r2_seasonal) <- "R-squared seasonality"
chart_Series(x=r2_seasonal, name="R-squared seasonality")

# plot coefficients with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- rainbow(NCOL(rolling_betas$coefficients))
chart_Series(x=rolling_betas$coefficients["2012-11-12"], 
             theme=plot_theme, 
             name="coefficients for rolling betas")
legend("bottom", legend=colnames(rolling_betas$coefficients["2012-11-12"]), 
       bg="white", lty=c(1, 1), lwd=c(2, 2), 
       col=plot_theme$col$line.col, bty="n")


### calculate forecasts of returns

library(matrixStats)

# forecast the returns from today's factors times the lagged betas
betas_lagged <- rutils::lag_xts(rolling_betas$coefficients)
returns_forecast <- rowSums(betas_lagged[, -1]*SPY_design[index(rolling_betas$coefficients)]) + betas_lagged[, 1]
tail(returns_forecast)

forecast_lm <- lm(returns_advanced[index(returns_forecast)] ~ returns_forecast)
summary(forecast_lm)
x11()
# scatterplot
plot(coredata(returns_advanced[index(returns_forecast)]), coredata(returns_forecast))

# cumulative returns_backtest: invest proportional to returns_forecast
returns_backtest <- cumsum(returns_forecast * returns_advanced[index(returns_forecast)])
chart_Series(x=-returns_backtest, name="cumulative returns")
chart_Series(x=-returns_backtest["2011-08-07/2011-08-12"], name="cumulative returns")
chart_Series(x=SPY["2011-08-07/2011-08-12", 1], name="cumulative returns")

bar <- returns_advanced[index(returns_forecast)] * returns_forecast
foo <- which.max(-bar)
chart_Series(x=bar[(foo-10):(foo+10), ], name="cumulative returns")

chart_Series(x=cumsum(returns_running[(foo-1000):(foo+1000), ]), name="cumulative returns")



### test for data snooping in PCR using random data

# create time index of one second intervals
in_dex <- seq(from=as.POSIXct("2016-01-01 00:00:00"),
              to=as.POSIXct("2016-01-30 00:00:00"), by="1 sec")

# perform one random PCR simulation using function run_random_pcr()
run_random_pcr(in_dex)

# perform 100 random PCR simulations
pnl_s <- sapply(1:100, function(x, in_dex) run_random_pcr(in_dex), in_dex=in_dex)
hist(pnl_s, breaks="FD", xlim=c(-5e4, 5e4), main="distribution of Pnl's")

# perform a random PCR and return the Pnl
run_random_pcr <- function(in_dex) {
  x_ts <- xts(exp(cumsum(rnorm(NROW(in_dex), sd=0.001))), order.by=in_dex)
  oh_lc <- xts::to.period(x=x_ts, period="minutes", name="random")
  oh_lc <- cbind(oh_lc, sample(x=10*(2:18), size=NROW(oh_lc), replace=TRUE))
  colnames(oh_lc)[ 5] <- "random.volume"
  returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=oh_lc)
  returns_advanced <- rutils::lag_xts(returns_running, k=-1)
  returns_rolling <- roll_vwap(oh_lc=oh_lc, x_ts=returns_running, win_dow=win_dow)
  var_running <- 6.5*60^3*HighFreq::run_variance(oh_lc=oh_lc)
  skew_running <- 6.5*60^4*HighFreq::run_skew(oh_lc=oh_lc)
  hurst_rolling <- roll_hurst(oh_lc=oh_lc, win_dow=win_dow)
  design_matrix <- cbind(returns_running, returns_rolling, var_running, skew_running, hurst_rolling, returns_running*var_running, returns_running*skew_running)
  design_matrix <- roll::roll_scale(data=design_matrix, width=60, min_obs=1)
  core_data <- coredata(design_matrix)
  core_data[is.na(core_data)] <- 0
  design_matrix <- xts(x=core_data, order.by=index(design_matrix))
  rolling_betas <- roll::roll_pcr(x=design_matrix, y=returns_advanced, width=1*60, comps=1:1, min_obs=1)
  rolling_betas$coefficients[1, ] <- 0
  betas_lagged <- rutils::lag_xts(rolling_betas$coefficients)
  returns_forecast <- rowSums(betas_lagged[, -1]*design_matrix[index(rolling_betas$coefficients)]) + betas_lagged[, 1]
  sum(returns_forecast * returns_advanced[index(returns_forecast)])
}  # end run_random_pcr


# create xts of random prices
x_ts <- xts(exp(cumsum(rnorm(NROW(in_dex), sd=0.001))), order.by=in_dex)
colnames(x_ts) <- "random"
# chart_Series(x=x_ts["2016-01-10 09/2016-01-10 10"], name="random prices")
# aggregate to minutes OHLC data
oh_lc <- xts::to.period(x=x_ts, period="minutes", name="random")
# chart_Series(x=oh_lc["2016-01-10"], name="random OHLC prices")
# add volume
oh_lc <- cbind(oh_lc, sample(x=10*(2:18), size=NROW(oh_lc), replace=TRUE))
colnames(oh_lc)[ 5] <- "random.volume"
# tail(oh_lc)

# create SPY_design
SPY <- oh_lc
returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=SPY)
returns_advanced <- rutils::lag_xts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"
returns_rolling <- roll_vwap(oh_lc=SPY, x_ts=returns_running, win_dow=win_dow)
colnames(returns_running) <- "returns"
colnames(returns_rolling) <- "returns.roll"
var_running <- 6.5*60^3*HighFreq::run_variance(oh_lc=SPY)
colnames(var_running) <- "variance"
skew_running <- 6.5*60^4*HighFreq::run_skew(oh_lc=SPY)
colnames(skew_running) <- "skew"
hurst_rolling <- roll_hurst(oh_lc=SPY, win_dow=win_dow)
colnames(hurst_rolling) <- "hurst"
SPY_design <- cbind(returns_running, returns_rolling, var_running, skew_running, hurst_rolling, returns_running*var_running, returns_running*skew_running)
colnames(SPY_design) <- c(colnames(SPY_design)[1:5], "rets_var", "rets_skew")

# scale SPY_design
SPY_design <- roll::roll_scale(data=SPY_design, width=60, min_obs=1)
core_data <- coredata(SPY_design)
core_data[is.na(core_data)] <- 0
SPY_design <- xts(x=core_data, order.by=index(SPY_design))

# perform PCR
rolling_betas <- roll_pcr(x=SPY_design, y=returns_advanced, width=1*60, comps=1:1, min_obs=1)
rolling_betas$coefficients[1, ] <- 0
betas_lagged <- rutils::lag_xts(rolling_betas$coefficients)
returns_forecast <- rowSums(betas_lagged[, -1]*SPY_design[index(rolling_betas$coefficients)]) + betas_lagged[, 1]

# forecast_lm <- lm(returns_advanced[index(returns_forecast)] ~ returns_forecast)
# summary(forecast_lm)

returns_backtest <- cumsum(returns_forecast * returns_advanced[index(returns_forecast)])
chart_Series(x=-returns_backtest, name="cumulative returns")



### rollSFM (rolling single-factor model) function to TTR

# rolling regression over time index
reg <- rollSFM(demo.xts, .index(demo.xts), 24)
rma <- reg$alpha + reg$beta*.index(demo.xts)
chart_Series(demo.xts, TA="add_TA(rma,on=1)")



###  Forecastable Component Analysis
library(ForeCA)
ret <- ts(diff(log(EuStockMarkets)) * 100) 
mod <- foreca(ret, spectrum.control=list(method="wosa"))
mod
summary(mod)
plot(mod)



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

