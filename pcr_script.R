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
corr_matrix <- cor(SPY_design[rangev])
colnames(corr_matrix) <- colnames(SPY_design)
rownames(corr_matrix) <- colnames(SPY_design)
# reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(corr_matrix, 
                       order="hclust", 
                       hclust.method="complete")
corr_matrix <- corr_matrix[ordern, ordern]
# plot the correlation matrix
colors <- colorRampPalette(c("red", "blue"))
corrplot(corr_matrix, title="Correlation Matrix", 
         tl.col="black", tl.cex=0.8, mar=c(0,0,1,0), 
         method="square", col=colors(NCOL(SPY_design)), 
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
p_ca <- prcomp(SPY_design[rangev], center=FALSE, scale=FALSE)
summary(p_ca)

# plot principal component loadings (weights)
p_ca$rotation
# plot loading barplots in multiple panels
ncols <- NCOL(SPY_design)
par(mfrow=c(ncols %/% 2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:(2*(ncols %/% 2))) {
  barplot(p_ca$rotation[, ordern], 
          las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, 
        col.main="red")
}  # end for

# calculate out-of-sample principal component time series
pcats <- xts(SPY_design %*% p_ca$rotation, order.by=index(SPY_design))
# colnames(pcats)

# redefine design matrix as the time series of principal components
SPY_design <- pcats[, 1:4]



### Example of rolling beta regressions using package roll (without forecasting)
# you can skip this

library(roll)

# example of rolling regressions using standard rollapply() - SLOW!
# specify regression formula
formulav <- XLP ~ VTI
# perform rolling beta regressions every month
betas <- rollapply(etfenv$returns, width=252,
                    FUN=function(design)
                      coef(lm(formulav, data=design))[2],
                    by=22, by.column=FALSE, align="right")
betas <- na.omit(betas)
# plot betas in x11() window
x11()
chart_Series(x=betas, name=paste("rolling betas", format(formulav)))

# perform daily rolling beta regressions in parallel - FAST!
betas <- roll::roll_lm(x=etfenv$returns[, "VTI"],
                        y=etfenv$returns[, "XLP"],
                        width=252)$coefficients
betas <- na.omit(betas[, 2])
chart_Series(x=betas, name=paste("rolling betas", format(formulav)))

# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
datav <- etfenv$returns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(datav, width=22,
                      FUN=function(design)
                        coef(lm(formulav, data=design))[2],
                      by.column=FALSE, align="right"),
  roll_lm=roll::roll_lm(x=datav[, "VTI"],
                        y=datav[, "XLP"],
                        width=22)$coefficients,
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



### Example using design matrix SPY_design
# first either load or calculate design matrix

# calculate returns advanced in time
returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=SPY)
returns_advanced <- rutils::lagxts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"
tail(cbind(returns_advanced, returns_running))


### perform rolling regressions of returns_advanced versus design
betas <- sapply(1:((NROW(SPY_design) %/% 10000)-1), function(x) {
  rangev <- 1:10000 + 10000*x
  summary(lm(returns_advanced[rangev, ] ~ SPY_design[rangev, ]))$coefficients[, "t value"]
})  # end sapply
betas <- t(betas)
colnames(betas)[-1] <- sapply(colnames(betas)[-1], function(x) strsplit(x, split = "]")[[1]][2])
apply(betas, MARGIN=2, sum)
plot.zoo(betas)


### perform rolling forecasting PCR regressions in parallel
# use only the first principal component: argument "comps"
betas_running <- roll::roll_pcr(x=SPY_design["2011/2012", ],
                          y=returns_advanced["2011/2012", ],
                          width=1*60, comps=1:1, min_obs=1)
betas_running$coefficients[1, ] <- 0

# calculate mean beta coefficients
sapply(betas_running$coefficients, mean)

# calculate rolling mean beta coefficients over time
betas_rolling <- rutils::roll_sum(xtes=betas_running$coefficients, look_back=11)/11
tail(betas_rolling)


### forecast the returns from today's factors times the lagged betas

# lag the betas by a single period
betas_lagged <- rutils::lagxts(betas_running$coefficients)

# forecast the returns from today's factors in SPY_design times the lagged betas
# note that "score" has minus sign, because betas_running are
# calculated in the opposite sign by roll_pcr()
score <-
  -rowSums(betas_lagged[, -1]*SPY_design[index(betas_running$coefficients)]) -
  betas_lagged[, 1]


### perform backtests: invest proportional to score

### trade immediately at the close
# lag score by one period into the future
score <- rutils::lagxts(score)
# pnls <- exp(cumsum(score * returns_running))
# or, invest fixed notional using sign()
pnls <- exp(cumsum(sign(score) * returns_running))

### or, trade at the open in the next period
# lag score two periods into future
score <- rutils::lagxts(score, lag=2)
# calculate open to open returns
returns_open <- HighFreq::run_returns(xtes=HighFreq::SPY, colnum=1, scalit=FALSE)
colnames(returns_open) <- "returns_advanced"
pnls <- exp(cumsum(sign(score) * returns_open))


### plotting

bench_mark <- HighFreq::SPY[index(pnls), 4]
bench_mark <- bench_mark / as.numeric(bench_mark[1, ])
bench_mark <- merge(pnls, bench_mark)[endpoints(bench_mark, on="days"), ]
tail(bench_mark)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(bench_mark, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
legend("topleft", legend=colnames(bench_mark),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

chart_Series(pnls, name="strategy cumulative returns")
chart_Series(x=SPY["2011/2012", 4], name="SPY prices")
add_TA(x=pnls, col='red', name="strategy cumulative returns")

# chart_Series(x=SPY["2011/2012", ], name="SPY prices")
# add_TA(x=pnls, col='red', name="strategy cumulative returns")

chart_Series(x=SPY["2011/2012", 4], name="SPY prices")
add_TA(x=betas_rolling[, "returns"], col='red', name="returns loading")

chart_Series(x=SPY["2011-07/2011-10", 4], name="SPY prices")
add_TA(x=betas_rolling["2011-07/2011-10", "returns"], col='red', name="returns loading")
add_TA(x=betas_rolling["2011-07/2011-10", "hurst"], col='red', name="hurst loading")



### test for data snooping in PCR using random data

# perform one random PCR simulation using function run_pcr() (below)
pnls <- run_pcr(HighFreq::SPY, design=SPY_design)
pnls <- run_pcr(HighFreq::SPY, design=SPY_design, trade_the_close=FALSE)
chart_Series(x=pnls, name="strategy cumulative returns")


# takes very long!!! - perform 100 random PCR simulations - takes very long!!!
pnls <- sapply(1:100, function(x) sum(run_pcr(HighFreq::SPY, design=SPY_design, random_ize=TRUE)))
pnls <- sapply(1:100, function(x) sum(run_pcr(random_ize=TRUE)))
sum(pnls>0)/length(pnls)
x11()
hist(pnls, breaks="FD", xlim=c(-5e6, 5e6), main="distribution of Pnl's")

# run PCR model and return the Pnl
run_pcr <- function(ohlc=NULL, design=NULL, trade_the_close=TRUE, random_ize=FALSE) {
  if (random_ize)
    ohlc <- HighFreq::random_OHLC(ohlc=ohlc)
  if (is.null(design))
    design <- get_design(ohlc, look_back=60)
  # calculate close to close returns
  returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=ohlc)
  # calculate returns advanced in time
  returns_advanced <- rutils::lagxts(returns_running, lag=-1)
  colnames(returns_advanced) <- "returns_advanced"
  # perform rolling forecasting PCR regressions in parallel
  # use only the first principal component: argument "comps"
  betas_running <- roll_pcr(x=design,
                            y=returns_advanced,
                            width=1*60, comps=1:1, min_obs=1)
  betas_running$coefficients[1, ] <- 0
  # lag the betas by a single period
  betas_lagged <- rutils::lagxts(betas_running$coefficients)
  # forecast the returns from today's factors in design times the lagged betas
  # note that "score" has minus sign, because betas_running are
  # calculated in the opposite sign by roll_pcr()
  score <-
    -rowSums(betas_lagged[, -1]*design[index(betas_running$coefficients)]) -
    betas_lagged[, 1]
  if (trade_the_close) {
    # trade immediately at the close
    # lag score by one period into the future
    score <- rutils::lagxts(score)
  }
  else {
    # trade at the open in the next period
    # lag score two periods into future
    score <- rutils::lagxts(score, lag=2)
    # calculate open to open returns
    returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=ohlc, colnum=1)
  }
  cumsum(sign(score) * returns_running)
}  # end run_pcr


# create a design matrix from OHLC data
get_design <- function(ohlc, look_back) {
  returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=ohlc)
  returns_rolling <- HighFreq::roll_vwap(ohlc=ohlc, xtes=returns_running, look_back=look_back)
  var_running <- run_variance(ohlc=ohlc)
  skew_running <- run_skew(ohlc=ohlc)
  hurst_rolling <- roll_hurst(ohlc=ohlc, look_back=look_back)
  design <- cbind(returns_running, returns_rolling, var_running, skew_running, hurst_rolling, returns_running*var_running, returns_running*skew_running)
  design <- roll::roll_scale(data=design, width=60, min_obs=1)
  core_data <- coredata(design)
  core_data[is.na(core_data)] <- 0
  design <- xts(x=core_data, order.by=index(design))
  colnames(design) <- c("returns", "returns.roll", "variance", "skew", "hurst", "rets_var", "rets_skew")
  design
}  # end get_design

SPY_design <- get_design(HighFreq::SPY, look_back=60)


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
indeks <- "2011-01-03" == format(index(rolling_betas$r.squared), "%Y-%m-%d")
r2_seasonal <- season_ality(rolling_betas$r.squared[!indeks])
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
indeks <- "2011-01-03" == format(index(rolling_betas$r.squared), "%Y-%m-%d")
r2_seasonal <- season_ality(rolling_betas$r.squared[!indeks])
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
betas_lagged <- rutils::lagxts(rolling_betas$coefficients)
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
indeks <- seq(from=as.POSIXct("2016-01-01 00:00:00"),
              to=as.POSIXct("2016-01-30 00:00:00"), by="1 sec")

# perform one random PCR simulation using function run_random_pcr()
run_random_pcr(indeks)

# perform 100 random PCR simulations
pnls <- sapply(1:100, function(x, indeks) run_random_pcr(indeks), indeks=indeks)
hist(pnls, breaks="FD", xlim=c(-5e4, 5e4), main="distribution of Pnl's")

# perform a random PCR and return the Pnl
run_random_pcr <- function(indeks) {
  xtes <- xts(exp(cumsum(rnorm(NROW(indeks), sd=0.001))), order.by=indeks)
  ohlc <- xts::to.period(x=xtes, period="minutes", name="random")
  ohlc <- cbind(ohlc, sample(x=10*(2:18), size=NROW(ohlc), replace=TRUE))
  colnames(ohlc)[ 5] <- "random.volume"
  returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=ohlc)
  returns_advanced <- rutils::lagxts(returns_running, k=-1)
  returns_rolling <- roll_vwap(ohlc=ohlc, xtes=returns_running, look_back=look_back)
  var_running <- 6.5*60^3*HighFreq::run_variance(ohlc=ohlc)
  skew_running <- 6.5*60^4*HighFreq::run_skew(ohlc=ohlc)
  hurst_rolling <- roll_hurst(ohlc=ohlc, look_back=look_back)
  design_matrix <- cbind(returns_running, returns_rolling, var_running, skew_running, hurst_rolling, returns_running*var_running, returns_running*skew_running)
  design_matrix <- roll::roll_scale(data=design_matrix, width=60, min_obs=1)
  core_data <- coredata(design_matrix)
  core_data[is.na(core_data)] <- 0
  design_matrix <- xts(x=core_data, order.by=index(design_matrix))
  rolling_betas <- roll::roll_pcr(x=design_matrix, y=returns_advanced, width=1*60, comps=1:1, min_obs=1)
  rolling_betas$coefficients[1, ] <- 0
  betas_lagged <- rutils::lagxts(rolling_betas$coefficients)
  returns_forecast <- rowSums(betas_lagged[, -1]*design_matrix[index(rolling_betas$coefficients)]) + betas_lagged[, 1]
  sum(returns_forecast * returns_advanced[index(returns_forecast)])
}  # end run_random_pcr


# create xts of random prices
xtes <- xts(exp(cumsum(rnorm(NROW(indeks), sd=0.001))), order.by=indeks)
colnames(xtes) <- "random"
# chart_Series(x=xtes["2016-01-10 09/2016-01-10 10"], name="random prices")
# aggregate to minutes OHLC data
ohlc <- xts::to.period(x=xtes, period="minutes", name="random")
# chart_Series(x=ohlc["2016-01-10"], name="random OHLC prices")
# add volume
ohlc <- cbind(ohlc, sample(x=10*(2:18), size=NROW(ohlc), replace=TRUE))
colnames(ohlc)[ 5] <- "random.volume"
# tail(ohlc)

# create SPY_design
SPY <- ohlc
returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=SPY)
returns_advanced <- rutils::lagxts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"
returns_rolling <- roll_vwap(ohlc=SPY, xtes=returns_running, look_back=look_back)
colnames(returns_running) <- "returns"
colnames(returns_rolling) <- "returns.roll"
var_running <- 6.5*60^3*HighFreq::run_variance(ohlc=SPY)
colnames(var_running) <- "variance"
skew_running <- 6.5*60^4*HighFreq::run_skew(ohlc=SPY)
colnames(skew_running) <- "skew"
hurst_rolling <- roll_hurst(ohlc=SPY, look_back=look_back)
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
betas_lagged <- rutils::lagxts(rolling_betas$coefficients)
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

