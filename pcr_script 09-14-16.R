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
betas <- rollapply(etfenv$returns, width=252, 
                    FUN=function(design_matrix) 
                      coef(lm(reg_formula, data=design_matrix))[2],
                    by=22, by.column=FALSE, align="right")
betas <- na.omit(betas)
# plot betas in x11() window
x11()
chart_Series(x=betas, name=paste("rolling betas", format(reg_formula)))

# perform daily rolling beta regressions in parallel - FAST!
betas <- roll::roll_lm(x=etfenv$returns[, "VTI"], 
                        y=etfenv$returns[, "XLP"],
                        width=252)$coefficients
betas <- na.omit(betas[, 2])
chart_Series(x=betas, name=paste("rolling betas", format(reg_formula)))

# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
datav <- etfenv$returns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(datav, width=22, 
                      FUN=function(design_matrix) 
                        coef(lm(reg_formula, data=design_matrix))[2],
                      by.column=FALSE, align="right"), 
  roll_lm=roll::roll_lm(x=datav[, "VTI"], 
                        y=datav[, "XLP"],
                        width=22)$coefficients, 
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


### More interesting example using SPY_design design matrix
# first either load or calculate design matrix

### Load design matrix called SPY_design containing columns of data aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)


### Calculate design matrix called SPY_design containing columns of data aggregations

lookb <- 5
returns_running <- run_returns(xtes=SPY)
returns_rolling <- roll_vwap(ohlc=SPY, xtes=returns_running, lookb=lookb)
colnames(returns_running) <- "returns"
colnames(returns_rolling) <- "returns.WA5"

var_running <- run_variance(ohlc=SPY)
var_rolling <- roll_vwap(ohlc=SPY, xtes=var_running, lookb=lookb)
colnames(var_running) <- "variance"
colnames(var_rolling) <- "variance.WA5"

skew_running <- run_skew(ohlc=SPY)
skew_rolling <- roll_vwap(ohlc=SPY, xtes=skew_running, lookb=lookb)
colnames(skew_running) <- "skew"
colnames(skew_rolling) <- "skew.WA5"

# sharpe_running <- run_sharpe(ohlc=SPY)
# sharpe_rolling <- roll_vwap(ohlc=SPY, xtes=sharpe_running, lookb=lookb)
# colnames(sharpe_running) <- "sharpe_running"
# colnames(sharpe_rolling) <- "sharpe_running.WA5"
# 
# sharpe_rolling <- roll_sharpe(ohlc=SPY, lookb=lookb)
# colnames(sharpe_rolling) <- "sharpe_rolling"

hurst_rolling <- roll_hurst(ohlc=SPY, lookb=lookb)
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


### perform rolling forecasting PCR regressions in parallel

# calculate close to close returns
returns_running <- run_returns(xtes=SPY)
# calculate returns advanced in time
returns_advanced <- rutils::lagxts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"

# perform rolling forecasting PCR regressions in parallel
# use only the first principal component: argument "comps"
betas_running <- roll_pcr(x=SPY_design["2011/2012", ], 
                          y=returns_advanced["2011/2012", ], 
                          width=1*60, comps=1:1, min_obs=1)
betas_running$coefficients[1, ] <- 0

# calculate mean beta coefficients
betas <- sapply(betas_running$coefficients, mean)

# calculate rolling mean beta coefficients over time
betas_rolling <- rutils::roll_sum(xtes=betas_running$coefficients, lookb=11)/11
tail(betas_rolling)


### forecast the returns from today's factors times the lagged betas

# lag the betas by a single period
betas_lagged <- rutils::lagxts(betas_running$coefficients)

# forecast the returns from today's factors in SPY_design times the lagged betas
# note that "returns_forecast" has minus sign, because betas_running are 
# calculated in the opposite sign by roll_pcr()
returns_forecast <- 
  -rowSums(betas_lagged[, -1]*SPY_design[index(betas_running$coefficients)]) - 
  betas_lagged[, 1]


### perform returns_backtest: invest proportional to returns_forecast

### trade immediately at the close
# lag returns_forecast by one period into the future
returns_forecast <- rutils::lagxts(returns_forecast)
returns_backtest <- cumsum(returns_forecast * returns_running)
# invest fixed notional using sign()
returns_backtest <- cumsum(sign(returns_forecast) * returns_running)

### trade at the open in the next period
# lag returns_forecast two periods into future
returns_forecast <- rutils::lagxts(returns_forecast, k=2)
# calculate open to open returns
returns_open <- run_returns(xtes=SPY, colnum=1)
colnames(returns_open) <- "returns_advanced"
returns_backtest <- cumsum(sign(returns_forecast) * returns_open)


### plotting

x11()

chart_Series(returns_backtest, name="strategy cumulative returns")

chart_Series(x=SPY["2011/2012", 4], name="SPY prices")
add_TA(x=returns_backtest, col='red', name="strategy cumulative returns")

chart_Series(x=SPY["2011/2012", ], name="SPY prices")
add_TA(returns_backtest, col='red', name="strategy cumulative returns")

chart_Series(x=SPY["2011/2012", 4], name="SPY prices")
add_TA(x=betas_rolling[, "returns"], col='red', name="returns loading")

chart_Series(x=SPY["2011-07/2011-10", 4], name="SPY prices")
add_TA(x=betas_rolling["2011-07/2011-10", "returns"], col='red', name="returns loading")
add_TA(x=betas_rolling["2011-07/2011-10", "hurst"], col='red', name="hurst loading")



### test for data snooping in PCR using random data

# perform one random PCR simulation using function run_pcr() (below)
returns_backtest <- run_pcr(SPY, design_matrix=SPY_design)
returns_backtest <- run_pcr(SPY, design_matrix=SPY_design, trade_the_close=FALSE)
chart_Series(x=returns_backtest, name="strategy cumulative returns")


# takes very long!!! - perform 100 random PCR simulations - takes very long!!!
pnls <- sapply(1:100, function(x) sum(run_pcr(SPY, design_matrix=SPY_design, random_ize=TRUE)))
pnls <- sapply(1:100, function(x) sum(run_pcr(random_ize=TRUE)))
sum(pnls>0)/length(pnls)
x11()
hist(pnls, breaks="FD", xlim=c(-5e6, 5e6), main="distribution of Pnl's")

# run PCR model and return the Pnl
run_pcr <- function(ohlc=NULL, design_matrix=NULL, trade_the_close=TRUE, random_ize=FALSE) {
  if (random_ize)
    ohlc <- HighFreq::random_OHLC(ohlc=ohlc)
  if (is.null(design_matrix))
    design_matrix <- get_design(ohlc, lookb=60)
  # calculate close to close returns
  returns_running <- run_returns(xtes=ohlc)
  # calculate returns advanced in time
  returns_advanced <- rutils::lagxts(returns_running, k=-1)
  colnames(returns_advanced) <- "returns_advanced"
  # perform rolling forecasting PCR regressions in parallel
  # use only the first principal component: argument "comps"
  betas_running <- roll_pcr(x=design_matrix, 
                            y=returns_advanced, 
                            width=1*60, comps=1:1, min_obs=1)
  betas_running$coefficients[1, ] <- 0
  # lag the betas by a single period
  betas_lagged <- rutils::lagxts(betas_running$coefficients)
  # forecast the returns from today's factors in design_matrix times the lagged betas
  # note that "returns_forecast" has minus sign, because betas_running are 
  # calculated in the opposite sign by roll_pcr()
  returns_forecast <- 
    -rowSums(betas_lagged[, -1]*design_matrix[index(betas_running$coefficients)]) - 
    betas_lagged[, 1]
  if (trade_the_close) {
    # trade immediately at the close
    # lag returns_forecast by one period into the future
    returns_forecast <- rutils::lagxts(returns_forecast)
  }
  else {
    # trade at the open in the next period
    # lag returns_forecast two periods into future
    returns_forecast <- rutils::lagxts(returns_forecast, k=2)
    # calculate open to open returns
    returns_running <- run_returns(xtes=ohlc, colnum=1)
  }
  cumsum(sign(returns_forecast) * returns_running)
}  # end run_pcr


# create a design matrix from OHLC data
get_design <- function(ohlc, lookb) {
  returns_running <- run_returns(xtes=ohlc)
  returns_rolling <- HighFreq::roll_vwap(ohlc=ohlc, xtes=returns_running, lookb=lookb)
  var_running <- run_variance(ohlc=ohlc)
  skew_running <- run_skew(ohlc=ohlc)
  hurst_rolling <- roll_hurst(ohlc=ohlc, lookb=lookb)
  design_matrix <- cbind(returns_running, returns_rolling, var_running, skew_running, hurst_rolling, returns_running*var_running, returns_running*skew_running)
  design_matrix <- roll::roll_scale(data=design_matrix, width=60, min_obs=1)
  core_data <- coredata(design_matrix)
  core_data[is.na(core_data)] <- 0
  design_matrix <- xts(x=core_data, order.by=index(design_matrix))
  colnames(design_matrix) <- c("returns", "returns.WA5", "variance", "skew", "hurst", "rets_var", "rets_skew")
  design_matrix
}  # end get_design

SPY_design <- get_design(SPY, lookb=60)



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
foo <- -sign(rutils::lagxts(returns_running))
sum(foo*returns_running)
chart_Series(x=cumsum(foo*returns_running), name="strategy cumulative returns")

