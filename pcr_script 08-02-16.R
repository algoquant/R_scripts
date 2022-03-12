### Install packages rutils, HighFreq, and roll from github

install.packages("devtools")
install.packages("matrixStats")
devtools::install_github(repo="algoquant/rutils")
devtools::install_github(repo="algoquant/HighFreq")
devtools::install_github(repo="algoquant/jjf234/roll")
library(HighFreq)


### example of rolling beta regressions using package roll

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


### now a more interesting example
### load SPY_design design matrix 

# load design matrix called SPY_design containing columns of aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)

# create advanced returns
returns_running <- run_returns(xtes=SPY)
returns_advanced <- rutils::lagxts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"
tail(cbind(returns_advanced, returns_running))


### create design matrix called SPY_design containing columns of aggregations

look_back <- 5
returns_running <- run_returns(xtes=SPY)
returns_rolling <- roll_vwap(ohlc=SPY, xtes=returns_running, look_back=look_back)
colnames(returns_running) <- "returns"
colnames(returns_rolling) <- "returns.WA5"

var_running <- run_variance(ohlc=SPY)
var_rolling <- roll_vwap(ohlc=SPY, xtes=var_running, look_back=look_back)
colnames(var_running) <- "variance"
colnames(var_rolling) <- "variance.WA5"

skew_running <- run_skew(ohlc=SPY)
skew_rolling <- roll_vwap(ohlc=SPY, xtes=skew_running, look_back=look_back)
colnames(skew_running) <- "skew"
colnames(skew_rolling) <- "skew.WA5"

sharpe_running <- run_sharpe(ohlc=SPY)
sharpe_rolling <- roll_vwap(ohlc=SPY, xtes=sharpe_running, look_back=look_back)
colnames(sharpe_running) <- "sharpe_running"
colnames(sharpe_rolling) <- "sharpe_running.WA5"

sharpe_rolling <- roll_sharpe(ohlc=SPY, look_back=look_back)
colnames(sharpe_rolling) <- "sharpe_rolling"

hurst_rolling <- roll_hurst(ohlc=SPY, look_back=look_back)
colnames(hurst_rolling) <- "hurst_rolling"

# select most significant factors plus interaction terms
SPY_design <- cbind(returns_running, returns_rolling, var_running, skew_running, 
                    hurst_rolling, returns_running*var_running, returns_running*skew_running)
colnames(SPY_design) <- c(colnames(SPY_design)[1:4], "hurst", "rets_var", "rets_skew")

# create advanced returns
returns_advanced <- rutils::lagxts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"
tail(cbind(returns_advanced, returns_running))

# apply rolling centering and scaling to the design matrix
library(roll)
SPY_design <- roll::roll_scale(data=SPY_design, width=6.5*60, min_obs=1)
# remove NAs
core_data <- coredata(SPY_design)
core_data[is.na(core_data)] <- 0
SPY_design <- xts(x=core_data, order.by=index(SPY_design))
sum(is.na(SPY_design))

# perform rolling forecasting PCR regressions in parallel
# use only the first principal component argument "comps"
rolling_betas <- roll_pcr(x=SPY_design["2011/2012", ], 
                          y=returns_advanced["2011/2012", ],
                          width=1*60, comps=1:1, min_obs=1)
rolling_betas$coefficients[1, ] <- 0

library(matrixStats)

# forecast the returns from today's factors times the lagged betas
betas_lagged <- rutils::lagxts(rolling_betas$coefficients)
returns_forecast <- 
  rowSums(betas_lagged[, -1]*SPY_design[index(rolling_betas$coefficients)]) + 
  betas_lagged[, 1]

# cumulative returns_backtest: invest proportional to returns_forecast
# note that "-returns_backtest" has minus sign, because rolling_betas are 
# produced in opposite sign by roll_pcr()
returns_backtest <- cumsum(returns_forecast * returns_advanced[index(returns_forecast)])
x11()
chart_Series(x=-returns_backtest, name="cumulative returns")


### test for data snooping in PCR using random data

# create time index of one second intervals
indeks <- seq(from=as.POSIXct("2016-01-01 00:00:00"),
              to=as.POSIXct("2016-01-30 00:00:00"), by="1 sec")

# perform one random PCR simulation using function run_random_pcr() (below)
run_random_pcr(indeks)

# takes very long!!! - perform 100 random PCR simulations - takes very long!!!
pnls <- sapply(1:100, function(x, indeks) run_random_pcr(indeks), indeks=indeks)
hist(pnls, breaks="FD", xlim=c(-5e4, 5e4), main="distribution of Pnl's")

# perform a random PCR and return the Pnl
run_random_pcr <- function(indeks) {
  xtes <- xts(exp(cumsum(rnorm(length(indeks), sd=0.001))), order.by=indeks)
  ohlc <- xts::to.period(x=xtes, period="minutes", name="random")
  ohlc <- cbind(ohlc, sample(x=10*(2:18), size=NROW(ohlc), replace=TRUE))
  colnames(ohlc)[ 5] <- "random.volume"
  returns_running <- run_returns(xtes=ohlc)
  returns_advanced <- rutils::lagxts(returns_running, k=-1)
  returns_rolling <- roll_vwap(ohlc=ohlc, xtes=returns_running, look_back=look_back)
  var_running <- run_variance(ohlc=ohlc)
  skew_running <- run_skew(ohlc=ohlc)
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

