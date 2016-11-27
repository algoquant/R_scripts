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
beta_s <- rollapply(env_etf$re_turns, width=252, 
                    FUN=function(design_matrix) 
                      coef(lm(reg_formula, data=design_matrix))[2],
                    by=22, by.column=FALSE, align="right")
beta_s <- na.omit(beta_s)
# plot beta_s in x11() window
x11()
chart_Series(x=beta_s, name=paste("rolling betas", format(reg_formula)))

# perform daily rolling beta regressions in parallel - FAST!
beta_s <- roll::roll_lm(x=env_etf$re_turns[, "VTI"], 
                        y=env_etf$re_turns[, "XLP"],
                        width=252)$coefficients
chart_Series(x=beta_s, name=paste("rolling betas", format(reg_formula)))

# compare speed of rollapply() versus roll_lm()
library(microbenchmark)
da_ta <- env_etf$re_turns["2012", c("VTI", "XLP")]
summary(microbenchmark(
  rollapply=rollapply(da_ta, width=22, 
                      FUN=function(design_matrix) 
                        coef(lm(reg_formula, data=design_matrix))[2],
                      by.column=FALSE, align="right"), 
  roll_lm=roll::roll_lm(x=da_ta[, "VTI"], 
                        y=da_ta[, "XLP"],
                        width=22)$coefficients, 
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


### now a more interesting example
### load SPY_design design matrix 

# load design matrix called SPY_design containing columns of aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)

# create advanced returns
returns_running <- run_returns(x_ts=SPY)
returns_advanced <- rutils::lag_xts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"
tail(cbind(returns_advanced, returns_running))


### create design matrix called SPY_design containing columns of aggregations

win_dow <- 5
returns_running <- run_returns(x_ts=SPY)
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

sharpe_running <- run_sharpe(oh_lc=SPY)
sharpe_rolling <- roll_vwap(oh_lc=SPY, x_ts=sharpe_running, win_dow=win_dow)
colnames(sharpe_running) <- "sharpe_running"
colnames(sharpe_rolling) <- "sharpe_running.WA5"

sharpe_rolling <- roll_sharpe(oh_lc=SPY, win_dow=win_dow)
colnames(sharpe_rolling) <- "sharpe_rolling"

hurst_rolling <- roll_hurst(oh_lc=SPY, win_dow=win_dow)
colnames(hurst_rolling) <- "hurst_rolling"

# select most significant factors plus interaction terms
SPY_design <- cbind(returns_running, returns_rolling, var_running, skew_running, 
                    hurst_rolling, returns_running*var_running, returns_running*skew_running)
colnames(SPY_design) <- c(colnames(SPY_design)[1:4], "hurst", "rets_var", "rets_skew")

# create advanced returns
returns_advanced <- rutils::lag_xts(returns_running, k=-1)
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
betas_lagged <- rutils::lag_xts(rolling_betas$coefficients)
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
in_dex <- seq(from=as.POSIXct("2016-01-01 00:00:00"),
              to=as.POSIXct("2016-01-30 00:00:00"), by="1 sec")

# perform one random PCR simulation using function run_random_pcr() (below)
run_random_pcr(in_dex)

# takes very long!!! - perform 100 random PCR simulations - takes very long!!!
pnl_s <- sapply(1:100, function(x, in_dex) run_random_pcr(in_dex), in_dex=in_dex)
hist(pnl_s, breaks="FD", xlim=c(-5e4, 5e4), main="distribution of Pnl's")

# perform a random PCR and return the Pnl
run_random_pcr <- function(in_dex) {
  x_ts <- xts(exp(cumsum(rnorm(length(in_dex), sd=0.001))), order.by=in_dex)
  oh_lc <- xts::to.period(x=x_ts, period="minutes", name="random")
  oh_lc <- cbind(oh_lc, sample(x=10*(2:18), size=NROW(oh_lc), replace=TRUE))
  colnames(oh_lc)[ 5] <- "random.volume"
  returns_running <- run_returns(x_ts=oh_lc)
  returns_advanced <- rutils::lag_xts(returns_running, k=-1)
  returns_rolling <- roll_vwap(oh_lc=oh_lc, x_ts=returns_running, win_dow=win_dow)
  var_running <- run_variance(oh_lc=oh_lc)
  skew_running <- run_skew(oh_lc=oh_lc)
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

