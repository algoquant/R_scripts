####################################
### Script for performing rolling forecasting PCR regressions 
### using package roll.
### The objective is to reproduce the betas_running$coefficients.
####################################


### Install packages rutils, HighFreq, and roll from github

install.packages("devtools")
devtools::install_github(repo="algoquant/rutils")
devtools::install_github(repo="algoquant/HighFreq")
devtools::install_github(repo="jjf234/roll")
library(HighFreq)
library(roll)


### Load design matrix called SPY_design containing columns of data aggregations

load("C:/Develop/data/SPY_design.RData")
head(SPY_design)


### Perform rolling forecasting PCR regressions in parallel

# calculate close to close returns
returns_running <- run_returns(x_ts=SPY)
# calculate returns advanced in time
returns_advanced <- rutils::lag_xts(returns_running, k=-1)
colnames(returns_advanced) <- "returns_advanced"

# perform rolling forecasting PCR regressions in parallel
# use only the first principal component: argument "comps"
betas_running <- roll_pcr(x=SPY_design["2011/2012", ], 
                          y=returns_advanced["2011/2012", ], 
                          width=1*60, comps=1:1, min_obs=1)
betas_running$coefficients[1, ] <- 0

# The objective is to reproduce the betas_running$coefficients


### inspect betas_running$coefficients

# calculate mean beta coefficients
beta_s <- sapply(betas_running$coefficients, mean)

# calculate rolling mean beta coefficients over time
betas_rolling <- rutils::roll_sum(x_ts=betas_running$coefficients, win_dow=11)/11
tail(betas_rolling)



