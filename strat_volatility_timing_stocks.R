############## homework
# Summary: Simulate a volatility timing strategy for stocks.
# Go long the stocks with volatilities less than the threshold.

## Run the setup code below

library(rutils)

# Load daily S&P500 percentage stock returns
# Download the file from the share drive:
# https://drive.google.com/drive/folders/1jNtoNCL8mLJyda0wu1pLCnjvqIPzd_WL
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- retstock
datev <- zoo::index(retp) # Dates vector
nrows <- NROW(retp) # number of rows
nstocks <- NCOL(retp) # number of stocks

# Calculate the market returns as the sum of the stock returns
retm <- rowSums(retp, na.rm=TRUE)

## End of setup code


# 1. (20pts)
# Calculate the EMA volatility of the stock returns
# using the function HighFreq::run_var().

lambdaf <- 0.94

vars <- HighFreq::run_var(retp, lambdaf=lambdaf)
vols <- sqrt(vars[, (nstocks+1):(2*nstocks)])

# You should get the following outputs:
dim(vols)
# [1] 9077  717


# Simulate a volatility timing strategy for stocks.
# For each day, go long the stocks with volatilities 
# less than the threshold volt and ignore the stocks 
# with volatilities above the threshold.
# Calculate the strategy PnLs as the sum of the returns 
# of the stocks with volatility below the threshold.
# Scale the PnLs to have the same volatility as the 
# market returns retm.
# You can use the functions rowSums() and rutils::lagit().

volt <- 0.02
lowvols <- (vols < volt)
lowvols <- rutils::lagit(lowvols, lagg=1)
pnls <- rowSums(lowvols*retp, na.rm=TRUE)
sdm <- sd(retm)
pnls <- pnls*sdm/sd(pnls)


# You should get the following outputs:
NROW(pnls)
# [1] 9077
sd(pnls)
# [1] 6.855101
head(pnls)
# [1] 0.0000000 -0.9723944 -2.3425605  0.8864859 -3.4509733 -1.6195441
tail(pnls)
# [1] 2.4723304 -0.8427842 -2.2301335  4.5138325  5.3267600 -3.8378921


# Combine the PnLs with the market returns retm, 
# coerce them into an xts time series and calculate 
# the Sharpe and Sortino ratios.

wealthv <- cbind(retm, pnls)
colnames(wealthv) <- c("EqualWeight", "VolTiming")
wealthv <- xts::xts(wealthv, order.by=datev)
sharper <- sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get the following outputs:
sharper
#         EqualWeight VolTiming
# Sharpe    0.5217810 0.7550849
# Sortino   0.6161066 0.9316949


# 2. (20pts)
# Calculate a list of the strategy PnLs by performing 
# an lapply() loop over the vector of lambda values:

lambdav <- seq(0.85, 0.95, by=0.01)

pnll <- lapply(lambdav, function(lambdaf) {
  vars <- HighFreq::run_var(retp, lambdaf=lambdaf)
  vols <- sqrt(vars[, (nstocks+1):(2*nstocks)])
  lowvols <- (vols < volt)
  lowvols <- rutils::lagit(lowvols, lagg=1)
  pnls <- rowSums(lowvols*retp, na.rm=TRUE)
  pnls <- pnls*sdm/sd(pnls)
  pnls <- xts::xts(pnls, order.by=datev)
  return(pnls)
}) # end lapply
names(pnll) <- lambdav

# You should get the following outputs:
NROW(pnll)
# [1] 11
names(pnll)
# "0.85" "0.86" "0.87" "0.88" "0.89" "0.9"  "0.91" "0.92" "0.93" "0.94" "0.95"

# Calculate a vector of Sharpe ratios of the PnLs
# and plot them.
# Your plot should be similar to strat_voltiming_stocks_sharpe.png

sharpev <- sqrt(252)*sapply(pnll, function(x) {
  return(mean(x)/sd(x))
}) # end sapply
names(sharpev) <- lambdav

plot(lambdav, sharpev, type="b", 
     main="Sharpe Ratios Of The Stock Volatility Timing Strategy",
     xlab="Lambda", ylab="Sharpe ratio")

# Find the lambda value with the maximum Sharpe ratio.

whichmax <- which.max(sharpev)
lambdaf <- lambdav[whichmax]
pnls <- pnll[[whichmax]]

# You should get the following outputs:
lambdaf
# [1] 0.88


# 3. (20pts)
# Calculate a list of the strategy PnLs by performing 
# an lapply() loop over the vector volv of volt values.
# Calculate a vector of Sharpe ratios of the PnLs
# and plot them.
# Your plot should be similar to strat_voltiming_stocks_sharpe2.png

volv <- seq(0.005, 0.015, by=0.001)

vars <- HighFreq::run_var(retp, lambdaf=lambdaf)
vols <- sqrt(vars[, (nstocks+1):(2*nstocks)])
pnll <- lapply(volv, function(volt) {
  lowvols <- (vols < volt)
  lowvols <- rutils::lagit(lowvols, lagg=1)
  pnls <- rowSums(lowvols*retp, na.rm=TRUE)
  pnls <- xts::xts(pnls, order.by=datev)
  return(pnls)
}) # end lapply
names(pnll) <- volv

sharpev <- sqrt(252)*sapply(pnll, function(x) {
  return(mean(x)/sd(x))
}) # end sapply
names(sharpev) <- volv
plot(volv, sharpev, type="b", 
     main="Sharpe Ratios Of The Stock Volatility Timing Strategy",
     xlab="Volatility threshold", ylab="Sharpe ratio")

# Find the volt value with the maximum Sharpe ratio.

whichmax <- which.max(sharpev)
volt <- volv[whichmax]
pnls <- pnll[[whichmax]]

# You should get the following outputs:
volt
# [1] 0.01

# Plot a dygraph() of the optimal cumulative PnLs.
# Your plot should be similar to strat_voltiming_stocks.png

colnames(wealthv) <- paste("Sharpe", paste(c("EqualWeight", "VolTiming"), round(sharper[1, ], 3), sep="="))
captiont <- paste("Stocks EMA Volatility Timing, lambda =", lambdaf, ", threshold =", volt)
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)


# 4. (20pts)
# Perform the Treynor-Mazuy market timing skill test 
# for the optimal volatility timing strategy, using 
# retm as the market returns. 
# Your plot should be similar to strat_voltiming_stocks_timing.png

# Create the design matrix for the Treynor-Mazuy test
desm <- cbind(pnls, retm)
# Add the Treynor-Mazuy term, which is the square of the market returns
desm <- cbind(desm, desm[, 2]^2)
colnames(desm) <- c("pnls", "market", "Treynor")
# Perform the Treynor-Mazuy test
desm <- as.data.frame(desm)
regmod <- lm(pnls ~ market + Treynor, data=desm); summary(regmod)
# Plot the residual scatterplot
resids <- regmod$residuals
plot.default(x=desm[, "market"], y=resids, xlab="market returns", ylab="voltiming")
title(main="Treynor-Mazuy Market Timing Test\n for Stock Volatility Timing", line=0.5)
# Plot the fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["market", "Estimate"]*desm[, "market"]
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=desm[, "market"], y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))

# Comment: The Treynor-Mazuy test t-value is 3.15, 
# which is significant, and means that the volatility timing 
# strategy has market timing skill.

# Comment:
# The strategy has market timing skill because it changes its 
# market exposure depending on the level of the volatility.
# When the market volatility is high, it reduces the number
# of stocks in its portfolio, which reduces its market 
# exposure.  
# When the market volatility is low, it buys more stocks, 
# and increases its market exposure.

# Comment:
# The strategy is dynamic and adjusts its market exposure 
# according to the level of the market volatility.
# In periods of low market volatility, there are more stocks 
# whose trailing volatilities are less than the threshold.  
# But in periods of high market volatility, there are fewer 
# stocks whose trailing volatilities are less than the 
# threshold.  
# The strategy deleverages in periods of high market 
# volatility, and leverages up in periods of low market 
# volatility.  
# When the market volatility is high, the strategy 
# rebalances to safer low volatility stocks, and when 
# the market volatility is low, the strategy rebalances 
# to more higher volatility stocks, with higher returns.  
# So the strategy times the market by using the stock 
# volatility.


# Extra
####################
# Comment: The downside deviation doesn't perform better 
# than the regular deviation.

# Calculate the PnLs of the high volatility strategy.
# Shorting the high volatility stocks doesn't perform well.

highvols <- !lowvols
pnlh <- rowSums(highvols*retp, na.rm=TRUE)
pnlh <- pnlh*sdm/sd(pnlh)
pnlh <- xts::xts(pnlh, order.by=datev)
dygraph(cumsum(pnlh), main="P&L of the high volatility strategy")
sqrt(252)*mean(pnlh)/sd(pnlh)

wv <- seq(-0.5, 0.5, by=0.1)
sharpev <- sqrt(252)*sapply(wv, function(x) {
  pnln <- pnls - x*pnlh
  return(mean(pnln)/sd(pnln))
}) # end sapply
sharpev
