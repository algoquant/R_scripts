#################################
### FRE7241 Homework #6 Solution Tuesday, December 9, 2025
#################################
# Max score 100pts

# This is the old code with the bug in it:
lookb <- 1 ##  Look-back interval in months
startp <- endd[max(1, tday-lookb)]
retis <- retp[startp:(endd[tday-1]-1), ]

# The bug is that the look-back period should
# greater than 1, because then endd[tday-1]
# is 1 day before startp.
# But it works fine for lookb = 1
# Why?


############## Part II
# Summary: Simulate a momentum strategy using the 
# idiosyncratic stock returns (residuals).

# Comments:
# The performance is much better for percentage returns
# (Sharpe=1.51) than for log returns (Sharpe=0.95).
# Not clear why.

# Idea:
# Trade the more volatile stocks because they are more trending.
# Doesn't work - low volatility stocks work better.

# Idea:
# Trade the principal components of the stock returns.
# Doesn't work.


# Idea:
# Calculate the principal components using LASSO.
# Calculate the principal components for the stocks
# with the highest autocorrelations.
# Calculate the principal components for the stocks
# with the highest correlations with XLK.

# Idea:
# Trade long only, without shorting stocks.

# Idea:
# Calculate the stock betas using aggregated returns.

# The equally-weighted portfolio of the aggregated
# returns has very good pnls?  Not really
retagg <- rowMeans(retw)
datev <- zoo::index(retw)
retagg <- xts(retagg, datev)
dygraph(cumsum(retagg))



# Idea:
# Test separately the low and high volatility stocks.
# Calculate the stock betas and alphas using
# different look-back intervals.
# Calculate the betas using a long looking-back 
# interval.
# Calculate the alphas using a short looking-back 
# interval.
# It doesn't improve the performance.
# The best performance is for both look-back 
# intervals equal to about 65 months.


# Idea:
# Bet against beta.


## Run the setup code below

library(rutils)

# Calculate the percentage SPY returns
retm <- na.omit(rutils::etfenv$returns$SPY)
colnames(retm) <- "SPY"
datev <- zoo::index(retm)
# Load daily S&P500 stock prices
# Download the file from the share drive:
# https://drive.google.com/drive/folders/1jNtoNCL8mLJyda0wu1pLCnjvqIPzd_WL
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select the stock prices since GOOGL
datev <- zoo::index(na.omit(pricestock$GOOGL))
pricev <- pricestock[datev]
retm <- retm[datev]
# Select the stock prices since SPY
# Select stocks with no NA values in their prices
numna <- sapply(pricev, function(x) sum(is.na(x)))
pricev <- pricev[, numna == 0]
# Drop penny stocks
pricel <- last(pricev)
pricel <- drop(coredata(pricel))
pricev <- pricev[, pricel > 1]
# Calculate the percentage stock returns
retp <- rutils::diffit(pricev)/rutils::lagit(pricev)
# Calculate the log stock returns
# retp <- rutils::diffit(log(pricestock))
retp[1, ] <- 0
# Select high volatility stocks
volv <- apply(retp, 2, sd)
volmed <- median(volv)
retp <- retp[, volv > volmed]
# Select 100 random stocks
set.seed(1121, "Mersenne-Twister", sample.kind="Rejection")
retp <- retp[, sample(NCOL(retp), 100)]
# Get the column names
nrows <- NROW(retp)
nstocks <- NCOL(retp)
colv <- colnames(retp)
datev <- zoo::index(retp)


# Calculate the correlation of stock returns
cormat <- cor(retp, use="pairwise.complete.obs")
# Sum up the off-diagonal elements of the correlation matrix
sumoff <- sum(cormat) - sum(diag(cormat))

# Calculate the principal components of stock returns
eigend <- eigen(cormat)
retpca <- retp %*% eigend$vectors
retpca <- xts::xts(retpca, order.by=datev)
colnames(retpca) <- paste0("PC", 1:nstocks)
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraph(cumsum(retpca[endd, 1:5]),
        main="Cumulative Returns of First Five Principal Components") %>%
  dyOptions(colors=RColorBrewer::brewer.pal(5, "Set1"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the sum of the partial autocorrelations of PCAs
pacsum <- sapply(retpca[, 1:30], function(x) {
  pacff <- stats::pacf(x, plot=FALSE, lag.max=20)
  return(sum(pacff$acf))
}) ## end sapply
plot(pacsum, type="b",
     main="Sum of Partial Autocorrelations of First 30 PCAs",
     xlab="PCA Index", ylab="Sum of Partial Autocorrelations")


# Calculate the alphas 
varm <- drop(var(retm))
retmm <- mean(retm)
alphav <- lapply(retp, function(rets) {
  betac <- drop(cov(rets, retp)/varm)
  return(rets - betac*retm)
})  ## end lapply
alphav <- do.call(cbind, alphav)
# Calculate the correlation of the alphas
cormat <- cor(alphav, use="pairwise.complete.obs")
sum(cormat) - sum(diag(cormat))



## End of setup code


# 1. (40pts) 
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)

# Perform a parallel loop over the end points,
# starting from the third end point.
# It may take long to run the loop.
# 
# At each end point, calculate the stock betas and 
# alphas over the previous month.
# 
# Calculate the out-of-sample stock alphas for 
# the next month, as the stock returns minus the 
# product of the stock betas times the VTI returns.
# 
# Calculate a vector of the momentum stock weights 
# as the differences between the alphas minus their 
# median value for that month.
# 
# Calculate the out-of-sample returns of the momentum 
# strategy for that month as the product of the stock 
# alphas times the stock weights.
# 
# Combine the momentum strategy returns with the 
# equal-weighted portfolio returns for that month.
# Combine the monthly returns into a single xts 
# time series.
# Add a third column to the time series with the
# combined returns of the momentum strategy and
# the equal-weighted portfolio.

lookb <- 65 ##  Look-back interval in months
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1

pnls <- mclapply(3:(npts-1), function(tday) {
  
  ##  Select the in-sample returns
  startp <- endd[max(1, tday-lookb)]
  # startp <- max(1, endd[tday-1]-lookb)
  # Include the most recent period
  # endp <- endd[tday]
  # Exclude the most recent period
  endp <- endd[tday-1]
  retis <- retp[startp:endp, ]
  datis <- zoo::index(retis)
  retmis <- retm[datis]
  varm <- drop(var(retmis))
  retmm <- mean(retmis)
  
  # Calculate the in-sample stock volatilities, betas, and alphas
  riskret <- lapply(retis, function(rets) {
    stdev <- sd(rets)
    betac <- drop(cov(rets, retmis)/varm)
    resid <- rets - betac*retmis
    alphac <- mean(rets) - betac*retmm
    c(alpha=alphac, beta=betac, stdev=stdev, ivar=var(resid))
  })  ## end lapply
  riskret <- do.call(rbind, riskret)
  
  # Extract the risk vectors
  alphav <- riskret[, "alpha"]
  betav <- riskret[, "beta"]
  volv <- riskret[, "stdev"]
  ivarv <- riskret[, "ivar"]
  # Calculate the momentum weights
  weightv <- alphav/ivarv
  weightv[is.na(weightv)] <- 0
  ##  Select the out-of-sample returns
  retos <- retp[(endd[tday]+1):endd[tday+1], ]
  datos <- zoo::index(retos)
  # Calculate the out-of-sample stock alphas
  retmos <- retm[datos]
  alphav <- lapply(colnames(retp), function(symb) {
    retos[, symb] - retmos*betav[symb]
  }) ## end lapply
  alphav <- do.call(cbind, alphav)
  alphav[is.na(alphav)] <- 0
  # Calculate the out-of-sample pnls of low and high alpha stocks
  pnlos <- alphav %*% weightv
  # Calculate the equal-weighted portfolio pnls
  retew <- rowMeans(retos, na.rm=TRUE)
  
  return(xts(cbind(retew, pnlos*sd(retew)/sd(pnlos)), order.by=datos))
  
}, mc.cores=ncores)  ## end mclapply

# Combine the PnLs into a single xts series
pnls <- rutils::do_call(rbind, pnls)
pnls[is.na(pnls)] <- 0
pnls[, 2] <- pnls[, 2]*sd(retm)/sd(pnls[, 2])
pnls <- cbind(pnls, 0.5*(pnls[, 1] + pnls[, 2]))
colnames(pnls) <- c("EqualWeight", "Momentum", "Combined")


# Calculate the Sharpe and Sortino ratios
sharper <- sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sharper <- round(sharper, 3)

# For all 717 stocks weekly rebalancing, lookb=45 weeks:
# You should get outputs similar to:
#         EqualWeight Momentum Combined
# Sharpe    0.9081984 1.068037 1.443835
# Sortino   1.1606118 1.351227 1.804493

# Plot a dygraph of the momentum strategy
captiont <- paste("Idiosyncratic Stock Momentum", "/ \n", 
                  paste0(paste(colnames(sharper), "= "), sharper[1, ], collapse=" / "))
endw <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Your plot should be similar to momentum_alpha.png



############## Strategy with fixed betas


# Calculate the in-sample stock volatilities, betas, and alphas
retisync <- lapply(retp, function(rets) {
  # cat("\nProcessing stock:", colnames(rets), "\n")
  retc <- na.omit(cbind(rets, retm))
  betac <- drop(cov(retc[, 1], retc[, 2])/var(retc[, 2]))
  resid <- retc[, 1] - betac*retc[, 2]
  return(resid)
})  ## end lapply
retisync <- do.call(cbind, retisync)

lookb <- 25
pnls <- mclapply(3:(npts-1), function(tday) {
  ##  Select the in-sample returns
  startp <- endd[max(1, tday-lookb)]
  # startp <- max(1, endd[tday-1]-lookb)
  # Include the most recent period
  # endp <- endd[tday]
  # Exclude the most recent period
  endp <- endd[tday-1]
  retis <- retisync[startp:endp, ]
  retis[is.na(retis)] <- 0
  ivar <- apply(retis, 2, var)
  # Calculate the momentum weights
  weightv <- colMeans(retis)/ivar
  weightv[is.na(weightv)] <- 0
  ##  Select the out-of-sample returns
  retos <- retisync[(endd[tday]+1):endd[tday+1], ]
  retos[is.na(retos)] <- 0
  # Calculate the out-of-sample pnls
  pnlos <- retos %*% weightv
  return(pnlos)
  
}, mc.cores=ncores)  ## end mclapply

# Combine the monthly PnLs into a single xts series
pnls <- rutils::do_call(rbind, pnls)
pnls[is.na(pnls)] <- 0
pnls[1:33] <- 0
# plot(cumsum(pnls), t="l")
pnls <- pnls*sd(retm)/sd(pnls)
pnls <- rbind(matrix(numeric(endd[3])), pnls)
pnls <- cbind(retm, pnls)
pnls <- cbind(pnls, 0.5*(pnls[, 1] + pnls[, 2]))
colnames(pnls) <- c("EqualWeight", "Momentum", "Combined")

# For all 717 stocks reverse momentum strategy, with
# weekly rebalancing, lookb=1 weeks:
# You should get outputs similar to:
#         EqualWeight Momentum Combined
# Sharpe    0.5468499 0.9961704 1.020533
# Sortino   0.6498130 1.4404620 1.417196



