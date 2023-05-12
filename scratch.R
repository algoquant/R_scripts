
# Simulate multiple daily stock momentum strategies
lambdas <- seq(0.1, 0.9, 0.01)
pnls <- sapply(lambdas, function(lambda) {
  retma <- HighFreq::run_mean(retp, lambda=lambda)
  posv <- rutils::lagit(retma, lagg=1)
  pnls <- (retp*posv)
  mean(pnls)/sd(pnls)
})

# Plot Sharpe ratios of momentum strategies
plot(x=lambdas, y=pnls, t="l",
     main="EWMA Trend Strategies",
     xlab="lambdas", ylab="Sharpe")


##############
# online PCA

# Setup data
ncols <- 9
nrows <- 1e3
# Simulate random uncorrelated returns
# retp <- matrix(rnorm(nrows*ncols), nrows, ncols)
# retp <- retp %*% diag(sqrt(1:ncols))

# Calculate random covariance matrix
covmat <- matrix(runif(ncols^2), nc=ncols)
covmat <- t(covmat) %*% covmat
# Calculate the Cholesky matrix
cholmat <- chol(covmat)
# Simulate random uncorrelated returns
retp <- matrix(rnorm(ncols*nrows), nc=ncols)
# Calculate correlated returns by applying Cholesky
retp <- retp %*% cholmat

# Calculate the PCA
pcaf <- prcomp(retp)

# Warmup period
startp <- nrows/10
pcav <- prcomp(retp[1:startp, ])
meanv <- pcav$center
pcav <- list(values=pcav$sdev[1:ncols]^2, vectors=pcav$rotation[, 1:ncols])

# Unit initial guess
pcav <- list(values=rep(1, ncols), vectors=diag(ncols))
pcav <- list(values=matrix(rep(1, ncols)), vectors=diag(ncols))

set.seed(1121)
vecm <- sapply(1:ncols, function(x) {
  x <- rnorm(ncols)
  x/sd(x)
  })
apply(vecm, 2, sd)
pcav <- list(values=rep(1, ncols), vectors=vecm)
# pcav <- list(values=matrix(rep(1, ncols)), vectors=vecm)

# meanv <- numeric(ncols)
meanv <- retp[1, ]
volv <- retp[1, ]
eigenret <- numeric(ncols)

covmat <- cov(rets)
eigend <- HighFreq::calc_eigen(covmat)
volv <- sapply(rets, var)
meanv <- colMeans(rets)
eigenret <- numeric(ncols)

push_sga(newdata=retp[i, ], evals=pcav$values, evecs=pcav$vectors, eigenret=eigenret, meanv=meanv, volv=volv, lambda=0.9090909, gamma=0.9)


# Update the PCA recursively
for (i in 2:nrows) {
  meanv <- onlinePCA::updateMean(meanv, retp[i, ], n=10)
  push_sga(newdata=retp[i, ], evals=pcav$values, evecs=pcav$vectors, eigenret=eigenret, meanv=meanv, volv=volv, lambda=0.9090909, gamma=0.9)
  sgapca_exCn(pcav$values, pcav$vectors, retp[i, ], meanv=meanv, n=10, gamma=0.9)
  # pcav <- onlinePCA::ghapca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], gamma=0.9, center=meanv)
  # pcav <- sgapca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], gamma=0.9, center=meanv)
  # pcav <- onlinePCA::sgapca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], gamma=0.9, center=meanv)
  # pcav <- onlinePCA::ccipca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], n=(i-1), l=2, center=meanv)
}  # end for

# Compare PCA methods
pcaf$sdev^2
drop(pcav$values)



##############
# btmomrun PCA Momentum Strategies

retscaled <- lapply(retp[insample], function(x) x/sd(x))
retscaled <- do.call(cbind, retscaled)

pcad <- prcomp(retscaled, center=FALSE, scale=FALSE)
pcad <- eigen((t(retscaled) %*% retscaled)/(NROW(retscaled)-1))
sqrt(foo$values[1:7])
pcad$sdev[1:7]
foo$vectors[1:7, 1:7]
pcad$rotation[1:7, 1:7]
all.equal(abs(foo$vectors), abs(pcad$rotation), check.attributes=FALSE)

library(microbenchmark)
summary(microbenchmark(
  prcomp=prcomp(retscaled, center=FALSE, scale=FALSE),
  eigen=eigen((t(retscaled) %*% retscaled)/(NROW(retscaled)-1)),
  times=10))[, c(1, 4, 5)]



lambdav <- seq(from=0.98, to=0.999, by=0.002)
lambdav <- seq(from=0.99, to=0.999, by=0.001)
pnls <- sapply(lambdav, btmomrun, retp=retpca[, 1:dimax])

lambdav <- seq(from=0.6, to=0.9, by=0.1)
pnls <- sapply(lambdav, btmomrun, trend=(-1), retp=retpca[, (dimax+1):NCOL(retpca)])

pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdav)
pnls <- xts::xts(pnls, datev)
tail(pnls)

colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)



############## test
# Summary: Rank the stocks according to their alpha.

## Run all the setup code below.

# Load S&P500 stock prices use your own directory
library(rutils)
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

## End of setup code.


# 1. (20pts)
# Extract the VTI returns from rutils::etfenv.
# You can use the function na.omit().

retvti <- na.omit(rutils::etfenv$returns$VTI)

# You should get the following output:
head(retvti)
#                     VTI
# 2001-06-01  0.006944472
# 2001-06-04  0.004315932
# 2001-06-05  0.014536383
# 2001-06-06 -0.008525201
# 2001-06-07  0.005123837
# 2001-06-08 -0.008554372


# Extract the closing S&P500 stock prices and
# calculate their log returns.
# Subset the S&P500 stock prices so that their 
# start date is the same as retvti.
# 
# You can use the functions eapply(), quantmod::Cl(), 
# rutils::do_call(), zoo::na.locf(), rutils::get_name(), 
# index(), and colnames().



# Extract the closing prices
pricev <- eapply(sp500env, quantmod::Cl)
# Flatten the prices into a single xts series
pricev <- rutils::do_call(cbind, pricev)
# Carry forward and backward non-NA prices
pricev <- zoo::na.locf(pricev, na.rm=FALSE)
pricev <- zoo::na.locf(pricev, fromLast=TRUE)
# Drop ".Close" from column names
colnames(pricev) <- rutils::get_name(colnames(pricev))
# Select prices after the year 2000
pricev <- pricev[index(retvti), ]

# You should get the following output:
head(pricev[, 1:5])
#                  EL      SEE     VRSK      PPG      DXC
# 2001-06-01 15.74512 15.31367 26.59934 16.46893 51.46832
# 2001-06-04 15.81309 15.28024 26.59934 16.56213 51.46832
# 2001-06-05 15.94147 15.28396 26.59934 16.68238 51.46832
# 2001-06-06 15.86973 15.13910 26.59934 16.59219 51.46832
# 2001-06-07 16.15669 15.22824 26.59934 16.64630 51.46832
# 2001-06-08 16.04719 15.09453 26.59934 16.45991 51.46832

# Calculate the log returns of pricev.
# You can use the functions rutils::diffit() and log().

retp <- rutils::diffit(log(pricev))

# You should get the following output:
head(retp[, 1:5])
#                      EL           SEE VRSK          PPG DXC
# 2001-06-01  0.000000000  0.0000000000    0  0.000000000   0
# 2001-06-04  0.004307257 -0.0021852625    0  0.005643048   0
# 2001-06-05  0.008085656  0.0002430429    0  0.007234613   0
# 2001-06-06 -0.004510393 -0.0095227188    0 -0.005421047   0
# 2001-06-07  0.017920781  0.0058708584    0  0.003256153   0
# 2001-06-08 -0.006800354 -0.0088192634    0 -0.011260562   0



# 2. (20pts)
# Calculate the stock betas and alphas.  Use VTI returns 
# as a proxy for the market.
# You can use the functions sapply(), cov(), var(), mean(),
# c(), and t().
# You cannot use the function lm().

varvti <- drop(var(retvti))
meanvti <- mean(retvti)

regd <- sapply(retp, function(rets) {
  betav <- cov(rets, retvti)/varvti
  alphav <- mean(rets) - betav*meanvti
  c(alpha=alphav, beta=betav)
})  # end sapply
regd <- t(regd)

# You should get the following outputs:
class(regd)
# [1] "matrix" "array" 
dim(regd)
# [1] 726   2
head(regd)
#              alpha      beta
# EL    4.444712e-05 0.8582245
# SEE   3.563555e-06 0.9886331
# VRSK  1.957477e-04 0.3756122
# PPG  -1.850810e-05 1.0590364
# DXC   1.679548e-04 0.4642649
# RTX  -5.157425e-06 1.0164509


# Sort regd according to alpha in descending order.
# You can use the function order().

regd <- regd[order(regd[, "alpha"], decreasing=TRUE), ]

# You should get the following outputs:
head(regd)
#              alpha          beta
# DISCA 0.0003136284 -0.0003986137
# KG    0.0003135034  0.0000000000
# CHKAQ 0.0003135034  0.0000000000
# PCP   0.0003133615  0.0004526781
# MON   0.0003133124  0.0006095027
# BRCM  0.0003127016  0.0025576078
tail(regd)
#             alpha     beta
# X   -0.0002160532 1.689157
# ATI -0.0002278351 1.726739
# GNW -0.0002316193 1.738810
# MS  -0.0002415598 1.770517
# CLF -0.0002435795 1.776960
# LNC -0.0002802909 1.894060


#######
# Scatterplot of alpha


#######
# Low beta/High alpha strategies

# In-sample alpha and beta
retvtis <- retvti["/2010"]
varvti <- var(retvtis)
meanvti <- mean(retvtis)

regd <- sapply(retp["/2010"], function(rets) {
  betav <- cov(rets, retvtis)/varvti
  alphav <- mean(rets) - betav*meanvti
  c(alpha=alphav, beta=betav)
})  # end sapply
regd <- t(regd)

regd <- regd[!(regd[, "beta"] == 0), ]
# Sort by beta
regd <- regd[order(regd[, "beta"]), ]
# Sort by alpha
regd <- regd[order(regd[, "alpha"], decreasing=TRUE), ]
head(regd)


foo <- pricev["2011/", rownames(regd[1:(NROW(regd) %/% 2), ])]
foo <- lapply(foo, function(x) x/as.numeric(x[1]))
foo <- rutils::do_call(cbind, foo)
# head(foo[, 1:5])
# foo <- xts::xts(rowMeans(foo), zoo::index(foo))
foo <- rutils::diffit(rowMeans(foo))
foo <- foo*sd(retvti["2011/", ])/sd(foo)
# dygraphs::dygraph(foo)

# Calculate compounded wealth from returns
wealthv <- cbind(retvti["2011/", ], foo)
colnames(wealthv) <- c("VTI", "Low beta")
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))

# Plot compounded wealth
endd <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low Beta Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)



#######


# Create a vector of strings called symbolv from the 
# column names of rutils::etfenv$returns, excluding 
# the symbols "VXX", SVXY", and "MTUM".
# You must use R code, not simply typing the symbol strings.
# You can use the functions colnames(), match(), c(), 
# the "==" operator, and the "!" operator.

symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[-match(c("VXX", "SVXY", "MTUM"), symbolv)]
# Or
symbolv <- symbolv[!((symbolv == "VXX") | (symbolv == "SVXY") | (symbolv == "MTUM"))]

# You should get the following output:
symbolv
#  [1] "VTI" "VEU" "IEF" "VNQ" "DBC" "XLY" "XLP" "XLE" "XLF" "XLV"
# [11] "XLI" "XLB" "XLK" "XLU" "VYM" "IVW" "IWB" "IWD" "IWF"

# Extract the symbolv columns from rutils::etfenv$returns, 
# and call it returns.
# Remove any rows with NA values from returns. 
# You can use the function na.omit().

retp <- na.omit(rutils::etfenv$returns[, symbolv])


# You should get the following output:
colnames(retp)
#  [1] "VTI" "VEU" "IEF" "VNQ" "DBC" "XLY" "XLP" "XLE" "XLF" "XLV"
# [11] "XLI" "XLB" "XLK" "XLU" "VYM" "IVW" "IWB" "IWD" "IWF"
# 
dim(retp)
# [1] 2870   19

# Create a function called calc_betas() which performs 
# a regression and returns a vector of betas.
# calc_betas() should accept the following arguments:
#   tseries - time series of asset returns,
#   mar_ket - time series of market returns,
#   calc_bull_bear - Boolean if TRUE then calculate the 
#     bull-market and bear-market betas, else only the 
#     single beta.  Default is FALSE.
#   threshold - threshold level for market returns.
#     For bull-market only select returns above threshold.
#     For bear-market only select returns below -threshold.
#     Default is threshold=0.01.
# 
# You can use the functions lm(), summary(), and c().

calc_betas <- function(tseries, mar_ket, calc_bull_bear=FALSE, threshold=0.01) {
  # Calculate beta
  betav <- summary(lm(tseries ~ mar_ket))$coefficients[2, 1]
  if (calc_bull_bear) {
    # Calculate bull beta
    series_sub <- tseries[mar_ket > threshold]
    market_sub <- mar_ket[mar_ket > threshold]
    bull_beta <- summary(lm(series_sub ~ market_sub))$coefficients[2, 1]
    # Calculate bear beta
    series_sub <- tseries[mar_ket<(-threshold)]
    market_sub <- mar_ket[mar_ket<(-threshold)]
    bear_beta <- summary(lm(series_sub ~ market_sub))$coefficients[2, 1]
    c(beta=betav, bull_beta=bull_beta, bear_beta=bear_beta)
  } else
    betav  # Return single beta
}  # end calc_betas

# Call calc_betas() to verify that it works correctly.
# 
# You should get the following output:
calc_betas(tseries=returns$XLB, mar_ket=returns$VTI)
# [1] 1.069499
# 
calc_betas(tseries=returns$XLB, mar_ket=returns$VTI, calc_bull_bear=TRUE)
#      beta bull_beta bear_beta 
# 1.0694993 0.7825841 1.0732377


# 2. (20pts)
# Perform an sapply loop over the columns 
# of returns, excluding the first one "VTI", and 
# apply calc_betas() to them.
# Also pass into sapply() the arguments: 
# mar_ket=returns$VTI, calc_bull_bear=TRUE, threshold=0.005
# through the dots arguments of sapply().
# Call the output matrix etf_betas.
# You can also use the function t().

etf_betas <- sapply(retp[, -1], calc_betas, 
                    mar_ket=returns$VTI, calc_bull_bear=TRUE, threshold=0.005)

etf_betas <- t(etf_betas)

# You should get the following output:
round(etf_betas, 3)
#       beta bull_beta bear_beta
# VEU  1.098     1.144     1.143
# IEF -0.150    -0.099    -0.151
# VNQ  1.324     1.576     1.488
# DBC  0.446     0.354     0.556
# XLY  1.000     0.948     1.001
# XLP  0.562     0.560     0.539
# XLE  1.202     1.249     1.337
# XLF  1.460     1.603     1.579
# XLV  0.727     0.736     0.640
# XLI  1.013     0.931     0.977
# XLB  1.069     0.868     1.120
# XLK  0.944     0.950     0.870
# XLU  0.632     0.774     0.719
# VYM  0.888     0.868     0.907
# IVW  0.929     0.885     0.968
# IWB  0.978     0.969     0.997
# IWD  1.036     1.067     1.069
# IWF  0.936     0.909     0.964

# Calculate the names of the ETFs whose bear_beta
# is greater than its bull_beta.
# You can use the function names().

is_bear <- etf_betas[, "bear_beta"] > etf_betas[, "bull_beta"]
names(is_bear[is_bear])

# You should get the following output:
# [1] "DBC" "XLY" "XLE" "XLI" "XLB" "VYM" "IVW" "IWB" "IWD" "IWF"


# 3. (20pts)
# Perform an sapply loop over a vector of years, 
# and in each year calculate a vector of single ETF betas.
# Call the output matrix etf_betas.
# You can use the functions sapply() and t().

years <- c("2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

etf_betas <- sapply(years, function(ye_ar) {
  sapply(retp[ye_ar, -1], calc_betas, mar_ket=returns$VTI[ye_ar])
})  # end sapply
etf_betas <- t(etf_betas)

# You should get the following matrix:
round(head(etf_betas, 4), 3)
#        VEU    IEF   VNQ   DBC   XLY   XLP   XLE   XLF   XLV   XLI   XLB   XLK
# 2007 1.121 -0.208 1.377 0.136 1.003 0.537 1.274 1.374 0.655 0.941 1.295 0.877
# 2008 1.100 -0.117 1.498 0.339 0.971 0.546 1.266 1.499 0.690 0.889 0.913 0.929
# 2009 1.146 -0.105 2.028 0.630 1.084 0.490 1.165 2.094 0.531 1.129 1.115 0.874
# 2010 1.205 -0.202 1.269 0.749 1.058 0.566 1.173 1.266 0.692 1.146 1.217 0.951
#        XLU   VYM   IVW   IWB   IWD   IWF
# 2007 0.787 0.865 0.901 0.985 1.049 0.926
# 2008 0.790 0.878 0.900 0.974 1.046 0.911
# 2009 0.534 1.053 0.875 0.989 1.125 0.864
# 2010 0.675 0.802 1.015 0.977 1.054 0.977


# Calculate a vector of the names of the ETFs 
# with the highest beta in every year.
# You can use the functions apply(), names(x), 
# and which.max().

apply(etf_betas, MARGIN=1, FUN=function(x) {
  names(x)[which.max(x)]
})  # end apply

# You should get the following vector:
# 
#  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017  2018
# "VNQ" "XLF" "XLF" "VNQ" "XLF" "XLE" "XLF" "XLI" "XLE" "XLE" "XLF" "XLK"


######
# Determine if intervals with higher volatility have larger Hurst exponents
# Conclusion: they don't

# Split returns into intervals using end points
retp <- na.omit(rutils::etfenv$returns$VTI)
# endd <- rutils::calc_endpoints(retp, interval="months")
endd <- HighFreq::calc_endpoints(length=NROW(retp), step=20)
npts <- NROW(endd)

# Calculate the daily volatilities in the intervals between end points
volats <- sapply(2:npts, function(np) {
  retp <- retp[(endd[np-1]+1):endd[np], ]
  c(retp=sum(retp), stdev=sd(retp))
})  # end sapply
volats <- t(volats)

# Calculate the Hurst exponent
(log(sd(volats[, "retp"])) - log(sd(retp)))/log(20)

# Select the intervals with higher volatility
indic <- (volats[, "stdev"] > 0.015)
# indic <- c(FALSE, indic)
sum(indic)

# Select the intervals with higher volatility
retsh <- lapply(endd[(2:npts)[indic]], function(ep) {
  retp[(ep-19):ep, ]
})  # end lapply
retsh <- do.call(rbind, retsh)
sd(retsh)

# Select the intervals with lower volatility
retsl <- lapply(endd[(2:npts)[!indic]], function(ep) {
  retp[(ep-19):ep, ]
})  # end lapply
retsl <- do.call(rbind, retsl)
sd(retsl)

# Calculate the Hurst for the higher volatility intervals
hursth <- (log(sd(volats[indic, "retp"])) - log(sd(retsh)))/log(20)
# Calculate the Hurst for the lower volatility intervals
hurstl <- (log(sd(volats[!indic, "retp"])) - log(sd(retsl)))/log(20)



######
# Determine if intervals with higher volatility have larger Hurst exponents
# Conclusion: they don't
# Version using the rescaled range.

closep <- cumsum(retp)
# Calculate rescaled ranges
rrange <- sapply(2:npts, function(np) {
  indeks <- (endd[np-1]+1):endd[np]
  stdev <- sd(retp[indeks])
  c(rrange=diff(range(closep[indeks]))/stdev, stdev=stdev)
})  # end sapply
rrange <- t(rrange)
mean(rrange[, "rrange"])

# Calculate the Hurst exponent
log(mean(rrange[, "rrange"]))/log(20)

# Select the intervals with higher volatility
indic <- (rrange[, "stdev"] > 0.015)
# indic <- c(FALSE, indic)
sum(indic)

# Select the intervals with higher volatility
retsh <- lapply(endd[(2:npts)[indic]], function(ep) {
  retp[(ep-19):ep, ]
})  # end lapply
retsh <- do.call(rbind, retsh)
sd(retsh)

# Select the intervals with lower volatility
retsl <- lapply(endd[(2:npts)[!indic]], function(ep) {
  retp[(ep-19):ep, ]
})  # end lapply
retsl <- do.call(rbind, retsl)
sd(retsl)

# Calculate the Hurst for the higher volatility intervals
hursth <- log(mean(rrange[indic, "rrange"]))/log(20)
# Calculate the Hurst for the lower volatility intervals
hurstl <- log(mean(rrange[!indic, "rrange"]))/log(20)




######
# RcppParallel is slower for vectors of size < 100,000 and faster for size > 100,000

vec1 <- as.numeric(c(1:1000000))
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vec1),
  rcpp=vectorSum(vec1),
  parallel=parallelVectorSum(vec1),
  times=10))[, c(1, 4, 5)]



## Load Polygon data from CSV file

# Set option to display fractional seconds
options(digits.secs=6)

# Load a data table from CSV file
dtable <- data.table::fread("/Users/jerzy/Develop/data/spy_ticks.csv")
colnames(dtable) <- c("microseconds", "price", "size", "delim", "exchange", "date")
dtable[, date := as.POSIXct(microseconds/1e6, origin="1970-01-01", tz="America/New_York")]
dtable <- dtable[!is.na(dtable$date)]
# Remove prices with small size
dtable <- dtable[(dtable$size > 1)]

foo <- rutils::diffit(dtable$price)
range(foo)
tblv <- table(foo)
tblv <- table(dtable$price)
tail(tblv, 44)
bar <- which.max(foo)
dtable[(bar-2):(bar+2)]

dtable[dtable$price == 401.6]

spyticks <- xts::xts(dtable[, c("price", "size")], order.by=dtable$date)
dygraph(spyticks$price["2022-06-08"])



# Load a data table from CSV file
dtable <- data.table::fread("/Users/jerzy/Develop/data/spy_001.csv")
dtable <- dtable[order(unixtime)]
# class(dtable] dim(dtable)
# dtable
# colnames(dtable) <- c("microseconds", "price", "size", "delim", "exchange", "date")
# Coerce time from string to seconds POSIXct date-time
# dtable[, date := strptime(date, "%Y-%m-%d %H:%M:%OS")]
dtable[, date := as.POSIXct(unixtime/1e6, origin="1970-01-01", tz="America/New_York")]
# Delete columns
# dtable[, c("microseconds", "delim", "exchange") := NULL]
# Delete NA rows
dtable <- dtable[!is.na(dtable$date)]


## Remove price jumps
# Calculate centered Hampel filter over 3 data points
medianv <- roll::roll_median(dtable$price, width=3)
medianv[1:2] <- dtable$price[1:2]
medianv <- rutils::lagit(medianv, lagg=-1, pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(dtable$price), look_back=3, method="nonparametric")
madv <- rutils::lagit(madv, lagg=-1, pad_zeros=FALSE)
# Calculate Z-scores
zscores <- ifelse(madv > 0, (dtable$price - medianv)/madv, 0)
range(zscores); mad(zscores)
madv <- mad(zscores[abs(zscores)>0])
hist(zscores, breaks=2000, xlim=c(-1*madv, 1*madv))
sizev <- dtable$size[abs(zscores)>2]
hist(sizev, breaks=20000, xlim=c(0, 200))
dtable$price[abs(zscores)>2] <- medianv[abs(zscores)>2]

# Coerce from data.table to xts time series
spyticks <- xts::xts(dtable[, c("price", "size")], order.by=dtable$date)

# Save SPY prices to .RData file
save(spyticks, file="/Users/jerzy/Develop/data/spy_ticks.RData")

# https://www.highcharts.com/forum/viewtopic.php?t=43354
# highcharter:::Highcharts.setOptions({
#   time: {
#     timezone: 'America/New_York'
#   }
# }]

highcharter::hchart(spyticks$price)

# Plot SPY and z-scores
datav <- cbind(closep, zscores)
colnames(datav) <- c("SPY", "zscores")
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="SPY and z-scores") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")





## Find the most trending portfolio of ETFs

# Select all the ETF symbols except "VXX", "SVXY" "MTUM", "QUAL", "VLUE", and "USMV"
symbolv <- colnames(rutils::etfenv$returns)
# symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV"))]
symbolv <- symbolv[grep(glob2rx("X*"), symbolv)]
# Extract columns of rutils::etfenv$returns and overwrite NA values
retp <- rutils::etfenv$returns[, symbolv]
nstocks <- NCOL(retp)
# retp <- na.omit(retp)
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
datev <- zoo::index(retp)
# endd <- rutils::calc_endpoints(retp, interval="weeks")
retsl <- rutils::lagit(retp, lagg=1)

weightv <- colMeans(retp["/2010"]*retsl["/2010"])
weightv <- colMeans(sign(retp["/2010"])*sign(retsl["/2010"]))
sort(weightv)

objfun <- function(weightv, retsl=retsl, retp=retp) {
  # weightv <- weightv/sqrt(sum(weightv^2))
  weightv <- weightv/sum(weightv)
  retp <- retp["/2010"] %*% weightv
  retsl <- retsl["/2010"] %*% weightv
  # -sum(retp*sign(retsl))/sum(retp^2)
  -sum(retp*sign(retsl))
}  # end objfun


# Perform portfolio optimization using DEoptim
optimd <- DEoptim::DEoptim(fn=objfun,
                           retsl=retsl, retp=retp,
                           upper=rep(100, nstocks),
                           lower=rep(-100, nstocks),
                           control=list(trace=FALSE, itermax=1e3, parallelType=1, 
                                        packages=c("dygraphs", "TTR", "xts", "quantmod", "rutils", "HighFreq")))
weightv <- optimd$optim$bestmem/sum(abs(optimd$optim$bestmem))

# Perform portfolio optimization using optim
optimd <- optim(fn=objfun, 
                par=rep(1, nstocks),
                method="L-BFGS-B",
                upper=rep(100, nstocks),
                lower=rep(-100, nstocks))

# Portfolio weights
weightv <- optimd$par
names(weightv) <- symbolv
sort(weightv)

retsport <- retp["2011/"] %*% weightv
endd <- rutils::calc_endpoints(retp["2011/"], interval="weeks")
plot(cumsum(retsport)[endd], t="l")
retstrat <- (sign(rutils::lagit(bar, lagg=1))*bar)
plot(cumsum(retstrat)[endd], t="l")



# Normalize the prices so that they start at 1.
pricesn <- lapply(prices, function(x) x/as.numeric(x[1]))
pricesn <- rutils::do_call(cbind, pricesn)
head(pricesn[, 1:5])

arimav <- arima.sim(n=1e3, model=list(ar=c(0.2, 0.2)))


arimav <- arima.sim(n=1e3, model=list(ar=c(0.2, 0.2)))

foo <- (sqrt((acfv[1]+acfv[1])^2-4*acfv[1]^2)-(acfv[1]+acfv[2]))/2/acfv[1]

foo <- (acfv[2] - acfv[1]^2 - acfv[1]*acfv[3] + acfv[1]^2*acfv[2])/(1 - acfv[1]^2)

foo <- mean((arimav - acfv[1]*arimal)*arimal)

foo <- lm(arimav ~ arimal)
foo <- foo$residuals
cor(foo, rutils::lagit(foo, 2))
coef(foo)

cov1 <- function(x) {
  y <- (arimav + x*arimal)
  sum(y*rutils::lagit(y, pad_zeros=FALSE))
}

cov1(1)

xlimv <- seq(-2.8, -2.4, 0.1)
plot(x=xlimv, y=sapply(xlimv, cov1), t="l")
curve(cov1, from=(-5), to=5)


btmomweight <- function(retp,
                        objfun=function(retp) (prod(1+retp)/sd(retp)),
                        look_back=12, rfreq="months", bid_offer=0.001,
                        endd=rutils::calc_endpoints(retp, interval=rfreq), ...) {
  # Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(1:(npts-1), function(it) {
    # Select the look-back returns
    startp <- endd[max(1, it-look_back+1)]
    retslb <- retp[startp:endd[it], ]
    # Calculate weights proportional to performance
    perfstat <- sapply(retslb, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    weightv <- perfstat
    # Scale weights so sum of squares is equal to 1
    weightv <- (weightv - mean(weightv))
    weightv <- weightv/sqrt(sum(weightv^2))
    weightv[!is.finite(weightv)] <- 0
    # Calculate the out-of-sample portfolio returns
    retsos <- retp[(endd[it]+1):endd[it+1], ] %*% weightv
    retsos
  })  # end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  # end btmomweight


######

# Multiply matrix rows using R
matrixr <- t(vectorv*t(matrixv))
# Multiply the matrix in place
matrixp <- mult_mat(vectorv, matrixv, byrow=TRUE)
all.equal(matrixr, matrixp)
# Compare the speed of Rcpp with R code
library(microbenchmark)
summary(microbenchmark(
      Rcpp=HighFreq::mult_mat(vectorv, matrixv, byrow=TRUE),
    Rcode=t(vectorv*t(matrixv)),
      times=10))[, c(1, 4, 5)]  # end microbenchmark summary

### Rolling

getpos1 <- function(zscores, threshold, coeff, lagg) {
  nrows <- NROW(zscores)
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  indic <- (zscores > threshold)
  indic <- HighFreq::roll_count(indic)
  posv <- ifelse(indic >= lagg, coeff, posv)
  indic <- (zscores < (-threshold))
  indic <- HighFreq::roll_count(indic)
  posv <- ifelse(indic >= lagg, -coeff, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv
}  # end getpos1

getpos2 <- function(zscores, threshold, coeff, lagg) {
  nrows <- NROW(zscores)
  indic <- rep(NA_integer_, nrows)
  indic[1] <- 0
  indic[zscores > threshold] <- coeff
  indic[zscores < (-threshold)] <- (-coeff)
  indic <- zoo::na.locf(indic, na.rm=FALSE)
  indic_sum <- HighFreq::roll_vec(tseries=matrix(indic), look_back=lagg)
  indic_sum[1:lagg] <- 0
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv <- ifelse(indic_sum == lagg, 1, posv)
  posv <- ifelse(indic_sum == (-lagg), -1, posv)
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv[1:lagg] <- 0
  posv
}  # end getpos2

foo <- getpos1(zscores, threshold, coeff, lagg)
bar <- getpos2(zscores, threshold, coeff, lagg)
all.equal(foo, bar)

foo <- cbind(foo, bar)
head(foo, 12)
tail(foo, 12)


###############
### Cointegration

symbolstock <- "XLE"
symboletf <- "XLB"
ohlc <- get(symbolstock, rutils::etfenv)
closep <- log(quantmod::Cl(ohlc))
ohlc <- get(symboletf, rutils::etfenv)
closetf <- log(quantmod::Cl(ohlc))
prices <- na.omit(cbind(closep, closetf))
colnames(prices) <- c(symbolstock, symboletf)
closep <- prices[, 1]
closetf <- prices[, 2]

# Calculate regression coefficients of XLB ~ XLE
betav <- drop(cov(closep, closetf)/var(closetf))
alpha <- drop(mean(closep) - betav*mean(closetf))


betas <- (1:40)/10

foo <- sapply(betas, function(betav) {
  alpha <- drop(mean(closep) - betav*mean(closetf))
  residuals <- (closep - alpha - betav*closetf)
  colnames(residuals) <- paste0(symbolstock, " vs ", symboletf)
  adftest <- tseries::adf.test(residuals, k=3)
  adftest$p.value  
})

dev.new(width=6, height=4, noRStudioGD=TRUE)
plot(x=betas, y=foo, t="l")

dygraphs::dygraph(residuals)


##########
zscores <- HighFreq::roll_zscores(respv=closep, predv=predv, look_back=look_back)

look_backs <- 5*(1:10)
# zscores <- sapply(look_backs, HighFreq::roll_zscores, respv=closep, predv=predv, startp = 0L, endd = 0L, step = 1L, stub = 0L)
zscores <- sapply(look_backs, function(look_back) {
  cat("look_back =", look_back, "\n")
  zscores <- HighFreq::roll_zscores(respv=closep, predv=predv, look_back=look_back)  
  zscores[1:look_back] <- 0
  zscores
})
colnames(zscores) <- paste0("zscore", look_backs)
foo <- apply(zscores, 2, sd)
foo/sqrt(look_backs)

# Standard deviation of square returns is proxy for kurtosis and stationarity


# Calculate trailing z-scores of SVXY
predv <- cbind(sqrt(varv), vxx, vti_close)
respv <- svxy
rollzscores <- drop(HighFreq::roll_zscores(respv=respv, predv=predv, look_back=look_back))
rollzscores[is.infinite(rollzscores)] <- 0

rollreg <- HighFreq::roll_reg(respv=respv, predv=predv, intercept=TRUE, look_back=look_back)
rollregscores <- rollreg[, NCOL(rollreg), drop=TRUE]
all.equal(rollregscores, rollzscores)

library(microbenchmark)
# HighFreq::roll_reg() is faster than roll_zscores()
summary(microbenchmark(
  roll_zscores=HighFreq::roll_zscores(respv=respv, predv=predv, look_back=look_back),
  roll_reg=HighFreq::roll_reg(respv=respv, predv=predv, intercept=TRUE, look_back=look_back),
  times=10))[, c(1, 4, 5)]

runreg <- HighFreq::run_reg(respv=respv, predv=predv, lambda=lambda, method="scale")
runzscores <- HighFreq::run_zscores(respv=respv, predv=predv, lambda=lambda, demean=FALSE)
# runzscores <- runreg[, 1, drop=TRUE]

runreg <- rutils::lagit(rutils::diffit(cbind(respv, predv)))

runreg[is.infinite(runreg)] <- 0
runreg[abs(runreg) > 1e8] <- 0
runreg[1:11, ] <- 0


# Portfolio objective function
objfun <- function(x) {
  forecasts <- sign(runreg %*% x)
  pnls <- (retp*forecasts)["/2017"]
  -mean(pnls)/sd(pnls[pnls < 0])
}  # end objfun

# 2-dim case
optimd <- optim(fn=objfun, 
                par=rep(1, NCOL(runreg)),
                method="L-BFGS-B",
                upper=rep(100, NCOL(runreg)),
                lower=rep(-100, NCOL(runreg)))

# Portfolio weights - static dollars not shares
weightv <- optimd$par

# Calculate out-of-sample pnls
forecasts <- runreg %*% weightv
pnls <- (retp*forecasts)
pnls <- xts::xts(cumsum(pnls), zoo::index(retp))
dygraphs::dygraph(pnls, main="VTI Stategy Using RunReg")
forecasts <- xts::xts(forecasts, zoo::index(retp))
dygraphs::dygraph(forecasts, main="VTI RunReg Forecasts")


###############
### Benchmark of HighFreq::run_reg() against R code

library(HighFreq)
# Load ETF returns
retp <- na.omit(rutils::etfenv$returns[, c("XLF", "VTI", "IEF")])
# Response equals XLF returns
respv <- retp[, 1]
# Predictor matrix equals VTI and IEF returns
predv <- retp[, -1]
# Calculate the rolling regressions
lambda <- 0.9
regs <- HighFreq::run_reg(respv=respv, predv=predv, lambda=lambda)
# Plot the rolling alphas
datav <- cbind(cumsum(respv), regs[, 2])
colnames(datav) <- c("XLF", "alphas")
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="Alphas of XLF Versus VTI and IEF") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

respv <- zoo::coredata(respv)
predv <- zoo::coredata(predv)
nrows <- NROW(predv)
ncols <- NCOL(predv)
means_resp <- matrix(nrow=nrows)
means_pred <- matrix(nrow=nrows, ncol=ncols)
vars <- predictor^2
covars <- matrix(rep(0, nrows*ncols), nrow=nrows, ncol=ncols)
betas <- matrix(rep(0, nrows*ncols), nrow=nrows, ncol=ncols)
alphas <- matrix(rep(0, nrows), nrow=nrows)
resids <- matrix(rep(0, nrows), nrow=nrows)
# varz <- matrix(rep(1, nrows), nrow=nrows)
# meanz <- matrix(rep(0, nrows), nrow=nrows)
lambda1 <- 1-lambda

# Perform loop over rows
means_resp[1, ] <- response[1, ]
means_pred[1, ] <- predictor[1, ]
for (it in 2:nrows) {
  # Calculate the mean as the weighted sum
  means_resp[it, ] <- lambda1*response[it, ] + lambda*means_resp[it-1, ]
  means_pred[it, ] <- lambda1*predictor[it, ] + lambda*means_pred[it-1, ]
  vars[it, ] <- lambda1*(predictor[it, ]-means_pred[it, ])^2 + lambda*vars[it-1, ]
  covars[it, ] <- lambda1*((respv[it, ]-means_resp[it, ])*(predictor[it, ]-means_pred[it, ])) + lambda*covars[it-1, ]
  betas[it, ] <- lambda1*covars[it, ]/vars[it, ] + lambda*betas[it-1, ]
  alphas[it, ] <- lambda1*(means_resp[it, drop=FALSE] - betas[it, ] %*% means_pred[it, ]) + lambda*alphas[it-1, ]
  # Calculate the z-score as the weighted sum of products of returns.
  resids[it, ] <- lambda1*(respv[it, drop=FALSE] - betas[it, ] %*% predictor[it, ]) + lambda*resids[it-1, ]
  # Calculate the mean and variance of the z-scores.
  # meanz[it, ] <- lambda1*resids[it, ] + lambda*meanz[it-1, ]
  # varz[it, ] <- lambda1*(resids[it, ] - resids[it-1, ])^2 + lambda*varz[it-1, ]
}  # end for

# regdatar <- cbind(alphas, betas, vars, (resids - meanz)/sqrt(varz))
regdatar <- cbind(resids, alphas, betas)
regdata <- HighFreq::run_reg(respv=respv, predv=predv, lambda=lambda)
all.equal(regdatar, regdata, check.attributes=FALSE)



###############
### Portfolio optimization of VTI, VXX, and SVXY

# Select ETFs
symbolv <- c("VTI", "VXX", "SVXY")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
prices <- na.omit(rutils::etfenv$prices[, symbolv])

# dygraph plot of VXX prices versus SVXY
dygraphs::dygraph(prices["2018-01-01/", 2:3], main="Returns of VXX and SVXY") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# dygraph plot of VXX returns versus SVXY
dygraphs::dygraph(cumsum(cbind(retp["2018-01-01/", 2], -2*retp["2018-01-01/", 3])), 
                  main="Returns of VXX and SVXY") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# In-sample returns
insample <- rbind(retp["/2018-02-01"], retp["2018-02-07/"])
insample <- retp["2018-03-01/"]

# Portfolio objective function
objfun <- function(x) {
  weightv <- c(1, x[1], x[2])
  pnls <- (insample %*% weightv)
  -mean(pnls)/sd(pnls[pnls < 0])
}  # end objfun

## Calculate in-sample weights
# 1-dim case
optimd <- optimize(objfun, c(-2, 1))
weightv <- c(-1, optimd$minimum)

# 2-dim case
optimd <- optim(fn=objfun, 
                par=c(-1, -1),
                method="L-BFGS-B",
                upper=c(2, 2),
                lower=c(-5, -5))

# Portfolio weights - static dollars not shares
weightv <- c(1, optimd$par)

weightv <- c(1, -2, -2)

weightv <- c(1, -2.1, -4.6)

# Calculate out-of-sample pnls
pnls <- (retp %*% weightv)
pnls <- xts::xts(cumsum(pnls), zoo::index(retp))
dygraphs::dygraph(pnls, main="Static Portfolio of ETFs")


## VTI AR strategy with VXX and SVXY as predictors

# Define response as the rolling sum of returns
# respv <- retp[, "VTI"]
numagg <- 5
respv <- roll::roll_mean(retp[, "VTI"], width=numagg, min_obs=1)
respv <- rutils::lagit(respv, lagg=(-numagg+1))

# Define predictor
# predv <- rutils::lagit(retp)
order_max <- 2
predv <- roll::roll_mean(retp, width=numagg, min_obs=1)
predv <- lapply(1:order_max, rutils::lagit, input=predv)
predv <- do.call(cbind, predv)
# colnames(predv) <- paste0("pred_", 1:NCOL(predv))
nrows <- NROW(predv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv)[1] <- "intercept"
model <- lm(respv ~ predictor - 1)
model_sum <- summary(model)


# In-Sample
eigen_max <- 6
# invmat <- MASS::ginv(predv)
invmat <- HighFreq::calc_inv(predictor, eigen_max=eigen_max)
coeff <- drop(invmat %*% response)
forecasts <- (predictor %*% coeff)
pnls <- sign(forecasts)*retp[, "VTI"]
wealth <- cbind(retp[, "VTI"], pnls)
colnames(wealth) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealth, function(x) mean(x)/sd(x[x<0]))
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="VIX Strategy In-Sample") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)


# Out-of-Sample
insample <- (datev < as.Date("2018-01-01"))
outsample <- (datev >= as.Date("2018-01-01"))
# invmat <- MASS::ginv(predictor[insample, ])
eigen_max <- 4
invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
coeff <- drop(invmat %*% response[insample, ])
forecasts <- (predictor[outsample, ] %*% coeff)
pnls <- sign(forecasts)*retp[outsample, "VTI"]
wealth <- cbind(retp[outsample, "VTI"], pnls)
colnames(wealth) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealth, function(x) mean(x)/sd(x[x<0]))
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="VIX Strategy Out-of-Sample") %>%
dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
dyLegend(show="always", width=500)


eigen_maxs <- 2:7
pnls <- lapply(eigen_maxs, function(eigen_max) {
cat("eigen_max =", eigen_max, "\n")
invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
coeff <- drop(invmat %*% response[insample, ])
forecasts <- (predictor[outsample, ] %*% coeff)
sign(forecasts)*response[outsample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_max)
colnames(pnls) <- paste0("eigen", eigen_maxs)
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
dyOptions(colors=colors, strokeWidth=1) %>%
dyLegend(show="always", width=500)



###############
### Tune parameters of AR strategies

eigen_maxs <- 2:7
pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max =", eigen_max, "\n")
  invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
  coeff <- drop(invmat %*% response[insample, ])
  forecasts <- (predictor[outsample, ] %*% coeff)
  sign(forecasts)*response[outsample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  pnls <- lapply(3:(NROW(years)-1), function(i) {
    insample <- (datev > years[i-1]) & (datev < years[i])
    outsample <- (datev >= years[i]) & (datev < years[i+1])
    invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
    coeff <- drop(invmat %*% response[insample, ])
    forecasts <- (predictor[outsample, ] %*% coeff)
    sign(forecasts)*retp[outsample, ]
  })  # end lapply
  do.call(rbind, pnls)
})  # end lapply


pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  pnls <- lapply(12:(NROW(months)-1), function(i) {
    insample <- (datev > months[i-3]) & (datev < months[i])
    outsample <- (datev > months[i]) & (datev < months[i+1])
    invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
    coeff <- drop(invmat %*% response[insample, ])
    forecasts <- (predictor[outsample, ] %*% coeff)
    sign(forecasts)*retp[outsample, ]
  })  # end lapply
  do.call(rbind, pnls)
})  # end lapply

pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    insample <- (datev > weeks[i-3]) & (datev < weeks[i])
    outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
    invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
    coeff <- drop(invmat %*% response[insample, ])
    forecasts <- (predictor[outsample, ] %*% coeff)
    sign(forecasts)*retp[outsample, ]
  })  # end lapply
  do.call(rbind, pnls)
})  # end lapply

pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Loop over aggregations - aggregations don't improve the forecasts!
numaggs <- 2:11
pnls <- lapply(numaggs, function(numagg) {
  cat("numagg=", numagg, "\n")
  predv <- roll::roll_mean(ratestdeviff, width=numagg, min_obs=1)
  predv <- cbind(rep(1, NROW(predv)), predv)
  predv <- rutils::lagit(predv)
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    # Define in-sample and out-of-sample intervals
    insample <- (datev > weeks[i-look_back]) & (datev < weeks[i])
    outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
    # Calculate forecasts and pnls out-of-sample
    invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
    coeff <- drop(invmat %*% response[insample, ])
    forecasts <- (predictor[outsample, ] %*% coeff)
    sign(forecasts)*retp[outsample, ]
  })  # end lapply
  pnls <- do.call(rbind, pnls)
})  # end sapply

pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", numaggs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


###############

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1)*nrows
# Calculate forecasts as function of eigen_max
forecasts <- lapply(2:5, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  # Calculate fitted coefficients
  # invmat <- MASS::ginv(predictor[insample, 1:eigen_max])
  invmat <- HighFreq::calc_inv(predictor[insample,], eigen_max=eigen_max)
  coeff <- drop(invmat %*% response[insample])
  # Calculate out-of-sample forecasts of vtis
  drop(predictor[outsample, ] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:5)


###############
## Multifactor AR rates

load(file="/Users/jerzy/Develop/lecture_slides/data/ratestdevata.RData")
rates <- do.call(cbind, as.list(rates_env))
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(closep) <- "VTI"
nrows <- NROW(closep)
datev <- zoo::index(closep)
rates <- na.omit(rates[datev])
closep <- closep[zoo::index(rates)]
datev <- zoo::index(closep)
retp <- rutils::diffit(closep)
ratestdeviff <- rutils::diffit(log(rates))

order_max <- 5
predv <- sapply(1:order_max, rutils::lagit, input=as.numeric(retp))
colnames(predv) <- paste0("pred_", 1:NCOL(predv))
predv <- cbind(predictor, rutils::lagit(ratestdeviff))
predv <- cbind(rep(1, NROW(predv)), predv)
colnames(predv)[1] <- "intercept"
respv <- returns

# Calculate forecasts as function of eigen_max
forecasts <- lapply(2:5, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  invmat <- HighFreq::calc_inv(predictor, eigen_max=eigen_max)
  coeff <- drop(invmat %*% response)
  drop(predictor %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:5)

pnls <- lapply(forecasts, function(x) {
  sign(x)*returns
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- names(forecasts)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 9
eigen_max <- 2
# Or
look_back <- 8
eigen_max <- 4
pnls <- lapply(51:(NROW(weeks)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  insample <- (datev > weeks[i-look_back]) & (datev < weeks[i])
  outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
  # Calculate forecasts and pnls out-of-sample
  invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=eigen_max)
  coeff <- drop(invmat %*% response[insample, ])
  forecasts <- (predictor[outsample, ] %*% coeff)
  sign(forecasts)*retp[outsample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)

vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)


###############


foo <- sapply(2:11, function(x) {
  pnls <- lapply(12:(NROW(months)-1), function(i) {
    # Define in-sample and out-of-sample intervals
    insample <- (datev > months[i-x]) & (datev < months[i])
    outsample <- (datev > months[i]) & (datev < months[i+1])
    # Calculate forecasts and pnls out-of-sample
    invmat <- MASS::ginv(predictor[insample, ])
    # invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=3)
    coeff <- drop(invmat %*% response[insample, ])
    forecasts <- (predictor[outsample, ] %*% coeff)
    sign(forecasts)*retp[outsample, ]
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  mean(pnls)/sd(pnls)
})
(2:11)[which.max(foo)]


foo <- sapply(2:11, function(x) {
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    # Define in-sample and out-of-sample intervals
    insample <- (datev > weeks[i-x]) & (datev < weeks[i])
    outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
    # Calculate forecasts and pnls out-of-sample
    invmat <- MASS::ginv(predictor[insample, ])
    # invmat <- HighFreq::calc_inv(predictor[insample, ], eigen_max=3)
    coeff <- drop(invmat %*% response[insample, ])
    forecasts <- (predictor[outsample, ] %*% coeff)
    sign(forecasts)*retp[outsample, ]
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  mean(pnls)/sd(pnls)
})
(2:11)[which.max(foo)]


###############

# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(retp, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  varv <- roll::roll_var(retp, width=look_back, min_obs=1)
  varv[1, ] <- 1
  varv[varv <= 0] <- 1
  # Calculate rolling Sharpe
  past <- roll::roll_mean(retp, width=look_back, min_obs=1)
  weightv <- past/sqrt(varv)
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv <- rutils::lagit(weightv)
  weightv <- roll::roll_mean(weightv, width=hold_period, min_obs=1)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weights*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  (pnls - costs)
}  # end btmomdaily

look_backs <- seq(50, 150, by=10)
pnls <- sapply(look_backs, btmomdaily, hold_period=9,
                  retp=retp, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, index(retp))

hold_periods <- seq(9, 21, by=2)
pnls <- sapply(hold_periods, btmomdaily, look_back=120,
                  retp=retp, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", hold_periods)
pnls <- xts::xts(pnls, index(retp))

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


###############
### Interest rate butterfly strategy
# Doesn't work well

load(file="/Users/jerzy/Develop/lecture_slides/data/ratestdevata.RData")
rates <- mget(ls(rates_env), envir=rates_env)
rates <- rutils::do_call(cbind, rates)
rates <- zoo::na.locf(rates, na.rm=FALSE)
rates <- zoo::na.locf(rates, fromLast=TRUE)

# Sort the columns of rates according bond maturity
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]

retp <- rutils::diffit(log(rates))

covmat <- cov(retp["2000/"])
cormat <- cor(retp["2000/"])
eigend <- eigen(cormat)
eigend$vectors

bfly <- rates["2000/"] %*% eigend$vectors[, 3]
datev <- zoo::index(rates["2000/"])
bfly <- xts::xts(bfly, datev)
dygraphs::dygraph(bfly, main="IR Butterfly") %>% 
  dyOptions(colors="blue", strokeWidth=2)

# ADF test


# Calculate the volatility
look_back <- 21
retp <- rutils::diffit(bfly)
volv <- roll::roll_sd(retp, width=look_back, min_obs=1)
volv[1, ] <- 1
# Calculate the z-scores of prices
meanv <- roll::roll_mean(bfly, width=look_back, min_obs=1)
zscores <- ifelse(volv > 0, (bfly - meanv)/volv, 0)
zscores <- zscores/sqrt(look_back)
sd(zscores)
hist(zscores)

# Calculate positions
posv <- rep(NA_integer_, NROW(bfly))
posv[zscores < (-1)] <- 1
posv[zscores > 1] <- (-1)
# Carry forward and backward non-NA posv
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- zoo::na.locf(posv, fromLast=TRUE)
posv <- rutils::lagit(posv)
pnls <- retp*posv

# Plot dygraph of in-sample VTI strategy
wealth <- cbind(retp, pnls)
colnames(wealth) <- c("BFLY", "Strategy")
dygraphs::dygraph(cumsum(wealth), main="Butterfly Strategy In-sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)



###############
### Test for app_zscore_returns_strat.R
# Find optimal parameters for SPY minutes data

load(file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")
ohlc <- ohlc["T09:00:00/T16:30:00"]
retp <- rutils::diffit(log(Cl(ohlc)))
nrows <- NROW(retp)

lambda <- 0.15
lagg <- 1
threshold <- 1.0
long_back <- 100

refvar <- rep(1, NROW(retp))
tseries <- cbind(retp, refvar)

# Run the model
calc_sharpe <- function(lambda, threshold) {
  zscores <- HighFreq::run_zscore(tseries, lambda=lambda)
  zscores <- zscores[, 1, drop=FALSE]
  zscores <- HighFreq::roll_scale(zscores, look_back=long_back, use_median=TRUE)
  zscores[is.na(zscores) | is.infinite(zscores)] <- 0
  zscores <- HighFreq::lagit(zscores, pad_zeros=TRUE)
  indic <- rep(0, nrows)
  indic <- ifelse(zscores > threshold, -1, indic)
  indic <- ifelse(zscores < (-threshold), 1, indic)
  indic_sum <- HighFreq::roll_vec(tseries=indic, look_back=lagg)
  indic_sum[1:lagg] <- 0
  positions <- rep(NA_integer_, nrows)
  positions[1] <- 0
  positions <- ifelse(indic_sum >= lagg, 1, positions)
  positions <- ifelse(indic_sum <= (-lagg), -1, positions)
  positions <- zoo::na.locf(positions, na.rm=FALSE)
  positions <- rutils::lagit(positions, lagg=1)
  pnls <- positions*returns
  mean(pnls)/sd(pnls[pnls<0])
}  # end calc_sharpe

calc_sharpe(0.3)


# Calculate heatmaps 
lambdas <- seq(0.1, 0.3, 0.01)
threshold <- 1.0
sharpes <- sapply(lambdas, calc_sharpe, threshold=threshold)
plot(lambdas, sharpes)

thresholds <- seq(0.5, 4, 0.2)
lambda <- 0.25
sharpes <- sapply(thresholds, calc_sharpe, lambda=lambda)
plot(thresholds, sharpes)



###############
### Stock Forecasting Using Interest Rate Data

# Load FRED data from csv file
# datav <- data.table::fread(file="C:/Develop/predictive/data/FRED_data.csv", stringsAsFactors=FALSE)

datav <- read.csv(file="C:/Develop/predictive/data/FRED_data.csv")
colnamev <- colnames(datav)[-1]
sapply(datav, class)
datav <- lapply(datav[, -1], as.numeric)
datav <- rutils::do_call(cbind, datav)
apply(datav, 2, class)

num_nona <- apply(datav, 2, function(x) sum(!is.na(x)))
colnamev <- colnamev[num_nona > 0]
datav <- lapply(1:NCOL(datav), function(x) {
  if (sum(!is.na(datav[, x])) > 0)
    datav[, x]
  else
    NULL
})  # end lapply
datav <- rutils::do_call(cbind, datav)
head(datav)
tail(datav)

diff_data <- rutils::diffit(datav, pad_zeros=FALSE)
bar <- var(diff_data, na.rm=TRUE)


corr_el <- sapply(2:NCOL(diff_data), function(x) {
  di_ff <- na.omit(diff_data[, c(1, x)])
  cor(di_ff[, 1], rutils::lagit(di_ff[, 2], pad_zeros=FALSE))
})  # end lapply
names(corr_el) <- colnamev[-1]
sort(corr_el)



###############
sum(!is.na(datav[, 1]))

which(!is.null(datav[, BAMLH0A0HYM2SY]))



datav <- lapply(datav[, -1], rutils::diffit)
datav <- rutils::do_call(cbind, datav)



retp <- lapply(datav[, -1], rutils::diffit)
retp <- rutils::do_call(cbind, returns)
colnames(retp) <- c("sentiment", "SPY")
cor(rutils::lagit(retp[, 1]), retp[, 2])


datav <- datav[, c("date", "sentiment", "close")]
colnames(datav) <- c("date", "sentiment", "SPY")
datav <- xts::xts(datav[, 2:3], as.Date.IDate(datav[, date]))



############### - copied to slides
### Simulate an AR strategy for VTI using IR yield curve
# Comment: PCA of yield curve have some forecasting power for VTI

wealth <- cbind(foo1, foo2, foo3)
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealth))
colnames(wealth) <- c("NoReg", "Reg=3", "Reg=2")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Yield Curve Strategy In-sample") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>% dyLegend(show="always", width=500)

# Load packages
library(rutils)

# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/ratestdevata.RData")
# Combine rates into single xts series
rates <- do.call(cbind, as.list(rates_env))
# Sort the columns of namesv according bond maturity
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]
tail(rates)
# Align rates dates with VTI prices
closep <- quantmod::Cl(rutils::etfenv$VTI)
nrows <- NROW(closep)
datev <- zoo::index(closep)
rates <- na.omit(rates[datev])
closep <- closep[zoo::index(rates)]
datev <- zoo::index(closep)
all.equal(datev, zoo::index(rates))
retp <- rutils::diffit(log(closep))
# Calculate VTI returns

# Calculate change in rates
ratestdeviff <- rutils::diffit(log(rates))

# Calculate eigen decomposition of correlation/covariance matrix
# eigend <- eigen(cov(ratestdeviff))
eigend <- eigen(cor(ratestdeviff))
rates_pca <- ratestdeviff %*% eigend$vectors

foo <- apply(rates_pca, 2, function(x) {
  pacfv <- pacf(x, lag=10, plot=FALSE)
  sum(pacfv$acf)
})  # end sapply
barplot(foo, main="PACF of Interest Rate PCs")

# Plot principal components of rates
# rates_pca <- rates %*% eigend$vectors[, 1:3]
# rates_pca <- apply(rates_pca, 2, cumsum)

datav <- cbind(retp, rates_pca[, 1:3])
colnames(datav) <- c("VTI", "FirstPC", "Steepener", "Butterfly")
colnamev <- colnames(datav)[c(1, 3)]
dygraphs::dygraph(cumsum(datav[, c(1, 3)]), main=paste(colnames(datav)[1], "and IR", colnames(datav)[3])) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")


# Pure in-sample with aggregations
numagg <- 40
# Define response as the rolling sum of returns
# respv <- returns
respv <- roll::roll_mean(retp, width=numagg, min_obs=1)
respv <- rutils::lagit(respv, lagg=(-numagg+1))
# Calculate predictor as the rolling rates PCAs
# predv <- rutils::lagit(rates_pca[, 1:2])
predv <- roll::roll_mean(rates_pca, width=numagg, min_obs=1)
predv <- rutils::lagit(predv)
# Calculate inverse of predictor
invmat <- MASS::ginv(predv)
coeff <- drop(invmat %*% response)
# coeff <- coeff/sqrt(sum(coeff^2))
forecasts <- (predictor %*% coeff)
pnls <- forecasts*response
# pnls <- pnls*sd(retp)/sd(pnls)
# Plot dygraph of in-sample VTI strategy
wealth <- cbind(retp, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Autoregressive Strategy Using Yield Curve") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Find best numagg
numaggs <- 5:50
foo <- sapply(numaggs, function(numagg) {
  respv <- roll::roll_mean(retp, width=numagg, min_obs=1)
  respv <- rutils::lagit(respv, lagg=(-numagg+1))
  predv <- roll::roll_mean(rates_pca, width=numagg, min_obs=1)
  predv <- rutils::lagit(predv)
  invmat <- MASS::ginv(predv)
  coeff <- drop(invmat %*% response)
  forecasts <- (predictor %*% coeff)
  pnls <- forecasts*returns
  sum(pnls)/sd(pnls)
})  # end sapply
numaggs[which.max(foo)]
plot(numaggs, foo, t="l", col="blue", lwd=2)


# Fit in-sample logistic regression
glmod <- glm((sign(respv)+1)/2 ~ predictor, family=binomial(logit))
summary(glmod)
coeff <- glmod$coefficients
predictv <- drop(design[, -1] %*% coeff)
ordern <- order(predictv)
# Calculate in-sample forecasts from logistic regression model
forecasts <- 1/(1+exp(-predictv))


# Define in-sample and out-of-sample intervals
insample <- (datev < as.Date("2020-01-01"))
outsample <- (datev > as.Date("2020-01-01"))
# Or without 2008 and 2009
insample <- ((datev < as.Date("2008-01-01")) | ((datev > as.Date("2010-01-01")) & (datev < as.Date("2012-01-01"))))
outsample <- (datev > as.Date("2012-01-01"))


# Calculate in-sample fitted coefficients
invmat <- MASS::ginv(predictor[insample, ])
coeff <- drop(invmat %*% response[insample, ])
names(coeff) <- colnames(predv)


# Calculate regularized inverse using RcppArmadillo
eigen_max <- 3
invmat <- HighFreq::calc_inv(predictor[insample], eigen_max=eigen_max)
coeff <- drop(invmat %*% response[insample])
names(coeff) <- colnames(predv)

## Define predictor as a rolling mean
numagg <- 10
predv <- roll::roll_mean(rates_pca, width=numagg, min_obs=1)
predv <- rutils::lagit(predv)
# Shift the response forward out-of-sample
respv <- roll::roll_mean(retp, width=numagg, min_obs=1)
respv <- rutils::lagit(respv, lagg=(-numagg+1))
# Calculate in-sample fitted coefficients
invmat <- MASS::ginv(predictor[insample])
coeff <- drop(invmat %*% response[insample])
names(coeff) <- colnames(predv)

# Calculate in-sample PnLs
forecasts <- predictor[insample] %*% coeff
pnls <- cumsum(forecasts*retp[insample])
pnls <- xts::xts(pnls, datev[insample])
colnames(pnls) <- "VTI"

# Calculate out-of-sample PnLs
forecasts <- (predictor[outsample, ] %*% coeff)
pnls <- forecasts*response[outsample, ]
# Plot dygraph of out-of-sample VTI strategy
wealth <- cbind(retp[outsample, ], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Autoregressive Strategy Using Yield Curve") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Trade the strategy
respv <- pnls
predv <- lapply(1:3, function(n) rutils::lagit(pnls, n))
predv <- do.call(cbind, predv)
# Calculate inverse of predictor
invmat <- MASS::ginv(predv)
coeff <- drop(invmat %*% response)
# coeff <- coeff/sqrt(sum(coeff^2))
forecasts <- rutils::lagit(predictor %*% coeff)
pnls <- forecasts*response



##### some optim stuff

u_til <- function(coeff) {
  forecasts <- predictor %*% coeff
  -sum(sign(forecasts)*returns)
}  # end u_til

# Optimize with respect to vector argument
optimd <- optim(fn=u_til, 
                par=rnorm(6),
                method="L-BFGS-B",
                upper=rep(10, 6),
                lower=rep(-10, 6))
# Weights
coeff <- optimd$par


# Calculate eigen decomposition of covariance matrix
eigend <- eigen(cov(ratestdeviff))
pcad <- eigend$vectors[, 1:2]
pcad %*% c(1, -1)

u_til <- function(coeffpc) {
  coeff <- pcad %*% coeffpc
  forecasts <- predictor %*% coeff
  -sum(sign(forecasts)*returns)
}  # end u_til

# Optimize with respect to vector argument
optimd <- optim(fn=u_til, 
                par=rnorm(2),
                method="L-BFGS-B",
                upper=rep(10, 2),
                lower=rep(-10, 2))
coeffpc <- optimd$par
coeff <- pcad %*% coeffpc
forecasts <- drop(predictor %*% coeff)
pnls <- cumsum(sign(forecasts)*returns)
pnls <- xts::xts(pnls, datev)
colnames(pnls) <- "VTI"
dygraphs::dygraph(pnls, main="Autoregressive Strategy Using Yield Curve") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(width=500)


## Define predictor as a rolling mean
numagg <- 5
predv <- roll::roll_mean(predictor, width=numagg, min_obs=1)
# Shift the response forward out-of-sample
respv <- roll::roll_mean(retp, width=numagg, min_obs=1)
respv <- rutils::lagit(respv, lagg=(-numagg+1))
# Calculate in-sample fitted coefficients
invmat <- MASS::ginv(predictor[insample])
coeff <- drop(invmat %*% retp[insample])
names(coeff) <- colnames(predv)

foo <- sapply(1*(1:20), function(numagg) {
  predv <- roll::roll_mean(predictor, width=numagg, min_obs=1)
  respv <- roll::roll_mean(retp, width=numagg, min_obs=1)
  respv <- rutils::lagit(respv, lagg=(-numagg+1))
  invmat <- MASS::ginv(predv)
  coeff <- drop(invmat %*% response)
  forecasts <- predictor %*% coeff
  sum(sign(forecasts)*returns)
})  # end sapply




###############
### Backtest an AR strategy with design as z-scores.
# Comment: The z-scores have low predictive power.

# Load packages
library(rutils)

# Calculate SVXY and VXX prices
svxy <- log(rutils::etfenv$SVXY)
svxy_close <- quantmod::Cl(svxy)
nrows <- NROW(svxy)
datev <- zoo::index(svxy)
vxx <- log(rutils::etfenv$VXX[datev])
vxx_close <- quantmod::Cl(vxx)

look_back <- 21

# Extract log OHLC prices
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)[datev]
closep <- log(quantmod::Cl(ohlc))
volumes <- quantmod::Vo(ohlc)
retp <- rutils::diffit(closep)


# Define response as a rolling mean
numagg <- 5
respv <- roll::roll_mean(retp, width=numagg, min_obs=1)
# Shift the response forward out-of-sample
respv <- rutils::lagit(respv, lagg=(-numagg+1))

# Calculate SVXY z-scores
indeks <- matrix(1:nrows, nc=1)
svxy_scores <- drop(HighFreq::roll_zscores(respv=svxy_close, design=indeks, look_back=look_back))
svxy_scores[1:look_back] <- 0
svxy_scores[is.infinite(svxy_scores)] <- 0
svxy_scores[is.na(svxy_scores)] <- 0
svxy_scores <- svxy_scores/sqrt(look_back)
# roll_svxy <- roll::roll_mean(svxy_close, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var_ohlc(svxy, look_back=look_back, scale=FALSE))
# svxy_scores <- (svxy_close - roll_svxy)/var_rolling

# Calculate VXX z-scores
vxx_scores <- drop(HighFreq::roll_zscores(respv=vxx_close, design=indeks, look_back=look_back))
vxx_scores[1:look_back] <- 0
vxx_scores[is.infinite(vxx_scores)] <- 0
vxx_scores[is.na(vxx_scores)] <- 0
vxx_scores <- vxx_scores/sqrt(look_back)
# roll_vxx <- roll::roll_mean(vxx_close, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var_ohlc(vxx, look_back=look_back, scale=FALSE))
# vxx_scores <- (vxx_close - roll_vxx)/var_rolling

# Calculate price z-scores
pricescores <- drop(HighFreq::roll_zscores(respv=closep, design=indeks, look_back=look_back))
pricescores[1:look_back] <- 0
pricescores[is.infinite(pricescores)] <- 0
pricescores[is.na(pricescores)] <- 0
pricescores <- pricescores/sqrt(look_back)
# roll_stock <- roll::roll_mean(closep, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
# pricescores <- (closep - roll_stock)/var_rolling

# Calculate volatility z-scores
volv <- log(quantmod::Hi(ohlc))-log(quantmod::Lo(ohlc))
volat_scores <- drop(HighFreq::roll_zscores(respv=volv, design=indeks, look_back=look_back))
volat_scores[1:look_back] <- 0
volat_scores[is.infinite(volat_scores)] <- 0
volat_scores[is.na(volat_scores)] <- 0
volat_scores <- volat_scores/sqrt(look_back)
# roll_vol <- roll::roll_mean(volv, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var(rutils::diffit(volv), look_back=look_back))
# volat_scores <- (volv - roll_vol)/var_rolling

# Calculate volume z-scores
volume_scores <- drop(HighFreq::roll_zscores(respv=volumes, design=indeks, look_back=look_back))
volume_scores[1:look_back] <- 0
volume_scores[is.infinite(volume_scores)] <- 0
volume_scores[is.na(volume_scores)] <- 0
volume_scores <- volume_scores/sqrt(look_back)
# volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var(rutils::diffit(volumes), look_back=look_back))
# volume_scores <- (volumes - volume_mean)/var_rolling

# Define design matrix
design <- cbind(vxx_scores - svxy_scores, volat_scores, pricescores, volume_scores)
colnames(design) <- c("vxx", "stock", "volv", "volume")

# Invert the predictor matrix
design_inv <- MASS::ginv(design)
# Calculate fitted coefficients
coeff <- drop(design_inv %*% response)
names(coeff) <- c("vxx", "stock", "volv", "volume")
barplot(coeff)
# Calculate forecast
forecasts <- drop(design %*% coeff)
forecasts <- rutils::lagit(forecasts)
pnls <- sign(forecasts)*returns
# forecasts <- xts::xts(forecasts, datev)
dygraph(cumsum(pnls))

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1)*nrows

# Invert the predictor matrix in-sample
design_inv <- MASS::ginv(design[insample, ])
# Calculate in-sample fitted coefficients
coeff <- drop(design_inv %*% response[insample, ])
names(coeff) <- c("vxx", "stock", "volv", "volume")
barplot(coeff)
# Calculate out-of-sample forecasts
forecasts <- drop(design[outsample, ] %*% coeff)
forecasts <- rutils::lagit(forecasts)
pnls <- sign(forecasts)*retp[outsample, ]
# forecasts <- xts::xts(forecasts, datev)
dygraph(cumsum(pnls))



###############
### Backtest a strategy trading at oversold and overbought 
# extreme price points using weights optimization.
# Comment: The z-scores have low predictive power.

# Define z-score weights
weightv <- c(15, -15, 0, 0)
names(weightv) <- c("vxx", "stock", "volv", "volume")

coeff <- 1
lagg <- 1
thresh_top <- 0.3
thresh_bot <- (-0.1)

# Define back_test as function of thresholds
back_test <- function(weights=c(25, 0, 0, 0),
                      nrows,
                      thresh_top=0.1,
                      thresh_bot=0.1,
                      design,
                      returns,
                      lagg,
                      coeff=coeff) {
  score <- drop(design %*% weightv)
  top_s <- (score > thresh_top)
  bottom_s <- (score < thresh_bot)
  indic <- rep(NA_integer_, nrows)
  indic[1] <- 0
  indic[bottom_s] <- coeff
  indic[top_s] <- (-coeff)
  indic <- zoo::na.locf(indic, na.rm=FALSE)
  indic_sum <- roll::roll_sum(indic, width=lagg, min_obs=1)
  indic_sum[1:lagg] <- 0
  positions <- rep(NA_integer_, nrows)
  positions[1] <- 0
  positions <- ifelse(indic_sum == lagg, 1, positions)
  positions <- ifelse(indic_sum == (-lagg), -1, positions)
  positions <- zoo::na.locf(positions, na.rm=FALSE)
  positions[1:lagg] <- 0
  positions <- rutils::lagit(positions, lagg=1)
  positions
}  # end back_test

# Objective function equal to minus strategy returns
object <- function(weightv, FUN, returns, ...) {
  -sum(retp*FUN(weightv, ...))
}  # end object

# Perform weights optimization
optimd <- optim(par=weightv,
                fn=object,
                FUN=back_test,
                nrows=nrows,
                # thresh_top=thresh_top,
                # thresh_bot=thresh_bot,
                design=design,
                retp=retp,
                lagg=lagg,
                coeff=coeff,
                method="L-BFGS-B",
                upper=rep(25, 4),
                lower=rep(-25, 4))
weightv <- optimd$par
names(weightv) <- c("vxx", "stock", "volv", "volume")
back_test(weightv, 
          nrows=nrows,
          # thresh_top=thresh_top,
          # thresh_bot=thresh_bot,
          design=design,
          retp=retp,
          lagg=lagg,
          coeff=coeff)

# Objective function equal to minus strategy returns
# For DEoptim only way to make it work.
objfun <- function(weights=c(25, 0, 0, 0),
                      nrows,
                      thresh_top=0.1,
                      thresh_bot=0.1,
                      design,
                      returns,
                      lagg,
                      coeff=coeff) {
  score <- drop(design %*% weightv)
  top_s <- (score > thresh_top)
  bottom_s <- (score < thresh_bot)
  indic <- rep(NA_integer_, nrows)
  indic[1] <- 0
  indic[bottom_s] <- coeff
  indic[top_s] <- (-coeff)
  indic <- zoo::na.locf(indic, na.rm=FALSE)
  indic_sum <- roll::roll_sum(indic, width=lagg, min_obs=1)
  indic_sum[1:lagg] <- 0
  positions <- rep(NA_integer_, nrows)
  positions[1] <- 0
  positions <- ifelse(indic_sum == lagg, 1, positions)
  positions <- ifelse(indic_sum == (-lagg), -1, positions)
  positions <- zoo::na.locf(positions, na.rm=FALSE)
  positions[1:lagg] <- 0
  positions <- rutils::lagit(positions, lagg=1)
  -sum(positions*returns)
}  # end objfun

# Perform portfolio optimization using DEoptim
optimd <- DEoptim::DEoptim(fn=objfun,
                           nrows=nrows,
                           # thresh_top=thresh_top,
                           # thresh_bot=thresh_bot,
                           design=design,
                           retp=retp,
                           lagg=lagg,
                           coeff=coeff,
                           upper=rep(25, 4),
                           lower=rep(-25, 4),
                           control=list(trace=FALSE, itermax=1e3, parallelType=1))
weightv <- optimd$optim$bestmem/sum(abs(optimd$optim$bestmem))
names(weightv) <- c("vxx", "stock", "volv", "volume")
# names(weightv) <- c("vxx", "svxy", "volv", "volume")

# Calculate strategy positions
positions <- back_test(weightv, 
          nrows=nrows,
          # thresh_top=thresh_top,
          # thresh_bot=thresh_bot,
          design=design,
          retp=retp,
          lagg=lagg,
          coeff=coeff)

# Number of trades
sum(abs(rutils::diffit(positions)))
# Calculate strategy returns
pnls <- positions*returns
dygraph(cumsum(pnls))



###############
### Label the turning points in prices.

# Extract log OHLC prices
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)
closep <- log(quantmod::Cl(ohlc))
volumes <- quantmod::Vo(ohlc)
retp <- rutils::diffit(closep)
nrows <- NROW(ohlc)

# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
volv <- roll::roll_sd(retp, width=look_back, min_obs=1)
volv <- rutils::lagit(volv, lagg=(-half_back))

# Calculate the z-scores of prices
mid_p <- 1:nrows  # mid point
startp <- (mid_p - half_back)  # start point
startp[1:half_back] <- 1
endd <- (mid_p + half_back)  # end point
endd[(nrows-half_back+1):nrows] <- nrows
closep <- as.numeric(closep)
pricescores <- (2*closep[mid_p] - closep[startp] - closep[endd])
pricescores <- ifelse(volv > 0, pricescores/volv, 0)
dygraph(pricescores)
hist(pricescores)

# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c(symbol, paste(symbol, "Z-Score"))
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "Z-Score")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(pricescores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- (pricescores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- (pricescores < threshold_s[1])
colnames(bottom_s) <- "bottoms"

# Backtest in-sample VTI strategy
positions <- rep(NA_integer_, nrows)
positions[1] <- 0
positions[top_s] <- (-1)
positions[bottom_s] <- 1
positions <- zoo::na.locf(positions)
# positions <- rutils::lagit(positions, 1)
pnls <- cumsum(retp*positions)
# Number of trades
sum(abs(rutils::diffit(positions))) / NROW(positions)

# Plot dygraph of in-sample VTI strategy
prices <- cbind(closep, pnls)
colnames(prices) <- c(symbol, paste(symbol, "Strategy"))
colnamev <- colnames(prices)
dygraphs::dygraph(prices, main=paste(symbol, "In-sample Strategy")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")



###############
### Apply logistic regression using bar labels as the response.
# Comment: The z-scores have low predictive power.

# Calculate log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)

# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
retp <- rutils::diffit(closep)
volv <- roll::roll_sd(retp, width=look_back, min_obs=1)
volv <- rutils::lagit(volv, lagg=(-half_back))
# Calculate the z-scores of prices
pricescores <- (2*closep - rutils::lagit(closep, half_back, pad_zeros=FALSE) - 
                   rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricescores <- ifelse(volv > 0, pricescores/volv, 0)

# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(pricescores, c(0.2, 0.8))
# Calculate the vectors of tops and bottoms
top_s <- (pricescores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- (pricescores < threshold_s[1])
colnames(bottom_s) <- "bottoms"

# Calculate SVXY and VXX prices
svxy <- log(quantmod::Cl(rutils::etfenv$SVXY))
nrows <- NROW(svxy)
datev <- zoo::index(svxy)
vxx <- log(quantmod::Cl(rutils::etfenv$VXX))
vxx <- vxx[datev]

# Calculate rolling VTI volatility
volv <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
volv <- xts::xts(sqrt(volv), zoo::index(ohlc))
colnames(volv) <- "volv"

# Calculate trailing z-scores of SVXY
design <- cbind(volv[datev], vxx, closep[datev])
respv <- svxy
zscores <- drop(HighFreq::roll_zscores(respv=respv, design=design, look_back=look_back))
zscores[1:look_back] <- 0
zscores[is.infinite(zscores)] <- 0
zscores[is.na(zscores)] <- 0
# zscores <- zscores/sqrt(look_back)

# Calculate SVXY medians
# medi_an <- roll::roll_median(svxy, width=look_back, min_obs=1)
# Calculate SVXY MAD
# madv <- HighFreq::roll_var(svxy, look_back=look_back, method="nonparametric")
# Calculate SVXY Hampel z-scores
# svxy_scores <- ifelse(madv > 0, (svxy - medi_an)/madv, 0)
# svxy_scores[1:look_back, ] <- 0
# Calculate SVXY z-scores
roll_svxy <- roll::roll_mean(svxy, width=look_back, min_obs=1)
svxy_sd <- roll::roll_sd(rutils::diffit(svxy), width=look_back, min_obs=1)
svxy_sd[1] <- 0
svxy_scores <- ifelse(svxy_sd > 0, (svxy - roll_svxy)/svxy_sd, 0)

# Calculate VXX z-scores
roll_vxx <- roll::roll_mean(vxx, width=look_back, min_obs=1)
vxx_sd <- roll::roll_sd(rutils::diffit(vxx), width=look_back, min_obs=1)
vxx_sd[1] <- 0
vxx_scores <- ifelse(vxx_sd > 0, (vxx - roll_vxx)/vxx_sd, 0)

# Calculate volatility z-scores
volv <- log(quantmod::Hi(ohlc))-log(quantmod::Lo(ohlc))
roll_vol <- roll::roll_mean(volv, width=look_back, min_obs=1)
volat_scores <- (volv - roll_vol)/roll_vol

# Calculate volume z-scores
volumes <- quantmod::Vo(ohlc)
volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
volume_scores <- (volumes - volume_mean)/volume_sd

# Define design matrix
design <- cbind(vxx_scores, svxy_scores, volat_scores[datev], volume_scores[datev])
colnames(design) <- c("vxx", "svxy", "volv", "volume")
design <- zoo::coredata(design)
top_s <- top_s[datev]

respv <- as.numeric(top_s[datev])


# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows

# Fit in-sample logistic regression for tops
# Only volv is significant
respv <- as.numeric((top_s[datev][insample]))

glm_tops <- glm(respv[insample] ~ design[insample], family=binomial(logit))
glm_tops <- glm(respv ~ design[insample], 
                data=design[insample],
                family=binomial(logit))
summary(glm_tops)

####################
# by hand

coeff <- glm_bottoms$coefficients
probs <- function(coeff) {
  plogis(drop(cbind(intercept=rep(1, NROW(insample)), design[insample]) %*% coeff))
}  # end probs
probs(rep(1, 5))



# Define likelihood function
likeli_hood <- function(coeff, response, design) {
  probs <- plogis(drop(design %*% coeff))
  -sum(respv*log(probs) + (1-response)*log((1-probs)))
}  # end likeli_hood
# Run likelihood function
likeli_hood(rep(1, 5), 
            respv=respv, 
            design=cbind(intercept=rep(1, NROW(insample)), design[insample]))


# Initial parameters
par_init <- rep(1, 5)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=par_init,
                   fn=likeli_hood, # Log-likelihood function
                   method="L-BFGS-B", # Quasi-Newton method
                   respv=respv,
                   design=cbind(intercept=rep(1, NROW(insample)), design[insample]), 
                   upper=rep(2, 5), # Upper constraint
                   lower=rep(-2, 5), # Lower constraint
                   hessian=TRUE)

# Optimal and actual parameters
optim_fit$par
unname(glm_tops$coefficients)

# Standard errors of parameters
sqrt(diag(solve(optim_fit$hessian)))
model_sum <- summary(glm_tops)
model_sum$coefficients[, 2]



# Find max likelihood parameters using DEoptim
optimd <- DEoptim::DEoptim(fn=likeli_hood,
                           upper=rep(2, 5), # Upper constraint
                           lower=rep(-2, 5), # Lower constraint
                           respv=respv,
                           design=cbind(intercept=rep(1, NROW(insample)), design[insample]), 
                           control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
optimd$optim$bestmem
unname(glm_tops$coefficients)



####################

# Fit in-sample logistic regression for bottoms
# Nothing is significant!
respv <- bottom_s[datev][insample]
glm_bottoms <- glm(respv ~ design[insample], family=binomial(logit))
summary(glm_bottoms)

# Find best in-sample thresholds
nrows <- NROW(respv)
datev <- zoo::index(respv)
retp <- retp[datev]
fitted_bottoms <- glm_bottoms$fitted.values
fitted_tops <- glm_tops$fitted.values

# Define runsimu as function of confidence levels
runsimu <- function(confi=c(0.1, 0.9)) {
  threshold <- quantile(fitted_bottoms, confi[1])
  bottom_s <- (fitted_bottoms < threshold)
  threshold <- quantile(fitted_tops, confi[2])
  top_s <- (fitted_tops > threshold)
  positions <- rep(NA_integer_, nrows)
  positions[1] <- 0
  positions[top_s] <- (-1)
  positions[bottom_s] <- 1
  positions <- zoo::na.locf(positions)
  -sum(retp*positions)
}  # end runsimu

runsimu()
confi <- c(0.5, 0.6)
names(confi) <- c("bottom", "top")
# Find weights with maximum variance
optimd <- optim(par=confi,
                fn=runsimu,
                method="L-BFGS-B",
                upper=rep(1, 2),
                lower=rep(0, 2))
confi <- optimd$par


# Run out-of-sample
positions <- rep(NA_integer_, NROW(outsample))
positions[1] <- 0

# Forecast over test data out-of-sample
coeff <- glm_bottoms$coefficients
forecasts <- plogis(drop(cbind(intercept=rep(1, NROW(outsample)), design[outsample]) %*% coeff))
threshold <- quantile(fitted_bottoms, confi[1])
bottom_s <- (fitted_bottoms < threshold)

coeff <- glm_tops$coefficients
forecasts <- plogis(drop(cbind(intercept=rep(1, NROW(outsample)), design[outsample]) %*% coeff))
threshold <- quantile(fitted_tops, confi[2])
top_s <- (forecasts > threshold)

positions[top_s] <- (-1)
positions[bottom_s] <- 1
positions <- zoo::na.locf(positions)




###############
### Create trending portfolios of similar ETFs.

# Load packages
library(rutils)

# Calculate ETF returns and volumes
# VXX with SVXY

# Calculate ETF returns and volumes
symbolv <- c("VTI", "SVXY", "VXX")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)

look_back <- 11
# Scale the volume by the rolling average volume
rets_scaled <- lapply(symbolv, function(symbol) {
  ohlc <- get(symbol, rutils::etfenv)
  retp <- rutils::diffit(log(quantmod::Cl(ohlc)))
  volumes <- quantmod::Vo(ohlc)
  volume_rolling <- roll::roll_mean(volumes, width=look_back)
  volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
  volumes <- volumes/volume_rolling
  # Divide  the returns by the volume - use trading time (volume clock)
  returns/volumes
})  # end lapply

# Scale by the High minus Low range
rets_scaled <- lapply(symbolv, function(symbol) {
  ohlc <- log(get(symbol, rutils::etfenv))
  retp <- rutils::diffit(quantmod::Cl(ohlc))
  rangev <- (quantmod::Hi(ohlc) - quantmod::Lo(ohlc))
  returns/rangev
})  # end lapply

# Scale by the rolling volatility
rets_scaled <- lapply(symbolv, function(symbol) {
  ohlc <- log(get(symbol, rutils::etfenv))
  retp <- rutils::diffit(quantmod::Cl(ohlc))
  varv <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
  colnames(varv) <- "variance"
  volv <- sqrt(varv)
  returns/volv
})  # end lapply

rets_scaled <- do.call(cbind, rets_scaled)
colnames(rets_scaled) <- do.call(rbind,(strsplit(colnames(rets_scaled), split="[.]")))[, 1]
rets_scaled <- na.omit(rets_scaled)
rets_scaled <- rets_scaled[datev]

weightv <- 1/sapply(rets_scaled, sd)/100
retsport <- rets_scaled %*% weightv

# PACF of AR(1) process
x11(width=6, height=5)
pacfv <- pacf(retsport, lag=10, xlab="", ylab="", main="")
abs(sum(pacfv$acf))

sum_pacf <- function(weightv) {
  retsport <- rets_scaled %*% weightv
  pacfv <- pacf(retsport, lag=10, plot=FALSE)
  -sum(pacfv$acf) - sum(retsport) + (1-sum(weightv^2))^2
}  # end sum_pacf


# Calculate end points
interval <- 21
nrows <- NROW(rets_scaled)
nagg <- nrows %/% interval
endd <- c(0, nrows - nagg*interval + (0:nagg)*interval)

# Calculate Hurst
hurstfun <- function(weightv) {
  retsport <- rets_scaled %*% weightv
  prices <- cumsum(retsport)
  rrange <- sapply(2:NROW(endd), function(ep) {
    indeks <- endd[ep-1]:endd[ep]
    diff(range(prices[indeks]))/sd(retsport[indeks])
  })  # end sapply
  # Calculate Hurst from single data point
  -log(mean(rrange))/log(interval) - sum(retsport) + (1-sum(weightv^2))^2
}  # end sum_pacf

optimd <- optim(par=c(0.1, 0.1, 0.1), 
                fn=hurstfun,
                method="L-BFGS-B",
                upper=c(10, 10, 10),
                lower=c(0, 0, 0))
# Optimal parameters and value
weightv <- optimd$par
retsport <- rets_scaled %*% weightv
pacf(retsport, lag=10, xlab="", ylab="", main="")
retsport <- xts::xts(retsport, datev)

datav <- cbind(returns$VTI, retsport)
sharper <- sqrt(252)*sapply(datav, function(x) mean(x)/sd(x[x<0]))

colnames(pnls) <- c(paste(symbol, "Returns"), "Strategy", "Buy", "Sell")

captiont <- paste("Strategy for", symbol, "Returns Scaled by the Trading Volumes / \n", 
                  paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                  "Number of trades=", ntrades)

colnamev <- colnames(datav)
dygraphs::dygraph(cumsum(datav), main="Autoregressive Portfolio") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

dygraphs::dygraph(datav, main="Autoregressive Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)



###############
### Autoregressive strategy using the principal components  
# of average returns as predictors.
# Mostly in app_ar_pca_strat.R

# Load packages
library(rutils)

# Calculate ETF returns and volumes
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
retp <- rutils::diffit(closep)
retc <- cumsum(retp)
volumes <- quantmod::Vo(ohlc)

insample <- 1:(nrows %/% 4)
outsample <- (nrows %/% 4 + 1):nrows

# Scale the volume by the rolling average volume
look_back <- 11
volume_rolling <- roll::roll_mean(volumes, width=look_back)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
volumes <- volumes/volume_rolling

# Divide  the returns by the volume - use trading time (volume clock)
# rets_scaled <- ifelse(volumes > 0, returns/volumes, 0)
rets_scaled <- returns/volumes

## Version using only recent returns

# First version
look_backs <- 2:25
design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x))
design <- do.call(cbind, design)
design[1, ] <- 0
design <- zoo::na.locf(design)
# sum(is.na(design))
design <- cbind(rets_scaled, design)
max_back <- last(look_backs)
# respv <- rutils::lagit(design[, max_back], lagg=(-max_back))
respv <- sqrt(max_back)*roll::roll_mean(retp, max_back)
response[1:(max_back-1)] <- 0
respv <- rutils::lagit(respv, lagg=(-max_back))

# Second version
# Define predictor matrix for forecasting
order_max <- 10
design <- lapply(1:order_max, rutils::lagit, input=retp)
design <- do.call(cbind, design)
colnames(design) <- paste0("pred_", 1:NCOL(design))
# respv <- returns
respv <- rutils::lagit(design[, order_max], lagg=(-order_max))


###########
## Old version
# Calculate design equal to the rolling means
look_backs <- c(5, 20, 80, 250)
# design <- lapply(look_backs, roll::roll_mean, x=retp)
# Scale the rolling means so they have similar volatities
design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(retp, x))
# design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(retp, x)/sqrt(roll::roll_var(retp, x)))
design <- do.call(cbind, design)
design[1, ] <- 0
design <- zoo::na.locf(design)
# sum(is.na(design))
colnames(design) <- paste0("back_", look_backs)
sapply(design, sd)
# Standardize (de-mean and scale) the design
# design <- lapply(design, function(x) {(x - mean(x))/sd(x)})
# design <- rutils::do_call(cbind, design)

# Define response as a rolling sum and shift it forward out-of-sample
respv <- rutils::lagit(design[, 1], lagg=(-look_backs[1]))

###########
## End Old version

# Calculate covariance matrix of design
covmat <- cov(design)
# Calculate eigenvectors and eigenvalues
eigend <- eigen(covmat)

# Define predictors as the principal components of design
# eigenvec <- eigend$vectors
predv <- xts::xts(design %*% eigend$vectors, order.by=datev)
colnames(predv) <- paste0("pc", 1:NCOL(predv))
# round(cov(ratestdeviff), 3)
predv <- rutils::lagit(predv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv)[1] <- "unit"

# Calculate in-sample fitted coefficients
dimax <- 4
invmat <- MASS::ginv(predictor[insample, 1:dimax])
coeff <- drop(invmat %*% response[insample])
# Calculate out-of-sample forecasts of returns
# forecasts <- drop(predictor[outsample, 1:3] %*% coeff[1:3])
forecasts <- drop(predictor[outsample, 1:dimax] %*% coeff)
mean((retp[outsample, ] - forecasts)^2)
drop(cor(retp[outsample, ], forecasts))

# Lag the positions to trade in next period
positions <- sign(rutils::lagit(forecasts))

# Calculate strategy pnls
pnls <- cumsum(positions*retp[outsample])
pnls <- cbind(cumsum(retp[outsample]), pnls)
colnames(pnls) <- c(symbol, "Strategy")
dygraphs::dygraph(pnls, main="Autoregressive Strategy Performance") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)


###############
# Case for look_backs = 2

# Calculate ETF returns and volumes
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
retp <- rutils::diffit(closep)
retc <- cumsum(retp)
volumes <- quantmod::Vo(ohlc)

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows

# Scale the volume by the rolling average volume
look_back <- 11
volume_rolling <- roll::roll_mean(volumes, width=look_back)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
volumes <- volumes/volume_rolling
# Divide  the returns by the volume - use trading time (volume clock)
rets_scaled <- returns/volumes

look_backs <- 2:5
design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x))
design <- do.call(cbind, design)
# look_backs <- 2
# design <- sqrt(2)*roll::roll_mean(rets_scaled, 2)
design[1, ] <- 0
design <- zoo::na.locf(design)
design <- cbind(rets_scaled, design)
max_back <- last(look_backs)

respv <- sqrt(max_back)*roll::roll_mean(retp, max_back)
response[1:(max_back-1)] <- 0
respv <- rutils::lagit(respv, lagg=(-max_back))
covmat <- cov(design)
eigend <- eigen(covmat)
predv <- xts::xts(design %*% eigend$vectors, order.by=datev)
predv <- rutils::lagit(predv)
# predv <- cbind(rep(1, nrows), predv)
invmat <- MASS::ginv(predictor[insample, ])
coeff <- drop(invmat %*% response[insample])
forecasts <- drop(predictor[outsample, ] %*% coeff)
forecasts <- roll::roll_mean(sign(forecasts), width=max_back)
forecasts[1:(max_back-1)] <- 1
pnls <- cumsum(forecasts*retp[outsample])
pnls <- cbind(cumsum(retp[outsample]), pnls)
colnames(pnls) <- c(symbol, "Strategy")
dygraphs::dygraph(pnls, main="Autoregressive Strategy Performance") %>%
dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
dyLegend(width=500)



###############
### PTS Sentiment SPY trading strategy
# Load packages

# Load packages
library(rutils)
library(data.table)

# Load data with SPY sentiment from csv file
datav <- data.table::fread(file="C:/Develop/predictive/data/correlation_news_to_price_change.csv", stringsAsFactors=FALSE)
datav <- datav[, c("date", "sentiment", "close")]
colnames(datav) <- c("date", "sentiment", "SPY")
sapply(datav, class)
datav <- xts::xts(datav[, 2:3], as.Date.IDate(datav[, date]))


retp <- lapply(datav[, -1], rutils::diffit)
retp <- rutils::do_call(cbind, returns)
colnames(retp) <- c("sentiment", "SPY")
cor(rutils::lagit(retp[, 1]), retp[, 2])

x11(width=6, height=5)
plot(SPY ~ sentiment, data=retp)

colnamev <- colnames(datav)
captiont <- paste(colnamev, collapse=" vs ")
dygraphs::dygraph(datav, main=captiont) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")




###############
### Benchmark of rolling functions.

# Benchmark the speed of TTR::runMedian()
ohlc <- HighFreq::SPY["2011"]
closep <- log(drop(coredata(Cl(ohlc))))
ro_ll <- roll::roll_median(closep, width=look_back)[-(1:(look_back-1))]
rcpp_roll <- RcppRoll::roll_median(closep, n=look_back)
tt_r <- TTR::runMedian(closep, n=look_back)[-(1:(look_back-1))]
all.equal(rcpp_roll, coredata(tt_r), check.attributes=FALSE)
all.equal(ro_ll, tt_r, check.attributes=FALSE)
library(microbenchmark)
# roll::roll_median() is several times faster than the other two
summary(microbenchmark(
  ro_ll=roll::roll_median(closep, width=look_back),
  rcpp_roll=RcppRoll::roll_median(closep, n=look_back),
  tt_r=TTR::runMedian(closep, n=look_back),
  times=10))[, c(1, 4, 5)]


# Benchmark the speed of HighFreq::roll_scale()
# Use roll and TTR
retp <- rutils::diffit(closep)
med_rets <- roll::roll_median(retp, width=look_back)
mad_rets <- TTR::runMAD(retp, n=look_back)
re_scaled <- (retp - med_rets)/mad_rets
# Use HighFreq::roll_scale()
re_scaledh <- HighFreq::roll_scale(matrix(retp, ncol=1), look_back=look_back, use_median=TRUE)
re_scaledh <- drop(re_scaledh)
# Same result up to factor of qnorm(0.75)
tail(re_scaled)/tail(re_scaledh)
library(microbenchmark)
# HighFreq::roll_scale() is over twice as fast
summary(microbenchmark(
  h_freq=HighFreq::roll_scale(matrix(retp, ncol=1), look_back=look_back, use_median=TRUE),
  rcpp_roll={
    (retp - roll::roll_median(retp, width=look_back))/TTR::runMAD(retp, n=look_back)
  },
  times=10))[, c(1, 4, 5)]





###############
### Prototype of function get_data() for rutils

get_data <- function(symbolv,
                     data_dir = NULL, # the directory containing csv files
                     data_env = NULL, # the environment for writing xts into
                     startd = "2000-01-01",
                     endd = Sys.Date(),
                     date_fun = match.fun("as.Date"),
                     formatv = "%Y-%m-%d",
                     header = TRUE,
                     echo = TRUE,
                     scrub = TRUE,
                     api.key = NULL) {
  if (is.null(data_dir)) {
    # download prices from Tiingo
    output <- quantmod::getSymbols.tiingo(symbolv,
                                           env = data_env,
                                           from = startd,
                                           to = endd,
                                           adjust = TRUE,
                                           auto.assign = TRUE,
                                           api.key = api.key)
    # Adjust the OHLC prices and save back to data_env
    # output <- lapply(symbolv,
    #                   function(symbol) {
    #                     assign(symbol,
    #                            value = adjust_ohlc(get(symbol, envir = data_env)),
    #                            envir = data_env)
    #                     symbol
    #                   }
    # )  # end lapply
    invisible(output)
  } else {
    # load from csv files
    file_names <- file.path(data_dir, paste0(symbolv, ".csv"))
    invisible(sapply(file_names, function(file_name) {
      if (echo)
        cat("Loading instrument: \t", file_name, "\n")
      datav <- xts::as.xts(zoo::read.zoo(file = file_name,
                                         header = header, sep = ",",
                                         drop = FALSE,
                                         FUN = date_fun,
                                         format = formatv))
      if (scrub) {
        # overwrite NA values
        datav <- rutils::na_locf(datav)
        datav <- rutils::na_locf(datav, from_last = TRUE)
      }  # end if
      assign(rutils::get_name(colnames(datav)[1]),
             datav,
             envir = data_env)
      file_name
    }))  # end sapply
  }  # end if
}  # end get_data



###############
### PTS AAPL tick data trading strategy

# Load packages
library(rutils)
library(data.table)

## Load data with AAPL stock features from csv file
raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl20201021.csv", sep="\t")
# tail(raw_ticks)
# class(raw_ticks)
# sapply(raw_ticks, class)
# unlist(sapply(raw_ticks, function(x) if (is.numeric(x)) sum(x))) == 0
# Remove empty columns
raw_ticks <- raw_ticks[, .(timestamp=V10, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 10)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Or more recent data
raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl20201102.csv", sep=",")
raw_ticks <- raw_ticks[, .(timestamp=V8, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 8)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Bind additional pieces of data together
foo <- data.table::fread(file="C:/Develop/predictive/data/aapl20201030.csv", sep="\t")
foo <- foo[, c(1:3, 8)]
colnames(foo) <- c("price", "volume", "seconds", "timestamp")
foo <- foo[, .(timestamp, seconds, price, volume)]
bar <- (last(raw_ticks)$price - first(foo)$price)
foo[, price := (price + bar)]
raw_ticks <- rbind(raw_ticks, foo)



## Apply Hampel filter to remove price jumps

look_back <- 111
half_window <- look_back %/% 2
medi_an <- TTR::runMedian(raw_ticks$price, n=look_back)
medi_an <- rutils::lagit(medi_an, lagg=-half_window, pad_zeros=FALSE)
madv <- TTR::runMAD(raw_ticks$price, n=look_back)
madv <- rutils::lagit(madv, lagg=-half_window, pad_zeros=FALSE)
madv[1:half_window] <- 1
madv[madv == 0] <- 1

zscores <- (raw_ticks$price - medi_an)/madv
zscores[is.na(zscores)] <- 0
zscores[!is.finite(zscores)] <- 0
sum(is.na(zscores))
sum(!is.finite(zscores))
range(zscores)
mad(zscores)
foo <- hist(zscores, breaks=1000, xlim=c(-5*mad(zscores), 5*mad(zscores)))

threshold <- 3
bad_ticks <- (abs(zscores) > threshold)
good_ticks <- raw_ticks[!bad_ticks]
good_ticks <- raw_ticks[volume > 10]

## Calculate a vector of returns
retp <- rutils::diffit(good_ticks$price)
nrows <- NROW(retp)


## Simple big tick contrarian strategy - trade on next tick after large volume tick

# Trade on large volume and non-zero return
big_ticks <- (good_ticks$volume >= 2000) & (abs(retp) > 0)
# Or: Trade on large volume and go flat if zero return
# big_ticks <- (good_ticks$volume >= 100)
positions <- rep(NA_integer_, nrows)
positions[1] <- 0
positions[big_ticks] <- -sign(retp[big_ticks])
positions <- zoo::na.locf(positions)
positions <- rutils::lagit(positions, 3)
pnls <- cumsum(retp*positions)
x11(width=6, height=5)
plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(positions))) / NROW(positions)
# Plot dygraph
datev <- as.POSIXct(good_ticks$seconds, origin="1970-01-01")
# There are many duplicate dates:
NROW(good_ticks$seconds) == NROW(unique(good_ticks$seconds))
# Make dates unique:
datev <- xts::make.index.unique(datev, drop=TRUE)
pnls <- xts::xts(pnls, datev)
# dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, good_ticks$price)
colnamev <- c("Strategy", "AAPL")
colnames(pnls) <- colnamev
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)

datav <- cbind(good_ticks, positions, pnls$Strategy)
data.table::fwrite(datav, file="C:/Develop/predictive/data/aapl_strategy.csv")


## Simple big tick contrarian strategy - trade on large volume ticks only

big_ticks <- raw_ticks[volume >= 400]
retp <- rutils::diffit(big_ticks$price)
# Flip position or flatten if returns == 0
positions <- (-rutils::lagit(sign(retp)))
pnls <- cumsum(retp*positions)
plot(pnls, t="l")
datav <- cbind(big_ticks, positions, pnls$Strategy)
data.table::fwrite(datav, file="C:/Develop/predictive/data/aapl_strategy.csv")
# Number of trades
sum(abs(rutils::diffit(positions))) / NROW(positions)
# Plot dygraph
datev <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnls <- xts::xts(pnls, datev)
dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, big_ticks$price)
colnamev <- c("Strategy", "AAPL")
colnames(pnls) <- colnamev
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)


# Or always carry a position - doesn't work so well
positions <- rep(NA_integer_, NROW(retp))
positions[1] <- 0
positions <- ifelse(retp > 0, -1, positions)
positions <- ifelse(retp < (-1), 1, positions)
positions <- zoo::na.locf(positions)
# positions <- (-rutils::lagit(sign(retp)))
positions <- rutils::lagit(positions)


# temp stuff
foo <- tail(raw_ticks, 33)
datev <- as.POSIXct(foo$V3, origin="1970-01-01")
ohlc <- foo[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=seconds]
sum(foo[foo$seconds == foo[33]$seconds]$volume)


# Aggregate to OHLC
# tail(raw_ticks)
ohlc <- raw_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=seconds]
# all.equal(ohlc$seconds, unique(raw_ticks$seconds))
startd <- as.numeric(as.POSIXct("2020-10-21 09:30:00"))
endd <- as.numeric(as.POSIXct("2020-10-21 16:00:00"))
ohlc <- ohlc[seconds >= startd & seconds <= endd]
foo <- hist(ohlc$volume[-which(ohlc$volume > max(ohlc$volume)/1000)], breaks=1e3, xlim=c(0, 1000))


## Simple OHLC contrarian strategy - trade on large volume only

foo <- ohlc[volume >= 200]
retp <- rutils::diffit(foo$close)
nrows <- NROW(retp)
# foo <- hist(retp, breaks=500, xlim=c(-0.1, 0.1))
positions <- (-rutils::lagit(sign(retp)))
pnls <- cumsum(retp*positions)
plot(pnls, t="l")

foo <- lapply(100*(2:10), function(x) {
  retp <- rutils::diffit(ohlc[volume >= x]$close)
  positions <- (-rutils::lagit(sign(retp)))
  cumsum(retp*positions)
})  # end lapply
sapply(foo, NROW)
sapply(foo, last)


## AR strategy for OHLC - too complicated?

# Calculate a vector of returns
retp <- rutils::diffit(ohlc$close)
nrows <- NROW(retp)
hist(retp, breaks=500, xlim=c(-0.1, 0.1))

insample <- 1:(nrows %/% 2)
out_of_sample <- (nrows %/% 2 + 1):nrows

order_max <- 5  # Define maximum order parameter
look_back <- 5

predv <- rutils::roll_sum(retp, look_back=look_back)
# Shift the response forward into out-of-sample
respv <- rutils::lagit(predictor, lagg=(-look_back))
# Define predictor matrix for forecasting
predv <- sapply(1+look_back*(0:order_max), rutils::lagit,
                     input=predv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv) <- paste0("pred_", 1:NCOL(predv))
# Calculate forecasts as function of ordern
forecasts <- lapply(2:NCOL(predv), function(ordern) {
  # Calculate fitted coefficients
  invmat <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(invmat %*% response[insample])
  # Calculate out-of-sample forecasts of returns
  drop(predictor[out_of_sample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predv))

# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*retp[out_of_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, index(prices[out_of_sample]))
plot(pnls[, 1], t="l")
plot.zoo(pnls)



## Simple contrarian strategy using Hampel filter - doesn't work too well

# Calculate a time series of rolling z-scores
look_back <- 5
# prices <- big_ticks$price
retp <- rutils::diffit(big_ticks$price)
medi_an <- TTR::runMedian(retp, n=look_back)
medi_an[1:look_back] <- 1
# sum(is.na(medi_an))
madv <- TTR::runMAD(retp, n=look_back)
madv[1:look_back] <- 1
madv[madv < 1e-6] <- 1
# sum(is.na(madv))
zscores <- ifelse(madv!=0, (retp-medi_an)/madv, 0)
zscores[1:look_back] <- 0
# sum(is.na(zscores))
# madv <- zoo::na.locf(zscores)
# mad_zscores <- TTR::runMAD(zscores, n=look_back)
# mad_zscores[1:look_back, ] <- 0

tail(zscores)
mad(zscores)
range(zscores)
x11(width=6, height=5)
hist(zscores, breaks=200, xlim=c(-5, 5), freq=FALSE)

# Calculate positions and pnls from z-scores
positions <- rep(NA_integer_, NROW(retp))
positions[1] <- 0
# threshold <- 3*mad(zscores)
threshold <- 1
positions <- ifelse(zscores > threshold, -1, positions)
positions <- ifelse(zscores < (-threshold), 1, positions)
# positions <- ifelse(zscores > 2*mad_zscores, -1, positions)
# positions <- ifelse(zscores < (-2*mad_zscores), 1, positions)
positions <- zoo::na.locf(positions)
# Number of trades
# sum(abs(rutils::diffit(positions))) / NROW(positions)
positions_lag <- rutils::lagit(positions, lagg=2)
pnls <- cumsum(positions_lag*returns)

plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(positions))) / NROW(positions)
# Plot dygraph
datev <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnls <- xts::xts(pnls, datev)
dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, big_ticks$price)
colnamev <- c("Strategy", "AAPL")
colnames(pnls) <- colnamev
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)




############### temp

indic <- rutils::diffit(positions)
indic_buy <- (indic > 0)
indic_sell <- (indic < 0)
retsc <- cumsum(retp)


pnls <- cbind(pnls, retsc[indic_buy], retsc[indic_sell])
colnames(pnls)[3:4] <- c("Buy", "Sell")

colnamev <- colnames(pnls)
dygraphs::dygraph(pnls, main=captiont) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[3], axis="y2", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
  dySeries(name=colnamev[4], axis="y2", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")




############### homework
# Summary: Study how the dispersion of the Hampel z-scores 
# depends on the level of volatility in the interval.
# Yes, zscores have higher dispersion on more volatile days.
# But so what?

ohlc <- HighFreq::SPY["T09:31:00/T15:59:00"]
# ohlc <- rutils::etfenv$VTI
nrows <- NROW(ohlc)
ohlc <- log(ohlc[, 1:4])
closep <- Cl(ohlc)
# Calculate the zscores
look_back <- 11
medi_an <- TTR::runMedian(closep, n=look_back)
medi_an[1:look_back, ] <- 1
zscores <- (closep-medi_an)
zscores[1:look_back, ] <- 0
mad_zscores <- TTR::runMAD(zscores, n=10*look_back)
mad_zscores[1:(10*look_back), ] <- 0
zscores <- ifelse(mad_zscores != 0, zscores/mad_zscores, 0)

# Calculate the log variance for SPY
varv <- xts::apply.daily(ohlc, HighFreq::calc_var_ohlc)
# For VTI
# varv <- sapply((2*look_back):nrows, function(ro_w) {
#   HighFreq::calc_var_ohlc(ohlc[(ro_w-look_back+1):ro_w, ], scale=FALSE)
# })  # end sapply
# varv <- c(varv[1]+numeric(2*look_back-1), varv)
# x11(width=6, height=5)
# plot(varv)
# Plot the VTI volatility
volv <- sqrt(varv)
# volv <- xts::xts(sqrt(varv), index(ohlc))
dygraphs::dygraph(volv, main="VTI Volatility")


# Plot zscores versus volatility for VTI
plot(as.numeric(zscores) ~ as.numeric(volv))


# Calculate dates with high volatility
is_high <- (volv > max(volv)/10)
is_high <- is_high[is_high]
high_days <- index(is_high)
high_days <- as.Date(high_days)
# low_days <- high_days[!high_days]

datev <- index(ohlc)
datev <- as.Date(datev)

is_high <- datev %in% high_days
high_scores <- zscores[is_high]
low_scores <- zscores[!is_high]

# Plot histogram of zscores
range(low_scores)
low_scores <- low_scores[low_scores > quantile(low_scores, 0.05)]
low_scores <- low_scores[low_scores < quantile(low_scores, 0.95)]
x11(width=6, height=5)
hist(low_scores, xlim=c(quantile(low_scores, 0.05), quantile(low_scores, 0.95)), breaks=50, main=paste("low_scores", "look_back =", look_back))

range(high_scores)
high_scores <- high_scores[high_scores > quantile(high_scores, 0.05)]
high_scores <- high_scores[high_scores < quantile(high_scores, 0.95)]
x11(width=6, height=5)
hist(high_scores, xlim=c(quantile(high_scores, 0.05), quantile(high_scores, 0.95)), breaks=50, main=paste("high_scores", "look_back =", look_back))



###############
### PTS AAPL features PCA dimension reduction trading strategy

# Load packages
library(rutils)

# Load data with AAPL stock features from csv file
datav <- data.table::fread(file="C:/Develop/predictive/data/jerzy_aapl20200720.csv", stringsAsFactors=FALSE)
retp <- datav$price_change_plus5min
datav <- datav[, -"price_change_plus5min"]
datav <- as.matrix(datav)
cor_vec <- drop(cor(retp, datav))
barplot(cor_vec, main="Correlations of Features to the AAPL Returns")
data_scaled <- scale(datav, center=TRUE, scale=TRUE)
sd_data <- apply(datav, MARGIN=2, sd)
meandata <- apply(datav, MARGIN=2, mean)

# Calculate correlation matrix
cormat <- cor(data_scaled)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat,
                       order="hclust",
                       hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colors <- colorRampPalette(c("red", "white", "blue"))
x11()
corrplot(cormat, title="AAPL Features Correlation Matrix",
         tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
         method="square", col=colors(8),
         cl.offset=0.75, cl.cex=0.7,
         cl.align.text="l", cl.ratio=0.25)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
                method="complete", col="red")


# Perform PCA
pcad <- prcomp(data_scaled, scale=FALSE)
# Plot barplots with PCA vectors weights in multiple panels
x11()
nweightv <- 6
par(mfrow=c(nweightv/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:nweightv) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
} # end for

# Inspect principal component time series
round(cor(pcad$x), 4)
plot(pcad$x[, 1], t="l")

# Calculate correlations of principal component time series and returns
returns_std <- (retp - mean(retp))/sd(retp)
stdev <- apply(pcad$x, MARGIN=2, sd)
# pcats <- scale(pcad$x, center=TRUE, scale=TRUE)
cor_vec <- cor(retp, pcad$x)
# apply(retv_std*pcats, MARGIN=2, sum)/NROW(retv_std)
# Calculate weights equal to correlations
weightv <- cor_vec/stdev
x11()
barplot(weightv)

# Invert all the principal component time series
invmat <- solve(pcad$rotation)
weightinv <- drop(weights %*% invmat)
weightinv <- weightinv/sd_data
foo <- drop(datav %*% weightv_solved)
cor(retp, foo)
barplot(weightinv)
barplot(weightinv, main="Weights of Features in New Feature")


# Simulate trading strategy
positions <- rep(NA_integer_, NROW(retp))
positions[1] <- 0
# Long positions
# indic <- (datav[, feature4] + datav[, feature5])
indic <- (datav[, feature4] + datav[, feature5])
positions <- indic
# positions <- ifelse(indic >= lagg, 1, positions)
# Short positions
# indic <- ((closep - vwapv) < (-threshold*rangev))
# indic <- HighFreq::roll_count(indic)
# positions <- ifelse(indic >= lagg, -1, positions)
# positions <- zoo::na.locf(positions, na.rm=FALSE)
# Lag the positions to trade in next period
positions <- rutils::lagit(positions, lagg=1)
pnls <- cumsum(coeff*retp*positions)
plot(pnls[(1e3*(1:(NROW(retp) %/% 1e3)))], t="l")




###############
### Backtest rescaled range strategy

# Load packages
library(HighFreq)

# Calculate rolling rescaled cumulative returns from OHLC data
roll_range <- function(ohlc, look_back=11) {
  retp <- rutils::diffit(ohlc[, 4])
  rangev <- HighFreq::roll_sum(retp, look_back=look_back)
  var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
  look_back <- sqrt(look_back)
  hurst_rolling <- ifelse((var_rolling==0) | (rangev==0),
                          0.0,
                          rangev/var_rolling/look_back)
  # Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  # end roll_range


# Calculate rolling rescaled cumulative returns from returns data
roll_range <- function(retp, cum_returns, look_back=11) {
  rangev <- HighFreq::roll_sum(retp, look_back=look_back)
  var_rolling <- sqrt(HighFreq::roll_var(retp, look_back=look_back))
  look_back <- sqrt(look_back)
  hurst_rolling <- ifelse((var_rolling==0) | (rangev==0),
                          0.0,
                          rangev/var_rolling/look_back)
  # Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  # end roll_range


retp <- rutils::diffit(drop(zoo::coredata(quantmod::Cl(HighFreq::SPY))))
nrows <- NROW(retp)
dim(retp) <- c(nrows, 1)
# cum_retv <- cumsum(retp)



# Calculate rolling Hurst for SPY
hurst_rolling <- HighFreq::roll_hurst(ohlc=HighFreq::SPY, look_back=7)
# Calculate rolling rescaled range
hurst_rolling <- roll_range(ohlc=HighFreq::SPY, look_back=5)
hurst_rolling <- roll_range(retp=retp, look_back=5)
# chart_Series(hurst_rolling["2009-03-10/2009-03-12"], name="SPY hurst_rolling")

# threshold <- 0.5

x11(width=6, height=5)
quantiles <- quantile(hurst_rolling, c(0.01, 0.99))
hist(hurst_rolling, xlim=quantiles, breaks=1e2)


# Trade on rescaled range

quantiles <- quantile(hurst_rolling, c(0.2, 0.8))
positions <- rep(NA_integer_, nrows)
positions[1] <- 0
# Flip only if two consecutive signals in same direction
positions <- ifelse((re_scaled > quantiles[2]) & (rescaled_lag > quantiles[2]), -1, positions)
positions <- ifelse((re_scaled < quantiles[1]) & (rescaled_lag < quantiles[1]), 1, positions)
positions <- zoo::na.locf(positions)
positions <- rutils::lagit(positions, 2)
pnls <- cumsum(retp*positions)
x11(width=6, height=5)
plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(positions))) / NROW(positions)



###############
### Backtest momentum strategy

# Load packages
library(rutils)

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
returns100[1, ] <- 0
returns100 <- zoo::na.locf(retv100, na.rm=FALSE)
indeks <- cumsum(rowMeans(retv100))

lagg <- 5
sum_roll <- rutils::roll_sum(retv100, look_back=lagg)
weightv <- matrixStats::rowRanks(sum_roll)
weightv <- (weights - 50)
weightv <- rutils::lagit(weightv, lagg=1)

wealth <- -weights*returns100
wealth <- -weights*rutils::lagit(sum_roll, lagg=(-lagg))
wealth <- cumsum(rowMeans(wealth))
x11()
plot(wealth, t="l")
wealth <- xts::xts(wealth, index(retv100))
dygraphs::dygraph(wealth)

# Combine index with AAPL
wealth <- cbind(wealth, indeks)
# wealth <- xts(wealth, index(retv100))
colnamev <- c("Strategy", "Index")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)




# Define backtest functional for daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(retp, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  varv <- roll::roll_var(retp, width=look_back)
  varv <- zoo::na.locf(varv, na.rm=FALSE)
  # varv[is.na(varv)] <- 1
  varv[varv <= 0] <- 1
  # Calculate rolling Sharpe
  past <- roll::roll_mean(retp, width=look_back)
  past[1:look_back, ] <- 1
  weightv <- past/sqrt(varv)
  # weightv <- ifelse(varv > 0, past/sqrt(varv), 0)
  # weightv[varv == 0] <- 0
  weightv[1:look_back, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0
  weightv <- rutils::lagit(weightv, 2)
  # Calculate momentum profits and losses
  future <- rutils::lagit(past, (-look_back))
  pnls <- tre_nd*rowMeans(weights*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  cumsum(pnls - costs)
}  # end btmomdaily


wealth <- btmomdaily(retp=retv100, look_back=5, bid_offer=0, tre_nd=(-1))

# Combine index with AAPL
wealth <- cbind(wealth, indeks)
wealth <- xts(wealth, index(retv100))
colnamev <- c("Strategy", "Index")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="Momentum S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)



###############
### Relationship between liquidity (turnover) and linear dependence (correlation).

# Do illiquid stocks with a lower dollar turnover have significant autocorrelation?

# The answer is that there's no significant relationship between liquidity (turnover) 
# and linear dependence (correlation).

# Load packages
library(rutils)

load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

startd <- "2000-01-01"
lagg <- 25
datav <- lapply(sp500env, function(ohlc) {
  if (start(ohlc) < startd) {
    prices <- quantmod::Cl(ohlc)
    volumes <- quantmod::Vo(ohlc)
    retp <- rutils::diffit(log(prices))
    # Calculate autocorrelations from PACF
    p_acf <- pacf(na.omit(retp), plot=FALSE)
    c(turnover=sum(volumes*prices), dependence=sum(p_acf$acf))
    # Calculate autocorrelations from Hurst
    # endd <- rutils::calc_endpoints(ohlc, lagg)
    # c(turnover=sum(volumes*prices), dependence=calc_hurst_hilo(Hi(ohlc), Lo(ohlc), endd))
  } else NULL
})  # end lapply

# Bind and sort
datav <- do.call(rbind, datav)
datav <- datav[order(datav[, "dependence"], decreasing=FALSE), ]

# Plot
x11()
hist(datav)
plot(datav)
chart_Series(quantmod::Cl(sp500env$TYL))
dygraphs::dygraph(quantmod::Cl(sp500env$TYL))



###############
### Test autoregressive strategy for all ETFs in rutils::etfenv

library(rutils)
ordern <- 3
look_back <- 5

back_test <- function(symbol) {
  prices <- log(quantmod::Cl(get(symbol, rutils::etfenv)))
  retp <- rutils::diffit(prices)
  retp <- as.numeric(retp)
  nrows <- NROW(retp)
  insample <- 1:(nrows %/% 2)
  out_of_sample <- (nrows %/% 2 + 1):nrows
  predv <- rutils::roll_sum(retp, look_back=look_back)
  respv <- rutils::lagit(predictor, lagg=(-look_back))
  predv <- sapply(1+look_back*(0:ordern), rutils::lagit, input=predv)
  predv <- cbind(rep(1, nrows), predv)
  colnames(predv) <- paste0("pred_", 1:NCOL(predv))
  invmat <- MASS::ginv(predictor[insample, 1:ordern])
  coeff <- drop(invmat %*% response[insample])
  forecasts <- drop(predictor[out_of_sample, 1:ordern] %*% coeff)
  sign(forecasts)*retp[out_of_sample]
}  # end back_test


test_s <- lapply(rutils::etfenv$symbolv, back_test)
names(test_s) <- rutils::etfenv$symbolv
sort(sapply(test_s, sum))

forecasts <- test_s[[1]]
pnls <- xts(cumsum(sign(forecasts)*retp[out_of_sample]), index(prices[out_of_sample]))



###############
### Variance ratios

# Find stocks with largest variance ratios
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
symbolv <- colnames(retp)
nweightv <- NROW(symbolv)
lagg <- 25
varatios <- sapply(retp, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 500)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
varatios <- sort(unlist(varatios), decreasing=TRUE)

# Find ETFs with largest variance ratios
retp <- rutils::etfenv$returns
symbolv <- colnames(retp)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "IEF"))]
retp <- retp[, symbolv]
varatios <- sapply(retp, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
varatios <- sort(unlist(varatios), decreasing=TRUE)
# symbolv <- names(varatios)

# Find PCAs with largest variance ratios
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
pcad <- prcomp(retp, scale=TRUE)
pcarets <- xts(pcad$x/100, order.by=index(retp))
varatios <- sapply(pcarets, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
varatios <- sort(unlist(varatios), decreasing=TRUE)
pcarets <- pcarets[, names(varatios)]
save(pcarets, file="/Volumes/external/Develop/data/pcarets.RData")
x11()
dygraphs::dygraph(cumsum(pcarets[, "PC2"]))
barplot(sort(pcad$rotation[, "PC2"]))

# Second PCA
retsc <- cumsum(pcarets)
endd <- rutils::calc_endpoints(pcarets, interval=5)
retsc <- retsc[endd, ]
retsc <- rutils::diffit(retsc)
pcad <- prcomp(retsc, scale=TRUE)
pcarets <- xts(pcad$x/100, order.by=index(retsc))
varatios <- sapply(pcarets, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
varatios <- sort(unlist(varatios), decreasing=TRUE)


## optim
objfun <- function(weightv, returns, lagg) {
  retp <- (retp %*% weightv)
  -calc_var(retp, lagg)/calc_var(retp)/lagg
}  # end objfun

symbolv <- colnames(retp)
nweightv <- NROW(symbolv)
optimd <- optim(par=rep(1/nweightv, nweightv),
                fn=objfun,
                retp=retp,
                lagg=lagg,
                method="L-BFGS-B",
                upper=rep(10, nweightv),
                lower=rep(-10, nweightv))
# Optimal parameters
weightv <- optimd$par
# weightv <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weightv)
names(weightv) <- colnames(retp)
objfun(weightv, returns, lagg)
optimd$value
pnls <- cumsum(retp %*% weightv)
pnls <- xts::xts(pnls, zoo::index(retp))
dygraphs::dygraph(pnls)


# DEoptim
optimd <- DEoptim::DEoptim(objfun,
                           retp=retp,
                           lagg=lagg,
                           upper=rep(10, nweightv),
                           lower=rep(-10, nweightv),
                           control=list(trace=FALSE, itermax=500))

# Extract optimal parameters into weights vector
weightv <- optimd$optim$bestmem
# weightv <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weightv)
names(weightv) <- colnames(retp)
objfun(weightv, returns, lagg)
pnls <- cumsum(retp %*% weightv)
pnls <- xts::xts(pnls, zoo::index(retp))
dygraphs::dygraph(pnls)



###############
### Hurst stuff

lagg <- 25
ohlc <- rutils::etfenv$VTI
endd <- rutils::calc_endpoints(ohlc, lagg)
highp <- Hi(ohlc)
lowp <- Lo(ohlc)
calc_hurst_hilo(highp, lowp, endd)

# Find ETFs with largest Hurst
hurstv <- sapply(rutils::etfenv$symbolv, function(symbol) {
  ohlc <- get(symbol, rutils::etfenv)
  endd <- rutils::calc_endpoints(ohlc, lagg)
  highp <- Hi(ohlc)
  lowp <- Lo(ohlc)
  calc_hurst_hilo(highp, lowp, endd)
})  # end eapply
hurstv <- sort(hurstv, decreasing=TRUE)

plot(hurstv, varatios)
text(x=hurstv, y=varatios, labels=names(varatios))


# Calculate Hurst from returns
endd <- rutils::calc_endpoints(retp, lagg)
hurstv <- sapply(retp, calc_hurst_rets, endd)
hurstv <- sort(hurstv, decreasing=TRUE)

# Find stocks with largest Hurst
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
startd <- "2000-01-01"
hurstv <- eapply(sp500env, function(ohlc) {
  # ohlc <- get(symbol, rutils::etfenv)
  # Check if data starts before 2000
  if (start(ohlc) < startd) {
    ohlc <- ohlc[paste0("/", startd)]
    endd <- rutils::calc_endpoints(ohlc, lagg)
    highp <- Hi(ohlc)
    lowp <- Lo(ohlc)
    calc_hurst_hilo(highp, lowp, endd)
  } else NULL
})  # end eapply
hurstv <- sort(unlist(hurstv), decreasing=TRUE)
symbolv <- names(hurstv[1:100])
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- retp[, symbolv]
save(retp, file="/Users/jerzy/Develop/lecture_slides/data/sp100_rets_hurst.RData")



## Find portfolio with largest Hurst

# Vector of initial portfolio weights
weightv <- rep(1/nweightv, nweightv)

# objfun with shrinkage
objfun <- function(weightv, returns, endd) {
  -calc_hurst_rets(retp %*% weightv, endd)
}  # end objfun


# Portfolio optimization using principal components
# Perform PCA
pcad <- prcomp(retp, center=TRUE, scale=TRUE)
# eigend <- eigen(cor(retp))
# all.equal(abs(pcad$rotation), abs(eigend$vectors), check.attributes=FALSE)
# Calculate principal component time series
rets_pca <- scale(retp) %*% pcad$rotation
# all.equal(pcad$x, rets_pca, check.attributes=FALSE)
round(cor(rets_pca), 4)
# Calculate the returns from the principal component
# time series rets_pca:
rot_inv <- solve(pcad$rotation)
solved <- rets_pca %*% rot_inv

hurst_pca <- apply(rets_pca, 2, calc_hurst_rets, endd=endd)
sort(hurst_pca, decreasing=TRUE)

optimd <- optim(par=rep(1/nweightv, nweightv),
                fn=objfun,
                retp=rets_pca,
                endd=endd,
                method="L-BFGS-B",
                upper=rep(10, nweightv),
                lower=rep(-10, nweightv))
# Optimal parameters
weightv <- optimd$par
weightv <- 0.01*weightv/sd(rets_pca %*% weightv)
# weightv <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weightv)
names(weightv) <- colnames(rets_pca)
objfun(weightv, rets_pca, endd)
optimd$value


# Find portfolio with largest Hurst
objfun <- function(weightv, returns, endd) {
  -calc_hurst_rets(retp %*% weightv, endd)
  + (sum(weightv^2) - 1)^2
}  # end objfun
# library(parallel)
# num_cores <- detectCores()
# cluster <- makeCluster(num_cores-1)
# clusterExport(cluster, varlist=c("calc_hurst_rets"))
optimd <- DEoptim::DEoptim(objfun,
                           retp=rets_pca,
                           endd=endd,
                           upper=rep(10, nweightv),
                           lower=rep(-10, nweightv),
                           # cluster=cluster,
                           # parVar=c("calc_hurst_rets"),
                           control=list(trace=FALSE, itermax=500, parVar=c("calc_hurst_rets"), parallelType=1))
# Stop R processes over cluster under Windows
# stopCluster(cluster)

# Extract optimal parameters into weights vector
weightv <- optimd$optim$bestmem
weightv <- 0.01*weightv/sd(rets_pca %*% weightv)
# weightv <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weightv)
names(weightv) <- colnames(rets_pca)
objfun(weightv, rets_pca, endd)


# Plot wealth
portf_hurst <- drop(rets_pca %*% weightv)
calc_hurst_rets(portf_hurst, endd)
portf_hurst <- xts::xts(portf_hurst, index(retp))
colnames(portf_hurst) <- "max_hurst_deopt"
wealth <- cumsum(portf_hurst)
# wealth <- cumprod(1 + portf_hurst)
datav <- cbind(Cl(rutils::etfenv$VEU)[index(retp)], wealth)
colnames(datav)[1] <- "VEU"
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red")



## Calculate autocorrelations
retp <- rutils::etfenv$returns
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
returns_lag <- rutils::lagit(retp)
auto_cor <- sapply(colnames(retp), function(symbol) {
  retp <- retp[, symbol]
  mean(retp*returns_lag[, symbol])/var(retp)
})  # end sapply
auto_cor <- sort(auto_cor, decreasing=TRUE)


## Find portfolio with largest autocorrelations

# Vector of initial portfolio weights
retp <- rutils::etfenv$returns
symbolv <- colnames(retp)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "IEF"))]
retp <- retp[, symbolv]
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
returns_lag <- rutils::lagit(retp)

nweightv <- NCOL(retp)
weightv <- rep(1/nweightv, nweightv)

# objfun with shrinkage
objfun <- function(weightv, returns, returns_lag) {
  retp <- returns %*% weightv
  -drop(mean(retp*(retv_lag %*% weightv))/var(retp))
}  # end objfun
objfun(weightv, returns, returns_lag)


optimd <- optim(par=rep(1/nweightv, nweightv),
                fn=objfun,
                retp=retp,
                returns_lag=retv_lag,
                method="L-BFGS-B",
                upper=rep(10, nweightv),
                lower=rep(-10, nweightv))
# Optimal parameters
weightv <- optimd$par
names(weightv) <- colnames(retp)
weightv <- sort(weightv, decreasing=TRUE)
objfun(weightv, returns, returns_lag)
optimd$value
# wippp
pnls <- xts::xts(cumsum(retp %*% weightv), zoo::index(retp))
dygraphs::dygraph(pnls)

# DEoptim
optimd <- DEoptim::DEoptim(objfun,
                           retp=retp,
                           returns_lag=retv_lag,
                           upper=rep(10, nweightv),
                           lower=rep(-10, nweightv),
                           # cluster=cluster,
                           # parVar=c("calc_hurst_rets"),
                           control=list(trace=FALSE, itermax=500, parallelType=1))
# Extract optimal parameters into weights vector
weightv <- optimd$optim$bestmem
weightv <- 0.01*weightv/sd(rets_pca %*% weightv)
# weightv <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weightv)
names(weightv) <- colnames(retp)
sort(weightv, decreasing=TRUE)
objfun(weightv, returns, returns_lag)
pnls <- xts::xts(cumsum(retp %*% weightv), zoo::index(retp))
dygraphs::dygraph(pnls)


# Plot wealth
portf_hurst <- drop(rets_pca %*% weightv)
calc_hurst_rets(portf_hurst, endd)
portf_hurst <- xts::xts(portf_hurst, index(retp))
colnames(portf_hurst) <- "max_hurst_deopt"
wealth <- cumsum(portf_hurst)
# wealth <- cumprod(1 + portf_hurst)
datav <- cbind(Cl(rutils::etfenv$VEU)[index(retp)], wealth)
colnames(datav)[1] <- "VEU"
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red")




# More stuff below

datav <- exp(cumsum(rnorm(1e7)/100))
datev <- seq.POSIXt(from=Sys.time(), by="sec", length.out=NROW(datav))
datav <- xts(datav, datev)

aggv <- seq.int(from=1e2, to=1e3, by=1e2)
volv <- sapply(aggv, function(interval) {
  # spy_agg <- rutils::to_period(ohlc=datav, k=interval)
  # HighFreq::calc_var_ohlc(spy_agg)
  endd <- rutils::calc_endpoints(datav, interval=interval)
  sd(rutils::diffit(log(datav[endd])))
})  # end sapply

aggv <- c("seconds", "minutes", "hours", "days")
agglog <- log(c(1, 60, 3600, 6.5*3600))
agglog <- log(c(1, 60, 3600, 24*3600))
volv <- sapply(aggv, function(interval) {
  spy_agg <- rutils::to_period(ohlc=HighFreq::SPY, k=interval)
  # spy_agg <- rutils::to_period(ohlc=HighFreq::SPY, period=interval)
  # HighFreq::calc_var_ohlc(spy_agg)
  retp <- rutils::diffit(spy_agg[, 4])
  sd(retp)
})  # end sapply


names(volv) <- paste0("agg_", aggv)
volog <- log(volv)
agglog <- log(aggv)
agglog <- agglog - mean(agglog)
volog <- volog - mean(volog)
model <- lm(volog ~ agglog)
hurstlm <- summary(model)$coeff[2, 1]
hurstfun <- sum(volog*agglog)/sum(agglog^2)
all.equal(hurstlm, hurstfun)



###############
### Backtests

# Define backtest functional
backtest_rolling <- function(retp, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  varv <- HighFreq::roll_var(retv_weighted, look_back=look_back)
  varv <- zoo::na.locf(varv, na.rm=FALSE)
  varv[is.na(varv)] <- 0
  # Calculate rolling Sharpe
  past <- roll::roll_mean(retp, width=look_back)
  weightv <- past/sqrt(varv)
  weightv[varv == 0] <- 0
  weightv[1:look_back, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0
  weightv <- rutils::lagit(weightv)
  # Calculate momentum profits and losses
  pnls <- tre_nd*rowMeans(weights*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  cumprod(1 + pnls - costs)
}  # end backtest_rolling


# Define backtest functional
backtest_weighted <- function(retp, returns_weighted,
                              look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  varv <- HighFreq::roll_var(retv_weighted, look_back=look_back)
  varv <- zoo::na.locf(varv, na.rm=FALSE)
  varv[is.na(varv)] <- 0
  # Calculate rolling Sharpe
  past <- roll::roll_mean(retv_weighted, width=look_back)
  weightv <- past/sqrt(varv)
  weightv[varv == 0] <- 0
  weightv[1:look_back, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0
  weightv <- rutils::lagit(weightv)
  # Calculate momentum profits and losses
  pnls <- tre_nd*rowMeans(weights*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weightv)))
  cumprod(1 + pnls - costs)
}  # end backtest_weighted


# Define backtest functional
backtestmomentum <- function(retp, returns_weighted,
                              objfunc=function(retp) (sum(retp)/sd(retp)),
                              look_back=12, re_balance="months", bid_offer=0.001,
                              endd=rutils::calc_endpoints(retp, interval=re_balance),
                              with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
  nrows <- NROW(endd)
  startp <- c(rep_len(1, look_back-1), endd[1:(nrows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(startp, endd)
  # Calculate look-forward intervals
  look_fwds <- cbind(endd + 1, rutils::lagit(endd, -1))
  look_fwds[nrows, 1] <- endd[nrows]
  # Calculate past performance over look-back intervals
  past <- t(apply(look_backs, 1, function(ep) sapply(retv_weighted[ep[1]:ep[2]], objfunc)))
  past[is.na(past)] <- 0
  # Calculate future performance
  future <- t(apply(look_fwds, 1, function(ep) sapply(retp[ep[1]:ep[2]], sum)))
  future[is.na(future)] <- 0
  # Scale weights so sum of squares is equal to 1
  weightv <- past
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnls <- rowSums(weights*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*cumprod(1 + pnls)*rowSums(abs(rutils::diffit(weightv)))
  pnls <- (pnls - costs)
  if (with_weightv)
    rutils::lagit(cbind(pnls, weightv))
  else
    rutils::lagit(pnls)
}  # end backtestmomentum


# Perform sapply loop over look_backs
endd <- rutils::calc_endpoints(retp, interval="weeks")
look_backs <- seq(3, 15, by=1)
objfunc <- function(retp) sum(retp)/sd(retp)
profilevs <- sapply(look_backs, function(look_back) {
  pnls <- backtestmomentum(retp=retp, returns_weighted=retv_weighted,
                             endd=endd,
                             look_back=look_back, objfunc=objfunc)
  last(cumprod(1 + pnls))
})  # end sapply
x11(width=6, height=4)
plot(x=look_backs, y=profilevs, t="l",
     main="Strategy PnL as function of look_back",
     xlab="look_back (months)", ylab="pnl")

datev <- index(retp[endd])
weightsaw <- c(0.30, 0.55, 0.15)
retsaw <- returns %*% weightvaw
wealthaw <- cumprod(1 + retsaw)
wealthaw <- xts::xts(wealthaw[endd], datev)

look_back <- look_backs[which.max(profilevs)]
pnls <- backtestmomentum(retp=retp, returns_weighted=retv_weighted,
                           look_back=look_back, endd=endd,
                           objfunc=objfunc, with_weights=TRUE)
tail(pnls)
retsmom <- as.numeric(pnls[, 1])
wealth <- cumprod(1 + retsmom)

datav <- cbind(wealth, wealthaw)
colnames(datav) <- c("Momentum Strategy", "All_weather")

dygraphs::dygraph(datav, main="Momentum Strategy") %>%
  dyAxis("y", label="All_weather", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="All_weather", axis="y", label="All_weather", strokeWidth=2, col="blue")


# Plot multiple wealth curves
# Perform sapply loop over look_backs
wealth <- sapply(look_backs, backtestmomentum,
                  retp=retp,
                  returns_weighted=retv_weighted,
                  endd=endd,
                  objfunc=objfunc)
wealth <- apply(wealth, 2, function(x) cumprod(1 + x))
colnames(wealth) <- paste0("look_back=", look_backs)
wealth <- xts(wealth, datev)
tail(wealth)

plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(wealth))
chart_Series(wealth,
             theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(wealth),
       inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(retp)),
       col=plot_theme$col$line.col, bty="n")



###############
### VXX and SVXY

# retp <- -etfenv$returns$VXX
retp <- na.omit(etfenv$returns$SVXY)
retp <- cbind(retp, -na.omit(etfenv$returns$VXX))
which_na <- which(is.na(returns$VXX))
returns$VXX[which_na] <- returns$SVXY[which_na]
retp <- cumprod(1+rowMeans(retp))
sum(is.na(retp))
head(retp)
volumes <- cbind(quantmod::Vo(etfenv$SVXY), quantmod::Vo(etfenv$VXX))
volumes$VXX.Volume[which_na] <- volumes$SVXY.Volume[which_na]
volumes <- rowMeans(volumes)

roll_vwap <- rutils::roll_sum(xtsv=retp*volumes, look_back=look_back)
volume_rolling <- rutils::roll_sum(xtsv=volumes, look_back=look_back)
roll_vwap <- roll_vwap/volume_rolling
roll_vwap[is.na(roll_vwap)] <- 0
roll_vwap

plot(retp, t="l", lwd=2)
lines(roll_vwap, col="red", lwd=2)



###############
### Wilcoxon tests

# Data
sample1 <- rnorm(200)
sample2 <- rnorm(100, mean=0.1)
# Mann-Whitney-Wilcoxon rank sum test
wilcoxt <- wilcox.test(sample1, sample2, paired=FALSE)
wilcoxt$statistic
# Calculate U statistic of Mann-Whitney-Wilcoxon test
datav <- c(sample1, sample2)
ranks <- rank(datav)
sum(ranks[1:200]) - 100*201
sum(ranks[201:300]) - 50*101

# Data
sample1 <- rnorm(100)
sample2 <- rnorm(100, mean=0.1)
# Wilcoxon signed rank test
wilcoxt <- wilcox.test(sample1, sample2, paired=TRUE)
wilcoxt$statistic
# Calculate V statistic of Wilcoxon test
datav <- (sample1 - sample2)
sum(rank(abs(datav))[datav>0])


###############
### C:/Develop/predictive/data

library(rutils)
datav <- read.zoo(file="C:/Develop/predictive/data/predictions_long_account.csv", header=TRUE, sep=",")
datav <- as.xts(datav)
datev <- index(datav)
colnamev <- colnames(datav)
datav <- lapply(datav, as.numeric)
datav <- rutils::do_call(cbind, datav)
datav <- xts(datav, datev)
colnames(datav) <- colnamev

core_data <- datav[, 8:9]
colnames(core_data) <- c("actual", "predicted")
core_data <- core_data["2020-01-10/"]
core_data <- na.omit(core_data)
datev <- index(core_data)
datev <- as.Date(datev)
datev <- unique(datev)
datev <- as.Date(datev)
foo <- sapply(datev, function(dat_e) {
  foo <- core_data[(datev == dat_e), ]
  sum(foo[foo[, 2] > 0.15, 1])
})  # end sapply
names(foo) <- datev


tail(core_data, 33)
dim(core_data)
head(core_data, 33)
core_data <- coredata(core_data)
core_data <- cbind(core_data[, 2, drop=FALSE], core_data[, 1, drop=FALSE])
plot(core_data)

ma_x <- max(core_data[, 1])
sapply((1:8)*ma_x/10, function(x) {
  sum(core_data[core_data[, 1]>x, 2])
})  # end sapply
sum(core_data[core_data[, 1]>0.15, 2])


foo <- datav["2020-01-10/"]
foo <- na.omit(foo)
sum(foo[, 8])
sum(foo[foo[, 9]>0.15, 8])




###############
### Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/test.cpp")


indeks <- rutils::etfenv$returns[, "VTI", drop=FALSE]
indeks <- na.omit(indeks)
datev <- index(indeks)

retp <- rutils::etfenv$returns[, "XLF", drop=FALSE]
retp <- na.omit(retp)
retp <- retp[datev]
calc_alpha(retp, indeks, typev="wilcoxon")

symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[-grep("VTI", symbolv, ignore.case=TRUE)]

foo <- sapply(symbolv, function(symbol) {
  cat(symbol, "\n")
  retp <- rutils::etfenv$returns[, symbol, drop=FALSE]
  retp <- na.omit(retp)
  retp <- retp[datev]
  datev <- datev[index(retp)]
  calc_alpha(retp, datev, typev="wilcoxon")
})  # end sapply




###############
### Scale minutely returns by the volume to make them closer to normal or stationary.
# Use trading time (volume clock)

library(HighFreq)
ohlc <- HighFreq::SPY
prices <- drop(coredata(quantmod::Cl(ohlc)))
volumes <- drop(coredata(quantmod::Vo(ohlc)))
retp <- rutils::diffit(log(prices))
retp <- returns/sd(retp)
prices <- cumsum(retp)

# Scale the volume by the rolling average volume
look_back <- 111
volume_rolling <- rutils::roll_sum(xtsv=volumes, look_back=look_back)
volumes <- look_back*volumes/volume_rolling

## wippp
## Add to homeworks
## Statistics for different volume scaling exponents.
# Dividing by the square root of the volume works better
# than dividing by the volume itself.
statis_tic <- sapply((1:20)/20, function(ex_po) {
  rets_scaled <- ifelse(volumes > 0, returns/(volumes^ex_po), 0)
  rets_scaled <- rets_scaled/sd(rets_scaled)
  # Calculate moments and perform JB normality tests
  # tseries::jarque.bera.test(rets_scaled)$statistic
  # moments::moment(rets_scaled, order=4)
  # Calculate autocorrelations from PACF
  pa_cf <- pacf(as.numeric(rets_scaled), lag=10, plot=FALSE)
  sum(pa_cf$acf)
  # Standard deviation of square returns is proxy for kurtosis and stationarity
  # sd(rets_scaled^2)
  # calc_hurst(rets_scaled, endd)
})  # end sapply
x11(width=6, height=5)
plot(statis_tic, t="l")


# Divide returns by the volume (volume clock).
rets_scaled <- ifelse(volumes > 0, returns/volumes, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
madv <- mad(rets_scaled)

# Plot densities of the returns
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(density(retp), xlim=5*c(-madv, madv), 
     lwd=3, mgp=c(2, 1, 0), col="blue", 
     xlab="returns (standardized)", ylab="frequency", 
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(rets_scaled, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
       leg=c("minutely", "scaled", "normal"),
       lwd=6, lty=1, col=c("blue", "red", "green"))

# Plot the cumulative scaled returns
prices_scaled <- cumsum(rets_scaled)
prices_scaled <- xts(prices_scaled, index(ohlc))
dygraphs::dygraph(prices_scaled[60*(1:(NROW(prices_scaled) %/% 60))], main="SPY Prices")


# Plot the cumulative scaled returns with close prices
datav <- cbind(prices, prices_scaled)
colnamev <- c("SPY Prices", "Scaled by Volume")
# datav <- xts::to.hourly(datav)
colnames(datav) <- colnamev
dygraphs::dygraph(datav[60*(1:(NROW(datav) %/% 60))], main="SPY Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)


## Calculate Hurst

# Define a single aggregation interval.
interval <- 35^2
agglog <- log(interval)
# Calculate index of end points spaced apart by interval.
nrows <- NROW(prices)
nagg <- nrows %/% interval
endd <- c(0, nrows - nagg*interval + (0:nagg)*interval)

# Calculate Hurst from single data point
calc_hurst <- function(retp, endd) {
  prices <- cumsum(retp)
  r_s <- sapply(2:NROW(endd), function(ep) {
    indeks <- endd[ep-1]:endd[ep]
    diff(range(prices[indeks]))/sd(retp[indeks])
  })  # end sapply
  log(mean(r_s))/agglog
}  # end calc_hurst

calc_hurst(retp, endd)



###############
### Scale minutely returns by the price range to make them closer to normal or stationary

library(HighFreq)

# symbol <- "SPY"
ohlc <- HighFreq::SPY

## Calculate price range
rangev <- log(drop(coredata(Hi(ohlc)/Lo(ohlc))))
# Around 1.8% of bars have zero range
sum(rangev==0)/NROW(rangev)
# Remove bars with zero range
# ohlc <- ohlc[!(rangev==0)]
# Remove bars with zero returns
retp <- rutils::diffit(log(drop(coredata(Cl(ohlc)))))
# zero_rets <- (retp==0)
# ohlc <- ohlc[!zero_rets]

datev <- index(ohlc)
nrows <- NROW(ohlc)
endd <- xts::endpoints(ohlc, on="hours")
closep <- Cl(ohlc)[endd]
rangev <- log(drop(coredata(Hi(ohlc)/Lo(ohlc))))
rangev <- (rangev + c(0, rangev[-NROW(rangev)]))/2
retp <- rutils::diffit(log(drop(coredata(Cl(ohlc)))))
# retp <- c(0, returns)
sum(is.na(retp))
sum(is.infinite(retp))

## Distribution of raw returns is bimodal
x11()
# Standardize raw returns to make later comparisons
retp <- returns/sd(retp)
madv <- mad(retp)
range(retp)
# Standard deviation of square returns is proxy for kurtosis and stationarity
sd((retp/sd(retp))^2)
hist(retp, breaks=1111, xlim=c(-3, 3), freq=FALSE)
lines(density(retp, bw=0.2), col='red', lwd=2)

## Calculate moments and perform JB normality tests
sapply(1:4, moments::moment, x=retp)
tseries::jarque.bera.test(retp)
shapiro.test(retp)


# Regress volume on range
datav <- cbind(volume=log(volumes), range=log(rangev))
# datav[!is.finite(datav)] <- NA
datav <- na.omit(is.finite(datav)*datav)
# foo <- lm(paste(colnames(datav), collapse=" ~ "), data=as.data.frame(datav))
foo <- lm(volume ~ range, as.data.frame(datav))
plot(volume ~ range, as.data.frame(datav[samplev, ]))
abline(foo, lwd=3, col="red")


## Apply Manly transformation to make returns more normal
lambda <- 1.5

# Modulus
# Divide returns by square root of volume (volume clock)


## Calculate moments and perform JB normality test
sum(is.na(rets_scaled))
sum(is.infinite(rets_scaled))
range(rets_scaled)
sapply(1:4, moments::moment, x=rets_scaled)
tseries::jarque.bera.test(rets_scaled)
x11()
madv <- mad(rets_scaled)
hist(rets_scaled, breaks=11111, xlim=10*c(-madv, madv), freq=FALSE)
pacf(rets_scaled)


## Scale returns using price range
rets_scaled <- ifelse(rangev>0, returns/rangev, 0)
# rets_scaled <- returns/rangev
rets_scaled <- rets_scaled/sd(rets_scaled)
madv <- mad(rets_scaled)
hist(rets_scaled, breaks=1111, xlim=c(-3, 3), freq=FALSE)
lines(density(rets_scaled, bw=0.2), col='blue', lwd=2)

# JB statistic for different range scaling exponents
statis_tic <- sapply((1:6)/2, function(ex_po) {
  rets_scaled <- ifelse(rangev>0, returns/(rangev^ex_po), 0)
  rets_scaled <- rets_scaled/sd(rets_scaled)
  tseries::jarque.bera.test(rets_scaled)$statistic
})  # end sapply


# Stationarity statistic for different scaling exponents
statis_tic <- sapply((1:20)/10, function(ex_po) {
  # rets_scaled <- ifelse(volumes>0, returns/(volumes^x), 0)
  rets_scaled <- ifelse(rangev>0, returns/(rangev^ex_po), 0)
  # rets_scaled <- returns/(rangev^ex_po)
  # Remove zero returns
  # rets_scaled <- rets_scaled[!(rets_scaled==0)]
  # rets_scaled <- rets_scaled/sd(rets_scaled)
  # Standard deviation of square returns is proxy for kurtosis and stationarity
  sd((rets_scaled/sd(rets_scaled))^2)
})  # end sapply



###############
### Fractional differencing




############### homework
# Summary: Strategy using weekly and monthly stock returns.
# It's implemented in app_roll_portf9.R

library(HighFreq)

# Aggregate the VTI returns to monthly and run pacf()

closep <- Cl(rutils::etfenv$VTI)
retp <- rutils::diffit(log(closep))
returns_adv <- rutils::lagit(retp, lagg=(-1))
returns_adv <- as.numeric(retv_adv)

# endd <- rutils::calc_endpoints(closep, interval="weeks")
# week_ly <- closep[endd]
# week_ly <- rutils::diffit(log(week_ly))
week_ly <- rutils::diffit(log(closep), lagg=5)
week_ly <- as.numeric(week_ly)
returns_adv <- rutils::lagit(week_ly, lagg=(-1))
returns_adv <- as.numeric(retv_adv)
# endd <- rutils::calc_endpoints(closep, interval="months")
# month_ly <- closep[endd]
# month_ly <- rutils::diffit(log(month_ly))
month_ly <- rutils::diffit(log(closep), lagg=25)
month_ly <- as.numeric(month_ly)
returns_adv <- rutils::lagit(month_ly, lagg=(-1))
returns_adv <- as.numeric(retv_adv)

# Objective function for simple optimization
objfun <- function(wei_ght) {
  # weightv <- c(wei_ght, 1-wei_ght)
  sum((retv_adv - (wei_ght*week_ly + (1-wei_ght)*month_ly))^2)
}  # end objfun
objfun(0.5)
foo <- optimize(f=objfun, interval=c(-10, 10))
unlist(foo)
wei_ght <- unlist(foo)[1]
positions <- sign(wei_ght*week_ly + (1-wei_ght)*month_ly)
positions_lag <- rutils::lagit(positions, lagg=2)
pnls <- cumsum(positions_lag*returns)
x11()
end_days <- rutils::calc_endpoints(retp, "days")
plot.zoo(-pnls[end_days], main="pnls", xlab=NA, ylab=NA)




############### homework
# Summary: Create a contrarian strategy using
# the Hampel filter.
# It's implemented in app_roll_portf10.R

library(rutils)


# 1. (10pts)
# Define a rolling look-back window and a half window:
# Use the %/% operator.

look_back <- 11
half_window <- look_back %/% 2

# Calculate a time series of rolling z-scores
# (called zscores), using the Hampel filter code
# from the lecture slides.

prices <- Cl(HighFreq::SPY)["T09:31:00/T15:59:00"]
retp <- rutils::diffit(log(prices))

medi_an <- TTR::runMedian(prices, n=look_back)
medi_an[1:look_back, ] <- 1
sum(is.na(medi_an))
madv <- TTR::runMAD(prices, n=look_back)
madv[1:look_back, ] <- 1
sum(is.na(madv))
zscores <- ifelse(madv!=0, (prices-medi_an)/madv, 0)
zscores[1:look_back, ] <- 0
madv <- zoo::na.locf(zscores)
sum(is.na(zscores))
mad_zscores <- TTR::runMAD(zscores, n=look_back)
mad_zscores[1:look_back, ] <- 0

# You should get the following output:
tail(zscores)
mad(zscores)
range(zscores)
hist(zscores, breaks=30, xlim=c(-10, 10), freq=FALSE)

# Calculate positions and pnls from z-scores
positions <- rep(NA_integer_, NROW(prices))
positions[1] <- 0
# threshold <- 3*mad(zscores)
# positions <- ifelse(zscores > threshold, -1, positions)
# positions <- ifelse(zscores < (-threshold), 1, positions)
positions <- ifelse(zscores > 2*mad_zscores, -1, positions)
positions <- ifelse(zscores < (-2*mad_zscores), 1, positions)
positions <- zoo::na.locf(positions)
positions_lag <- rutils::lagit(positions, lagg=2)
pnls <- cumsum(positions_lag*returns)
x11()
end_days <- rutils::calc_endpoints(prices, "days")
plot.zoo(pnls[end_days], main="pnls", xlab=NA, ylab=NA)



############### homework - Hurst exponents almost random
# Summary: Calculate a time series of monthly Hurst
# exponents and the volatility for the SPY series.
# Demonstrate that the changes of the Hurst exponent
# have negative autocorrelations, that Hurst exponent
# is anti-persistent over time.
# Calculate the correlation between the SPY Hurst
# exponent and the level of volatility.
#
# Regress the Hurst exponent versus the standard
# deviation, and create plots and perform a regression
# of the two.

# Observation:
# The Hurst exponent is low when the volatility is high.
# When the volatility is low then the Hurst exponent
# can be both high and low.


library(HighFreq)

# This is best version
# Calculate Hurst exponent using median of range ratios
calc_hurst_hilo <- function(highp, lowp, endd) {
  range_ratios <- sapply(seq_along(endd)[-1], function(it) {
    startpoint <- endd[it-1]
    endpoint <- endd[it]
    highp <- highp[startpoint:endpoint]
    lowp <- lowp[startpoint:endpoint]
    log((max(highp) - min(lowp))/mean(highp - lowp))/log(endpoint-startpoint)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_hilo

# Calculate Hurst exponent using median of range ratios
calc_hursto <- function(highp, lowp, endd) {
  range_ratios <- sapply(seq_along(endd)[-1], function(it) {
    highp <- highp[endd[it-1]:endd[it]]
    lowp <- lowp[endd[it-1]:endd[it]]
    (max(highp) - min(lowp))/mean(highp - lowp)
  })  # end sapply
  log(median(na.omit(range_ratios)))/log(median(rutils::diffit(endd)))
}  # end calc_hursto

# Calculate Hurst exponent from returns
calc_hurst_rets <- function(rets, endd) {
  retsc <- cumsum(rets)
  range_ratios <- sapply(seq_along(endd)[-1], function(it) {
    startpoint <- endd[it-1]
    endpoint <- endd[it]
    rets <- rets[startpoint:endpoint]
    retsc <- retsc[startpoint:endpoint]
    log((max(retsc) - min(retsc))/sd(rets))/log(endpoint-startpoint)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_rets


ohlc <- log(HighFreq::SPY)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days"))

library(microbenchmark)
summary(microbenchmark(
  calc_hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")),
  calc_hursto=calc_hursto(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



# 2. (20pts)
# Calculate a vector of monthly end points from
# the ohlc, and call it endd.
# use the function rutils::calc_endpoints().

endd <- rutils::calc_endpoints(ohlc, interval="months")

# Perform an sapply() loop over the length of endd.
# Inside the loop calculate the standard deviation of
# returns and the cumulative trading volumes.
# The output should be a matrix called volat_hurst.

volat_hurst <- sapply(seq_along(endd)[-1],
  function(it) {
    ohlc <- ohlc[endd[it-1]:endd[it]]
    highp <- quantmod::Hi(ohlc)
    lowp <- quantmod::Lo(ohlc)
    c(volatility=HighFreq::calc_var_ohlc(ohlc),
      hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")))
  })  # end sapply
# rbind list into single xts or matrix
volat_hurst <- t(volat_hurst)

x11()
plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diffit(volat_hurst)
plot(foo)
pacf(foo[, 1])


# wippp
############### homework
# Summary: Calculate a time series of annual Hurst
# exponents for S&P500 stocks.
# Plot a scatterplot of Hurst for the years 2008 and 2009.

# Set up the data as follows:
library(HighFreq)

# Load S&P500 constituent stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

ohlc <- log(sp500env$SIG)
quantmod::chart_Series(Cl(ohlc))
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="months"))

endd <- rutils::calc_endpoints(ohlc, interval="years")
volat_hurst <- sapply(seq_along(endd)[-1],
                      function(it) {
                        ohlc <- ohlc[endd[it-1]:endd[it]]
                        highp <- quantmod::Hi(ohlc)
                        lowp <- quantmod::Lo(ohlc)
                        c(volatility=HighFreq::calc_var_ohlc(ohlc),
                          hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="months")))
                      })  # end sapply
# Transpose the matrix
volat_hurst <- t(volat_hurst)

plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diffit(volat_hurst)
plot(foo)
plot(cbind(foo[, 1], volat_hurst[, 2]))
pacf(foo[, 1])

model <- lm(volat_hurst[, 2] ~ volat_hurst[, 1])
model_sum <- summary(model)
model_sum$coefficients[2, 3]
plot(volat_hurst[, 2] ~ volat_hurst[, 1])
abline(model)

###

# Scrub data in sp500env

x[is.infinite(x)] <- NA
x <- zoo::na.locf(x)
x <- zoo::na.locf(x, fromLast=TRUE)


###


hurst_prof <- eapply(sp500env, function(ohlc) {
  endd <- rutils::calc_endpoints(ohlc, interval="years")
  if (NROW(endd) > 3) {
    ohlc <- log(ohlc)
    volat_hurst <- sapply(seq_along(endd)[-1],
                          function(it) {
                            ohlc <- ohlc[endd[it-1]:endd[it]]
                            highp <- quantmod::Hi(ohlc)
                            lowp <- quantmod::Lo(ohlc)
                            c(volatility=HighFreq::calc_var_ohlc(ohlc),
                              hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="months")))
                          })  # end sapply
    # Transpose the matrix
    volat_hurst <- t(volat_hurst)
    # Scrub the data
    volat_hurst <- zoo::na.locf(volat_hurst)
    zoo::na.locf(volat_hurst, fromLast=TRUE)
  } else {
    c("Not enough years for Hurst\n")
    cbind(volatility=rep(1, NROW(endd)), hurst=rep(0.5, NROW(endd)))
  }
})  # end eapply

hurst_prof <- lapply(hurst_prof, function(volat_hurst) {
  # Scrub the data
  volat_hurst[is.infinite(volat_hurst)] <- NA
  volat_hurst <- zoo::na.locf(volat_hurst)
  zoo::na.locf(volat_hurst, fromLast=TRUE)
})  # end lapply

save(hurst_prof, file="/Users/jerzy/Develop/lecture_slides/data/sp500_perf.RData")

bar <- sapply(hurst_prof, function(x) sum(is.na(x) | is.infinite(x)))
max(bar)
which.max(bar)
hurst_prof[[names(which.max(bar))]]

get_tval <- function(x) {
  cat("NROW(x) = ", NROW(x), "\n")
  x <- na.omit(x)
  if ((NROW(x) > 3) & (sum(is.na(x))==0)) {
    model <- lm(x[, 2] ~ x[, 1])
    summary(model)$coefficients[2, 3]
  } else 1
}  # end get_tval
bar <- sapply(hurst_prof, get_tval)

bar <- sapply(hurst_prof, function(x) {
  # cat("dim(x) = ", dim(x), "\n")
  if (NROW(x) > 3) {
    model <- lm(x[, 2] ~ x[, 1])
    summary(model)$coefficients[2, 3]
  } else 0
})  # end sapply

bar <- sort(bar)
hist(bar, freq=FALSE)
which.max(bar)
bar[[names(which.max(bar))]]
hurst_prof[[names(which.max(bar))]]
which.min(bar)
bar[[names(which.min(bar))]]
hurst_prof[[names(which.min(bar))]]
plot(hurst_prof[[names(which.min(bar))]])

bar <- sapply(hurst_prof, function(x) {
  max(x[, 2])
})  # end sapply
retp <- prices[, names(tail(bar, 100))]
retp <- rutils::diffit(log(retp))
save(retp, file="/Users/jerzy/Develop/lecture_slides/data/sp100_rets.RData")

colnamev <- colnames(hurst_prof$AAPL)
bar <- lapply(hurst_prof, function(x) {
  x <- cbind((x[, 1]-min(x[, 1]))/(max(x[, 1]-min(x[, 1]))), (x[, 2]-min(x[, 2]))/(max(x[, 2])-min(x[, 2])))
  colnames(x) <- colnamev
  x
})  # end lapply
foo <- NULL
unlist(sapply(hurst_prof, function(x) {
  foo <<- rbind(foo, x)
  NULL
}))  # end sapply
plot(foo)



###############
### Forecasting for univariate regression

library(HighFreq)
source("C:/Develop/R/scripts/market_making.R")

data_dir <- "/Volumes/external/Develop/data/ib_data/"
symbol <- "ES"
load(paste0(data_dir, symbol, "_ohlc.RData"))
nrows <- NROW(ohlc)
ohlc <- coredata(ohlc)

look_back <- 500
design <- cbind(rep(1, look_back), 1:look_back)
deg_free <- (look_back - NCOL(design))
design_inv <- MASS::ginv(design)
design2 <- MASS::ginv(crossprod(design))
lagg <- 1
oo_s <- cbind(1, look_back + lagg)
oos_t <- t(oo_s)

# influ_ence <- design %*% design_inv
# fit_ted <- drop(influ_ence %*% tseries)

# tseries <- ohlc[(look_back+1):(look_back+100), 4]
# val_ue <- ohlc[201, 4]
# calc_zscore(val_ue, tseries, design, design_inv, design2, oo_s, oos_t, deg_free)

zscores <- sapply((look_back+1):nrows, function(x) {
  tseries <- ohlc[(x-look_back):(x-1), 4]
  val_ue <- ohlc[x, 4]
  calc_zscore(val_ue, tseries, design, design_inv, design2, oo_s, oos_t, deg_free)
})  # end sapply
zscores <- c(rep(0, look_back), zscores)

hist(zscores, breaks=100, freq=FALSE)
quantile(zscores, 0.9)

foo <- which((zscores < quantile(zscores, 0.1)) & (zscores > quantile(zscores, 0.01)))
foo <- which((zscores > quantile(zscores, 0.9)) & (zscores < quantile(zscores, 0.99)))
foo <- foo[(foo>(30)) & (foo<(nrows-100))]
foo <- lapply(foo, function(x) {
  ohlc[(x-30):(x+100), 4]/ohlc[x, 4]
})
foo <- rutils::do_call(cbind, foo)
foo <- rowMeans(foo)
plot(foo, t="l")




###############
### Tests for HighFreq functions

library(rutils)
# detach("package:HighFreq")
library(dygraphs)

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/calc_weights.cpp")


## Tests for sorting and ranking

datav <- round(runif(7), 2)
all.equal(datav, drop(sort_back(datav)))
all.equal(rank(datav), drop(calc_ranks(datav)))
drop(calc_ranks_m(datav))
sum(calc_ranks_m(datav))
datav <- xts::xts(runif(7), seq.Date(Sys.Date(), by=1, length.out=7))
all.equal(rank(drop(coredata(datav))), drop(calc_ranks(datav)))



## Load S&P500 stock returns
# retp <- na.omit(rutils::etfenv$returns[, 1:9])
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- returns100["2000/"]
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
nrows <- NROW(retp)
ncols <- NCOL(retp)


## Tests for HighFreq::calc_weights()

## Calculate ranksharpe weights using R
weightsr <- sapply(retp, function(x) mean(x)/sd(x))
weightsr <- rank(weightsr)
weightsr <- (weightsr - mean(weightsr))

# weightsr <- 0.01*weightv/stddev(retp*weightv)

## Calculate weights using RcppArmadillo
weightv <- drop(calc_weights(retp, model_type="ranksharpe", scale=FALSE))
all.equal(weightv, weightsr, check.attributes=FALSE)


## Calculate max_sharpe weights using R

# Calculate covariance matrix of ETF returns
# retp <- na.omit(rutils::etfenv$returns[, 1:16])
eigend <- eigen(cov(retp))
# Calculate regularized inverse of covariance matrix
dimax <- 3
eigenvec <- eigend$vectors[, 1:dimax]
eigen_val <- eigend$values[1:dimax]
invmat <- eigenvec %*% (t(eigenvec) / eigen_val)
# Define shrinkage intensity and apply shrinkage to the mean returns
alpha <- 0.5
colmeans <- colMeans(retp)
colmeans <- ((1-alpha)*colmeans + alpha*mean(colmeans))

# Calculate the portfolio weights
weightsr <- invmat %*% colmeans
ncols <- NCOL(retp)
# Scale the weights so portfolio is equal to equal weight portfolio
weightsr <- weightsr*sd(retp %*% rep(1/ncols, ncols))/sd(retp %*% weightsr)

## Calculate weights using RcppArmadillo
weightv <- drop(calc_weights(retp, model_type="max_sharpe", alpha=alpha, dimax=3, scale=FALSE))
all.equal(weightv, drop(weightsr), check.attributes=FALSE)



## Calculate returns on equal weight portfolio
indeks <- rowMeans(retp)
stdev <- sd(indeks[indeks<0])
indeks <- xts(indeks, index(retp))

foo <- weight_returns(retp, weightv)
bar <- retp %*% weightv
all.equal(foo, bar)


# Define maximum Sharpe portfolio weights
calc_weightv <- function(retp) {
  # eigend <- eigen(cov(retp))
  # # set tolerance for determining zero eigenvalues
  # precision <- sqrt(.Machine$double.eps)
  # # check for zero eigenvalues
  # notzero <- (eigend$values > (precision * eigend$values[1]))
  # invmat <- eigend$vectors[, notzero] %*% (t(eigend$vectors[, notzero])/eigend$values[notzero])
  # weightv <- invmat %*% apply(retp, 2, mean)
  # weightv/sum(abs(weightv))

  weightv <- sapply(retp[((nrows-500):nrows)], median)
  # weightv <- (order(order(weightv)-1)-1)
  # weightv <- order(order(weightv))
  # weightv <- order(weightv)
  weightv <- (order(weightv)-1)
  # weightv <- order(order(weightv, decreasing=TRUE), decreasing=TRUE)
  # weightv <- ordern[ordern]

} # end calc_weights
weightv <- calc_weights(retspub)


foo <- drop(calc_weights(retp[((nrows-500):nrows)], typev="rankrob", alpha=0, scale=FALSE))


all.equal(weightv, foo)
foo <- cbind(weightv, foo)
head(foo, 11)
tail(foo, 11)


weightv <- colMeans(retp)
weightv <- sapply(retp, function(x) mean(x)/sd(x))
weightv <- sapply(retp, moments::skewness)
weightv <- drop(HighFreq::calc_ranks(weightv))
weightv <- (weights - mean(weightv))
names(weightv) <- colnames(retp)


weightv <- drop(calc_weights(retp, typev="max_sharpe", alpha=0))
pnls <- (retp %*% weightv)
pnls <- xts(cumsum(pnls), order.by=index(retp))
prices <- cumsum(rowMeans(retp))
pnls <- cbind(pnls, prices)
colnames(pnls) <- c("Strategy", "Index")

colnamev <- colnames(pnls)
captiont <- paste("Momentum Strategy for S&P500 Stocks")
dygraphs::dygraph(pnls, main=captiont) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")




###############
### Strategy for market making using limit orders

data_dir <- "/Volumes/external/Develop/data/ib_data"
setwd(dir=data_dir)
load("ohlc.RData")
ohlc_data <- coredata(ohlc)
nrows <- NROW(ohlc_data)

ohlc_lag <- rutils::lagit(ohlc_data)
closep <- ohlc_data[, 4]

buy_spread <- 0.25
sell_spread <- 0.25

# Vectorized version

buy_price <- (ohlc_lag[, 3] - buy_spread)
sell_price <- (ohlc_lag[, 2] + sell_spread)

buy_ind <- (ohlc_data[, 3] < buy_price)
n_buy <- cumsum(buy_ind)
sell_ind <- (ohlc_data[, 2] > sell_price)
n_sell <- cumsum(sell_ind)

buy_s <- numeric(nrows)
buy_s[buy_ind] <- buy_price[buy_ind]
buy_s <- cumsum(buy_s)
sell_s <- numeric(nrows)
sell_s[sell_ind] <- sell_price[sell_ind]
sell_s <- cumsum(sell_s)

pnls <- ((sell_s-buy_s) - closep*(n_sell-n_buy))


# Loop version

buy_price <- numeric(nrows)
sell_price <- numeric(nrows)
n_buy <- numeric(nrows)
n_sell <- numeric(nrows)
buy_s <- numeric(nrows)
sell_s <- numeric(nrows)
pnls <- numeric(nrows)

for (it in 2:nrows) {
  buy_price[it] <- (ohlc_lag[it, 3] - buy_spread)
  sell_price[it] <- (ohlc_lag[it, 2] + sell_spread)

  buy_ind <- (ohlc_data[it, 3] < buy_price[it])
  sell_ind <- (ohlc_data[it, 2] > sell_price[it])
  n_buy[it] <- n_buy[it-1] + buy_ind
  n_sell[it] <- n_sell[it-1] + sell_ind
  buy_s[it] <- buy_s[it-1] + buy_ind*buy_price[it]
  sell_s[it] <- sell_s[it-1] + sell_ind*sell_price[it]
  pnls[it] <- ((sell_s[it] - buy_s[it]) - closep[it]*(n_sell[it] - n_buy[it]))
}  # end for


plot(pnls[c(1, rutils::calc_endpoints(ohlc, interval="minutes"))], t="l", main="Market Making Strategy")




###############
### Strategy using OHLC technical indicators

library(HighFreq)

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/lm_arma.cpp")


# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")


# load OHLC data
# ohlc <- HighFreq::SPY
# ohlc <- HighFreq::SPY["2010-10/2010-11"]
# ohlc <- rutils::etfenv$VTI
# load recent ES1 futures data
# load(file="/Volumes/external/Develop/data/ES1.RData")
# or
# ohlc <- read.zoo(file="/Volumes/external/Develop/data/new_bar/ES1.csv", header=TRUE, sep=",",
#                   drop=FALSE, format="%Y-%m-%d %H:%M",
#                   FUN=as.POSIXct, tz="America/New_York")
# ohlc <- as.xts(ohlc)
# ohlc <- ohlc["T09:00:00/T16:30:00"]
# save(ohlc, file="/Volumes/external/Develop/data/ES1.RData")
# load recent combined futures data
load(file="/Volumes/external/Develop/data/combined.RData")

# set up data for signal
symbol <- "UX1"
ohlc <- com_bo[, paste(symbol, c("Open", "High", "Low", "Close"), sep=".")]

ohlc_log <- log(ohlc)
# sum(is.na(ohlc))
# sapply(ohlc, class)
# tail(ohlc, 11)
closep <- Cl(ohlc_log)
close_num <- drop(coredata(closep))
retp <- rutils::diffit(closep)
# regression with closep prices as response requires closep to be a vector
# closep <- drop(coredata(closep)
# plot dygraph
dygraphs::dygraph(xts::to.hourly(closep), main=symbol)
# random data
# retp <- xts(rnorm(NROW(ohlc), sd=0.01), index(ohlc))
# closep <- drop(coredata(cumsum(retp))

# Define OHLC data
openp <- Op(ohlc_log)
highp <- Hi(ohlc_log)
high_num <- as.numeric(highp)
lowp <- Lo(ohlc_log)
low_num <- as.numeric(lowp)
close_high <- (close_num == high_num)
close_high_count <- drop(HighFreq::roll_count(close_high))
close_low <- (closep == lowp)
close_low_count <- drop(HighFreq::roll_count(close_low))
open_high <- (openp == highp)
open_high_count <- drop(HighFreq::roll_count(open_high))
open_low <- (openp == lowp)
open_low_count <- drop(HighFreq::roll_count(open_low))


# Set up data for trading
symbol <- "ES1"
retp <- rutils::diffit(log(com_bo[, paste(symbol, "Close", sep=".")]))



# variance <- (highp - lowp)^2
look_back <- 11
varv <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
colnames(varv) <- "variance"
volv <- sqrt(varv)
colnames(volv) <- "volv"
volumes <- Vo(ohlc)
colnames(volumes) <- "volume"

# Define current and future returns
# retp <- rutils::diffit(closep)
# trailing average returns
retp <- rutils::diffit(closep, lagg=look_back)/sqrt(look_back)
colnames(retp) <- "returns"
# returns_adv <- rutils::lagit(retp, lagg=-1)
# or
# returns_adv <- 0.5*(retv_adv + rutils::lagit(retv_adv, lagg=-1))
returns_adv <- rutils::lagit(rutils::diffit(closep, lagg=look_back), lagg=-look_back)/sqrt(look_back)
# returns_adv <- rutils::lagit(HighFreq::roll_sum(retp, look_back=look_back), lagg=-look_back)/look_back
# returns_adv <- xts(retv_adv, index(ohlc))
colnames(retv_adv) <- "returns_adv"
# Scale returns using sigmoid
# returns_adv <- plogis(retv_adv, scale=-quantile(retv_adv, 0.01))
# returns_adv <- (retv_adv - median(retv_adv))
# colnames(retv_adv) <- "returns_adv"



# Begin old stuff

###############
### Strategy using rolling z-scores over OHLC technical indicators
# with regression and dimension reduction


# colnames(retp) <- "returns"
# create design matrix
# datev <- xts::.index(ohlc)
indeks <- 1:NROW(ohlc)
design <- matrix(indeks, nc=1)

model <- HighFreq::calc_lm(respv=as.numeric(retv_adv), design=cbind(retp, varv))
model$coefficients

# old: calculate score as the residual of the regression of the time series of closep prices
look_back <- 11
score <- HighFreq::roll_zscores(respv=close_num, design=design, look_back=look_back)
colnames(score) <- "score"
score[1:look_back] <- 0
# or
score <- calc_signal(look_back, close_num, design)
hist(score, freq=FALSE)
# hist(score, xlim=c(-10, 10), freq=FALSE)

# old: perform parallel loop over look_backs
look_backs <- 15:35
library(parallel)
num_cores <- detectCores()
cluster <- makeCluster(num_cores-1)
# clusterExport(cluster, varlist=c("closep", "design"))
signal_s <- parLapply(cluster, X=look_backs, fun=calc_signal, closep=close_num, design=design)


# trade entry and exit levels
enter <- 1.0
exit <- 0.5
pnls <- calc_revert(signal_s[[1]], returns, enter, exit)
quantmod::chart_Series(pnls[endpoints(pnls, on="days")])

# old uses calc_revert(): run strategies over a vector of trade entry levels
run_strategies <- function(score, returns, enters, exit, return_series=TRUE) {
  # sapply(enters, calc_revert, score=score, retp=retp, exit=exit)
  pnls <- lapply(enters, calc_revert, score=score, retp=retp, exit=exit)
  pnls <- rutils::do_call(cbind, pnls)
  if (return_series) {
    pnls <- rowSums(pnls)
  } else {
    pnls <- as.numeric(pnls[NROW(pnls)])
  }  # end if
  return(pnls)
}  # end run_strategies

# define vector of trade entry levels
enters <- (5:30)/10
# pnls <- run_strategies(signal_s[[1]], returns, enters, exit=exit)
# pnls <- xts(pnls, index(ohlc))
# quantmod::chart_Series(pnls)
clusterExport(cluster, varlist=c("calc_revert"))


## old uses run_strategies(): simulate ensemble of strategies and return heatmap of final pnls
pnls <- parLapply(cluster, X=signal_s, fun=run_strategies, retp=retp, enters=enters, exit=exit, return_series=FALSE)
pnls <- rutils::do_call(rbind, pnls)
colnames(pnls) <- paste0("enter=", enters)
rownames(pnls) <- paste0("look_back=", look_backs)
heatmap(pnls, Colv=NA, Rowv=NA, col=c("red", "blue"))
rgl::persp3d(z=pnls, col="green")
plot(colSums(pnls), t="l", xlab="")


## Simulate ensemble of strategies and return the average pnls
pnls <- parLapply(cluster, X=signal_s[1:10], fun=run_strategies, retp=retp, enters=enters, exit=exit, return_series=TRUE)
pnls <- rutils::do_call(cbind, pnls)
pnls <- xts(pnls, index(ohlc))
colnames(pnls) <- paste0("look_back=", look_backs[1:10])
# plot matrix using plot.zoo()
colors <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
plot.zoo(pnls[endpoints(pnls, on="days")], main="pnls", lwd=2,
         plot.type="single", xlab="", ylab="pnls", col=colors)
# Add legend
legend("bottomright", legend=colnames(pnls), col=colors, lty=1, lwd=4, inset=0.05, cex=0.8)
# plot single dygraph
pnls <- rowSums(pnls)
pnls <- xts(pnls, index(ohlc))
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls)[endpoints(pnls, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


stopCluster(cluster)  # Stop R processes over cluster under Windows

# Count the number of consecutive TRUE elements, and reset to zero after every FALSE element
roll_countr <- function(score) {
  count_true <- integer(NROW(score))
  count_true[1] <- score[1]
  for (it in 2:NROW(score)) {
    if (score[it])
      count_true[it] <- count_true[it-1] + score[it]
    else
      count_true[it] <- score[it]
  }  # end for
  return(count_true)
}  # end roll_countr
foo <- logical(21)
foo[sample(NROW(foo), 12)] <- TRUE
barr <- roll_countr(foo)
foo <- roll_countr(close_high)
bar <- hist(foo, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar <- hist(close_high_count, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar <- hist(close_low_count, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar$counts
all.equal(roll_countr(close_high), drop(HighFreq::roll_count(close_high)), check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_countr(close_high),
  rcpp=HighFreq::roll_count(close_high),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# Contrarian strategy
nrows <- NROW(ohlc)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[close_high] <- (-1)
posv[close_low] <- 1
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- rutils::lagit(posv, lagg=1)

# Contrarian strategy using HighFreq::roll_count()
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[close_high_count>2] <- (-1)
posv[close_low_count>2] <- 1
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- rutils::lagit(posv, lagg=1)

# Contrarian strategy using roll_cum()
posv <- rep(0, nrows)
posv[close_high] <- (-1)
posv[close_low] <- 1
posv <- roll_cum(posv, 2)
posv <- rutils::lagit(posv, lagg=1)


# wipp
# Contrarian strategy using roll_maxmin()
look_back <- 11
max_min <- roll_maxmin(close_num, look_back)
close_max <- (close_num == max_min[, 1])
close_min <- (close_num == max_min[, 2])
volv <- HighFreq::roll_var_ohlc(ohlc=ohlc_log, look_back=5*look_back, scale=FALSE)
volv <- sqrt(volv)
volv[1] <- volv[2]
colnames(volv) <- "volv"
dra_w <- rutils::diffit(close_num, lagg=look_back)
dra_w <- as.numeric(dra_w/volv)
max_min <- roll_maxmin(dra_w, look_back)
draw_max <- (dra_w == max_min[, 1])
draw_min <- (dra_w == max_min[, 2])

posv <- rep(NA_integer_, nrows)
posv[1] <- 0
# posv[close_max] <- (-1)
# posv[close_min] <- 1
posv[(dra_w>4) & draw_max & close_max] <- (-1)
posv[(dra_w<(-4)) & draw_min & close_min] <- 1
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- rutils::lagit(posv, lagg=1)

# Number of trades
sum(abs(rutils::diffit(posv))) / NROW(posv)

# Calculate strategy pnls
pnls <- cumsum(posv*returns)
colnames(pnls) <- "strategy"


# dygraphs plot
endd <- xts::endpoints(pnls, on="days")
dygraphs::dygraph(pnls[endd], main="ES1 strategy")
# data for plot
datav <- cbind(closep, pnls)[endd]
colnames(datav) <- c(symbol, "pnls")
# or
datav <- cbind(closep, posv)
colnames(datav) <- c(symbol, "position")
# dygraphs plot with two "y" axes
second_series <- colnames(datav)[2]
dygraphs::dygraph(datav, main=paste(symbol, "Strategy Using OHLC Technical Indicators")) %>%
  dyAxis("y", label=symbol, independentTicks=TRUE) %>%
  dyAxis("y2", label=second_series, independentTicks=TRUE) %>%
  dySeries(second_series, axis="y2", col=c("blue", "red"))


x11()
# datev <- index(ohlc)
posv <- xts::xts(posv, index(ohlc))
# rangev <- "2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"
rangev <- "2018-02-05/2018-02-07"
dygraphs::dygraph(pnls[rangev], main="ES1 strategy")
# Calculate integer index of date range
# rangev <- index(ohlc["2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"])
# rangev <- index(ohlc[rangev])
# rangev <- (which(datev==min(rangev)):which(datev==max(rangev)))
# plot prices
chart_Series(x=Cl(ohlc[rangev]))
# Add background shading of areas
add_TA(posv[rangev] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(posv[rangev] < 0, on=-1,
       col="lightgrey", border="lightgrey")

# Calculate integer index of date range
datev <- xts::.index(ohlc)
rangev <- xts::.index(ohlc[rangev])
rangev <- (which(datev==min(rangev)):which(datev==max(rangev)))
# Add vertical lines
# close_high_count <- xts::xts(close_high_count, index(ohlc))
# close_low_count <- xts::xts(close_low_count, index(ohlc))
close_high_count <- drop(close_high_count)
close_low_count <- drop(close_low_count)
abline(v=which(close_high_count[rangev]>0), col='red')
abline(v=which(close_low_count[rangev]>0), col='blue')

draw_max <- xts::xts(draw_max, index(ohlc))
draw_min <- xts::xts(draw_min, index(ohlc))
abline(v=draw_max[rangev], col='blue')
abline(v=draw_min[rangev], col='red')
# Add background shading of areas
chart_Series(x=Cl(ohlc[rangev]))
add_TA(draw_max[rangev], on=-1,
       col="blue", border="blue")
add_TA(draw_min[rangev], on=-1,
       col="red", border="red")

# wipp
# dygraphs plot with max_min lines
datav <- xts::xts(cbind(close_num, max_min), index(ohlc))[rangev]
colnames(datav) <- c(symbol, "max", "min")
colors <- c("blue", "red", "green")
dygraphs::dygraph(datav, main=paste(symbol, "max and min lines")) %>%
  dyOptions(colors=colors)

# Standard plot with max_min lines
# plot(as.numeric(datav[, 1]), type="l", col="blue",
#      main=paste(symbol, "max and min lines"),
#      xlab="", ylab="")
# lines(datav[, 2], col="red")
# lines(datav[, 3], col="green")
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(datav, theme=plot_theme, name=paste(symbol, "max and min lines"))
legend(x="left", title=NULL, legend=colnames(datav),
       inset=0.1, cex=1.2, bg="white", bty="n",
       lwd=6, lty=1, col=colors)


# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vectorv, look_back) {
  nrows <- NROW(vectorv)
  max_min <- matrix(numeric(2*nrows), nc=2)
  # Startup periods
  max_min[1, 1] <- vectorv[1]
  max_min[1, 2] <- vectorv[1]
  for (it in 2:(look_back-1)) {
    sub_vec <- vectorv[1:it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  # remaining periods
  for (it in look_back:nrows) {
    sub_vec <- vectorv[(it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr

max_min <- roll_maxmin(close_num, look_back)
max_minr <- roll_maxminr(close_num, look_back)
all.equal(max_min, max_minr)
bar <- TTR::runMax(x=close_num, n=look_back)
all.equal(max_min[-(1:look_back), 1], bar[-(1:look_back)])
bar <- TTR::runMin(x=close_num, n=look_back)
all.equal(max_min[-(1:look_back), 2], bar[-(1:look_back)])
max_min <- xts(max_min, index(closep["2014-05"]))
dygraphs::dygraph(max_min[, 1]-closep["2014-05"])

library(microbenchmark)
summary(microbenchmark(
  tt_r=TTR::runMax(x=closep["2014-05"], n=look_back),
  rcpp=roll_maxmin(as.numeric(closep["2014-05"]), look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# end of old stuff


###############
### Strategy using static betas over OHLC technical indicators
# with regression and dimension reduction

# Load OHLC futures data
load(file="/Volumes/external/Develop/data/combined.RData")

# Define OHLC technical indicators
# residuals of the regression of the time series of closep prices
datev <- xts::.index(ohlc)
# foo <- unique(datev)
design <- matrix(datev, nc=1)
# foo <- MASS::ginv(design)
look_back <- 11
zscores <- HighFreq::roll_zscores(respv=closep,
                                   design=design,
                                   look_back=look_back)
colnames(zscores) <- "zscores"
zscores[1:3] <- 0
close_open <- (closep-openp)
colnames(close_open) <- "close_open"
close_high <- (closep-highp)
colnames(close_high) <- "close_high"
close_low <- (closep-lowp)
colnames(close_low) <- "close_low"
# skew <- ((highp+lowp) - (openp+closep))
skew <- ((highp+lowp) - (openp+closep))
colnames(skew) <- "skew"
# moment_um <- ((closep-openp) - (highp-lowp))
moment_um <- ((closep-openp) - (highp-lowp)) + 1.0
colnames(moment_um) <- "moment_um"

# close_high <- (highp - rutils::lagit(highp))
# close_low <- (lowp - rutils::lagit(lowp))
# Select only independent indicators

indicator_s <- cbind(retp, volv, skew)

# indicator_s <- cbind(retp, close_open, close_high, close_low, volv, skew, moment_um, zscores)
# colnames(indicator_s) <- c("close_high", "openp_highp", "closep_highp")
# indicator_s <- cbind(retp, volv, skew, moment_um, indicator_s)
# indicator_s <- cbind(openp-highp, openp-lowp, openp-closep, closep-highp, closep-lowp, highp-lowp)
# colnames(indicator_s) <- c("open_high", "open_low", "open_close", "close_high", "close_low", "high_low")
# indicator_s <- cbind(skew, moment_um, indicator_s)
# Select only independent indicators
# indicator_s <- cbind(openp-highp, closep-highp)
# colnames(indicator_s) <- c("openp_highp", "closep_highp")
# indicator_s <- cbind(retp, skew, indicator_s)
colnamev <- colnames(indicator_s)

# Scale indicator_s using roll_scale()
look_back <- 11
indicator_s <- roll::roll_scale(data=indicator_s, width=look_back, min_obs=1)
indicator_s[1, ] <- 0
round(cor(indicator_s), 3)
indicator_s <- cbind(indicator_s, zscores)
indicator_s[1:3, ] <- 0
colnamev <- colnames(indicator_s)


# Scale indicator_s using sigmoid
indicator_s <- lapply(1:NCOL(indicator_s), function(colnum) {
  x <- plogis(indicator_s[, colnum], scale=-quantile(indicator_s[, colnum], 0.01))
  (x - median(x))
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)

round(cor(indicator_s), 3)

# Calculate PCA of technical indicators
pcad <- prcomp(indicator_s)
pcad$sdev
pcad$rotation


## Create design matrix for SPY or ES1
# design <- cbind(retp, close_high, close_low, rutils::diffit(varv), rutils::diffit(volumes))
# design from pcad
# rolling average
indicator_s <- lapply(1:NCOL(indicator_s), function(colnum) {
  HighFreq::roll_sum(indicator_s[, colnum], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- colnamev
# design <- as.data.frame(cbind(retv_adv, returns, varv))
# colnames(design) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
design <- as.data.frame(cbind(retv_adv, indicator_s))
colnames(design)[1] <- "returns_adv"
# or
design <- cbind(HighFreq::roll_sum(retp, look_back=look_back),
                 HighFreq::roll_sum(moment_um, look_back=look_back),
                 HighFreq::roll_sum(skew, look_back=look_back))
design <- as.data.frame(cbind(retv_adv, design))
# design <- cbind(retv_adv>0, design)
colnames(design) <- c("indic", "returns", "momentum", "skew")

# design <- cbind(design, rutils::lagit(design, lagg=1), rutils::lagit(design, lagg=2), rutils::lagit(design, lagg=3), rutils::lagit(design, lagg=4))
# design <- cbind(retp, close_high, close_low, returns/sqrt(varv), close_high/sqrt(varv), varv, volumes)
# colnames(design)[4:5] <- c("returns_s", "close_high_s")
## Apply rolling centering and scaling to the design matrix
design <- lapply(design, function(x) (x-mean(x))/sd(x))
design <- rutils::do_call(cbind, design)
sum(is.na(design))



## Create design matrix for ES1, TY1, UX1
# design <- cbind(retp, close_high, close_low, rutils::diffit(varv), rutils::diffit(volumes))
# design from pcad
# define indicators
look_back <- 5
indicator_s <- c("ES1.Close", "TY1.Close", "TU1.Close", "UX1.Close", "UX2.Close")
dygraphs::dygraph(ohlc[, indicator_s[2]]-ohlc[, indicator_s[3]])
indicator_s <- lapply(indicator_s, function(colnum) {
  colnum <- ohlc[, colnum]
  score <- rutils::diffit(closep, lagg=look_back)/sqrt(look_back)/sqrt(HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE))
  HighFreq::roll_sum(indicator_s[, colnum], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- colnamev
design <- as.data.frame(cbind(retv_adv, design))
# colnames(design) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
design <- as.data.frame(cbind(retv_adv, indicator_s))
colnames(design)[1] <- "returns_adv"
# or
design <- cbind(HighFreq::roll_sum(retp, look_back=look_back),
                 HighFreq::roll_sum(moment_um, look_back=look_back),
                 HighFreq::roll_sum(skew, look_back=look_back))
design <- as.data.frame(cbind(retv_adv, design))
# design <- cbind(retv_adv>0, design)
colnames(design) <- c("indic", "returns", "momentum", "skew")

# design <- cbind(design, rutils::lagit(design, lagg=1), rutils::lagit(design, lagg=2), rutils::lagit(design, lagg=3), rutils::lagit(design, lagg=4))
# design <- cbind(retp, close_high, close_low, returns/sqrt(varv), close_high/sqrt(varv), varv, volumes)
# colnames(design)[4:5] <- c("returns_s", "close_high_s")
## Apply rolling centering and scaling to the design matrix
design <- lapply(design, function(x) (x-mean(x))/sd(x))
design <- rutils::do_call(cbind, design)
sum(is.na(design))





## run regressions of future returns against different indicators

# lm formula
colnamev <- colnames(design)
formulav <- as.formula(paste(colnamev[1], paste(colnamev[-1], collapse=" + "), sep="~"))
formulav <- as.formula(paste(colnamev[1], paste(paste(colnamev[-1], collapse=" + "), "- 1"), sep="~"))


# find extreme returns
excess <- which(retv_adv > quantile(retv_adv, 0.99) | returns_adv < quantile(retv_adv, 0.01))
excess <- sort(unique(c(excess, excess+1, excess-1)))

# perform regression
model <- lm(formulav, data=design)
# model <- lm(retv_adv[-excess] ~ design[-excess, ])
model_sum <- summary(model)
model_sum$coefficients
weightv <- model_sum$coefficients[, 1]
weightv <- model_sum$coefficients[, 1][-1]
score <- xts(as.matrix(design)[, -1] %*% weightv, order.by=index(ohlc))
score <- rutils::lagit(score)


# Signal from z-scores (t-values) of trailing slope
design <- matrix(xts::.index(ohlc), nc=1)
look_back <- 3
score <- HighFreq::roll_zscores(respv=closep, design=design, look_back=look_back)
score <- roll::roll_scale(data=score, width=look_back, min_obs=1)
score[1:look_back, ] <- 0
score[is.infinite(score)] <- NA
score <- zoo::na.locf(score, na.rm=FALSE)
sum(is.infinite(score))
sum(is.na(score))
sd(score)
hist(score, freq=FALSE)
plot(score, t="l")


# wipp
# Simulate ensemble of strategies using slope as technical indicator
# mean-reverting strategies
# par_am <- cbind(6:10, rep((3:12)/10, each=NROW(6:10)))
posit_mat <- sapply(4:8, function(look_short) {
  # mean reverting signal
  score_short <- calc_signal(ohlc=ohlc_log,
                               closep=close_num,
                               design=design,
                               look_short=look_short, high_freq=FALSE)
  # Simulate the positions of mean reverting strategy
  sim_revert(score_short, returns, close_high, close_low, enter, exit, trade_lag=1)
})  # end sapply
par_am <- cbind(8:12, rep((3:12)/10, each=NROW(8:12)))
posit_mat <- sapply(1:NROW(par_am), function(it) {
  look_short <- par_am[it, 1]
  enter <- par_am[it, 2]
  score <- HighFreq::roll_zscores(respv=closep, design=design, look_back=look_short)
  score[1:look_short, ] <- 0
  # Scale score using roll_scale()
  score <- roll::roll_scale(data=score, width=look_short, min_obs=1)
  score[1:look_short, ] <- 0
  # score <- rutils::lagit(score, lagg=1)
  # Calculate positions, either: -1, 0, or 1
  posv <- rep(NA_integer_, nrows)
  posv[1] <- 0
  posv[score < (-enter)] <- 1
  posv[score > enter] <- (-1)
  zoo::na.locf(posv, na.rm=FALSE)
})  # end sapply
posv <- rowMeans(posit_mat)
posv[is.na(posv)] <- 0
posv <- rutils::lagit(posv, lagg=1)
# plot(posv, t="l")
pnls <- cumsum(posv*returns)
pnls <- closep + 2*pnls
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls)[endpoints(closep, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# Scale returns using MAD median
num_retv <- as.numeric(retp)
foo <- sapply((look_back+1):NROW(num_returns), function(it) {
  sub_vec <- num_retv[(it-look_back+1):it]
  (num_retv[it]-median(sub_vec))/mad(sub_vec, constant=1.0)
})  # end sapply
tail(foo)
bar <- HighFreq::roll_scale(matrixv=retp, look_back=look_back, use_median=TRUE)
bar[is.infinite(bar), ] <- 0
tail(drop(bar))
summary(microbenchmark(
  roll=roll::roll_scale(data=num_returns, width=look_back, min_obs=1),
  rcpp=roll_scale(matrixv=num_returns, look_back=look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

foo <- (num_returns-median(num_returns))/mad(num_returns, constant=1.0)
bar <- HighFreq::calc_scaled(matrixv=num_returns)
all.equal(foo, drop(bar))

library(microbenchmark)
summary(microbenchmark(
  pure_r=(num_returns-median(num_returns))/mad(num_returns, constant=1.0),
  rcpp=HighFreq::calc_scaled(matrixv=num_returns),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# wipp
# Newer code: optimize strategies using slope as technical indicator

# OHLC data setup
ohlc <- log(HighFreq::SPY["2010-10/2010-11"])
ohlc_log <- log(ohlc)
openp <- Op(ohlc_log)
highp <- Hi(ohlc_log)
lowp <- Lo(ohlc_log)
closep <- Cl(ohlc_log)
retp <- rutils::diffit(closep)
# colnames(retp) <- "returns"
close_num <- as.numeric(closep)
close_high <- (close_num == high_num)
close_low <- (close_num == low_num)
indeks <- 1:NROW(ohlc)
design <- matrix(indeks, nc=1)

look_back <- 15
run_signal <- function(look_back, returns) {
  score <- HighFreq::roll_scale(matrixv=retp, look_back=look_back, use_median=TRUE)
  score[1:look_back, ] <- 0
  # score[is.infinite(score), ] <- 0
  score[is.infinite(score)] <- NA
  score <- zoo::na.locf(score, na.rm=FALSE)
  rutils::lagit(score, lagg=1)
}  # end run_signal
score <- run_signal(look_back, returns)
run_signal <- function(look_back, closep, design) {
  score <- HighFreq::roll_zscores(respv=closep, design=design, look_back=look_back)
  score[1:look_back, ] <- 0
  # score <- HighFreq::roll_scale(matrixv=score, look_back=look_back, use_median=TRUE)
  # score[1:look_back, ] <- 0
  # score[is.infinite(score), ] <- 0
  score[is.infinite(score)] <- NA
  score <- zoo::na.locf(score, na.rm=FALSE)
  rutils::lagit(score, lagg=1)
}  # end run_signal
score <- run_signal(look_back, closep, design)
hist(score, freq=FALSE)
hist(score, xlim=c(-10, 10), freq=FALSE)

# perform parallel loop over look_backs under Windows
look_backs <- 15:35
library(parallel)
cluster <- makeCluster(num_cores-1)
clusterExport(cluster, varlist=c("closep", "design"))
signal_s <- parLapply(cluster, X=look_backs, fun=run_signal, closep=closep, design=design)


# close_high and close_low are Boolean vectors which are TRUE if the close price is at the high or low price
run_strategy <- function(score, returns, enter, exit, close_high=TRUE, close_low=TRUE) {
  posv <- rep(NA_integer_, NROW(score))
  posv[1] <- 0
  # posv[score < (-enter)] <- 1
  posv[(score < (-enter)) & close_low] <- 1
  # posv[score > enter] <- (-1)
  posv[(score > enter) & close_high] <- (-1)
  posv[abs(score) < exit] <- 0
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- posv + rutils::lagit(posv, lagg=1)
  pnls <- cumsum(posv*returns)
  pnls[NROW(pnls)]
  # colnames(pnls) <- "strategy"
}  # end run_strategy
# trade entry and exit levels
enter <- 2.0
exit <- 0.5
pnls <- run_strategy(signal_s[[1]], returns, enter, exit, close_high, close_low)


run_strategies <- function(score, returns, enters, exit, close_high=TRUE, close_low=TRUE) {
  sapply(enters, run_strategy, score=score, retp=retp, exit=exit, close_high=close_high, close_low=close_low)
  # pnls <- lapply(enters, run_strategy, score=score, retp=retp, exit=exit)
  # pnls <- rutils::do_call(cbind, pnls)
  # rowSums(pnls)
}  # end run_strategies
# trade entry levels
enters <- (5:40)/10
foo <- run_strategies(signal_s[[1]], returns, enters, exit=exit, close_high, close_low)
clusterExport(cluster, varlist=c("run_strategy"))
pnls <- parLapply(cluster, X=signal_s, fun=run_strategies, retp=retp, enters=enters, exit=exit, close_high=close_high, close_low=close_low)

stopCluster(cluster)  # Stop R processes over cluster under Windows
pnls <- rutils::do_call(cbind, pnls)
rownames(pnls) <- paste0("enter=", enters)
colnames(pnls) <- paste0("look_back=", look_backs)
heatmap(pnls, Colv=NA, Rowv=NA, col=c("red", "blue"))
pnls <- rowSums(pnls)
pnls <- xts(pnls, index(ohlc))
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls)[endpoints(pnls, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# trade ensemble of strategies using slope as technical indicator
# mean-reverting strategies
foo <- sapply(2:15, function(look_back) {
  score <- HighFreq::roll_zscores(respv=closep,
                          design=design,
                          look_back=look_back)
  score[1:3, ] <- 0
  # score <- rutils::lagit(score)
  -sign(score)
})  # end sapply
# trending strategies
bar <- sapply(10*(10:15), function(look_back) {
  score <- HighFreq::roll_zscores(respv=closep,
                          design=design,
                          look_back=look_back)
  score[1:3, ] <- 0
  # score <- rutils::lagit(score)
  sign(score)
})  # end sapply
posv <- cbind(foo, bar)
posv <- rowMeans(posv)
posv[is.na(posv)] <- 0
posv <- rutils::lagit(posv)
plot(posv, t="l")
pnls <- cumsum(posv*returns)
pnls <- closep + 3*pnls
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# regress returns_adv versus moment_um indicator
# moment_um <- moment_um[abs(moment_um)>0.9]
foo <- sapply(1:10, function(look_back) {
  if (look_back>1)
    returns_adv <- rutils::lagit(HighFreq::roll_sum(retp, look_back=look_back), lagg=-look_back)/look_back
  else
    returns_adv <- rutils::lagit(retp, lagg=-look_back)
  model <- lm(retv_adv ~ moment_um)
  model_sum <- summary(model)
  model_sum$coefficients[2, 3]
})  # end sapply

# use average returns as predictor
foo <- sapply(1:11, function(look_back) {
  if (look_back>1)
    retp <- HighFreq::roll_sum(retp, look_back=look_back)/look_back
  else
    retp <- returns
  model <- lm(retv_adv ~ returns)
  model_sum <- summary(model)
  model_sum$coefficients[2, 3]
})  # end sapply

foo <- cbind(retv_adv, returns)
colnamev <- colnames(foo)
formulav <- as.formula(paste(colnamev[1], paste(colnamev[-1], collapse=" + "), sep="~"))
# perform regression
# returns_adv <- plogis(retv_adv, scale=-quantile(foo, 0.1))
excess <- ((foo[, 2]>quantile(foo[, 2], 0.05)) & (foo[, 2]<quantile(foo[, 2], 0.95)))
model <- lm(formulav, data=foo[excess])
model_sum <- summary(model)
model_sum
plot(formulav, data=foo[excess])
abline(model, lwd=2, col="red")

# perform optimization
# objective function equal to the strategy Sharpe ratio
# plus a penalty term for the weight constraint:
# sum(weightv) == 1.
objfun <- function(weightv, indicator_s, returns) {
  score <- rutils::lagit(indicator_s %*% weightv)
  pnls <- score*returns
  se_lect <- ((pnls>quantile(pnls, 0.05)) & (pnls<quantile(pnls, 0.95)))
  pnls <- pnls[se_lect]
  -mean(pnls)/sd(pnls) + (sum(weightv) - 1)^2
}  # end objfun

# perform parameter optimization using function optim()
optimd <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=objfun,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s,
                retp=retp)
weightv <- optimd$par
names(weightv) <- colnames(indicator_s)
score <- xts(indicator_s %*% weightv, order.by=index(ohlc))
score <- rutils::lagit(score)


# perform logistic regression
glmod <- glm(formulav, data=design, family=binomial)
summary(glmod)
glm_predict <- predict(glmod, newdata=design, type="response")
enter <- 0.58
forecastvs <- data.frame((glm_predict>enter), coredata(design[, 1]))
colnames(forecastvs) <- c("lm_pred", "realized")
table(forecastvs)
score <- xts(design %*% rota_tion, order.by=index(ohlc))


# perform lda
l_da <- MASS::lda(formulav, data=design)
summary(l_da)
lda_predict <- predict(l_da, newdata=design)
forecastvs <- data.frame(lda_predict$class, coredata(design[, 1]))
colnames(forecastvs) <- c("lda_pred", "realized")
table(forecastvs)

# perform qda
q_da <- MASS::qda(formulav, data=design)
summary(q_da)
qda_predict <- predict(q_da, newdata=design)
forecastvs <- data.frame(qda_predict$class, coredata(design[, 1]))
colnames(forecastvs) <- c("qda_pred", "realized")
table(forecastvs)


# Calculate PCA of design
pcad <- prcomp(design)
pcad$sdev
pcad$rotation
# lm
model <- lm(retv_adv ~ pcad$x - 1)
model_sum <- summary(model)
model_sum$coefficients


# Curated PCs
rota_tion <- cbind(PC1=rep(0.2, 5),
                   PC2=c(-2, -1, 0, 1, 2),
                   PC3=c(-1, 0.5, 1, 0.5, -1))
pcats <- xts(design %*% rota_tion, order.by=index(design))

model <- lm(retv_adv ~ pcats - 1)
model_sum <- summary(model)
model_sum$coefficients


## Perform in-sample
insample <- 1:2000
# Define OHLC technical indicators
indic_in <- indicator_s[insample]
# Scale indic_in using sigmoid
indic_in <- lapply(1:NCOL(indic_in), function(colnum) {
  x <- plogis(indic_in[, colnum], scale=-quantile(indic_in[, colnum], 0.01))
  (x - median(x))
})  # end lapply
indic_in <- rutils::do_call(cbind, indic_in)
design_in <- as.data.frame(cbind(retv_adv[insample], indic_in))
colnames(design_in)[1] <- "returns_adv"

# perform optimization
optimd <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=objfun,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s[insample],
                retp=retp[insample])
weightv <- optimd$par
names(weightv) <- colnames(indicator_s)
score <- xts(indicator_s %*% weightv, order.by=index(ohlc))
score <- rutils::lagit(score)


# perform regression
model <- lm(formulav, data=design_in)
# model <- lm(retv_adv[-excess] ~ design[-excess, ])
model_sum <- summary(model)
weightv <- model_sum$coefficients[, 1][-1]

# or
pcad <- prcomp(design[insample, ])
pcad$sdev
pcad$rotation
model <- lm(retv_adv[insample] ~ pcad$x - 1)
model_sum <- summary(model)
model_sum$coefficients

# or
model <- lm(retv_adv[insample] ~ pcats[insample] - 1)
model_sum <- summary(model)

# or
model <- lm(retv_adv[insample] ~ design[insample, ] - 1)
model_sum <- summary(model)


weightv <- model_sum$coefficients[, 1]
# weightv <- weightv[-1]
t_vals <- rep(TRUE, NROW(weightv))
t_vals <- (abs(model_sum$coefficients[, 3]) > 2)
weightv[!t_vals] <- 0


## Perform out-of-sample
outsample <- 2001:NROW(ohlc)
# Define OHLC technical indicators
indic_out <- indicator_s[outsample, ]
# Scale indic_in using sigmoid
indic_out <- lapply(1:NCOL(indic_out), function(colnum) {
  x <- plogis(indic_out[, colnum], scale=-quantile(indicator_s[insample, colnum], 0.01))
  (x - median(indicator_s[insample, colnum]))
})  # end lapply
indic_out <- rutils::do_call(cbind, indic_out)
score <- xts(indic_out %*% weightv, order.by=index(ohlc[outsample]))
# Simulate strategy
pnls <- cumsum(score*retp[outsample])
colnames(pnls) <- "strategy"


# or
score <- xts(as.matrix(design)[, t_vals] %*% weightv[t_vals], order.by=index(ohlc))
score <- rutils::lagit(score)

# or
score <- xts(design %*% rota_tion, order.by=index(ohlc))
# score <- xts(design %*% pcad$rotation[, t_vals], order.by=index(ohlc))
score <- xts(as.matrix(design)[, -1] %*% weightv, order.by=index(ohlc))
score <- xts(score[, t_vals] %*% weightv[t_vals], order.by=index(ohlc))
score <- rutils::lagit(score)


# Simulate strategy
pnls <- cumsum(score*returns)
colnames(pnls) <- "strategy"

# plot
library(dygraphs)
dygraphs::dygraph(cbind(closep, pnls), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))



# design <- cbind(rets_lag2, zscores[[3]], hu_rst, sharpe_rolling)
# colnames(design) <- c("returns", "variance", "skew", "hurst")
endd <- xts::endpoints(design, "years")

## Apply rolling centering and scaling to the design matrix
# library(roll)
design <- roll::roll_scale(data=design, width=100*look_back, min_obs=1)
# remove NAs
design[is.na(design)] <- 0
sum(is.na(design))

## perform regressions of future returns against different indicators

# Single indicator
returns_adv <- returns + close_high + close_low
model <- lm(retv_adv ~ returns_adv)
summary(model)

# three indicators - lower lows is most significant
model <- lm(retv_adv ~ returns + close_high + close_low)
summary(model)

# Single indicator
# lower lows indicator works well in bearish periods
returns_adv <- -returns - close_high + close_low
returns_adv <- sign(retv_adv)
model <- lm(retv_adv ~ returns_adv)
summary(model)

mo_del <- lm(rets_adv2 ~ design)
summary(mo_del)
coef(summary(mo_del))
betas <- -coef(summary(mo_del))[-1, 1]

dimax <- 2
covmat <- cov(excess)

# Calculate eigen decomposition
eigend <- eigen(matrixv)
eigenval <- eigend$values
eigenvec <- eigend$vectors

# Check for zero singular values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
notzero <- (eigenval > (precision*eigenval[1]))

# Calculate generalized inverse from eigen decomposition
eigen_invmat <- eigenvec[, notzero] %*%
  (t(eigenvec[, notzero]) / eigenval[notzero])

# perform eigen decomposition and calculate eigenvectors and eigenvalues
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
# Calculate regularized inverse
invmat <- eigenvec[, 1:dimax] %*% (t(eigenvec[, 1:dimax]) / eigend$values[1:dimax])
# Calculate the maximum Sharpe ratio portfolio weights
# weightv <- invmat %*% colMeans(excess)
# weightv <- rep(mean(colMeans(excess)), NCOL(excess))
weightv <- colMeans(excess)
weightv <- invmat %*% weightv
weightv <- drop(weightv/sqrt(sum(weightv^2)))


# Simulate strategy
pnls <- cumsum(score*returns)
colnames(pnls) <- "strategy"




###############
### State space model and Kalman filter

## Simulate state space model

# Length of data
# nrows <- NROW(endd)
nrows <- 100  # Number of time points
# n <- 5    # Number of observations at each time point
# p <- 2    # Number of covariates


# True parameter values

rho_v <- 2.0
rho_w <- 1.0
gg <- 1.0
hh <- 1.0

# Allocate state vector xx
xx <- numeric(nrows)
# Transition equation for state vector under AR(1) process
set.seed(1121)
# xx[1] <- rnorm(1, sd=rho_w)
for (it in 2:nrows) {
  xx[it] <- gg*xx[it-1] + rnorm(1, sd=rho_w)
}  # end for

# Measurement equation for measured vector
yy <- hh*xx + rnorm(nrows, sd=rho_v)

# Plot
x11()
matplot(cbind(xx, yy), type="l", xlab="", ylab="", lty=1, col=1:2, lwd=2,
        main="State space model \nand Kalman filter")
legend("top", leg=c("state", "observed"), lty=1, lwd=6, col=1:2, inset=0.05)


## Apply Kalman filter

# process matrix
aa <- 1.0
# process variance
qq <- 1.0
# measurement variance
rr <- 1.0

# Allocate predicted vector zz
zz <- numeric(nrows)
zz[1] <- aa*yy[1]
# Allocate process variance vector pp
pp <- numeric(nrows)
pp[1] <- 1
# Allocate predicted variance vector ppp
ppp <- numeric(nrows)
ppp[1] <- aa^2*pp[1] + qq
# Allocate Kalman gain vector kk
kk <- numeric(nrows)
kk[1] <- ppp[1]*hh/(ppp[1]*hh^2+rr)

# Apply Kalman filter recursivelly
for (it in 2:nrows) {
  # Prediction equations for predicted vector zz and predicted variance pp
  zz[it] <- aa*yy[it-1]
  ppp[it] <- aa^2 + qq
  # Correction (measurement) equations
  kk[it] <- ppp[it]*hh/(ppp[it]*hh^2+rr)
  zz[it] <- zz[it] + kk[it]*(yy[it] - hh*zz[it])
  pp[it] <- (1-kk[it]*hh)*ppp[it]
}  # end for


# Plot
x11()
matplot(cbind(xx, yy, zz), type="l", xlab="", ylab="", lty=1, col=1:3, lwd=2,
        main="State space model \nand Kalman filter")
legend("top", leg=c("state", "observed", "Kalman filter"), lty=1, lwd=6, col=1:3, inset=0.05)



set.seed(1121)
d_lm <- dlm::dlm(FF=hh, V=rho_v, GG=gg, W=rho_w, m0=0, C0=100)

a_r <- dlm::dlmModARMA(ar=gg, ma=0.0, sigma2=rho_w)

# Generate data
X <- array(rnorm(nrows*n*p), c(n, p, nrows))
X[, 1, ] <- 1


Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/kalman_filter.cpp")




# Calculate ETF prices
symbolv <- colnames(rutils::etfenv$prices)
symbolv <- symbolv[!(symbolv=="VXX")]
prices <- rutils::etfenv$prices[, symbolv]
# Carry forward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- na.omit(prices)
# Calculate simple ETF returns
retp <- rutils::diffit(prices)
# Calculate the daily excess returns
# riskf is the daily risk-free rate
riskf <- 0.03/260
excess <- returns - riskf


# Define monthly endd without initial warmpup period
endd <- rutils::calc_endpoints(retp, interval="months")
endd <- endd[endd>50]
nrows <- NROW(endd)
# Define 12-month look_back interval and startp over sliding window
look_back <- 12
startp <- c(rep_len(1, look_back-1), endd[1:(nrows-look_back+1)])

# Define the shrinkage intensity
alpha <- 0.5
dimax <- 3

# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(endd),
                     function(i) {
                       # Subset the excess returns
                       excess <- excess[startp[i-1]:endd[i-1], ]
                       eigend <- eigen(cov(excess))
                       # Calculate regularized inverse of covariance matrix
                       dimax <- 3
                       eigenvec <- eigend$vectors[, 1:dimax]
                       eigen_val <- eigend$values[1:dimax]
                       invmat <- eigenvec %*% (t(eigenvec) / eigen_val)
                       # Apply shrinkage to the mean returns
                       colmeans <- colMeans(excess)
                       colmeans <- ((1-alpha)*colmeans + alpha*mean(colmeans))
                       # Calculate weights using R
                       weightv <- invmat %*% colmeans
                       weightv <- weightv/sum(weightv)
                       # Subset the returns to out-of-sample returns
                       retp <- retp[(endd[i-1]+1):endd[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(retp %*% weightv, index(retp))
                     }  # end anonymous function
)  # end lapply

# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"


# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(endd),
                     function(i) {
                       # Subset the excess returns
                       excess <- excess[startp[i-1]:endd[i-1], ]
                       # Apply regularized inverse to mean of excess
                       weightv <- HighFreq::calc_weights(excess, dimax, alpha)
                       # Subset the returns to out-of-sample returns
                       retp <- retp[(endd[i-1]+1):endd[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(retp %*% weightv, index(retp))
                     }  # end anonymous function
)  # end lapply
# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"

indicator_s <- HighFreq::roll_portf(excess, returns, startp-1, endd-1, dimax, alpha)
indicator_s <- xts(indicator_s, index(retp))
colnames(indicator_s) <- "strat_rets"

# Compare RcppArmadillo with R
all.equal(strat_rets, indicator_s[index(strat_rets)])

# Plot dygraph
dygraphs::dygraph(cumsum(indicator_s),
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")



###############
### Benchmark eigen decomposition function in RcppArmadillo

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/calc_eigen.cpp")

eigend <- calc_eigen(scale(prices_ts, scale=FALSE))
model <- prcomp(prices_ts)
all.equal(model$sdev^2, drop(eigend$values))
all.equal(unname(model$rotation), -eigend$vectors)

library(microbenchmark)
summary(microbenchmark(
  rcpp=calc_eigen(prices_ts),
  pure_r=prcomp(prices_ts),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### ARIMA simulation

library(rutils)

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/sim_arima.cpp")

coeff <- -0.8
innov <- rnorm(100)

arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
for (i in 2:NROW(innov)) arimav[i] <- coeff*arimav[i-1] + innov[i]
foo <- arimav

arimav <- filter(c(innov), filter=coeff, method="recursive")
all.equal(as.numeric(arimav), foo)


# two coeff
coeff <- c(-0.8, 0.2)

# old
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
for (i in 3:NROW(innov)) {
  arimav[i] <- coeff[1]*arimav[i-1] + coeff[2]*arimav[i-2] + innov[i]
}  # end for
foo <- arimav

# vectorized
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
for (i in 2:NROW(coeff)) {
  arimav[i] <- coeff[1:(i-1)] %*% arimav[(i-1):1] + innov[i]
}  # end for

for (i in (NROW(coeff)+1):NROW(innov)) {
  arimav[i] <- coeff %*% arimav[(i-1):(i-NROW(coeff))] + innov[i]
}  # end for
foo <- arimav

arimav <- filter(innov, filter=coeff, method="recursive")
all.equal(as.numeric(arimav), foo)


# vectorized vector of coeff
coeff <- c(-0.8, 0.2)
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
for (i in 2:NROW(coeff)) {
  arimav[i] <- coeff[1:(i-1)] %*% arimav[(i-1):1] + innov[i]
}  # end for

for (i in (NROW(coeff)+1):NROW(innov)) {
  arimav[i] <- coeff %*% arimav[(i-1):(i-NROW(coeff))] + innov[i]
}  # end for
foo <- arimav

arimav <- filter(innov, filter=coeff, method="recursive")
all.equal(as.numeric(arimav), foo)

arimav <- sim_arima(innov, rev(coeff))
all.equal(as.numeric(arimav), foo)

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::sim_arima(innov, rev(coeff)),
  pure_r=filter(innov, filter=coeff, method="recursive"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### Convolutions and filtering

# Rcpp::sourceCpp(file="C:/Develop/lecture_slides/assignments/roll_wsum.cpp")
library(HighFreq)

weightv <- c(1, rep(1e-5, 10))

weightv <- exp(-0.2*1:11)
weightv <- weightv/sum(weightv)
vectorv <- as.numeric(rutils::etfenv$VTI[, 6])
weighted <- HighFreq::roll_wsum(vectorv=vectorv, weights=rev(weightv))
filtered <- filter(x=vectorv, filter=weightv, method="convolution", sides=1)

all.equal(as.numeric(vectorv), as.numeric(weighted))

all.equal(as.numeric(filtered[-(1:10)]), as.numeric(weighted))


library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vectorv=vectorv, weights=weightv),
  pure_r=filter(x=vectorv, filter=weightv, method="convolution", sides=1, circular=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


filtered <- roll_wsum_arma(vectorv, rev(weightv))
all.equal(as.numeric(filtered[-(1:10)]), as.numeric(weighted))

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vectorv=vectorv, weights=weightv),
  arma=roll_wsum_arma(vectorv, rev(weightv)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### Stitching and Updating Data for S&P500 Constituents
# in tests already

library(rutils)

# load new data
load("/Users/jerzy/Develop/lecture_slides/data/sp5002018.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_new are the new symbols
symbols_new <- ls(sp500env)
# sp500env_new is the new data
sp500env_new <- sp500env


# load old data
load("/Users/jerzy/Develop/lecture_slides/data/sp5002017.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_old are the old symbols
symbols_old <- ls(sp500env)
# sp500env_old is the old data
sp500env_old <- sp500env
rm(sp500env)

# find the new symbols that are not in the old symbols
is_in <- symbols_new %in% symbols_old
symbols_new[!is_in]

# find the old symbols that are in the new symbols
is_in <- symbols_old %in% symbols_new
symbolv <- symbols_old[is_in]
# find the old symbols that are not in the new symbols
symbols_old[!is_in]

# Create a new environment to store the updated data
sp500env <- new.env()
# Copy the old symbols that are also in the new symbols from sp500env_old to sp500env
# for (symbol in symbolv) {
#   assign(symbol, get(symbol, envir=sp500env_old), envir=sp500env)
#   # sp500env$symbol <- sp500env_old$symbol
# }  # end for

# Stitch the old and new data and copy it into sp500env
for (symbol in symbolv) {
  # get old data
  old_data <- get(symbol, envir=sp500env_old)
  endd <- end(old_data)
  # get new data
  new_data <- get(symbol, envir=sp500env_new)
  # Stitch the old and new data only if old is older
  if (start(old_data) < start(new_data)) {
    closep <- new_data[, 4]
    # diff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] - as.numeric(closep))
    # diff the Close prices
    closep <- rutils::diffit(log(closep))
    # Calculate new extended Close prices
    new_close <- as.numeric(old_data[endd, 4])*exp(cumsum(closep[index(new_data)>endd]))
    # foo <- as.numeric(new_data[endd, 4])*exp(cumsum(closep[index(new_data)>endd]))
    # all.equal(new_data[index(new_data)>endd, 4], foo)
    # Stitch the Close prices
    new_close <- rbind(old_data[, 4], new_close)
    # all.equal(NROW(index(new_close)), NROW(unique(index(new_close))))
    # new_data <- (new_data[, 1:3] + as.numeric(new_close))
    # undiff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] + as.numeric(new_close[index(new_data)]))
    new_data[, 4] <- new_close[index(new_data)]
    new_data[, 6] <- new_close[index(new_data)]
    # Stitch all the prices
    new_data <- rbind(old_data, new_data[index(new_data)>endd])
  }  # end if
  # Copy the data
  assign(symbol, new_data, envir=sp500env)
}  # end for

# verify that all symbols were stitched
all.equal(symbolv, ls(sp500env))
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))

save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")



###############
### sweep() for matrix multiplication

matrixv1 <- matrix(rnorm(1e6), ncol=100)
vectorv <- rnorm(1e2)
matrixv2 <- diag(vectorv)
product <- matrixv1 %*% matrixv2
product2 <- sweep(matrixv1, 2, vectorv, FUN="*")
all.equal(product, product2)

# sweep() is about 5 times faster
library(microbenchmark)
summary(microbenchmark(
  matrix_mult=(matrixv1 %*% matrixv2),
  sweep=sweep(matrixv1, 2, vectorv, FUN="*"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### Portfolio optimization with constraints

# This is an objective function equal to the portfolio
# variance plus a penalty term for the weight constraint:
# sum(weightv) == 1.

objfun <- function(weightv, returns) {
  var(retp %*% weightv) +
    (sum(weightv) - 1)^2
}  # end objfun

# Perform portfolio optimization with the same two weight
# constraints as in p.1
# You must use function optim().

optimd <- optim(par=rep(1.0, NCOL(retp)),
                fn=objfun,
                method="L-BFGS-B",
                upper=rep(1, NCOL(retp)),
                lower=rep(-1, NCOL(retp)),
                retp=retp)

weightv <- optimd$par

var(retp %*% weightv)


# You should get output similar to the following:
# > optimd$par


objfun <- function(weightv, returns, confl, portfolio_sub) {
  # pnls <- returns %*% weightv
  # var(pnls) +
  t(weightv) %*% covmat %*% weightv +
    1000*(sum(weightv) - 1)^2 +
    1000*(sum(weights*portfolio_sub[-1]) - portfolio_sub[1])^2
}  # end objfun



###############
### plot multiple dygraphs in the same RStudio window

# Create the time series
temperature <- ts(frequency = 12, start = c(1980, 1),
                  data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5,
                           25.2, 26.5, 23.3, 18.3, 13.9, 9.6))
rainfall <- ts(frequency = 12, start = c(1980, 1),
               data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0,
                        135.6, 148.5, 216.4, 194.1, 95.6, 54.4))

# Create a list of dygraphs objects
library(dygraphs)
dyplot <- list(
  dygraphs::dygraph(temperature, group="temp_rain", main="temperature", width=400, height=200),
  dygraphs::dygraph(rainfall, group="temp_rain", main="rainfall", width=400, height=200)
)  # end list

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dyplot))



###############
### plot multiple dygraphs in the same RStudio window

# load packages
library(quantmod)
library(dygraphs)

# download time series into an environment
symbolv <- c("VTI", "EEM")
data_env <- new.env()
quantmod::getSymbols(symbolv, from="2017-01-01", env=data_env)

dygraphs::dygraph(data_env$EEM[, 1:4]) %>% dygraphs::dyCandlestick()

# Create a list of dygraphs objects in a loop
dyplot <- eapply(data_env, function(xtsv) {
  dygraphs::dygraph(xtsv[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(xtsv)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
})  # end eapply

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dyplot))

## perform same plotting as above using pipes syntax
# Create a list of dygraphs objects in a loop
eapply(data_env, function(xtsv) {
  dygraphs::dygraph(xtsv[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(xtsv)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
}) %>% # end eapply
  # render the dygraphs objects using htmltools
  htmltools::tagList() %>% htmltools::browsable()




###############
### dygraph plot with highlighting of specific points

library(xts)
library(dygraphs)
# Convert numeric time index of ldeaths into class 'Date' (approximately)
datev <- as.Date(365*(zoo::index(ldeaths)-1970))
# Convert time index from class 'Date' to 'POSIXct'
datev <- as.POSIXct.Date(datev)

# Convert ldeaths into xts time series
ldx <- xts::xts(as.numeric(ldeaths), order.by=datev)
# Calculate number of years
nyears <- NROW(datev)/12
# Calculate the January dates
jan_datev <- datev[1 + 12*(0:(nyears - 1))]
# or
# jan_datev <- datev[which(months(datev)=="January")]
# or
# jan_datev <- datev[grep("Jan", months(datev), ignore.case=TRUE)]
# Calculate the July dates
jul_datev <- datev[7 + 12*(0:(nyears - 1))]
# or
# jul_datev <- datev[which(months(datev)=="July")]

# Create dygraph object
dyplot <- dygraphs::dygraph(ldx, main="Dygraph of ldeaths with Annotations") %>%
  dyHighlight(highlightCircleSize=5)

# Add annotations for the January and July dates to dygraph object
for (i in 1:NROW(jan_datev)) {
  dyplot <- dygraphs::dyAnnotation(dyplot, x=jan_datev[i], text="Jan")
}  # end for
for (i in 1:NROW(jul_datev)) {
  dyplot <- dygraphs::dyAnnotation(dyplot, x=jul_datev[i], text="Jul")
}  # end for

# plot dygraph object
dyplot



###############
### exponentiation operator is a function:

'^'(3, 2)
"^"(3, 2)



###############
### Split random xts time series into daily list and rbind it back into the original xts

xtsv <- xts(x=rnorm(100), order.by=(Sys.time()-3600*(1:100)))
# Split time series into daily list
listv <- xts::split.xts(xtsv, "days")
# rbind the list back into a time series and compare with the original
all.equal(xtsv, rutils::do_call(rbind, listv))



###############
### Code to check for duplicate dates

# Create xts with duplicate dates
x <- xts(rnorm(5), order.by=c(Sys.Date() + 1:4, Sys.Date() + 2))
diff(index(x))

rutils::diffit(index(x))

as.POSIXct(make.index.unique(.index(x), drop=TRUE), origin="1970-01-01")


###############
### Example of an apply() error.
# When apply() parses a data frame with a Boolean column by rows,
# then it adds a leading space to TRUE.
# This may be because FALSE has 5 letters, while TRUE has only 4 letters.
# Below is example of how it works.

# Create data frame with two columns: Boolean column and character column
dframe <- data.frame(
  new_instr=(rnorm(10)>0),
  namev=paste0("instrument", 1:10),
  stringsAsFactors=FALSE)
# perform apply() loop - requires gsub() to remove leading space in TRUE
apply(dframe, MARGIN=1, function(insv) {
  cat("processing instrument:", insv["namev"], "\n")
  cat(insv["new_instr"], "\n")
  if (as.logical(gsub(" ", "", insv["new_instr"])))
    # below doesn't work
    # if (as.logical(insv["new_instr"]))
    1
  else
    0
})  # end apply


# sapply() loop doesn't introduce leading space to TRUE
sapply(1:NROW(dframe), function(itv) {
  insv <- unlist(dframe[itv, ])
  cat("processing instrument:", insv["namev"], "\n")
  cat(insv["new_instr"], "\n")
  # if (as.logical(gsub(" ", "", insv["new_instr"])))
    if (as.logical(insv["new_instr"]))
    1
  else
    0
})  # end sapply



###############
### Brownian bridge puzzle: given deck of 52 cards, every time you randomly choose a red card you're account increases by $1, but if you choose black card you're account decreases by -$1
# At any point you can choose to continue playing, or to stop and keep your net wins.
# The optimal strategy is to stop playing if the current net wins are greater than the expected value of wins from continuing to play.
# Calculate the expected value of the optimal strategy, assuming you start with zero in your account.

# stratmat <- matrix(nrow=4, ncol=4)
npos <- 26
stratmat <- outer(npos:0, npos:0, function(positive, negative)
  (negative - positive))
stratmat[, npos+1] <- 0
stratmat[npos+1, ] <- npos:0

probs <- outer(npos:0, npos:0, function(positive, negative)
  positive/(positive + negative))
probs[, npos+1] <- 0

for (i in npos:1) {
  for (j in npos:1)
    stratmat[i, j] <- max(stratmat[i, j],
                           probs[i, j]*stratmat[i+1, j] + (1-probs[i, j])*stratmat[i, j+1])
  for (j in npos:1)
    stratmat[j, i] <- max(stratmat[j, i],
                           probs[j, i]*stratmat[j+1, i] + (1-probs[j, i])*stratmat[j, i+1])
}  # end for

stratmat[1, 1]

stratfun <- function(cash, positive, negative) {
  # cat(paste("args=", cash, positive, negative, "\n"))
  probv <- positive/(positive + negative)
  if (positive==0)
    max(cash, 0)
  else if (negative==0)
    max(cash + positive, 0)
  else
    max(cash,
        probv*stratfun(cash+1, positive-1, negative) +
          (1-probv)*stratfun(cash-1, positive, negative-1))
}  # end stratfun

# stratfun(0, 26, 26)
stratfun(0, 3, 3)
stratfun(3, 0, 3)
stratfun(2, 1, 0)
stratfun(-3, 3, 0)
stratfun(0, 3, 3)

sapply(3:1, function(positive, negative)
  stratfun(negative-positive, positive, negative),
  negative=3:1)



###############
### Simulating several managers, with only one manager with skill,
# and try to determine how much data is needed to determine which manager has skill

library(rutils)
nman <- 11
# Daily probability as function of Sharpe ratio
sharper <- 0.4
probv <- (sharper/sqrt(250)+1)/2
# Adjust probability to account for multiple managers
p1 <- (0.5*nman + (probv - 0.5)*(nman-1)) / nman
p2 <- (0.5*nman - (probv - 0.5)) / nman
meanv <- c(2*p1-1, rep(2*p2-1, nman-1))

# length of Brownian motion
nrows <- 5000

# Simulate Brownian motion
volv <- 0.01
set.seed(1121)  # reset random number generator
retp <- sapply(meanv, rnorm, n=nrows, sd=volv)
retp <- apply(retp, 2, cumsum)
# apply(retp, 2, mean)
# plot.zoo(retp, plot.type="single")

# length of lookback window
look_back <- 100
# define endd with beginning stub
nagg <- nrows %/% look_back
endd <- c(0, nrows-look_back*nagg+look_back*(0:nagg))
nrows <- NROW(endd)
# startp are single-period lag of endd
startp <- endd[c(1, 1:(nrows-1))] + 1
fix_points <- (startp > endd)
startp[fix_points] <- endd[fix_points]

# Returns aggregated over non-overlapping windows
retagg <- apply(retp, 2, function(x) (x[endd]-x[startp]))


# Switch to best manager with biggest total returns
bestm <- apply(retagg, 1, which.max)
bestm <- rutils::lagit(bestm)
bestm[1] <- 1
# bestm <- c(rep(1, NROW(endd)-NROW(bestm)), bestm)
pnls <- retagg[cbind(1:NROW(retagg), bestm)]
# pnls <- lapply(seq_along(endd), function(datev) {
#   retp[startp[datev]:endd[datev], bestm[datev]]
# })  # end lapply
# pnls <- rutils::do_call(c, pnls)
plot.zoo(cumsum(pnls))


## cum_pnl for multi-manager strategy
cum_pnl <- function(sharper, retp=NULL, meanv=NULL, nman, nrows, look_back, volv=0.01) {
  # Simulate Brownian motion
  if(is.null(retp)) {
    probv <- (sharper/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*nman + (probv - 0.5)*(nman-1)) / nman
    p2 <- (0.5*nman - (probv - 0.5)) / nman
    meanv <- c(2*p1-1, rep(2*p2-1, nman-1))
    set.seed(1121)  # reset random number generator
    retp <- sapply(meanv, rnorm, n=nrows, sd=volv)
    retp <- apply(retp, 2, cumsum)
  } else {
    nman <- NCOL(retp)
    nrows <- NROW(retp)
  }  # end if

  # define endd with beginning stub
  nagg <- nrows %/% look_back
  endd <- c(0, nrows-look_back*nagg+look_back*(0:nagg))
  nrows <- NROW(endd)
  # startp are single-period lag of endd
  startp <- endd[c(1, 1:(nrows-1))] + 1
  fix_points <- (startp > endd)
  startp[fix_points] <- endd[fix_points]

  # total returns over non-overlapping windows
  retagg <- apply(retp, 2, function(x) (x[endd]-x[startp]))

  # Switch to best manager with biggest total returns
  bestm <- apply(retagg, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  bestm <- c(rep(1, NROW(endd)-NROW(bestm)), bestm)
  pnls <- lapply(seq_along(endd), function(datev) {
    retp[startp[datev]:endd[datev], bestm[datev]]
  })  # end lapply
  # return total expected pnl
  pnls <- rutils::do_call(c, pnls)
  sum(pnls)
}  # end cum_pnl

cum_pnl(retp=retp, look_back=100)

cum_pnl(sharper=0.4, nman=11, look_back=100, nrows=5000)



## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, nrows=NULL, sharper=NULL, retp=NULL, meanv=NULL, nman=NULL, volv=0.01) {
  # Calculate drifts
  if(is.null(meanv)) {
    probv <- (sharper/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*nman + (probv - 0.5)*(nman-1)) / nman
    p2 <- (0.5*nman - (probv - 0.5)) / nman
    meanv <- volv*look_back*c(2*p1-1, rep(2*p2-1, nman-1))
  } else {
    nman <- NROW(meanv)
  }  # end if
  # Simulate Brownian motion
  if(is.null(retp)) {
    # set.seed(1121)  # reset random number generator
    nagg <- nrows %/% look_back
    retp <- sapply(meanv, rnorm, n=nagg, sd=sqrt(look_back)*volv)
  } else {
    nman <- NCOL(retp)
    nrows <- NROW(retp)
  }  # end if

  # Switch to best manager with biggest total returns
  bestm <- apply(retp, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  # return total expected pnl
  # pnls <- retp[cbind(1:NROW(retp), bestm)]
  sum(retp[cbind(1:NROW(retp), bestm)])
}  # end cum_pnl



## cum_pnl for multi-manager strategy (simplest version)
cum_pnl <- function(look_back, nrows, sharper=NULL, retp=NULL, meanv=NULL, nman=NULL, volv=0.01) {
  # Calculate drifts
  if(is.null(meanv)) {
    probv <- (sharper/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*nman + (probv - 0.5)*(nman-1)) / nman
    p2 <- (0.5*nman - (probv - 0.5)) / nman
    meanv <- volv*look_back*c(2*p1-1, rep(2*p2-1, nman-1))
  } else {
    nman <- NROW(meanv)
  }  # end if

  # Calculate probability of selecting the best manager
  probv <- integrate(function(x, ...)
    dnorm(x, mean=meanv[1], ...)*pnorm(x, mean=meanv[2], ...)^(nman-1),
            low=-3.0, up=3.0,
            sd=sqrt(look_back)*volv)$value
  # return total expected pnl
  nagg <- nrows %/% look_back
  nagg*(probv*meanv[1] + (1-probv)*meanv[2])
}  # end cum_pnl

cum_pnl(sharper=0.4, nman=11, meanv=meanv, look_back=100, nrows=5000)
cum_pnl(sharper=0.4, nman=11, look_back=100, nrows=5000)
cum_pnl(look_back=100, sharper=0.4, nman=11, nrows=5000)

# Calculate average pnl
foo <- mean(sapply(1:10000, function(x)
  cum_pnl(meanv=meanv, look_back=100, nrows=5000)))

foo <- mean(sapply(1:10000, function(x)
  cum_pnl(look_back=100, sharper=0.4, nman=11, nrows=500000)))

# perform loop over lookback windows
look_backs <- 100*(1:20)
foo <- sapply(look_backs, cum_pnl,
              sharper=0.4, nman=11, nrows=50000)
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(cumsum(pnls), t="l")

# perform loop over number of managers
nman <- 2*(1:50)
foo <- sapply(nman, cum_pnl,
              retp=NULL, sharper=0.4, look_back=100, nrows=50000, meanv=NULL, volv=0.01)
foo <- cbind(nman, foo)
plot(foo, t="l")



###############
### Simulation of asset returns, with a time-dependent drift (skill) plus a random noise.

# define daily volatility: daily prices change by volv units
volv <- 0.01
nrows <- 50000
nman <- 3
# rate of drift (skill) change
ra_te <- 2*pi
# Daily probability as function of Sharpe ratio
sharper <- 0.4
probv <- (sharper/sqrt(250)+1)/2
# Adjust probability to account for two managers
probv <- 0.5 + (probv-0.5)/2
# define growth rate
mea_n <- volv*(2*probv-1)
# time-dependent drift (skill)
# drift <- 0.01*sin(ra_te*(1:nrows)/nrows)
# drift <- rutils::do_call(c, lapply(1:nman, function(x) (drift + 2*pi*x/nman)))
drift <- sapply(1:nman, function(x)
  mea_n*sin(ra_te*(1:nrows)/nrows + 2*pi*x/nman))

# Simulate multiple price paths

# retp <- xts(volv*rnorm(nrows) + drift - volv^2/2,
#                 order.by=seq.Date(Sys.Date()-nrows+1, Sys.Date(), by=1))
# chart_Series(x=retp, name="Multiple price paths")

set.seed(1121)  # reset random number generator
retp <- matrix(volv*rnorm(nman*nrows) - volv^2/2, nc=nman) + drift
# retp <- exp(matrixStats::colCumsums(retp))
# Create zoo time series
# retp <- xts(retp, order.by=seq.Date(Sys.Date()-NROW(retp)+1, Sys.Date(), by=1))
# plot zoo time series
colors <- colorRampPalette(c("red", "blue"))(NCOL(retp))
# colors <- colors[order(order(retp[NROW(retp), ]))]
par(mfrow=c(2, 2))
par(mar=c(3, 1, 1, 1), oma=c(1, 1, 1, 1))
plot.zoo(drift, main="time-dependent growth rates", lwd=3, xlab=NA, ylab=NA, plot.type="single", col=colors)
plot.zoo(retp, main="simulated returns", xlab=NA, ylab=NA, plot.type="single", col=colors)
plot.zoo(apply(retp, 2, cumsum),
         main="simulated prices", xlab=NA, ylab=NA, plot.type="single", col=colors)
# plot_theme <- chart_theme()
# plot_theme$col$line.col <- colors
# chart_Series(retp, theme=plot_theme, name="Multiple price paths")


## Calculate pnl over lookback window
# Calculate cumulative returns
pnls <- apply(retp, 2, cumsum)
# length of lookback window
look_back <- 100
# define endd with beginning stub
nagg <- nrows %/% look_back
endd <- c(0, nrows-look_back*nagg+look_back*(0:nagg))
nrows <- NROW(endd)
# startp are single-period lag of endd
startp <- endd[c(1, 1:(nrows-1))] + 1
fix_points <- (startp > endd)
startp[fix_points] <- endd[fix_points]

# total returns aggregated over non-overlapping windows
retagg <- apply(pnls, 2, function(x) (x[endd]-x[startp]))

# Switch to best manager with biggest total returns
bestm <- apply(retagg, 1, which.max)
bestm <- rutils::lagit(bestm)
bestm[1] <- 1
# bestm <- c(rep(1, NROW(endd)-NROW(bestm)), bestm)
pnls <- retagg[cbind(1:NROW(retagg), bestm)]
plot.zoo(cumsum(pnls))


## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, returns) {
  nrows <- NROW(retp)
  # define endd with beginning stub
  nagg <- nrows %/% look_back
  endd <- c(0, nrows-look_back*nagg+look_back*(0:nagg))
  nrows <- NROW(endd)
  # startp are single-period lag of endd
  startp <- endd[c(1, 1:(nrows-1))] + 1
  fix_points <- (startp > endd)
  startp[fix_points] <- endd[fix_points]
  # total returns aggregated over non-overlapping windows
  retp <- apply(retp, 2, function(x) (x[endd]-x[startp]))
  # Switch to best manager with biggest total returns
  bestm <- apply(retp, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  # return total expected pnl
  # pnls <- retp[cbind(1:NROW(retp), bestm)]
  sum(retp[cbind(1:NROW(retp), bestm)])
}  # end cum_pnl

cum_pnl(look_back=100, retp=pnls)


## cum_pnl for trend-following multi-manager strategy (without endd)
cum_pnl <- function(look_back, returns, pnls) {
  # nrows <- NROW(retp)
  # total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=look_back)
  # Switch to best manager with biggest total returns
  bestm <- apply(retagg, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  # return total expected pnl
  # pnls <- retp[cbind(1:NROW(retp), bestm)]
  sum(retp[cbind(1:NROW(retp), bestm)])
}  # end cum_pnl

# Calculate cumulative returns
pnls <- apply(retp, 2, cumsum)
cum_pnl(look_back=100, retp=retp, pnls=pnls)


## perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
pnls <- sapply(look_backs, cum_pnl, se_lect=1, retp=retp, pnls=pnls)
pnls <- cbind(look_backs, pnls)
plot(pnls, t="l")
# plot(cumsum(pnls), t="l")


## pre-calculate row order indices for a vector of look_backs
# perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
orderstats <- lapply(look_backs, function(look_back) {
  # total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=look_back)
  ordern <- t(apply(retagg, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  # end lapply
names(orderstats) <- look_backs


## cum_pnl for long-short multi-manager strategy (without endd)
cum_pnl <- function(select_best=NULL, select_worst=NULL, returns, ordern) {
  nrows <- NROW(retp)
  if(!is.null(select_best)) {
    ncols <- NCOL(retp)
    bestm <- ordern[, (ncols-select_best+1):ncols]
    bestm <- cbind(1:nrows, bestm)
  } else {
    bestm <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    worstm <- ordern[, 1:select_worst]
    worstm <- cbind(1:nrows, worstm)
  } else {
    worstm <- NULL
  }  # end if
  # return total expected pnl
  # pnls <- retp[bestm]-retp[worstm]
  sum(retp[bestm])/select_best-sum(retp[worstm])/(if(is.null(select_worst)) 1)
}  # end cum_pnl

# Calculate pnl for long-short multi-manager strategy
cum_pnl(select_best=1, select_worst=1, retp=retp, ordern=orderstats[[5]])


## perform loop over lookback windows
pnls <- sapply(orderstats, cum_pnl, select_best=1, select_worst=1, retp=retp)
pnls <- cbind(look_backs, pnls)
plot(pnls, t="l")
# plot(cumsum(pnls), t="l")


nman <- 5
drift <- sapply(1:nman, function(x)
  mea_n*sin(ra_te*(1:nrows)/nrows + 2*pi*x/nman))
set.seed(1121)  # reset random number generator
retp <- matrix(volv*rnorm(nman*nrows) - volv^2/2, nc=nman) + drift
# Calculate cumulative returns
pnls <- apply(retp, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
look_backs <- 20*(1:50)
orderstats <- lapply(look_backs, function(look_back) {
  # total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=look_back)
  ordern <- t(apply(retagg, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  # end lapply
names(orderstats) <- look_backs

## cum_pnl for long-short multi-manager strategy (without endd)
cum_pnl <- function(select_best=NULL, select_worst=NULL, returns, ordern) {
  nrows <- NROW(retp)
  if(!is.null(select_best)) {
    ncols <- NCOL(retp)
    bestm <- ordern[, (ncols-select_best+1):ncols]
    bestm <- cbind(1:nrows, bestm)
  } else {
    bestm <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    worstm <- ordern[, 1:select_worst]
    worstm <- cbind(1:nrows, worstm)
  } else {
    worstm <- NULL
  }  # end if
  # return total expected pnl
  # pnls <- retp[bestm]-retp[worstm]
  sum(retp[bestm])-sum(retp[worstm])
}  # end cum_pnl

# Calculate pnl for long-short multi-manager strategy
# cum_pnl(select_best=1, select_worst=1, retp=retp, ordern=orderstats[[5]])

# perform loop over lookback windows
pnls <- sapply(orderstats, cum_pnl, select_best=1, select_worst=NULL, retp=retp)
pnls <- cbind(look_backs, pnls)
# par(mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1))
# plot(pnls, t="l", main="Trend-following PnL, as function of lookback window")


## double the drift
set.seed(1121)  # reset random number generator
retp <- matrix(volv*rnorm(nman*nrows) - volv^2/2, nc=nman) + 2*drift
# Calculate cumulative returns
pnls <- apply(retp, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
orderstats2x <- lapply(look_backs, function(look_back) {
  # total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=look_back)
  ordern <- t(apply(retagg, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  # end lapply
names(orderstats2x) <- look_backs

plot.zoo(cbind(pnls[, 2], pnls2x), main="Long-short Ensemble PnL, as function of lookback window",
         lwd=2, xaxt="n", xlab="lookback windows", ylab="PnL", plot.type="single", col=c("black", "red"))
# Add x-axis
axis(1, seq_along(look_backs), look_backs)
# Add legend
legend(x="top", legend=paste0("SR=", c(0.4, 0.8)),
       inset=0.0, cex=0.8, bg="white",
       lwd=6, lty=c(1, 1), col=c("black", "red"))




## parallel version with loops - much slower and more complicated
# initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(num_cores-1)

foo <- sapply(look_backs, function(look_back) {
  # define endd with beginning stub
  nagg <- nrows %/% look_back
  endd <- c(0, nrows-look_back*nagg+look_back*(0:nagg))
  nrows <- NROW(endd)
  # startp are single-period lag of endd
  startp <- endd[c(1, 1:(nrows-1))] + 1
  # redefine endd
  endd <- cbind(startp, endd)

  # perform parallel loop over returns
  clusterExport(cluster, varlist=c("nrows", "endd"))
  sharper <- parApply(cluster, MARGIN=2, returns, function(retp) {
    sapply(2:nrows, function(datev) {
      xtsv <- retp[endd[datev, 1]:endd[datev, 2]]
      # Calculate annualized Sharpe ratio of returns
      sum(xtsv)/sd(xtsv)
    })  # end sapply
  })  # end parApply

  # sharper <- rutils::do_call(cbind, sharper)
  sharper[which(is.na(sharper), arr.ind=TRUE)] <- 1

  # Calculate dispersion of SRs
  # c(by_strategy=mean(apply(sharper, 1, sd)),
  #   by_period=mean(apply(sharper, 2, sd)))
  # diff_sr <- apply(sharper, 2, rutils::diffit) / sharper
  # mean(abs(diff_sr))
  # c(by_strategy=mean(apply(sharper, 1, sd)),
  #   by_period=mean(apply(diff_sr, 1, sd)))
  cum_pnl(sharper, returns, endd)
})  # end sapply

foo <- t(foo)
dim(foo)
foo
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(foo[, 1]/foo[, 2], t="l")

## end perform loop over lookback windows




###############
### Portfolio optimization using quadratic solver

load("/Volumes/external/Develop/data/etf_data.RData")
ls(etfenv)
dim(etfenv$returns)
colnames(etfenv$returns)



## perform standard calibration over ohlc interval
optimd <- optim(par=rep(0.5, 2*NCOL(design)),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=rep(2, 2*NCOL(design)),
                lower=rep(-2, 2*NCOL(design)),
                design=design[datev],
                retp=retv_running[datev],
                lambda=lambda)

betas <- optimd$par
names(betas) <- c(paste0(colnames(design), "_long"), paste0(colnames(design), "_short"))


## cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(betas, la_g=15, design=design, retp=retv_running, lambda=0) {
  ncols <- NCOL(design)
  positions <- rep.int(NA, NROW(design))
  positions[1] <- 0
  # buy signal
  bu_y <- (design %*% betas[1:ncols] < -1)
  positions[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lagit(bu_y, lag=la_g))
  # Sell signal
  positions[se_ll] <- -1.0
  positions[bu_y] <- 1.0
  positions <- zoo::na.locf(positions, na.rm=FALSE)
  positions <- c(0, positions[-NROW(positions)])
  # pnls <- positions*returns
  # betav <- (sum(pnls*returns) - sum(pnls)*sum(retp)) / (sum(pnls*pnls) - sum(pnls)^2 )
  # -(exp(sum(pnls) - betav*sum(retp)) - 1)
  # -(exp(sum(positions*returns))-1) # / (sum(abs(rutils::diffit(positions))) / 2/ 1e5) / abs(sum(positions>0) - sum(positions<0))
  -((exp(sum(positions*returns))-1) - lambda*sum(abs(betas)))
}  # end cum_pnl

cum_pnl(betas=betas, design=design[datev], retp=retv_running[datev])

# perform calibration over ohlc interval
optimd <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, NCOL(design)),
                           lower=rep(-2, NCOL(design)),
                           design=design[datev],
                           retp=retv_running[datev],
                           lambda=lambda,
                           control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))


betas <- optimd$optim$bestmem
names(betas) <- colnames(design)
# names(betas) <- colnames(design)
optimd$optim$bestval
cum_pnl(betas, design=design[datev])


bu_y <- (design %*% betas[1:ncols] < -1)

cum_pnl <- function(interval) {
  positions <- rep.int(NA, NROW(design))
  positions[1] <- 0
  positions[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lagit(bu_y, lag=interval))
  positions[se_ll] <- -1.0
  positions[bu_y] <- 1.0
  positions <- zoo::na.locf(positions, na.rm=FALSE)
  positions <- c(0, positions[-NROW(positions)])
  exp(sum((positions*returns_running)))-1
}  # end cum_pnl

cum_pnl(200)
sapply(20*(1:30), cum_pnl)



###############
### Convert correlation matrix into distance object
dis_tance <- xts::.index(volvpikes)
dis_tance <- abs(outer(X=dis_tance, Y=dis_tance, FUN="-"))
# dis_tance <- rutils::diffit(xts::.index(volvpikes))
dis_tance <- as.dist(dis_tance)
# Perform hierarchical clustering analysis
cluster <- hclust(dis_tance)
plot(cluster, ann=FALSE, xlab="", ylab="")
title("clustering of volvpikes", line=0.0)
foo <- cutree(cluster, h=2000)
# foo <- cutree(cluster, k=100)
NROW(volvpikes)
NROW(foo)
NROW(unique(foo))
tail(foo)
tail(volvpikes)
bar <- match(index(volvpikes), index(varv))
tail(bar)



hc <- hclust(dist(USArrests))
plot(hc)
cutree(hc, k=5)
cutree(hc, h=50)

returns_future <- rutils::roll_sum(retv_running, look_back=5)
returns_future <- rutils::lagxts(retv_running, lag=-5)
colnames(retv_future) <- "returns_future"
foo <- lm(retv_future["2008"] ~ design["2008"] - 1)
summary(foo)


##

16*sd(rutils::etfenv$returns[, "VTI"])
sqrt(250)
250/5

# Summary: Create a functional which aggregates
# Asset returns over lookback and look-forward
# intervals.


# define functional

# 1. (20pts) Create a functional called roll_agg(),

# Should perform only a single


###############
###
# 4. (20pts) Create a scatterplot of returns and forward returns
# Create a scatterplot of alphas for "2008" and "2009",
# and add labels with ETF names,
# use columns of "alphas_capm" and functions plot() and text(),

dim(fwd_rets)
dim(pnls)

foo <- na.omit(merge(fwd_rets[, 5], pnls[, 5]))
colnames(foo) <- c("forward_returns", "past_returns")
foo <- as.data.frame(foo)
head(foo)
dim(foo)

x11()
# perform regression
reg_formula <- paste(colnames(foo), collapse=" ~ ")
mo_del <- lm(reg_formula, data=foo)
summary(mo_del)
# plot scatterplot using formula
plot(foo[, 2], foo[, 1], xlab="past returns", ylab="forward returns")
# plot(foo)
title(main="Simple Regression", line=-1)
# Add regression line
abline(mo_del, lwd=2, col="red")


# Select weights proportional to pnls
dim(pnls)
weightv <- coredata(pnls[index(fwd_rets)])
weightv <- weightv/sqrt(rowSums(weightv^2))

# bar <- matrixStats::rowMaxs(weightv)
bar <- coredata(fwd_rets)
dim(bar)

# Select best and worst models in each period
bes_t <- apply(weightv, 1, which.max)
wors_t <- apply(weightv, 1, function(x) {which.min(x)})
bes_t <- apply(weightv, 1, which.max)
wors_t <- apply(weightv, 1, which.min)

back_test <- rowSums(weights*bar)
x11()
plot(cumsum(back_test), t="l")

# back_test <- t(weightv) %*% bar
back_test <- rowSums(weights*bar)
back_test <- xts(back_test, order.by=index(fwd_rets))
x11()
chart_Series(x=cumsum(back_test), name="Back-test of EWMA strategies")

plot(cumsum(back_test), t="l")
NROW(back_test)


#########

# define lookback windows


# Create a functional for performing rolling
# aggregations over overlapping intervals.
# Apply the functional to roll the function simu_ewma()
# over overlapping 12-month intervals in the past.

# 1. (20pts) Create a functional called roll_agg(),
# which should accept four arguments:
#  xtsv - an xts series containing one or more columns of data,
#  endd - integer vector of end points,
#  look_back - number of intervals in the lookback window,
#  FUN - name of of an aggregation function,
#  "..." - optional dots arguments to FUN.

# The functional roll_agg() should perform an lapply()
# loop over endd, subset the xtsv series, and pass
# it to FUN, together with the dots "..." argument.
# roll_agg() should return an xts series, with each
# row equal to the vector returned by FUN.
# hint: You can adapt code from the slide:
# Performing Aggregations Over Overlapping Intervals.

roll_agg <- function(xtsv, endd, look_back, FUN, ...) {
  nrows <- NROW(endd)
  # startp are multi-period lag of endd
  startp <-  endd[c(rep_len(1, look_back-1), 1:(nrows-look_back+1))]
  # perform lapply() loop over length of endd
  agg_s <- lapply(2:nrows,
                  function(indeks) {
                    FUN(xtsv[startp[indeks]:endd[indeks]], ...)
                  })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=index(xtsv[endd]))
  agg_s
}  # end roll_agg


# 2. (20pts) Create an aggregation function called
# agg_regate(), which calls the function simu_ewma()
# and calculates the Sharpe ratios of the EWMA strategy,
# for a given vector of lambdas.
# agg_regate() should accept three arguments:
#  ohlc - an OHLC series containing four columns of data,
#  lambdas - integer vector of lambda parameters,
#  "..." - additional dots arguments to be passed to simu_ewma().
# hint: You can adapt code from the slide:
# Simulating Multiple EWMA Strategies

agg_regate <- function(ohlc, lambdas, ...) {
  sapply(lambdas, function(lambda) {
    # Simulate EWMA strategy and calculate Sharpe ratio
    retp <- simu_ewma(ohlc=ohlc, lambda=lambda, ...)[, "returns"]
    sqrt(260)*sum(retp)/sd(retp)/NROW(retp)
  })  # end sapply
}  # end agg_regate

# Source the function simu_ewma() from the file
# ewma_model.R, using function source().
# Download the latest version from NYU Classes.

source("C:/Develop/R/scripts/ewma_model.R")

# Define ohlc series, the EWMA look_back, and lambdas.

library(HighFreq)
ohlc <- rutils::etfenv$VTI["/2011"]
look_back <- 51
lambdas <- seq(0.001, 0.01, 0.001)

# Call agg_regate() as follows:
agg_regate(ohlc, lambdas, look_back=look_back)

# You should get the following output:
#  [1] 0.1220623 0.1620571 0.1887122 0.2399056 0.2308350 0.1594881 0.1702486 0.1539695 0.1136539
# [10] 0.1180002


# 3. (20pts) Apply the functional roll_agg() to roll
# the function simu_ewma() over overlapping 12-month
# intervals in the past.

# Define end points at the end of each month.
# Use function endpoints() from package xts.

endd <- xts::endpoints(ohlc, on="months")
nrows <- NROW(endd)

# Define number of monthly intervals per lookback interval:
look_back <- 12

# Note that there are two different windows in this simulation.
# The first window is the EWMA window, called look_back and equal
# to 51 by default.
# The second window is the lookback interval, called look_back.
# To avoid an error, the endd should be greater than
# the EWMA look_back, except for the first endd, which
# should be equal to zero.
# Adjust the endd so that they are greater than the
# EWMA look_back.

endd[(endd > 0) & (endd <= look_back)] <- look_back+1

# Run roll_agg() as follows:

sharper <- roll_agg(xtsv=ohlc,
                          endd=endd,
                          look_back=look_back,
                          FUN=agg_regate,
                          lambdas=lambdas,
                          look_back=look_back)

# You should get the following output:
# > sharper[1:6, 1:5]
#                  [,1]       [,2]       [,3]       [,4]       [,5]
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-30 -2.2223479 -2.2223479 -2.2223479 -2.2223479 -2.2223479
# 2007-04-30 -0.9446608 -0.9446608 -0.9446608 -0.9446608 -0.9446608
# 2007-05-31  0.0550219  0.0550219  0.0550219  0.0550219  0.0550219
# 2007-06-29 -0.3290286 -0.3290286 -0.3290286 -0.3290286 -0.3290286


#########

bar <- "foo"
bar <- 10

bar <- "foo"
assign(bar, 10)



###

var1 <- sum(pc1*pc1)
# make returns orthogonal to pc1
datev <- index(retp)
retp <- apply(retp, MARGIN=2,
                  function(x) {x - sum(pc1*x)*pc1/var1})
# apply(retp, MARGIN=2, function(x) sum(pc1*x)) # verify orthogonality

###

x11()
foo <- seq(0, 2*pi, length.out=24)
plot(x=cos(foo), y=sin(foo), asp=1)
abline(a=0, b=-0.1, col="red")
abline(a=0, b=10, col="blue")


###

heatmap(sharper, col=colorRampPalette(c("blue", "red"))(22))

summary(microbenchmark(
  tee=-t(pnls) %*% pnls,
  sumv=-sum(pnls*pnls),
  times=10))[, c(1, 4, 5)]


###

w1 <- sqrt(0.5] w2 <- w1
foo <- matrix(c(w1, w2, -w2, w1), nc=2)
t(foo) %*% foo
# bar <- returns %*% t(solve(foo))
(t(bar) %*% bar) / NROW(bar)

covmat <- function(retp, an_gle=0) {
  w1 <- cos(an_gle)
  w2 <- sin(an_gle)
  matrixv <- matrix(c(w1, -w2, w2, w1), nc=2)
  compo_nents <- returns %*% t(matrixv)
  (t(compo_nents) %*% compo_nents) / NROW(compo_nents)
}  # end covmat

bar <- covmat(retp, an_gle=pi/4)
(t(retp) %*% returns) / NROW(retp)
(t(bar) %*% bar) / NROW(bar)

angle_s <- seq(0, pi/2, by=pi/24)
covmat <- sapply(angle_s, function(an_gle)
  covmat(retp, an_gle=an_gle)[1, 1])
plot(x=angle_s, y=covmat, t="l")

optimd <- optimize(
  f=function(an_gle)
    -covmat(retp, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- optimd$minimum
bar <- covmat(retp, an_gle=an_gle)
tan(an_gle)

w1 <- cos(an_gle)
w2 <- sin(an_gle)
matrixv <- matrix(c(w1, -w2, w2, w1), nc=2)
compo_nents <- returns %*% t(matrixv)
(t(compo_nents) %*% compo_nents) / NROW(compo_nents)

plot(x=compo_nents[, 1], y=compo_nents[, 2],
     xlim=c(-10, 10), ylim=c(-10, 10))

mo_del <- lm(reg_formula, data=retp)
# get regression coefficients
coef(summary(mo_del))

foo <- cbind(rnorm(1000, sd=0.2), rnorm(1000)) %*% t(matrixv)
(t(foo) %*% foo) / NROW(foo)
plot(x=foo[, 1], y=foo[, 2])
summary(lm(foo[, 1] ~ foo[, 2]))

optimd <- optimize(
  f=function(an_gle)
    -covmat(foo, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- optimd$minimum
tan(an_gle)

###

library(plotly)

df <- data.frame(Date=seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by="days"),
                 Value=sample(100:200, size=244, replace=T))

plot_ly(data=df, x=df$Date, y=df$Value, type="scatter", mode="lines") %>%
  add_trace(x=~df$Date, y=~df$Value, name="20yr Treasury rate") %>%
  layout(xaxis=list(range=c( as.numeric(max(df$Date)-30)*86400000,
                                 as.numeric(max(df$Date))*86400000   ),
                      rangeslider=list(type="date")  ))

###


