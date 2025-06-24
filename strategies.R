####################################
# Daily SPY AR strategy using the range volatility.

symboln <- "SPY"
ohlc <- get(symboln, rutils::etfenv)["2010/"]
colnames(ohlc) <- rutils::get_name(colnames(ohlc), 2)
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- symboln
# Daily log returns
retp <- rutils::diffit(log(closep))
# Daily volatility
retv <- (ohlc$High - ohlc$Low)/ohlc$Close
# Daily trading volume
voluv <- ohlc$Volume
# lambdaf <- 0.8
# retv <- HighFreq::run_mean(retv, lambdaf=lambdaf)

# Predictor matrix
orderp <- 5
respv <- retp
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0(symboln, 1:orderp)
# Add the volatility of SPY
predx <- lapply(1:orderp, rutils::lagit, input=rutils::diffit(retv))
predx <- rutils::do_call(cbind, predx)
colnames(predx) <- paste0("Vol", 1:orderp)
predm <- cbind(predm, predx)
# Add the volume of SPY
predx <- lapply(1:orderp, rutils::lagit, input=rutils::diffit(voluv))
predx <- rutils::do_call(cbind, predx)
colnames(predx) <- paste0("Volume", 1:orderp)
predm <- cbind(predm, predx)

# Standardize the predictor matrix
predm <- lapply(predm, function(x) {
  return (x/sd(x))
}) # end lapply
predm <- do.call(cbind, predm)
sapply(predm, sd)

# Perform the regression
regmod <- lm(respv ~ predm - 1)
summary(regmod)

# Comment: The volatility doesn't forecast returns


# Calculate the fitted AR coefficients
predinv <- MASS::ginv(predm)
coeff <- 1e2*drop(predinv %*% respv)
names(coeff) <- colnames(predm)
# Calculate the in-sample forecasts of SPY (fitted values)
fcasts <- predm %*% coeff
# Scale the forecasts
# scalev <- HighFreq::run_mean(abs(fcasts), lambda=0.999)
# fcasts <- ifelse(scalev > 1e-4, fcasts/scalev, 0)
# Calculate the AR strategy PnLs
pnls <- retp*fcasts
# Scale the PnL volatility to that of SPY
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c(symboln, "Strategy")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the AR strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



####################################
# Daily contrarian SPY strategy

# Comment: Scaling the returns by the volatility doesn't improve the performance.

expv <- seq(0.1, 0.9, 0.1)
pnlc <- sapply(expv, function(lambdaf) {
  retm <- HighFreq::run_mean(retp, lambdaf=lambdaf)
  volm <- HighFreq::run_mean(voluv, lambdaf=lambdaf)
  weightv <- rutils::lagit(retm/(volm^expp))
  # weightv <- rutils::lagit(retm)
  pnls <- 1e2*(-weightv*retp)
  # pnlm <- rutils::lagit(HighFreq::run_mean(abs(pnls), lambdaf=0.9))
  # pnlm[1:10] <- 1.0
  # pnls <- 0.01*pnls/pnlm
  mean(pnls)/sd(pnls)
}) # end sapply
plot(expv, pnlc, t="l", xlab="Exponent", ylab="Sharpe ratio",
     main="Exponent vs. Sharpe ratio")
expp <- expv[which.max(pnlc)]
lambdaf <- expv[which.max(pnlc)]

endw <- rutils::calc_endpoints(pnls, interval="weeks")
captiont <- paste0("EMA SPY", " / lambdaf=", lambdaf, " / expp=", expp)
dygraph(cumsum(pnls)[endw], main=captiont) %>%
  dyOptions(colors=c("blue"), strokeWidth=2)

wealthv <- cbind(retp, pnls, 0.5*(retp+pnls))
colnames(wealthv) <- c("Stocks", "Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2)
dygraph(cumsum(wealthv$Combined)[endw], main=captiont) %>%
  dyOptions(colors=c("blue"), strokeWidth=2)



####################################
# Daily contrarian BuyDip stock strategy using the alpha portfolios with respect to SPY.

# Load daily ETF percentage returns
symbolv <- c("SPY", "XLV", "XLP", "XLU", "XLK", "XLB", "XLI", "XLF", "XLE", "XLY", "EEM", "GLD", "VNQ", "TLT")
retstock <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- index(retstock)
nstocks <- NCOL(retstock)

pcad <- prcomp(retstock, center=FALSE, scale=FALSE)
pcap <- pcad$x
# volv <- apply(pcap, 2, sd)
# plot(volv)
retstock <- pcap[, 8:14]
nstocks <- NCOL(retstock)


# Load daily S&P500 stock percentage returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select top stock returns
# load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returnstop.RData")
# Subset to the SPY index
# spy <- na.omit(rutils::etfenv$returns$SPY)
# retstock <- retstock[datev]
retstock <- retstock["2010-01-01/"]
# pricestock <- pricestock["2010-01-01/"]
# volv <- sapply(retstock, sd, na.rm=TRUE)
# retstock <- retstock[, (volv < 0.02)]
retstock[is.na(retstock)] <- 0
# retstock[is.na(retstock) | (pricestock < 0.1)] <- 0
# retstock <- cbind(retstock, spy)
retstock$ENDO <- NULL
datev <- index(retstock)
nstocks <- NCOL(retstock)

expv <- seq(0.1, 0.9, 0.1)
pnlc <- sapply(expv, function(expp) {
  varm <- HighFreq::run_var(spy, lambdaf=lambdaf)
  retm <- varm[, 1]
  varm <- (varm[, 2])^expp
  weightv <- rutils::lagit(retm/varm)
  pnls <- -20*weightv*spy
  # pnlm <- rutils::lagit(HighFreq::run_mean(abs(pnls), lambdaf=0.9))
  # pnlm[1:10] <- 1.0
  # pnls <- 0.01*pnls/pnlm
  mean(pnls)/sd(pnls)
}) # end sapply
plot(expv, pnlc, t="l", xlab="Exponent", ylab="Sharpe ratio",
     main="Exponent vs. Sharpe ratio")
expp <- expv[which.max(pnlc)]
lambdaf <- expv[which.max(pnlc)]

endw <- rutils::calc_endpoints(pnls, interval="weeks")
captiont <- paste0("EMA SPY", " / lambdaf=", lambdaf, " / expp=", expp)
dygraph(cumsum(pnls)[endw], main=captiont) %>%
  dyOptions(colors=c("blue"), strokeWidth=2)

wealthv <- cbind(spy, pnls, 0.5*(spy+pnls))
colnames(wealthv) <- c("Stocks", "Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2)
dygraph(cumsum(wealthv$Combined)[endw], main=captiont) %>%
  dyOptions(colors=c("blue"), strokeWidth=2)

# Calculate the static alpha portfolios for all the stocks - better than just trading the stocks
alphac <- lapply(retstock, function(x) {
  # cat("Calculating alpha for", colnames(x), "\n")
  retp <- cbind(x, spy)
  retp <- na.omit(retp)
  # Calculate the downside returns
  retn <- retp
  retn[retn > 0] <- 0
  # cat("retp =", NROW(retp), "\n")
  # Calculate the downside beta and alpha
  if (NROW(retp) > 10) {
    betac <- drop(cov(retn[, 1], retn[, 2])/var(retn[, 2]))
    # Return the alpha
    return (retp[, 1] - betac*retp[, 2])
  }
}) # end lapply
alphac <- do.call(cbind, alphac)
alphac$SPY <- spy[-1]


# Calculate the running alpha portfolios for all the stocks
alphac <- lapply(retstock, function(x) {
  # cat("Calculating alpha for", colnames(x), "\n")
  retp <- cbind(x, spy)
  retp <- na.omit(retp)
  # Calculate the downside returns
  retn <- retp
  retn[retn > 0] <- 0
  # cat("retp =", NROW(retp), "\n")
  # Calculate the downside beta and alpha
  if (NROW(retp) > 10) {
    covarv <- HighFreq::run_covar(retn, 0.9)
    covarv <- rutils::lagit(covarv)
    covarv[1, ] <- 1.0
    betac <- covarv[, 1]/covarv[, 3]
    # Return the alpha
    return (retp[, 1] - betac*retp[, 2])
  }
}) # end lapply
alphac <- do.call(cbind, alphac)
alphac$SPY <- spy[-1]


# Calculate the trailing Kelly ratios of the stocks using C++
# alpham <- HighFreq::run_var(alphac, lambdaf=lambdaf)
alpham <- HighFreq::run_var(retstock, lambdaf=lambdaf)
volm <- alpham[, (nstocks+1):(2*nstocks)]
volm[is.na(volm)] <- 0.0001
volm[volm < 0.0001] <- 0.0001
kellyr <- alpham[, 1:nstocks]/volm
colnames(kellyr) <- colnames(retstock)

# Calculate the EMA returns
retm <- alpham[, 1:nstocks]

# Calculate the contrarian strategy weights equal to the EMA returns
weightm <- -retm
weightm[is.na(weightm)] <- 0
weightm[1, ] <- 0
weightm <- rutils::lagit(weightm)
# weightm <- (weightm - rowMeans(weightm, na.rm=TRUE))
# weightm[weightm < 0] <- 0

# Calculate the average of the absolute returns - proxy for the volatility
retd <- matrixStats::rowMeans2(abs(alphac), na.rm=TRUE)
retd <- matrixStats::rowMeans2(abs(retstock), na.rm=TRUE)
plot(retd, t="l")

# Calculate the contrarian strategy weights equal to the EMA returns
weightm <- matrix(NA, nrow=NROW(retstock), ncol=nstocks)
weightm[retm < threshd] <- -retm[retm < threshd]
# weightm[retm < -retd] <- 1
weightm[retm > 0] <- 0
weightm[1, ] <- 0
weightm <- zoo::na.locf(weightm)
weightm <- rutils::lagit(weightm)

# BuyDip strategy parameters
lambdaf <- 0.8
threshd <- -0.05
threshvar <- 0.4
nums <- 10

# Calculate the BuyDip strategy weights
weightm <- run_weights(retstock, lambdaf=lambdaf, nums=nums, threshd=threshd, threshvar=threshvar)
weightm <- rutils::lagit(weightm)
colnames(weightm) <- colnames(retstock)
weightm <- xts(weightm, datev)

sum(weightm)
foo <- sapply(weightm, function(x) {
  sum(abs(x))
}) # end sapply
foo <- sort(foo, decreasing=TRUE)
head(foo)

dygraph(weightm$BBBY)

weightsum <- rowSums(weightm)
plot(weightsum, t="l")
weightm <- weightm/abs(weightsum)

foo <- cbind(cumsum(spy), weightsum)
colv <- colnames(foo)
dygraphs::dygraph(foo, main="SPY and Weights") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)


# Calculate the contrarian strategy PnLs - multiply retstock
# pnlm <- weightm*alphac
pnlm <- weightm*retstock
pnls <- rowSums(pnlm, na.rm=TRUE)
pnls <- xts(pnls, datev)
endw <- rutils::calc_endpoints(pnls, interval="weeks")
captiont <- paste0("BuyDip", " / lambdaf=", lambdaf, " / nums=", nums, " / threshd=", threshd, " / threshvar=", threshvar)
dygraph(cumsum(pnls)[endw], main=captiont) %>%
  dyOptions(colors=c("blue"), strokeWidth=2)

sum(is.na(pnlm))
symboln <- "BBBY"
dygraph(cumsum(get(symboln, pnlm))[endw], main=symboln) %>%
  dyOptions(colors=c("blue"), strokeWidth=2)
dygraph(cumsum(get(symboln, retstock))[endw], main=symboln) %>%
  dyOptions(colors=c("blue"), strokeWidth=2)

volv <- sapply(retstock, function(x) {
  x <- x[abs(x) > 0]
  sd(x, na.rm=TRUE)
}) # end sapply

pnlsum <- sapply(pnlm, sum)

plot(volv, pnlsum, cex=0.5, xlim=c(0, 0.05), 
     xlab="Volatility", ylab="PnL",
     main="Contrarian Strategy PnLs")

dygraph(cumsum(xts(rowSums(pnlm[, volv < 0.05], na.rm=TRUE), datev))[endw])


pnlsum <- sort(pnlsum, decreasing=TRUE)
head(pnlsum)
tail(pnlsum)

# Compare the contrarian strategy with SPY
spy <- rutils::etfenv$returns$SPY[datev]
# pnls <- sd(spy[spy<0])*pnls/sd(pnls[pnls<0])
wealthv <- cbind(spy, pnls)
colnames(wealthv) <- c("Stocks", "Strategy")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="BuyDip Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



####################################
# Kitchen sink strategy using returns, the log trading volumes, and volatility.
# The log differences of the trading volumes have no forecasting ability.
# The log of the trading volumes have small forecasting ability, but they failed in March 2020.
# The volatility has small forecasting ability for very small lambda.

ohlc <- rutils::etfenv$SPY
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- "SPY"
retp <- rutils::diffit(log(closep))
retv <- sqrt(HighFreq::run_var(retp, lambdaf=0.1)[, 2])
# Predictor matrix
respv <- retp["2010/"]
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0("SPY", 1:orderp)
# Add the volatility of SPY
predx <- lapply(1:orderp, rutils::lagit, input=retv)
predx <- rutils::do_call(cbind, predx)
colnames(predx) <- paste0("Vol", 1:orderp)
predm <- cbind(predm, predx)
# Add the volume of SPY
volumv <- rutils::diffit(log(quantmod::Vo(ohlc))["2010/"])
predx <- lapply(1:orderp, rutils::lagit, input=volumv)
predx <- rutils::do_call(cbind, predx)
colnames(predx) <- paste0("Vol", 1:orderp)
predm <- cbind(predm, predx)
# predm <- lapply(predm, function(x) {
#   x/sd(x)
# }) # end lapply
# predm <- do.call(cbind, predm)

regmod <- lm(respv ~ predm - 1)
summary(regmod)

regmod <- lm(respv["/2019"] ~ predm["/2019"] - 1)
summary(regmod)

coeff <- coef(summary(regmod))[, 3]

predinv <- MASS::ginv(predm["/2019"])
coeff <- drop(predinv %*% respv["/2019"])

names(coeff) <- colnames(predm)
barplot(coeff, xlab="", ylab="coeff", col="grey",
        main="Coefficients of Kitchen Sink Autoregressive Model")
# Calculate the in-sample forecasts of SPY (fitted values)
fcasts <- predm %*% coeff
pnls <- respv*fcasts
pnls <- pnls*sd(respv[respv<0])/sd(pnls[pnls<0])

wealthv <- cbind(respv, pnls)
colnames(wealthv) <- c("SPY", "Kitchen sink")
sqrt(252)*sapply(wealthv["/2019"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv["2019/"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endd], 
                  main="Kitchen Sink Autoregressive Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(as.Date("2019-01-01"), label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)



####################################
# Daily trading strategy for XLK and NVDA

# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

retp <- na.omit(rutils::etfenv$returns$XLK - 1.7*retstock$NVDA)

dygraph(cumsum(retp))
pacf(retp)

### Autoregressive strategy for cointegrated AAPL and XLK

nrows <- NROW(retp)
# Define the response and predictor matrices
orderp <- 21
predm <- lapply(1:orderp, rutils::lagit, input=retp)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0("lag", 1:orderp)

# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retp
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
resids <- (fcasts - retp)
vars <- sum(resids^2)/(nrows-NROW(coeff))
pred2 <- crossprod(predm)
covmat <- vars*MASS::ginv(pred2)
coeffsd <- sqrt(diag(covmat))
coefft <- drop(coeff/coeffsd)
coeffn <- paste0("phi", 0:(NROW(coefft)-1))
barplot(coefft ~ coeffn, xlab="", ylab="t-value", col="grey",
        main="Coefficient t-values of AR Forecasting Model")
fcastv <- sqrt(HighFreq::run_var(fcasts, lambdaf=0.2)[, 2])
fcastsc <- ifelse(fcastv > 0, fcasts/fcastv, 0)
# Trade the scaled forecasts
pnls <- retp*fcastsc
# Trade only on very large forecasts - not really better
threshz <- 5
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(fcastsc > threshz, 1, posv)
posv <- ifelse(fcastsc < (-threshz), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
pnls <- retp*posv


pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("Stocks", "Strategy")
wealthv <- xts(wealthv, index(retp))
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv), main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



####################################
# Intraday trading strategy for SPY and NVDA

pricel <- lapply(names(spy), function(x) {
  datav <- na.omit(cbind(spy[[x]][, 1], nvda[[x]][, 1]))
  datav[, 1] - 4*datav[, 2]
}) # end lapply

datav <- pricel[[1]]
colv <- colnames(datav)
dygraphs::dygraph(datav, main="SPY/NVDA Pair") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)

dygraph(datav, main="SPY/NVDA Pair") %>%
  dyLegend("always", width=300)



####################################

pricev <- na.omit(rutils::etfenv$prices[, c("XLK", "SPY")])
retp <- rutils::diffit(pricev)
betav <- seq(1.0, 3.0, 0.1)
foo <- sapply(betav, function(betac) {
  retp <- (retp[, 2] - betac*retp[, 1])
  sum(pacf(retp, lag=10, plot=FALSE)$acf)
}) # end sapply
plot(betav, foo, type="l", xlab="Beta", ylab="Sum of PACF", main="Sum of PACF for XLK/SPY Pair")

# Load the prices
dtable <- data.table::fread("/Users/jerzy/Develop/data/raw/state_XLK2SPY1RatchetpricEMAzScoreLamb95Volf70_20250304.csv")
datev <- as.POSIXct(dtable$timeMillisec/1e3, origin="1970-01-01", tz="America/New_York")
dtable <- dtable[, -2]
dtable <- lapply(dtable, as.numeric)
dtable <- do.call(cbind, dtable)
dtable <- xts::xts(dtable, order.by=datev)

# Compress the prices by removing the unchanged prices
retp <- rutils::diffit(dtable$pricePortf)
dtable <- dtable[!(retp==0), ]

dygraphs::dygraph(dtable[, c("pricePortf", "priceRef")], main="XLK/SPY Pair") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

datav <- dtable[, c("pricePortf", "zScore")]
colv <- colnames(datav)
dygraphs::dygraph(datav, main="XLK/SPY Pair") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)




####################################
# Daily stock low volatility strategies

# VTI returns
retv <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retv) # Dates vector
# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- retstock[datev]
nrows <- NROW(retp) # number of rows
nstocks <- NCOL(retp) # number of stocks
# To simplify, set NAs to zero
# retp[is.na(retp)] <- 0

# Calculate the average of all stock returns
retew <- rowMeans(retp, na.rm=TRUE)
volew <- sd(retew)

# Define backtest functional for daily low volatility strategy
# If trend=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(retp, lambdaf=0.8, varf=2e-5, prob=0.5, trend=1, bidask=0.0, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate the trailing Kelly ratio
  nstocks <- NCOL(retp)
  varm <- HighFreq::run_var(retp, lambdaf=lambdaf)
  meanm <- varm[, 1:nstocks]
  vars <- varm[, (nstocks+1):(2*nstocks)]
  # varmean <- rowMeans(vars, na.rm=TRUE)
  # meanm[is.na(meanm) | is.nan(meanm)] <- 0
  # vars[is.na(vars) | is.nan(vars) | vars==0] <- 1
  # vars[is.na(vars) | is.nan(vars)] <- 1
  # vars[vars < varf] <- varf
  # varq <- matrixStats::rowQuantiles(vars, probs=prob, na.rm=TRUE)
  # Calculate the trailing inverse volatility
  # weightv <- 1/vars
  weightv <- ifelse(vars < varf, 1, 0)
  # weightv <- ifelse(vars < varq, 1/vars, 0)
  # weightv <- ifelse(vars < varq, meanm/vars, 0)
  # weightv <- weightv/rowSums(weightv, na.rm=TRUE)
  # Calculate the trailing Kelly ratio
  # weightv <- meanm/vars
  weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))
  weightv <- rutils::lagit(weightv)
  # Calculate the momentum profits and losses
  pnls <- trend*rowSums(weightv*retp, na.rm=TRUE)
  # Calculate the transaction costs
  # costv <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)), na.rm=TRUE)
  costv <- 0
  return (pnls - costv)
}  # end btmomdaily

# Simulate multiple daily stock momentum strategies
lambdav <- seq(0.5, 0.9, 0.1)
pnls <- sapply(lambdav, btmomdaily, retp=retp, varf=5.4e-5, trend=1)

volv <- 1e-5*seq(2.5, 6.5, 0.5)
pnls <- sapply(volv, btmomdaily, retp=retp, lambdaf=0.8)

probs <- seq(0.2, 0.9, 0.1)
pnls <- sapply(probs, btmomdaily, retp=retp, lambdaf=0.8, varf=2e-5)

# pnls[1:100, ] <- 0
# pnls[is.na(pnls) | is.nan(pnls)] <- 0
# Scale the momentum volatility to the equal weight index
# pnls <- apply(pnls, MARGIN=2, function(pnl) volew*pnl/sd(pnl))
pnls <- xts::xts(pnls, datev)
# colnames(pnls) <- paste0("lambda=", lambdav)
sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
colnames(pnls) <- paste0("lambda=", lambdav, " / sharpe=", sharper)
# colnames(pnls) <- paste0("volf=", volv, " / sharpe=", sharper)
# colnames(pnls) <- paste0("prob=", probs, " / sharpe=", sharper)
# sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))

# Plot dygraph of daily stock low volatility strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
endd <- rutils::calc_endpoints(retp, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
                  main="Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Combine the low volatility strategy with VTI
pnlb <- pnls[, which.max(sharper)]
pnlb <- sd(retv)*pnlb/sd(pnlb)
combv <- na.omit(cbind(retv, pnlb))
colnames(combv) <- c("VTI", "LoVol")
colv <- colnames(combv)
sharper <- sqrt(252)*sapply(combv, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
captiont <- paste("LoVol Strategy", "/ \n",
                  paste0(paste0(colv, " SR="), sharper, collapse=" / "))
dygraphs::dygraph(cumsum(combv)[endd], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



####################################
# Intraday strategy with position is equal to the sign of the price difference

# Load intraday minute prices for SPY
load(file="/Users/jerzy/Develop/data/SPY_minute_202425.RData")
pricev <- pricel[[300]]
pricev <- Cl(pricev)
colnames(pricev) <- "SPY"
retp <- rutils::diffit(pricev)

# Simulate the strategy
# The position is equal to the sign of the price difference
posv <- sign(pricev - as.numeric(pricev[1]))
posv[1] <- 0
posv <- rutils::lagit(posv)
pnls <- retp*posv
# Plot the strategy
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("Stocks", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv), main="Switching Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

bidask <- 0.01
wealthv <- lapply(pricel, function(pricev) {
  # pricev <- Cl(pricev)
  pricev <- pricev["T09:30:00/T16:00:00"]
  retp <- rutils::diffit(pricev)
  posv <- sign(pricev - as.numeric(pricev[1]))
  posv[1] <- 0
  posv <- rutils::lagit(posv)
  pnlv <- retp*posv
  costv <- bidask*abs(rutils::diffit(posv))
  pnlv <- pnlv - costv
  cbind(retp, pnlv)
}) # end lapply
wealthv <- do.call(rbind, wealthv)

colnames(wealthv) <- c(symboln, "Strategy")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv), main="Switching Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)


pricel <- lapply(filev, function(filen) {
  cat("file: ", filen, "\n")
  dtable <- data.table::fread(filen)
  datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
  pricev <- xts::xts(dtable[, 2], order.by=datev)
  pricev <- pricev["T09:30:00/T16:00:00"]
  colnames(pricev)[1] <- symboln
  retp <- rutils::diffit(pricev)
  # Compress the prices by removing the unchanged prices
  retp[1, ] <- 1
  pricev[!(retp==0), ]
}) # end lapply
namev <- as.Date(sapply(pricel, function(x) as.Date(end(x))))
namev <- unname(namev)
names(pricel) <- namev
filen <- paste0(dirp, symboln, datan, ".RData")
save(pricel, file=filen)


# Prices of SPY vs QQQ pair
pricel <- lapply(seq_along(qqq), function(it) {
  cbx <- na.omit(cbind(qqq[[it]], spy[[it]]))
  cbx[, 1] - cbx[, 2]
  # (qqq[[it]] - spy[[it]])
}) # end lapply


# Calculate the distribution of intraday stock price crossings of the open price.
crossv <- lapply(pricel, function(pricev) {
  
  # pricev <- pricev[, 1]
  # pricev <- pricev["T09:30:00/T16:00:00"]
  posv <- sign(pricev - as.numeric(pricev[1]))
  # posv[1] <- 0
  which(as.logical(rutils::diffit(posv)))[-1]
  # indeks <- as.numeric(index(pricev))
  
}) # end lapply
crossv <- do.call(c, crossv)
plot(density(crossv))

# Brownian motion
crossv <- lapply(pricel, function(pricev) {
  pricev <- cumsum(rnorm(NROW(pricev)))
  posv <- sign(pricev - pricev[1])
  which(as.logical(rutils::diffit(posv)))[-1]
}) # end lapply
crossv <- do.call(c, crossv)
plot(density(crossv))



####################################
# Daily ETF momentum strategy using the alpha portfolios with respect to SPY.

# Select ETF returns
symbolv <- colnames(rutils::etfenv$returns)
# symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ", "VYM"))]
symbolv <- symbolv[!(symbolv %in% c("MTUM", "QUAL", "VLUE", "USMV", "AIEQ", "VYM"))]
# Select small number of ETF returns
symbolv <- c("SPY", "VXX", "SVXY", "XLV", "XLP", "XLU", "XLK", "XLB", "XLI", "XLF", "XLE", "USO", "XLY", "EEM", "GLD", "VNQ", "TLT")
# Select ETF returns without VXX, SVXY, and USO
symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "USO"))]
retstock <- rutils::etfenv$returns[, symbolv]

nstocks <- NCOL(retstock)

# Calculate the trailing Kelly ratios of the alphas using lapply()
# kellyr <- lapply(alphac, function(x) {
#   cat("Calculating alpha for", colnames(x), "\n")
#   retm <- HighFreq::run_var(x, lambdaf)
#   retm[, 1]/retm[, 2]
# }) # end lapply
# kellyr <- do.call(cbind, kellyr)

# Calculate the trailing Kelly ratios of the alpha portfolios using C++
# alpham <- alphac
# alpham[alpham > 0.0] <- 0.0
# alpham <- HighFreq::run_var(alpham, lambdaf=lambdaf)
alpham <- HighFreq::run_var(alphac, lambdaf=lambdaf)
volm <- alpham[, (nstocks+1):(2*nstocks)]
volm[is.na(volm)] <- 0.0001
volm[volm < 0.0001] <- 0.0001
kellyr <- alpham[, 1:nstocks]/volm
# kellyr <- 1/volm
colnames(kellyr) <- colnames(retstock)

# Calculate the contrarian strategy PnLs - multiply retstock
weightm <- -alpham[, 1:nstocks]
weightm[is.na(weightm)] <- 0
weightm <- rutils::lagit(weightm)
# weightm <- (weightm - rowMeans(weightm, na.rm=TRUE))
# weightm[weightm < 0] <- 0
pnls <- rowSums(weightm*alphac, na.rm=TRUE)
pnls <- xts(pnls, index(alphac))
dygraph(cumsum(pnls))


# Calculate the trending strategy PnLs - multiply the alphac, not retstock
weightm <- kellyr
weightm[is.na(weightm)] <- 0
weightm <- rutils::lagit(weightm)
weightm <- weightm/sqrt(rowSums(weightm^2, na.rm=TRUE))
weightm[is.na(weightm)] <- 0
# Select only positive weights
weightm[weightm < 0] <- 0
pnls <- rowSums(weightm*alphac, na.rm=TRUE)
pnls <- xts(pnls, index(alphac))
dygraph(cumsum(pnls))

kellym <- rowMeans(kellyr, na.rm=TRUE)
kellym[1] <- 0
plot(kellym, t="l")
kellyg <- kellyr
kellyg[is.na(kellyg)] <- 0
weightm <- ifelse(kellyg < kellym, 1, -1)

pnls <- pnls[datev]
pnls <- sd(spy)*pnls/sd(pnls)
wealthv <- cbind(spy, pnls)
colnames(wealthv) <- c("Stocks", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Lowvol Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



####################################
# Daily low downside volatility stock momentum strategy.

# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Remove the stocks with recent NA returns
retstock <- retstock[, !is.na(retstock[NROW(retstock)])]
nstocks <- NCOL(retstock)

# Calculate the trailing downside volatilities
lambdaf <- 0.9
# Downside volatility doesn't improve performance that much
retn <- retstock
retn[retn > 0] <- 0
varm <- HighFreq::run_var(retn, lambdaf=lambdaf)
varm <- varm[, (nstocks+1):(2*nstocks)]
varm[is.na(varm)] <- 0.00001
varm[varm < 0.00001] <- 0.00001
kellyr <- 1/varm
colnames(kellyr) <- colnames(retstock)

# Calculate the low volatility strategy PnLs
weightm <- kellyr
weightm[is.na(weightm)] <- 0
weightm <- rutils::lagit(weightm)
pnls <- rowSums(weightm*retstock, na.rm=TRUE)
pnls <- xts(pnls, index(retstock))
dygraph(cumsum(pnls))

# Compare the low volatility strategy with SPY
pnls <- pnls[datev]
pnls <- sd(spy)*pnls/sd(pnls)
wealthv <- cbind(spy, pnls)
colnames(wealthv) <- c("Stocks", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Lowvol Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


