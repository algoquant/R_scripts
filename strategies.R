################################################
# Daily stock momentum strategy using the alpha portfolios with respect to SPY.

# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Select top stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returnstop.RData")
# Subset to the SPY index
spy <- na.omit(rutils::etfenv$returns$SPY)
datev <- index(spy)
# retstock <- retstock[datev]
# retstock <- cbind(retstock, spy)

lambdaf <- 0.3

# Calculate the static alpha portfolios for all the stocks - no advantage compared to just stocks
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
    covarv <- HighFreq::run_covar(retn, lambdaf)
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
alpham <- HighFreq::run_var(retstock, lambda=lambdaf)
volm <- alpham[, (nstocks+1):(2*nstocks)]
volm[is.na(volm)] <- 0.0001
volm[volm < 0.0001] <- 0.0001
kellyr <- alpham[, 1:nstocks]/volm
colnames(kellyr) <- colnames(retstock)

# Calculate the contrarian strategy PnLs - multiply retstock
weightv <- -alpham[, 1:nstocks]
weightv[is.na(weightv)] <- 0
weightv <- rutils::lagit(weightv)
# weightv <- (weightv - rowMeans(weightv, na.rm=TRUE))
# weightv[weightv < 0] <- 0
pnls <- rowSums(weightv*retstock, na.rm=TRUE)
pnls <- xts(pnls, index(retstock))
dygraph(cumsum(pnls))

# Compare the contrarian strategy with SPY
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



################################################
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
# alpham <- HighFreq::run_var(alpham, lambda=lambdaf)
alpham <- HighFreq::run_var(alphac, lambda=lambdaf)
volm <- alpham[, (nstocks+1):(2*nstocks)]
volm[is.na(volm)] <- 0.0001
volm[volm < 0.0001] <- 0.0001
kellyr <- alpham[, 1:nstocks]/volm
# kellyr <- 1/volm
colnames(kellyr) <- colnames(retstock)

# Calculate the contrarian strategy PnLs - multiply retstock
weightv <- -alpham[, 1:nstocks]
weightv[is.na(weightv)] <- 0
weightv <- rutils::lagit(weightv)
# weightv <- (weightv - rowMeans(weightv, na.rm=TRUE))
# weightv[weightv < 0] <- 0
pnls <- rowSums(weightv*alphac, na.rm=TRUE)
pnls <- xts(pnls, index(alphac))
dygraph(cumsum(pnls))


# Calculate the trending strategy PnLs - multiply the alphac, not retstock
weightv <- kellyr
weightv[is.na(weightv)] <- 0
weightv <- rutils::lagit(weightv)
weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))
weightv[is.na(weightv)] <- 0
# Select only positive weights
weightv[weightv < 0] <- 0
pnls <- rowSums(weightv*alphac, na.rm=TRUE)
pnls <- xts(pnls, index(alphac))
dygraph(cumsum(pnls))

kellym <- rowMeans(kellyr, na.rm=TRUE)
kellym[1] <- 0
plot(kellym, t="l")
kellyg <- kellyr
kellyg[is.na(kellyg)] <- 0
weightv <- ifelse(kellyg < kellym, 1, -1)

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



################################################
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
varm <- HighFreq::run_var(retn, lambda=lambdaf)
varm <- varm[, (nstocks+1):(2*nstocks)]
varm[is.na(varm)] <- 0.00001
varm[varm < 0.00001] <- 0.00001
kellyr <- 1/varm
colnames(kellyr) <- colnames(retstock)

# Calculate the low volatility strategy PnLs
weightv <- kellyr
weightv[is.na(weightv)] <- 0
weightv <- rutils::lagit(weightv)
pnls <- rowSums(weightv*retstock, na.rm=TRUE)
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


