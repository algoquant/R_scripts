##############################
### R scripts for backtesting trading strategies.


# Select the ETFs for the portfolio
symbolv <- c("VXX", "USO", "VTI", "XLK", "TLT")
retp <- na.omit(rutils::etfenv$returns["2018-06/", symbolv])
datev <- index(retp)
nweights <- NCOL(retp)
indeks <- xts(retp %*% rep(1/sqrt(nweights), nweights), datev)
# Calculate the stock betas
betav <- sapply(retp, function(x) {
  cov(retp$VTI, x)/var(retp$VTI)
}) # end sapply

# Simulate the portfolio strategy
lambdaf <- 0.981
pnls <- back_testx(retp=retp, betav=betav, dimax=dimax, lambda=lambdaf)
# Calculate the portfolio beta
betats <- cbind(cumsum(pnls[, 1]), pnls[, 2])
betats <- xts(betats, datev)
# Plot the portfolio beta
colnames(betats) <- c("Pnl", "Beta")
colnamev <- colnames(betats)
captiont <- paste("Running Portfolio Strategy for", paste(symbolv, collapse=", "))
dygraphs::dygraph(betats, main=captiont) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=500)

pnlv <- cbind(retp$VTI, pnls[, 2]*retp$VTI)
pnlv <- xts(pnlv, datev)
colnames(pnlv) <- c("VTI", "PnLs")
dygraph(cumsum(pnlv))



##############################
# The patient ratchet pair strategy.

# Load the reference prices
pricev <- lapply(4:6, function(x) {
  load(paste0("/Users/jerzy/Develop/data/SPY_minute_20240", x, ".RData"))
  do.call(rbind, pricel)
}) # end lapply
pricev <- do.call(rbind, pricev)
priceref <- pricev
# Calculate the symbol name
symbolref <- rutils::get_name(colnames(priceref))

# Load the target prices
pricev <- lapply(4:6, function(x) {
  load(paste0("/Users/jerzy/Develop/data/XLK_minute_20240", x, ".RData"))
  do.call(rbind, pricel)
}) # end lapply
pricev <- do.call(rbind, pricev)
pricetarg <- pricev
# Calculate the symbol name
symboltarg <- rutils::get_name(colnames(pricetarg))

# Calculate the pair prices
betac <- 1.0
pricev <- pricetarg - betac*priceref
retv <- rutils::diffit(pricev)

# Simulate the patient ratchet strategy in strat_ratchet.R
zfact <- 1.5
lambdaf <- 0.95
pospnls <- ratchet_patient(pricev, lambdaf=lambdaf, zfact=zfact)
# Calculate the strategy PnLs
pnls <- pospnls[, 1]

# Calculate the number of trades
flipi <- rutils::diffit(pospnls[, 2])
ntrades <- sum(abs(flipi) > 0)

# Calculate transaction costs
bidask <- 0.1
costs <- 0.5*bidask*abs(flipi)
pnls <- (pnls - costs)

# Bind the PnLs with the pair returns
pnls <- cbind(retv, pnls)
colnames(pnls) <- c("Pair", "Strategy")

## Calculate the overnight dates - the first time stamp of the day
daton <- xts::.index(pricev)
daton <- rutils::diffit(daton)
daton <- which(daton > 1000)
pricev[daton]
# Calculate the overnight PnLs
pnlon <- pnls[daton, 2]
sum(pnlon)
# Plot the overnight PnLs
dygraph(cumsum(pnlon))


# Calculate the Sharpe ratios
sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)

# Plot the strategy PnLs
captiont <- paste0("Ratchet Strategy For ", symboltarg, "/", symbolref)
captiont <- paste(captiont, "\n", 
                  paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                  "Number of trades=", ntrades)
colnamev <- colnames(pnls)
dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=500)
