############## research
## Star50 Chinese index prices

# Download the Star50 prices
star50 <- data.table::fread("/Users/jerzy/Downloads/Star50_Data.csv", tz="")
# Convert the timestamp column to Date
datev <- as.Date(star50$Date, format="%m/%d/%Y", tz="America/New_York")
# Coerce columns to numeric
star50 <- star50[, -1]
star50 <- sapply(star50, function(x) {
  x <- gsub(",", "", x)
  x <- gsub("M", "000000", x)
  x <- gsub("B", "000000000", x)
  x <- gsub("K", "000", x)
  x <- gsub("%", "", x)
  return(as.numeric(x))
}) # end sapply  
# Convert to xts and plot
star50 <- xts::xts(star50, order.by=datev)
dygraph(star50$Price, main="Star50 Prices") %>%
  dyOptions(colors=c("blue"), strokeWidth=1)


############## research
## Chain together the intraday PnLs

# Get all the file names
stratn <- "Ladder"
datan <- "pnls"
# Get all the file names for the symbol MU
filev <- Sys.glob("/Users/jerzy/Develop/trading/logs/Ladder_pnls*.csv")
# filev <- filev[1:8]

pnls <- lapply(filev, function(filen) {
  pnls <- data.table::fread(filen, tz="")
  datev <- as.POSIXct(pnls$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  pnls <- xts::xts(pnls[, -1], order.by=datev)
  return(pnls)
}) # end lapply
pnls <- do.call(rbind, pnls)

# symboln <- "SPY"
pnlc <- rowSums(pnls)
pnlc <- xts::xts(pnlc, order.by=index(pnls))
dygraph(pnlc, main="Intraday PnLs")


# Download the intraday PnLs from Alpaca and save them to a CSV file.
# Rscript /Users/jerzy/Develop/R/alpaca_pnls.R /Users/jerzy/Develop/trading/config/.env_ladder
filen <- "/Users/jerzy/Develop/R/alpaca_pnls_20260923_20261002.csv"
pnls <- data.table::fread(filen, tz="")
datev <- as.POSIXct(pnls$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
pnls <- xts::xts(pnls$profit_loss, order.by=datev)
dygraph(pnls, main="Alpaca Intraday PnLs")



############## research
## Chain together the intraday minute prices for all the symbols for all the dates.

# Specify the symbols to load
symbolv <- c("MU", "SPY", "META", "AMAT", "AMD", "MSFT", "AVGO", "GOOGL", "JPM", "PANW", "TSLA", "AAPL", "AMZN", "NVDA", "CRM", "XLK", "XOM", "QCOM", "COIN", "ORCL", "PLTR", "WMT", "INTC", "NFLX", "CRWV", "SPCX")
# Get all the file names for the symbol MU
filev <- Sys.glob("/Users/jerzy/Develop/data/minutes/live/MU_*.csv")
# Extract the dates from the file names
datev <- gsub(".*MU_(\\d{8})\\.csv", "\\1", filev)

# Load the minute prices for each symbol, chain them, and save them to parquet files
library(nanoparquet)
for (symboln in symbolv) {
  cat("Loading minute prices for", symboln, "\n")
  filev <- paste0("/Users/jerzy/Develop/data/minutes/live/", symboln, "_", datev, ".csv")
  # Select only the files that exist
  filev <- filev[file.exists(filev)]
  ohlc <- lapply(filev, function(filen) {
    ohlc <- data.table::fread(filen, tz="")
    datev <- as.POSIXct(ohlc$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
    ohlc <- xts::xts(ohlc[, -(1:2)], order.by=datev)
    return(ohlc)
    # return(ohlc[, -1])
  }) # end lapply
  ohlc <- do.call(rbind, ohlc)
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "Trades", "VWAP")
  # colnames(ohlc) <- c("timestamp", "Open", "High", "Low", "Close", "Volume", "Trades", "VWAP")
  # filen <- paste0("/Users/jerzy/Develop/data/minutes/chained/", symboln, ".parquet")
  filen <- paste0("/Users/jerzy/Develop/data/minutes/chained/", symboln, ".RData")
  # nanoparquet::write_parquet(ohlc, filen)
  save(ohlc, file=filen)
} # end for

# Load the chained prices for a specific symbol and plot them
symboln <- "SPY"
filen <- paste0("/Users/jerzy/Develop/data/minutes/chained/", symboln, ".parquet")
ohlc <- nanoparquet::read_parquet(filen)
datev <- as.POSIXct(ohlc$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
ohlc <- xts::xts(ohlc, order.by=datev)
# load(filen)
dygraph(ohlc$Close, main=paste(symboln, "Minute Prices"))


# Load the chained prices for all the symbols and cbind their Close prices into a single xts object
pricel <- lapply(symbolv, function(symboln) {
  filen <- paste0("/Users/jerzy/Develop/data/minutes/chained/", symboln, ".RData")
  load(filen)
  pricev <- ohlc$Close
  colnames(pricev) <- symboln
  return(pricev)
}) # end lapply
pricestock <- do.call(cbind, pricel)
pricestock <- zoo::na.locf(pricestock, na.rm=FALSE)
pricestock <- zoo::na.locf(pricestock, fromLast=TRUE)
dygraph(pricestock[, symboln], main=paste(symboln, "Minute Prices"))
# Save the combined prices to an RData file
save(pricestock, file="/Users/jerzy/Develop/data/minutes/chained/stock_prices.RData")
retstock <- rutils::diffit(pricestock)


# Calculate the trailing variance and z-scores of prices
lambdaf <- 0.99 # Decay factor
zscores <- HighFreq::run_zscores(pricestock[, symboln], lambdaf=lambdaf)
datav <- cbind(pricestock[, symboln], zscores[, 3])
colnames(datav) <- c(symboln, "Z-Scores")
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main=paste0(symboln, " Prices and Z-scores")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(axis="y2", label=colnamev[2], strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)



##############
# Simulate Bollinger strategies for all the stocks.

load("/Users/jerzy/Develop/data/minutes/chained/stock_prices.RData")
pricestock <- rutils::diffit(pricestock)
symbolv <- colnames(pricestock)

lambdaf <- 0.8
threshv <- 0.1

pnls <- lapply(pricestock, function(pricex) {
  # Calculate the z-scores
  zscores <- HighFreq::run_zscores(pricex, lambdaf=lambdaf)
  zscores <- zscores[, 3]
  zscores[1:11] <- 0
  # Simulate the patient Bollinger Strategy quickly
  posv <- rep(NA_integer_, NROW(pricex)) ##  Stock positions
  posv[1] <- 0
  posv <- ifelse(zscores > threshv, -1, posv)
  posv <- ifelse(zscores < -threshv, 1, posv)
  posv <- zoo::na.locf(posv)
  # Lag the positions to trade in the next period
  posv <- rutils::lagit(posv, lagg=1)
  retx <- rutils::diffit(pricex)
  pnls <- posv*retx
  return(pnls)
}) # end lapply)
pnls <- do.call(cbind, pnls)
colnames(pnls) <- symbolv
sort(sapply(pnls, function(x) sqrt(252)*mean(x)/sd(x[x<0])), decreasing=TRUE)
dygraph(cumsum(pnls$CRM), main="CRM Strategy PnLs")
pnlm <- xts::xts(rowSums(pnls), order.by=index(pnls))
dygraph(cumsum(pnlm), main="Combined Strategy PnLs") %>%
  dyOptions(colors=c("blue"), strokeWidth=1)


sharpev <- sapply(pnll, function(pnls) {
  # pnls <- pnls[!is.na(pnls)]
  pnls <- pnls["2010/"][, "Combined"]
  sharper <- sqrt(252)*mean(pnls)/sd(pnls[pnls<0])
  # sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
  # names(sharper) <- colnames(pnls)
  # sharper <- round(sharper, 3)
  return(sharper)
}) # end sapply)
# names(sharpev) <- colnames(retstock)
sharpev <- sort(sharpev, decreasing=TRUE)
head(sharpev, 33)
tail(sharpev, 33)

symboln <- "BRK-B"
pnls <- pnll[[symboln]]["2000/"]
sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
captiont <- paste0(paste0(names(sharper), " SR=", sharper), collapse=" / ")
dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)



############## research
## Analyze intraday z-scores, positions, and PnLs for a specific date.

# Load the target positions for a specific date
daten <- "20261005"
stratn <- "Ladder"
datan <- "zscore"
filen <- paste0("/Users/jerzy/Develop/trading/logs/", stratn, "_", datan, "_", daten, ".csv")
datam <- data.table::fread(filen, tz="")
datev <- as.POSIXct(datam$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
datam <- xts::xts(datam[, -1], order.by=datev)
colv <- colnames(datam)
# datam <- sign(datam)
# weightv <- data.table::fread("/Users/jerzy/Downloads/stock_weights2.csv")
# colv <- weightv$Symbol
# weightv <- weightv$Weight
# datam <- HighFreq::mult_mat(weightv, datam)
# datam <- xts::xts(datam, order.by=datev)
# colnames(datam) <- colv
symboln <- "PANW"
posym <- datam[, symboln]
colnames(posym) <- stratn
dygraph(posym, main=paste(symboln, stratn, datan, daten))

# Load the minute prices for all the symbols for daten
# From /Users/jerzy/Develop/data/minutes/live/
pricel <- lapply(colv, function(symboln) {
  file_name <- paste0("/Users/jerzy/Develop/data/minutes/live/", symboln, "_", daten, ".csv")
  ohlc <- data.table::fread(file_name, tz="")
  datev <- as.POSIXct(ohlc$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  pricev <- xts::xts(ohlc$close, order.by=datev)
  colnames(pricev) <- symboln
  return(pricev)
}) # end lapply
pricel <- do.call(cbind, pricel)
colnames(pricel) <- colv
dygraph(pricel[, symboln], main=paste(symboln, "Minute Prices"))

# Load the positions
datan <- "positions"
filen <- paste0("/Users/jerzy/Develop/trading/logs/Momentum_", stratn, "_", datan, "_", daten, ".csv")
posm <- data.table::fread(filen, tz="")
datev <- as.POSIXct(posm$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
posm <- xts::xts(posm[, -1], order.by=datev)
dygraph(posm[, symboln], main=paste(symboln, stratn, datan, daten))

# dygraph with two y-axes
datav <- na.omit(cbind(datam[, symboln], posm[, symboln]))
lab1 <- "Z-scores"
lab2 <- "Positions"
coln <- c(lab1, lab2)
colnames(datav) <- coln
dygraphs::dygraph(datav, main=paste(symboln, "Z-scores and Positions")) %>%
  dyAxis("y", label=lab1, independentTicks=TRUE) %>%
  dyAxis("y2", label=lab2, independentTicks=TRUE) %>%
  dySeries(name=coln[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=coln[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)


# Load the PnLs
stratn <- "Ladder"
datan <- "pnls"
filen <- paste0("/Users/jerzy/Develop/trading/logs/Momentum_", stratn, "_", datan, "_", daten, ".csv")
pnls <- data.table::fread(filen, tz="")
datev <- as.POSIXct(pnls$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
pnls <- xts::xts(pnls[, -1], order.by=datev)
pnlc <- sapply(pnls, last)
sort(pnlc)
# pnlc <- rowSums(pnls, na.rm=TRUE)
# pnlc <- xts::xts(pnlc, order.by=index(pnls))
# dygraph(pnlc, main=paste0("Intraday PnLs for ", daten))

# Calculate the PnLs
retm <- rutils::diffit(pricel)
retm <- retm[index(datam)]
pnlc <- -rutils::lagit(datam)*retm
# pnlc <- sapply(pnlc, function(x) sum(x, na.rm=TRUE))
# symboln <- "MU"
# dygraph(cumsum(pnlc[, symboln]), main=paste0("Intraday PnL for ", symboln))
# pnlc <- rowSums(pnlc, na.rm=TRUE)
# pnlc <- xts::xts(pnlc, order.by=index(datam))
# dygraph(cumsum(pnlc), main=paste0("Intraday PnLs for ", daten))

# dygraph with two y-axes
datav <- na.omit(cbind(pricel[, symboln], pnls[, symboln]))
lab1 <- "Prices"
lab2 <- "PnLs"
coln <- c(lab1, lab2)
colnames(datav) <- coln
dygraphs::dygraph(datav, main=paste(symboln, "Minute Prices")) %>%
  dyAxis("y", label=lab1, independentTicks=TRUE) %>%
  dyAxis("y2", label=lab2, independentTicks=TRUE) %>%
  dySeries(name=coln[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=coln[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)


############## research
## Intraday minute price bars.

ohlc1 <- data.table::fread("/Users/jerzy/Develop/data/minutes/live/AMAT_20260811.csv", tz="")
ohlc2 <- data.table::fread("/Users/jerzy/Develop/data/minutes/live/XLK_20260724.csv", tz="")
ohlc3 <- data.table::fread("/Users/jerzy/Develop/data/minutes/live/XLK_20260727.csv", tz="")

date1 <- as.POSIXct(ohlc1$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
date2 <- as.POSIXct(ohlc2$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
date3 <- as.POSIXct(ohlc3$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
datev <- c(date1, date2, date3)

pricexlk <- c(ohlc1$close, ohlc2$close, ohlc3$close)
pricexlk <- xts::xts(pricexlk, order.by=datev)
dygraph(pricexlk, main="SPY Minute Prices") %>%
  dyOptions(colors=c("blue"), strokeWidth=1)

pricenvda <- c(ohlc1$close, ohlc2$close, ohlc3$close)
pricenvda <- xts::xts(pricenvda, order.by=datev)
dygraph(pricenvda, main="NVDA Minute Prices") %>%
  dyOptions(colors=c("red"), strokeWidth=1)

# Align the two price series to the same time index
pricev <- merge(pricexlk, pricenvda, join="inner")

# dygraph with two y-axes
dygraphs::dygraph(pricev, main="XLK and NVDA Minute Prices") %>%
  dyAxis("y", label="XLK", independentTicks=TRUE) %>%
  dyAxis("y2", label="NVDA", independentTicks=TRUE) %>%
  dySeries(name="pricexlk", axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name="pricenvda", axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate the intraday returns for SPY and NVDA
retp <- rutils::diffit(pricev)
retp <- retp[-which.max(abs(rowSums(retp)))]
volv <- sapply(retp, sd)

# Combine the two returns into a single portfolio return
pricec <- pricev$pricenvda - pricev$pricexlk*volv["pricenvda"]/volv["pricexlk"]
betac <- 1.0
pricec <- pricev$pricenvda - betac*pricev$pricexlk
colnames(pricec) <- "combined"
pricec <- pricec["T09:30/T16:00"]
dygraph(pricec, main=paste0("Combined Minute Prices / betac = ", betac)) %>%
  dyOptions(colors=c("blue"), strokeWidth=1)
  

dygraph(pricenvda["T09:30/T16:00"], main="NVDA Minute Prices") %>%
  dyOptions(colors=c("red"), strokeWidth=1)

foo <- xts::xts(ohlc3$close, order.by=date3)
foo <- foo["T09:30/T16:00"]


############## research
## OHLC price range as a contrarian indicator.
# The intraday Close price range relative to the High-Low range 
# can be used as a contrarian indicator.
# But it works best during periods of high volatility.

symboln <- "SPY"
ohlc <- log(get(symboln, rutils::etfenv))
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)  ##  Daily returns
openp <- quantmod::Op(ohlc)
retd <- (closep - openp)  ##  Daytime returns
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))  ##  Overnight returns
colnames(reton) <- "overnight"
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
hilo <- highp - lowp  # Intraday High-Low range
hilol <- rutils::lagit(hilo, lagg=1)

# Calculate the range of the Close price relative to the High-Low range.
rangev <- (closep - lowp)/hilo

# Scatter plot of the range versus the overnight returns
plot(x=as.numeric(reton), y=as.numeric(rangev), 
     main="Range vs Overnight Returns", xlab="Returns", ylab="Range")

# Scatter plot of the daily returns versus the lagged range
plot(x=as.numeric(rutils::lagit(rangev)), y=as.numeric(retp), 
     main="Daily Returns vs Range", xlab="Range", ylab="Returns")

# Scatter plot of the Hi-Low range versus the overnight returns
plot(x=abs(as.numeric(reton)), y=as.numeric(hilo), 
     main="Hi-Low vs Overnight Returns", xlab="Overnight Returns", ylab="Hi-Low")

# Scatter plot of the Hi-Low range versus the overnight returns
plot(x=abs(as.numeric(reton)), y=as.numeric(retd), 
     main="Daytime vs Overnight Returns", xlab="Overnight Returns", ylab="Daytime Returns")



# Dygraph the rangev of the Close price with two y-axis.

datav <- cbind(rangev, log(closep))
colnames(datav) <- c("Range", "Close")
colv <- colnames(datav)
captiont <- "Range vs Close"
dygraphs::dygraph(datav, main=captiont) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="red") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="blue")

# Simulate the contrarian strategy
threshv <- 0.2
posv <- rep(NA_integer_, NROW(ohlc)) ##  Stock positions
posv[1] <- 0
posv <- ifelse(rangev > (1-threshv), -1, posv)
posv <- ifelse(rangev < threshv, 1, posv)
posv <- zoo::na.locf(posv)
# Lag the positions to trade in the next period
posv <- rutils::lagit(posv, lagg=1)
pnls <- posv*retp
pnls <- cbind(retp, pnls)
colnames(pnls) <- c(symboln, "Contrarian")
sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
names(sharper) <- colnames(pnls)

captiont <- paste0("Contrarian Strategy ", paste0(paste0(names(sharper), " SR=", sharper), collapse=" / "))

dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)



############## homework
## Calculate the ratio of the intraday range of prices 
# divided by the intraday return of prices.
# The range is larger when the intraday returns are 
# smaller and when the overnight returns are smaller.

# Calculate the range of daily prices for SPY
ohlc <- log(rutils::etfenv$SPY)
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retd <- (closep - openp)
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
hilo <- highp - lowp
hilol <- rutils::lagit(hilo, lagg=1)

# The rescaled range is slightly larger than the theoretical value
sqrt(pi/2) ##  Theoretical range 1.253314
mean(hilo)/sd(retd)

# The rescaled range is larger when the intraday returns are small:
selv <- (retd < 0.01)
mean(hilo[selv])/sd(retd[selv])
# mean(hilo[selv]/abs(retd[selv]))
# Average profit?
mean((hilo/2 - abs(retd))[selv])

# The rescaled range is larger when the overnight returns are small
# selv <- ((hilol+abs(reton)) < 0.01)
selv <- (abs(reton) < 0.01)
mean(hilo[selv])/sd(retd[selv])
# mean(hilo[selv]/abs(retd[selv]))
# Average profit?
mean((hilo/2 - abs(retd))[selv])

# Rescaled Range of SPY versus Overnight Returns
# shows that the range is larger when the overnight returns are smaller.
plot(x=abs(coredata(reton)), y=abs(coredata(hilo/retd)),
     main="Rescaled Range of SPY
     versus Overnight Returns",
     xlab="ReturnON", ylab="Range",
     xlim=c(0, 0.03), ylim=c(0, 100))


# Calculate the range of daily prices for all the ETFs
rangev <- sapply(rutils::etfenv$symbolv, function(symboln) {
  ohlc <- log(get(symboln, rutils::etfenv))
  ohlc <- ohlc["2021/"]
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retd <- (closep - openp)
  highp <- quantmod::Hi(ohlc)
  lowp <- quantmod::Lo(ohlc)
  hilo <- highp - lowp
  mean(hilo)/sd(retd)
}) ##  end sapply
rangev <- sort(rangev)


##############
# Momentum Strategy for ETFs

# Extract ETF returns
symbolv <- c("VTI", "IEF", "DBC")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
# Calculate a vector of monthly end points
rebalf <- "months"
endd <- rutils::calc_endpoints(retp, interval=rebalf)
npts <- NROW(endd)
# Perform backtests for vector of look-back intervals
lookbv <- seq(3, 12, by=1)
volt <- 0.01 ##  Target volatility of momentum strategy
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
pnll <- mclapply(lookbv, sim_momw, retp=retp, endd=endd, objfun=objfun, volt=volt, mc.cores=ncores)
sharpev <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl[pnl<0]))
# Plot Sharpe ratios of momentum strategies
plot(x=lookbv, y=sharpev, t="b",
     main="Momentum Sharpe as Function of Look-back Interval",
     xlab=paste0("look-back (", rebalf, ")"), ylab="Sharpe")

# Calculate best pnls of momentum strategy
whichmax <- which.max(sharpev)
lookb <- lookbv[whichmax]
pnls <- pnll[[whichmax]]
# Calculate returns of all-weather benchmark
weightaw <- c(0.30, 0.55, 0.15)
retaw <- retp %*% weightaw
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retaw, pnls)
wealthv <- xts::xts(wealthv, order.by=datev)
colnames(wealthv) <- c("All-weather", "Momentum")
cor(wealthv)
wealthv <- xts::xts(wealthv, order.by=datev)
sharper <- sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sharper

# Plot dygraph of all-weather benchmark and momentum strategy
colnames(wealthv) <- paste(colnames(wealthv), round(sharper[1, ], 3), sep=" = ")
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Optimal Momentum for ETFs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)


##############
# Combine the lists of daily minute OHLC bars.

# Load the new list of daily minute OHLC bars for QQQ
loadd <- load("/Users/jerzy/Develop/data/minutes/QQQ_minutes_list.RData")
ohlcn <- ohlc
# Load the old list of daily minute OHLC bars for QQQ
loadd <- load("/Users/jerzy/Develop/data/minutes/QQQ_2025.RData")
ohlco <- pricel
# Select from the list of ohlco only those elements that are not in the list ohlcn
ohlco <- ohlco[!(names(ohlco) %in% names(ohlcn))]
# Combine the ohlco and ohlcn lists into a single list of ohlc
ohlcl <- c(ohlco, ohlcn)
save(ohlcl, file="/Users/jerzy/Develop/data/minutes/QQQ_minutes_list.RData")


# Convert the OHLC bars to a list of xts objects, one for each day
loadd <- load("/Users/jerzy/Develop/data/minutes/QQQ_minutes.RData")
datev <- zoo::index(ohlc)
datev <- format(datev, "%Y-%m-%d")
ohlcl <- lapply(unique(datev), function(d) {
  selv <- (datev == d)
  return(ohlc[selv, ])
}) # end lapply
names(ohlcl) <- unique(datev)
save(ohlcl, file="/Users/jerzy/Develop/data/minutes/QQQ_minutes_list.RData")
ohlcn <- ohlcl

datev <- sapply(ohlco, function(x) {
  return(unique(format(zoo::index(x), "%Y-%m-%d")))
}) # end lapply
names(ohlco) <- datev


##############
# Simulate a momentum strategy of Bollinger strategies.
# First simulate Bollinger strategies and combine them 
# with stocks.
# A Bollinger strategy combined with the stock has a 
# lower volatility and higher Sharpe ratio than the 
# stock alone.
# Then simulate a momentum strategy of the combined 
# Bollinger strategies.
# Needs more work to optimize the parameters of the 
# Bollinger and momentum strategy.

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
symbolv <- sort(colnames(retstock))

lambdaf <- 0.4
threshv <- 0.7

pnll <- lapply(retstock, function(retp) {
  # retp <- na.omit(get(symboln, retstock))
  retp <- na.omit(retp)
  retp <- retp[retp != 0]  # Remove zero returns
  volp <- HighFreq::run_var(retp, lambdaf=lambdaf)
  retm <- volp[, 1]
  volp <- sqrt(volp[, 2])
  # Calculate the z-scores
  zscores <- retm/volp
  zscores[1:11] <- 0
  # Simulate the patient Bollinger Strategy quickly
  posv <- rep(NA_integer_, NROW(retp)) ##  Stock positions
  posv[1] <- 0
  posv <- ifelse(zscores > threshv, -1, posv)
  posv <- ifelse(zscores < -threshv, 1, posv)
  posv <- zoo::na.locf(posv)
  # Lag the positions to trade in the next period
  posv <- rutils::lagit(posv, lagg=1)
  pnls <- posv*retp
  pnls <- cbind(retp, pnls, 0.5*(retp+pnls))
  colnames(pnls) <- c(colnames(retp), "Bollinger", "Combined")
  return(pnls)
}) # end lapply)
# pnll <- do.call(cbind, pnll)
# names(pnll) <- colnames(retstock)


sharpev <- sapply(pnll, function(pnls) {
  # pnls <- pnls[!is.na(pnls)]
  pnls <- pnls["2010/"][, "Combined"]
  sharper <- sqrt(252)*mean(pnls)/sd(pnls[pnls<0])
  # sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
  # names(sharper) <- colnames(pnls)
  # sharper <- round(sharper, 3)
  return(sharper)
}) # end sapply)
# names(sharpev) <- colnames(retstock)
sharpev <- sort(sharpev, decreasing=TRUE)
head(sharpev, 33)
tail(sharpev, 33)

symboln <- "BRK-B"
pnls <- pnll[[symboln]]["2000/"]
sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
captiont <- paste0(paste0(names(sharper), " SR=", sharper), collapse=" / ")
dygraphs::dygraph(cumsum(pnls), main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)


pnlm <- lapply(pnll, function(pnls) {
  pnls <- pnls["2000/"][, "Combined"]
  return(pnls)
}) # end lapply)
pnlm <- do.call(cbind, pnlm)
colnames(pnlm) <- colnames(retstock)

lambdaf <- 0.7
pnlema <- HighFreq::run_mean(pnlm, lambda=lambdaf)
pnlema <- HighFreq::run_var(pnlm, lambda=lambdaf)
nstocks <- NCOL(pnlm)
pnlema <- pnlema[, (1:nstocks)]/sqrt(pnlema[, (nstocks+1):(2*nstocks)])
# sum(is.na(pnlema))

probv <- 0.9
quantv <- matrixStats::rowQuantiles(coredata(pnlema), probs=probv, na.rm=TRUE)

weightv <- pnlema
weightv[is.na(weightv)] <- 0.0
weightv[weightv < quantv] <- 0.0
weightv <- rutils::lagit(weightv, lagg=1)

pnls <- weightv*pnlm
# pnls[is.na(pnls)] <- 0.0
pnls <- rowSums(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, order.by=index(pnlm))
dygraph(cumsum(pnls), main="Combined Strategy PnLs")


# No shorting, so set negative Kelly ratios to zero
kellyr[kellyr < 0] <- 0.0
# Scale the Kelly ratios so the portfolio has the target volatility
# kellyr <- kellyr*volt^2/rowSums(kellyr*varm, na.rm=TRUE)
kellyr[1:11, ] <- varfloor # Set the first few Kelly ratios to small number to avoid large position sizes at the beginning of the backtest
# Lag the Kelly ratios to avoid look-ahead bias
kellyr <- rutils::lagit(kellyr, lagg=1)
weightv <- kellyr

# Plot the kelly ratios
symboln <- "ARW"
colv <- which(colnames(kellyr) == symboln)
retv <- retsm[, colv]
retv <- xts::xts(retv, order.by=datev)
dygraph(retv, main=paste("Ret for", symboln))
varv <- varm[, colv]
varv <- xts::xts(varv, order.by=datev)
dygraph(varv, main=paste("Var for", symboln))
kellys <- kellyr[, colv]
kellys <- xts::xts(kellys, order.by=datev)
dygraph(kellys, main=paste("Kelly Ratios for", symboln))

retv <- get(symboln, reton)
retv <- xts::xts(retv, order.by=datev)
dygraph(cumsum(na.omit(retv)), main=paste("Reton for", symboln))


##############
# Volatility regime switching strategy.

load("/Users/jerzy/Develop/data/etf_ohlc.RData")
envv <- etfenv

load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
envv <- sp500env

lambdaf <- 0.1
lambdavol <- 0.3
volscale <- 0.01
volthresh <- 0.02
volt <- 0.01
rangev <- "2007/"
symboln <- "SPY"


sharpem <- eapply(envv, function(ohlc) {
  # ohlc <- get(symboln, envv)
  ohlc <- log(ohlc[rangev])
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retp <- rutils::diffit(closep) ##  daily returns
  colnames(retp) <- "daily"
  retd <- (closep - openp) ##  daytime returns
  colnames(retd) <- "daytime"
  reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE)) ##  overnight returns
  colnames(reton) <- "overnight"
  
  rets <- cbind(retp, retd, reton)
  retp <- rets$daily
  retm <- HighFreq::run_mean(retp, lambda=lambdaf)
  varv <- HighFreq::run_var_ohlc(ohlc, lambda=lambdavol)
  varv[1:5] <- 1.0
  varv <- rutils::lagit(varv, lagg=1, pad_zeros=FALSE)
  volv <- sqrt(varv)
  retp <- rets$daily
  posv <- -sign(retm)
  posv <- rutils::lagit(posv, lagg=1)
  pnls <- posv*retp
  probv <- (1 + tanh((volv-volthresh)/volscale))/2
  pnls <- probv*pnls + (1-probv)*retp*volt/volv
  pnls <- cbind(retp, pnls)
  colnames(pnls) <- c("Stock", "Strategy")
  sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
  names(sharper) <- colnames(pnls)
  sharper <- round(sharper, 3)
  return(sharper)
  
}) # end eapply)

sharpem <- do.call(rbind, sharpem)
sharpem <- sharpem[order(sharpem[, 2], decreasing=TRUE), ]

head(sharpem, 10)

plot(sharpem[, "Strategy"], t="h", ylab="Sharpe Ratio", xlab="Stock Rank", main="Sharpe Ratios of Volatility Regime Switching Strategy")


# Save the positions to a CSV file
sharpem <- data.frame(cbind(Symbol=rownames(sharpem), sharpem))
filen <- paste0("/Users/jerzy/Develop/trading/backtests/regimevol_etf.txt")
filen <- paste0("/Users/jerzy/Develop/trading/backtests/regimevol_stocks.txt")
data.table::fwrite(sharpem, filen)



##############
# Performing the ADF test in daytime returns of S&P500 stocks 
# shows that most of the stocks have significant negative autocorrelations 
# in their daytime returns, which is consistent with the presence of mean 
# reversion in daytime returns. The ADF test statistic is negative and less 
# than the critical value at the 5% significance level for most of the stocks, 
# indicating that we can reject the null hypothesis of a unit root and conclude 
# that the daytime returns are stationary. This suggests that there may be 
# opportunities for mean reversion strategies in the daytime returns of S&P500 stocks.


# Load the S&P500 daytime returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns_daytime.RData")

symbolv <- colnames(retd)
symboln <- "AKAM"
adfpval <- sapply(symbolv, function(symboln) {
  retdt <- na.omit(get(symboln, retd))
  retdt <- retdt[!(retdt == 0)]
  retc <- cumsum(retdt["2010/"])
  # adfres <- tseries::adf.test(retc, alternative="stationary")
  # return(adfres$p.value)
  return(abs(diff(range(retc)))/sd(retdt))
})  # end sapply
adfpval <- sort(adfpval, decreasing=FALSE)
head(adfpval, 33)
tail(adfpval, 33)
plot(adfpval, t="l")


symboln <- "AIEQ"
ohlc <- get(symboln, rutils::etfenv)
ohlc <- log(ohlc)
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retdt <- (closep - openp) ##  daytime returns
retc <- cumsum(retdt["2010/"])
adfres <- tseries::adf.test(retc, alternative="stationary")
dygraph(retc, main=paste(symboln, "Cumulative Daytime Returns"))

adfpval <- sapply(rutils::etfenv$symbolv, function(symboln) {
  cat("Calculating ADF p-value for", symboln, "\n")
  ohlc <- get(symboln, rutils::etfenv)
  ohlc <- log(ohlc)
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retdt <- (closep - openp) ##  daytime returns
  retc <- cumsum(retdt["2010/"])
  # adfres <- tseries::adf.test(retc, alternative="stationary")
  # return(adfres$p.value)
  return(abs(diff(range(retc)))/sd(retdt))
})  # end sapply
adfpval <- sort(adfpval, decreasing=FALSE)


##############
# Overnight stock momentum strategy.
# Code from the file:
# /Users/jerzy/Develop/Presentations/strat_momentum.R

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns_overnight.RData")
rangev <- "2007/"
retref <- reton[rangev]
datev <- index(retref)
nstocks <- NCOL(retref)
retm <- rowMeans(retref, na.rm=TRUE)
retm <- xts::xts(retm, order.by=datev)
symboln <- "Index"

varfloor <- 1e-8 ##  Variance floor to prevent division by zero in Kelly ratio calculations
volt <- 0.03 ##  Volatility target for scaling the strategy PnLs
retstock <- retstock[datev]
datev <- index(retstock)
nrows <- length(datev)
reton <- reton[datev]
nstocks <- NCOL(reton)
# Average overnight returns for all stocks
retm <- rowMeans(reton, na.rm=TRUE)
retm <- xts::xts(retm, order.by=datev)

stringv <- "Overnight"
captiont <- paste("Momentum With Overnight Stock Returns")

lambdaf <- 0.99
probv <- 0.99

# Calculate the EMA returns and volatilities
varm <- HighFreq::run_var(retref, lambda=lambdaf)
retsm <- varm[, 1:nstocks]
retsm[is.na(retsm)] <- 0.0
colnames(retsm) <- colnames(retref)
varm <- varm[, (nstocks+1):(2*nstocks)]
varm[1:3] <- 1.0
varm[is.na(varm)] <- 1.0
varm <- varm + varfloor; # Add small number to prevent division by zero
# Calculate the Kelly ratios
kellyr <- retsm*volt^2/sqrt(varm)

# Set small Kelly ratios less than the threshold to zero
# This improves the performance by increasing the leverage on the stocks with large Kelly ratios
quantv <- matrixStats::rowQuantiles(kellyr, probs=probv, na.rm=TRUE)
kellyr[kellyr < quantv] <- 0.0

# No shorting, so set negative Kelly ratios to zero
kellyr[kellyr < 0] <- 0.0
# Scale the Kelly ratios so the portfolio has the target volatility
# kellyr <- kellyr*volt^2/rowSums(kellyr*varm, na.rm=TRUE)
kellyr[1:11, ] <- varfloor # Set the first few Kelly ratios to small number to avoid large position sizes at the beginning of the backtest
# Lag the Kelly ratios to avoid look-ahead bias
kellyr <- rutils::lagit(kellyr, lagg=1)
weightv <- kellyr

# Plot the kelly ratios
symboln <- "ARW"
colv <- which(colnames(kellyr) == symboln)
retv <- retsm[, colv]
retv <- xts::xts(retv, order.by=datev)
dygraph(retv, main=paste("Ret for", symboln))
varv <- varm[, colv]
varv <- xts::xts(varv, order.by=datev)
dygraph(varv, main=paste("Var for", symboln))
kellys <- kellyr[, colv]
kellys <- xts::xts(kellys, order.by=datev)
dygraph(kellys, main=paste("Kelly Ratios for", symboln))

retv <- get(symboln, reton)
retv <- xts::xts(retv, order.by=datev)
dygraph(cumsum(na.omit(retv)), main=paste("Reton for", symboln))


# Calculate the recent kelly ratios
weightv <- kellyr[nrows, ]
weightv <- sort(weightv, decreasing = TRUE)
names(head(weightv, 21))
tail(weightv, 21)
weightv[which(names(weightv) == symboln)]
dygraph(cumsum(na.omit(get(symboln, reton))), main=paste("Reton for", symboln))

# Create a vector of the symbols with positive Kelly ratios
symbolv <- names(weightv[weightv > 0])
# Calculate the latest stock prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
pricev <- drop(coredata(pricestock[NROW(pricestock), symbolv]))
# Calculate the number of shares needed to buy $1000 for each stock
nshares <- round(1000/pricev, 1)
# Calculate the total dollar value of the positions
posv <- round(sum(nshares*pricev))
# Calculate the latest SPY price
load("/Users/jerzy/Develop/data/etf_prices.RData")
pricespy <- drop(coredata(pricetf[NROW(pricetf), "SPY"]))
# Calculate the number of shares of SPY needed to short to hedge the market risk of the positions
pospy <- round(posv/pricespy)
# Create a data frame of the positions
posdf <- data.frame(cbind(Ticker=c(symbolv, "SPY"), Shares=c(nshares, -pospy)))
# Save the positions to a CSV file
filen <- paste0("/Users/jerzy/Develop/MachineTrader.io/python-trading/strategies-single/args2.csv")
data.table::fwrite(posdf, filen)


# "ARW"  "CRS"  "SATS" "ATI"  "EMN"  "IRM"  "SANM" "NUE"  "AVT"  "CGNX" "LII"
# "CARR" "HST"  "TNDM" "TLN"  "CHWY" "SMTC" "AXON" "VST"  "AMG"



# "ARWR" "FCEL" "HUT"  "CRSP" "HOV"  "BZH"  "SIRI" "RIOT" "ENPH" "C"    "NVAX"
# "OPEN" "BX"   "LNG" "AIG"  "SMR"  "QUBT" "CDE"  "THC"  "CRK"  "SRPT" "MRNA"
# "TLRY" "NTLA" "MP"   "ODP"  "AVAV" "TSLA" "ALV"  "KKR"  "ET"

# "ARWR" "FCEL" "HOV"  "HUT"  "QUBT" "SIRI" "NVAX" "BZH"  "SMR"  "CRSP" "LNG"
# "BX"   "AXON" "ENPH" "MGM"  "MP"   "FUBO" "C"    "MRNA" "CDE"  "OPEN" "ET"
# "CYTK" "AIG"  "HL"   "THC"  "GILD" "RIOT" "NLY"  "AMBA" "KTOS"

# "CRS"  "ARW"  "EMN"  "SANM" "FIVE" "ZD"   "ATI"  "TNDM" "SNX"  "AMG"  "MEDP"



# Calculate PnLs as the overnight returns times the Kelly ratios
pnls <- retref*kellyr

# Calculate the cumulative PnLs for each stock
pnlss <- colSums(pnls["2020/"], na.rm = TRUE)
pnlss <- sort(pnlss, decreasing = TRUE)
plot(density(pnlss))
head(pnlss, 31)
tail(pnlss, 31)
# Plot the cumulative PnLs for the top 100 stocks
plot(pnlss[1:100], t="b", ylab="Cumulative PnL", xlab="Stock Rank", 
     main="Cumulative Momentum PnLs for the Top 100 Stocks")

# Calculate the combined strategy PnLs
pnls <- rowSums(pnls, na.rm=TRUE)
pnls[is.na(pnls)] <- 0
# Combine the strategy PnLs with the average overnight returns for all stocks
pnls <- cbind(retm, pnls)
colnames(pnls) <- c(stringv, "Strategy")
# Calculate Sharpe ratios
sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
# Calculate the cumulative PnLs
pnls <- cumsum(pnls)
# Plot the dygraph
colnamev <- colnames(pnls)
captiont <- paste0(paste0(c(stringv, "Momentum"), " SR="), sharper, collapse=" / ")
dygraphs::dygraph(pnls[, 1:2], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)



##############
# Overnight minus daytime returns

load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

# Calculate the total trading volume for each symbol
volumev <- eapply(sp500env, function(ohlc) {
  # ohlc <- sp500env$GLYC
  sum(quantmod::Vo(ohlc))
})  # end eapply
volumev <- sort(unlist(volumev), decreasing=TRUE)

# Select the top 100 symbols with the largest total trading volumes
symbolv <- names(volumev[1:100])

# The top 100 most liquid stocks don't have significant overnight minus daytime returns.
# There's no relationship between overnight minus daytime returns and total trading volume.

envv <- sp500env
symbolv <- names(envv)
symboln <- "MKL"
# Calculate the overnight minus daytime returns
pnls <- sapply(symbolv, function(symboln) {
  ohlc <- get(symboln, envir=envv)
  ohlc <- log(ohlc)
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retd <- (closep - openp)
  colnames(retd) <- "daytime"
  reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
  colnames(reton) <- "overnight"
  retx <- (reton - retd)["2026"]
  return(sum(retx)/var(retx))
  # dygraph(cumsum(reton - retd)["2026"], main=paste(symboln, "Overnight - Daytime Returns"))
})  # end sapply
pnls <- sort(unlist(pnls), decreasing=TRUE)
names(head(pnls))

plot(x = volumev, y = pnls, xlim = c(0, median(volumev)), ylim = c(-median(pnls), median(pnls)),
     xlab = "Total Trading Volume", ylab = "Overnight - Daytime Returns",
     main = "Overnight - Daytime Returns vs Total Trading Volume")

# Calculate the overnight minus daytime returns
pnls <- eapply(envv, function(ohlc) {
  # ohlc <- get(symboln, envir=envv)
  ohlc <- log(ohlc)
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retd <- (closep - openp)
  colnames(retd) <- "daytime"
  reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
  colnames(reton) <- "overnight"
  retx <- (reton - retd)["2026"]
  return(sum(retx)/var(retx))
  # dygraph(cumsum(reton - retd)["2026"], main=paste(symboln, "Overnight - Daytime Returns"))
})  # end eapply
# names(pnls) <- names(etfenv)
pnls <- sort(unlist(pnls), decreasing=TRUE)
names(head(pnls, 100))



##############################
### Scratch
##############################

colnames(ohlc)[1:4] <- paste(symboln, c("Open","High","Low","Close"), sep=".")
dygraph(log(Cl(ohlc)), main=paste(symboln))

symboln <- "QQQ"
filen <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, "_minutes_markets.RData")
loadd <- load(filen)
# loadd <- load("/Users/jerzy/Develop/data/minutes/SPY_2025.RData")
# closep <- lapply(pricel, quantmod::Cl)
# closep <- do.call(rbind, closep)
closep <- quantmod::Cl(ohlc)
colnames(closep) <- symboln
retp <- rutils::diffit(closep, lagg=3)
retl <- rutils::lagit(retp, lagg=3)
dated <- format(zoo::index(closep), "%H:%M")


##############################
### Quant
##############################

##############
# Optimization wrapper function.

# Wrapper for multivariate optimization using BFGS or coordinate descent
optim <- function(f, x0, method = c("auto", "bfgs", "coordescent"),
                  maxiter = 200, tol = 1e-4,
                  h = 1e-5, c1 = 1e-4, rho = 0.5) {
  ##  Initialize the variables
  x <- x0                     ##  Initial guess for coordinates
  narg <- NROW(x)             ##  Number of coordinates
  
  ##  Match the method argument
  method <- match.arg(method)
  
  ##  Automatic method selection based on problem size
  if (method == "auto") {
    if (narg <= 20) {
      method <- "bfgs"        ##  Use BFGS for small/medium problems
    } else {
      method <- "coordescent" ##  Use coordinate descent for large problems
    }
  }
  
  ##  Call the appropriate solver
  if (method == "bfgs") {
    result <- optim_bfgs(f, x0,
                         maxiter = maxiter, tol = tol,
                         h = h, c1 = c1, rho = rho)
  } else {
    result <- optim_coordescent(f, x0,
                                maxiter = maxiter, tol = tol,
                                h = h, c1 = c1, rho = rho)
  }
  
  ##  Return the result
  return(result)
} ##  end optim

# Example usage:
optim(rosenbrock, c(-1.2, 1))


# Function to plot the optimization path on the Rosenbrock function
plot_rosenbrock_path <- function(result, main = "Optimization Path on Rosenbrock") {
  ##  Rosenbrock function
  rosefunny <- function(x, y) {
    100 * (y - x^2)^2 + (1 - x)^2
  }
  
  ##  Extract path
  H <- result$history
  xs <- H[, 1]
  ys <- H[, 2]
  
  ##  Create grid for contour
  xg <- seq(min(xs) - 0.5, max(xs) + 0.5, length.out = 200)
  yg <- seq(min(ys) - 0.5, max(ys) + 0.5, length.out = 200)
  z  <- outer(xg, yg, rosefunny)
  
  ##  Draw contour plot
  contour(xg, yg, z, nlevels = 40, col = "grey60",
          xlab = "x", ylab = "y", main = main)
  
  ##  Draw optimization path
  lines(xs, ys, col = "blue", lwd = 2)
  points(xs, ys, pch = 20, col = "blue")
  
  ##  Mark start and end
  points(xs[1], ys[1], pch = 19, col = "red", cex = 1.4)
  points(xs[length(xs)], ys[length(ys)], pch = 19, col = "darkgreen", cex = 1.4)
  
  legend("topright",
         legend = c("Path", "Start", "End"),
         col = c("blue", "red", "darkgreen"),
         pch = c(20, 19, 19),
         lwd = c(2, NA, NA))
}


optiml <- solve_coordescent(rosenbrock, x0 = c(-1.2, 1))
plot_rosenbrock_path(optiml, main = "Coordinate Descent Failure on Rosenbrock")


plot_rosenbrock_surface <- function(result,
                                    main = "3D Surface Plot of Rosenbrock with Optimization Path") {
  ##  Rosenbrock function
  rosefunny <- function(x, y) {
    100 * (y - x^2)^2 + (1 - x)^2
  }
  
  ##  Extract optimization path
  H <- result$history
  xs <- H[, 1]
  ys <- H[, 2]
  
  ##  Create grid for surface
  xg <- seq(min(xs) - 0.5, max(xs) + 0.5, length.out = 100)
  yg <- seq(min(ys) - 0.5, max(ys) + 0.5, length.out = 100)
  z  <- outer(xg, yg, rosefunny)
  
  ##  Draw 3D surface
  p <- persp(xg, yg, z,
             theta = 40, phi = 30,
             expand = 0.6,
             col = "lightblue",
             shade = 0.5,
             ticktype = "detailed",
             xlab = "x", ylab = "y", zlab = "f(x,y)",
             main = main)
  
  ##  Transform path points into 3D perspective coordinates
  path_z <- rosefunny(xs, ys)
  pts <- trans3d(xs, ys, path_z, p)
  
  ##  Draw optimization path
  lines(pts, col = "red", lwd = 2)
  points(pts, pch = 20, col = "red")
  
  ##  Mark start and end
  points(trans3d(xs[1], ys[1], path_z[1], p), pch = 19, col = "darkgreen", cex = 1.4)
  points(trans3d(xs[length(xs)], ys[length(ys)], path_z[length(path_z)], p),
         pch = 19, col = "blue", cex = 1.4)
  
  legend("topright",
         legend = c("Path", "Start", "End"),
         col = c("red", "darkgreen", "blue"),
         pch = c(20, 19, 19),
         lwd = c(2, NA, NA))
}

optiml <- solve_coordescent(rosenbrock, x0 = c(-1.2, 1))
plot_rosenbrock_surface(optiml,
                        main = "Coordinate Descent Failure on Rosenbrock (3D View)")

plot_rosenbrock_rgl <- function(result,
                                main = "Rosenbrock Surface with Optimization Path") {
  ##  Load rgl
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Package 'rgl' is required.")
  }
  
  rgl::open3d()
  rgl::clear3d()   ##  <-- updated here
  
  ##  Rosenbrock function
  rosefunny <- function(x, y) {
    100 * (y - x^2)^2 + (1 - x)^2
  }
  
  ##  Extract optimization path
  H <- result$history
  xs <- H[, 1]
  ys <- H[, 2]
  zs <- rosefunny(xs, ys)
  
  ##  Create grid for surface
  xg <- seq(min(xs) - 0.5, max(xs) + 0.5, length.out = 100)
  yg <- seq(min(ys) - 0.5, max(ys) + 0.5, length.out = 100)
  z  <- outer(xg, yg, rosefunny)
  
  ##  Draw 3D surface
  p <- rgl::persp3d(xg, yg, z,
                    col = "green",
                    alpha = 0.7,
                    smooth = TRUE,
                    specular = "black",
                    xlab = "x", ylab = "y", zlab = "f(x,y)",
                    main = main)
  
  ##  Draw optimization path
  rgl::lines3d(xs, ys, zs, col = "red", lwd = 3)
  rgl::points3d(xs, ys, zs, col = "red", size = 6)
  
  ##  Mark start and end
  rgl::points3d(xs[1], ys[1], zs[1], col = "darkgreen", size = 10)
  rgl::points3d(xs[length(xs)], ys[length(ys)], zs[length(zs)],
                col = "blue", size = 10)
  
  ##  Add labels
  rgl::text3d(xs[1], ys[1], zs[1], texts = "Start", col = "darkgreen", adj = c(1.2, 1.2))
  rgl::text3d(xs[length(xs)], ys[length(ys)], zs[length(zs)],
              texts = "End", col = "blue", adj = c(1.2, 1.2))
} ##  end plot_rosenbrock_rgl

library(rgl)
plot_rosenbrock_rgl(optiml)
rgl::rglwidget(elementId="rosenbrock", width=400, height=400)



##############
# Lasso regression via coordinate descent

# Soft-thresholding function
softt <- function(z, lambda) {
  if (lambda == 0)
    return(z)
  else
    return(sign(z) * max(abs(z) - lambda, 0))
} ##  end softt

reg_lasso <- function(respv, predm, lambda, maxit = 1000, tol = 1e-6) {
  ##  Initialize the variables
  nrows <- nrow(predm)
  ncols <- ncol(predm)
  colsq <- colSums(predm^2) / nrows
  betav <- rep(0, ncols)
  ##  Loop over iterations
  for (iter in 1:maxit) {
    betap <- betav
    ##  Loop over the predictors
    for (coln in 1:ncols) {
      ##  Calculate the partial residual excluding current predictor
      r_j <- respv - predm[, -coln] %*% betav[-coln]
      ##  Calculate the covariate
      c_j <- sum(predm[, coln] * r_j) / nrows
      ##  Apply soft-thresholding to the covariate
      c_j <- sign(c_j) * max(abs(c_j) - lambda, 0)
      ##  Update beta_j coefficient
      betav[coln] <- c_j / colsq[coln]
    } ##  end for coln
    ##  Break when converged
    if (sum(abs(betav - betap)) < tol) break
  } ##  end for iter
  return(betav)
} ##  end reg_lasso


# Solve multivariate regression using coordinate descent
reg_cd <- function(respv, predm, maxit = 1000, tol = 1e-6) {
  
  ##  Initialize variables
  ncols <- ncol(predm)
  colsq <- colSums(predm^2)
  betav <- rep(0, ncols)
  
  ##  Loop over iterations
  for (iter in 1:maxit) {
    betap <- betav
    ##  Loop over the predictors
    for (coln in 1:ncols) {
      ##  Calculate the partial residual excluding current predictor
      r_j <- respv - predm[, -coln] %*% betav[-coln]
      ##  Calculate the covariate
      c_j <- sum(predm[, coln] * r_j)
      ##  Update beta_j
      betav[coln] <- c_j / colsq[coln]
    } ##  end for coln
    
    ##  Break when converged
    if (sum(abs(betav - betap)) < tol) break
    
  } ##  end for iter
  
  return(betav)
  
} ##  end reg_cd



# Perform LASSO regression on this simulated data
# Define predictor matrix
nrows <- 100
ncols <- 5
predm <- matrix(runif(nrows*ncols), ncol=ncols)
colnames(predm) <- paste0("pred", 1:ncols)
# Response equals weighted predictor plus random noise
weightv <- runif(3:(ncols+2), min=(-1), max=1)
noisev <- rnorm(nrows, sd=2)
respv <- (1 + predm %*% weightv + noisev)

# Solve regression using lm()
# regmod <- lm(respv ~ predm)
regmod <- lm(respv ~ predm - 1)
regmod$coefficients

# Solve regression using coordinate descent
betav <- reg_cd(respv, predm)
betav


# Solve lasso regression using coordinate descent

lambda <- 0.0
betav <- reg_lasso(respv, predm, lambda)
betav
regmod <- glmnet::glmnet(respv, predm, alpha=1, lambda=lambda/nrows, standardize=FALSE, intercept=TRUE)
as.numeric(regmod$beta)

# Calculate lasso objective function:
lassobj <- function(respv, predm, betav, lambda) {
  nrows <- nrow(predm)
  residv <- respv - predm %*% betav
  objval <- (t(residv) %*% residv) / (2*nrows) + lambda * sum(abs(betav))
  return(as.numeric(objval))
} ##  end lassobj

# Minimize lasso objective function using optim():
lambda <- 0.1
objfun <- function(betav) {
  lasso_obj(respv, predm, betav, lambda)
}
betainit <- rep(0, ncols)
optimres <- optim(betainit, objfun, method="BFGS")
optimres$par



##############
### Wilcoxon tests

# Data
sample1 <- rnorm(200)
sample2 <- rnorm(100, mean=0.1)
# Mann-Whitney-Wilcoxon rank sum test
wilcoxt <- wilcox.test(sample1, sample2, paired=FALSE)
wilcoxt$statistic
# Calculate the U statistic of Mann-Whitney-Wilcoxon test
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
# Calculate the V statistic of Wilcoxon test
datav <- (sample1 - sample2)
sum(rank(abs(datav))[datav>0])



##############
### Tests for HighFreq functions

library(rutils)
# detach("package:HighFreq")
library(dygraphs)

# Compile Rcpp functions
Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/calc_weights.cpp")


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

## Calculate the ranksharpe weights using R
weightsr <- sapply(retp, function(x) mean(x)/sd(x))
weightsr <- rank(weightsr)
weightsr <- (weightsr - mean(weightsr))

# weightsr <- 0.01*weightv/stddev(retp*weightv)

## Calculate the weights using RcppArmadillo
weightv <- drop(calc_weights(retp, model_type="ranksharpe", scalit=FALSE))
all.equal(weightv, weightsr, check.attributes=FALSE)


## Calculate the max_sharpe weights using R

# Calculate the covariance matrix of ETF returns
# retp <- na.omit(rutils::etfenv$returns[, 1:16])
eigend <- eigen(cov(retp))
# Calculate the regularized inverse of covariance matrix
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

## Calculate the weights using RcppArmadillo
weightv <- drop(calc_weights(retp, model_type="max_sharpe", alpha=alpha, dimax=3, scalit=FALSE))
all.equal(weightv, drop(weightsr), check.attributes=FALSE)



## Calculate the returns on equal weight portfolio
indeks <- rowMeans(retp)
stdev <- sd(indeks[indeks<0])
indeks <- xts(indeks, index(retp))

foo <- weight_returns(retp, weightv)
bar <- retp %*% weightv
all.equal(foo, bar)


# Define maximum Sharpe portfolio weights
calc_weightv <- function(retp) {
  ##  eigend <- eigen(cov(retp))
  ##  # set tolerance for determining zero eigenvalues
  ##  precision <- sqrt(.Machine$double.eps)
  ##  # check for zero eigenvalues
  ##  notzero <- (eigend$values > (precision * eigend$values[1]))
  ##  invmat <- eigend$vectors[, notzero] %*% (t(eigend$vectors[, notzero])/eigend$values[notzero])
  ##  weightv <- invmat %*% apply(retp, 2, mean)
  ##  weightv/sum(abs(weightv))
  
  weightv <- sapply(retp[((nrows-500):nrows)], median)
  ##  weightv <- (order(order(weightv)-1)-1)
  ##  weightv <- order(order(weightv))
  ##  weightv <- order(weightv)
  weightv <- (order(weightv)-1)
  ##  weightv <- order(order(weightv, decreasing=TRUE), decreasing=TRUE)
  ##  weightv <- ordern[ordern]
  
} ##  end calc_weights
weightv <- calc_weights(retspub)


foo <- drop(calc_weights(retp[((nrows-500):nrows)], typev="rankrob", alpha=0, scalit=FALSE))


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

colv <- colnames(pnls)
captiont <- paste("Momentum Strategy for S&P500 Stocks")
dygraphs::dygraph(pnls, main=captiont) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="red") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="blue")



##############
### Benchmark eigen decomposition function in RcppArmadillo

Rcpp::sourceCpp(file="/Users/jerzy/Develop/R/Rcpp/calc_eigen.cpp")

eigend <- calc_eigen(scale(prices_ts, scalit=FALSE))
regmod <- prcomp(prices_ts)
all.equal(regmod$sdev^2, drop(eigend$values))
all.equal(unname(regmod$rotation), -eigend$vectors)

library(microbenchmark)
summary(microbenchmark(
  rcpp=calc_eigen(prices_ts),
  pure_r=prcomp(prices_ts),
  times=10))[, c(1, 4, 5)]  ##  end microbenchmark summary



##############
# plot multiple dygraphs in the same RStudio window

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
)  ##  end list

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dyplot))




##############
## plot multiple dygraphs in the same RStudio window

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
})  ##  end eapply

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dyplot))

## perform same plotting as above using pipes syntax
# Create a list of dygraphs objects in a loop
eapply(data_env, function(xtsv) {
  dygraphs::dygraph(xtsv[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(xtsv)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
}) %>% ##  end eapply
  ##  render the dygraphs objects using htmltools
  htmltools::tagList() %>% htmltools::browsable()




##############
## dygraph plot with highlighting of specific points

library(xts)
library(dygraphs)
# Convert numeric time index of ldeaths into class 'Date' (approximately)
datev <- as.Date(365*(zoo::index(ldeaths)-1970))
# Convert time index from class 'Date' to 'POSIXct'
datev <- as.POSIXct.Date(datev)

# Convert ldeaths into xts time series
ldx <- xts::xts(as.numeric(ldeaths), order.by=datev)
# Calculate the number of years
nyears <- NROW(datev)/12
# Calculate the January dates
jandates <- datev[1 + 12*(0:(nyears - 1))]
# or
# jandates <- datev[which(months(datev)=="January")]
# or
# jandates <- datev[grep("Jan", months(datev), ignore.case=TRUE)]
# Calculate the July dates
juldates <- datev[7 + 12*(0:(nyears - 1))]
# or
# juldates <- datev[which(months(datev)=="July")]

# Create dygraph object
dyplot <- dygraphs::dygraph(ldx, main="Dygraph of ldeaths with Annotations") %>%
  dyHighlight(highlightCircleSize=5)

# Add annotations for the January and July dates to dygraph object
for (daten in jandates) {
  dyplot <- dygraphs::dyAnnotation(dyplot, x=daten, text="Jan")
}  ##  end for
for (daten in juldates) {
  dyplot <- dygraphs::dyAnnotation(dyplot, x=daten, text="Jul")
}  ##  end for

# plot dygraph object
dyplot



##############
## Code to check for duplicate dates

# Create xts with duplicate dates
x <- xts(rnorm(5), order.by=c(Sys.Date() + 1:4, Sys.Date() + 2))
diff(index(x))

rutils::diffit(index(x))

as.POSIXct(make.index.unique(.index(x), drop=TRUE), origin="1970-01-01")


##############
## Example of an apply() error.
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
    ##  below doesn't work
    ##  if (as.logical(insv["new_instr"]))
    1
  else
    0
})  ##  end apply


# sapply() loop doesn't introduce leading space to TRUE
sapply(1:NROW(dframe), function(itv) {
  insv <- unlist(dframe[itv, ])
  cat("processing instrument:", insv["namev"], "\n")
  cat(insv["new_instr"], "\n")
  ##  if (as.logical(gsub(" ", "", insv["new_instr"])))
  if (as.logical(insv["new_instr"]))
    1
  else
    0
})  ##  end sapply


##############
# RcppParallel is slower for vectors of size < 100,000 and faster for size > 100,000

vec1 <- as.numeric(c(1:1000000))
library(microbenchmark)
summary(microbenchmark(
  cumsum=cumsum(vec1),
  rcpp=vectorSum(vec1),
  parallel=parallelVectorSum(vec1),
  times=10))[, c(1, 4, 5)]




##############
# Plots for Udemy

# Extract SPY prices
closep <- na.omit(rutils::etfenv$prices$SPY)
nrows <- NROW(closep)
# Calculate EMA prices using HighFreq::run_mean()
pricema <- HighFreq::run_mean(closep, lambda=0.9)
# Combine prices with EMA prices
pricev <- cbind(closep, pricema)
colnames(pricev)[2] <- "SPY EMA"
# Plot dygraph
dygraphs::dygraph(pricev["2020"], main="SPY Prices and EMA Prices") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the fast and slow EMAs
lambdafa <- 0.89
lambdasl <- 0.95
# Calculate the EMA prices
emaf <- HighFreq::run_mean(closep, lambda=lambdafa)
emas <- HighFreq::run_mean(closep, lambda=lambdasl)
pricev <- cbind(closep, emaf, emas)
colnames(pricev) <- c("SPY", "EMA fast", "EMA slow")
# Plot dygraph
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2020"], main="SPY Dual EMA Prices") %>%
  dySeries(name=colv[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], strokeWidth=2, col="red") %>%
  dySeries(name=colv[3], strokeWidth=2, col="purple") %>%
  dyLegend(show="always", width=300)


# Calculate SPY prices
emad <- (emaf - emas)
pricev <- cbind(closep, emad)
symboln <- "SPY"
colnames(pricev) <- c(symboln, paste(symboln, "Returns"))
# Plot dygraph of SPY Returns
colv <- colnames(pricev)
dygraphs::dygraph(pricev["2008/2009"], main=paste(symboln, "EMA Returns")) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)



##############################
### Autoregressive strategies
##############################


##############
## Kitchen sink strategy with multiple predictors

svxy <- log(rutils::etfenv$SVXY)
datev <- zoo::index(svxy)
nrows <- NROW(svxy)
vxx <- log(rutils::etfenv$VXX[datev])
vti <- rutils::etfenv$VTI[datev]

closep <- quantmod::Cl(log(vti))
retp <- rutils::diffit(closep)
volumv <- quantmod::Vo(vti)
# Calculate trailing average volume
volumr <- HighFreq::run_mean(volumv, lambda=0.9)
# Scale the returns using volume clock to trading time
retsc <- ifelse(volumv > 0, volumr*retp/volumv, 0)
# Define the response and predictor matrices
respv <- retsc
orderp <- 20
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)

foo <- lapply(1:orderp, rutils::lagit, input=rutils::diffit(vxx))
foo <- rutils::do_call(cbind, foo)
predm <- cbind(predm, foo)
foo <- lapply(1:orderp, rutils::lagit, input=rutils::diffit(svxy))
foo <- rutils::do_call(cbind, foo)
predm <- cbind(predm, foo)


# predm <- cbind(predm, predm^2)
# predm <- cbind(rep(1, nrows), predm)
# colnames(predm) <- c("phi0", paste0("retlag", 1:orderp), paste0("ret2lag", 1:orderp))
colnames(predm) <- c(paste0("retlag", 1:orderp), paste0("ret2lag", 1:orderp))
# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% respv
sum(coeff[1:orderp])
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff

# Calculate the kitchen sink strategy in-sample
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
                  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# Define in-sample and out-of-sample intervals
nrows <- NROW(retp)
cutoff <- (nrows %/% 2) + 200
datev[cutoff]
insample <- 1:cutoff
outsample <- (cutoff + 1):nrows
# Calculate the optimal AR coefficients
predinv <- MASS::ginv(predm[insample, ])
coeff <- drop(predinv %*% respv[insample])
# Calculate reduced inverse of the predictor matrix from SVD
svdec <- svd(predm[insample, ])
dimax <- 11
predinv <- svdec$v[, 1:dimax] %*%
  (t(svdec$u[, 1:dimax]) / svdec$d[1:dimax])
coeff <- drop(predinv %*% respv[insample])
# Calculate the strategy PnLs
fcasts <- predm %*% coeff
pnls <- fcasts*retp
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
colv <- colnames(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



##############
# Daytime returns of XLK and AAPL.
# The daytime returns of XLK and AAPL have significant negative autocorrelations.
# But the cointegrated daytime returns don't have significant negative autocorrelations.

ohlc <- rutils::etfenv$XLK
openp <- log(quantmod::Op(ohlc))
closep <- log(quantmod::Cl(ohlc))
retxlk <- (closep - openp)
# retxlk <- rutils::diffit(closep)
datev <- zoo::index(retxlk)
dygraph(cumsum(retxlk))

load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ohlc <- sp500env$AAPL
openp <- log(quantmod::Op(ohlc))
closep <- log(quantmod::Cl(ohlc))
retaapl <- (closep - openp)
# retaapl <- rutils::diffit(closep)
retaapl <- retaapl[datev]
dygraph(cumsum(retaapl))

betav <- drop(cov(retaapl, retxlk)/var(retxlk))
retc <- (retaapl - betav*retxlk)
dygraph(cumsum(retc))
pacf(retaapl)
pacf(retxlk)
pacf(retc)

dygraph(-cumsum(retaapl*sign(rutils::lagit(retaapl))))
dygraph(-cumsum(retxlk*sign(rutils::lagit(retxlk))))
dygraph(-cumsum(retc*sign(rutils::lagit(retc))))



##############
## Autoregressive strategy for cointegrated AAPL and XLK

# Align two time series

# Load the list of daily XLK prices
load("/Users/jerzy/Develop/data/XLK_second_202311.RData")
xlkl <- pricel
namev <- names(xlkl)
# Load the list of daily AAPL prices
load("/Users/jerzy/Develop/data/AAPL_second_202311.RData")
aapll <- pricel
# Select the same days
aapll <- aapll[namev]
namev <- names(aapll)
aapll <- aapll[namev]
namev <- names(aapll)
# Remove NA values
namev <- namev[!is.na(namev)]
# Select the same days
aapll <- aapll[namev]
xlkl <- xlkl[namev]
all.equal(names(aapll), names(xlkl))


# Select single day of XLK
xlk <- xlkl[[2]]
# Get numeric time index
xlkt <- xts::.index(xlk)

# Select single day of AAPL
aapl <- aapll[[2]]
aaplt <- xts::.index(aapl)

# Select the AAPL prices aligned with XLK
nearl <- calc_nearest(matrix(xlkt), matrix(aaplt))
aapln <- aapl[nearl]

xlkl <- lapply(xlkl, function(ts) {
  ts[, 1]
}) ##  end lapply
names(xlkl) <- namev

aapll <- lapply(seq_along(aapll), function(it) {
  ##  cat("it: ", it, "\n")
  ##  Get numeric time index
  xlkt <- xts::.index(xlkl[[it]])
  aaplt <- xts::.index(aapll[[it]])
  ##  Select the AAPL prices aligned with XLK
  nearl <- calc_nearest(matrix(xlkt), matrix(aaplt))
  aapln <- aapll[[it]][nearl, 1]
  aapln
}) ##  end lapply
names(aapll) <- namev

# Scrub the bad XLK prices
xlkl <- lapply(xlkl, function(ts) {
  ##  cat("it: ", it, "\t")
  ts[, 1]
}) ##  end lapply


save(aapll, xlkl, file="/Users/jerzy/Develop/data/AAPL_XLK.RData")
load(file="/Users/jerzy/Develop/data/AAPL_XLK.RData")

sapply(aapll, NROW)
sapply(xlkl, NROW)

bkup <- aapll
dygraph(aapll[[1]])
aapll <- bkup

# Scrub the bad AAPL prices
aapll <- lapply(aapll, function(ts) {
  cat("it: ", it, "\t")
  ##  ts <- ts[, 1]
  ##  datev <- index(ts)
  scrub_online(ts, tol=0.1)
  ts
  ##  cat("nscrub: ", nscrub, "\n")
  ##  xts(ts, datev)
}) ##  end lapply

# Scrub the bad XLK prices
xlkl <- lapply(xlkl, function(ts) {
  cat("it: ", it, "\t")
  ##  ts <- ts[, 1]
  ##  datev <- index(ts)
  scrub_online(ts, tol=0.1)
  ts
  ##  cat("nscrub: ", nscrub, "\n")
  ##  xts(ts, datev)
}) ##  end lapply


# Calculate time series of prices
xlkts <- do.call(rbind, xlkl)
aaplts <- do.call(rbind, aapll)
dygraph(aaplts)



# Calculate the beta of AAPL with respect to XLK
betav <- cov(xlkts[, 1], aaplts[, 1])/var(xlkts[, 1])
betav <- drop(betav)
pricec <- (aaplts[, 1] - betav*xlkts[, 1])

retp <- rutils::diffit(spy)
betav <- cov(retp, rutils::diffit(aapl))/var(retp)
betav <- drop(betav)
pricel <- lapply(seq_along(spyl), function(it) {
  (aapll[[it]] - betav*spyl[[it]])
}) ##  end lapply


dygraph(pricec)
retc <- rutils::diffit(pricec)
retc <- as.numeric(retc)
sum(abs(retc) > 0.1)
retc[abs(retc) > 0.1] <- 0
pacf(retc)



## Autoregressive strategy for cointegrated AAPL and XLK

nrows <- NROW(retc)
# Define the response and predictor matrices
orderp <- 2
predm <- lapply(1:orderp, rutils::lagit, input=retc)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0("lag", 1:orderp)

# Calculate the fitted autoregressive coefficients
predinv <- MASS::ginv(predm)
coeff <- predinv %*% retc
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
resids <- (fcasts - retc)
vars <- sum(resids^2)/(nrows-NROW(coeff))
pred2 <- crossprod(predm)
covmat <- vars*MASS::ginv(pred2)
coeffsd <- sqrt(diag(covmat))
coefft <- drop(coeff/coeffsd)
coeffn <- paste0("phi", 0:(NROW(coefft)-1))
barplot(coefft ~ coeffn, xlab="", ylab="t-value", col="grey",
        main="Coefficient t-values of AR Forecasting Model")
fcastv <- sqrt(HighFreq::run_var(fcasts, lambda=0.2)[, 2])
fcastsc <- ifelse(fcastv > 0, fcasts/fcastv, 0)
# Trade the scaled forecasts
pnls <- retc*fcastsc
# Trade only on very large forecasts - not really better
threshz <- 5
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(fcastsc > threshz, 1, posv)
posv <- ifelse(fcastsc < (-threshz), -1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
pnls <- retc*posv


pnls <- pnls*sd(retc[retc<0])/sd(pnls[pnls<0])
wealthv <- cbind(retc, pnls)
colnames(wealthv) <- c("Stocks", "Strategy")
wealthv <- xts(wealthv, index(pricec))
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
dygraphs::dygraph(cumsum(wealthv),
                  main="Autoregressive Strategy In-Sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



# Calculate transaction costs
flipi <- rutils::diffit(fcastsc)
costv <- 0.5*bidask*abs(flipi)
wealthv <- cbind(retc, pnls - costv)


##############
# Simulate a ratchet strategy for the price reverting to the open price.

# Calculate SPY dollar returns
ohlc <- HighFreq::SPY
nrows <- NROW(ohlc)
pricev <- quantmod::Cl(ohlc)

loadd <- load("/Users/jerzy/Develop/data/minutes/QQQ_minutes_markets.RData")
pricev <- quantmod::Cl(ohlc)
colnames(pricev) <- rutils::get_name(colnames(pricev))
endd <- rutils::calc_endpoints(pricev, interval=5)
pricev <- pricev[endd]
retp <- rutils::diffit(pricev, lagg=1)
colnames(retp) <- "SPY"
retl <- rutils::lagit(retp, lagg=1)

# Simulate the ratchet strategy for single day
pricev <- pricev["2025-06-02"]
simout <- ratchetx(pricev)
simout <- xts::xts(simout, order.by=index(pricev))
colnames(simout) <- c("PnLs", "posv", "zscores")
pnls <- simout$PnLs
dygraph(cumsum(pnls), main="Ratchet Strategy PnLs") %>%
  dyOptions(colors=c("blue"), strokeWidth=1)

# Coerce the pricev into a list of daily prices and apply the ratchet strategy to each day
pricev <- quantmod::Cl(ohlc)
pricel <- split(pricev, f="days")
head(pricel$`2025-11-20`)
tail(pricel$`2025-11-20`)
dygraph(pricel$`2025-11-20`)


pnll <- lapply(pricel, function(pricev) {
  simout <- ratchetx(pricev)
  pnls <- simout[, 1]
  pnls <- xts::xts(pnls, order.by=index(pricev))
  return(pnls)
}) ##  end lapply
pnls <- rutils::do_call(rbind, pnll)
pnlsub <- pnls["T9:30/T10:30"]



##############
# Bollinger ratchet strategy

# Calculate the positions
pricet <- sign(zscores)*trunc(abs(zscores))
priced <- rutils::diffit(pricet)
posv <- integer(nrows)
# posv <- rep(NA_integer_, nrows)
# posv[1] <- 0
# posv <- zoo::na.locf(posv, na.rm=FALSE)
buymode <- TRUE ##  Buy mode
sellmode <- TRUE ##  Sell short mode

for (i in 2:nrows) {
  ##  cat("i =", i, "\n")
  ##  cat("pricet[i] =", pricet[i], "\n")
  ##  cat("posv[i-1] =", posv[i-1], "\n")
  ##  Compare the current position with the z-score
  if (pricet[i] > -posv[i-1]) { ##  Need to short more
    if (sellmode) {
      ##  posv[i] <- -pricet[i] ##  Sell short
      posv[i] <- posv[i-1] - 1 ##  Sell short 1 share
      buymode <- FALSE
    } else {
      sellmode <- TRUE
      posv[i] <- posv[i-1] ##  Do nothing
    }  ##  end if
  } else if (pricet[i] < -posv[i-1]) { ##  Need to buy more
    if (pricet[i] < -posv[i-1]) {
      if (buymode) {
        ##  posv[i] <- -pricet[i] ##  Buy
        posv[i] <- posv[i-1] + 1 ##  Buy 1 share
        sellmode <- FALSE
      } else {
        buymode <- TRUE
        posv[i] <- posv[i-1] ##  Do nothing
      }  ##  end if
    }  ##  end if
  } else {
    posv[i] <- posv[i-1] ##  Do nothing
  }  ##  end if
}  ##  end for


pnls <- retp*rutils::lagit(posv, lagg=1)
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

dygraphs::dygraph(cumsum(wealthv),
                  main="Autoregressive Strategy With Returns Scaled By Volume") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)




##############
# Perform PCA of the predictor matrix and use it for forecasting.
# Result: it doesn't improve performance because the lagged returns
# are uncorrelated and they have equal volatilities.

# Calculate the VTI returns and trading volumes
ohlc <- rutils::etfenv$VTI
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(log(closep))
volumv <- quantmod::Vo(ohlc)
# Calculate the trailing average volume
volumr <- HighFreq::run_mean(volumv, lambda=0.7)
# Scale the returns using volume clock to trading time
retc <- ifelse(volumv > 0, volumr*retp/volumv, 0)

## Calculate the predictor matrix
orderm <- 3
predm <- lapply(1:orderm, rutils::lagit, input=retc)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0("phi", 1:orderm)
# Rotate the predictor matrix to an orthogonal basis
rotmat <- cbind(c(1, 1, 1)/3, c(1, 0, -1), c(1, 2, -1))
predm <- predm %*% rotmat
predm <- cbind(rep(1, NROW(predm)), predm)
colnames(predm) <- c("phi0", paste0("phi", 1:(NCOL(predm)-1)))
# Calculate the in-sample AR coefficients
predinv <- MASS::ginv(predm[insample, ])
coeff <- predinv %*% retc[insample, ]
# Calculate the out-of-sample forecasts of VTI
fcasts <- predm[outsample, ] %*% coeff
pnls <- retp[outsample, ]*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))


## Calculate the predictor matrix
orderm <- 10
predm <- lapply(1:orderm, rutils::lagit, input=retc)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0("phi", 1:orderm)
cormat <- cor(predm)
round(cormat, 4)
# Calculate the PCA
pcad <- prcomp(predm, center=FALSE, scale=FALSE)
round(pcad$rotation, 4)
predpca <- pcad$x[, 1:2]
predpca <- cbind(rep(1, NROW(predpca)), predpca)
colnames(predpca) <- c("phi0", paste0("phi", 1:(NCOL(predpca)-1)))
# Calculate the in-sample AR coefficients
# predinv <- MASS::ginv(predpca)
predinv <- MASS::ginv(predpca[insample, ])
# coeff <- predinv %*% retc
coeff <- predinv %*% retc[insample, ]
# Calculate the out-of-sample forecasts of VTI
# fcasts <- predpca %*% coeff
fcasts <- predpca[outsample, ] %*% coeff
# pnls <- retp*fcasts
pnls <- retp[outsample, ]*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
# wealthv <- cbind(retp, pnls)
wealthv <- cbind(retp[outsample, ], pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of autoregressive strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd],
                  main="Autoregressive Strategy With Returns Scaled By Volume") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



##########
## Calculate the rolling z-scores of the returns, using the function roll_zscores().
# Use the zscores to trade the returns, and calculate the strategy Sharpe ratio.
# What is predv?

zscores <- HighFreq::roll_zscores(respv=closep, predv=predv, lookb=lookb)

lookbs <- 5*(1:10)
# zscores <- sapply(lookbs, HighFreq::roll_zscores, respv=closep, predv=predv, startp = 0L, endd = 0L, step = 1L, stub = 0L)
zscores <- sapply(lookbs, function(lookb) {
  cat("lookb =", lookb, "\n")
  zscores <- HighFreq::roll_zscores(respv=closep, predv=predv, lookb=lookb)
  zscores[1:lookb] <- 0
  zscores
})
colnames(zscores) <- paste0("zscores", lookbs)
foo <- apply(zscores, 2, sd)
foo/sqrt(lookbs)

# Standard deviation of square returns is proxy for kurtosis and stationarity


# Calculate the trailing z-scores of SVXY
predv <- cbind(sqrt(varv), vxx, vti_close)
respv <- svxy
rollzscores <- drop(HighFreq::roll_zscores(respv=respv, predv=predv, lookb=lookb))
rollzscores[is.infinite(rollzscores)] <- 0

rollreg <- HighFreq::roll_reg(respv=respv, predv=predv, intercept=TRUE, lookb=lookb)
rollregscores <- rollreg[, NCOL(rollreg), drop=TRUE]
all.equal(rollregscores, rollzscores)

library(microbenchmark)
# HighFreq::roll_reg() is faster than roll_zscores()
summary(microbenchmark(
  roll_zscores=HighFreq::roll_zscores(respv=respv, predv=predv, lookb=lookb),
  roll_reg=HighFreq::roll_reg(respv=respv, predv=predv, intercept=TRUE, lookb=lookb),
  times=10))[, c(1, 4, 5)]

runreg <- HighFreq::run_reg(respv=respv, predv=predv, lambda=lambdaf, method="scale")
runzscores <- HighFreq::run_zscores(respv=respv, predv=predv, lambda=lambdaf, demean=FALSE)
# runzscores <- runreg[, 1, drop=TRUE]

runreg <- rutils::lagit(rutils::diffit(cbind(respv, predv)))

runreg[is.infinite(runreg)] <- 0
runreg[abs(runreg) > 1e8] <- 0
runreg[1:11, ] <- 0


# Portfolio objective function
objfun <- function(x) {
  fcasts <- sign(runreg %*% x)
  pnls <- (retp*fcasts)["/2017"]
  -mean(pnls)/sd(pnls[pnls < 0])
}  ##  end objfun

# 2-dim case
optimd <- optim(fn=objfun,
                par=rep(1, NCOL(runreg)),
                method="L-BFGS-B",
                upper=rep(100, NCOL(runreg)),
                lower=rep(-100, NCOL(runreg)))

# Portfolio weights - static dollars not shares
weightv <- optimd$par

# Calculate the out-of-sample pnls
fcasts <- runreg %*% weightv
pnls <- (retp*fcasts)
pnls <- xts::xts(cumsum(pnls), zoo::index(retp))
dygraphs::dygraph(pnls, main="VTI Stategy Using RunReg")
fcasts <- xts::xts(fcasts, zoo::index(retp))
dygraphs::dygraph(fcasts, main="VTI RunReg Forecasts")



# Needs updating
##############
## Benchmark of HighFreq::run_reg() against R code

# Regression of XLF returns versus VTI and IEF
library(HighFreq)
# Load ETF returns
retp <- na.omit(rutils::etfenv$returns[, c("XLF", "VTI", "IEF")])
# Response equals XLF returns
respv <- retp[, 1]
# Predictor matrix equals VTI and IEF returns
predm <- retp[, -1]
# Calculate the rolling regressions
lambdaf <- 0.9
controll <- param_reg()
regs <- HighFreq::run_reg(respv=respv, predm=predm, lambda=lambdaf, controll=controll)
# Plot the rolling alphas
datav <- cbind(cumsum(respv), regs[, 2])
colnames(datav) <- c("XLF", "alphas")
colv <- colnames(datav)
dygraphs::dygraph(datav, main="Alphas of XLF Versus VTI and IEF") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red")

# respv <- zoo::coredata(respv)
# predv <- zoo::coredata(predv)
nrows <- NROW(predv)
ncols <- NCOL(predv)
respm <- matrix(nrow=nrows)
predm <- matrix(nrow=nrows, ncol=ncols)
vars <- predv^2
covars <- matrix(rep(0, nrows*ncols), nrow=nrows, ncol=ncols)
betas <- matrix(rep(0, nrows*ncols), nrow=nrows, ncol=ncols)
alphas <- matrix(rep(0, nrows), nrow=nrows)
resids <- matrix(rep(0, nrows), nrow=nrows)
# varz <- matrix(rep(1, nrows), nrow=nrows)
# meanz <- matrix(rep(0, nrows), nrow=nrows)
lambda1 <- 1-lambdaf

# Perform loop over rows
respm[1, ] <- respv[1, ]
predm[1, ] <- predv[1, ]
for (it in 2:nrows) {
  ##  Calculate the mean as the weighted sum
  respm[it, ] <- lambda1*respv[it, ] + lambdaf*respm[it-1, ]
  predm[it, ] <- lambda1*predv[it, ] + lambdaf*predm[it-1, ]
  vars[it, ] <- lambda1*(predv[it, ] - predm[it, ])^2 + lambdaf*vars[it-1, ]
  covars[it, ] <- lambda1*((respv[it, ]-respm[it, ])*(predv[it, ]-predm[it, ])) + lambdaf*covars[it-1, ]
  betas[it, ] <- lambda1*covars[it, ]/vars[it, ] + lambdaf*betas[it-1, ]
  alphas[it, ] <- lambda1*(respm[it, drop=FALSE] - betas[it, ] %*% predm[it, ]) + lambdaf*alphas[it-1, ]
  ##  Calculate the z-score as the weighted sum of products of returns.
  resids[it, ] <- lambda1*(respv[it, drop=FALSE] - betas[it, ] %*% predv[it, ]) + lambdaf*resids[it-1, ]
  ##  Calculate the mean and variance of the z-scores.
  ##  meanz[it, ] <- lambda1*resids[it, ] + lambdaf*meanz[it-1, ]
  ##  varz[it, ] <- lambda1*(resids[it, ] - resids[it-1, ])^2 + lambdaf*varz[it-1, ]
}  ##  end for

# regdatar <- cbind(alphas, betas, vars, (resids - meanz)/sqrt(varz))
regdatar <- cbind(resids, alphas, betas)
regdata <- HighFreq::run_reg(respv=respv, predm=predv, lambda=lambdaf, controll=controll)
all.equal(regdatar, regdata, check.attributes=FALSE)




##############
## Tune parameters of AR strategies

dimaxs <- 2:7
pnls <- lapply(dimaxs, function(dimax) {
  cat("dimax =", dimax, "\n")
  invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
  coeff <- drop(invmat %*% respv[insample, ])
  fcasts <- (predv[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", dimaxs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=300)


pnls <- lapply(dimaxs, function(dimax) {
  cat("dimax=", dimax, "\n")
  pnls <- lapply(3:(NROW(years)-1), function(i) {
    insample <- (datev > years[i-1]) & (datev < years[i])
    outsample <- (datev >= years[i]) & (datev < years[i+1])
    invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
    coeff <- drop(invmat %*% respv[insample, ])
    fcasts <- (predv[outsample, ] %*% coeff)
    sign(fcasts)*retp[outsample, ]
  })  ##  end lapply
  do.call(rbind, pnls)
})  ##  end lapply


pnls <- lapply(dimaxs, function(dimax) {
  cat("dimax=", dimax, "\n")
  pnls <- lapply(12:(NROW(months)-1), function(i) {
    insample <- (datev > months[i-3]) & (datev < months[i])
    outsample <- (datev > months[i]) & (datev < months[i+1])
    invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
    coeff <- drop(invmat %*% respv[insample, ])
    fcasts <- (predv[outsample, ] %*% coeff)
    sign(fcasts)*retp[outsample, ]
  })  ##  end lapply
  do.call(rbind, pnls)
})  ##  end lapply

pnls <- lapply(dimaxs, function(dimax) {
  cat("dimax=", dimax, "\n")
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    insample <- (datev > weeks[i-3]) & (datev < weeks[i])
    outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
    invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
    coeff <- drop(invmat %*% respv[insample, ])
    fcasts <- (predv[outsample, ] %*% coeff)
    sign(fcasts)*retp[outsample, ]
  })  ##  end lapply
  do.call(rbind, pnls)
})  ##  end lapply

pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", dimaxs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Loop over aggregations - aggregations don't improve the forecasts!
numaggs <- 2:11
pnls <- lapply(numaggs, function(numagg) {
  cat("numagg=", numagg, "\n")
  predv <- roll::roll_mean(ratestdeviff, width=numagg, min_obs=1)
  predv <- cbind(rep(1, NROW(predv)), predv)
  predv <- rutils::lagit(predv)
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    ##  Define in-sample and out-of-sample intervals
    insample <- (datev > weeks[i-lookb]) & (datev < weeks[i])
    outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
    ##  Calculate the forecasts and pnls out-of-sample
    invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
    coeff <- drop(invmat %*% respv[insample, ])
    fcasts <- (predv[outsample, ] %*% coeff)
    sign(fcasts)*retp[outsample, ]
  })  ##  end lapply
  pnls <- do.call(rbind, pnls)
})  ##  end sapply

pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", numaggs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=300)


##############

insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1)*nrows
# Calculate the forecasts as function of dimax
fcasts <- lapply(2:5, function(dimax) {
  cat("dimax=", dimax, "\n")
  ##  Calculate the fitted coefficients
  ##  invmat <- MASS::ginv(predv[insample, 1:dimax])
  invmat <- HighFreq::calc_inv(predv[insample,], dimax=dimax)
  coeff <- drop(invmat %*% respv[insample])
  ##  Calculate the out-of-sample forecasts of vtis
  drop(predv[outsample, ] %*% coeff)
})  ##  end lapply
names(fcasts) <- paste0("n=", 2:5)


##############
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

dimax <- 5
predv <- sapply(1:dimax, rutils::lagit, input=as.numeric(retp))
colnames(predv) <- paste0("pred_", 1:NCOL(predv))
predv <- cbind(predv, rutils::lagit(ratestdeviff))
predv <- cbind(rep(1, NROW(predv)), predv)
colnames(predv)[1] <- "intercept"
respv <- returns

# Calculate the forecasts as function of dimax
fcasts <- lapply(2:5, function(dimax) {
  cat("dimax=", dimax, "\n")
  invmat <- HighFreq::calc_inv(predv, dimax=dimax)
  coeff <- drop(invmat %*% respv)
  drop(predv %*% coeff)
})  ##  end lapply
names(fcasts) <- paste0("n=", 2:5)

pnls <- lapply(fcasts, function(x) {
  sign(x)*retp
})  ##  end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- names(fcasts)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=300)


# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
lookb <- 9
dimax <- 2
# Or
lookb <- 8
dimax <- 4
pnls <- lapply(51:(NROW(weeks)-1), function(i) {
  ##  Define in-sample and out-of-sample intervals
  insample <- (datev > weeks[i-lookb]) & (datev < weeks[i])
  outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
  ##  Calculate the forecasts and pnls out-of-sample
  invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
  coeff <- drop(invmat %*% respv[insample, ])
  fcasts <- (predv[outsample, ] %*% coeff)
  sign(fcasts)*retp[outsample, ]
})  ##  end lapply
pnls <- do.call(rbind, pnls)

vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colv <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)


##############


foo <- sapply(2:11, function(x) {
  pnls <- lapply(12:(NROW(months)-1), function(i) {
    ##  Define in-sample and out-of-sample intervals
    insample <- (datev > months[i-x]) & (datev < months[i])
    outsample <- (datev > months[i]) & (datev < months[i+1])
    ##  Calculate the forecasts and pnls out-of-sample
    invmat <- MASS::ginv(predv[insample, ])
    ##  invmat <- HighFreq::calc_inv(predv[insample, ], dimax=3)
    coeff <- drop(invmat %*% respv[insample, ])
    fcasts <- (predv[outsample, ] %*% coeff)
    sign(fcasts)*retp[outsample, ]
  })  ##  end lapply
  pnls <- do.call(rbind, pnls)
  mean(pnls)/sd(pnls)
})
(2:11)[which.max(foo)]


foo <- sapply(2:11, function(x) {
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    ##  Define in-sample and out-of-sample intervals
    insample <- (datev > weeks[i-x]) & (datev < weeks[i])
    outsample <- (datev > weeks[i]) & (datev < weeks[i+1])
    ##  Calculate the forecasts and pnls out-of-sample
    invmat <- MASS::ginv(predv[insample, ])
    ##  invmat <- HighFreq::calc_inv(predv[insample, ], dimax=3)
    coeff <- drop(invmat %*% respv[insample, ])
    fcasts <- (predv[outsample, ] %*% coeff)
    sign(fcasts)*retp[outsample, ]
  })  ##  end lapply
  pnls <- do.call(rbind, pnls)
  mean(pnls)/sd(pnls)
})
(2:11)[which.max(foo)]




##############
## Autoregressive strategy using the principal components
# of average returns as predictors.
# Mostly in app_ar_pca_strat.R

# Load packages
library(rutils)

# Calculate the ETF returns and volumes
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
lookb <- 11
volume_rolling <- roll::roll_mean(volumes, width=lookb)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
volumes <- volumes/volume_rolling

# Divide  the returns by the volume - use trading time (volume clock)
# retc <- ifelse(volumes > 0, returns/volumes, 0)
retc <- returns/volumes

## Version using only recent returns

# First version
lookbs <- 2:25
predm <- lapply(lookbs, function(x) sqrt(x)*roll::roll_mean(retc, x))
predm <- do.call(cbind, predm)
predm[1, ] <- 0
predm <- zoo::na.locf(predm)
# sum(is.na(predm))
predm <- cbind(retc, predm)
max_back <- last(lookbs)
# respv <- rutils::lagit(predm[, max_back], lagg=(-max_back))
respv <- sqrt(max_back)*roll::roll_mean(retp, max_back)
respv[1:(max_back-1)] <- 0
respv <- rutils::lagit(respv, lagg=(-max_back))

# Second version
# Define predictor matrix for forecasting
dimax <- 10
predm <- lapply(1:dimax, rutils::lagit, input=retp)
predm <- do.call(cbind, predm)
colnames(predm) <- paste0("pred_", 1:NCOL(predm))
# respv <- returns
respv <- rutils::lagit(predm[, dimax], lagg=(-dimax))


###########
## Old version
# Calculate the predictor equal to the rolling means
lookbs <- c(5, 20, 80, 250)
# predm <- lapply(lookbs, roll::roll_mean, x=retp)
# Scale the rolling means so they have similar volatities
predm <- lapply(lookbs, function(x) sqrt(x)*roll::roll_mean(retp, x))
# predm <- lapply(lookbs, function(x) sqrt(x)*roll::roll_mean(retp, x)/sqrt(roll::roll_var(retp, x)))
predm <- do.call(cbind, predm)
predm[1, ] <- 0
predm <- zoo::na.locf(predm)
# sum(is.na(predm))
colnames(predm) <- paste0("back_", lookbs)
sapply(predm, sd)
# Standardize (de-mean and scale) the predictor
# predm <- lapply(predm, function(x) {(x - mean(x))/sd(x)})
# predm <- rutils::do_call(cbind, predm)

# Define response as a rolling sum and shift it forward out-of-sample
respv <- rutils::lagit(predm[, 1], lagg=(-lookbs[1]))

###########
## End Old version

# Calculate the covariance matrix of predictor
covmat <- cov(predm)
# Calculate the eigenvectors and eigenvalues
eigend <- eigen(covmat)

# Define predictors as the principal components of predictor
# eigenvec <- eigend$vectors
predv <- xts::xts(predm %*% eigend$vectors, order.by=datev)
colnames(predv) <- paste0("pc", 1:NCOL(predv))
# round(cov(ratestdeviff), 3)
predv <- rutils::lagit(predv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv)[1] <- "unit"

# Calculate the in-sample fitted coefficients
dimax <- 4
invmat <- MASS::ginv(predv[insample, 1:dimax])
coeff <- drop(invmat %*% respv[insample])
# Calculate the out-of-sample forecasts of returns
# fcasts <- drop(predv[outsample, 1:3] %*% coeff[1:3])
fcasts <- drop(predv[outsample, 1:dimax] %*% coeff)
mean((retp[outsample, ] - fcasts)^2)
drop(cor(retp[outsample, ], fcasts))

# Lag the positions to trade in next period
posv <- sign(rutils::lagit(fcasts))

# Calculate the strategy pnls
pnls <- cumsum(posv*retp[outsample])
pnls <- cbind(cumsum(retp[outsample]), pnls)
colnames(pnls) <- c(symbol, "Strategy")
dygraphs::dygraph(pnls, main="Autoregressive Strategy Performance") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)


##############
# Case for lookbs = 2

# Calculate the ETF returns and volumes
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
lookb <- 11
volume_rolling <- roll::roll_mean(volumes, width=lookb)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
volumes <- volumes/volume_rolling
# Divide  the returns by the volume - use trading time (volume clock)
retc <- returns/volumes

lookbs <- 2:5
predm <- lapply(lookbs, function(x) sqrt(x)*roll::roll_mean(retc, x))
predm <- do.call(cbind, predm)
# lookbs <- 2
# predm <- sqrt(2)*roll::roll_mean(retc, 2)
predm[1, ] <- 0
predm <- zoo::na.locf(predm)
predm <- cbind(retc, predm)
max_back <- last(lookbs)

respv <- sqrt(max_back)*roll::roll_mean(retp, max_back)
respv[1:(max_back-1)] <- 0
respv <- rutils::lagit(respv, lagg=(-max_back))
covmat <- cov(predm)
eigend <- eigen(covmat)
predv <- xts::xts(predm %*% eigend$vectors, order.by=datev)
predv <- rutils::lagit(predv)
# predv <- cbind(rep(1, nrows), predv)
invmat <- MASS::ginv(predv[insample, ])
coeff <- drop(invmat %*% respv[insample])
fcasts <- drop(predv[outsample, ] %*% coeff)
fcasts <- roll::roll_mean(sign(fcasts), width=max_back)
fcasts[1:(max_back-1)] <- 1
pnls <- cumsum(fcasts*retp[outsample])
pnls <- cbind(cumsum(retp[outsample]), pnls)
colnames(pnls) <- c(symbol, "Strategy")
dygraphs::dygraph(pnls, main="Autoregressive Strategy Performance") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)




##############
## Backtest rescaled range strategy

# Load packages
library(HighFreq)

# Calculate the rolling rescaled cumulative returns from OHLC data
roll_range <- function(ohlc, lookb=11) {
  retp <- rutils::diffit(ohlc[, 4])
  rangev <- HighFreq::roll_sum(retp, lookb=lookb)
  var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, lookb=lookb, scalit=FALSE))
  lookb <- sqrt(lookb)
  hurst_rolling <- ifelse((var_rolling==0) | (rangev==0),
                          0.0,
                          rangev/var_rolling/lookb)
  ##  Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  ##  end roll_range


# Calculate the rolling rescaled cumulative returns from returns data
roll_range <- function(retp, cum_returns, lookb=11) {
  rangev <- HighFreq::roll_sum(retp, lookb=lookb)
  var_rolling <- sqrt(HighFreq::roll_var(retp, lookb=lookb))
  lookb <- sqrt(lookb)
  hurst_rolling <- ifelse((var_rolling==0) | (rangev==0),
                          0.0,
                          rangev/var_rolling/lookb)
  ##  Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  ##  end roll_range


retp <- rutils::diffit(drop(zoo::coredata(quantmod::Cl(HighFreq::SPY))))
nrows <- NROW(retp)
dim(retp) <- c(nrows, 1)
# cum_retv <- cumsum(retp)



# Calculate the rolling Hurst for SPY
hurst_rolling <- HighFreq::roll_hurst(ohlc=HighFreq::SPY, lookb=7)
# Calculate the rolling rescaled range
hurst_rolling <- roll_range(ohlc=HighFreq::SPY, lookb=5)
hurst_rolling <- roll_range(retp=retp, lookb=5)
# chart_Series(hurst_rolling["2009-03-10/2009-03-12"], name="SPY hurst_rolling")

# threshz <- 0.5

x11(width=6, height=5)
quantiles <- quantile(hurst_rolling, c(0.01, 0.99))
hist(hurst_rolling, xlim=quantiles, breaks=1e2)


# Trade on rescaled range

quantiles <- quantile(hurst_rolling, c(0.2, 0.8))
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
# Flip only if two consecutive signals in same direction
posv <- ifelse((re_scaled > quantiles[2]) & (rescaled_lag > quantiles[2]), -1, posv)
posv <- ifelse((re_scaled < quantiles[1]) & (rescaled_lag < quantiles[1]), 1, posv)
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, 2)
pnls <- cumsum(retp*posv)
x11(width=6, height=5)
plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(posv))) / NROW(posv)



##############
## Relationship between liquidity (turnover) and linear dependence
## (autocorrelation).

# Do illiquid stocks with a lower dollar turnover have significant
# autocorrelation?

# The answer is that there's no significant relationship between liquidity
# (turnover) and linear dependence (correlation).

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
    ##  Calculate the autocorrelations from PACF
    p_acf <- pacf(na.omit(retp), plot=FALSE)
    c(turnover=sum(volumes*prices), dependence=sum(p_acf$acf))
    ##  Calculate the autocorrelations from Hurst
    ##  endd <- rutils::calc_endpoints(ohlc, lagg)
    ##  c(turnover=sum(volumes*prices), dependence=calc_hurst_hilo(Hi(ohlc), Lo(ohlc), endd))
  } else NULL
})  ##  end lapply

# Bind and sort
datav <- do.call(rbind, datav)
datav <- datav[order(datav[, "dependence"], decreasing=FALSE), ]

# Plot
x11()
hist(datav)
plot(datav)
chart_Series(quantmod::Cl(sp500env$TYL))
dygraphs::dygraph(quantmod::Cl(sp500env$TYL))



##############
## Test autoregressive strategy for all ETFs in rutils::etfenv

library(rutils)
ordern <- 3
lookb <- 5

back_test <- function(symbol) {
  prices <- log(quantmod::Cl(get(symbol, rutils::etfenv)))
  retp <- rutils::diffit(prices)
  retp <- as.numeric(retp)
  nrows <- NROW(retp)
  insample <- 1:(nrows %/% 2)
  out_of_sample <- (nrows %/% 2 + 1):nrows
  predv <- rutils::roll_sum(retp, lookb=lookb)
  respv <- rutils::lagit(predv, lagg=(-lookb))
  predv <- sapply(1+lookb*(0:ordern), rutils::lagit, input=predv)
  predv <- cbind(rep(1, nrows), predv)
  colnames(predv) <- paste0("pred_", 1:NCOL(predv))
  invmat <- MASS::ginv(predv[insample, 1:ordern])
  coeff <- drop(invmat %*% respv[insample])
  fcasts <- drop(predv[out_of_sample, 1:ordern] %*% coeff)
  sign(fcasts)*retp[out_of_sample]
}  ##  end back_test


test_s <- lapply(rutils::etfenv$symbolv, back_test)
names(test_s) <- rutils::etfenv$symbolv
sort(sapply(test_s, sum))

fcasts <- test_s[[1]]
pnls <- xts(cumsum(sign(fcasts)*retp[out_of_sample]), index(prices[out_of_sample]))



##############
# Determine if intervals with higher volatility have larger Hurst exponents
# Conclusion: they don't

# Split returns into intervals using end points
retp <- na.omit(rutils::etfenv$returns$VTI)
# endd <- rutils::calc_endpoints(retp, interval="months")
endd <- HighFreq::calc_endpoints(length=NROW(retp), step=20)
npts <- NROW(endd)

# Calculate the daily volatilities in the intervals between end points
volats <- sapply(2:(npts-1), function(np) {
  retp <- retp[(endd[np-1]+1):endd[np], ]
  c(retp=sum(retp), stdev=sd(retp))
})  ##  end sapply
volats <- t(volats)

# Calculate the Hurst exponent
(log(sd(volats[, "retp"])) - log(sd(retp)))/log(20)

# Select the intervals with higher volatility
indic <- (volats[, "stdev"] > 0.015)
# indic <- c(FALSE, indic)
sum(indic)

# Select the intervals with higher volatility
retsh <- lapply(endd[(2:(npts-1))[indic]], function(ep) {
  retp[(ep-19):ep, ]
})  ##  end lapply
retsh <- do.call(rbind, retsh)
sd(retsh)

# Select the intervals with lower volatility
retsl <- lapply(endd[(2:(npts-1))[!indic]], function(ep) {
  retp[(ep-19):ep, ]
})  ##  end lapply
retsl <- do.call(rbind, retsl)
sd(retsl)

# Calculate the Hurst for the higher volatility intervals
hursth <- (log(sd(volats[indic, "retp"])) - log(sd(retsh)))/log(20)
# Calculate the Hurst for the lower volatility intervals
hurstl <- (log(sd(volats[!indic, "retp"])) - log(sd(retsl)))/log(20)


## Version using the rescaled range.

closep <- cumsum(retp)
# Calculate the rescaled ranges
rrange <- sapply(2:(npts-1), function(np) {
  indeks <- (endd[np-1]+1):endd[np]
  stdev <- sd(retp[indeks])
  c(rrange=diff(range(closep[indeks]))/stdev, stdev=stdev)
})  ##  end sapply
rrange <- t(rrange)
mean(rrange[, "rrange"])

# Calculate the Hurst exponent
log(mean(rrange[, "rrange"]))/log(20)

# Select the intervals with higher volatility
indic <- (rrange[, "stdev"] > 0.015)
# indic <- c(FALSE, indic)
sum(indic)

# Select the intervals with higher volatility
retsh <- lapply(endd[(2:(npts-1))[indic]], function(ep) {
  retp[(ep-19):ep, ]
})  ##  end lapply
retsh <- do.call(rbind, retsh)
sd(retsh)

# Select the intervals with lower volatility
retsl <- lapply(endd[(2:(npts-1))[!indic]], function(ep) {
  retp[(ep-19):ep, ]
})  ##  end lapply
retsl <- do.call(rbind, retsl)
sd(retsl)

# Calculate the Hurst for the higher volatility intervals
hursth <- log(mean(rrange[indic, "rrange"]))/log(20)
# Calculate the Hurst for the lower volatility intervals
hurstl <- log(mean(rrange[!indic, "rrange"]))/log(20)



##############
## Variance ratios

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
})  ##  end sapply
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
})  ##  end sapply
varatios <- sort(unlist(varatios), decreasing=TRUE)
# symbolv <- names(varatios)

# Find PCAs with largest variance ratios
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
pcad <- prcomp(retp, scalit=TRUE)
pcarets <- xts(pcad$x/100, order.by=index(retp))
varatios <- sapply(pcarets, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  ##  end sapply
varatios <- sort(unlist(varatios), decreasing=TRUE)
pcarets <- pcarets[, names(varatios)]
save(pcarets, file="/Volumes/external/Develop/data/pcarets.RData")
x11()
dygraphs::dygraph(cumsum(pcarets[, "PC2"]))
barplot(sort(pcad$rotation[, "PC2"]))

# Second PCA
retc <- cumsum(pcarets)
endd <- rutils::calc_endpoints(pcarets, interval=5)
retc <- retc[endd, ]
retc <- rutils::diffit(retc)
pcad <- prcomp(retc, scalit=TRUE)
pcarets <- xts(pcad$x/100, order.by=index(retc))
varatios <- sapply(pcarets, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  ##  end sapply
varatios <- sort(unlist(varatios), decreasing=TRUE)


## optim
objfun <- function(weightv, returns, lagg) {
  retp <- (retp %*% weightv)
  -calc_var(retp, lagg)/calc_var(retp)/lagg
}  ##  end objfun

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



##############
## Hurst stuff

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
})  ##  end eapply
hurstv <- sort(hurstv, decreasing=TRUE)

plot(hurstv, varatios)
text(x=hurstv, y=varatios, labels=names(varatios))


# Calculate the Hurst from returns
endd <- rutils::calc_endpoints(retp, lagg)
hurstv <- sapply(retp, calc_hurst_rets, endd)
hurstv <- sort(hurstv, decreasing=TRUE)

# Find stocks with largest Hurst
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
startd <- "2000-01-01"
hurstv <- eapply(sp500env, function(ohlc) {
  ##  ohlc <- get(symbol, rutils::etfenv)
  ##  Check if data starts before 2000
  if (start(ohlc) < startd) {
    ohlc <- ohlc[paste0("/", startd)]
    endd <- rutils::calc_endpoints(ohlc, lagg)
    highp <- Hi(ohlc)
    lowp <- Lo(ohlc)
    calc_hurst_hilo(highp, lowp, endd)
  } else NULL
})  ##  end eapply
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
}  ##  end objfun


# Portfolio optimization using principal components
# Perform PCA
pcad <- prcomp(retp, center=TRUE, scalit=TRUE)
# eigend <- eigen(cor(retp))
# all.equal(abs(pcad$rotation), abs(eigend$vectors), check.attributes=FALSE)
# Calculate the principal component time series
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
}  ##  end objfun
# library(parallel)
# num_cores <- detectCores()
# cluster <- makeCluster(num_cores-1)
# clusterExport(cluster, varlist=c("calc_hurst_rets"))
optimd <- DEoptim::DEoptim(objfun,
                           retp=rets_pca,
                           endd=endd,
                           upper=rep(10, nweightv),
                           lower=rep(-10, nweightv),
                           ##  cluster=cluster,
                           ##  parVar=c("calc_hurst_rets"),
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
colv <- colnames(datav)
dygraphs::dygraph(datav, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red")



## Calculate the autocorrelations
retp <- rutils::etfenv$returns
retp[1, is.na(retp[1, ])] <- 0
retp <- zoo::na.locf(retp, na.rm=FALSE)
returns_lag <- rutils::lagit(retp)
auto_cor <- sapply(colnames(retp), function(symbol) {
  retp <- retp[, symbol]
  mean(retp*retp_lag[, symbol])/var(retp)
})  ##  end sapply
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
}  ##  end objfun
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
                           ##  cluster=cluster,
                           ##  parVar=c("calc_hurst_rets"),
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
colv <- colnames(datav)
dygraphs::dygraph(datav, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue") %>%
  dySeries(name=colv[2], axis="y2", col="red")




# More stuff below

datav <- exp(cumsum(rnorm(1e7)/100))
datev <- seq.POSIXt(from=Sys.time(), by="sec", length.out=NROW(datav))
datav <- xts(datav, datev)

aggv <- seq.int(from=1e2, to=1e3, by=1e2)
volv <- sapply(aggv, function(aggn) {
  ##  spy_agg <- rutils::to_period(ohlc=datav, k=aggn)
  ##  HighFreq::calc_var_ohlc(spy_agg)
  endd <- rutils::calc_endpoints(datav, interval=aggn)
  sd(rutils::diffit(log(datav[endd])))
})  ##  end sapply

aggv <- c("seconds", "minutes", "hours", "days")
agglog <- log(c(1, 60, 3600, 6.5*3600))
agglog <- log(c(1, 60, 3600, 24*3600))
volv <- sapply(aggv, function(aggn) {
  spy_agg <- rutils::to_period(ohlc=HighFreq::SPY, k=aggn)
  ##  spy_agg <- rutils::to_period(ohlc=HighFreq::SPY, period=aggn)
  ##  HighFreq::calc_var_ohlc(spy_agg)
  retp <- rutils::diffit(spy_agg[, 4])
  sd(retp)
})  ##  end sapply


names(volv) <- paste0("agg_", aggv)
volog <- log(volv)
agglog <- log(aggv)
agglog <- agglog - mean(agglog)
volog <- volog - mean(volog)
regmod <- lm(volog ~ agglog)
hurstlm <- summary(regmod)$coeff[2, 1]
hurstfun <- sum(volog*agglog)/sum(agglog^2)
all.equal(hurstlm, hurstfun)





##############
## Forecasting for univariate regression

library(HighFreq)
source("/Users/jerzy/Develop/R/scripts/market_making.R")

data_dir <- "/Volumes/external/Develop/data/ib_data/"
symbol <- "ES"
load(paste0(data_dir, symbol, "_ohlc.RData"))
nrows <- NROW(ohlc)
ohlc <- coredata(ohlc)

lookb <- 500
predm <- cbind(rep(1, lookb), 1:lookb)
degf <- (lookb - NCOL(predm))
predinv <- MASS::ginv(predm)
pred2 <- MASS::ginv(crossprod(predm))
lagg <- 1
datax <- cbind(1, lookb + lagg)
datat <- t(datax)

# influ_ence <- predm %*% predinv
# fit_ted <- drop(influ_ence %*% tseries)

# tseries <- ohlc[(lookb+1):(lookb+100), 4]
# closep <- ohlc[201, 4]
# calc_zscore(closep, tseries, predm, predinv, pred2, datax, datat, degf)

zscores <- sapply((lookb+1):nrows, function(x) {
  tseries <- ohlc[(x-lookb):(x-1), 4]
  closep <- ohlc[x, 4]
  calc_zscore(closep, tseries, predm, predinv, pred2, datax, datat, degf)
})  ##  end sapply
zscores <- c(rep(0, lookb), zscores)

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




##############################
### Crossover strategies
##############################

##############
# Intraday dual EMA crossover strategy for ETFs.

# Load the list of intraday minute prices for ETFs
load("/Users/jerzy/Develop/data/minutes/SPY.RData")

# Add the names to the list of time series of prices
names(pricel) <- as.Date(sapply(pricel, function(x) as.Date(end(x))))
save(pricel, file="/Users/jerzy/Develop/data/minutes/SPY.RData")

# Select day of the tariff panic
dayy <- "2025-04-07"
ohlc <- pricel[[dayy]]
# Scrub the price spikes
pricev <- ohlc[, 4]
# pricev <- scrub_center(pricev, tolv=2)
scrub_online(pricev, tolv=2)
pricev <- xts::xts(pricev, order.by=index(ohlc))
dygraphs::dygraph(pricev, main=paste("SPY Minute Prices on", dayy)) %>%
  dyOptions(colors=c("blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# Calculate the PnLs of the dual EMA crossover strategy
lambdaf <- 0.21
lambdas <- 0.99
timev <- "T06:00:00/T09:30:00"
coeff <- -1
pricev <- pricev[timev, ]
retv <- rutils::diffit(pricev)
nrows <- NROW(pricev)
emaf <- HighFreq::run_mean(pricev, lambda=lambdaf)
emas <- HighFreq::run_mean(pricev, lambda=lambdas)
crossi <- sign(emaf - emas)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(crossi == 1, coeff, posv)
posv <- ifelse(crossi == (-1), -coeff, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- rutils::lagit(posv, lagg=1)
pnls <- posv*retv
tradez <- abs(rutils::diffit(posv))
# costs <- 0.5*input$bidask*tradez
# pnls <- (pnls - costs)
wealth <- cbind(retv, pnls)
colnames(wealth) <- c("SPY", "Strategy")
colv <- colnames(wealth)

captiont <- paste("Intraday Dual EMA Crossover Strategy")
dygraphs::dygraph(cumsum(wealth), main=captiont) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)



##############
# VWAP strategies

# Load a single day of intraday minute bar prices
filen <- "/Users/jerzy/Develop/data/minutes/SPY/2025-06-13.csv"
ohlc <- xts::as.xts(zoo::read.zoo(file = filen,
                                  header = TRUE, sep = ",",
                                  FUN = as.POSIXct,
                                  format = "%Y-%m-%d %H:%M:%S"))
colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
dygraphs::dygraph(ohlc$Volume, main="SPY Minute Volumes on 2025-06-13")
hilo <- ohlc$High - ohlc$Low
dygraphs::dygraph(hilo, main="SPY Minute (High - Low) on 2025-06-13")

# Market liquidity is equal to the square root of the trading volume
# divided by the price volatility
liqv <- sqrt(ohlc$Volume)/hilo
dygraphs::dygraph(liqv, main="SPY Liquidity on 2025-06-13")
datav <- cbind(ohlc$Close, liqv)
colnames(datav) <- c("Close", "Liquidity")
colv <- colnames(datav)
# Market liquidity is mostly noise
dygraphs::dygraph(datav["2025-06-13 09:30:00/2025-06-13 16:00:00"],
                  main="SPY Close and Liquidity on 2025-06-13") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

dygraphs::dygraph(datav["2025-06-13 09:30:00/2025-06-13 16:00:00"],
                  main="SPY Close and Liquidity on 2025-06-13") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Get the prices during the trading hours
pricen <- ohlc["2025-06-13 09:30:00/2025-06-13 16:00:00"]
# Get the Close and VWAP prices
clvwap <- pricen[, c("Close", "VWAP")]
clvwap <- cbind(clvwap, 0.5*(clvwap$Close + clvwap$VWAP))
colnames(clvwap) <- c("Close", "VWAP", "Average")
clvwapd <- rutils::diffit(clvwap)
dygraphs::dygraph(clvwap, main="Close and VWAP") %>%
  dyOptions(colors=c("blue", "green", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the partial autocorrelation function of the VWAP
pacf(clvwapd$VWAP, lag=10)
pacfv <- sapply(clvwapd, function(x) {
  pacfl <- pacf(na.omit(x), lag=10, plot=FALSE)
  sum(pacfl$acf)
})
pacfv <- sort(pacfv)

# Idea: Combine all the strategies below into a single strategy

# VWAP trending: position is equal to the lagged difference of the VWAP.
posv <- sign(rutils::lagit(clvwapd$Average, lagg=1))
pnls <- posv*clvwapd$Close
wealthv <- cbind(clvwapd$Close, pnls)
colnames(wealthv) <- c("SPY", "PnLs")
dygraphs::dygraph(cumsum(wealthv), main="VWAP Trending Strategy PnLs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# VWAP trending: position is equal to the EMA of the lagged difference of the VWAP.
VWAPEMA <- HighFreq::run_mean(clvwapd$Average, lambda=0.9)
posv <- sign(rutils::lagit(VWAPEMA, lagg=1))
pnls <- posv*clvwapd$Close
wealthv <- cbind(clvwapd$Close, pnls)
colnames(wealthv) <- c("SPY", "PnLs")
dygraphs::dygraph(cumsum(wealthv), main="VWAP Trending Strategy PnLs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# VWAP-stock crossover: position is equal to the lagged difference of the stock price minus the VWAP.
posv <- -sign(rutils::lagit(clvwap$VWAP - clvwap$Average, lagg=1))
pnls <- posv*clvwapd$Close
wealthv <- cbind(clvwapd$Close, pnls)
colnames(wealthv) <- c("SPY", "PnLs")
dygraphs::dygraph(cumsum(wealthv), main="VWAP Crossover Strategy PnLs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# VWAP crossover: position is equal to the lagged difference of the VWAP minus the EMA of the VWAP.
VWAPEMA <- HighFreq::run_mean(clvwap$Average, lambda=0.9)
posv <- sign(rutils::lagit(clvwap$Average - VWAPEMA, lagg=1))
pnls <- posv*clvwapd$Close
wealthv <- cbind(clvwapd$Close, pnls)
colnames(wealthv) <- c("SPY", "PnLs")
dygraphs::dygraph(cumsum(wealthv), main="VWAP Crossover Strategy PnLs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# Plot the Close and the EMA of the VWAP
datav <- cbind(ohlc$Close, VWAPEMA)
colnames(datav) <- c("Close", "VWAPEMA")
colv <- colnames(datav)
dygraphs::dygraph(datav["2025-06-13 09:30:00/2025-06-13 16:00:00"],
                  main="SPY Close and VWAPEMA on 2025-06-13") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)



# Volume reverting: position is equal to minus the lagged difference of the Volume.
posv <- sign(rutils::lagit(rutils::diffit(ohlc$Volume), lagg=1))
retv <- rutils::diffit(ohlc$Close)
pnls <- posv*retv
wealthv <- cbind(retv, pnls)
colnames(wealthv) <- c("SPY", "PnLs")
dygraphs::dygraph(cumsum(wealthv), main="Volume Reverting Strategy PnLs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


##############
## Bolllinger bands strategy
# Load pricev minute SPY prices for 20240531
load(file="/Users/jerzy/Develop/data/SPY_minute_20240531.RData")
nrows <- NROW(pricev)

# Calculate the z-scores
lambdaf <- 0.9
zscores <- (pricev - rutils::lagit(HighFreq::run_mean(pricev, lambda=lambdaf)))
volv <- rutils::lagit(sqrt(HighFreq::run_var(pricev, lambda=lambdaf)))
zscores <- ifelse(volv > 0, zscores/volv, 0)
zscores <- xts::xts(zscores, index(pricev))

# Trade the z-scores
retv <- rutils::diffit(pricev)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(zscores > 1, -1, posv)
posv <- ifelse(zscores < (-1), 1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- rutils::lagit(posv)
pnls <- retv*posv
# Calculate transaction costs
costv <- 0.05*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
pnls <- cbind(retv, pnls)
colnames(pnls) <- c("Pair", "Strategy")
colv <- colnames(pnls)
dygraphs::dygraph(cumsum(pnls), main="Simulated Performance of Pair Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



# Plot the z-scores
zscores <- cbind(zscores, rep(1, nrows), rep(-1, nrows))
colnames(zscores) <- c("Z-Scores", paste("\U03B8 = 1"), paste("\U03B8 = -1"))
colv <- colnames(zscores)
dygraphs::dygraph(zscores, main="Z-Scores For SPY Minute Prices") %>%
  dyOptions(colors=c("blue", "red", "red"), strokeWidth=2) %>%
  dySeries(name=colv[2], strokePattern="dashed", strokeWidth=2) %>%
  dyAnnotation(x=index(zscores[111]), width=60, height=20, text=colv[2]) %>%
  dySeries(name=colv[3], strokePattern="dashed", strokeWidth=2) %>%
  dyAnnotation(x=index(zscores[211]), width=60, height=20, text=colv[3]) %>%
  dyLegend(show="never")




##############################
### Volatility strategies
##############################

##############
# Beta of AAPL with respect to VTI
# using the range of daily prices.

# Load the VTI OHLC prices
ohlc <- rutils::etfenv$VTI["2023"]
hilovti <- (ohlc[, 2] - ohlc[, 3])

# Load the AAPL OHLC prices
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
ohlc <- sp500env$AAPL["2023"]
hiloaapl <- (ohlc[, 2] - ohlc[, 3])
all.equal(index(hilovti), index(hiloaapl))

# Calculate the beta of AAPL with respect to VTI
betav <- cov(hilovti[, 1], hiloaapl[, 1])/var(hilovti[, 1])
betav <- drop(betav)

pricel <- lapply(seq_along(pricespy), function(it) {
  (pricaapl[[it]] - betav*pricespy[[it]])
}) ##  end lapply



##############
# Summary: Simulate a volatility timing strategy using 
# the downside volatility.
# Conclusion:
# The downside volatility decays faster than standard 
# volatility but the volatility timing strategy does not
# perform better.

## Run the setup code below

library(rutils)

# The bid-ask spread for the most liquid ETFs
bidask <- 0.0001
# Calculate the SPY daily percentage returns
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
datev <- zoo::index(retp)
# Calculate the downside returns
retd <- retp
retd[retd > 0] <- 0
# Calculate the EMA downside volatility
volv <- HighFreq::run_var(retd, lambda=0.5)
volv <- sqrt(volv[, 2])
volv[1:5] <- 1  ##  To avoid initial NAs

# Calculate the EMA downside volatility
lambdaf <- 0.9
varv <- HighFreq::run_var(retp, lambdaf=lambdaf)
varv <- sqrt(varv[, 2])
vard <- HighFreq::run_var(retd, lambdaf=lambdaf)
vard <- sqrt(vard[, 2])
# Plot dygraph of SPY and downside volatility
datav <- xts::xts(cbind(varv, vard), order.by=datev)
dygraph(datav, main="SPY Volatility and Downside Volatility") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

## End of setup code


# 1. (10pts)
# Calculate the positions as the inverse of the lagged 
# EMA volatility.
# You can use the function rutils::lagit().
# Set the first value of volv to 1, to avoid division 
# by zero.

volv <- rutils::lagit(volv)
volv[1] <- 1
posv <- 1/volv
posv <- na.locf(posv)

# You should get the following outputs:
NROW(posv)
# [1] 8298
head(posv)
# [1] 1.0000 201.1293 304.0789 328.5427 473.3764 329.8407
tail(posv)
# [1] 568.8807 345.5384 605.4911 473.4110 403.9418 397.2903


# Calculate the PnLs minus the transaction costs.
# Scale the PnL volatility to that of SPY.

pnls <- retp*posv
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# You should get the following outputs:
head(pnls)
#                     SPY
# 1993-02-01  2.453129e-05
# 1993-02-02  1.437974e-03
# 1993-02-03  1.105102e-02
# 1993-02-04  4.744005e-03
# 1993-02-05 -1.165146e-03
# 1993-02-08 -2.484138e-05
tail(pnls)
#                     SPY
# 2026-01-09  0.012930946
# 2026-01-12  0.001838174
# 2026-01-13 -0.004239854
# 2026-01-14 -0.008096925
# 2026-01-15  0.003790361
# 2026-01-16 -0.001153826

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c(symboln, "Strategy")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get the following outputs:
#               SPY  Strategy
# Sharpe  0.5463022 0.6791736
# Sortino 0.6809656 0.8291622


# Plot dygraph of the strategy.
# Your plot should be similar to strat_vol_timing_spy.png

endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main="Volatility Timing Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



# 2. (20pts)
# Calculate the Sharpe ratios of the strategy for 
# the vector of lambda decay parameters lambdav.
# Use parallel computing with the package parallel.

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
lambdav <- seq(0.1, 0.9, 0.1)

pnlv <- mclapply(lambdav, function(lambdaf) {
  ##  Calculate the EMA volatility
  volv <- HighFreq::run_var(retd, lambda=lambdaf)
  volv <- sqrt(volv[, 2])
  volv <- rutils::lagit(volv)
  volv[1:5] <- 1
  ##  Calculate the positions and PnLs
  posv <- 1/volv
  pnls <- retp*posv
  costv <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costv)
  return(sqrt(252)*mean(pnls)/sd(pnls))
}, mc.cores=ncores)  ## end mclapply
pnlv <- unlist(pnlv)

# You should get the following outputs:
round(pnlv, 4)
# [1] 0.5959 0.6973 0.7080 0.6958 0.6792 0.6566 0.6386 0.6363 0.6545

# Calculate the largest Sharpe ratio and its lambda 
# decay parameter.
# You can use the functions max(), which.max().

max(pnlv)
lambdaf <- lambdav[which.max(pnlv)]

# You should get the following outputs:
# [1] 0.7080153
# [1] 0.3

# Plot the Sharpe ratios versus lambda decay parameters.
# Your plot should be similar to strat_vol_timing_profile_spy.png

plot(lambdav, pnlv, type="b",
     main="Volatility Timing Strategy",
     xlab="Lambda Decay Factor", ylab="Sharpe Ratio")


##############
# Summary: Simulate a crossover strategy using the VIX index.

# Motivation:
# The high-low range of the VIX index drops (decays)
# much faster than the VIX index level itself.
# But the high-low range is much more volatile (noisy).



# Get the VIX index prices
load(file="/Users/jerzy/Develop/data/vix/vix_data.RData")
vixohlc <- vixenv$vixohlc
datev <- zoo::index(vixohlc)
# Get the SPY prices
spy <- rutils::etfenv$prices$SPY[datev]
retspy <- rutils::diffit(log(spy))
datev <- zoo::index(spy)
vixohlc <- vixohlc[datev]
vixp <- quantmod::Cl(vixohlc)
nrows <- NROW(vixohlc)

highp <- quantmod::Hi(vixohlc)
lowp <- quantmod::Lo(vixohlc)
hilo <- highp - lowp
hilo <- hilo + rutils::lagit(hilo, lagg=1)
minp <- HighFreq::run_min(hilo, lambda=0.7)
maxp <- HighFreq::run_max(hilo, lambda=0.7)

riskp <- hilo/vixp
riskp <- retspy/hilo

datav <- na.omit(cbind(hilo, minp))
colnames(datav) <- c("VIX", "Min")
dygraph(datav["2020"]) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)

datav <- na.omit(cbind(rutils::etfenv$prices$SPY, hilo))
colnames(datav) <- c("SPY", "VIX_HL")
# Plot dygraph of SPY and VIX high-low range with dual y-axes
dygraph(datav["2020"]) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyAxis("y", label="SPY", independentTicks=TRUE) %>%
  dyAxis("y2", label="VIX_HL", independentTicks=TRUE) %>%
  dySeries(name="SPY", axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name="VIX_HL", axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)


# Go short SPY if the VIX high-low range is 20% above
# its running minimum.
# Go long SPY if the VIX high-low range is 20% below
# its running maximum.

posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv <- ifelse(hilo > 1.2*minp, -1, posv)
posv <- ifelse(hilo < 0.8*maxp, 1, posv)
posv <- zoo::na.locf(posv, na.rm=FALSE)
posv <- rutils::lagit(posv, lagg=1)
pnls <- posv*retspy
# tradez <- abs(rutils::diffit(posv))
# costs <- 0.5*input$bidask*tradez
# pnls <- (pnls - costs)
wealth <- cbind(retspy, pnls)
colnames(wealth) <- c("SPY", "Strategy")
colv <- colnames(wealth)
captiont <- paste("VIX Crossover Strategy")
dygraphs::dygraph(cumsum(wealth), main=captiont) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)


##############
# Summary: Simulate an autoregressive strategy using
# past returns and the differenced squared returns.

# Motivation:
# The autoregressive coefficients of stock returns
# depend on the level of volatility.
# The autoregressive coefficients are more negative
# when the volatility is higher.

# Result:
# There are at least four ways of using the
# volatility as a predictor in the the AR model:
# 1. Use the squared returns.
# 2. Use the de-meaned squared returns.
# 3. Use the differenced squared returns.
# 4. Use the VIX index.
#
# The first three produce similar results.

# Result:
# Adding the squared returns as predictors improves
# the out-of-sample performance of the AR strategy
# by only a small amount.

# Idea:
# Use the t-values as the coefficients.

# Idea:
# Use EMA returns and volatility.

# Idea:
# Use VIX instead of realized volatility.

# Idea:
# Use OHLC prices.  Scale the returns by the high-low
# range.



# Define the response and predictor matrices
retp <- na.omit(rutils::etfenv$returns$SPY)
datev <- zoo::index(retp)
nrows <- NROW(retp)
respv <- retp
orderp <- 2
# Add the lagged returns as predictors
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0("lag", 1:orderp)
# Add the differenced squared returns as predictors
volv <- respv^2/sd(respv)
volv <- volv - mean(volv)
retsq <- lapply(1:orderp, rutils::lagit, input=volv)
retsq <- rutils::do_call(cbind, retsq)
# retsq <- rutils::diffit(retsq)
colnames(retsq) <- paste0("retsq", 1:orderp)
# Demean the predictors
predm <- cbind(predm, retsq)
predm <- lapply(predm, function(x) return(x - mean(x)))
predm <- rutils::do_call(cbind, predm)
sapply(predm, mean)
# Add the intercept
predm <- cbind(rep(1, nrows), predm)
colnames(predm)[1] <- "phi0"

# Define in-sample and out-of-sample intervals
insample <- 1:(nrows %/% 2)
outsample <- (nrows %/% 2 + 1):nrows
cutoff <- nrows %/% 2

# Calculate the in-sample fitted AR coefficients
predinv <- MASS::ginv(predm[insample, ])
coeff <- drop(predinv %*% respv[insample, ])
names(coeff) <- colnames(predm)
# Calculate the in-sample forecasts of SPY (fitted values)
fcasts <- predm %*% coeff

# Calculate the AR strategy PnLs
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Calculate the EMA returns and volatility.
lambdaf <- 0.9
volma <- HighFreq::run_var(retp, lambda=lambdaf)
retma <- volma[, 1]
volma <- sqrt(volma[, 2])

# Calculate the in-sample and out-of-sample Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("SPY", "ARMulti")
sqrt(252)*sapply(wealthv[insample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sqrt(252)*sapply(wealthv[outsample, ], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the AR strategies
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
colorv <- colorRampPalette(c("blue", "red"))(NCOL(wealthv))
dygraphs::dygraph(cumsum(wealthv)[endw],
                  main="Multifactor Autoregressive Strategy") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyEvent(datev[cutoff], label="cutoff", strokePattern="solid", color="red") %>%
  dyLegend(show="always", width=300)




# Not promising idea
##############
# Summary: Trend following strategy using the EMA covariance.

# The covariance drops when stocks drop, but it's quite noisy.

# Calculate the EMA covariance, variances and the returns
retp <- na.omit(rutils::etfenv$returns[, c("SPY", "TLT")])
retew <- 0.4*retp[, 1] + 0.6*retp[, 2]
lambdaf <- 0.995
# HighFreq::run_covar() returns a matrix with five columns
# of data:
# The EMA covariances, the variances, and the EMA means.
covm <- 1e3*HighFreq::run_covar(retp, lambdaf)
covm <- rutils::lagit(covm, lagg=1)
covv <- covm[, 1]
var1 <- covm[, 2]
var2 <- covm[, 3]
ret1 <- covm[, 4]
ret2 <- covm[, 5]


datav <- cbind(cumsum(retp$SPY), covv)
colv <- colnames(datav)
dygraphs::dygraph(datav[-(1:1000), ],
                  main="SPY and Covariance") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)



############## Update the GARCH slides
# Summary: In the likelihood function for the GARCH model, 
# transform the parameters so they satisfy a+b<1
# Then calibrate the GARCH model
# Add the argument mu for the average return?

# Log-likelihood of GARCH(1,1)
garch_ll <- function(theta, r) {
  ##  Transform parameters to enforce constraints
  omega <- exp(theta[1])
  a_raw <- exp(theta[2])
  b_raw <- exp(theta[3])
  
  denom <- 1 + a_raw + b_raw
  alpha <- a_raw / denom
  beta  <- b_raw / denom
  
  ##  Initialize conditional variance
  T <- length(r)
  h <- rep(var(r), T)
  
  ##  Estimate the GARCH(1,1) variance
  for (t in 2:T) {
    h[t] <- omega + alpha * r[t-1]^2 + beta * h[t-1]
  }
  
  ##  Gaussian log-likelihood
  ll <- -0.5 * sum(log(h) + r^2 / h)
  return(-ll)  ##  optim() minimizes
}

# Calibration wrapper
calibrate_garch <- function(r) {
  theta0 <- c(log(var(r)), 0, 0)  ##  reasonable starting point
  
  opt <- optim(
    par = theta0,
    fn  = garch_ll,
    r   = r,
    method = "BFGS",
    control = list(maxit = 5000)
  )
  
  ##  Extract parameters
  theta <- opt$par
  omega <- exp(theta[1])
  a_raw <- exp(theta[2])
  b_raw <- exp(theta[3])
  
  denom <- 1 + a_raw + b_raw
  alpha <- a_raw / denom
  beta  <- b_raw / denom
  
  list(
    omega = omega,
    alpha = alpha,
    beta  = beta,
    convergence = opt$convergence,
    loglik = -opt$value
  )
}




##############################
### Intraday strategies
##############################


##############
# Calculate the seasonality of autocorrelations of intraday returns.

# Calculate SPY percentage returns
symboln <- "SPY"
ohlc <- HighFreq::SPY
# Load the intraday returns of QQQ from RData file
loadd <- load("/Users/jerzy/Develop/data/minutes/QQQ_minutes_markets.RData")
# Load the list of intraday returns of QQQ from RData file
loadd <- load("/Users/jerzy/Develop/data/minutes/QQQ_2025.RData")
ohlc <- do.call(rbind, get(loadd))
# Calculate the log prices
symboln <- rutils::get_name(colnames(ohlc)[1])
pricev <- log(quantmod::Cl(ohlc))
colnames(pricev) <- symboln
nrows <- NROW(ohlc)
# Calculate the endpoints of 5-minute intervals
# endd <- rutils::calc_endpoints(pricev, interval=5)
# pricev <- pricev[endd]
# Calculate the returns
retp <- rutils::diffit(pricev, lagg=1)
# Calculate the lagged returns
retl <- rutils::lagit(retp, lagg=1)
# Calculate the instantaneous autocorrelation of returns
covv <- (retp*retl)/retp^2
covv[is.na(covv)] <- 0
sum(is.na(covv))
madv <- mad(covv)
plot(density(covv), xlim=c(-20*madv, 20*madv))
covv[abs(covv) > 100*madv] <- 0
# Aggregate the autocorrelations by time of day
dated <- format(zoo::index(pricev), "%H:%M")
covagg <- tapply(X=covv, INDEX=dated, FUN=mean)
# Coerce to xts
datev <- as.POSIXct(paste(Sys.Date(), names(covagg)))
covagg <- xts::xts(covagg, datev)
# covagg <- covagg[-(1:6)]
covagg[abs(covagg) > 1] <- 0
dygraph(covagg, main="Seasonality of Intraday Autocorrelation") %>%
  dyOptions(colors="blue", strokeWidth=1)
# Aggregate covagg by hour
hourd <- format(zoo::index(covagg), "%H")
covagh <- tapply(X=covagg, INDEX=hourd, FUN=mean)
# Plot with x-axis as hour of day
datev <- as.POSIXct(paste(Sys.Date(), names(covagh)), format="%Y-%m-%d %H")
covagh <- xts::xts(covagh, datev)
dygraph(covagh, main="Seasonality of Intraday Autocorrelation by Hour") %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyAxis("x", label="Hour of Day", valueFormatter = "function(d) { return new Date(d).getHours(); }")



##############
# Load a single day of intraday minute bar prices

filen <- "/Users/jerzy/Develop/data/minutes/SPY/2024-02-29.csv"
ohlc <- xts::as.xts(zoo::read.zoo(file = filen,
                                  header = TRUE, sep = ",",
                                  FUN = as.POSIXct,
                                  format = "%Y-%m-%d %H:%M:%S"))
dygraphs::dygraph(ohlc$SPY.Volume, main="SPY Minute Volumes on 2024-02-29")



##############
# Load the list of intraday minute prices, coerce them to numeric, and change
# the time zone to UTC

load("/Users/jerzy/Develop/data/minutes/SPY.RData")
pricev <- rutils::do_call(rbind, pricel)
datev <- index(pricev)
pricev <- lapply(pricev, as.numeric)
pricev <- rutils::do_call(cbind, pricev)
colnames(pricev) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
pricev <- xts(pricev, datev)

load("/Users/jerzy/Develop/data/minutes/XLK.RData")
pricel <- lapply(pricel, function(pricev) {
  datev <- index(pricev)
  datev <- lubridate::force_tz(datev, "UTC") ##  Same clock time in UTC time zone
  datev <- lubridate::with_tz(datev, "America/New_York") ##  Same moment of time in New_York time zone.
  pricev <- lapply(pricev, as.numeric)
  pricev <- rutils::do_call(cbind, pricev)
  pricev <- xts(pricev, datev)
  colnames(pricev) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
  pricev
}) ##  end lapply
save(pricel, file="/Users/jerzy/Develop/data/minutes/XLK.RData")

pricev <- pricel[[111]]
class(drop(coredata(pricev$Close)))
dygraph(pricev$Volume)

datev <- index(pricev)


##############
# Intraday beta of XLK versus SPY

# Load minute XLK and SPY prices for 202405
load(file="/Users/jerzy/Develop/data/XLK_minute_202405.RData")
load(file="/Users/jerzy/Develop/data/SPY_minute_202405.RData")

hilospy <- sapply(pricespy, function(x) max(x)-min(x))
hiloxlk <- sapply(pricexlk, function(x) max(x)-min(x))
regmod <- lm(hiloxlk ~ hilospy)
summary(regmod)
plot(hiloxlk ~ hilospy)
abline(regmod, col="red", lwd=2)
# betav ~ 0.464
betav <- drop(cov(hilospy, hiloxlk)/var(hilospy))

foo <- lapply(seq_along(pricexlk), function(x) (pricexlk[[x]] - betav * pricespy[[x]]))
foo <- do.call(rbind, foo)
dygraph(foo)

# Using the daily OHLC prices
ohlc <- rutils::etfenv$XLK["2020/"]
hiloxlk <- (ohlc[, 2] - ohlc[, 3])
ohlc <- rutils::etfenv$VTI["2020/"]
hilovti <- (ohlc[, 2] - ohlc[, 3])
hiloxlk <- as.numeric(hiloxlk)
hilovti <- as.numeric(hilovti)
regmod <- lm(hiloxlk ~ hilovti)
summary(regmod)
plot(hiloxlk ~ hilovti)
abline(regmod, col="red", lwd=2)


##############
## Load Polygon data from CSV file

# rbind prices
dtable <- data.table::fread("/Users/jerzy/Develop/data/SPY_second_big_20230623.csv")
datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
foo <- xts::xts(dtable[, .(price, volume)], order.by=datev)
colnames(foo)[1] <- "SPY"
load(file="/Users/jerzy/Develop/data/SPY_second_202306.RData")
head(pricev)
tail(pricev)
pricev <- rbind(pricev, foo)
save(pricev, file="/Users/jerzy/Develop/data/SPY_second_202306.RData")
dygraphs::dygraph(pricev$SPY, main="SPY Big Ticks (>= 100)") %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Set option to display fractional seconds
options(digits.secs=6)

# Load a data table with second QQQ big ticks (>= 100) from CSV file
dtable <- data.table::fread("/Users/jerzy/Develop/data/SPY_second_202306.csv")
# Subset columns
# dtable <- dtable[, list(datetime, price, avg_price, unixtime)]
# Coerce unixtime in seconds to POSIXct - needs microseconds
# dtable[, date := as.POSIXct(unixtime, origin="1970-01-01", tz="America/New_York")]
# pricev <- xts::xts(dtable$price, order.by=dtable$date)
datev <- as.POSIXct(dtable$timestamp/1e9, origin="1970-01-01", tz="America/New_York")
# datev <- seq.POSIXt(as.POSIXct("2023-05-22 09:30:00", origin="1970-01-01", tz="America/New_York"),
#                     by=1, length.out=NROW(dtable))
pricev <- xts::xts(dtable[, .(price, volume)], order.by=datev)
colnames(pricev)[1] <- "SPY"
# Save SPY prices to .RData file
save(pricev, file="/Users/jerzy/Develop/data/SPY_second_202306.RData")
dygraphs::dygraph(pricev$SPY, main="SPY Big Ticks (>= 100)") %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Calculate the trailing means and volatility
lambdaf <- 0.9
meanv <- HighFreq::run_mean(pricev, lambda=lambdaf)
zscores <- (pricev - meanv)
volat <- HighFreq::run_var(zscores, lambda=lambdaf)
volat <- sqrt(volat)
zscores <- ifelse(volat > 0, zscores/volat, 0)

# Plot dygraph of the trailing z-scores of VTI prices
datav <- cbind(pricev, zscores)
colnames(datav) <- c("QQQ", "Z-Scores")
colv <- colnames(datav)
dygraphs::dygraph(datav,
                  main="QQQ Trailing Price Z-Scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)



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
# Calculate the centered Hampel filter over 3 data points
medianv <- roll::roll_median(dtable$price, width=3)
medianv[1:2] <- dtable$price[1:2]
medianv <- rutils::lagit(medianv, lagg=-1, pad_zeros=FALSE)
madv <- HighFreq::roll_var(matrix(dtable$price), lookb=3, method="nonparametric")
madv <- rutils::lagit(madv, lagg=-1, pad_zeros=FALSE)
# Calculate the Z-scores
zscores <- ifelse(madv > 0, (dtable$price - medianv)/madv, 0)
range(zscores); mad(zscores)
madz <- mad(zscores[abs(zscores)>0])
hist(zscores, breaks=2000, xlim=c(-1*madz, 1*madz))
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
colv <- colnames(datav)
dygraphs::dygraph(datav, main="SPY and z-scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red")



##############
# Intraday contrarian strategy which reverts to the open price:
# Sell if current price is above the open price,
# and Buy if current price is below the open price.

# Calculate the positions
sim_revert <- function(pricev, threshz) {
  nrows <- NROW(pricev)
  retp <- rutils::diffit(pricev)
  volv <- sd(retp[1:11])
  pricev <- as.numeric(pricev)
  openp <- pricev[1]
  posv <- numeric(nrows)
  zscoremax <- 0
  zscoremin <- 0
  for (td in 12:nrows) {
    ##  cat("td =", td, "\n")
    zscores <- round((pricev[td] - openp)/volv)
    if ((zscores > threshz) & (zscores > zscoremax)) {
      posv[td] <- (-1)
      zscoremax <- zscores
    } else if ((zscores < -threshz) & (zscores < zscoremin)) {
      posv[td] <- 1
      zscoremin <- zscores
    } else {
      posv[td] <- 0
    } ##  end if
  } ##  end for
  
  posv
  ##  pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
  
} ##  end sim_revert


# Calculate the pnls
sim_revert <- function(pricev, threshz) {
  nrows <- NROW(pricev)
  retp <- rutils::diffit(pricev)
  ##  volv <- sd(retp["T09:30:00/T09:40:00"])
  volv <- sd(retp[1:11])
  ##  pricev <- pricev["T09:40:00/T16:00:00"]
  pricev <- as.numeric(pricev)
  openp <- pricev[1]
  posv <- numeric(nrows)
  zscoremax <- 0
  zscoremin <- 0
  for (td in 11:nrows) {
    ##  cat("td =", td, "\n")
    zscores <- round((pricev[td] - openp)/volv)
    if ((zscores > threshz) & (zscores > zscoremax)) {
      posv[td] <- posv[td-1] - 1
      zscoremax <- zscores
    } else if ((zscores < -threshz) & (zscores < zscoremin)) {
      posv[td] <- posv[td-1] + 1
      zscoremin <- zscores
    } else {
      posv[td] <- posv[td-1]
    } ##  end if
  } ##  end for
  
  cbind(retp, retp*rutils::lagit(posv))
  ##  pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
  
} ##  end sim_revert

load("/Users/jerzy/Develop/data/SPY_minute_2023.RData")
pricev <- pricev[, 1]
pricev <- pricev["T09:30:00/T16:00:00"]
retp <- rutils::diffit(pricev)
endd <- rutils::calc_endpoints(pricev, interval="days")
npts <- NROW(endd)
threshz <- 5

# Calculate the positions
np <- 2
posv <- sim_revert(pricev[(endd[np-1]+1):endd[np], ], threshz)
# Calculate the pnls between the end points
posv <- lapply(2:(npts-1), function(np) {
  sim_revert(pricev[(endd[np-1]+1):endd[np], ], threshz)
})  ##  end lapply
posv <- rutils::do_call(c, posv)
posv <- cumsum(posv)
pnls <- retp*rutils::lagit(posv)
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])


# Calculate the pnls
pnls <- sim_revert(pricev[(endd[np-1]+1):endd[np], ], threshz)
# Calculate the pnls between the end points
pnls <- lapply(2:(npts-1), function(np) {
  sim_revert(pricev[(endd[np-1]+1):endd[np], ], threshz)
})  ##  end lapply
wealthv <- rutils::do_call(rbind, pnls)

# wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

dygraphs::dygraph(cumsum(wealthv),
                  main="Bollinger Ladder Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



##############
### PTS AAPL tick data trading strategy

# Load packages
library(rutils)
library(data.table)

## Load data with AAPL stock features from csv file
raw_ticks <- data.table::fread(file="/Users/jerzy/Develop/predictive/data/aapl20201021.csv", sep="\t")
# tail(raw_ticks)
# class(raw_ticks)
# sapply(raw_ticks, class)
# unlist(sapply(raw_ticks, function(x) if (is.numeric(x)) sum(x))) == 0
# Remove empty columns
raw_ticks <- raw_ticks[, .(timestamp=V10, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 10)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Or more recent data
raw_ticks <- data.table::fread(file="/Users/jerzy/Develop/predictive/data/aapl20201102.csv", sep=",")
raw_ticks <- raw_ticks[, .(timestamp=V8, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 8)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Bind additional pieces of data together
foo <- data.table::fread(file="/Users/jerzy/Develop/predictive/data/aapl20201030.csv", sep="\t")
foo <- foo[, c(1:3, 8)]
colnames(foo) <- c("price", "volume", "seconds", "timestamp")
foo <- foo[, .(timestamp, seconds, price, volume)]
bar <- (last(raw_ticks)$price - first(foo)$price)
foo[, price := (price + bar)]
raw_ticks <- rbind(raw_ticks, foo)



## Apply Hampel filter to remove price jumps

lookb <- 111
half_window <- lookb %/% 2
medianv <- TTR::runMedian(raw_ticks$price, n=lookb)
medianv <- rutils::lagit(medianv, lagg=-half_window, pad_zeros=FALSE)
madv <- TTR::runMAD(raw_ticks$price, n=lookb)
madv <- rutils::lagit(madv, lagg=-half_window, pad_zeros=FALSE)
madv[1:half_window] <- 1
madv[madv == 0] <- 1

zscores <- (raw_ticks$price - medianv)/madv
zscores[is.na(zscores)] <- 0
zscores[!is.finite(zscores)] <- 0
sum(is.na(zscores))
sum(!is.finite(zscores))
range(zscores)
mad(zscores)
foo <- hist(zscores, breaks=1000, xlim=c(-5*mad(zscores), 5*mad(zscores)))

threshz <- 3
tickb <- (abs(zscores) > threshz)
tickg <- raw_ticks[!tickb]
tickg <- raw_ticks[volumv > 10]

## Calculate the a vector of returns
retp <- rutils::diffit(tickg$price)
nrows <- NROW(retp)


## Simple big tick contrarian strategy - trade on next tick after large volume tick

# Trade on large volume and non-zero return
big_ticks <- (tickg$volume >= 2000) & (abs(retp) > 0)
# Or: Trade on large volume and go flat if zero return
# big_ticks <- (tickg$volume >= 100)
posv <- rep(NA_integer_, nrows)
posv[1] <- 0
posv[big_ticks] <- -sign(retp[big_ticks])
posv <- zoo::na.locf(posv)
posv <- rutils::lagit(posv, 3)
pnls <- cumsum(retp*posv)
x11(width=6, height=5)
plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(posv))) / NROW(posv)
# Plot dygraph
datev <- as.POSIXct(tickg$seconds, origin="1970-01-01")
# There are many duplicate dates:
NROW(tickg$seconds) == NROW(unique(tickg$seconds))
# Make dates unique:
datev <- xts::make.index.unique(datev, drop=TRUE)
pnls <- xts::xts(pnls, datev)
# dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, tickg$price)
colv <- c("Strategy", "AAPL")
colnames(pnls) <- colv
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=300)

datav <- cbind(tickg, posv, pnls$Strategy)
data.table::fwrite(datav, file="/Users/jerzy/Develop/predictive/data/aapl_strategy.csv")


## Simple big tick contrarian strategy - trade on large volume ticks only

big_ticks <- raw_ticks[volumv >= 400]
retp <- rutils::diffit(big_ticks$price)
# Flip position or flatten if returns == 0
posv <- (-rutils::lagit(sign(retp)))
pnls <- cumsum(retp*posv)
plot(pnls, t="l")
datav <- cbind(big_ticks, posv, pnls$Strategy)
data.table::fwrite(datav, file="/Users/jerzy/Develop/predictive/data/aapl_strategy.csv")
# Number of trades
sum(abs(rutils::diffit(posv))) / NROW(posv)
# Plot dygraph
datev <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnls <- xts::xts(pnls, datev)
dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, big_ticks$price)
colv <- c("Strategy", "AAPL")
colnames(pnls) <- colv
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=300)


# Or always carry a position - doesn't perform so well
posv <- rep(NA_integer_, NROW(retp))
posv[1] <- 0
posv <- ifelse(retp > 0, -1, posv)
posv <- ifelse(retp < (-1), 1, posv)
posv <- zoo::na.locf(posv)
# posv <- (-rutils::lagit(sign(retp)))
posv <- rutils::lagit(posv)


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

foo <- ohlc[volumv >= 200]
retp <- rutils::diffit(foo$close)
nrows <- NROW(retp)
# foo <- hist(retp, breaks=500, xlim=c(-0.1, 0.1))
posv <- (-rutils::lagit(sign(retp)))
pnls <- cumsum(retp*posv)
plot(pnls, t="l")

foo <- lapply(100*(2:10), function(x) {
  retp <- rutils::diffit(ohlc[volumv >= x]$close)
  posv <- (-rutils::lagit(sign(retp)))
  cumsum(retp*posv)
})  ##  end lapply
sapply(foo, NROW)
sapply(foo, last)


## AR strategy for OHLC - too complicated?

# Calculate the a vector of returns
retp <- rutils::diffit(ohlc$close)
nrows <- NROW(retp)
hist(retp, breaks=500, xlim=c(-0.1, 0.1))

insample <- 1:(nrows %/% 2)
out_of_sample <- (nrows %/% 2 + 1):nrows

dimax <- 5  ##  Define maximum order parameter
lookb <- 5

predv <- rutils::roll_sum(retp, lookb=lookb)
# Shift the response forward into out-of-sample
respv <- rutils::lagit(predv, lagg=(-lookb))
# Define predictor matrix for forecasting
predv <- sapply(1+lookb*(0:dimax), rutils::lagit,
                input=predv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv) <- paste0("pred_", 1:NCOL(predv))
# Calculate the forecasts as function of ordern
fcasts <- lapply(2:NCOL(predv), function(ordern) {
  ##  Calculate the fitted coefficients
  invmat <- MASS::ginv(predv[insample, 1:ordern])
  coeff <- drop(invmat %*% respv[insample])
  ##  Calculate the out-of-sample forecasts of returns
  drop(predv[out_of_sample, 1:ordern] %*% coeff)
})  ##  end lapply
names(fcasts) <- paste0("p=", 2:NCOL(predv))

# Calculate the out-of-sample PnLs
pnls <- sapply(fcasts, function(x) {
  cumsum(sign(x)*retp[out_of_sample])
})  ##  end sapply
colnames(pnls) <- names(fcasts)
pnls <- xts::xts(pnls, index(prices[out_of_sample]))
plot(pnls[, 1], t="l")
plot.zoo(pnls)



## Simple contrarian strategy using Hampel filter - doesn't perform too well

# Calculate the a time series of rolling z-scores
lookb <- 5
# prices <- big_ticks$price
retp <- rutils::diffit(big_ticks$price)
medianv <- TTR::runMedian(retp, n=lookb)
medianv[1:lookb] <- 1
# sum(is.na(medianv))
madv <- TTR::runMAD(retp, n=lookb)
madv[1:lookb] <- 1
madv[madv < 1e-6] <- 1
# sum(is.na(madv))
zscores <- ifelse(madv!=0, (retp-medianv)/madv, 0)
zscores[1:lookb] <- 0
# sum(is.na(zscores))
# madv <- zoo::na.locf(zscores)
# madz <- TTR::runMAD(zscores, n=lookb)
# madz[1:lookb, ] <- 0

tail(zscores)
mad(zscores)
range(zscores)
x11(width=6, height=5)
hist(zscores, breaks=200, xlim=c(-5, 5), freq=FALSE)

# Calculate the positions and pnls from z-scores
posv <- rep(NA_integer_, NROW(retp))
posv[1] <- 0
# threshz <- 3*mad(zscores)
threshz <- 1
posv <- ifelse(zscores > threshz, -1, posv)
posv <- ifelse(zscores < (-threshz), 1, posv)
# posv <- ifelse(zscores > 2*madz, -1, posv)
# posv <- ifelse(zscores < (-2*madz), 1, posv)
posv <- zoo::na.locf(posv)
# Number of trades
# sum(abs(rutils::diffit(posv))) / NROW(posv)
posl <- rutils::lagit(posv, lagg=2)
pnls <- cumsum(posl*retp)

plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(posv))) / NROW(posv)
# Plot dygraph
datev <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnls <- xts::xts(pnls, datev)
dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, big_ticks$price)
colv <- c("Strategy", "AAPL")
colnames(pnls) <- colv
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=300)




##############
### Scale minutely returns by the price range to make them closer to normal or stationary

library(HighFreq)

# symbol <- "SPY"
ohlc <- HighFreq::SPY

## Calculate the price range
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
closep <- quantmod::Cl(ohlc)[endd]
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

## Calculate the moments and perform JB normality tests
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
lambdaf <- 1.5

# Modulus
# Divide returns by square root of volume (volume clock)


## Calculate the moments and perform JB normality test
sum(is.na(retc))
sum(is.infinite(retc))
range(retc)
sapply(1:4, moments::moment, x=retc)
tseries::jarque.bera.test(retc)
x11()
madv <- mad(retc)
hist(retc, breaks=11111, xlim=10*c(-madv, madv), freq=FALSE)
pacf(retc)


## Scale returns using price range
retc <- ifelse(rangev>0, returns/rangev, 0)
# retc <- returns/rangev
retc <- retc/sd(retc)
madv <- mad(retc)
hist(retc, breaks=1111, xlim=c(-3, 3), freq=FALSE)
lines(density(retc, bw=0.2), col='blue', lwd=2)

# JB statistic for different range scaling exponents
statv <- sapply((1:6)/2, function(expov) {
  retc <- ifelse(rangev>0, returns/(rangev^expov), 0)
  retc <- retc/sd(retc)
  tseries::jarque.bera.test(retc)$statistic
})  ##  end sapply


# Stationarity statistic for different scaling exponents
statv <- sapply((1:20)/10, function(expov) {
  ##  retc <- ifelse(volumes>0, returns/(volumes^x), 0)
  retc <- ifelse(rangev>0, returns/(rangev^expov), 0)
  ##  retc <- returns/(rangev^expov)
  ##  Remove zero returns
  ##  retc <- retc[!(retc==0)]
  ##  retc <- retc/sd(retc)
  ##  Standard deviation of square returns is proxy for kurtosis and stationarity
  sd((retc/sd(retc))^2)
})  ##  end sapply




##############
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
}  ##  end for


plot(pnls[c(1, rutils::calc_endpoints(ohlc, interval="minutes"))], t="l", main="Market Making Strategy")



##############
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
lookb <- 111
volume_rolling <- rutils::roll_sum(xtsv=volumes, lookb=lookb)
volumes <- lookb*volumes/volume_rolling

## wippp
## Add to homeworks
## Statistics for different volume scaling exponents.
# Dividing by the square root of the volume works better
# than dividing by the volume itself.
statv <- sapply((1:20)/20, function(expov) {
  retc <- ifelse(volumes > 0, returns/(volumes^expov), 0)
  retc <- retc/sd(retc)
  ##  Calculate the moments and perform JB normality tests
  ##  tseries::jarque.bera.test(retc)$statistic
  ##  moments::moment(retc, order=4)
  ##  Calculate the autocorrelations from PACF
  pacfl <- pacf(as.numeric(retc), lag=10, plot=FALSE)
  sum(pacfl$acf)
  ##  Standard deviation of square returns is proxy for kurtosis and stationarity
  ##  sd(retc^2)
  ##  calc_hurst(retc, endd)
})  ##  end sapply
x11(width=6, height=5)
plot(statv, t="l")


# Divide returns by the volume (volume clock).
retc <- ifelse(volumes > 0, returns/volumes, 0)
retc <- retc/sd(retc)
madv <- mad(retc)

# Plot densities of the returns
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(density(retp), xlim=5*c(-madv, madv),
     lwd=3, mgp=c(2, 1, 0), col="blue",
     xlab="returns (standardized)", ylab="frequency",
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(retc, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
       leg=c("minutely", "scaled", "normal"),
       lwd=6, lty=1, col=c("blue", "red", "green"))

# Plot the cumulative scaled returns
prices_scaled <- cumsum(retc)
prices_scaled <- xts(prices_scaled, index(ohlc))
dygraphs::dygraph(prices_scaled[60*(1:(NROW(prices_scaled) %/% 60))], main="SPY Prices")


# Plot the cumulative scaled returns with close prices
datav <- cbind(prices, prices_scaled)
colv <- c("SPY Prices", "Scaled by Volume")
# datav <- xts::to.hourly(datav)
colnames(datav) <- colv
dygraphs::dygraph(datav[60*(1:(NROW(datav) %/% 60))], main="SPY Prices") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=300)


## Calculate the Hurst

# Define a single aggregation interval.
aggn <- 35^2
agglog <- log(aggn)
# Calculate the index of end points spaced apart by interval.
nrows <- NROW(prices)
nagg <- nrows %/% aggn
endd <- c(0, nrows - nagg*aggn + (0:nagg)*aggn)

# Calculate the Hurst from single data point
calc_hurst <- function(retp, endd) {
  prices <- cumsum(retp)
  r_s <- sapply(2:NROW(endd), function(ep) {
    indeks <- endd[ep-1]:endd[ep]
    diff(range(prices[indeks]))/sd(retp[indeks])
  })  ##  end sapply
  log(mean(r_s))/agglog
}  ##  end calc_hurst

calc_hurst(retp, endd)




##############
# Calculate the price range and daytime returns of intraday ETF pair prices.

# Load the intraday second prices for SPY and aggregate them to minute prices.
dirin <- "/Users/jerzy/Develop/data/raw/"
dirp <- "/Users/jerzy/Develop/data/"
symboln <- "SPY"
datan <- "_second_2025"
filev <- Sys.glob(paste0(dirin, symboln, datan, "*.csv"))
pricel <- lapply(filev, function(filen) {
  cat("file: ", filen, "\n")
  dtable <- data.table::fread(filen)
  datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
  pricev <- xts::xts(dtable[, 2:3], order.by=datev)
  pricev <- pricev["T09:30:00/T16:00:00"]
  colnames(pricev)[1] <- symboln
  pricev
}) ##  end lapply
namev <- as.Date(sapply(pricel, function(x) as.Date(end(x))))
namev <- unname(namev)
names(pricel) <- namev
pricel <- lapply(pricel, xts::to.minutes)
pricel <- lapply(pricel, quantmod::Cl)
for (x in 1:NROW(pricel)) {colnames(pricel[[x]]) <- symboln}
priceref <- pricel

# Repeat the above for XLK
symboln <- "XLK"
pricetarg <- pricel

# Calculate the intraday ETF pair prices.
pricel <- lapply(names(priceref), function(x) {
  2*pricetarg[[x]] - priceref[[x]]
}) ##  end lapply

# Calculate the price range and daytime returns of intraday ETF pair prices.
rangev <- sapply(pricel, function(x) {
  c(range=max(x) - min(x), retd=abs(as.numeric(x[NROW(x), ]) - as.numeric(x[1, ])))
}) ##  end sapply
rangev <- t(rangev)
plot(rangev[, 1], rangev[, 2],
     main="Range vs the Daytime Returns of ETF Pair Prices",
     xlab="Range", ylab="Retd", col="blue")

foo <- rangev[, 1] / rangev[, 2]
plot(foo)
which.max(foo)




##############
# revert_to_open strategy for intraday ETF pair prices

# Compile the C++ code for revert_to_open()
Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")
# Load the list of intraday AAPL prices
symboln <- "AAPL"
filen <- paste0("/Users/jerzy/Develop/data/", symboln, "_second_202402.RData")
load(filen)

# Sample AAPL prices to minutes
pricel <- lapply(pricel, function(pricev) {
  pricev <- pricev[, 1]
  pricev <- quantmod::Cl(xts::to.minutes(pricev))
  colnames(pricev) <- symboln
  pricev
}) ##  end lapply

# Sample AAPL and XLK prices to minutes
pricel <- lapply(seq_along(aapll), function(it) {
  aapl <- aapll[[it]][, 1]
  aapl <- quantmod::Cl(xts::to.minutes(aapl))
  xlk <- xlkl[[it]][, 1]
  xlk <- quantmod::Cl(xts::to.minutes(xlk))
  (aapl - xlk)
}) ##  end lapply



# Run revert_to_open() on the prices
pnls <- lapply(pricel, function(pricev) {
  ##  stratm <- revert_to_open(pricev[, 1])
  stratm <- bollinger_double(pricev[, 1], lambdaf=0.9)
  cbind(rutils::diffit(pricev[, 1]), stratm[, 1])
}) ##  end lapply
pnls <- do.call(rbind, pnls)
pnls <- cumsum(pnls)
colnames(pnls) <- c(symboln, "Strategy")
colv <- colnames(pnls)
dygraphs::dygraph(pnls, main=paste(symboln, "Strategy")) %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)


pnls <- lapply(seq_along(pricel), function(el) {
  pricev <- (pricel[[el]][, 1] - pricaapl[[el]][, 1])
  stratm <- revert_to_open(pricev[, 1])
  cbind(rutils::diffit(pricev[, 1]), stratm[, 1])
}) ##  end lapply
pnls <- do.call(rbind, pnls)
pnls <- cumsum(pnls)



Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/back_test.cpp")
load("/Users/jerzy/Develop/data/SPY_second_202310.RData")
NROW(pricel)
pricev <- pricel[[21]]["T09:30:00/T10:30:00"][, 1]
retv <- rutils::diffit(pricev)
datev <- zoo::index(pricev)

lambdaf <- 0.99
threshz <- 1.0
threshd <- 5.0
threshbad <- 5.0
varin <- 1


# Calculate the price-scores
zscores <- lapply(pricel, function(pricev) {
  ##  cat("file: ", numv, "\n")
  pricev <- pricev["T09:30:00/T16:00:00"][, 1]
  pricema <- rutils::lagit(drop(HighFreq::run_mean(pricev, lambda=lambdaf)))
  pricevar <- rutils::lagit(drop(HighFreq::run_var(pricev, lambda=lambdaf)))
  pricevar <- sqrt(pricevar)
  ifelse(pricevar > 0, (pricev - pricema)/pricevar, 0)
}) ##  end lapply

zscores <- do.call(c, zscores)
zscores <- unname(zscores)
sum(is.na(zscores))
plot(zscores[1e4:2e4], t="l")
range(zscores)

pricev <- lapply(pricel, function(pricev) {
  ##  cat("file: ", numv, "\n")
  pricev["T09:30:00/T16:00:00"][, 1]
}) ##  end lapply
pricev <- do.call(rbind, pricev)
dygraph(pricev)

datav <- cbind(pricev, zscores)
colnames(datav) <- c("price", "zscores")
colv <- colnames(datav)
dygraphs::dygraph(datav, main="Prices and Z-scores") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colv[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colv[2], strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)


ntrades <- 0
pnls <- lapply(pricel, function(pricev) {
  ##  Calculate EWMA prices
  ##  retv[abs(retv) > 0.1] <- 0
  pricev <- pricev[, 1]["T09:30:00/T16:00:00"]
  stratm <- revstrat(pricev, lambdaf, threshz, threshd, threshbad, varin)
  ##  Calculate strategy pnls
  ##  pnls <- posv*retv
  pnls <- stratm[, 1]
  posv <- stratm[, 2]
  ##  cat("posv =", tail(posv), "\n")
  retv <- rutils::diffit(pricev)
  ##  Calculate indicator of flipped positions
  flipi <- rutils::diffit(posv)
  ##  Calculate the number of trades
  ntrades <<- ntrades + sum(abs(flipi) > 0)
  ##  Calculate transaction costs
  costv <- 0.5*bidask*abs(flipi)
  pnls <- (pnls - costv)
  pnls <- cbind(retv, pnls)
  pnls
}) ##  end lapply
pnls <- do.call(rbind, pnls)



stratm <- bollinger_strat(pricev, lambdaf, threshz, varin)
stratm <- xts(stratm, datev)
colnames(stratm) <- c("pnls", "pos", "zscores")

posv <- stratm$pos
dygraph(posv)

pnls <- rutils::lagit(posv)*retv

dygraph(cumsum(stratm$pnls))
dygraph(cumsum(pnls))
dygraph(pricev)
dygraph(stratm$zscores)



##############
## Simulate the ratchet strategy

pnlv <- 0.5*(highp - lowp) - 0.75*abs(closep - openp)
dygraph(cumsum(pnlv))



### Trending Portfolio for VTI and VXX
captiont <- paste("Trending Portfolio for", paste(paste(symbolv, round(c(1, -weightv), 2), sep="="), collapse=", "))

foo <- log(na.omit(rutils::etfenv$prices[, c("VTI", "VXX")]))
foo <- rutils::diffit(foo)

foo <- na.omit(rutils::etfenv$returns[, c("VTI", "VXX")])

betac <- drop(cov(foo[, 2], foo[, 1])/var(foo[, 1]))
betac <- drop(cov(foo["2021/", 2], foo["2021/", 1])/var(foo["2021/", 2]))



endd <- rutils::calc_endpoints(priceref, interval=10)
priceref <- priceref[endd, ]
pricetarg <- pricetarg[endd, ]


## Load the intraday minute prices for SPY and aggregate them to hourly prices
pricev <- lapply(4:6, function(x) {
  load(paste0("/Users/jerzy/Develop/data/SPY_second_20240", x, ".RData"))
  pricel <- lapply(pricel, function(x) {
    quantmod::Cl(to.hourly(x[, 1]))
  }) ##  end lapply
  do.call(rbind, pricel)
}) ##  end lapply
pricev <- do.call(rbind, pricev)
priceref <- pricev
colnames(priceref) <- "SPY"

## Load the intraday minute prices for XLK and aggregate them to hourly prices
pricev <- lapply(4:6, function(x) {
  load(paste0("/Users/jerzy/Develop/data/XLK_second_20240", x, ".RData"))
  pricel <- lapply(pricel, function(x) {
    quantmod::Cl(to.hourly(x[, 1]))
  }) ##  end lapply
  do.call(rbind, pricel)
}) ##  end lapply
pricev <- do.call(rbind, pricev)
pricetarg <- pricev
colnames(pricetarg) <- "XLK"

symboltarg <- rutils::get_name(colnames(pricetarg))
symbolref <- rutils::get_name(colnames(priceref))
pricev <- pricetarg - 0.68*priceref
retv <- rutils::diffit(pricev)


## Simulate the modified Patient Ratchet strategy

pospnls <- ratchet_patient(pricev, lambdaf=0.43, volf=0.1, zfact=8)
flipi <- rutils::diffit(pospnls[, 2])
ntrades <- sum(abs(flipi) > 0)
posv <- pospnls[, 2]
datev <- index(pricev)
posv <- xts(posv, datev)
colnames(posv) <- "posv"
dygraph(posv)
pnls <- pospnls[, 1]
costs <- 0.5*0.05*abs(flipi)
pnls <- (pnls - costs)
pnls <- cbind(retv, pnls)
sharper <- sqrt(252)*sapply(pnls, function(x) mean(x)/sd(x[x<0]))
sharper <- round(sharper, 3)
pnls <- cumsum(pnls)
colnames(pnls) <- c(paste(symbolref, "Returns"), "Strategy")
captiont <- paste("Strategy for", symbolref, "/ \n",
                  paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                  "Number of trades=", ntrades)
dygraphs::dygraph(cbind(pnls[, 2], posv), main=captiont) %>%
  dyAxis("y", label=colv[2], independentTicks=TRUE) %>%
  dyAxis("y2", label="posv", independentTicks=TRUE) %>%
  dySeries(name=colv[2], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name="posv", axis="y2", strokeWidth=1, col="red")


## Calculate the overnight interval - the first time the next day
daton <- xts::.index(pricev)
daton <- rutils::diffit(daton)
daton <- which(daton > 1000)
pricev[daton]
# Calculate the overnight returns - slightly negative
reton <- retv[daton]
dygraph(cumsum(reton))
sum(reton)



# Load the intraday minute prices
pricev <- lapply(4:6, function(x) {
  load(paste0("/Users/jerzy/Develop/data/SPY_second_20240", x, ".RData"))
  pricel <- lapply(pricel, function(x) {
    x <- x[, 1]
    nrows <- NROW(x) %/% 10
    x[10*(1:nrows), ]
  })
  do.call(rbind, pricel)
}) ##  end lapply
pricev <- do.call(rbind, pricev)
priceref <- pricev
colnames(priceref) <- "SPY"

pricev <- lapply(4:6, function(x) {
  load(paste0("/Users/jerzy/Develop/data/XLK_second_20240", x, ".RData"))
  pricel <- lapply(pricel, function(x) {
    x <- x[, 1]
    nrows <- NROW(x) %/% 10
    x[10*(1:nrows), ]
  })
  do.call(rbind, pricel)
}) ##  end lapply
pricev <- do.call(rbind, pricev)
pricetarg <- pricev
colnames(pricetarg) <- "XLK"



##############################
### Momentum strategies
##############################

##############
# btmomrun PCA Momentum Strategies

retcaled <- lapply(retp[insample], function(x) x/sd(x))
retcaled <- do.call(cbind, retcaled)

pcad <- prcomp(retcaled, center=FALSE, scale=FALSE)
pcad <- eigen((t(retcaled) %*% retcaled)/(NROW(retcaled)-1))
sqrt(foo$values[1:7])
pcad$sdev[1:7]
foo$vectors[1:7, 1:7]
pcad$rotation[1:7, 1:7]
all.equal(abs(foo$vectors), abs(pcad$rotation), check.attributes=FALSE)

library(microbenchmark)
summary(microbenchmark(
  prcomp=prcomp(retcaled, center=FALSE, scale=FALSE),
  eigen=eigen((t(retcaled) %*% retcaled)/(NROW(retcaled)-1)),
  times=10))[, c(1, 4, 5)]



lambdas <- seq(from=0.98, to=0.999, by=0.002)
lambdas <- seq(from=0.99, to=0.999, by=0.001)
pnls <- sapply(lambdas, btmomrun, retp=retpca[, 1:dimax])

lambdas <- seq(from=0.6, to=0.9, by=0.1)
pnls <- sapply(lambdas, btmomrun, trend=(-1), retp=retpca[, (dimax+1):NCOL(retpca)])

pnls <- apply(pnls, MARGIN=2, function(pnl) indeksd*pnl/sd(pnl))
colnames(pnls) <- paste0("lambda=", lambdas)
pnls <- xts::xts(pnls, datev)
tail(pnls)

colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endd], main="Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)




##############
### Backtest momentum strategy

# Load packages
library(rutils)

load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
returns100[1, ] <- 0
returns100 <- zoo::na.locf(retv100, na.rm=FALSE)
indeks <- cumsum(rowMeans(retv100))

lagg <- 5
sum_roll <- rutils::roll_sum(retv100, lookb=lagg)
weightv <- matrixStats::rowRanks(sum_roll)
weightv <- (weights - 50)
weightv <- rutils::lagit(weightv, lagg=1)

wealth <- -weights*retp100
wealth <- -weights*rutils::lagit(sum_roll, lagg=(-lagg))
wealth <- cumsum(rowMeans(wealth))
x11()
plot(wealth, t="l")
wealth <- xts::xts(wealth, index(retv100))
dygraphs::dygraph(wealth)

# Combine index with AAPL
wealth <- cbind(wealth, indeks)
# wealth <- xts(wealth, index(retv100))
colv <- c("Strategy", "Index")
colnames(wealth) <- colv
dygraphs::dygraph(wealth, main="S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=300)




# Define backtest functional for daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
btmomdaily <- function(retp, lookb=252, bidask=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  ##  Calculate the rolling variance
  varv <- roll::roll_var(retp, width=lookb)
  varv <- zoo::na.locf(varv, na.rm=FALSE)
  ##  varv[is.na(varv)] <- 1
  varv[varv <= 0] <- 1
  ##  Calculate the rolling Sharpe
  past <- roll::roll_mean(retp, width=lookb)
  past[1:lookb, ] <- 1
  weightv <- past/sqrt(varv)
  ##  weightv <- ifelse(varv > 0, past/sqrt(varv), 0)
  ##  weightv[varv == 0] <- 0
  weightv[1:lookb, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0
  weightv <- rutils::lagit(weightv, 2)
  ##  Calculate the momentum profits and losses
  future <- rutils::lagit(past, (-lookb))
  pnls <- tre_nd*rowMeans(weights*future)
  ##  Calculate the transaction costs
  costs <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
  cumsum(pnls - costs)
}  ##  end btmomdaily


wealth <- btmomdaily(retp=retv100, lookb=5, bidask=0, tre_nd=(-1))

# Combine index with AAPL
wealth <- cbind(wealth, indeks)
wealth <- xts(wealth, index(retv100))
colv <- c("Strategy", "Index")
colnames(wealth) <- colv
dygraphs::dygraph(wealth, main="Momentum S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=300)



##############
### Simulate trading strategy with rolling Sharpe ratio

# Define backtest functional
backtest_rolling <- function(retp, lookb=252, bidask=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  ##  Calculate the rolling variance
  varv <- HighFreq::roll_var(retv_weighted, lookb=lookb)
  varv <- zoo::na.locf(varv, na.rm=FALSE)
  varv[is.na(varv)] <- 0
  ##  Calculate the rolling Sharpe
  past <- roll::roll_mean(retp, width=lookb)
  weightv <- past/sqrt(varv)
  weightv[varv == 0] <- 0
  weightv[1:lookb, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0
  weightv <- rutils::lagit(weightv)
  ##  Calculate the momentum profits and losses
  pnls <- tre_nd*rowMeans(weights*retp)
  ##  Calculate the transaction costs
  costs <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
  cumprod(1 + pnls - costs)
}  ##  end backtest_rolling


# Define backtest functional
backtest_weighted <- function(retp, returns_weighted,
                              lookb=252, bidask=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  ##  Calculate the rolling variance
  varv <- HighFreq::roll_var(retv_weighted, lookb=lookb)
  varv <- zoo::na.locf(varv, na.rm=FALSE)
  varv[is.na(varv)] <- 0
  ##  Calculate the rolling Sharpe
  past <- roll::roll_mean(retv_weighted, width=lookb)
  weightv <- past/sqrt(varv)
  weightv[varv == 0] <- 0
  weightv[1:lookb, ] <- 1
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0
  weightv <- rutils::lagit(weightv)
  ##  Calculate the momentum profits and losses
  pnls <- tre_nd*rowMeans(weights*retp)
  ##  Calculate the transaction costs
  costs <- 0.5*bidask*rowSums(abs(rutils::diffit(weightv)))
  cumprod(1 + pnls - costs)
}  ##  end backtest_weighted


# Define backtest functional
backtestmomentum <- function(retp, returns_weighted,
                             objfunc=function(retp) (sum(retp)/sd(retp)),
                             lookb=12, rebalf="months", bidask=0.001,
                             endd=rutils::calc_endpoints(retp, interval=rebalf),
                             with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  ##  Define look-back and look-forward intervals
  nrows <- NROW(endd)
  startp <- c(rep_len(1, lookb-1), endd[1:(nrows-lookb+1)])
  ##  Calculate the look-back intervals
  lookbs <- cbind(startp, endd)
  ##  Calculate the look-forward intervals
  look_fwds <- cbind(endd + 1, rutils::lagit(endd, -1))
  look_fwds[nrows, 1] <- endd[nrows]
  ##  Calculate the past performance over look-back intervals
  past <- t(apply(lookbs, 1, function(ep) sapply(retv_weighted[ep[1]:ep[2]], objfunc)))
  past[is.na(past)] <- 0
  ##  Calculate the future performance
  future <- t(apply(look_fwds, 1, function(ep) sapply(retp[ep[1]:ep[2]], sum)))
  future[is.na(future)] <- 0
  ##  Scale weights so sum of squares is equal to 1
  weightv <- past
  weightv <- weightv/sqrt(rowSums(weightv^2))
  weightv[is.na(weightv)] <- 0  ##  Set NA values to zero
  ##  Calculate the momentum profits and losses
  pnls <- rowSums(weights*future)
  ##  Calculate the transaction costs
  costs <- 0.5*bidask*cumprod(1 + pnls)*rowSums(abs(rutils::diffit(weightv)))
  pnls <- (pnls - costs)
  if (with_weightv)
    rutils::lagit(cbind(pnls, weightv))
  else
    rutils::lagit(pnls)
}  ##  end backtestmomentum


# Perform sapply loop over lookbs
endd <- rutils::calc_endpoints(retp, interval="weeks")
lookbs <- seq(3, 15, by=1)
objfunc <- function(retp) sum(retp)/sd(retp)
profilevs <- sapply(lookbs, function(lookb) {
  pnls <- backtestmomentum(retp=retp, returns_weighted=retv_weighted,
                           endd=endd,
                           lookb=lookb, objfunc=objfunc)
  last(cumprod(1 + pnls))
})  ##  end sapply
x11(width=6, height=4)
plot(x=lookbs, y=profilevs, t="l",
     main="Strategy PnL as function of lookb",
     xlab="lookb (months)", ylab="pnl")

datev <- index(retp[endd])
weightsaw <- c(0.30, 0.55, 0.15)
retsaw <- returns %*% weightvaw
wealthaw <- cumprod(1 + retsaw)
wealthaw <- xts::xts(wealthaw[endd], datev)

lookb <- lookbs[which.max(profilevs)]
pnls <- backtestmomentum(retp=retp, returns_weighted=retv_weighted,
                         lookb=lookb, endd=endd,
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
# Perform sapply loop over lookbs
wealth <- sapply(lookbs, backtestmomentum,
                 retp=retp,
                 returns_weighted=retv_weighted,
                 endd=endd,
                 objfunc=objfunc)
wealth <- apply(wealth, 2, function(x) cumprod(1 + x))
colnames(wealth) <- paste0("lookb=", lookbs)
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




##############################
### Portfolio strategies
##############################

##############
## Find the optimal Hurst portfolio of AAPL and XLK

# Calculate the Hurst exponent from returns
calc_hurst_rets <- function(retp, lagg) {
  retp <- na.omit(retp)
  endp <- rutils::calc_endpoints(retp, interval=lagg)
  retc <- cumsum(retp)
  ranger <- sapply(seq_along(endp)[-1], function(it) {
    startp <- endp[it-1]
    endp <- endp[it]
    retp <- retp[startp:endp]
    retc <- retc[startp:endp]
    log((max(retc) - min(retc))/sd(retp))/log(endp-startp)
  })  ##  end sapply
  median(na.omit(ranger))
}  ##  end calc_hurst_rets

objfun <- function(weightv, retp=retp) {
  ##  weightv <- c(1, weightv)
  ##  weightv <- weightv/sqrt(sum(weightv^2))
  retp <- retp %*% weightv
  -calc_hurst_rets(retp, lagg)
}  ##  end objfun

calc_hurst_rets(retp %*% weightv, lagg)



# Extract the closing prices
symbolv <- c("AAPL", "XLK")
pricev <- cbind(pricestock$AAPL, quantmod::Cl(rutils::etfenv$XLK))

symbolv <- c("VTI", "VXX")
pricev <- rutils::etfenv$prices[, symbolv]

pricev <- na.omit(pricev)
colnames(pricev) <- symbolv
retp <- rutils::diffit(log(pricev))

weightl <- ((-10):10 + 0.1)
hurstm <- sapply(weightl, function(w1) {
  sapply(weightl, function(w2) {
    calc_hurst_rets(retp %*% c(w2, w1), lagg)
  })  ##  end sapply
})  ##  end sapply
rownames(hurstm) <- weightl
colnames(hurstm) <- weightl
indeks <- which(hurstm == min(hurstm, na.rm=TRUE), arr.ind=TRUE)
weightv <- weightl[indeks[1, ]]

calc_hurst_rets(retp %*% weightv, lagg)



## Find the portfolio with the smallest aggregated variance

# Calculate the aggregated variance
calc_agg_var <- function(pricev, lagg) {
  endp <- rutils::calc_endpoints(pricev, interval=lagg)
  retp <- rutils::diffit(pricev[endp, ])
  var(retp)
}  ##  end calc_agg_var

# Objective function equal to the aggregated variance
objfun <- function(weightv, pricev=pricev) {
  weightv <- weightv/sqrt(sum(weightv^2))
  pricep <- pricev %*% weightv
  retp <- rutils::diffit(pricep[endp, ])
  var(retp)
}  ##  end objfun

symbolv <- c("VTI", "VXX")
pricev <- log(na.omit(rutils::etfenv$prices[, symbolv]))
datev <- index(pricev)
endp <- rutils::calc_endpoints(pricev, interval=7)
objfun(c(1, 1), pricev=pricev)

# ##  Perform portfolio optimization using the quasi-Newton method
ncols <- NCOL(pricev)
optimd <- optim(par=rep(1, ncols),
                fn=objfun,
                pricev=pricev,
                method="L-BFGS-B",
                upper=rep(100, ncols),
                lower=rep(-100, ncols))

# Portfolio weights
weightv <- optimd$par
names(weightv) <- symbolv
pricep <- pricev %*% weightv
pricep <- xts(pricep, datev)
dygraph(pricep)
retp <- rutils::diffit(pricep)

pacfl <- pacf(retp, lag=10)
pacsum <- sum(pacfl$acf)

retport <- xts(retport, index(retp))
captiont <- paste("Trending Portfolio for", paste(symbolv, collapse=", "), " / PACSum=", round(pacsum, 3))
dygraph(cumsum(retport), main=captiont)



## Find the most trending portfolio

symbolv <- c("VTI", "SVXY", "VXX")
pricev <- log(na.omit(rutils::etfenv$prices[, symbolv]))
datev <- index(pricev)
ncols <- NCOL(retp)
retp <- rutils::diffit(pricev)
retm <- rutils::lagit(retp$VTI, lagg=-1)

# Objective function equal to the sum of the PACF
objfun <- function(weightv, retp=retp) {
  retp <- retp %*% weightv
  pacfl <- pacf(retp, lag=10, plot=FALSE)
  -sum(pacfl$acf)
}  ##  end objfun

# Objective function equal to the sum of PnLs
objfun <- function(weightv, retp=retp) {
  retp <- retp %*% weightv
  retp <- retp/sd(retp)
  -sum(sign(retm)*sign(retp))
}  ##  end objfun

# Objective function equal to the sum of PnLs
objfun <- function(weightv, retp=retp) {
  retp <- (retp - weightv[1]*retsvxy - weightv[2]*retvxx)
  -sum(retp)/sd(retp)
}  ##  end objfun

# ##  Perform portfolio optimization using the quasi-Newton method
ncols <- NCOL(retp)
optimd <- optim(par=rep(1, ncols),
                fn=objfun,
                retp=retp,
                method="L-BFGS-B",
                upper=rep(100, ncols),
                lower=rep(-100, ncols))

# Portfolio weights
weightv <- optimd$par
names(weightv) <- symbolv
retport <- retp %*% weightv
pacfl <- pacf(retport, lag=10)
pacsum <- sum(pacfl$acf)
retport <- xts(retport, index(retp))
captiont <- paste("Trending Portfolio for", paste(paste(symbolv, round(-weightv, 2), sep="="), collapse=", "), " / PACSum=", round(pacsum, 3))
# captiont <- paste("Trending Portfolio for", paste(symbolv, collapse=", "), " / PACSum=", round(pacsum, 3))
dygraph(-cumsum(retport), main=captiont)

foo <- sign(retp$VTI)*sign(rutils::lagit(retport, lagg=1))
dygraph(cumsum(foo))


## Find the optimal portfolio of VTI, VXX, and SVXY

symbolv <- c("VTI", "VXX", "SVXY")
# retp <- rutils::etfenv$returns["2018-04/", symbolv]
retp <- rutils::etfenv$returns[, symbolv]
retp <- na.omit(retp)
retp <- zoo::na.locf(retp, na.rm=FALSE)

## Find the cointegrated portfolio of VTI, VXX, and SVXY

## Find the optimal portfolio of VTI, VXX, and SVXY

# weightv <- rep(1, 3)
objfun <- function(weightv, retp=retp) {
  ##  weightv <- c(1, weightv)
  weightv <- weightv/sqrt(sum(weightv^2))
  retp <- retp %*% weightv
  -sum(retp)/sd(retp)
}  ##  end objfun

# ##  Perform portfolio optimization using the quasi-Newton method
ncols <- NCOL(retp)
optimd <- optim(par=rep(1, ncols),
                fn=objfun,
                retp=retp,
                method="L-BFGS-B",
                upper=rep(100, ncols),
                lower=rep(-100, ncols))

# Portfolio weights
weightv <- optimd$par
weightv <- weightv/sqrt(sum(weightv^2))
# weightv <- c(1, optimd$par)
names(weightv) <- symbolv

pnls <- retp %*% weightv
pnls <- xts(pnls, index(retp))
dygraph(cumsum(pnls))
endd <- rutils::calc_endpoints(retp["2011/"], interval="weeks")
plot(cumsum(pnls)[endd], t="l")
retstrat <- (sign(rutils::lagit(bar, lagg=1))*bar)
plot(cumsum(retstrat)[endd], t="l")



filec <- "/Users/jerzy/Develop/data/VXX_second_202308.RData"
load(filec)





##############
# online PCA

# Setup data
ncols <- 9
nrows <- 1e3
# Simulate random uncorrelated returns
# retp <- matrix(rnorm(nrows*ncols), nrows, ncols)
# retp <- retp %*% diag(sqrt(1:ncols))

# Calculate the random covariance matrix
covmat <- matrix(runif(ncols^2), nc=ncols)
covmat <- t(covmat) %*% covmat
# Calculate the Cholesky matrix
cholmat <- chol(covmat)
# Simulate random uncorrelated returns
retp <- matrix(rnorm(ncols*nrows), nc=ncols)
# Calculate the correlated returns by applying Cholesky
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
  ##  pcav <- onlinePCA::ghapca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], gamma=0.9, center=meanv)
  ##  pcav <- sgapca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], gamma=0.9, center=meanv)
  ##  pcav <- onlinePCA::sgapca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], gamma=0.9, center=meanv)
  ##  pcav <- onlinePCA::ccipca(lambda=pcav$values, U=pcav$vectors, x=retp[i, ], n=(i-1), l=2, center=meanv)
}  ##  end for

# Compare PCA methods
pcaf$sdev^2
drop(pcav$values)


#########
# Select the stocks with the smallest betas and largest alphas 
# in-sample, and calculate their compounded wealth out-of-sample.

# In-sample alpha and beta
retmis <- retm["/2010"]
varm <- var(retmis)
retmm <- mean(retmis)

regd <- sapply(retp["/2010"], function(rets) {
  betav <- cov(rets, retmis)/varm
  alphav <- mean(rets) - betav*retmm
  c(alpha=alphav, beta=betav)
})  ##  end sapply
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
foo <- foo*sd(retm["2011/", ])/sd(foo)
# dygraphs::dygraph(foo)

# Calculate the compounded wealth from returns
wealthv <- cbind(retm["2011/", ], foo)
colnames(wealthv) <- c("VTI", "Low beta")
sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))

# Plot compounded wealth
endd <- rutils::calc_endpoints(wealthv, interval="months")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Low Beta Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)




##############
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
  ##  weightv <- weightv/sqrt(sum(weightv^2))
  weightv <- weightv/sum(weightv)
  retp <- retp["/2010"] %*% weightv
  retsl <- retsl["/2010"] %*% weightv
  ##  -sum(retp*sign(retsl))/sum(retp^2)
  -sum(retp*sign(retsl))
}  ##  end objfun


# Perform portfolio optimization using DEoptim
optimd <- DEoptim::DEoptim(fn=objfun,
                           retsl=retsl, retp=retp,
                           upper=rep(100, nstocks),
                           lower=rep(-100, nstocks),
                           control=list(trace=FALSE, itermax=1e3, parallelType=1,
                                        packages=c("dygraphs", "TTR", "xts", "quantmod", "rutils", "HighFreq")))
weightv <- optimd$optim$bestmem/sum(abs(optimd$optim$bestmem))

# ##  Perform portfolio optimization using the quasi-Newton method
optimd <- optim(fn=objfun,
                par=rep(1, nstocks),
                method="L-BFGS-B",
                upper=rep(100, nstocks),
                lower=rep(-100, nstocks))

# Portfolio weights
weightv <- optimd$par
names(weightv) <- symbolv
sort(weightv)

pnls <- retp["2011/"] %*% weightv
endd <- rutils::calc_endpoints(retp["2011/"], interval="weeks")
plot(cumsum(pnls)[endd], t="l")
retstrat <- (sign(rutils::lagit(bar, lagg=1))*bar)
plot(cumsum(retstrat)[endd], t="l")



# Normalize the prices so that they start at 1.
pricesn <- lapply(prices, function(x) x/as.numeric(x[1]))
pricesn <- rutils::do_call(cbind, pricesn)
head(pricesn[, 1:5])

arimav <- arima.sim(n=1e3, regmod=list(ar=c(0.2, 0.2)))


arimav <- arima.sim(n=1e3, regmod=list(ar=c(0.2, 0.2)))

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


sim_momw <- function(retp,
                        objfun=function(retp) (prod(1+retp)/sd(retp)),
                        lookb=12, rfreq="months", bidask=0.001,
                        endd=rutils::calc_endpoints(retp, interval=rfreq), ...) {
  ##  Perform loop over end points
  npts <- NROW(endd)
  pnls <- lapply(1:(npts-1), function(it) {
    ##  Select the look-back returns
    startp <- endd[max(1, it-lookb+1)]
    retslb <- retp[startp:endd[it], ]
    ##  Calculate the weights proportional to performance
    perfstat <- sapply(retslb, objfun)
    perfstat[!is.finite(perfstat)] <- 0
    weightv <- perfstat
    ##  Scale weights so sum of squares is equal to 1
    weightv <- (weightv - mean(weightv))
    weightv <- weightv/sqrt(sum(weightv^2))
    weightv[!is.finite(weightv)] <- 0
    ##  Calculate the out-of-sample portfolio returns
    retsos <- retp[(endd[it]+1):endd[it+1], ] %*% weightv
    retsos
  })  ##  end lapply
  pnls <- rutils::do_call(c, pnls)
  pnls
}  ##  end sim_momw


##############
## Cointegration

symboltarg <- "XLE"
symbolref <- "XLB"
ohlc <- get(symboltarg, rutils::etfenv)
closep <- log(quantmod::Cl(ohlc))
ohlc <- get(symbolref, rutils::etfenv)
closetf <- log(quantmod::Cl(ohlc))
prices <- na.omit(cbind(closep, closetf))
colnames(prices) <- c(symboltarg, symbolref)
closep <- prices[, 1]
closetf <- prices[, 2]

# Calculate the regression coefficients of XLB ~ XLE
betav <- drop(cov(closep, closetf)/var(closetf))
alpha <- drop(mean(closep) - betav*mean(closetf))


betas <- (1:40)/10

foo <- sapply(betas, function(betav) {
  alpha <- drop(mean(closep) - betav*mean(closetf))
  residuals <- (closep - alpha - betav*closetf)
  colnames(residuals) <- paste0(symboltarg, " vs ", symbolref)
  adftest <- tseries::adf.test(residuals, k=3)
  adftest$p.value
})

dev.new(width=6, height=4, noRStudioGD=TRUE)
plot(x=betas, y=foo, t="l")

dygraphs::dygraph(residuals)



##############
### Portfolio optimization of VTI, VXX, and SVXY

# Select ETFs
symbolv <- c("VTI", "VXX", "SVXY")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)
prices <- na.omit(rutils::etfenv$prices[, symbolv])

# dygraph plot of VXX prices versus SVXY
dygraphs::dygraph(prices["2018-01-01/", 2:3], main="Returns of VXX and SVXY") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# dygraph plot of VXX returns versus SVXY
dygraphs::dygraph(cumsum(cbind(retp["2018-01-01/", 2], -2*retp["2018-01-01/", 3])),
                  main="Returns of VXX and SVXY") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# In-sample returns
insample <- rbind(retp["/2018-02-01"], retp["2018-02-07/"])
insample <- retp["2018-03-01/"]

# Portfolio objective function
objfun <- function(x) {
  weightv <- c(1, x[1], x[2])
  pnls <- (insample %*% weightv)
  -mean(pnls)/sd(pnls[pnls < 0])
}  ##  end objfun

## Calculate the in-sample weights
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

# Calculate the out-of-sample pnls
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
dimax <- 2
predv <- roll::roll_mean(retp, width=numagg, min_obs=1)
predv <- lapply(1:dimax, rutils::lagit, input=predv)
predv <- do.call(cbind, predv)
# colnames(predv) <- paste0("pred_", 1:NCOL(predv))
nrows <- NROW(predv)
predv <- cbind(rep(1, nrows), predv)
colnames(predv)[1] <- "intercept"
regmod <- lm(respv ~ predv - 1)
regsum <- summary(regmod)


# In-Sample
dimax <- 6
# invmat <- MASS::ginv(predv)
invmat <- HighFreq::calc_inv(predv, dimax=dimax)
coeff <- drop(invmat %*% respv)
fcasts <- (predv %*% coeff)
pnls <- sign(fcasts)*retp[, "VTI"]
wealth <- cbind(retp[, "VTI"], pnls)
colnames(wealth) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealth, function(x) mean(x)/sd(x[x<0]))
colv <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="VIX Strategy In-Sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# Out-of-Sample
insample <- (datev < as.Date("2018-01-01"))
outsample <- (datev >= as.Date("2018-01-01"))
# invmat <- MASS::ginv(predv[insample, ])
dimax <- 4
invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
coeff <- drop(invmat %*% respv[insample, ])
fcasts <- (predv[outsample, ] %*% coeff)
pnls <- sign(fcasts)*retp[outsample, "VTI"]
wealth <- cbind(retp[outsample, "VTI"], pnls)
colnames(wealth) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealth, function(x) mean(x)/sd(x[x<0]))
colv <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="VIX Strategy Out-of-Sample") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colv[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=300)


dimaxs <- 2:7
pnls <- lapply(dimaxs, function(dimax) {
  cat("dimax =", dimax, "\n")
  invmat <- HighFreq::calc_inv(predv[insample, ], dimax=dimax)
  coeff <- drop(invmat %*% respv[insample, ])
  fcasts <- (predv[outsample, ] %*% coeff)
  sign(fcasts)*respv[outsample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", dimax)
colnames(pnls) <- paste0("eigen", dimaxs)
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=300)




##############
### Create trending portfolios of similar ETFs.

# Load packages
library(rutils)

# Calculate the ETF returns and volumes
# VXX with SVXY

# Calculate the ETF returns and volumes
symbolv <- c("VTI", "SVXY", "VXX")
retp <- na.omit(rutils::etfenv$returns[, symbolv])
datev <- zoo::index(retp)

lookb <- 11
# Scale the volume by the rolling average volume
retc <- lapply(symbolv, function(symbol) {
  ohlc <- get(symbol, rutils::etfenv)
  retp <- rutils::diffit(log(quantmod::Cl(ohlc)))
  volumes <- quantmod::Vo(ohlc)
  volume_rolling <- roll::roll_mean(volumes, width=lookb)
  volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
  volumes <- volumes/volume_rolling
  ##  Divide  the returns by the volume - use trading time (volume clock)
  returns/volumes
})  ##  end lapply

# Scale by the High minus Low range
retc <- lapply(symbolv, function(symbol) {
  ohlc <- log(get(symbol, rutils::etfenv))
  retp <- rutils::diffit(quantmod::Cl(ohlc))
  rangev <- (quantmod::Hi(ohlc) - quantmod::Lo(ohlc))
  returns/rangev
})  ##  end lapply

# Scale by the rolling volatility
retc <- lapply(symbolv, function(symbol) {
  ohlc <- log(get(symbol, rutils::etfenv))
  retp <- rutils::diffit(quantmod::Cl(ohlc))
  varv <- HighFreq::roll_var_ohlc(ohlc=ohlc, lookb=lookb, scalit=FALSE)
  colnames(varv) <- "variance"
  volv <- sqrt(varv)
  returns/volv
})  ##  end lapply

retc <- do.call(cbind, retc)
colnames(retc) <- do.call(rbind,(strsplit(colnames(retc), split="[.]")))[, 1]
retc <- na.omit(retc)
retc <- retc[datev]

weightv <- 1/sapply(retc, sd)/100
pnls <- retc %*% weightv

# PACF of AR(1) process
x11(width=6, height=5)
pacfl <- pacf(pnls, lag=10, xlab="", ylab="", main="")
abs(sum(pacfl$acf))

sum_pacf <- function(weightv) {
  pnls <- retc %*% weightv
  pacfl <- pacf(pnls, lag=10, plot=FALSE)
  -sum(pacfl$acf) - sum(pnls) + (1-sum(weightv^2))^2
}  ##  end sum_pacf


# Calculate the end points
npts <- 21
nrows <- NROW(retc)
nagg <- nrows %/% npts
endd <- c(0, nrows - nagg*npts + (0:nagg)*npts)

# Calculate the Hurst
hurstfun <- function(weightv) {
  pnls <- retc %*% weightv
  prices <- cumsum(pnls)
  rrange <- sapply(2:NROW(endd), function(ep) {
    indeks <- endd[ep-1]:endd[ep]
    diff(range(prices[indeks]))/sd(pnls[indeks])
  })  ##  end sapply
  ##  Calculate the Hurst from single data point
  -log(mean(rrange))/log(npts) - sum(pnls) + (1-sum(weightv^2))^2
}  ##  end sum_pacf

optimd <- optim(par=c(0.1, 0.1, 0.1),
                fn=hurstfun,
                method="L-BFGS-B",
                upper=c(10, 10, 10),
                lower=c(0, 0, 0))
# Optimal parameters and value
weightv <- optimd$par
pnls <- retc %*% weightv
pacf(pnls, lag=10, xlab="", ylab="", main="")
pnls <- xts::xts(pnls, datev)

datav <- cbind(returns$VTI, pnls)
sharper <- sqrt(252)*sapply(datav, function(x) mean(x)/sd(x[x<0]))

colnames(pnls) <- c(paste(symbol, "Returns"), "Strategy", "Buy", "Sell")

captiont <- paste("Strategy for", symbol, "Returns Scaled by the Trading Volumes / \n",
                  paste0(c("Index SR=", "Strategy SR="), sharper, collapse=" / "), "/ \n",
                  "Number of trades=", ntrades)

colv <- colnames(datav)
dygraphs::dygraph(cumsum(datav), main="Autoregressive Portfolio") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red")

dygraphs::dygraph(datav, main="Autoregressive Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)



##############
## Calculate the alphas of S&P500 stocks using the Wilcoxon signed-rank test.

# Compile Rcpp functions
Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/test_arma.cpp")


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
})  ##  end sapply




##############
### Portfolio optimization using quadratic solver

load("/Volumes/external/Develop/data/etf_data.RData")
ls(etfenv)
dim(etfenv$returns)
colnames(etfenv$returns)



## perform standard calibration over ohlc interval
optimd <- optim(par=rep(0.5, 2*NCOL(predm)),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=rep(2, 2*NCOL(predm)),
                lower=rep(-2, 2*NCOL(predm)),
                predm=predm[datev],
                retp=retp[datev],
                lambdaf=lambdaf)

betas <- optimd$par
names(betas) <- c(paste0(colnames(predm), "_long"), paste0(colnames(predm), "_short"))


## cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(betas, lagg=15, predm=predm, retp=retp, lambdaf=0) {
  ncols <- NCOL(predm)
  posv <- rep.int(NA, NROW(predm))
  posv[1] <- 0
  ##  buy signal
  buysig <- (predm %*% betas[1:ncols] < -1)
  posv[buysig] <- 1.0
  sellsig <- as.logical(rutils::lagit(buysig, lag=lagg))
  ##  Sell signal
  posv[sellsig] <- -1.0
  posv[buysig] <- 1.0
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- c(0, posv[-NROW(posv)])
  ##  pnls <- posv*retp
  ##  betav <- (sum(pnls*retp) - sum(pnls)*sum(retp)) / (sum(pnls*pnls) - sum(pnls)^2 )
  ##  -(exp(sum(pnls) - betav*sum(retp)) - 1)
  ##  -(exp(sum(posv*retp))-1) ##  / (sum(abs(rutils::diffit(posv))) / 2/ 1e5) / abs(sum(posv>0) - sum(posv<0))
  -((exp(sum(posv*retp))-1) - lambdaf*sum(abs(betas)))
}  ##  end cum_pnl

cum_pnl(betas=betas, predm=predm[datev], retp=retp[datev])

# perform calibration over ohlc interval
optimd <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, NCOL(predm)),
                           lower=rep(-2, NCOL(predm)),
                           predm=predm[datev],
                           retp=retp[datev],
                           lambdaf=lambdaf,
                           control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))


betas <- optimd$optim$bestmem
names(betas) <- colnames(predm)
# names(betas) <- colnames(predm)
optimd$optim$bestval
cum_pnl(betas, predm=predm[datev])


buysig <- (predm %*% betas[1:ncols] < -1)

cum_pnl <- function(lagg) {
  posv <- rep.int(NA, NROW(predm))
  posv[1] <- 0
  posv[buysig] <- 1.0
  sellsig <- as.logical(rutils::lagit(buysig, lag=lagg))
  posv[sellsig] <- -1.0
  posv[buysig] <- 1.0
  posv <- zoo::na.locf(posv, na.rm=FALSE)
  posv <- c(0, posv[-NROW(posv)])
  exp(sum((posv*retp_running)))-1
}  ##  end cum_pnl

cum_pnl(200)
sapply(20*(1:30), cum_pnl)



##############
### Convert correlation matrix into distance object
distv <- xts::.index(volvpikes)
distv <- abs(outer(X=distv, Y=distv, FUN="-"))
# distv <- rutils::diffit(xts::.index(volvpikes))
distv <- as.dist(distv)
# Perform hierarchical clustering analysis
cluster <- hclust(distv)
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

retf <- rutils::roll_sum(retp, lookb=5)
retf <- rutils::lagxts(retp, lag=-5)
colnames(retf) <- "retf"
foo <- lm(retf["2008"] ~ predm["2008"] - 1)
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



##############################
### Miscellaneous strategies
##############################


##############
# Jolyon strategy PnLs

library(rutils)
pnlj <- zoo::read.zoo("/Users/jerzy/Develop/trading/jolyon/JMSPY2.csv", header=TRUE, sep=",", FUN = as.Date)
pnlj <- xts::as.xts(pnlj)
colnames(pnlj) <- c("PnLs")
dygraphs::dygraph(cumsum(pnlj), main="JM Strategy PnLs") %>%
  dyOptions(colors=c("blue"), strokeWidth=2) %>%
  dyLegend(show="never", width=300)

retp <- etfenv$returns$SPY[index(pnlj)]
wealthv <- cbind(retp, pnlj)
colnames(wealthv) <- c("SPY", "PnLs")
colv <- colnames(wealthv)
dygraphs::dygraph(cumsum(wealthv), main="JM Strategy PnLs") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

pnlj <- pnlj*sd(retp)/sd(pnlj)
cor(retp, pnlj)
wealthv <- cbind(retp, pnlj, 0.5*(retp + pnlj))
colnames(wealthv) <- c("SPY", "Strategy", "Combined")
colv <- colnames(wealthv)
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x))
sharper <- round(sharper, 3)
captiont <- paste0(paste(colv, sharper, sep=" = "), collapse=" / ")
captiont <- paste0("Sharpe Ratios: ", captiont)
dygraph(cumsum(wealthv), main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



##############
# Daily SPY AR strategy for Jolyon's data
# Daily log returns
retp <- na.omit(etfenv$returns$SPY["2020/"])
# Predictor matrix
orderp <- 5
respv <- retp
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0(symboln, 1:orderp)
rangev <- paste0("/", start(pnlj))
# Calculate the fitted AR coefficients
predinv <- MASS::ginv(predm[rangev])
coeff <- 1e2*drop(predinv %*% respv[rangev])
names(coeff) <- colnames(predm)
# Calculate the in-sample forecasts of SPY (fitted values)
fcasts <- predm %*% coeff
# Scale the forecasts
# scalev <- HighFreq::run_mean(abs(fcasts), lambda=0.999)
# fcasts <- ifelse(scalev > 1e-4, fcasts/scalev, 0)
# Calculate the AR strategy PnLs
pnls <- retp*fcasts
colnames(pnls) <- c("PnLAR")
pnls <- pnls[index(pnlj)]
# Scale the PnL volatility to that of SPY
pnls <- pnls*sd(retp[index(pnlj)])/sd(pnls)

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(pnlj, pnls)
colnames(wealthv) <- c("Strategy", "AR")
cor(wealthv)
# Plot dygraph of the AR strategy
colv <- colnames(wealthv)
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x))
sharper <- round(sharper, 3)
captiont <- paste0(paste(colv, sharper, sep=" = "), collapse=" / ")
captiont <- paste0("Sharpe Ratios: ", captiont)
dygraph(cumsum(wealthv), main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)




##############################
### Homeworks tests
##############################

############## homework
# Summary: Implement the Newton/IRLS multivariate optimization 
# algorithm for calibrating the logistic regression model.

# AI code:
logistic_irls <- function(X, y, tol = 1e-8, maxit = 50) {
  ##  X: n x p design matrix (including intercept column)
  ##  y: binary response vector (0/1)
  
  n <- nrow(X)
  p <- ncol(X)
  
  beta <- rep(0, p)        ##  initial coefficients
  history <- list()
  
  for (iter in 1:maxit) {
    eta <- X %*% beta
    p_hat <- 1 / (1 + exp(-eta))      ##  logistic mean
    
    ##  weights and working response
    W <- as.vector(p_hat * (1 - p_hat))
    z <- eta + (y - p_hat) / W
    
    ##  IRLS step: solve weighted least squares
    WX <- X * W
    H <- t(X) %*% WX                  ##  Hessian
    g <- t(X) %*% (y - p_hat)         ##  gradient
    
    step <- solve(H, g)
    beta_new <- beta + step
    
    history[[iter]] <- beta_new
    
    ##  convergence check
    if (max(abs(beta_new - beta)) < tol) {
      beta <- beta_new
      break
    }
    
    beta <- beta_new
  }
  
  list(
    coefficients = beta,
    fitted = p_hat,
    iterations = iter,
    history = history
  )
} # end logistic_irls



############## homework
# Simulate a Brownian bridge stopping problem.
# Given deck of 52 cards, every time you randomly choose a
# red card your account increases by $1, but if you choose
# black card your account decreases by -$1 At any point you
# can choose to continue playing, or to stop and keep your
# net wins.
# The optimal strategy is to stop playing if the current net
# wins are greater than the expected value of wins from
# continuing to play.
# Calculate the expected value of the optimal strategy,
# assuming you start with zero in your account.

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
}  ##  end for

stratmat[1, 1]

stratfun <- function(cash, positive, negative) {
  ##  cat(paste("args=", cash, positive, negative, "\n"))
  probv <- positive/(positive + negative)
  if (positive==0)
    max(cash, 0)
  else if (negative==0)
    max(cash + positive, 0)
  else
    max(cash,
        probv*stratfun(cash+1, positive-1, negative) +
          (1-probv)*stratfun(cash-1, positive, negative-1))
}  ##  end stratfun

# stratfun(0, 26, 26)
stratfun(0, 3, 3)
stratfun(3, 0, 3)
stratfun(2, 1, 0)
stratfun(-3, 3, 0)
stratfun(0, 3, 3)

sapply(3:1, function(positive, negative)
  stratfun(negative-positive, positive, negative),
  negative=3:1)



############## homework
# Select the manager with skill.
# Find how many data points are needed to determine which manager has skill,
# when there are multiple managers with only one manager having skill.
# Simulate several managers, with only one manager with skill, and try to
# determine how much data is needed to determine which manager has skill.

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
set.seed(1121)  ##  reset random number generator
retp <- sapply(meanv, rnorm, n=nrows, sd=volv)
retp <- apply(retp, 2, cumsum)
# apply(retp, 2, mean)
# plot.zoo(retp, plot.type="single")

# length of lookback window
lookb <- 100
# define endd with beginning stub
nagg <- nrows %/% lookb
endd <- c(0, nrows-lookb*nagg+lookb*(0:nagg))
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
# })  ##  end lapply
# pnls <- rutils::do_call(c, pnls)
plot.zoo(cumsum(pnls))


## cum_pnl for multi-manager strategy
cum_pnl <- function(sharper, retp=NULL, meanv=NULL, nman, nrows, lookb, volv=0.01) {
  ##  Simulate Brownian motion
  if(is.null(retp)) {
    probv <- (sharper/sqrt(250)+1)/2
    ##  Adjust probability to account for multiple managers
    p1 <- (0.5*nman + (probv - 0.5)*(nman-1)) / nman
    p2 <- (0.5*nman - (probv - 0.5)) / nman
    meanv <- c(2*p1-1, rep(2*p2-1, nman-1))
    set.seed(1121)  ##  reset random number generator
    retp <- sapply(meanv, rnorm, n=nrows, sd=volv)
    retp <- apply(retp, 2, cumsum)
  } else {
    nman <- NCOL(retp)
    nrows <- NROW(retp)
  }  ##  end if
  
  ##  define endd with beginning stub
  nagg <- nrows %/% lookb
  endd <- c(0, nrows-lookb*nagg+lookb*(0:nagg))
  nrows <- NROW(endd)
  ##  startp are single-period lag of endd
  startp <- endd[c(1, 1:(nrows-1))] + 1
  fix_points <- (startp > endd)
  startp[fix_points] <- endd[fix_points]
  
  ##  total returns over non-overlapping windows
  retagg <- apply(retp, 2, function(x) (x[endd]-x[startp]))
  
  ##  Switch to best manager with biggest total returns
  bestm <- apply(retagg, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  bestm <- c(rep(1, NROW(endd)-NROW(bestm)), bestm)
  pnls <- lapply(seq_along(endd), function(datev) {
    retp[startp[datev]:endd[datev], bestm[datev]]
  })  ##  end lapply
  ##  return total expected pnl
  pnls <- rutils::do_call(c, pnls)
  sum(pnls)
}  ##  end cum_pnl

cum_pnl(retp=retp, lookb=100)

cum_pnl(sharper=0.4, nman=11, lookb=100, nrows=5000)



## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(lookb, nrows=NULL, sharper=NULL, retp=NULL, meanv=NULL, nman=NULL, volv=0.01) {
  ##  Calculate the drifts
  if(is.null(meanv)) {
    probv <- (sharper/sqrt(250)+1)/2
    ##  Adjust probability to account for multiple managers
    p1 <- (0.5*nman + (probv - 0.5)*(nman-1)) / nman
    p2 <- (0.5*nman - (probv - 0.5)) / nman
    meanv <- volv*lookb*c(2*p1-1, rep(2*p2-1, nman-1))
  } else {
    nman <- NROW(meanv)
  }  ##  end if
  ##  Simulate Brownian motion
  if(is.null(retp)) {
    ##  set.seed(1121)  ##  reset random number generator
    nagg <- nrows %/% lookb
    retp <- sapply(meanv, rnorm, n=nagg, sd=sqrt(lookb)*volv)
  } else {
    nman <- NCOL(retp)
    nrows <- NROW(retp)
  }  ##  end if
  
  ##  Switch to best manager with biggest total returns
  bestm <- apply(retp, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  ##  return total expected pnl
  ##  pnls <- retp[cbind(1:NROW(retp), bestm)]
  sum(retp[cbind(1:NROW(retp), bestm)])
}  ##  end cum_pnl



## cum_pnl for multi-manager strategy (simplest version)
cum_pnl <- function(lookb, nrows, sharper=NULL, retp=NULL, meanv=NULL, nman=NULL, volv=0.01) {
  ##  Calculate the drifts
  if(is.null(meanv)) {
    probv <- (sharper/sqrt(250)+1)/2
    ##  Adjust probability to account for multiple managers
    p1 <- (0.5*nman + (probv - 0.5)*(nman-1)) / nman
    p2 <- (0.5*nman - (probv - 0.5)) / nman
    meanv <- volv*lookb*c(2*p1-1, rep(2*p2-1, nman-1))
  } else {
    nman <- NROW(meanv)
  }  ##  end if
  
  ##  Calculate the probability of selecting the best manager
  probv <- integrate(function(x, ...)
    dnorm(x, mean=meanv[1], ...)*pnorm(x, mean=meanv[2], ...)^(nman-1),
    low=-3.0, up=3.0,
    sd=sqrt(lookb)*volv)$value
  ##  return total expected pnl
  nagg <- nrows %/% lookb
  nagg*(probv*meanv[1] + (1-probv)*meanv[2])
}  ##  end cum_pnl

cum_pnl(sharper=0.4, nman=11, meanv=meanv, lookb=100, nrows=5000)
cum_pnl(sharper=0.4, nman=11, lookb=100, nrows=5000)
cum_pnl(lookb=100, sharper=0.4, nman=11, nrows=5000)

# Calculate the average pnl
foo <- mean(sapply(1:10000, function(x)
  cum_pnl(meanv=meanv, lookb=100, nrows=5000)))

foo <- mean(sapply(1:10000, function(x)
  cum_pnl(lookb=100, sharper=0.4, nman=11, nrows=500000)))

# perform loop over lookback windows
lookbs <- 100*(1:20)
foo <- sapply(lookbs, cum_pnl,
              sharper=0.4, nman=11, nrows=50000)
foo <- cbind(lookbs, foo)
plot(foo, t="l")
plot(cumsum(pnls), t="l")

# perform loop over number of managers
nman <- 2*(1:50)
foo <- sapply(nman, cum_pnl,
              retp=NULL, sharper=0.4, lookb=100, nrows=50000, meanv=NULL, volv=0.01)
foo <- cbind(nman, foo)
plot(foo, t="l")



##############
# Simulation of asset returns, with a time-dependent drift (skill) plus a random
# noise.

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

set.seed(1121)  ##  reset random number generator
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


## Calculate the pnl over lookback window
# Calculate the cumulative returns
pnls <- apply(retp, 2, cumsum)
# length of lookback window
lookb <- 100
# define endd with beginning stub
nagg <- nrows %/% lookb
endd <- c(0, nrows-lookb*nagg+lookb*(0:nagg))
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
cum_pnl <- function(lookb, returns) {
  nrows <- NROW(retp)
  ##  define endd with beginning stub
  nagg <- nrows %/% lookb
  endd <- c(0, nrows-lookb*nagg+lookb*(0:nagg))
  nrows <- NROW(endd)
  ##  startp are single-period lag of endd
  startp <- endd[c(1, 1:(nrows-1))] + 1
  fix_points <- (startp > endd)
  startp[fix_points] <- endd[fix_points]
  ##  total returns aggregated over non-overlapping windows
  retp <- apply(retp, 2, function(x) (x[endd]-x[startp]))
  ##  Switch to best manager with biggest total returns
  bestm <- apply(retp, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  ##  return total expected pnl
  ##  pnls <- retp[cbind(1:NROW(retp), bestm)]
  sum(retp[cbind(1:NROW(retp), bestm)])
}  ##  end cum_pnl

cum_pnl(lookb=100, retp=pnls)


## cum_pnl for trend following multi-manager strategy (without endd)
cum_pnl <- function(lookb, returns, pnls) {
  ##  nrows <- NROW(retp)
  ##  total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=lookb)
  ##  Switch to best manager with biggest total returns
  bestm <- apply(retagg, 1, which.max)
  bestm <- rutils::lagit(bestm)
  bestm[1] <- 1
  ##  return total expected pnl
  ##  pnls <- retp[cbind(1:NROW(retp), bestm)]
  sum(retp[cbind(1:NROW(retp), bestm)])
}  ##  end cum_pnl

# Calculate the cumulative returns
pnls <- apply(retp, 2, cumsum)
cum_pnl(lookb=100, retp=retp, pnls=pnls)


## perform loop over lookback windows
# lengths of lookbacks windows
lookbs <- 20*(1:50)
pnls <- sapply(lookbs, cum_pnl, subx=1, retp=retp, pnls=pnls)
pnls <- cbind(lookbs, pnls)
plot(pnls, t="l")
# plot(cumsum(pnls), t="l")


## pre-calculate row order indices for a vector of lookbs
# perform loop over lookback windows
# lengths of lookbacks windows
lookbs <- 20*(1:50)
orderstats <- lapply(lookbs, function(lookb) {
  ##  total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=lookb)
  ordern <- t(apply(retagg, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  ##  end lapply
names(orderstats) <- lookbs


## cum_pnl for long-short multi-manager strategy (without endd)
cum_pnl <- function(select_best=NULL, select_worst=NULL, returns, ordern) {
  nrows <- NROW(retp)
  if(!is.null(select_best)) {
    ncols <- NCOL(retp)
    bestm <- ordern[, (ncols-select_best+1):ncols]
    bestm <- cbind(1:nrows, bestm)
  } else {
    bestm <- NULL
  }  ##  end if
  if(!is.null(select_worst)) {
    worstm <- ordern[, 1:select_worst]
    worstm <- cbind(1:nrows, worstm)
  } else {
    worstm <- NULL
  }  ##  end if
  ##  return total expected pnl
  ##  pnls <- retp[bestm]-retp[worstm]
  sum(retp[bestm])/select_best-sum(retp[worstm])/(if(is.null(select_worst)) 1)
}  ##  end cum_pnl

# Calculate the pnl for long-short multi-manager strategy
cum_pnl(select_best=1, select_worst=1, retp=retp, ordern=orderstats[[5]])


## perform loop over lookback windows
pnls <- sapply(orderstats, cum_pnl, select_best=1, select_worst=1, retp=retp)
pnls <- cbind(lookbs, pnls)
plot(pnls, t="l")
# plot(cumsum(pnls), t="l")


nman <- 5
drift <- sapply(1:nman, function(x)
  mea_n*sin(ra_te*(1:nrows)/nrows + 2*pi*x/nman))
set.seed(1121)  ##  reset random number generator
retp <- matrix(volv*rnorm(nman*nrows) - volv^2/2, nc=nman) + drift
# Calculate the cumulative returns
pnls <- apply(retp, 2, cumsum)

## pre-calculate row order indices for a vector of lookbs
lookbs <- 20*(1:50)
orderstats <- lapply(lookbs, function(lookb) {
  ##  total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=lookb)
  ordern <- t(apply(retagg, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  ##  end lapply
names(orderstats) <- lookbs

## cum_pnl for long-short multi-manager strategy (without endd)
cum_pnl <- function(select_best=NULL, select_worst=NULL, returns, ordern) {
  nrows <- NROW(retp)
  if(!is.null(select_best)) {
    ncols <- NCOL(retp)
    bestm <- ordern[, (ncols-select_best+1):ncols]
    bestm <- cbind(1:nrows, bestm)
  } else {
    bestm <- NULL
  }  ##  end if
  if(!is.null(select_worst)) {
    worstm <- ordern[, 1:select_worst]
    worstm <- cbind(1:nrows, worstm)
  } else {
    worstm <- NULL
  }  ##  end if
  ##  return total expected pnl
  ##  pnls <- retp[bestm]-retp[worstm]
  sum(retp[bestm])-sum(retp[worstm])
}  ##  end cum_pnl

# Calculate the pnl for long-short multi-manager strategy
# cum_pnl(select_best=1, select_worst=1, retp=retp, ordern=orderstats[[5]])

# perform loop over lookback windows
pnls <- sapply(orderstats, cum_pnl, select_best=1, select_worst=NULL, retp=retp)
pnls <- cbind(lookbs, pnls)
# par(mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1))
# plot(pnls, t="l", main="Trend following PnL, as function of lookback window")


## double the drift
set.seed(1121)  ##  reset random number generator
retp <- matrix(volv*rnorm(nman*nrows) - volv^2/2, nc=nman) + 2*drift
# Calculate the cumulative returns
pnls <- apply(retp, 2, cumsum)

## pre-calculate row order indices for a vector of lookbs
orderstats2x <- lapply(lookbs, function(lookb) {
  ##  total returns aggregated over overlapping windows
  retagg <- apply(pnls, 2, rutils::diffit, lag=lookb)
  ordern <- t(apply(retagg, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  ##  end lapply
names(orderstats2x) <- lookbs

plot.zoo(cbind(pnls[, 2], pnls2x), main="Long-short Ensemble PnL, as function of lookback window",
         lwd=2, xaxt="n", xlab="lookback windows", ylab="PnL", plot.type="single", col=c("black", "red"))
# Add x-axis
axis(1, seq_along(lookbs), lookbs)
# Add legend
legend(x="top", legend=paste0("SR=", c(0.4, 0.8)),
       inset=0.0, cex=0.8, bg="white",
       lwd=6, lty=c(1, 1), col=c("black", "red"))




## parallel version with loops - much slower and more complicated
# initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(num_cores-1)

foo <- sapply(lookbs, function(lookb) {
  ##  define endd with beginning stub
  nagg <- nrows %/% lookb
  endd <- c(0, nrows-lookb*nagg+lookb*(0:nagg))
  nrows <- NROW(endd)
  ##  startp are single-period lag of endd
  startp <- endd[c(1, 1:(nrows-1))] + 1
  ##  redefine endd
  endd <- cbind(startp, endd)
  
  ##  perform parallel loop over returns
  clusterExport(cluster, varlist=c("nrows", "endd"))
  sharper <- parApply(cluster, MARGIN=2, returns, function(retp) {
    sapply(2:nrows, function(datev) {
      xtsv <- retp[endd[datev, 1]:endd[datev, 2]]
      ##  Calculate the annualized Sharpe ratio of returns
      sum(xtsv)/sd(xtsv)
    })  ##  end sapply
  })  ##  end parApply
  
  ##  sharper <- rutils::do_call(cbind, sharper)
  sharper[which(is.na(sharper), arr.ind=TRUE)] <- 1
  
  ##  Calculate the dispersion of SRs
  ##  c(by_strategy=mean(apply(sharper, 1, sd)),
  ##    by_period=mean(apply(sharper, 2, sd)))
  ##  diff_sr <- apply(sharper, 2, rutils::diffit) / sharper
  ##  mean(abs(diff_sr))
  ##  c(by_strategy=mean(apply(sharper, 1, sd)),
  ##    by_period=mean(apply(diff_sr, 1, sd)))
  cum_pnl(sharper, returns, endd)
})  ##  end sapply

foo <- t(foo)
dim(foo)
foo
foo <- cbind(lookbs, foo)
plot(foo, t="l")
plot(foo[, 1]/foo[, 2], t="l")

## end perform loop over lookback windows



# Not finished yet.  Is it too simple?
############## test
# Summary: Simulate a dual crossover strategy using 
# the EMA returns.

## Run the setup code below

# Extract the log SPY returns
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
datev <- zoo::index(retp)
nrows <- NROW(retp)
lambdaf <- 0.8 ##  Fast lambda
lambdas <- 0.9 ##  Slow lambda

## End of setup code


# 1. (10pts)
# Copy the function sim_ema2() from the lecture slides, 
# and replace closep with retp.



# Aggregating works well
############## homework
# Summary: Calculate the covariance matrix for returns
# over weekly non-overlapping intervals.
# Calculate the out-of-sample performance of the
# maximum Sharpe portfolio.
# Simulate a rolling portfolio strategy.

# Comment:
# Aggregating the returns over 17 business day
# intervals greatly improves the performance.

# Comment:
# It's slightly better to scale the returns by the volume
# instead of the square root of the volume.
# But both improve the performance of the out-of-sample
# Sharpe portfolio.


# From the slide Optimal Stock Portfolio Out-of-Sample
# Load stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select the stock prices since GOOGL
datev <- zoo::index(na.omit(pricestock$GOOGL))
pricev <- pricestock[datev]
# Select stocks with no NA values in their prices
numna <- sapply(pricev, function(x) sum(is.na(x)))
pricev <- pricev[, numna == 0]
# Drop penny stocks
pricel <- last(pricev)
pricel <- drop(coredata(pricel))
pricev <- pricev[, pricel > 1]
# Calculate the log stock returns
retp <- rutils::diffit(log(pricev))
retp[1, ] <- 0
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
symbolv <- colnames(retp)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
retis <- retp["/2014"] ##  In-sample returns
raterf <- 0.03/252
retx <- (retis - raterf) ##  Excess returns


# Calculate the weekly log returns
endw <- rutils::calc_endpoints(pricev, interval="weeks")
retw <- rutils::diffit(log(pricev[endw]))
# Or
# retw <- lapply(2:NROW(endw), function(tday) {
#   pricel <- coredata(pricev[endw[tday-1]+1, ])
#   return((pricev[endw[tday], ] - pricel)/pricel)
# }) ##  end lapply
# retw <- do.call(rbind, retw)
# datev <- zoo::index(retw)


# Scale the returns using volume clock to trading time
ohlc <- rutils::etfenv$VTI
ohlc <- ohlc[datev] ##  Subset the returns to GOOGL
volumv <- quantmod::Vo(ohlc)
volumr <- HighFreq::run_mean(volumv, lambda=0.92)
volums <- (as.numeric(volumr/volumv))
retp <- reto*volums

# Maximum Sharpe weights in-sample interval
covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
colmeanv <- colMeans(retx, na.rm=TRUE)
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- symbolv
head(sort(weightv))
tail(sort(weightv))
# Calculate the portfolio returns
pnls <- HighFreq::mult_mat(weightv, reto)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
# retew <- xts::xts(rowMeans(reto, na.rm=TRUE), datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])

# Condition number of correlation matrices equal the sum of the negative eigenvalues.
cormat <- cor(retis, use="pairwise.complete.obs")
eigenv <- eigen(cormat)$values
condnum <- sum(eigenv[eigenv < 0])
condnum <- kappa(cormat)


# Calculate the in-sample Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")
sqrt(252)*sapply(wealthv["/2014"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
                  main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)


# Calculate the out-of-sample pnls as a function of lambdaf

lambdav <- seq(0.8, 0.99, 0.01)
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
pnlv <- mclapply(lambdav, function(lambdaf) {
  
  volumr <- HighFreq::run_mean(volumv, lambda=lambdaf)
  volums <- (as.numeric(volumr/volumv))
  retp <- reto*volums
  retis <- retp["/2014"] ##  In-sample returns
  covmat <- cov(retis, use="pairwise.complete.obs")
  invreg <- MASS::ginv(covmat)
  colmeanv <- colMeans((retis - raterf), na.rm=TRUE)
  weightv <- drop(invreg %*% colmeanv)
  ##  Calculate the portfolio returns
  pnls <- HighFreq::mult_mat(weightv, reto)
  pnls <- rowMeans(pnls, na.rm=TRUE)
  pnls <- xts::xts(pnls, datev)
  pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
  pnls <- pnls["2015/"]
  
  return(mean(pnls)/sd(pnls))
  
}, mc.cores=ncores)  ## end mclapply
pnlv <- unlist(pnlv)
plot(lambdav, pnlv, type="b",
     main="Out-of-Sample Sharpe Ratio vs Lambda Decay Factor",
     xlab="Lambda Decay Factor", ylab="Sharpe Ratio")
lambdaf <- lambdav[which.max(pnlv)]


# Calculate the out-of-sample pnls as a function of the aggregation interval
aggv <- 11:21
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
colmeanv <- colMeans((retis - raterf), na.rm=TRUE)
pnlv <- mclapply(aggv, function(aggi) {
  
  ##  Calculate the weekly log returns
  endw <- rutils::calc_endpoints(pricev, interval=aggi)
  retw <- rutils::diffit(log(pricev[endw]))
  retw[1,] <- 0
  ##  retw <- lapply(2:NROW(endw), function(tday) {
  ##    pricel <- coredata(pricev[endw[tday-1]+1, ])
  ##    return((pricev[endw[tday], ] - pricel)/pricel)
  ##  }) ##  end lapply
  ##  retw <- do.call(rbind, retw)
  ##  Calculate the maximum Sharpe weights in-sample interval
  covmat <- cov(retw["/2014"], use="pairwise.complete.obs")
  invreg <- MASS::ginv(covmat)
  weightv <- drop(invreg %*% colmeanv)
  ##  Calculate the portfolio returns
  ##  pnls <- HighFreq::mult_mat(weightv, retp)
  ##  pnls <- rowMeans(pnls, na.rm=TRUE)
  pnls <- drop(retp %*% weightv)
  pnls <- xts::xts(pnls, datev)
  pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
  pnls <- pnls["2015/"]
  
  return(mean(pnls)/sd(pnls))
  
}, mc.cores=ncores)  ## end mclapply
pnlv <- unlist(pnlv)
plot(aggv, pnlv, type="b",
     main="Out-of-Sample Sharpe Ratio vs Aggregation Interval",
     xlab="Aggregation Interval", ylab="Sharpe Ratio")
aggi <- aggv[which.max(pnlv)]


# Perform a loop over the aggregation dates
endw <- index(retw)
nrows <- NROW(endw)
lookb <- 10  ##  Look-back interval in weeks
dimax <- 9
pnls <- mclapply((lookb+1):nrows, function(tday) {
  
  cat("\nProcessing tday =", tday, "of", nrows)
  ##  Define in-sample and out-of-sample intervals
  insample <- paste0(endw[tday-lookb], "/", endw[tday-1])
  ##  insample <- (1:endd[tday-1]) ##  Expanding look-back
  outsample <- paste0((endw[tday-1]+1), "/", endw[tday])
  ##  Calculate the returns over non-overlapping intervals
  retis <- retp[insample] ##  In-sample returns
  ##  Calculate the maximum Sharpe weights in-sample interval
  covmat <- cov(retis, use="pairwise.complete.obs")
  eigend <- eigen(covmat)
  eigenvec <- eigend$vectors
  eigenval <- eigend$values
  invred <- eigenvec[, 1:dimax] %*%
    (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
  ##  invreg <- MASS::ginv(covmat)
  colmeanv <- colMeans((retis - raterf), na.rm=TRUE)
  weightv <- drop(invred %*% colmeanv)
  ##  Calculate the portfolio returns
  retos <- retp[outsample]
  pnls <- HighFreq::mult_mat(weightv, retos)
  pnls <- rowMeans(pnls, na.rm=TRUE)
  pnls <- xts::xts(pnls, index(retos))
  pnls <- pnls*sd(retew[outsample])/sd(pnls)
  
  return(pnls)
  
}, mc.cores=ncores)  ## end mclapply
pnls <- do.call(rbind, pnls)
dygraph(cumsum(pnls)) %>%
  dyOptions(colors=c("blue"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)


############## homework
## Sort stocks based on their overnight returns.

symboln <- "TSLA"
ohlc <- log(get(symboln, sp500env))
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep) ##  daily returns
colnames(retp) <- "daily"
retd <- (closep - openp) ##  daytime returns
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, pad_zeros=FALSE)) ##  overnight returns
colnames(reton) <- "overnight"
# Calculate the Sharpe and Sortino ratios
symboln <- "CVX"
rets <- get(symboln, retl)#["2010/"]
wealthv <- cbind(rets$daily, rets$overnight, rets$daytime)
# wealthv <- cbind(retp, reton, retd)
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot the log wealth
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main=paste(symboln, "Daily, Overnight, and Daytime Returns")) %>%
  dySeries(name="daily", strokeWidth=1, col="blue") %>%
  dySeries(name="overnight", strokeWidth=1, col="red") %>%
  dySeries(name="daytime", strokeWidth=1, col="green") %>%
  dyLegend(width=500)

wealthv <- (reton - retd)
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main=paste(symboln, "Overnight Minus Daytime Returns")) %>%
  dyOptions(colors="blue", strokeWidth=2)


# Calculate the overnight returns of the top momentum stocks
symbolv <- c("CVX", "FE", "VZ", "CNP", "OKE", "RTX", "AEP", "DUK")
wealthv <- reton[, symbolv]
wealthv <- retstock[, symbolv]
wealthv <- rowSums(wealthv, na.rm=TRUE)
wealthv <- xts::xts(wealthv, order.by=index(reton))
colnames(wealthv) <- "Overnight Returns"
dygraphs::dygraph(cumsum(wealthv), 
                  main="Top Momentum Overnight Returns") %>%
  dyOptions(colors="blue", strokeWidth=1)


# Load OHLC prices from .RData file
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

# Perform a loop over the sp500env environment and calculate the overnight returns for each stock
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
retl <- mclapply(sp500env, function(ohlc) {
  ohlc <- log(ohlc)
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retp <- rutils::diffit(closep) ##  daily returns
  colnames(retp) <- "daily"
  retd <- (closep - openp) ##  daytime returns
  colnames(retd) <- "daytime"
  reton <- (openp - rutils::lagit(closep, pad_zeros=FALSE)) ##  overnight returns
  colnames(reton) <- "overnight"
  # highp <- quantmod::Hi(ohlc)
  # lowp <- quantmod::Lo(ohlc)
  # hilo <- (highp - lowp) ##  range of daily prices
  return(cbind(retp, retd, reton))
}, mc.cores=ncores) # end mclapply

reton <- lapply(retl, function(x) x$overnight)
reton <- do.call(cbind, reton)
colnames(reton) <- rutils::get_name(names(sp500env))

# Calculate the volatility of the daily returns
volv <- sapply(retl, function(x) {
  return(sd(x$daily, na.rm=TRUE))
}) ##  end sapply
# Calculate the rescaled range of the overnight and daytime returns
ranger <- sapply(retl, function(x) {
  x <- x["2010/"]
  reton <- x$overnight
  nrows <- NROW(reton)
  if (nrows < 300) return(NULL)
  csum <- cumsum(reton)
  rangon <- (max(csum)-min(csum))/sd(reton)/nrows
  retd <- x$daytime
  csum <- cumsum(retd)
  ranged <- (max(csum)-min(csum))/sd(retd)/nrows
  return(rangon/ranged)
}) ##  end sapply
ranger <- unlist(ranger)
ranger <- sort(ranger, decreasing=TRUE)
head(ranger)
tail(ranger)

# Scatterplot of range ratio versus volatility of daily returns
plot(volv, ranger, xlim=c(0, 0.05), 
     xlab="Volatility of Daily Returns", ylab="Range Ratio",
     main="Scatterplot of Range Ratio vs Volatility of Daily Returns")

symbolv <- names(ranger)
symboln <- symbolv[1]
x <- retl[[symboln]]["2010/"]
datav <- cbind(x$overnight, x$daytime)
dygraph(cumsum(datav), main=paste(symboln, "Overnight and Daytime Returns")) %>%
  dyOptions(colors=c("red", "blue"), strokeWidth=1)


ranger <- lapply(retl, function(x) {
  x <- x["2022/"]
  return((x$overnight - x$daytime)/drop(var(x$daily)))
}) ##  end lapply
ranger <- do.call(cbind, ranger)
colnames(ranger) <- names(retl)
datev <- zoo::index(ranger)

pnlc <- sapply(ranger["/2019"], function(x) {
  return(sum(x, na.rm=TRUE))
}) ##  end sapply
pnlc <- sort(pnlc, decreasing=TRUE)
hist(pnlc)
head(pnlc)
tail(pnlc)
range(pnlc)
symbolv <- names(pnlc)
symboln <- symbolv[1]

madv <- mad(pnlc)
pnlc[abs(pnlc) > 3*madv] <- 0

rangeb <- ranger %*% pnlc
rangeb <- HighFreq::mult_mat(pnlc, ranger)
rangeb <- rowSums(rangeb, na.rm=TRUE)

rangeb <- ranger[, names(pnlc)[1:(NROW(pnlc) %/% 4)]]
rangeb <- ranger["2020/", names(pnlc)[1:(NROW(pnlc) %/% 4)]]
wealthv <- rowSums(rangeb, na.rm=TRUE)
wealthv <- xts::xts(wealthv, order.by=datev)
wealthv <- xts::xts(wealthv, order.by=zoo::index(rangeb))
sqrt(252)*c(mean(wealthv)/sd(wealthv), mean(wealthv)/sd(wealthv[wealthv<0]))
dygraph(cumsum(wealthv), main="Overnight Minus Daytime Returns") %>%
  dyOptions(colors="blue", strokeWidth=1)


weightv <- kellyr[nrows, ]
names(weightv) <- colnames(reton)
weightv[is.na(weightv)] <- 0
sum(weightv > 0)
weightv <- weightv[weightv > 0]


############## homework
# Sort stocks by the smallest rescaled range of their daytime returns.

ranger <- sapply(retl, function(x) {
  retd <- x$daytime#["2023/"]
  nrows <- NROW(retd)
  if (nrows < 300) return(NULL)
  csum <- cumsum(retd)
  ranged <- (max(csum)-min(csum))/sd(retd)/nrows
  return(ranged)
}) ##  end sapply
ranger <- unlist(ranger)
ranger <- sort(ranger, decreasing=FALSE)
head(ranger)
tail(ranger)

reton <- retonbak[, names(head(ranger, 200))]
retstock <- retstockb[, names(head(ranger, 200))]

weightv <- kellyr[nrows, ]
names(weightv) <- colnames(reton)
weightv[is.na(weightv)] <- 0
sum(weightv > 0)
weightv <- weightv[weightv > 0]


############## homework
# Trade the daytime or overnight returns based on their lagged returns.
# The strategy doesn't perform well.

# Load daily OHLC bars
symboln <- "SPY"
ohlc <- log(get(symboln, rutils::etfenv))
datev <- zoo::index(ohlc)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)
# colnames(retp) <- "daily"
retd <- (closep - openp)
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"

# Trade the daytime returns based on the overnight returns
# pnls <- sign(reton)*retd
# pnls <- pnls*sd(retd[retd<0])/sd(pnls[pnls<0])

# Trade the overnight returns based on the lagged daytime returns
pnls <- -reton*sign(rutils::lagit(retd))
pnls <- pnls*sd(reton[reton<0])/sd(pnls[pnls<0])

# Trade overnight returns based on the lagged overnight returns
# pnls <- -reton*sign(rutils::lagit(reton, lagg=1, pad_zeros=FALSE))
# pnls <- pnls*sd(reton[reton<0])/sd(pnls[pnls<0])

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c(rutils::get_name(colnames(retp)), "Strategy")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

dygraphs::dygraph(cumsum(wealthv),
                  main="Autoregressive Strategy of Daytime Returns") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)


# Bollinger strategy using the overnight returns scaled by volatility

volv <- HighFreq::run_var_ohlc(ohlc, lambda=0.7)
volv <- sqrt(volv)
zscore <- reton/rutils::lagit(volv, lagg=1)
dygraph(zscore)
# Trade the daytime returns based on the overnight returns
pnls <- -retd*zscore
pnls[1] <- 0



############## homework
# The overnight returns of XLK were negatively correlated with
# its previous daytime returns, until 2010, but not after that.
# Below is a strategy that holds an overnight position opposite
# to the previous daytime return.

symboln <- "XLK"
ohlc <- log(get(symboln, rutils::etfenv))
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retd <- (closep - openp)
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"
pnlv <- reton*sign(retd)
colnames(pnlv) <- symboln
dygraph(cumsum(pnlv), main=symboln)

comv <- cbind(reton, pnlv, (reton+pnlv)/2)
colnames(comv) <- c("ONReturn", "Strategy", "Average")
dygraphs::dygraph(cumsum(comv["2010/"]), main=colnames(comv)) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)
sapply(comv["2010/"], function(x) sqrt(252)*mean(x)/sd(x))

# The strategy of holding an overnight position in proportion
# to the sign of the daytime return is profitable, even though
# the correlation between daytime and overnight returns is low.
# Because the sign is a good classifier of the overnight returns.

regmod <- lm(reton ~ retd)
summary(regmod)
plot(reton ~ retd, main="Daytime vs Overnight Returns", col="blue")
abline(regmod, col="red", lwd=2)

datav <- cbind(reton, retd)
datas <- datav[abs(datav$daytime) > 0.02, ]
regmod <- lm(overnight ~ daytime, data=datas)
summary(regmod)
plot(overnight ~ daytime, data=datas,
     main="Daytime vs Overnight Returns", col="blue")
abline(regmod, col="red", lwd=2)


# Fit in-sample logistic regression
datav <- cbind((as.numeric(sign(reton)+1)/2), retd)
colnames(datav)[1] <- "overnight"
datav <- as.data.frame(datav)
datav[, 1] <- as.integer(datav[, 1])
glmod <- glm(overnight ~ daytime, data=datav, family=binomial(logit))
summary(glmod)

datav <- cbind((as.numeric(sign(retd)+1)/2), reton)
colnames(datav) <- c("daytime", "overnight")
datav <- as.data.frame(datav)
datav[, 1] <- as.integer(datav[, 1])
glmod <- glm(daytime ~ overnight, data=datav, family=binomial(logit))
summary(glmod)


# Calculate the strategy of holding an overnight position opposite
# to the previous daytime return.

pnls <- sapply(rutils::etfenv$symbolv, function(symboln) {
  ohlc <- log(get(symboln, rutils::etfenv)["2010/"])
  openp <- quantmod::Op(ohlc)
  closep <- quantmod::Cl(ohlc)
  retd <- (closep - openp)
  colnames(retd) <- "daytime"
  reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
  colnames(reton) <- "overnight"
  pnlv <- reton*sign(retd)
  sum(pnlv)
}) ##  end sapply
pnls <- sort(pnls)



############## homework
# Summary: Simulate an overnight strategy combined 
# with range volatility timing.

## Run the setup code below

library(rutils)

# Calculate the range of daily prices for SPY
symboln <- "SPY"
ohlc <- log(get(symboln, rutils::etfenv))
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep) ##  daily returns
colnames(retp) <- "daily"
retd <- (closep - openp) ##  daytime returns
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1)) ##  overnight returns
colnames(reton) <- "overnight"
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
hilo <- (highp - lowp) ##  range of daily prices

volt <- 0.01 ##  Volatility target for scaling the strategy PnLs


## End of setup code


# 1. (10pts)
# Calculate the range volatility using the function HighFreq::run_var_ohlc()

lambdaf <- 0.3
volv <- HighFreq::run_var_ohlc(ohlc, lambda=lambdaf)
volv <- rutils::lagit(volv, lagg=1)
volv[1] <- 1
pnls <- reton*volt/volv
reton[1] <- 0

wealthv <- cbind(reton, pnls)
wealthv[1, ] <- 0
colnames(wealthv) <- c("Overnight", "Strategy")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraph(cumsum(wealthv)[endw],
        main="Overnight Returns Scaled by the Range Volatility") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1)

# Extra stuff
# Calculate the dates of ohlc
datev <- zoo::index(ohlc)
datev <- format(datev, "%Y-%m-%d")
# datev <- unique(datev)

# lapply over the dates to calculate the ohlc for each date
ohlcl <- lapply(unique(datev), function(d) {
  selv <- (datev == d)
  return(ohlc[selv, ])
}) ##  end lapply
ohlc <- ohlcl
names(ohlc) <- unique(datev)

save(ohlc, file="/Users/jerzy/Develop/data/minutes/SPY_minutes_list.RData")

# end Extra stuff


############## homework
## Calculate the idiosyncratic overnight returns

# Conclusion:
# There's little advantage in using the idiosyncratic 
# overnight returns instead of the overnight returns.  

# Calculate the SPY overnight returns
symboln <- "SPY"
ohlc <- log(get(symboln, rutils::etfenv))
openp <- quantmod::Op(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep) ##  daily returns
colnames(retp) <- "daily"
retd <- (closep - openp) ##  daytime returns
colnames(retd) <- "daytime"
retonspy <- (openp - rutils::lagit(closep, lagg=1)) ##  overnight returns
colnames(retonspy) <- "overnight"
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
hilo <- (highp - lowp) ##  range of daily prices


# Calculate the idiosyncratic overnight returns
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
datespy <- index(retonspy)
retid <- mclapply(reton, function(rets) {
  # cat(colnamev, "\n")
  rets <- na.omit(rets)[datespy]
  retm <- retonspy[index(rets)]
  # if (NROW(retm) == 0) return(NULL)
  betav <- drop(cov(rets, retm)/var(retm))
  return(rets - betav*retm)
}, mc.cores=ncores) # end mclapply
retid <- do.call(cbind, retid)
colnames(retid) <- colnames(reton)

foo <- rowSums(retonbak, na.rm=TRUE)
foo <- xts::xts(foo, order.by=index(retonbak))
sd(foo)
dygraph(cumsum(foo), main="Cumulative Idiosyncratic Overnight Returns") %>%
  dyOptions(colors="blue", strokeWidth=1)



############## homework
# Summary: Create a shiny app which simulates 
# the stop-start strategy for different ETFs.
# Solution file is: app_stop_start_strat.R
# 
# 1. (50pts)
# Modify the file app_ema_plot2.R and add code 
# from the file app_risk_parity_strat.R
# Adapt the function sim_stopstart() from the lecture 
# slides.  Add to it the arguments pricev and retp.
# 
# Rename your app to: yourname_stop_start_strat.R
# and upload it to Brightspace.
# You must submit a complete shiny app, so that 
# the user can hit "run" and run it, without any 
# modifications.
# 
# The user should be able to select any of the 
# ETFs in rutils::etfenv$symbolv.
# Add shading to the plot.
# 
# The shiny app output should be similar to: 
# shiny_stop_start_strat1.png
# shiny_stop_start_strat2.png
# shiny_stop_start_strat3.png
# 
# Hint: 
# Before running the app, delete everything from 
# your workspace, so that the app doesn't use 
# objects from your workspace:
rm(list=ls())


# test
# this is already in slides?  Just Hurst is in slides
##############
# Summary: Calculate the partial autocorrelations 
# and the Ljung-Box statistics for ETFs and stocks.

## Run the setup code below

library(rutils)

# Calculate the daily, daytime, and overnight returns of SPY
symboln <- "SPY"
ohlc <- log(get(symboln, rutils::etfenv))
datev <- zoo::index(ohlc)
nrows <- NROW(ohlc)
openp <- quantmod::Op(ohlc)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
closep <- quantmod::Cl(ohlc)
retp <- rutils::diffit(closep)
colnames(retp) <- "daily"
retd <- (closep - openp)
colnames(retd) <- "daytime"
reton <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))
colnames(reton) <- "overnight"

## End of setup code


# 1. (10pts)
# Calculate the sums of the partial autocorrelations
# for retp, retd, and reton, with lag.max=5.

sum(stats::pacf(retp, lag.max=5)$acf)
sum(stats::pacf(retd, lag.max=5)$acf)
sum(stats::pacf(reton, lag.max=5)$acf)

# You should get the following outputs:
# [1] -0.1623676
# [1] -0.1569918
# [1] -0.08875341

# This shows that the overnight returns have less
# negative autocorrelation than the daily and daytime 
# returns.

# 2. (20pts)
# Select the ETF symbols, excluding "VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ".

symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "QUAL", "VLUE", "USMV", "AIEQ"))]

# You should get the following output:
symbolv
#  [1] "SPY"  "VTI"  "QQQ"  "VEU"  "EEM"  "XLY"  "XLP"  "XLE"  "XLF"  "XLV" 
# [11] "XLI"  "XLB"  "XLK"  "XLU"  "VYM"  "IVW"  "IWB"  "IWD"  "IWF"  "IEF" 
# [21] "TLT"  "VNQ"  "DBC"  "GLD"  "USO"  "IVE"  "VTV"  "DYNF"


# Calculate the sums of the partial autocorrelations
# for symbolv with lag.max=5, from 2010 till today.
# Sort them in decreasing order.

pacfs <- sapply(symbolv, function(symb) {
  x <- na.omit(rutils::etfenv$returns["2010/", symb])
  pacff <- stats::pacf(x, plot=FALSE, lag.max=5)
  return(sum(pacff$acf))
}) ## end sapply
pacfs <- sort(pacfs, decreasing=TRUE)

# You should get the following output:
round(pacfs, 4)
#   USO     XLE     DBC     GLD     VNQ     XLB     XLY     XLI     IEF     VEU 
# 0.0731  0.0143 -0.0044 -0.0380 -0.0643 -0.0655 -0.0673 -0.0715 -0.0746 -0.0767 
#   DYNF     XLU     EEM     XLF     XLV     VTI     IWD     IWB     IVE     QQQ 
# -0.0998 -0.1041 -0.1079 -0.1147 -0.1153 -0.1166 -0.1180 -0.1205 -0.1295 -0.1319 
#   SPY     VTV     IVW     TLT     VYM     XLK     XLP     IWF 
# -0.1328 -0.1348 -0.1382 -0.1445 -0.1481 -0.1525 -0.1772 -0.2103 

# This shows that the oil, energy, and commodity ETFs have the
# highest (least negative) autocorrelations, while the technology
# and growth ETFs have the lowest (most negative) autocorrelations.



plot(pacfs, type="b",
     main="Sum of Partial Autocorrelations of ETF Returns",
     xlab="ETF Index", ylab="Sum of Partial Autocorrelations")




############## homework
# Summary: Calculate the out-of-sample stock portfolio returns
# as a function of the lambda shrinkage intensity.

## Run the setup code below
# Most of this code is from the lecture slides.

library(rutils)
# Load S&P 500 stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Subset the returns to dates after 2005
retp <- retstock["2005/"]
# Remove the stocks with any NA values
numna <- sapply(retp, function(x) sum(is.na(x)))
retp <- retp[, numna==0]
# Select 100 random stocks
retp <- retp[, sample(NCOL(retp), 100)]
ncols <- NCOL(retp)
symbolv <- colnames(retp)
datev <- zoo::index(retp)
# Calculate the in-sample returns
raterf <- 0.03/252
retis <- retp["/2014"]
retx <- (retis - raterf) ##  Excess returns

# Calculate the weights using the Moore-Penrose inverse
colmeanv <- colMeans(retx, na.rm=TRUE)
covmat <- cov(retx, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- symbolv
# weightv <- weightv/sqrt(sum(weightv^2))

# Objective with shrinkage penalty
objfun <- function(weightv, retp, lambdaf, alphaf) {
  retportf <- retp %*% weightv
  stdev <- sd(retportf)
  if (stdev == 0)
    return(0)
  else {
    penaltyv <- lambdaf*((1-alphaf)*sum(weightv^2) +
                           alphaf*sum(abs(weightv)))
    return(-mean(retportf)/stdev + penaltyv)
  }  ## end if
}  ## end objfun

# Calculate the in-sample portfolio weights
# using optimization with no shrinkage
lambdaf <- 0.0 ; alphaf <- 0.0
optiml <- optim(par=numeric(ncols),
                fn=objfun,
                retp=retx,
                lambdaf=lambdaf,
                alphaf=alphaf,
                method="L-BFGS-B",
                ##  control=list(factr=1e5),
                upper=rep(10, ncols),
                lower=rep(-10, ncols))
weighto <- optiml$par
names(weighto) <- symbolv
weighto <- weighto*sqrt(sum(weightv^2))/sqrt(sum(weighto^2))

# Compare the weights from the two methods
all.equal(weighto, weightv, check.attributes=FALSE)


# Calculate the equal-weighted portfolio returns
# maximum Sharpe portfolio returns
# pnlmaxs <- HighFreq::mult_mat(weightv, retp)
# pnlmaxs <- rowMeans(pnlmaxs, na.rm=TRUE)
# pnlmaxs <- xts::xts(pnlmaxs, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)

# datev <- zoo::index(na.omit(retstock$GOOGL))
# retp <- retstock[datev] ##  Subset the returns to GOOGL
# retis <- retp["/2014"] ##  In-sample returns
# raterf <- 0.03/252
# retx <- (retis - raterf) ##  Excess returns

# Calculate the maximum Sharpe portfolio returns
pnlmaxs <- retp %*% weighto
pnlmaxs <- xts::xts(pnlmaxs, datev)
pnlmaxs <- pnlmaxs*sd(retew["/2014"])/sd(pnlmaxs["/2014"])

# Calculate the out-of-sample Sharpe and Sortino ratios
# of the two portfolios
wealthv <- cbind(retew, pnlmaxs)
colnames(wealthv) <- c("Equal_Weight", "Max_Sharpe")
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the two portfolios
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
                  main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
  dyEvent(end(retew["/2014"]), label="cutoff", strokePattern="solid", color="red") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)


## End of setup code


# 1. (20pts)
# Create a vector of shrinkage intensities

lambdaf <- 10000.0 ; alphaf <- 0.0
optiml <- optim(par=numeric(ncols),
                fn=objfun,
                retp=retx,
                lambdaf=lambdaf,
                alphaf=alphaf,
                method="L-BFGS-B",
                ##  control=list(factr=1e5),
                upper=rep(10, ncols),
                lower=rep(-10, ncols))
weighto <- optiml$par
names(weighto) <- symbolv
weighto <- weighto*sqrt(sum(weightv^2))/sqrt(sum(weighto^2))


# Calculate the maximum Sharpe portfolio returns
pnlshrink <- retp %*% weighto
pnlshrink <- xts::xts(pnlshrink, datev)
pnlshrink <- pnlshrink*sd(retew["/2014"])/sd(pnlshrink["/2014"])

# Calculate the out-of-sample Sharpe and Sortino ratios
# of the two portfolios
wealthv <- cbind(pnlmaxs, pnlshrink)
colnames(wealthv) <- c("Max_Sharpe", "Shrinkage")
sqrt(252)*sapply(wealthv["2015/"], function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the two portfolios
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
                  main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
  dyEvent(end(retew["/2014"]), label="cutoff", strokePattern="solid", color="red") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)


# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls, (retp+pnls)/2)
colnames(wealthv) <- c("VTI", "AR_Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of contrarian strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
                  main="VTI Daily Contrarian Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)




volf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf)[, 2])
vols <- sqrt(HighFreq::run_var(retp, lambda=lambdas)[, 2])

# You should get the outputs:
tail(volf)
# [1] 0.008833582 0.008866551 0.008698291 0.008822120 0.008939642 0.008801316
tail(vols)
# [1] 0.008448128 0.008461511 0.008419252 0.008458098 0.008498641 0.008466526


# Create a function called sim_volma(), which simulates
# a moving average strategy using fast and slow trailing
# volatilities.
#
# The trading strategy is as follows:
# Buy one share of VTI if the fast volatility is below
# the slow volatility.
# Sell one share of VTI short if the fast volatility is
# above the slow volatility.
#
# The function sim_volma() should return two columns:
# the PnLs and the positions of the strategy.
#
# Adapt the code from the slide:
#   Simulation Function for the Dual EMA Crossover Strategy



# Not very interesting
############## test
# Summary: Rank the stocks according to their alpha.

## Run the setup code below

# Load S&P500 stock prices use your own directory
library(rutils)
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

## End of setup code.


# 1. (20pts)
# Extract the VTI returns from rutils::etfenv.
# You can use the function na.omit().

retm <- na.omit(rutils::etfenv$returns$VTI)

# You should get the output:
head(retm)
#                     VTI
# 2001-06-01  0.006944472
# 2001-06-04  0.004315932
# 2001-06-05  0.014536383
# 2001-06-06 -0.008525201
# 2001-06-07  0.005123837
# 2001-06-08 -0.008554372


# Extract the closing S&P500 stock prices and
# Calculate the their log returns.
# Subset the S&P500 stock prices so that their
# start date is the same as retm.
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
pricev <- pricev[index(retm), ]

# You should get the output:
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

# You should get the output:
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

varm <- drop(var(retm))
retmm <- mean(retm)

regd <- sapply(retp, function(rets) {
  betav <- cov(rets, retm)/varm
  alphav <- mean(rets) - betav*retmm
  c(alpha=alphav, beta=betav)
})  ##  end sapply
regd <- t(regd)

# You should get the outputs:
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

# You should get the outputs:
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



############## homework
# Summary: Perform cross-validation of an AR strategy 
# to find the best dimension reduction parameter.

# To-do:
# Simulate all the AR strategies using the coefficients from cross-validation.
# Combine their returns in a single strategy, and compare its performance to buy-and-hold.
# Take the average of the coefficients from cross-validation.

# Observations:

# Why doesn't dimension reduction improve the performance of the AR strategy?

# The only case where dimension reduction improves the performance of the 
# AR strategy is when the whole time is divided in half.

# Cross-validation doesn't work the same in finance because of the 
# non-stationarity of the returns, and because of the small sample sizes.

# For long in-sample intervals, the AR strategy performs well out-of-sample.

# For long in-sample intervals, dimension reduction doesn't improve the 
# performance of the AR strategy either.
# The AR strategy performs well out-of-sample, without dimension reduction.

# For short in-sample intervals, dimension reduction doesn't improve 
# the performance of the AR strategy, because the effect of overfitting is not large.
# Overfitting is largest in intervals with large volatility, because the AR strategy 
# can fit the noise in the returns, which is larger when volatility is larger.

# For short in-sample intervals, dimension reduction is worse than buy-and-hold, 
# because stocks have been rallying.

# For short in-sample intervals, dimension reduction is worse out-of-sample than 
# AR without dimension reduction. 

# Dimension reduction is worse over the whole time period than AR without dimension reduction, 
# because it's much worse in-sample, and only slightly better out-of-sample.

## Run the setup code below

# Below is code from the slides that you can use 
# to perform the cross-validation of the AR strategy.

# Extract the log SPY returns
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
datev <- zoo::index(retp)
nrows <- NROW(retp)

# Define the response and predictor matrices
respv <- retp
orderp <- 8 ##  9 predictors!!!
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
predm <- cbind(rep(1, nrows), predm)
# colnames(predm) <- c("phi0", paste0("lag", 1:orderp))

# Calculate the fitted AR coefficients
predinv <- MASS::ginv(predm)
coeff <- drop(predinv %*% respv)
names(coeff) <- colnames(predm)
# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- predm %*% coeff
# Calculate the AR strategy PnLs
pnls <- retp*fcasts
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c("VTI", "ARstrat")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))


# Calculate the SVD decomposition of the predictor matrix predm
svdec <- svd(predm)
# Calculate the inverse predm using dimension reduction with dimax=3
dimax <- 3
predinv <- svdec$v[, 1:dimax] %*% diag(1/svdec$d[1:dimax]) %*% t(svdec$u[, 1:dimax])



## End of setup code


# 1. (10pts)
# Calculate a vector of end points of 2-year intervals 
# using the function xts::endpoints().

endd <- xts::endpoints(retp, on="years", k=2)
endd <- c(0, nrows/2, nrows)

# You should get the output:
endd
# [1] 0  233  737 1244 1748 2248 2752 3256 3758 4263 4767 5269 5773
# [14] 6276 6779 7284 7785 8287 8298
NROW(endd)
# [1] 19
datev[endd]
#  [1] "1993-12-31" "1995-12-29" "1997-12-31" "1999-12-31" "2001-12-31" "2003-12-31"
#  [7] "2005-12-30" "2007-12-31" "2009-12-31" "2011-12-30" "2013-12-31" "2015-12-31"
# [13] "2017-12-29" "2019-12-31" "2021-12-31" "2023-12-29" "2025-12-31" "2026-01-16"


# 2. (30pts)
# Perform an sapply loop over the end points endd, 
# starting from the second end point, and ending with 
# the second to last end point, to avoid the last 
# stub period.
# For each end point: 
# 1. Use the previous end point as the start point, 
#   and subset the returns retp to be the in-sample 
#   returns.
# 2. Fit the AR coefficients using the in-sample returns.
# 3. Calculate the AR strategy PnLs for the whole time 
#   period using the fitted AR coefficients.
# 4. Calculate the strategy Sharpe ratios for the whole 
#   time period.
# 5. Return a vector of the fitted AR coefficients and
#   the Sharpe ratios.

sharpev <- sapply(2:(NROW(endd)-1), function(tday) {
  startp <- endd[tday-1] + 1
  endp <- endd[tday]
  intin <- -(startp:endp)
  retin <- retp[intin]
  samplout <- (startp:endp)
  ##  retout <- retp[samplout]
  ##  Fit the AR coefficients using the in-sample returns
  ##  orderp <- 8
  predmsub <- lapply(1:orderp, rutils::lagit, input=retin)
  predmsub <- rutils::do_call(cbind, predmsub)
  predmsub <- cbind(rep(1, NROW(predmsub)), predmsub)
  ##  colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
  predinv <- MASS::ginv(predmsub)
  coeff <- drop(predinv %*% retin)
  ##  names(coeff) <- colnames(predm)
  ##  Calculate the AR strategy PnLs for the whole time period
  fcasts <- predm %*% coeff
  pnls <- retp*fcasts
  ##  pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
  ##  Calculate the out-of-sample strategy Sharpe ratios
  pnls <- pnls[samplout]
  sharper <- sqrt(252)*mean(pnls)/sd(pnls)
  return(c(coeff, Sharpe=sharper))
}) ##  end sapply
sharpev <- t(sharpev)
sharpev[, "Sharpe"]
plot(density(sharpev[, "Sharpe"]), main="Density of AR Strategy Sharpe Ratios")
mean(sharpev[, "Sharpe"])
# [1] 0.2924459
sqrt(252)*mean(retp)/sd(retp)


# 3. (30pts)
# Perform the same loop as in p.2 but now apply dimension reduction 
# to the inverse of the in-sample predictor matrix predmsub.


sharpev <- sapply(2:(NROW(endd)-1), function(tday) {
  startp <- endd[tday-1] + 1
  endp <- endd[tday]
  intin <- -(startp:endp)
  retin <- retp[intin]
  samplout <- (startp:endp)
  ##  Fit the AR coefficients using the in-sample returns
  ##  orderp <- 8
  predmsub <- lapply(1:orderp, rutils::lagit, input=retin)
  predmsub <- rutils::do_call(cbind, predmsub)
  predmsub <- cbind(rep(1, NROW(predmsub)), predmsub)
  ##  colnames(predm) <- c("phi0", paste0("lag", 1:orderp))
  ##  Calculate the SVD decomposition of the predictor matrix predm
  svdec <- svd(predmsub)
  ##  Calculate the inverse predm using dimension reduction with dimax=3
  predinv <- svdec$v[, 1:dimax] %*% diag(1/svdec$d[1:dimax]) %*% t(svdec$u[, 1:dimax])
  ##  predinv <- MASS::ginv(predmsub)
  coeff <- drop(predinv %*% retin)
  ##  names(coeff) <- colnames(predm)
  ##  Calculate the AR strategy PnLs for the whole time period
  fcasts <- predm %*% coeff
  pnls <- retp*fcasts
  ##  pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
  ##  Calculate the out-of-sample strategy Sharpe ratios
  pnls <- pnls[samplout]
  sharper <- sqrt(252)*mean(pnls)/sd(pnls)
  return(c(coeff, Sharpe=sharper))
}) ##  end sapply
sharpev <- t(sharpev)
sharpev[, "Sharpe"]
plot(density(sharpev[, "Sharpe"]), main="Density of AR Strategy Sharpe Ratios")
mean(sharpev[, "Sharpe"])
sqrt(252)*mean(retp)/sd(retp)



############## homework
# Summary: Study how the dispersion of the Hampel z-scores
# depends on the level of volatility in the interval.
# Yes, zscores have higher dispersion on more volatile days.
# But so what?

ohlc <- HighFreq::SPY["T09:31:00/T15:59:00"]
# ohlc <- rutils::etfenv$VTI
nrows <- NROW(ohlc)
ohlc <- log(ohlc[, 1:4])
closep <- quantmod::Cl(ohlc)
# Calculate the zscores
lookb <- 11
medianv <- TTR::runMedian(closep, n=lookb)
medianv[1:lookb, ] <- 1
zscores <- (closep-medianv)
zscores[1:lookb, ] <- 0
madz <- TTR::runMAD(zscores, n=10*lookb)
madz[1:(10*lookb), ] <- 0
zscores <- ifelse(madz != 0, zscores/madz, 0)

# Calculate the log variance for SPY
varv <- xts::apply.daily(ohlc, HighFreq::calc_var_ohlc)
# For VTI
# varv <- sapply((2*lookb):nrows, function(ro_w) {
#   HighFreq::calc_var_ohlc(ohlc[(ro_w-lookb+1):ro_w, ], scalit=FALSE)
# })  ##  end sapply
# varv <- c(varv[1]+numeric(2*lookb-1), varv)
# x11(width=6, height=5)
# plot(varv)
# Plot the VTI volatility
volv <- sqrt(varv)
# volv <- xts::xts(sqrt(varv), index(ohlc))
dygraphs::dygraph(volv, main="VTI Volatility")


# Plot zscores versus volatility for VTI
plot(as.numeric(zscores) ~ as.numeric(volv))


# Calculate the dates with high volatility
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
hist(low_scores, xlim=c(quantile(low_scores, 0.05), quantile(low_scores, 0.95)), breaks=50, main=paste("low_scores", "lookb =", lookb))

range(high_scores)
high_scores <- high_scores[high_scores > quantile(high_scores, 0.05)]
high_scores <- high_scores[high_scores < quantile(high_scores, 0.95)]
x11(width=6, height=5)
hist(high_scores, xlim=c(quantile(high_scores, 0.05), quantile(high_scores, 0.95)), breaks=50, main=paste("high_scores", "lookb =", lookb))



############## homework
# Summary: Strategy using weekly and monthly stock returns.
# It's implemented in app_roll_portf9.R

library(HighFreq)

# Aggregate the VTI returns to monthly and run pacf()

closep <- quantmod::Cl(rutils::etfenv$VTI)
retp <- rutils::diffit(log(closep))
retadv <- rutils::lagit(retp, lagg=(-1))
retadv <- as.numeric(retadv)

# endd <- rutils::calc_endpoints(closep, interval="weeks")
# weekv <- closep[endd]
# weekv <- rutils::diffit(log(weekv))
weekv <- rutils::diffit(log(closep), lagg=5)
weekv <- as.numeric(weekv)
retadv <- rutils::lagit(weekv, lagg=(-1))
retadv <- as.numeric(retadv)
# endd <- rutils::calc_endpoints(closep, interval="months")
# monthv <- closep[endd]
# monthv <- rutils::diffit(log(monthv))
monthv <- rutils::diffit(log(closep), lagg=25)
monthv <- as.numeric(monthv)
retadv <- rutils::lagit(monthv, lagg=(-1))
retadv <- as.numeric(retadv)

# Objective function for simple optimization
objfun <- function(weightv) {
  ##  weightv <- c(weightv, 1-weightv)
  sum((retadv - (weightv*weekv + (1-weightv)*monthv))^2)
}  ##  end objfun
objfun(0.5)
foo <- optimize(f=objfun, interval=c(-10, 10))
unlist(foo)
weightv <- unlist(foo)[1]
posv <- sign(weightv*weekv + (1-weightv)*monthv)
posl <- rutils::lagit(posv, lagg=2)
pnls <- cumsum(posl*retp)
x11()
endd <- rutils::calc_endpoints(retp, "days")
plot.zoo(-pnls[endd], main="pnls", xlab=NA, ylab=NA)




############## homework
# Summary: Create a contrarian strategy using
# the Hampel filter.
# It's implemented in app_roll_portf10.R

library(rutils)


# 1. (10pts)
# Define a rolling look-back window and a half window:
# Use the %/% operator.

lookb <- 11
half_window <- lookb %/% 2

# Calculate the a time series of rolling z-scores
# (called zscores), using the Hampel filter code
# from the lecture slides.

prices <- quantmod::Cl(HighFreq::SPY)["T09:31:00/T15:59:00"]
retp <- rutils::diffit(log(prices))

medianv <- TTR::runMedian(prices, n=lookb)
medianv[1:lookb, ] <- 1
sum(is.na(medianv))
madv <- TTR::runMAD(prices, n=lookb)
madv[1:lookb, ] <- 1
sum(is.na(madv))
zscores <- ifelse(madv!=0, (prices-medianv)/madv, 0)
zscores[1:lookb, ] <- 0
madv <- zoo::na.locf(zscores)
sum(is.na(zscores))
madz <- TTR::runMAD(zscores, n=lookb)
madz[1:lookb, ] <- 0

# You should get the output:
tail(zscores)
mad(zscores)
range(zscores)
hist(zscores, breaks=30, xlim=c(-10, 10), freq=FALSE)

# Calculate the positions and pnls from z-scores
posv <- rep(NA_integer_, NROW(prices))
posv[1] <- 0
# threshz <- 3*mad(zscores)
# posv <- ifelse(zscores > threshz, -1, posv)
# posv <- ifelse(zscores < (-threshz), 1, posv)
posv <- ifelse(zscores > 2*madz, -1, posv)
posv <- ifelse(zscores < (-2*madz), 1, posv)
posv <- zoo::na.locf(posv)
posl <- rutils::lagit(posv, lagg=2)
pnls <- cumsum(posl*retp)
x11()
endd <- rutils::calc_endpoints(prices, "days")
plot.zoo(pnls[endd], main="pnls", xlab=NA, ylab=NA)



############## homework - Hurst exponents almost random
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
# Calculate the Hurst exponent using median of range ratios
calc_hurst_hilo <- function(highp, lowp, endd) {
  range_ratios <- sapply(seq_along(endd)[-1], function(it) {
    startp <- endd[it-1]
    endp <- endd[it]
    highp <- highp[startp:endp]
    lowp <- lowp[startp:endp]
    log((max(highp) - min(lowp))/mean(highp - lowp))/log(endp-startp)
  })  ##  end sapply
  median(na.omit(range_ratios))
}  ##  end calc_hurst_hilo

# Calculate the Hurst exponent using median of range ratios
calc_hursto <- function(highp, lowp, endd) {
  range_ratios <- sapply(seq_along(endd)[-1], function(it) {
    highp <- highp[endd[it-1]:endd[it]]
    lowp <- lowp[endd[it-1]:endd[it]]
    (max(highp) - min(lowp))/mean(highp - lowp)
  })  ##  end sapply
  log(median(na.omit(range_ratios)))/log(median(rutils::diffit(endd)))
}  ##  end calc_hursto

# Calculate the Hurst exponent from returns
calc_hurst_rets <- function(retp, endd) {
  retp <- na.omit(retp)
  retc <- cumsum(retp)
  range_ratios <- sapply(seq_along(endd)[-1], function(it) {
    startp <- endd[it-1]
    endp <- endd[it]
    retp <- retp[startp:endp]
    retc <- retc[startp:endp]
    log((max(retc) - min(retc))/sd(retp))/log(endp-startp)
  })  ##  end sapply
  median(na.omit(range_ratios))
}  ##  end calc_hurst_rets


ohlc <- log(HighFreq::SPY)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days"))

library(microbenchmark)
summary(microbenchmark(
  calc_hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")),
  calc_hursto=calc_hursto(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")),
  times=10))[, c(1, 4, 5)]  ##  end microbenchmark summary



# 2. (20pts)
# Calculate the a vector of monthly end points from
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
                      })  ##  end sapply
# rbind list into single xts or matrix
volat_hurst <- t(volat_hurst)

x11()
plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diffit(volat_hurst)
plot(foo)
pacf(foo[, 1])


# wippp
############## homework
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
                      })  ##  end sapply
# Transpose the matrix
volat_hurst <- t(volat_hurst)

plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diffit(volat_hurst)
plot(foo)
plot(cbind(foo[, 1], volat_hurst[, 2]))
pacf(foo[, 1])

regmod <- lm(volat_hurst[, 2] ~ volat_hurst[, 1])
regsum <- summary(regmod)
regsum$coefficients[2, 3]
plot(volat_hurst[, 2] ~ volat_hurst[, 1])
abline(regmod)

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
                          })  ##  end sapply
    ##  Transpose the matrix
    volat_hurst <- t(volat_hurst)
    ##  Scrub the data
    volat_hurst <- zoo::na.locf(volat_hurst)
    zoo::na.locf(volat_hurst, fromLast=TRUE)
  } else {
    c("Not enough years for Hurst\n")
    cbind(volatility=rep(1, NROW(endd)), hurst=rep(0.5, NROW(endd)))
  }
})  ##  end eapply

hurst_prof <- lapply(hurst_prof, function(volat_hurst) {
  ##  Scrub the data
  volat_hurst[is.infinite(volat_hurst)] <- NA
  volat_hurst <- zoo::na.locf(volat_hurst)
  zoo::na.locf(volat_hurst, fromLast=TRUE)
})  ##  end lapply

save(hurst_prof, file="/Users/jerzy/Develop/lecture_slides/data/sp500_perf.RData")

bar <- sapply(hurst_prof, function(x) sum(is.na(x) | is.infinite(x)))
max(bar)
which.max(bar)
hurst_prof[[names(which.max(bar))]]

get_tval <- function(x) {
  cat("NROW(x) = ", NROW(x), "\n")
  x <- na.omit(x)
  if ((NROW(x) > 3) & (sum(is.na(x))==0)) {
    regmod <- lm(x[, 2] ~ x[, 1])
    summary(regmod)$coefficients[2, 3]
  } else 1
}  ##  end get_tval
bar <- sapply(hurst_prof, get_tval)

bar <- sapply(hurst_prof, function(x) {
  ##  cat("dim(x) = ", dim(x), "\n")
  if (NROW(x) > 3) {
    regmod <- lm(x[, 2] ~ x[, 1])
    summary(regmod)$coefficients[2, 3]
  } else 0
})  ##  end sapply

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
})  ##  end sapply
retp <- prices[, names(tail(bar, 100))]
retp <- rutils::diffit(log(retp))
save(retp, file="/Users/jerzy/Develop/lecture_slides/data/sp100_rets.RData")

colv <- colnames(hurst_prof$AAPL)
bar <- lapply(hurst_prof, function(x) {
  x <- cbind((x[, 1]-min(x[, 1]))/(max(x[, 1]-min(x[, 1]))), (x[, 2]-min(x[, 2]))/(max(x[, 2])-min(x[, 2])))
  colnames(x) <- colv
  x
})  ##  end lapply
foo <- NULL
unlist(sapply(hurst_prof, function(x) {
  foo <<- rbind(foo, x)
  NULL
}))  ##  end sapply
plot(foo)


############## homework
# Summary: Simulate a contrarian strategy using
# the z-scores of the running regressions of the 
# daily SPY returns versus the VIX index returns.

# Conclusions:
# Maybe promising.  Performs well.  

## Run the setup code below

library(rutils)

# Get the VIX index prices
load(file="/Users/jerzy/Develop/data/vix/vix_data.RData")
vixohlc <- vixenv$vixohlc
datev <- zoo::index(vixohlc)

# Get the SPY returns
symboln <- "SPY"
rets <- na.omit(get(symboln, rutils::etfenv$returns))
rets <- rets[datev]
datev <- zoo::index(rets)
vixohlc <- vixohlc[datev]
vixp <- quantmod::Cl(vixohlc)
retvix <- rutils::diffit(vixp)/rutils::lagit(vixp)
retvix[1, ] <- 0.01
nrows <- NROW(rets) ##  number of rows

## End of setup code


# 1. (10pts)
# Plot a scatterplot of the SPY returns versus the VIX returns, 
# and calculate the correlation between them.

plot(coredata(retvix), coredata(rets), 
     xlab="VIX Returns", ylab="SPY Returns",
     main="Scatterplot of SPY Returns vs VIX Returns")
cor(retvix, rets)

# Calculate the regression residuals of SPY returns versus VIX returns
betac <- drop(cov(rets, retvix)/var(retvix))
residv <- rets - betac*retvix
# Dygraph the regression residuals
dygraph(residv, main="Residuals") %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Plot a scatterplot of the SPY returns versus the lagged residuals, 
# and calculate the correlation between them.
residv <- rutils::lagit(residv, lagg=1)
cor(residv, rets)
regmod <- lm(rets ~ residv)
coefreg <- summary(regmod)$coeff
plot(coredata(residv), coredata(rets), 
     xlab="Lagged Residuals", ylab="SPY Returns",
     main="Scatterplot of SPY Returns vs Lagged Residuals")
abline(a=coefreg["(Intercept)", "Estimate"], 
       b=coefreg["residv", "Estimate"], 
       col="red", lwd=2)
text(x=0.0, y=0.8*max(coredata(rets)), 
     paste("Lagged Residuals Coefficient =", round(coefreg["residv", "Estimate"], 4), 
           "\n t-value =", round(coefreg["residv", "t value"], 2)))

# Calculate the positions as the sign of the lagged residuals
posv <- sign(residv)
pnls <- -rets*posv

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(rets, pnls, 0.5*(rets+pnls))
colnames(wealthv) <- c(symboln, "Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Volatility Trending Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name=colnames(wealthv)[3], strokeWidth=2) %>%
  dyLegend(show="always", width=500)


# Regression of SPY returns versus VXX returns
rets <- na.omit(rutils::etfenv$returns[, c("SPY", "VXX")])
datev <- zoo::index(rets)
betac <- drop(cov(rets$SPY, rets$VXX)/var(rets$VXX))
residv <- rets$SPY - betac*rets$VXX
posv <- sign(residv)
posv <- rutils::lagit(posv, lagg=1)
pnls <- -rets$SPY*posv
wealthv <- cbind(rets$SPY, pnls, 0.5*(rets$SPY + pnls))
colnames(wealthv) <- c(symboln, "Strategy", "Combined")


############## homework
# Summary: Simulate a contrarian strategy using the 
# residuals of SPY versus the time index.

# Get the SPY returns
symboln <- "SPY"
rets <- na.omit(get(symboln, rutils::etfenv$returns))
rets <- rets[datev]
datev <- zoo::index(rets)

# Regression of SPY returns versus the time index
indeks <- xts::.index(rets)
reti <- rutils::diffit(indeks)
betac <- drop(cov(rets, reti)/var(reti))
residv <- rets - betac*reti
posv <- sign(residv)
posv <- rutils::lagit(posv, lagg=1)
pnls <- -rets*posv


############## homework
# Summary: Simulate a trend following strategy using 
# the slope of the EMA volatility.

# Conclusions:
# Not promising.  Doesn't perform well.  
# strat_vol_trend.R

## Run the setup code below

library(rutils)

# Get the SPY returns
symboln <- "SPY"
rets <- na.omit(get(symboln, rutils::etfenv$returns))
datev <- zoo::index(rets) ##  Dates vector
nrows <- NROW(rets) ##  number of rows
nstocks <- NCOL(rets) ##  number of stocks

## End of setup code


# 1. (10pts)
# Calculate the EMA volatility of the stock returns
# using the function HighFreq::run_var().

lambdaf <- 0.99

# Calculate the EMA volatility of SPY
vols <- HighFreq::run_var(matrix(rets), lambdaf=lambdaf)
vols <- sqrt(vols[, 2])

# Calculate the positions as the difference of the volatility of SPY
posv <- sign(rutils::diffit(vols))
posv <- rutils::lagit(posv, lagg=1)
pnls <- rets*posv
# pnls <- pnls*sd(retm[retm<0])/sd(pnls[pnls<0])
pnls <- xts::xts(pnls, order.by=datev)

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(rets, pnls, 0.5*(rets+pnls))
colnames(wealthv) <- c(symboln, "Strategy", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main="Volatility Trending Strategy") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name=colnames(wealthv)[3], strokeWidth=2) %>%
  dyLegend(show="always", width=500)



############## homework
# Summary: Calculate the median of EMA stock volatilities.

# Conclusions:
# The standard deviation of the median of the EMA stock 
# volatilities is only slightly lower than the EMA 
# market volatility.  So it's not a significant improvement.
# 
# The interquartile range has a similar standard deviation 
# as the median.

## Run the setup code below

library(rutils)

# Load daily S&P500 percentage stock returns
# Download the file from the share drive:
# https://drive.google.com/drive/folders/1jNtoNCL8mLJyda0wu1pLCnjvqIPzd_WL
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
# Calculate the stock volatilities
vols <- sapply(retstock, sd, na.rm=TRUE)
volmed <- median(vols)
# Select all the stocks
retp <- retstock
# Select stocks with volatilities less than the median volatility
retp <- retstock[, vols < volmed]

datev <- zoo::index(retp) ##  Dates vector
nrows <- NROW(retp) ##  number of rows
nstocks <- NCOL(retp) ##  number of stocks

# Calculate the market returns as the mean of the stock returns
retm <- rowMeans(retp, na.rm=TRUE)
# Calculate the median of the returns across stocks for each day
retm <- matrixStats::rowMedians(retp, na.rm=TRUE)

## End of setup code


# 1. (10pts)
# Calculate the EMA volatility of the stock returns
# using the function HighFreq::run_var().

lambdaf <- 0.99

# Calculate the market EMA volatility of retm
volmark <- HighFreq::run_var(matrix(retm), lambdaf=lambdaf)
volmark <- sqrt(volmark[, 2])
# Calculate the EMA volatilities of the stock returns
vols <- HighFreq::run_var(retp, lambdaf=lambdaf)
vols <- sqrt(vols[, (nstocks+1):(2*nstocks)])
# Calculate the median of the EMA volatilities across stocks for each day
volmed <- matrixStats::rowMedians(vols, na.rm=TRUE)
# volmed <- xts::xts(volmed, order.by=datev)
# dygraph(volmed, main="Median of EMA Stock Volatilities") %>%
#   dyOptions(colors="blue", strokeWidth=1) %>%
#   dyLegend(show="always", width=300)

# Scatterplot of volmed and volmark
plot(volmed, volmark, xlab="Median of EMA Stock Volatilities", ylab="EMA Market Volatility",
     main="Scatterplot of Median of EMA Stock Volatilities vs EMA Market Volatility")

# Dygraph of volmed and volmark
datav <- cbind(volmed, volmark)
datav <- xts::xts(datav, order.by=datev)
colnames(datav) <- c("MedianVol", "MarketVol")
dygraph(datav, main="Median of EMA Stock Volatilities and Market Volatility") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# The median volatility has lower standard deviation than the market volatility
diffdata <- rutils::diffit(datav)
# sapply(diffdata, function(x) sd(x)/diff(range(x)))
sapply(diffdata, sd) / sapply(datav, function(x) diff(range(x)))

# Calculate the first and third quartiles of the EMA volatilities across stocks for each day
volq1 <- matrixStats::rowQuantiles(vols, probs=0.25, na.rm=TRUE)
volq3 <- matrixStats::rowQuantiles(vols, probs=0.75, na.rm=TRUE)
datav <- xts::xts(cbind(volq3 - volq1), order.by=datev)
dygraph(datav, main="Interquartile Range of EMA Stock Volatilities") %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=300)



# How well does it perform?
############## homework
# Summary: Simulate a volatility timing strategy
# using aggregated returns and volatilities.

# Calculate SPY percentage returns
symboln <- "SPY"
pricev <- log(na.omit(get(symboln, rutils::etfenv$prices)))
datev <- zoo::index(pricev)
retp <- rutils::diffit(pricev)
# Calculate the EMA volatility
volma <- sqrt(HighFreq::run_var(retp, lambda=0.9)[, 2])
volma <- rutils::diffit(volma)
datav <- cbind(retp, volma)
colnames(datav)[2] <- "Volatility"


colv <- colnames(datav)
dygraphs::dygraph(datav["2020-02/2020-05"], 
                  main="SPY Returns and Volatility") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# Calculate monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)
# Calculate monthly returns and volatilities
retvol <- sapply(2:npts, function(tday) {
  retis <- retp[(endd[tday-1]+1):endd[tday]]
  return(c(ret=sum(retis), vol=sd(retis)))
})  ##  end sapply
retvol <- t(retvol)

retvol <- cbind(retp, volma)

# Calculate the rolling sums of the returns and the squared returns

retsq <- retp^2
lookb <- 5
retm <- HighFreq::roll_sum(retp, lookb=lookb)
retd <- rutils::diffit(retm, lagg=lookb)
retsqm <- sqrt(HighFreq::roll_sum(retsq, lookb=lookb))
retsqd <- rutils::diffit(retsqm, lagg=lookb)
retsql <- rutils::lagit(retsqm, lagg=lookb)
retvol <- cbind(retd, retsqd)
# retvol <- cbind(retd, retsql)
colnames(retvol) <- c("Returns", "Volatility")

# Perform regression of returns versus the volatilities
formobj <- as.formula(paste(colnames(retvol), collapse=" ~ "))
regmod <- lm(formobj, data=as.data.frame(retvol))
summary(regmod)$coefficients
# Plot the returns versus the volatilities with regression line
plot(formula=formobj, data=retvol, 
     main="Monthly Returns Versus Volatility",
     pch=20, cex=0.5, col="blue",
     xlab="Volatility", ylab="Returns")
abline(regmod, col="red", lwd=2)
text(x=0.03, y=0.05, cex=1.2, col="blue", 
     labels=paste("t-value =", round(summary(regmod)$coefficients[2, 3], 2)))

retvol <- xts::xts(retvol, order.by=datev)
retvol <- cbind(pricev, retvol[, "Volatility"])
colnames(retvol)[2] <- "Volatility"
colv <- colnames(retvol)
dygraphs::dygraph(retvol["2020-02/2020-05"], 
                  main="SPY Returns and Volatility") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=2, col="red") %>%
  dyLegend(show="always", width=300)

# The positions are the inverse of the volatility
# posv <- rutils::lagit(retvol[, "Volatility", drop=FALSE], lagg=lookb)
posv <- retvol[, "Volatility", drop=FALSE]
posv <- HighFreq::roll_sum(posv, lookb=lookb)
# posv <- HighFreq::roll_scale(posv, lookb=lookb, center=FALSE)
# posv[1:lookb, ] <- 0
posv <- ifelse(posv > sd(posv), 1, -1)
posv <- rutils::lagit(posv)
# Calculate the PnLs minus the transaction costs
pnls <- -retp*posv
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
# pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])
# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c(symboln, "Strategy")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of the strategy
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main="Inverse Volatility Timing Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)




# Adapted from the volatility crossover strategy.
############## homework
# Summary: Simulate a crossover strategy using 
# fast and slow EMA volatilities.
# Add a term to the slow volatility.

# Idea:
# Introduce a factor to multiply the slow volatility.
# Make the slow lambda very close to 1.
# This will make it a generalization of the 
# volatility threshold strategy.
# Test it out-of-sample.


## Run the setup code below

library(rutils)
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
datev <- zoo::index(retp)
nrows <- NROW(retp)
lambdaf <- 0.8 ##  Fast lambda
lambdas <- 0.9 ##  Slow lambda

## End of setup code


# 1. (20pts) 
# Calculate the fast (volf) and slow (vols) trailing 
# volatilities of retp using lambdaf and lambdas.
# You can use the functions HighFreq::run_var()
# and sqrt(). 

volf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf)[, 2])
vols <- sqrt(HighFreq::run_var(retp, lambda=lambdas)[, 2])

# You should get the outputs:
tail(volf)
# [1] 0.008833582 0.008866551 0.008698291 0.008822120 0.008939642 0.008801316
tail(vols)
# [1] 0.008448128 0.008461511 0.008419252 0.008458098 0.008498641 0.008466526


# Create a function called sim_volcross(), which simulates 
# a volatility crossover strategy using fast and slow EMA 
# volatilities.
# 
# The trading strategy is as follows:
# Buy one dollar of VTI if the fast volatility is below 
# the slow volatility.
# Sell one dollar of VTI short if the fast volatility is 
# above the slow volatility.
# 
# The function sim_volcross() should return two columns:
# the PnLs and the positions of the strategy.
# 
# Hint: Adapt the code from the slide: 
#   Simulation Function for the Dual EMA Crossover Strategy

sim_volcross <- function(lambdaf=0.8, lambdas=0.9, bidask=0.0) {
  if (lambdaf >= lambdas) return(NA)
  ##  Calculate the volatilities
  volf <- sqrt(HighFreq::run_var(retp, lambda=lambdaf)[, 2])
  vols <- sqrt(HighFreq::run_var(retp, lambda=lambdas)[, 2])
  ##  Calculate the positions, either: -1, 0, or 1
  posv <- sign(vols - volf)
  ##  Lag the positions to trade on next day
  posv <- rutils::lagit(posv, lagg=1)
  ##  Calculate the PnLs of strategy
  pnls <- retp*posv
  costs <- 0.5*bidask*abs(rutils::diffit(posv))
  pnls <- (pnls - costs)
  datav <- cbind(pnls, posv)
  colnames(datav) <- c("PnL", "Position")
  return(datav)
}  ##  end sim_volcross


# You should get the output:

pnls <- sim_volcross(lambdaf=lambdaf, lambdas=lambdas)
head(pnls)
#                     PnL Position
# 2001-06-01  0.000000000        0
# 2001-06-04  0.000000000        0
# 2001-06-05  0.014536383        1
# 2001-06-06  0.008525201       -1
# 2001-06-07 -0.005123837       -1
# 2001-06-08  0.008554372       -1

tail(pnls)
#                      PnL Position
# 2024-12-23 -0.0056482303       -1
# 2024-12-24 -0.0105563303       -1
# 2024-12-26 -0.0005719958       -1
# 2024-12-27  0.0108902591       -1
# 2024-12-30  0.0111133152       -1
# 2024-12-31  0.0034789832       -1


# 2. (20pts) 
# Create a function called calc_sharpe(), which calls 
# sim_volcross() to simulate a moving average strategy 
# using trailing volatilities, and calculates its 
# Sharpe ratio.
# Adapt the code from the slide: 
#   Dual EMA Strategy Performance Matrix

# Calculate the Sharpe ratio of moving average strategy 
# using trailing volatilities.

calc_sharpe <- function(lambdaf, lambdas, bidask=0.0) {
  if (lambdaf >= lambdas) return(NA)
  pnls <- sim_volcross(lambdaf=lambdaf, lambdas=lambdas, bidask=bidask)[, "PnL"]
  sqrt(252)*mean(pnls)/sd(pnls)
}  ##  end calc_sharpe

# You should get the output:
sharper <- calc_sharpe(lambdaf=lambdaf, lambdas=lambdas)
sharper
# [1] 0.5780439


# Create two vectors of lambdas (run this):
lambdafv <- seq(from=0.98, to=0.999, by=0.001)
lambdasv <- seq(from=0.98, to=0.999, by=0.001)

# Calculate a matrix of Sharpe ratios for the vectors 
# lambdafv and lambdasv.
# You can perform two sapply() loops over the lambda 
# vectors.

sharpem <- sapply(lambdasv, function(lambdas) {
  sapply(lambdafv, function(lambdaf) {
    calc_sharpe(lambdaf=lambdaf, lambdas=lambdas)
  })  ##  end sapply
})  ##  end sapply
colnames(sharpem) <- lambdasv
rownames(sharpem) <- lambdafv


# You should get the output:
round(sharpem, 2)
#       0.98 0.981 0.982 0.983 0.984 0.985 0.986 0.987 0.988 0.989 0.99 0.991 0.992 0.993 0.994 0.995 0.996 0.997 0.998 0.999
# 0.98    NA  0.27  0.25  0.21  0.24  0.29  0.31  0.36  0.39  0.44 0.46  0.49  0.38  0.40  0.51  0.54  0.44  0.42  0.52  0.53
# 0.981   NA    NA  0.23  0.22  0.28  0.30  0.36  0.38  0.39  0.49 0.45  0.43  0.36  0.44  0.52  0.58  0.43  0.46  0.56  0.53
# 0.982   NA    NA    NA  0.27  0.27  0.34  0.39  0.41  0.43  0.47 0.44  0.37  0.39  0.47  0.47  0.53  0.38  0.48  0.54  0.51
# 0.983   NA    NA    NA    NA  0.33  0.38  0.39  0.40  0.46  0.46 0.40  0.37  0.41  0.44  0.47  0.52  0.37  0.42  0.57  0.51
# 0.984   NA    NA    NA    NA    NA  0.41  0.40  0.46  0.47  0.42 0.39  0.38  0.43  0.42  0.45  0.49  0.37  0.45  0.52  0.51
# 0.985   NA    NA    NA    NA    NA    NA  0.44  0.44  0.44  0.39 0.38  0.41  0.42  0.48  0.49  0.48  0.40  0.46  0.51  0.51
# 0.986   NA    NA    NA    NA    NA    NA    NA  0.42  0.39  0.43 0.41  0.39  0.41  0.41  0.49  0.52  0.50  0.45  0.48  0.52
# 0.987   NA    NA    NA    NA    NA    NA    NA    NA  0.41  0.38 0.43  0.42  0.40  0.45  0.52  0.45  0.44  0.47  0.49  0.52
# 0.988   NA    NA    NA    NA    NA    NA    NA    NA    NA  0.40 0.40  0.41  0.39  0.46  0.54  0.47  0.42  0.52  0.48  0.51
# 0.989   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA 0.36  0.38  0.40  0.47  0.56  0.53  0.44  0.44  0.45  0.50
# 0.99    NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA  0.40  0.46  0.54  0.47  0.47  0.46  0.42  0.43  0.42
# 0.991   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA  0.54  0.50  0.49  0.40  0.41  0.41  0.42  0.38
# 0.992   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA  0.50  0.45  0.41  0.38  0.39  0.44  0.36
# 0.993   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA  0.36  0.41  0.43  0.36  0.42  0.37
# 0.994   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA  0.38  0.42  0.41  0.40  0.31
# 0.995   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA  0.36  0.40  0.29  0.31
# 0.996   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA  0.34  0.32  0.26
# 0.997   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA  0.33  0.20
# 0.998   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA    NA  0.26
# 0.999   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA   NA    NA    NA    NA    NA    NA    NA    NA    NA    NA


# 3. (20pts) 
# Calculate the PnLs for the optimal strategy.
# Adapt the code from the slide: 
#   Optimal Dual EMA Strategy

whichv <- which(sharpem == max(sharpem, na.rm=TRUE), arr.ind=TRUE)
lambdaf <- lambdafv[whichv[1]]
lambdas <- lambdasv[whichv[2]]
stratopt <- sim_volcross(lambdaf=lambdaf, lambdas=lambdas)
pnls <- stratopt[, "PnL"]
wealthv <- cbind(retp, pnls)
colnames(wealthv)[2] <- "MA Strat"
# Calculate the Sharpe and Sortino ratios
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
sharper <- round(sharper, 3)

# You should get the outputs:
# The crossover strategy has a negative correlation to VTI
cor(wealthv)[1, 2]
# [1] -0.2448652
lambdaf
# [1] 0.981
lambdas
# [1] 0.995
sharper
#   VTI MA Strat
# 0.452    0.578

# Plot the wealth of the optimal strategy

pnls <- sim_volcross(lambdaf=lambdaf, lambdas=lambdas, bidask=0)
tail(pnls)
pnls <- pnls[, "PnL"]

# Scale the PnL volatility to that of SPY
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls, 0.5*(retp + pnls))
colnames(wealthv) <- c(symboln, "Strategy", "Combined")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot dygraph of the strategy with shading

endw <- rutils::calc_endpoints(wealthv, interval="weeks")
captiont <- paste("Volatility Crossover Strategy for", symboln)
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



# Plot a dygraph of the optimal strategy with shading.
# Don't use end dates.

# Create colors for background shading
posv <- stratopt[, "Position"]
crossd <- (rutils::diffit(posv) != 0)
shadev <- posv[crossd]
crossd <- c(zoo::index(shadev), end(posv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot Optimal Dual EMA strategy
captiont <- paste("Optimal MA Volatility Strategy, Sharpe", "\n",
                  paste(paste(names(sharper), sharper, sep="="), collapse=", "))
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=captiont) %>% 
  dyOptions(colors=c("blue", "red"), strokeWidth=2)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  ##  end for
# Plot the dygraph object
dyplot

# Your plot should be similar to ewvol_optim.png

# Plot a dygraph of the optimal strategy combined with VTI.
# Use end dates.

wealthv <- cbind(wealthv, (retp+pnls)/2)
colnames(wealthv)[3] <- "Combined"
sharper <- sqrt(252)*sapply(wealthv, function (x) mean(x)/sd(x))
sharper <- round(sharper, 3)
captiont <- paste("MA Volatility Strategy With VTI, Sharpe", "\n",
                  paste(paste(names(sharper), sharper, sep="="), collapse=", "))

# Plot a dygraph of stock index and momentum strategy
# Plot dygraph of the strategy with shading

endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=2) %>%
  dySeries(name="Combined", strokeWidth=3) %>%
  dyLegend(show="always", width=300) %>%
  dyCSS("/Users/jerzy/Develop/lecture_slides/css/dygraph.css")

# Your plot should be similar to ewvol_combo.png

# CSS is for smaller title font - Don't need CSS
# Code in the file dygraph.css
# .dygraph-title {
#   font-weight: bold;
#   font-size: 16px;
# }



# Needs more work?  Not promising.
############## homework
# Summary: Simulate a volatility timing strategy
# using the ratio of two EMA volatilities.
# Conclusion:
# The performance isn't any better than using a single
# EMA volatility.
# Idea:
# Add a term to the slow volatility.
# Introduce a factor to multiply the slow volatility.
# Adapt the volatility crossover strategy.

## Run the setup code below

library(rutils)

# The bid-ask spread for the most liquid ETFs
bidask <- 0.0001
# Calculate the SPY daily percentage returns
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
# Calculate the EMA volatility
volv <- HighFreq::run_var(retp, lambda=0.5)
volv <- sqrt(volv[, 2])

## End of setup code


# 1. (10pts)
# Calculate the positions as the inverse of the lagged 
# EMA volatility.
# You can use the function rutils::lagit().
# Set the first value of volv to 1, to avoid division 
# by zero.

volv <- rutils::lagit(volv)
volv[1] <- 1
posv <- 1/volv

# You should get the following outputs:
NROW(posv)
# [1] 8298
head(posv)
# [1] 1.0000 201.1293 304.0789 328.5427 473.3764 329.8407
tail(posv)
# [1] 568.8807 345.5384 605.4911 473.4110 403.9418 397.2903


# Calculate the PnLs minus the transaction costs.
# Scale the PnL volatility to that of SPY.

pnls <- retp*posv
costv <- 0.5*bidask*abs(rutils::diffit(posv))
pnls <- (pnls - costv)
pnls <- pnls*sd(retp[retp<0])/sd(pnls[pnls<0])

# You should get the following outputs:
head(pnls)
#                     SPY
# 1993-02-01  2.453129e-05
# 1993-02-02  1.437974e-03
# 1993-02-03  1.105102e-02
# 1993-02-04  4.744005e-03
# 1993-02-05 -1.165146e-03
# 1993-02-08 -2.484138e-05
tail(pnls)
#                     SPY
# 2026-01-09  0.012930946
# 2026-01-12  0.001838174
# 2026-01-13 -0.004239854
# 2026-01-14 -0.008096925
# 2026-01-15  0.003790361
# 2026-01-16 -0.001153826

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
colnames(wealthv) <- c(symboln, "Strategy")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get the following outputs:
#               SPY  Strategy
# Sharpe  0.5463022 0.6791736
# Sortino 0.6809656 0.8291622


# Plot dygraph of the strategy.
# Your plot should be similar to strat_vol_timing_spy.png

endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main="Volatility Timing Strategy") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)



# 2. (20pts)
# Calculate the Sharpe ratios of the strategy for 
# the vector of lambda decay parameters lambdav.
# Use parallel computing with the package parallel.

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
lambdafv <- seq(0.1, 0.9, 0.1)
lambdasv <- seq(0.1, 0.9, 0.1)

sharpem <- mclapply(lambdasv, function(lambdas) {
  vols <- HighFreq::run_var(retp, lambda=lambdas)
  vols <- sqrt(vols[, 2])
  lapply(lambdafv, function(lambdaf) {
    if (lambdaf <= lambdas) return(NA)
    ##  Calculate the EMA volatility
    volf <- HighFreq::run_var(retp, lambda=lambdaf)
    volf <- sqrt(volf[, 2])
    ##  Calculate the positions and PnLs
    posv <- vols/volf
    posv <- rutils::lagit(posv)
    pnls <- retp*posv
    costv <- 0.5*bidask*abs(rutils::diffit(posv))
    pnls <- (pnls - costv)
    return(sqrt(252)*mean(pnls)/sd(pnls))
  }) ## end lapply
}, mc.cores=ncores)  ## end mclapply

sharpem <- do.call(rbind, sharpem)
whichmax <- which(sharpem == max(sharpem, ), arr.ind=TRUE)

lambdas <- lambdasv[8]
lambdaf <- lambdafv[7]


# You should get the following outputs:
round(sharpem, 4)
# [1] 0.5959 0.6973 0.7080 0.6958 0.6792 0.6566 0.6386 0.6363 0.6545

# Calculate the largest Sharpe ratio and its lambda 
# decay parameter.
# You can use the functions max(), which.max().

max(pnlv)
lambdaf <- lambdav[which.max(pnlv)]

# You should get the following outputs:
# [1] 0.7080153
# [1] 0.3

# Plot the Sharpe ratios versus lambda decay parameters.
# Your plot should be similar to strat_vol_timing_profile_spy.png

plot(lambdav, pnlv, type="b",
     main="Volatility Timing Strategy",
     xlab="Lambda Decay Factor", ylab="Sharpe Ratio")



############## homework
# Summary: Simulate a momentum strategy using the
# idiosyncratic returns of sector ETFs named X*.

# Doesn't perform well.  Select only some of the ETFs.
# Adapt code from strat_momentum_idiosyncratic.R

## Run the setup code below

library(rutils)

# Select the ETF symbols starting with X:
symbolv <- rutils::etfenv$symbolv
symbolv <- symbolv[grep("^X", symbolv)]
# Add SPY and TLT to the list of symbols
symbolv <- c("SPY", "TLT", symbolv)
nstocks <- NROW(symbolv)
# Calculate the percentage stock returns
retp <- na.omit(rutils::etfenv$returns[, symbolv])
# Extract SPY from the returns matrix
retm <- retp$SPY
# Drop SPY from the returns matrix
retp <- retp[, -1]
datev <- zoo::index(retm)

# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
npts <- NROW(endd)


sim_momw <- function(retp, objfun, lookb=12, rebalf="months", volt=0.01, 
                        bidask=0.0, endd=rutils::calc_endpoints(retp, interval=rebalf), ...) {
  ##  Perform loop over end points
  npts <- NROW(endd)
  datev <- zoo::index(retp)
  pnls <- lapply(1:(npts-1), function(tday) {
    ##  Select the in-sample returns
    startp <- endd[max(1, tday-lookb)]
    retis <- retp[startp:endd[tday], ]
    intos <- (endd[tday]+1):endd[tday+1]
    retos <- retp[intos]
    datos <- zoo::index(retos)
    if (NROW(retis) < 4) return(xts::xts(numeric(NROW(intos)), datos))
    ##  Calculate weights proportional to the performance
    perfstat <- sapply(retis, objfun)
    weightv <- perfstat
    ##  Calculate the in-sample portfolio returns
    pnlis <- HighFreq::mult_mat(weightv, retis)
    pnlis <- rowMeans(pnlis, na.rm=TRUE)
    ##  Scale weights so in-sample pnl volatility is same as target
    weightv <- weightv*volt/sd(pnlis)
    ##  Calculate the out-of-sample momentum returns
    pnlos <- HighFreq::mult_mat(weightv, retos)
    pnlos <- rowMeans(pnlos, na.rm=TRUE)
    return(xts::xts(pnlos, datos))
  })  ##  end lapply
  return(rutils::do_call(rbind, pnls))
}  ##  end sim_momw

## End of setup code


# New stuff

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1

# Perform backtests for vector of look-back intervals
lookbv <- seq(50, 250, by=10)
pnll <- mclapply(lookbv, sim_momw, retp=retid, endd=endd, objfun=objfun, mc.cores=ncores)
sharpev <- sqrt(252)*sapply(pnll, function(pnl) mean(pnl)/sd(pnl))
# Plot Sharpe ratios of momentum strategies
plot(x=lookbv, y=sharpev, t="b",
     main="Momentum Sharpe as Function of Look-back Interval",
     xlab=paste0("look-back (", rebalf, ")"), ylab="Sharpe")

pnls <- pnll[[which.max(sharpev)]]
lookb <- lookbv[which.max(sharpev)]

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retm, pnls, 0.5*(retm+pnls))
wealthv[is.na(wealthv)] <- 0
colnames(wealthv) <- c("SPY", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# colnames(wealthv) <- paste(colnames(wealthv), round(sharper[1, ], 3), sep=" = ")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Optimal Momentum for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name=colnames(wealthv)[3], strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# End new stuff


# Momentum strategy using PCA of ETF returns - doesn't perform well
# 1. (40pts)
# Calculate the principal components of stock returns
cormat <- cor(retp)
eigend <- eigen(cormat)
retpca <- retp %*% eigend$vectors
retpca <- xts::xts(retpca, order.by=datev)
colnames(retpca) <- paste0("PC", 1:nstocks)
# Calculate a vector of weekly end points
endd <- rutils::calc_endpoints(retp, interval="weeks")
npts <- NROW(endd)
colorv <- colorRampPalette(c("red", "blue"))(5)
dygraph(cumsum(retpca[endd, 1:5]),
        main="Cumulative Returns of First Five Principal Components") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=300)

# Calculate the sum of the partial autocorrelations of PCAs
pacsum <- sapply(retpca, function(x) {
  pacff <- stats::pacf(x, plot=FALSE, lag.max=20)
  return(sum(pacff$acf))
}) ## end sapply
plot(pacsum, type="b",
     main="Sum of Partial Autocorrelations of First 30 PCAs",
     xlab="PCA Index", ylab="Sum of Partial Autocorrelations")


# Works well
# 2. (40pts)
# Calculate the rolling idiosyncratic returns of the ETFs,
# and use them to simulate a momentum strategy.

# Perform a parallel loop over the end points,
# starting from the third end point.

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
lookb <- 190 ##  Look-back interval in weeks
volt <- 0.01 ##  Volatility target for the momentum strategy

# Simulate the momentum strategy using the idiosyncratic returns of the ETFs.
# Calculate the in-sample stock betas since inception - but doesn't improve much
sim_mom <- function(lookb) {
  cat("lookb =", lookb, "\n")
  
  pnls <- mclapply(1:(npts-1), function(tday) {
    ##  cat("Processing end point", tday, "of", npts, "\n")
    ##  Select the since-inception dates
    endp <- endd[tday]
    incept <- (1:endp)
    ##  Select the in-sample dates
    startp <- endd[max(1, tday-lookb)]
    intin <- (startp:endp)
    ##  Select the out-of-sample returns
    intos <- (endd[tday]+1):endd[tday+1]
    retos <- retp[intos]
    datos <- zoo::index(retos)
    retmos <- retm[intos]
    ##  If the in-sample interval is too short, return a vector of zeros for the out-of-sample PnLs
    if (NROW(intin) < 11) {
      outp <- matrix(numeric(2*NROW(intos)), ncol=2)
      return(xts(outp, order.by=datos))
    } ##  end if
    ##  Select the since-inception returns
    retinc <- retp[incept]
    retminc <- retm[incept]
    varminc <- drop(var(retminc))
    retmminc <- mean(retminc)
    ##  Select the in-sample returns
    retis <- retp[intin]
    retmis <- retm[intin]
    varm <- drop(var(retmis))
    retmm <- mean(retmis)
    ##  Calculate the in-sample stock volatilities, betas, and alphas
    riskret <- lapply(colnames(retp), function(symb) {
      stdev <- sd(retis[, symb])
      betac <- drop(cov(retinc[, symb], retminc)/varminc)
      resid <- retis[, symb] - betac*retmis
      alphac <- mean(retis[, symb]) - betac*retmm
      c(alpha=alphac, beta=betac, stdev=stdev, ivar=var(resid))
    })  ## end lapply
    riskret <- do.call(rbind, riskret)
    rownames(riskret) <- colnames(retp)
    ##  Calculate the momentum weights
    alphav <- riskret[, "alpha"]
    betav <- riskret[, "beta"]
    ivarv <- riskret[, "ivar"]
    weightv <- alphav/ivarv
    weightv[is.na(weightv)] <- 0
    ##  Calculate the in-sample idiosyncratic returns
    residis <- lapply(colnames(retp), function(symb) {
      return(retis[, symb] - retmis*betav[symb])
    }) ## end lapply
    residis <- do.call(cbind, residis)
    ##  Scale the weights to match the target volatility
    weightv <- weightv*volt/sd(residis %*% weightv)  
    ##  Calculate the out-of-sample idiosyncratic returns
    residos <- lapply(colnames(retp), function(symb) {
      retos[, symb] - retmos*betav[symb]
    }) ## end lapply
    residos <- do.call(cbind, residos)
    residos[is.na(residos)] <- 0
    ##  Calculate the out-of-sample pnls of low and high alpha stocks
    pnlos <- residos %*% weightv
    ##  Calculate the equal-weighted portfolio pnls
    retew <- rowMeans(retos, na.rm=TRUE)
    ##  pnlm <- cbind(retew, pnlos*sd(retew)/sd(pnlos))
    pnlm <- cbind(retew, pnlos)
    pnlm <- xts(pnlm, order.by=datos)
    return(pnlm)
    
  }, mc.cores=ncores)  ## end mclapply
  
  # Combine the PnLs into a single xts series
  pnls <- rutils::do_call(rbind, pnls)
  colnames(pnls) <- c("EqualWeight", "Momentum")
  pnls$Momentum <- pnls$Momentum*sd(retm)/sd(pnls$Momentum)
  return(pnls)
  
} ##  end sim_mom

lookbv <- seq(150, 250, by=10)
pnll <- lapply(lookbv, sim_mom)

sharpev <- sapply(pnll, function(pnls) {
  pnls <- pnls$Momentum
  sqrt(252)*mean(pnls)/sd(pnls)
}) ##  end sapply

plot(lookbv, sharpev, type="b", main="Sharpe Ratios of Momentum Strategy for Different Look-back Intervals",
     xlab="Look-back Interval in Weeks", ylab="Sharpe Ratio")

pnls <- pnll[[which.max(sharpev)]]
lookb <- lookbv[which.max(sharpev)]

# pnls <- rbind(matrix(numeric(endd[3])), pnls)
# pnls <- cbind(retp$SPY, pnls)
pnls <- cbind(pnls, 0.5*(pnls[, 1] + pnls[, 2]))
colnames(pnls) <- c("EqualWeight", "Momentum", "Combined")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# Plot a dygraph of the momentum strategy
# endw <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endd],
                  main="Idiosyncratic Momentum Strategy for ETFs") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)


# Create the design matrix for the Treynor-Mazuy test
desm <- cbind(pnls$Momentum, retm)
# Aggregate the returns over the weekly end points
# desm <- HighFreq::roll_sumep(desm, endd=(endd[-1]-1))
# Add the Treynor-Mazuy term, which is the square of the market returns
desm <- cbind(desm, desm[, 2]^2)
colnames(desm) <- c("pnls", "SPY", "Treynor")
# Perform the Treynor-Mazuy test
desm <- as.data.frame(desm)
regmod <- lm(pnls ~ SPY + Treynor, data=desm); summary(regmod)

# Plot the residual scatterplot
resids <- regmod$residuals
plot.default(x=desm[, "SPY"], y=resids, xlab="SPY", ylab="momentum")
title(main="Treynor-Mazuy Market Timing Test\n for Momentum vs SPY", line=0.5)
# Plot the fitted (predicted) response values
coefreg <- summary(regmod)$coeff
fitv <- regmod$fitted.values - coefreg["SPY", "Estimate"]*desm[, "SPY"]
tvalue <- round(coefreg["Treynor", "t value"], 2)
points.default(x=desm[, "SPY"], y=fitv, pch=16, col="red")
text(x=0.0, y=max(resids), paste("Treynor test t-value =", tvalue))



weightm <- mclapply(1:(npts-1), function(tday) {
  ##  cat("Processing end point", tday, "of", npts, "\n")
  ##  Select the in-sample dates
  startp <- endd[max(1, tday-lookb)]
  endp <- endd[tday]
  intin <- (startp:endp)
  ##  Select the out-of-sample returns
  intos <- (endd[tday]+1):endd[tday+1]
  datos <- zoo::index(retp[intos])
  ##  If the in-sample interval is too short, return a vector of zeros for the out-of-sample PnLs
  if (NROW(intin) < 11) {
    return(numeric(NCOL(retp)))
  } ##  end if
  ##  Select the in-sample returns
  retis <- retp[intin]
  retmis <- retm[intin]
  varm <- drop(var(retmis))
  retmm <- mean(retmis)
  ##  Calculate the in-sample stock volatilities, betas, and alphas
  riskret <- lapply(retis, function(rets) {
    stdev <- sd(rets)
    betac <- drop(cov(rets, retmis)/varm)
    resid <- rets - betac*retmis
    alphac <- mean(rets) - betac*retmm
    c(alpha=alphac, beta=betac, stdev=stdev, ivar=var(resid))
  })  ## end lapply
  riskret <- do.call(rbind, riskret)
  ##  Calculate the momentum weights
  alphav <- riskret[, "alpha"]
  betav <- riskret[, "beta"]
  ivarv <- riskret[, "ivar"]
  weightv <- alphav/ivarv
  
  return(drop(weightv))
  
}, mc.cores=ncores)  ## end mclapply

weightm <- rutils::do_call(rbind, weightm)
weightm <- xts::xts(weightm, order.by=datev[endd[-1]])
dim(weightm)
tail(weightm)

# Plot dygraph of the momentum weights with highlighting of the individual ETFs on hover
dygraphs::dygraph(weightm[-(1:51)], main="Momentum Weights of ETFs") %>%
  dyOptions(colors=colorRampPalette(c("blue", "red"))(NCOL(weightm)), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# For 11 ETFs starting with X, with weekly rebalancing,
# lookb=45 weeks:
#           EqualWeight Momentum Combined
# Sharpe    0.5580635 0.8930563 1.067797
# Sortino   0.6701961 1.2419834 1.363638


# Doesn't perform well
# 3. (40pts)
# Simulate a momentum strategy of the EMA idiosyncratic
# returns of the ETFs.

# To-do: Compare with HighFreq::sim_portfoptim() which performs well.


Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/run_moment_idiosync.cpp")
retid <- run_moment_idiosync(retp, 0.9)
colnames(retid) <- colnames(retp)[-1]

pnls <- rowMeans(retid)
pnls <- pnls*sd(retp$SPY)/sd(pnls)
pnls <- cbind(retp$SPY, pnls)
colnames(pnls) <- c("SPY", "Momentum")
# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot a dygraph of the momentum strategy
dygraphs::dygraph(cumsum(pnls)[endd],
                  main="EMA Idiosyncratic Momentum Strategy for ETFs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)



# Needs update - ex-post scaling causes leak so it's incorrect
# Copy from homeworks
############## homework
# Summary: Simulate a momentum strategy using the 
# rolling stock alphas.

# Idea:
# Bet against beta.

# Idea:
# Calculate the stock betas and alphas using
# different look-back intervals.
# Calculate the betas using a long looking-back 
# interval.
# Calculate the alphas using a short looking-back 
# interval.


## Run the setup code below

library(rutils)

# Calculate the percentage SPY returns
symboln <- "SPY"
retspy <- na.omit(get(symboln, rutils::etfenv$returns))
# retspy <- na.omit(rutils::etfenv$returns$SPY)
colnames(retspy) <- "SPY"
datev <- zoo::index(retspy)
nrows <- NROW(retspy)
# Load daily S&P500 stock prices
# Download the file from the share drive:
# https://drive.google.com/drive/folders/1jNtoNCL8mLJyda0wu1pLCnjvqIPzd_WL
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select the stock prices since SPY
pricestock <- pricestock[datev]
# Select stocks with no NA values in their prices
numna <- sapply(pricestock, function(x) sum(is.na(x)))
pricestock <- pricestock[, numna == 0]
# Drop penny stocks
pricel <- last(pricestock)
pricel <- drop(coredata(pricel))
pricestock <- pricestock[, pricel > 1]
# Calculate the dollar and percentage stock returns
retd <- rutils::diffit(pricestock)
retp <- retd/rutils::lagit(pricestock)
retp[1, ] <- 0


## End of setup code


# 1. (40pts) 
# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)

# Perform a parallel loop over the monthly end points,
# starting from the second end point.
# It may take long to run the loop.
# 
# At each end point, calculate the stock betas and 
# alphas over the previous month.
# 
# Calculate the out-of-sample stock alphas for 
# the next month, as the stock returns minus the 
# product of the stock betas times the SPY returns.
# 
# Calculate a vector of the momentum stock weights 
# as the differences between the alphas minus their 
# median value for that month.
# 
# Calculate the out-of-sample PnLs of the momentum 
# strategy for that month as the product of the stock 
# alphas times the stock weights.
# 
# Scale the out-of-sample PnLs to match their volatility 
# to the equal-weighted portfolio.
# At each end point, calculate the volatility of the 
# equal weight portfolio, and scale the out-of-sample 
# PnLs to match it.
# 
# Combine the momentum strategy PnLs with the 
# equal-weighted portfolio returns for that month.
# Combine the monthly returns into a single xts 
# time series.
# 
# Add a third column to the time series with the
# combined returns of the momentum strategy and
# the equal-weighted portfolio.

lookb <- 1 ##  Look-back interval in months
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1

pnls <- mclapply(2:npts, function(tday) {
  
  ##  Select the in-sample returns
  startp <- endd[max(1, tday-lookb)]
  retis <- retp[startp:(endd[tday-1]-1)]
  datis <- zoo::index(retis)
  retspy <- retspy[datis]
  varspy <- drop(var(retspy))
  meanspy <- mean(retspy)
  ##  Select the out-of-sample returns
  retos <- retp[endd[tday-1]:endd[tday], ]
  datos <- zoo::index(retos)
  
  ##  Calculate the stock volatilities, betas, and alphas
  riskret <- lapply(retis, function(rets) {
    stdev <- sd(rets)
    betac <- drop(cov(rets, retspy))/varspy
    resid <- rets - betac*retspy
    alphac <- mean(rets) - betac*meanspy
    c(alpha=alphac, beta=betac, stdev=stdev, ivol=sd(resid))
  })  ##  end lapply
  riskret <- do.call(rbind, riskret)
  
  ##  Extract the risk vectors
  alphav <- riskret[, "alpha"]
  betav <- riskret[, "beta"]
  volv <- riskret[, "stdev"]
  ##  Calculate the weights of low and high alpha stocks
  medianv <- median(alphav)
  ##  medianv <- mean(alphav)
  ##  weightv <- ifelse(alphav <= medianv, 1, -1)
  weightv <- (alphav - medianv)#*(volv - median(volv))
  ##  Calculate the out-of-sample stock alphas
  retspyos <- retspy[datos]
  alphav <- lapply(colnames(retp), function(symb) {
    retos[, symb] - retspyos*betav[symb]
  }) ##  end lapply
  alphav <- do.call(cbind, alphav)
  ##  Calculate the out-of-sample returns of low and high alpha stocks
  pnlos <- alphav %*% weightv
  ##  Calculate the equal-weighted portfolio returns
  retew <- rowMeans(retos, na.rm=TRUE)
  ##  Scale the out-of-sample PnLs to match their volatility to the equal-weighted portfolio
  pnlos <- pnlos*sd(retew)/sd(pnlos)
  
  return(xts(cbind(retew, pnlos), order.by=datos))
  
}, mc.cores=ncores)  ##  end mclapply

# Combine the monthly PnLs into a single xts series
pnls <- rutils::do_call(rbind, pnls)
pnls <- cbind(pnls, 0.5*(pnls[, 1] + pnls[, 2]))
colnames(pnls) <- c("EqualWeight", "Momentum", "Combined")


# You should get outputs similar to:

dim(pnls)
# [1] 6424    3
tail(pnls)
#             EqualWeight      Momentum     Combined
# 2025-10-10 -0.022456970 -0.0250554210 -0.023756195
# 2025-10-13  0.011697427  0.0146588669  0.013178147
# 2025-10-14  0.009966074 -0.0002395157  0.004863279
# 2025-10-15  0.001182137  0.0043997057  0.002790922
# 2025-10-16 -0.008731574 -0.0074334744 -0.008082524
# 2025-10-17  0.005059896  0.0043382509  0.004699073

# Calculate the Sharpe and Sortino ratios
sqrt(252)*sapply(pnls, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))

# You should get outputs similar to:
#         EqualWeight  Momentum Combined
# Sharpe    0.7545901 0.7931681 1.156422
# Sortino   0.9670510 1.0885526 1.369870

# Plot a dygraph of the momentum strategy
endw <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(cumsum(pnls)[endw],
                  main="Alpha Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Your plot should be similar to momentum_alpha.png




# Using the momentum beta works OK but not great
############## homework
# Summary: Simulate a strategy using the VTI returns and the momentum beta.

betetf <- sapply(retp, function(x) cov(retp$VTI, x)/var(retp$VTI))
betam <- weightv %*% betetf
colnames(betam) <- "beta"
betam <- xts::xts(betam, order.by=datev[endd])
datav <- cbind(betam, retvti)
datav <- zoo::na.locf(datav)
datav <- zoo::na.locf(datav, fromLast=TRUE)
sum(is.na(datav))
posv <- rutils::lagit(datav$beta, lagg=1)
pnlv <- retvti*posv

dygraph(retvti)
dygraph(datav$beta)
dygraph(cumsum(pnlv))


##############
# Simulate a strategy using the correlation between
# the stock returns and the trading volumes.

# Motivation:
# When the stock prices drop the trading volumes
# usually increase.

# Observation:
# The correlation is very noisy.
# The correlation spikes higher just after the stock
# prices start rebounding after a recent selloff.
# Can be used as a buy signal in momentum strategies,
# to avoid momentum crashes.



# Load stock returns
ohlc <- rutils::etfenv$SPY
closep <- log(quantmod::Cl(ohlc))
volumev <- log(quantmod::Vo(ohlc))
# retp <- rutils::diffit(closep)
# volumr <- rutils::diffit(volumev)
retp <- rutils::diffit(cbind(closep, volumev))
colnames(retp) <- c("Returns", "VolumeChanges")

# Calculate the EMA covariance between returns and volume returns.
# HighFreq::run_covar() returns a matrix with five columns
# of data:
# The EMA covariances, the variances, and the EMA means.
lambdaf <- 0.95
covm <- HighFreq::run_covar(retp, lambdaf)
# covm <- rutils::lagit(covm, lagg=1)
covv <- covm[, 1]
var1 <- covm[, 2]
var2 <- covm[, 3]
ret1 <- covm[, 4]
ret2 <- covm[, 5]

# Calculate the EMA correlation between returns and volume returns.
corrv <- covv/sqrt(var1*var2)
corrv <- rutils::lagit(corrv, lagg=1)
# Plot dygraph of SPY and correlation with dual y-axes
datav <- cbind(closep, corrv)
colv <- c("SPY", "Correlation")
colnames(datav) <- colv
dygraphs::dygraph(datav[-(1:1000), ],
                  main="SPY and Correlation") %>%
  dyAxis("y", label=colv[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colv[2], independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=2, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)



# Needs more work.
############## homework
# Summary: Simulate a daily stock momentum strategy
# using the EMA idiosyncratic returns.

# Works well for lambdaf <- 0.9999 and
# with the weights equal to minus the alphas.

# Idea:
# Use slow lambda for betas and fast lambda for alphas.
# Doesn't perform any better than single lambdaf <- 0.999

# Idea:
# Aggregate the returns over several days.
# Doesn't perform any better than daily returns.


# Idea:
# Are the betas ever negative?
# Bet against beta.
# Works well for lambdaf <- 0.1 and
# with the weights equal to the betas.
# It's actually betting for beta!
#
# Calculate the weights as the betas minus their mean.
# Doesn't perform as well.

# Calculate the trailing EMA betas using C++ code.

# Dates vector
retm <- na.omit(rutils::etfenv$returns$VTI)
colnames(retm) <- "VTI"

# Remove the extreme returns
retp <- lapply(retp, function(rets) {
  rets[which.max(rets)] <- 0
  rets[which.min(rets)] <- 0
  rets[which.max(rets)] <- 0
  rets[which.min(rets)] <- 0
  return(rets)
}) ##  end lapply
retp <- do.call(cbind, retp)

# retm <- (coredata(retm))
datev <- zoo::index(retp)
lambdaf <- 0.9

lambdav <- seq(0.1, 0.9, 0.1)
lambdav <- seq(0.99, 0.999, 0.001)
lambdav <- seq(0.999, 0.9999, 0.0001)

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
# pnls <- mclapply(lambdav, function(lambdaf) {
retmn <- as.numeric(retm)
pnlv <- lapply(lambdav, function(lambdaf) {
  
  ##  Calculate the trailing alphas
  alpham <- lapply(retp, function(rets) {
    covd <- HighFreq::run_covar(cbind(rets, retm), lambda=0.999)
    betav <- covd[, 1]/covd[, 3]
    covd <- HighFreq::run_covar(cbind(rets, retm), lambda=lambdaf)
    alphav <- covd[, 4] - betav*covd[, 5]
    return(cbind(betav, alphav))
  }) ##  end lapply
  betam <- lapply(alpham, function(x) x[, 1])
  betam <- do.call(cbind, betam)
  alpham <- lapply(alpham, function(x) x[, 2])
  alpham <- do.call(cbind, alpham)
  ##  colnames(betam) <- colnames(retp)
  
  ##  Calculate the trailing alphas
  ##  meanm <- HighFreq::run_mean(retp, lambda=lambdaf)
  ##  retmm <- HighFreq::run_mean(retm, lambda=lambdaf)
  ##  alpham <- meanm - betam*drop(retmm)
  
  ##  Calculate the weights using lagged alphas
  weightv <- rutils::lagit(alpham)
  scalev <- sqrt(rowSums(weightv^2, na.rm=TRUE))
  scalev[1:2] <- 1
  weightv <- weightv/scalev
  ##  betav <- rowMeans(betam, na.rm=TRUE)
  ##  weightv <- weightv - rutils::lagit(betav)
  ##  Calculate the weights using lagged alphas
  ##  weightv <- rutils::lagit(alpham)
  
  ##  Calculate the out-of-sample alphas
  ##  alphaf <- retp - HighFreq::mult_mat(retm, rutils::lagit(betam), byrow=FALSE)
  
  ##  alphaf <- retp - rutils::lagit(betam)*drop(coredata(retm))
  ##  Calculate the momentum profits and losses
  ##  pnls <- weightv*alphaf
  ##  Calculate the out-of-sample alphas
  alphal <- retp - rutils::lagit(betam)*retmn
  pnls <- weightv*alphal
  ##  pnls <- sapply(1:NROW(pnls), function(rown) {
  ##    x <- pnls[rown, ]
  ##    x[, which.max(x)] <- NA
  ##    x[, which.min(x)] <- NA
  ##    return(sum(x, na.rm=TRUE))
  ##  }) ##  end apply
  ##  pnls <- do.call(rbind, pnls)
  ##  pnls <- rowSums(pnls, na.rm=TRUE)
  return(sum(pnls))
  
  # }, mc.cores=ncores)  ## end mclapply
})  ## end mclapply

pnlv <- unlist(pnlv)
lambdaf <- lambdav[which.max(pnlv)]

pnls <- xts::xts(pnls, order.by=datev)
# Calculate the Sharpe and Sortino ratios
pnls <- pnls*sd(retm)/sd(pnls)
wealthv <- cbind(retm, pnls)
colnames(wealthv) <- c("VTI", "MaxAlpha")
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot a dygraph of the momentum strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraph(cumsum(wealthv)[endw],
        main="Daily Stock Momentum Strategy using EMA Alphas") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=300)




# Doesn't perform well
############## homework
# Summary: Simulate low volatility and low beta
# momentum strategies.

## Run the setup code below

library(rutils)

# Calculate the percentage VTI returns
retm <- na.omit(rutils::etfenv$returns$VTI)
colnames(retm) <- "VTI"
datev <- zoo::index(retm)
nrows <- NROW(retm)
# Load daily S&P500 stock prices
# Download the file from the share drive:
# https://drive.google.com/drive/folders/1jNtoNCL8mLJyda0wu1pLCnjvqIPzd_WL
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
# Select the stock prices since VTI
pricestock <- pricestock[datev]
# Select stocks with no NA values in their prices
numna <- sapply(pricestock, function(x) sum(is.na(x)))
pricestock <- pricestock[, numna == 0]
# Drop penny stocks
pricel <- last(pricestock)
pricel <- drop(coredata(pricel))
pricestock <- pricestock[, pricel > 1]
# Calculate the dollar and percentage stock returns
retd <- rutils::diffit(pricestock)
retp <- retd/rutils::lagit(pricestock)
retp[1, ] <- 0
# Select 100 random stocks
# set.seed(1121)
# retp <- retp[, sample(NCOL(retp), 100)]
# nstocks <- NCOL(retp)
# Calculate the returns of equal wealth portfolio
retew <- rowMeans(retp, na.rm=TRUE)
retew[1] <- 0

# Random data for testing
# retp <- xts(matrix(rnorm(NCOL(retp)*NROW(retp), sd=0.02), ncol=NCOL(retp)), order.by=index(retp))

## End of setup code


# 1. (20pts)
# Calculate a list of monthly stock volatilities,
# betas, and alphas.
# For each month, calculate the volatilities, betas,
# and alphas over a trailing look-back interval.
# Perform a parallel loop over the monthly end points.

# Calculate a vector of monthly end points
endd <- rutils::calc_endpoints(retp, interval="months")
npts <- NROW(endd)

# Perform a parallel loop over the end points,
# starting from the second end point.
# It takes some time to run this code.

lookb <- 8 ##  Look-back interval in months
library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
riskretl <- mclapply(2:(npts-1), function(tday) {
  
  ##  Select the in-sample returns
  startp <- endd[max(1, tday-lookb)]
  retis <- retp[startp:(endd[tday]-1), ]
  datos <- zoo::index(retis)
  
  ##  Calculate the stock volatilities, betas, and alphas
  riskret <- lapply(retis, function(rets) {
    stdev <- sd(rets)
    retm <- retm[datos]
    varm <- drop(var(retm))
    retmm <- mean(retm)
    betac <- drop(cov(rets, retm))/varm
    resid <- rets - betac*retm
    alphac <- mean(rets) - betac*retmm
    c(alpha=alphac, beta=betac, stdev=stdev, ivar=var(resid))
  })  ## end lapply
  riskret <- do.call(rbind, riskret)
  
  return(riskret)
  
}, mc.cores=ncores)  ## end mclapply


# Calculate the matrix of monthly stock weights.
# Perform a parallel loop over the end points,
# starting from the third end point.
# Perform a parallel loop over the end points,

# Calculate the monthly strategy PnLs

pnls <- mclapply(3:(npts-1), function(tday) {
  
  ##  Select the out-of-sample returns
  retos <- retp[endd[tday-1]:endd[tday], ]
  datos <- zoo::index(retos)
  
  ##  Calculate the median volatility
  riskv <- riskretl[[tday-2]][, "alpha"]
  ##  medianv <- median(riskv)
  medianv <- mean(riskv)
  ##  Calculate the weights of low and high volatility stocks
  ##  weightv <- ifelse(riskv <= medianv, 1, -1)
  weightv <- (riskv - medianv)
  ##  Calculate the out-of-sample returns of low and high volatility stocks
  pnlos <- retos %*% weightv
  ##  Calculate the equal-weighted portfolio returns
  retew <- rowMeans(retos, na.rm=TRUE)
  
  return(xts(cbind(retew, pnlos*sd(retew)/sd(pnlos)), order.by=datos))
  
}, mc.cores=ncores)  ## end mclapply



############## homework
# Summary: Simulate a daily stock momentum strategy
# using the t-values of the weights.
# Find the optimal lambda decay factor.
# The t-values of the weights are the weights
# divided by their trailing EMA volatilities.
# 
# Observations:
# Using t-values doesn't improve the performance.
# The best performing strategy is highly correlated
# to equal weight portfolio.
# The performance depends on whether you scale the weights or not.
#   weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))


# VTI returns
retv <- na.omit(rutils::etfenv$returns$VTI)
datev <- zoo::index(retv) ##  Dates vector
# Load daily S&P500 percentage stock returns
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
retp <- retstock[datev]
nrows <- NROW(retp) ##  number of rows
nstocks <- NCOL(retp) ##  number of stocks

# Calculate the trailing average returns and variance using C++ code
lambdaf <- 0.99
varm <- HighFreq::run_var(retp, lambda=lambdaf)
meanm <- varm[, 1:nstocks]
varm <- varm[, (nstocks+1):(2*nstocks)]
varm[1:11, ] <- 1
# Calculate the trailing Kelly ratios
weightv <- meanm/varm
# weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))
weightv <- rutils::lagit(weightv)

# Calculate the t-values of the weights
varm <- HighFreq::run_var(weightv, lambda=lambdaf)
varm <- varm[, (nstocks+1):(2*nstocks)]
tvalv <- weightv/sqrt(varm)
pnls <- rowSums(tvalv*retp, na.rm=TRUE)

# Calculate the momentum profits and losses
pnls <- rowSums(weightv*retp, na.rm=TRUE)
# retew <- rowMeans(retp, na.rm=TRUE)
pnls <- pnls*sd(retew)/sd(pnls)

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retew, pnls, 0.5*(retew + pnls))
wealthv <- xts::xts(wealthv, datev)
colnames(wealthv) <- c("EqualWeight", "Momentum", "Combined")
cor(wealthv)
sqrt(252)*sapply(wealthv, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot dygraph of stock index and momentum strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw],
                  main="Daily Momentum Strategy for Stocks") %>%
  dyOptions(colors=c("blue", "red", "green"), strokeWidth=1) %>%
  dySeries(name="Combined", strokeWidth=2) %>%
  dyLegend(show="always", width=300)


# Create a vector of lambda values
lambdav <- seq(0.99, 0.9999, 0.001)

library(parallel)  ##  Load package parallel
ncores <- detectCores() - 1
pnls <- mclapply(lambdav, function(lambdaf) {
  varm <- HighFreq::run_var(retp, lambda=lambdaf)
  meanm <- varm[, 1:nstocks]
  varm <- varm[, (nstocks+1):(2*nstocks)]
  varm[1:11, ] <- 1
  ##  Calculate the trailing Kelly ratios
  weightv <- meanm/varm
  weightv <- weightv/sqrt(rowSums(weightv^2, na.rm=TRUE))
  weightv <- rutils::lagit(weightv)
  ##  Calculate the momentum profits and losses
  pnls <- rowSums(weightv*retp, na.rm=TRUE)
  pnls <- pnls*sd(retew)/sd(pnls)
  return(pnls)
}, mc.cores=ncores)  ##  end mclapply

pnls <- rutils::do_call(cbind, pnls)
pnls <- xts::xts(pnls, order.by=datev)
colnames(pnls) <- paste0("lambda=", lambdav)
sharper <- sqrt(252)*apply(pnls, 2, function(x)
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
lambdaf <- lambdav[which.max(sharper[1, ])]


# Plot dygraph of daily stock momentum strategies
colorv <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls)[endw],
                  main="Daily Stock Momentum Strategies") %>%
  dyOptions(colors=colorv, strokeWidth=2) %>%
  dyLegend(show="always", width=500)



############## homework
# Portfolio Optimization Strategy PnLs using sim_portfoptim()

# Compile sim_portfoptim() in R by running this command:
# Rcpp::sourceCpp(file="/Users/jerzy/Develop/Rcpp/run_portf.cpp")

retp <- rutils::etfenv$returns[, c("VTI", "TLT", "DBC", "USO", "XLF", "XLK")]
retp <- na.omit(retp)
datev <- zoo::index(retp) # dates
# Simulate a portfolio optimization strategy
dimax <- 6
lambdaf <- 0.978 # Decay factor
lambdacov <- 0.995
lambdaw <- 0.9
pnls <- sim_portfoptim(retp, dimax, lambdaf, lambdacov, lambdaw)
colnames(pnls) <- c("pnls", "VTI", "TLT", "DBC", "USO", "XLF", "XLK")
pnls <- xts::xts(pnls, order.by=datev)
# Plot dygraph of strategy
wealthv <- cbind(retp$VTI, pnls$pnls)
colnames(wealthv) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
endd <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endd], main="Portfolio Optimization Strategy PnLs") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=300)
# Plot dygraph of the weights
symbolv <- "VTI"
stockweights <- cbind(cumsum(get(symbolv, retp)), get(symbolv, pnls))
colnames(stockweights)[2] <- "Weight"
colnamev <- colnames(stockweights)
endd <- rutils::calc_endpoints(pnls, interval="weeks")
dygraphs::dygraph(stockweights[endd], main="Returns and Weight") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(axis="y2", label=colnamev[2], strokeWidth=2, col="red")




############## homework
# Summary: Calculate portfolio weights under Ridge 
# and LASSO regularization in sample and apply 
# them out-of-sample.

## Run the setup code below
# This is just a copy of the code from the lecture slides.

library(rutils)

# Load the stock returns
load("/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")
datev <- zoo::index(na.omit(retstock$GOOGL))
retp <- retstock[datev] ##  Subset the returns to GOOGL
# Remove the stocks with any NA values
numna <- sapply(retp, function(x) sum(is.na(x)))
retp <- retp[, numna==0]
nstocks <- NCOL(retp)
datev <- zoo::index(retp)
symbolv <- colnames(retp)
retis <- retp["/2014"] ##  In-sample returns
raterf <- 0.03/252
retx <- (retis - raterf) ##  Excess returns

volt <- 0.01 ##  Volatility target for the strategy PnLs
insample <- 1:NROW(retis) # In-sample interval

# Calculate the Maximum Sharpe weights the in-sample interval
covmat <- cov(retis, use="pairwise.complete.obs")
invreg <- MASS::ginv(covmat)
colmeanv <- colMeans(retx, na.rm=TRUE)
weightv <- drop(invreg %*% colmeanv)
names(weightv) <- symbolv
head(sort(weightv))
tail(sort(weightv))
# Calculate the portfolio returns
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)
retew <- xts::xts(rowMeans(retp, na.rm=TRUE), datev)
pnls <- pnls*sd(retew["/2014"])/sd(pnls["/2014"])
wealthv <- cbind(retew, pnls)
colnames(wealthv) <- c("EqualWeight", "MaxSharpe")

# Calculate the in-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["/2014"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Calculate the out-of-sample Sharpe and Sortino ratios
sqrt(252)*sapply(wealthv["2015/"], function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
# Plot of cumulative portfolio returns
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], 
                  main="Out-of-Sample Maximum Sharpe Stock Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyEvent(zoo::index(last(retis[, 1])), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=300)

# Calculate the reduced inverse of covariance matrix
dimax <- 10
eigend <- eigen(covmat)
eigenvec <- eigend$vectors
eigenval <- eigend$values
invred <- eigenvec[, 1:dimax] %*% 
  (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
# Calculate portfolio weights and PnLs
colmeanv <- colMeans(retx["/2014"], na.rm=TRUE)
weightv <- invred %*% colmeanv
pnls <- HighFreq::mult_mat(weightv, retp)
pnls <- rowMeans(pnls, na.rm=TRUE)
pnls <- xts::xts(pnls, datev)


## End of setup code



# 1. (10pts)
# Calculate the optimal dimension reduction parameter 
# to maximize the out-of-sample Sharpe ratio.
# Hint: Adapt the lecture code.

# Define the vector of dimension reduction parameters
dimv <- seq(from=5, to=11)

# Perform an lapply loop over the vector of dimension reduction parameters
pnls <- lapply(dimv, function(dimax) {
  invred <- eigenvec[, 1:dimax] %*%
    (t(eigenvec[, 1:dimax]) / eigenval[1:dimax])
  ##  Calculate portfolio weights and PnLs
  weightv <- invred %*% colmeanv
  pnls <- retp %*% weightv
  pnls <- pnls*volt/sd(pnls[insample, ])
  return(pnls)
})  ##  end lapply
pnls <- do.call(cbind, pnls)
colv <- paste0("dim=", dimv)
colnames(pnls) <- colv
pnls <- xts::xts(pnls, datev)


# Calculate the out-of-sample Sharpe ratios of the optimal portfolios
sharpev <- sapply(pnls["2015/"], function(x) sqrt(252)*mean(x)/sd(x))
whichmax <- which.max(sharpev)
dimax <- dimv[whichmax]
# Plot Sharpe ratios of momentum strategies
plot(x=dimv, y=sharpev, t="b",
     main="Out-of-sample Sharpe as Function of Dimension Reduction",
     xlab="dimax", ylab="Sharpe")


# Dygraph of cumulative PnLs of the Dimension Reduction portfolios
colorv <- colorRampPalette(c("red", "blue"))(NROW(dimv))
endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
dygraphs::dygraph(cumsum(pnls["2015/"])[endw], 
                  main="Out-of-sample PnLs as Function of Dimension Reduction") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  # Highlight the optimal Ridge portfolio
  dySeries(name=colv[whichmax], col="green", strokeWidth=2) %>%
  # dyEvent(end(pnls["/2014"]), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)


# 2. (20pts)
# Calculate the optimal Ridge intensity parameter
# to maximize the out-of-sample Sharpe ratio.

# Define a vector of Ridge intensity parameters
lambdav <- seq(from=0, to=0.02, by=0.002)
unitm <- diag(nstocks) # Unit matrix for Ridge regularization

# Perform an lapply loop over the vector of Ridge intensity parameters
pnls <- lapply(lambdav, function(lambdar) {
  covridge <- covmat + lambdar*unitm
  invreg <- MASS::ginv(covridge)
  ##  Calculate portfolio weights and PnLs
  weightv <- invreg %*% colmeanv
  pnls <- retp %*% weightv
  pnls <- pnls*volt/sd(pnls[insample, ])
  return(pnls)
})  ##  end lapply
pnls <- do.call(cbind, pnls)
colv <- paste0("lambda=", lambdav)
colnames(pnls) <- colv
pnls <- xts::xts(pnls, datev)


# Calculate the out-of-sample Sharpe ratios of the optimal portfolios
sharpev <- sapply(pnls["2015/"], function(x) sqrt(252)*mean(x)/sd(x))
whichmax <- which.max(sharpev)
lambdamax <- lambdav[whichmax]
# Plot Sharpe ratios of momentum strategies
plot(x=lambdav, y=sharpev, t="b",
     main="Out-of-sample Sharpe as Function of Ridge Intensity",
     xlab="lambda", ylab="Sharpe")


# Dygraph of cumulative PnLs of the Ridge portfolios
colorv <- colorRampPalette(c("red", "blue"))(NROW(lambdav))
endw <- rutils::calc_endpoints(pnls["2015/"], interval="weeks")
dygraphs::dygraph(cumsum(pnls["2015/"])[endw], 
                  main="Out-of-sample PnLs as Function of Ridge Intensity") %>%
  dyOptions(colors=colorv, strokeWidth=1) %>%
  # Highlight the optimal Ridge portfolio
  dySeries(name=colv[whichmax], col="green", strokeWidth=2) %>%
  # dyEvent(end(pnls["/2014"]), label="in-sample", strokePattern="solid", color="red") %>%
  dyLegend(width=400)




##############################
### 
##############################



##############################
### 
##############################



##############################
### 
##############################



##############################
### 
##############################



