####################################
# Scripts for downloading data from external sources,
# and for loading data from files.

# Suppress spurious timezone warning message: "timezone of object is different than current timezone"
Sys.setenv(TZ=Sys.timezone())
# Load HighFreq
library(HighFreq)


##############
# Load intraday prices from CSV files, create a list of time series of prices,
# and save it into a binary file.

# Set parameters for directory with CSV files
dirin <- "/Users/jerzy/Develop/data/raw/"
dirp <- "/Users/jerzy/Develop/data/"
# setwd(dir=dirin)
symboln <- "AAPL"
# Get all CSV file names in the dirin directory
# datan <- "_minute_"
# datan <- "_minute_202308"
# datan <- "_10second_"
# datan <- "_second_"
monthv <- "04"
datan <- paste0("_second_2024", monthv)
filev <- Sys.glob(paste0(dirin, symboln, datan, "*.csv"))
# Loop over the file names, load the data from CSV files,
# and calculate a list of time series of prices.
pricel <- lapply(filev, function(filen) {
  cat("file: ", filen, "\n")
  # Load time series data from CSV file
  dtable <- data.table::fread(filen)
  # Calculate a time series of prices
  datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
  # pricev <- xts::xts(dtable[, .(aapl_price, aapl_volume)], order.by=datev)
  # strprice <- paste0(tolower(symboln), "_price")
  # strvol <- paste0(tolower(symboln), "_volumee")
  # Adjust price for SVXY stock split
  # indeks <- (dtable[, 2] > 80)
  # dtable[indeks, 2] <- dtable[indeks, 2]/2
  # Adjust price for VXX reverse stock split
  # indeks <- as.logical(dtable[, 2] < 20)
  # dtable[indeks, 2] <- 4*dtable[indeks, 2]
  pricev <- xts::xts(dtable[, 2:3], order.by=datev)
  pricev <- pricev["T09:30:00/T16:00:00"]
  # pricev <- pricev[, 1]
  colnames(pricev)[1] <- symboln
  pricev
}) # end lapply

# Assign names to the list equal to the dates
# names(pricel) <- substr(filev, 38, 45)
# namev <- sapply(filev, function(filen) {
#   charv <- strsplit(filen, "")
#   charv <- suppressWarnings(as.numeric(unlist(charv)))
#   charv <- charv[!is.na(charv)]
#   paste(charv, collapse="")
# }) # end sapply

# Assign names to the list equal to the dates
namev <- as.Date(sapply(pricel, function(x) as.Date(end(x))))
namev <- unname(namev)
names(pricel) <- namev


filen <- paste0(dirp, symboln, datan, ".RData")
save(pricel, file=filen)


## Calculate minute prices from second prices
pricel <- lapply(pricel, xts::to.minutes)
pricel <- lapply(pricel, quantmod::Cl)
for (x in 1:NROW(pricel)) {colnames(pricel[[x]]) <- symboln}
save(pricel, file=paste0(dirp, symboln, "_minute_2024", monthv, ".RData"))


## Subset prices in CSV files to a single day - 2024-10-04
# Two days of data were in each file, so needed to subset to a single day

startime <- as.POSIXct("2024-10-04 9:30", origin="1970-01-01", tz="America/New_York")
endtime <- as.POSIXct("2024-10-04 16:00", origin="1970-01-01", tz="America/New_York")

filev <- Sys.glob("/Users/jerzy/Develop/data/raw/*_second_20241005.csv")

filenn <- lapply(filev, function(filen) {
  cat("file: ", filen, "\n")
  dtable <- data.table::fread(filen)
  datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
  datev <- (datev > startime) & (datev < endtime)
  dtable <- dtable[datev, ]
  data.table::fwrite(dtable, file=filen)
  filen
}) # end lapply




## Calculate a list of returns
retl <- lapply(pricel, function(pricev) {
  retv <- rutils::diffit(pricev)
  # Set very large returns to zero, to eliminate bad data
  retv[abs(retv) > 0.3] <- 0.0
  # zoo::na.locf(retv)
  retv
}) # end lapply
sapply(retl, function(retv) {max(abs(retv))})

# Save the lists of prices and returns
save(pricel, retl, file=paste0(dirin, symboln, datan, format(Sys.Date(), "%Y%m%d"), ".RData"))



##############
# Convert the monthly time series of prices into a list of daily prices called pricel.
# filen <- "/Users/jerzy/Develop/data/SPY_minute_202307.RData"
filen <- "/Users/jerzy/Develop/data/SPY_second_202307.RData"
load(filen)
endd <- rutils::calc_endpoints(pricev, "days")
pricel <- lapply(2:NROW(endd), function(it) {
  pricev[(endd[it-1]+1):endd[it]]
})
# Assign names to the list equal to the dates
namev <- as.Date(sapply(pricel, function(x) as.Date(end(x))))
names(pricel) <- namev
foo <- do.call(rbind, pricel)
all.equal(foo, pricev)
save(pricel, file=filen)



##############
# Load the intraday OHLC prices from CSV files, chain them, and save the monthly time series into a binary file.

# Set parameters for directory with CSV files
dirin <- "/Users/jerzy/Develop/data/"
setwd(dir=dirin)
symboln <- "SPY"
# Load the existing data
datan <- "_second_"
# datan <- "_minute_202308"
# datan <- "_second_202308"
# datan <- "_minute_2"
# Load the monthly file
filen <- Sys.glob(paste0(dirin, symboln, datan, "*8.RData"))
load(file=filen)
# pricev <- NULL # If new month
# Get all CSV file names in the dirin directory
filev <- Sys.glob(paste0(dirin, symboln, datan, "*.csv"))

## Load the intraday OHLC prices from CSV files, chain them, and save the monthly time series into a binary file.
# Loop over the filev, load the data from CSV files,
# and save the OHLC prices to a binary file
countn <- 1
for (filen in filev) {
  cat("Counter: ", countn, "\n")
  # Load time series data from CSV file
  dtable <- data.table::fread(filen)
  datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
  pricen <- xts::xts(dtable[, .(price, volume)], order.by=datev)
  # colnames(pricen)[1] <- symboln
  colnames(pricen)[1] <- symboln
  pricev <- rbind(pricev, pricen)
  countn <- countn + 1
}  # end for

# Plot the monthly prices
dygraphs::dygraph(pricev[, symboln], main=paste(symboln, "Ticks")) %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Save the monthly file
save(pricev, file=filen)

# Combine the monthly files
load(file=Sys.glob(paste0("/Users/jerzy/Develop/data/", symboln, datan, "*6.RData")))
pricevo <- pricev
load(file=Sys.glob(paste0("/Users/jerzy/Develop/data/", symboln, datan, "*7.RData")))
pricevo <- rbind(pricev, pricevo)
load(file=Sys.glob(paste0("/Users/jerzy/Develop/data/", symboln, datan, "*8.RData")))
pricev <- rbind(pricev, pricevo)
save(pricev, file=paste0("/Users/jerzy/Develop/data/", symboln, datan, "2023.RData"))



##############
# Load the intraday OHLC prices from CSV files, chain them, and save them into a binary file.
# NVDA and XLK prices for November, December 2024, to January 2025.

# Chain the NVDA seconds prices
dirin <- "/Users/jerzy/Develop/data/raw/"
dirp <- "/Users/jerzy/Develop/data/"
symboln <- "NVDA"
datan <- "_second_"
# Find all available NVDA files
filev <- Sys.glob(paste0(dirin, symboln, datan, "*.csv"))
pricel <- lapply(filev, function(filen) {
  cat("file: ", filen, "\n")
  dtable <- data.table::fread(filen)
  datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
  pricev <- xts::xts(dtable[, 2:3], order.by=datev)
  pricev <- pricev["T09:30:00/T16:00:00"]
  colnames(pricev)[1] <- symboln
  # Remove stale unchanged prices
  retp <- rutils::diffit(pricev[, 1])
  pricev <- pricev[!(retp==0), ]
  pricev
}) # end lapply
namev <- as.Date(sapply(pricel, function(x) as.Date(end(x))))
namev <- unname(namev)
names(pricel) <- namev
save(pricel, file=paste0(dirp, symboln, "_seconds_202425.RData"))

# Aggregate the NVDA seconds prices to 10 seconds
pricel <- lapply(pricel, xts::to.period, period="seconds", k=10)
pricel <- lapply(pricel, quantmod::Cl)
for (x in 1:NROW(pricel)) {colnames(pricel[[x]]) <- symboln}
save(pricel, file=paste0(dirp, symboln, "_10seconds_202425.RData"))
# Aggregate the NVDA seconds prices to minutes
pricel <- lapply(pricel, xts::to.minutes)
pricel <- lapply(pricel, quantmod::Cl)
for (x in 1:NROW(pricel)) {colnames(pricel[[x]]) <- symboln}
save(pricel, file=paste0(dirp, symboln, "_minute_202425.RData"))


# Chain the XLK seconds prices, for the months with NVDA prices
symboln <- "XLK"
namev <- sapply(namev, gsub, pattern="-", replacement="")
filev <- sapply(namev, function(x) paste0(dirin, symboln, "_second_", x, ".csv"))
pricel <- lapply(filev, function(filen) {
  cat("file: ", filen, "\n")
  dtable <- data.table::fread(filen)
  datev <- as.POSIXct(dtable$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
  pricev <- xts::xts(dtable[, 2:3], order.by=datev)
  pricev <- pricev["T09:30:00/T16:00:00"]
  colnames(pricev)[1] <- symboln
  pricev
}) # end lapply
namev <- as.Date(sapply(pricel, function(x) as.Date(end(x))))
namev <- unname(namev)
names(pricel) <- namev
save(pricel, file=paste0(dirp, symboln, "_202425.RData"))

# Aggregate the XLK seconds prices to 10 seconds
pricel <- lapply(pricel, xts::to.period, period="seconds", k=10)
pricel <- lapply(pricel, quantmod::Cl)
for (x in 1:NROW(pricel)) {colnames(pricel[[x]]) <- symboln}
save(pricel, file=paste0(dirp, symboln, "_10seconds_202425.RData"))
# Aggregate the XLK seconds prices to minutes
pricel <- lapply(pricel, xts::to.minutes)
pricel <- lapply(pricel, quantmod::Cl)
for (x in 1:NROW(pricel)) {colnames(pricel[[x]]) <- symboln}
save(pricel, file=paste0(dirp, symboln, "_minute_202425.RData"))



##############
# Load trades data from CSV files, rbind it, and save into a CSV file

# Set parameters for directory with CSV files
symboln <- "SPY"
dirin <- "/Users/jerzy/Develop/data"
setwd(dir=dirin)
# Get all CSV file names in the dirin directory
filev <- Sys.glob(paste(dirin, "SPY_trades_*.csv", sep="/"))
# Loop over the filev, load the data from CSV files,
# and rbind them
tradem <- NULL
countn <- 1
for (filen in filev) {
  cat("Counter: ", countn, "\n")
  # Load time series data from CSV file
  dtable <- data.table::fread(filen)
  tradem <- rbind(tradem, dtable)
  countn <- countn + 1
}  # end for

# Select unique rows
datev <- tradem$timestamp
unq <- unique(datev)
indx <- match(unq, datev)
tradem <- tradem[indx, ]

# Save the trades data to a CSV file
data.table::fwrite(tradem, file="/Users/jerzy/Develop/data/SPY_trades_202306.csv")

# Save the trade prices
datev <- as.POSIXct(tradem$timestamp/1e3, origin="1970-01-01", tz="America/New_York")
pricev <- xts::xts(tradem$price, order.by=datev)
colnames(pricev) <- "SPY"
dygraphs::dygraph(pricev, main=paste(colnames(pricev), "Ticks")) %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=300)



##############
# Download intraday OHLC bars from Alpaca.

# https://docs.alpaca.markets/reference/stockbars

# Define the parameters for the Alpaca API
library(httr)
urls <- "https://data.alpaca.markets/v2/stocks/bars"
symboln <- "SPY"
startd <- "2024-01-01"
# Get the business days since 2024-01-01 using RQuantLib
datev <- seq.Date(from=as.Date(startd), to=Sys.Date(), by="day")
datev <- datev[RQuantLib::isBusinessDay("UnitedStates/NYSE", datev)]
# Output directory
dirp <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, "/")


# Loop over the dates, download the intraday prices from Alpaca,
# save them to CSV files.
pricel <- lapply(datev, function(x) {

  cat("datev: ", format(x), "\n")
  # Define the start time for the query
  startdt <- paste0(x, "T08:00:00Z")
  # Create the query string
  queryString <- list(
    symbols = symboln,
    timeframe = "1Min",
    start = startdt,
    limit = "1000",
    adjustment = "all",
    feed = "sip",
    sort = "asc"
  ) # end list

  # Submit the request to the Alpaca API
  respv <- httr::VERB("GET", urls, query=queryString, add_headers('APCA-API-KEY-ID'='PK4R79RKDPTHIELAEUJM', 'APCA-API-SECRET-KEY'='1bkm70sEJbHzE3VlTE1CpnU0U7hX865OqjRakgLu'), content_type("application/octet-stream"), accept("application/json"))
  # Extract the content from the response
  contv <- content(respv)
  # Extract the JSON data from the content
  ohlc <- contv$bars[[1]]
  # Convert the JSON data into OHLC matrix
  ohlc <- lapply(ohlc, function(x) unlist(x)[c("t","o","h","l","c","v","vw")])
  ohlc <- do.call(rbind, ohlc)
  # Coerce the time from string to date-time
  datev <- as.POSIXct(ohlc[, "t"], origin="1970-01-01", format="%Y-%m-%dT%H:%M:%OSZ")
  # Select the rows of ohlc from the same day
  datet <- as.Date(datev[1]) # Today's date
  sameday <- (as.Date(datev) == datet)
  ohlc <- ohlc[sameday, ]
  datev <- datev[sameday]
  # Remove the first time column from the OHLC prices
  ohlc <- ohlc[, -1]
  colnames(ohlc) <- paste(symboln, c("Open", "High", "Low", "Close", "Volume", "VWAP"), sep=".")
  # Coerce from matrix to xts
  ohlc <- xts::xts(ohlc, order.by=datev)
  # Save the OHLC prices to a CSV file
  filen <- paste0(dirp, x, ".csv")
  write.zoo(ohlc, file=filen, row.names=FALSE, col.names=TRUE, sep=",")
  ohlc

}) # end lapply

# Save the list of time series of prices to an RData file
filen <- paste0("/Users/jerzy/Develop/data/minutes/", symboln, ".RData")
save(pricel, file=filen)



##############
# Download OHLC bars from Polygon
# 20 years of data with current MT plan

# Setup code
symboln <- "SPY"
startd <- as.Date("2019-01-01")
# startd <- as.Date("2021-05-01")
endd <- Sys.Date()
tspan <- "minute"
# tspan <- "day"
# Polygon API key (old)
apikey <- "0P2f8j8CwAbdY4M8VYt_8pwdP0V4TunxbvRVC_"

# Create url for download
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symboln, "/range/1/", tspan, "/", startd, "/", endd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
# old code below to download OHLC bars from Polygon into JSON format file - not necessary
# download.file(urll, destfile="/Volumes/external/Develop/data/data.json")
# Read OHLC bars from json file
# ohlcn <- jsonlite::read_json("/Volumes/external/Develop/data/data.json")
# Download ohlcn prices in JSON format from Polygon
ohlcn <- jsonlite::read_json(urll)
class(ohlcn)
NROW(ohlcn)
names(ohlcn)
# Extract list of prices from json object
ohlcn <- ohlcn$results
# Coerce from list to matrix
ohlcn <- lapply(ohlcn, function(x) unlist(x)[c("t","o","h","l","c","v","vw")])
ohlcn <- do.call(rbind, ohlcn)
# Coerce the time from milliseconds to date-time
datev <- ohlcn[, "t"]/1e3
datev <- as.POSIXct(datev, origin="1970-01-01")
head(datev)
tail(datev)
# Coerce from matrix to xts
ohlcn <- ohlcn[, -1]
colnames(ohlcn) <- paste(symboln, c("Open", "High", "Low", "Close", "Volume", "VWAP"), sep=".")
ohlcn <- xts::xts(ohlcn, order.by=datev)
head(ohlcn)
tail(ohlcn)

# Copy to ohlc the incremental ohlcn data
# ohlc <- ohlcn
ohlc <- rbind(ohlc, ohlcn[zoo::index(ohlcn) > end(ohlc)])

# Save to file
filen <- paste0("/Users/jerzy/Develop/data/", symboln, "_minute.RData")
save(ohlc, file=filen)
# rm(ohlc)


# Or using rutils::getpoly()

ohlc <- rutils::getpoly(symbol=symboln, startd=startd, apikey=apikey)
colnames(ohlc) <- paste0(symboln, ".", colnames(ohlc))


# Candlestick plot of OHLC prices
dygraphs::dygraph(ohlc["2019/", 1:4], main=paste("Candlestick Plot of", symboln, "OHLC prices")) %>%
  dygraphs::dyCandlestick()
# Dygraphs plot of Close prices
dygraphs::dygraph(ohlc[, 4], main=paste(symboln, "Close prices")) %>%
  dySeries(colnames(ohlc[, 4]), label=symboln) %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=500)



##############
# Download daily OHLC bars from Tiingo for multiple ETF symbols in a loop
# Polygon doesn't adjust VTI, VXX, and SVXY prices - Download from Tiingo
# Polygon has bad QQQ prices - Download from Tiingo

## Setup code
startd <- as.Date("1990-01-01")
# startd <- as.Date("2021-05-01")
endd <- Sys.Date()
# tspan <- "day"
# apikey <- "UJcr9ctoMBXEBK1Mqu_KQAkUuBxLvEtE"

# Select ETF symbols for asset allocation
symbolv <- c("SPY", "VTI", "QQQ", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
             "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
             "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
             "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ")
# Create new environment for ETF data
etfenv <- new.env()


# Initialize Boolean vector of the symbols that were already downloaded
isdown <- symbolv %in% ls(etfenv)

# Download data from Tiingo using while loop
while (sum(!isdown) > 0) {
  for (symboln in symbolv[!isdown]) {
    cat("Processing:", symboln, "\n")
    tryCatch({  # With error handler
      # Download OHLC bars from Polygon
      # ohlc <- rutils::getpoly(symbol=symboln, startd=startd, apikey=apikey)
      # Download OHLC bars from Tiingo
      ohlc <- quantmod::getSymbols.tiingo(symboln, adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
      # colnames(ohlc) <- paste0(symboln, ".", colnames(ohlc))
      zoo::index(ohlc) <- as.Date(zoo::index(ohlc))
      # Save to environment
      assign(symboln, ohlc, envir=etfenv)
      Sys.sleep(1)
    },
    error={function(error_cond) print(paste("Error handler:", error_cond))},
    finally=print(paste0("symbol=", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbolv already downloaded
  isdown <- symbolv %in% ls(etfenv)
}  # end while

# Polygon doesn't adjust VTI, VXX, and SVXY prices - Download from Tiingo
# ohlc <- quantmod::getSymbols.tiingo("VTI", adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
# etfenv$VTI <- ohlc
# ohlc <- quantmod::getSymbols.tiingo("VXX", adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
# etfenv$VXX <- ohlc
# ohlc <- quantmod::getSymbols.tiingo("SVXY", adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
# etfenv$SVXY <- ohlc

# Save OHLC prices to .RData file
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

### Add stats to etfenv

# Extract Close prices
prices <- eapply(etfenv, quantmod::Cl)
prices <- do.call(cbind, prices)
# Drop ".Close" from colnames
colnames(prices) <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
# Calculate the log returns
returns <- xts::diff.xts(log(prices))
# returns[1, ] <- 0 # No - this biases the correlations
# Copy prices and returns into etfenv
etfenv$prices <- prices
etfenv$returns <- returns
# Copy symbolv into etfenv
etfenv$symbolv <- symbolv
# Calculate the risk-return statistics
riskstats <- PerformanceAnalytics::table.Stats(returns)
# Transpose the data frame
riskstats <- as.data.frame(t(riskstats))
# Add Name column
riskstats$Name <- rownames(riskstats)
# Add Sharpe ratio column
riskstats$'Arithmetic Mean' <- sapply(returns, mean, na.rm=TRUE)
riskstats$Sharpe <- sqrt(252)*riskstats$"Arithmetic Mean"/riskstats$Stdev
# Copy riskstats into etfenv
etfenv$riskstats <- riskstats
# Calculate the beta, alpha, Treynor ratio, and other performance statistics
capmstats <- PerformanceAnalytics::table.CAPM(Ra=returns[, symbolv],
                                              Rb=returns[, "SPY"], scale=252)
colnamev <- strsplit(colnames(capmstats), split=" ")
colnamev <- do.call(cbind, colnamev)[1, ]
colnames(capmstats) <- colnamev
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colnamev <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colnamev)
colnamev[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colnamev
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv
etfenv$capmstats <- capmstats
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")



##############
# Download daily OHLC bars from Tiingo for multiple S&P500 symbols in a loop
# Polygon doesn't adjust VTI, VXX, and SVXY prices - Download from Tiingo
# Polygon has bad QQQ prices - Download from Tiingo

# Select S&P500 symbols
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
symbolv <- sp500table$Ticker
# Create new environment for S&P500 data
sp500env <- new.env()

# Initialize Boolean vector of the symbols that were already downloaded
isdown <- symbolv %in% ls(sp500env)
nattempts <- 0
startd <- as.Date("1990-01-01")

# Download data from Tiingo using while loop
while ((sum(!isdown) > 0) & (nattempts<10)) {
  nattempts <- nattempts + 1
  for (symboln in symbolv[!isdown]) {
    cat("Processing:", symboln, "\n")
    tryCatch({  # With error handler
      # Download OHLC bars from Polygon
      # ohlc <- rutils::getpoly(symbol=symboln, startd=startd, apikey=apikey)
      # Tiingo uses hyphen not dot
      symbolg <- gsub("[.]", "-", symboln)
      # Download OHLC bars from Tiingo
      ohlc <- quantmod::getSymbols.tiingo(symbolg, adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
      # colnames(ohlc) <- paste0(symboln, ".", colnames(ohlc))
      # zoo::index(ohlc) <- as.Date(zoo::index(ohlc))
      # Save to environment
      assign(symboln, ohlc, envir=sp500env)
      Sys.sleep(1)
    },
    error={function(error_cond) print(paste("Error handler:", error_cond))},
    finally=print(paste0("symbol=", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbolv already downloaded
  isdown <- symbolv %in% ls(sp500env)
}  # end while

# Calculate the symbols not downloaded
isdown <- symbolv %in% ls(sp500env)
sum(!isdown)
symbolv[!isdown]

# Rename element "LOW" to "LWES"
sp500env$LWES <- sp500env$LOW
rm(LOW, envir=sp500env)
colnames(sp500env$LWES) <- paste("LWES", rutils::get_name(colnames(sp500env$LWES), 2), sep=".")
# Rename element "BRK.B" to "BRKB"
sp500env$BRKB <- sp500env$BRK.B
rm(BRK.B, envir=sp500env)
colnames(sp500env$BRKB) <- paste("BRKB", rutils::get_name(colnames(sp500env$BRKB), 2), sep=".")
# Rename element "BF.B" to "BFB"
sp500env$BFB <- sp500env$BF.B
rm(BF.B, envir=sp500env)
colnames(sp500env$BFB) <- paste("BFB", rutils::get_name(colnames(sp500env$BFB), 2), sep=".")


# Save OHLC prices to .RData file
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")


### Remove the obsolete S&P500 symbols

# Calculate the symbols with less than 1500 days of prices and no recent prices
endd <- end(sp500env$AAPL)
symbolb <- unlist(eapply(sp500env, function(pricev) {
  if ((NROW(pricev) < 1500) & (end(pricev) < endd)) {
    rutils::get_name(colnames(Cl(pricev)))
  }
}))  # end lapply

# Remove the obsolete S&P500 symbols from sp500env
rm(list=symbolb, envir=sp500env)

# Remove the obsolete S&P500 symbols from sp500_constituents.csv
sp500old <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
sp500old <- sp500old[!(sp500old$Ticker %in% symbolb), ]
write.csv(sp500old, file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv", row.names=FALSE)



##############
# Download minute bars for single symbol in a loop
# Has to be performed in batches since data limit=50000

# Setup code
symboln <- "AAPL"
startd <- as.Date("2019-01-01")
startdd <- startd - 1
# startd <- as.Date("2021-05-01")
# endd <- Sys.Date()
endd <- Sys.Date()
tspan <- "minute"
# tspan <- "day"
apikey <- "SDpnrBpiRzONMJdl48r6dOo0_mjmCu6r"
datenv <- new.env()
iter <- 1

# Run loop
while (startdd < startd) {
  cat("Downloading batch ", iter, "\t")
  cat("Start date =", format(startd), "\n")
  urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symboln, "/range/1/", tspan, "/", startd, "/", endd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
  # download.file(urll, destfile="/Volumes/external/Develop/data/data.json")
  # ohlc <- jsonlite::read_json("/Volumes/external/Develop/data/data.json")
  ohlc <- jsonlite::read_json(urll)
  ohlc <- ohlc$results
  # ohlc <- lapply(ohlc, unlist)
  ohlc <- lapply(ohlc, function(x) unlist(x)[c("t","o","h","l","c","v","vw")])
  ohlc <- do.call(rbind, ohlc)
  datev <- ohlc[, "t"]/1e3
  # Polygon seconds are moment in time in UTC
  datev <- as.POSIXct(datev, origin="1970-01-01", tz="America/New_York")
  ohlc <- ohlc[, -1]
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
  ohlc <- xts::xts(ohlc, order.by=datev)
  startdd <- startd
  startd <- as.Date(end(ohlc))-1
  # ohlc <- rbind(ohlc, datenv)
  # ohlc <- ohlc[!duplicated(index(ohlc)), ]
  # datenv <- ohlc
  assign(paste0("ohlc", iter), ohlc, envir=datenv)
  iter <- iter + 1
  cat("Downloaded done - sleeping for 1 sec ... \n")
  Sys.sleep(1)
}  # end while

# Combine xts
ohlc <- do.call(rbind, as.list(datenv))
# Remove duplicates
# datev <- xts::make.index.unique(sort(index(ohlc)), drop=TRUE)
# ohlc <- ohlc[datev]
ohlc <- ohlc[!duplicated(index(ohlc)), ]
colnames(ohlc) <- paste0(symboln, ".", colnames(ohlc))
# For daily prices only
zoo::index(ohlc) <- as.Date(zoo::index(ohlc))
# Save xts to file
# spyohlc <- ohlc
# save(spyohlc, file="/Volumes/external/Develop/data/spy_minutes.RData")
save(ohlc, file=paste0("/Users/jerzy/Develop/data/", symboln, "_", tspan, ".RData"))
# rm(spyohlc)


# Load existing minute bars of SPY prices
loadd <- load(file="/Volumes/external/Develop/data/spy_minutes.RData")
# Or
loadd <- load(file="/Volumes/external/Develop/data/spyvxx_minutes.RData")
# Plot the SPY and VXX Returns
colnamev <- colnames(returns)
dygraphs::dygraph(cumsum(returns), main="SPY and VXX Returns") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")


# Old code
# Coerce from json to data frame
# ohlc <- jsonlite::toJSON(ohlc)
# ohlc <- jsonlite::fromJSON(ohlc)
# sapply(ohlc, class)



##############
# Coerce time zones
# Index had America/New_York time zone while the clock time was actually UTC

load("/Volumes/external/Develop/data/spy_minutes.RData")
datev <- zoo::index(spyohlc)
# Get same moment of time in UTC time zone
datev <- lubridate::with_tz(datev, "UTC")
# Get same clock time in America/New_York time zone
datev <- lubridate::force_tz(datev, "America/New_York")
zoo::index(spyohlc) <- datev
save(spyohlc, file="/Volumes/external/Develop/data/spy_minutes.RData")



##############
# Update CSV files with S&P500 constituents

# Load CSV file with S&P500 old constituents
sp500old <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
dim(sp500old)
# Check if there are missing tickers
sum(is.na(sp500old$Ticker))
sum(sp500old$Ticker == "")
# Remove rows with missing tickers
# sp500old <- sp500old[!(sp500old$Ticker == ""), ]
# Replace dots with hyphens in tickers
# sp500old$Ticker <- gsub("[.]", "-", sp500old$Ticker)
# Remove suffixes from tickers
# sp500old$Ticker <- rutils::get_name(sp500old$Ticker)
# (needs recalculating) Load unavailable tickers from Tiingo download - mergers, etc.
# tickna <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/tickna.csv")
# tickna <- unname(unlist(tickna))
# Select available tickers
# sp500old <- sp500old[!(sp500old$Ticker %in% tickna), ]
# Extract all the old tickers
tickold <- unique(sp500old$Ticker)

# Download CSV file with current SPY ETF holdings
# https://www.ssga.com/us/en/intermediary/etfs/funds/spdr-sp-500-etf-trust-spy
# https://www.ishares.com/us/products/239726/ishares-core-sp-500-etf
# Save as CSV file and remove header and footer
# Load CSV file with current SPY holdings
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/SPY_holdings.csv")
# Remove the tickers for US DOLLAR
# sp500table <- sp500table[-which(sp500table$Ticker == "CASH_USD"), ]
sp500table <- sp500table[-which(sp500table$Name == "US DOLLAR"), ]
# Switch the first two columns
sp500table[, 1:2] <- sp500table[, c("Ticker", "Name")]
colnames(sp500table)[1:2] <- c("Ticker", "Name")
# Replace dots with hyphens in tickers
sp500table$Ticker <- gsub("[.]", "-", sp500table$Ticker)
newtickers <- sp500table$Ticker
# Select only those old tickers that are not new
tickold <- tickold[!(tickold %in% newtickers)]
# Select rows with old tickers
# sp500old <- sp500old[sp500old$Ticker %in% tickold, c("co_conm","Ticker")]
# Rename columns
# colnames(sp500old) <- c("Name", "Ticker")
# sp500old$Name <- stringr::str_to_title(sp500old$Name)
# Add columns
# sp500old$Identifier <- rep_len(NA, NROW(sp500old))
# sp500old$SEDOL <- rep_len(NA, NROW(sp500old))
# Combine the tickers
sp500table <- rbind(sp500table[, 1:4], sp500old[sp500old$Ticker %in% tickold, ])
sp500table <- sp500table[, c("Ticker", "Name", "Identifier", "SEDOL")]

# Save the tickers
write.csv(sp500table, file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv", row.names=FALSE)



##############
# (old) Read CSV file with S&P500 constituents

sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
dupv <- table(sp500table$gvkey)
dupv <- dupv[dupv > 1]
dupv <- sp500table[match(as.numeric(names(dupv)), sp500table$gvkey), ]
# Select unique gvkeys
keyv <- unique(sp500table$gvkey)
# foo <- sp500table[match(keyv, sp500table$gvkey), ]
# Save gvkeys into text file using the function cat()
cat(keyv, file="/Users/jerzy/Develop/data/WRDS/gvkeys.txt", sep="\n")
# Select unique cusips and remove empty cusips
cusipv <- unique(sp500table$co_cusip)
cusipv <- cusipv[-which(cusipv == "")]
# Some cusips are empty
which(cusipv == "")
# Remove empty cusips
cusipv <- cusipv[-which(cusipv == "")]
cat(cusipv, file="/Users/jerzy/Develop/data/WRDS/sp500_cusips.txt", sep="\n")
# Find the rows corresponding to the cusipv
rowv <- sp500table[match(cusipv, sp500table$co_cusip), ]
# Find the rows corresponding to duplicate gvkeys
dupv <- table(rowv$gvkey)
dupv <- dupv[dupv > 1]
dupv <- rowv[rowv$gvkey %in% as.numeric(names(dupv)), ]
# Select unique sp500 tickers
tickv <- unique(sp500table$Ticker)



##############
# Read CSV file with S&P500 OHLC prices

# Read CSV file with S&P500 constituents
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv", stringsAsFactors=FALSE)
# Select unique cusips and remove empty cusips
cusipv <- unique(sp500table$co_cusip)
cusipv <- cusipv[-which(cusipv == "")]
# Load OHLC prices from CSV file downloaded from WRDS by cusip
pricev <- read.csv(file="/Users/jerzy/Develop/data/WRDS/sp500_prices_bycusip.csv", stringsAsFactors=FALSE)
# pricev contains cusips not in cusipv
cusipv <- unique(pricev$cusip)
NROW(cusipv); NROW(cusipv)
# Select data only for cusipv
pricev <- pricev[pricev$cusip %in% cusipv, ]
# pricev contains tickers not in tickv
tickers <- unique(pricev$tic)
NROW(tickv); NROW(tickers)
# Select data only for tickv
pricev <- pricev[pricev$tic %in% tickv, ]
# Create new data environment
sp500env <- new.env()
# Read names table from csv file
names_table <- read.csv(file="/Users/jerzy/Develop/data/WRDS/compustat_table.csv", stringsAsFactors=FALSE)
# Perform OHLC aggregations by cusip column
pricev <- split(pricev, pricev$cusip)
pricep <- lapply(pricev, format_ohlc, environ_ment=sp500env)
plot(quantmod::Cl(sp500env$MSFT))
# Check if time indices are OK
foo <- eapply(sp500env, function(x) xts::isOrdered(index(x)))
sum(!unlist(foo))
# Save OHLC prices to .RData file
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")



##############
# Save OHLC prices for the most liquid S&P500 stocks.

# Load the daily OHLC prices for S&P500 stocks
load(file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

# Calculate the daily trading volumes
volumv <- eapply(sp500env, quantmod::Vo)
volumv <- do.call(cbind, volumv)
colnames(volumv) <- rutils::get_name(colnames(volumv))

# Calculate the total trading volumes
volumt <- sapply(volumv, sum, na.rm=TRUE)
volumt <- sort(volumt, decreasing=TRUE)

# Calculate the symbols of the 200 most liquid stocks
# in every year, since 1999.

yearv <- as.character(1999:2024)
volumy <- sapply(yearv, function(yearn) {
  volumy <- sapply(volumv[yearn], sum, na.rm=TRUE)
  names(head(sort(volumy, decreasing=TRUE), 200))
}) # end sapply

# Copy the OHLC prices for all the top stocks with
# highest liquidity into an environment called sp500top.

symbolv <- sort(unique(c(volumy)))
sp500top <- as.list(sp500env)[symbolv]
sp500top <- as.environment(sp500top)
save(sp500top, file="/Users/jerzy/Develop/lecture_slides/data/sp500top.RData")

## Calculate the stock prices and log returns from OHLC prices
pricestock <- eapply(sp500top, quantmod::Cl)
pricestock <- rutils::do_call(cbind, pricestock)
colnamev <- rutils::get_name(colnames(pricestock))
colnames(pricestock) <- colnamev
retstock <- xts::diff.xts(log(pricestock))
## Save the prices and returns
save(pricestock, file="/Users/jerzy/Develop/lecture_slides/data/sp500_pricestop.RData")
save(retstock, file="/Users/jerzy/Develop/lecture_slides/data/sp500_returnstop.RData")



##############
# Load S&P500 constituent stock prices from .RData file
# and calculate the daily percentage returns.

# Load OHLC prices from .RData file
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

## Calculate the stock prices from OHLC data
pricestock <- eapply(sp500env, quantmod::Cl)
pricestock <- rutils::do_call(cbind, pricestock)
# Modify column names
symbolv <- rutils::get_name(colnames(pricestock))
# colnamev <- do.call(rbind, strsplit(colnames(pricestock), split="[.]"))[, 1]
colnames(pricestock) <- symbolv
# No: Carry forward and backward non-NA prices - no only forward because backward skews volatility
# Don't carry prices forward because it skews correlations
# sum(is.na(pricestock))
# pricestock <- zoo::na.locf(pricestock, na.rm=FALSE)
# pricestock <- zoo::na.locf(pricestock, fromLast=TRUE)

# Scrub the prices using a three-point classifier (tri-filter)
pricel <- lapply(pricestock, function(pricev) {
  pricelog <- log(pricev)
  pricelag <- rutils::lagit(pricelog)
  pricelag[1] <- pricelag[2]
  pricadv <- rutils::lagit(pricelog, lagg=-1)
  pricadv[NROW(pricadv)] <- pricadv[NROW(pricadv)-1]
  diffl <- ifelse(abs(pricelag-pricadv) < 0.01, 0.01, abs(pricelag-pricadv))
  priced <- abs((pricelog - 0.5*(pricelag+pricadv))/diffl)
  indeks <- which(priced > 10)
  pricev[indeks] <- as.numeric(pricev[indeks-1])
  pricev
}) # end lapply
pricestock <- do.call(cbind, pricel)


## Calculate log returns of the S&P500 constituent stocks
retstock <- xts::diff.xts(log(pricestock))
# Remove the first row of NA returns
retstock <- retstock[-1, ]

# retstock[1, ] <- 0.01
# Or
# retstock <- lapply(pricestock, function(x)
#   xts::diff.xts(x)/rutils::lagit(x))
# retstock <- rutils::do_call(cbind, retstock)
# Calculate percentage returns by accounting for extra time over weekends
# retstock <- (24*3600)*xts::diff.xts(pricestock)/rutils::lagit(pricestock)/xts::diff.xts(.index(pricestock))

# First approach - allow for NA prices
# Get the symbols with at least 2000 days of prices and a recent price
endd <- end(pricestock)
symbolg <- unlist(sapply(pricestock, function(pricev) {
  pricev <- na.omit(pricev)
  if ((NROW(pricev) > 2000) & (end(pricev) == endd)) {
    rutils::get_name(colnames(pricev))
  }
}))  # end lapply

# Second approach - don't allow NA prices
# Get the symbols without any NA prices
symbolg <- unlist(lapply(pricestock, function(pricev) {
  if ((sum(is.na(pricev)) == 0)) {
    return(rutils::get_name(colnames(pricev)))
  } # end if
}))  # end lapply

## Select a random sample of 100 prices and returns of the S&P500 constituent stocks
set.seed(1121)
samplev <- sample(NROW(symbolg), s=100, replace=FALSE)
pricestock100 <- pricestock[, symbolg[samplev]]
retstock100 <- retstock[, symbolg[samplev]]


# Old code
# set.seed(1121)
# samplev <- sample(NROW(symbolg), s=100, replace=FALSE)
# pricestock100 <- mget(symbolg[samplev], sp500env)
# pricestock100 <- lapply(pricestock100, quantmod::Cl)
# pricestock100 <- rutils::do_call(cbind, pricestock100)
# colnamev <- rutils::get_name(colnames(pricestock100))
# colnames(pricestock100) <- colnamev
# retstock100 <- xts::diff.xts(log(pricestock100))

## Save the prices and returns
save(pricestock, pricestock100, file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")
save(retstock, retstock100, file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")

## Experimental
## Calculate scaled returns using price range - experimental
returns_scaled <- eapply(sp500env, function(ohlc) {
  ohlc <- log(ohlc)
  # openp <- quantmod::Op(ohlc)
  highp <- quantmod::Hi(ohlc)
  lowp <- quantmod::Lo(ohlc)
  closep <- quantmod::Cl(ohlc)
  # Scale returns using price range
  returns <- xts::diff.xts(closep)
  returns[1, ] <- 0
  ranged <- as.numeric(highp - lowp)
  ranged <- ifelse(ranged == 0, 1, ranged)
  # returns <- ifelse(ranged>0, returns/ranged, 0)
  returns <- returns/ranged
  # returns[is.na(returns)] <- 0
  zoo::na.locf(returns, na.rm=FALSE)
})  # end eapply

returns_scaled <- rutils::do_call(cbind, returns_scaled)
returns_scaled[is.na(returns_scaled)] <- 0
sum(is.na(returns_scaled))
sum(!is.finite(returns_scaled))
# returns_scaled <- zoo::na.locf(returns_scaled, na.rm=FALSE)
# returns_scaled <- zoo::na.locf(returns_scaled, fromLast=TRUE)
colnames(returns_scaled) <- colnamev

returns100_scaled <- returns_scaled[, samplev]

## Experimental end



##############
# Read minute OHLC stock prices from CSV files and coerce them into xts.

# Load time series data from CSV file
ohlc <- data.table::fread("/Volumes/external/Develop/data/spy_minutes.csv")
datev <- unname(unlist(ohlc[, 2]/1e3))
datev <- as.POSIXct(datev, tz="UTC", origin="1970-01-01")
datev <- lubridate::force_tz(datev, "America/New_York")
ohlc <- xts::xts(ohlc[, -(1:2)], order.by=datev)
save(ohlc, file="/Volumes/external/Develop/data/spy_minutes.RData")



##############
## Download VIX CSV files from CBOE

# Old stuff - ignore?
# Download text data from URL - doesn't download data - just html code
foo <- RCurl::getURL("https://markets.cboe.com/us/futures/market_statistics/historical_data/")
foo <- readLines("https://markets.cboe.com/us/futures/market_statistics/historical_data/")
foo <- read.csv("https://markets.cboe.com/us/futures/market_statistics/historical_data/")
class(foo)
NROW(foo)
bar <- grep(glob2rx("*.csv"), foo)
# end old stuff

# Read CBOE futures expiration dates
datev <- read.csv(file="/Users/jerzy/Develop/R/data/futures_expiration_dates_codes.csv",
                  stringsAsFactors=FALSE, row.names=1)
dirin <- "C:/Users/Jerzy/Downloads/vix_data"
# dir.create(dirin)
symbolv <- rownames(datev)
# Select only some symbolv
indeks <- 19:NROW(symbolv)
symbolv <- symbolv[indeks]
filev <- file.path(dirin, paste0(symbolv, ".csv"))
filelog <- file.path(dirin, "log_file.txt")
urlcboe <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
urls <- paste0(urlcboe, datev[indeks, 1])
# Download files in a loop
for (it in seq_along(urls)) {
  tryCatch(  # Warning and error handler
    download.file(urls[it], destfile=filev[it], quiet=TRUE),
    # Warning handler captures warning condition
    warning=function(warning_cond) {
      cat(paste("warning handler: ", warning_cond, "\n"), file=filelog, append=TRUE)
    },  # end warning handler
    # Error handler captures error condition
    error=function(error_cond) {
      cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
    },  # end error handler
    finally=cat(paste("Processing file name =", filev[it], "\n"), append=TRUE)
  )  # end tryCatch
}  # end for



## Read CBOE futures data from CSV files

# Define utility function
# last <- function(x) x[NROW(x)]

# Load VIX futures data from binary file
load(file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")
colnamev <- colnames(env_vix$VXF8)

# Get all CSV file names in the dirin directory
# dirin <- "C:/Users/Jerzy/Downloads/vix_data"
dirin <- "/Users/jerzy/Develop/data/vix_data/"
filev <- Sys.glob(paste(dirin, "VX*.csv", sep="/"))

env_vixn <- new.env()

# Loop over the filev, load data from CSV files,
# and copy the OHLC prices to env_vixn
for (filen in filev) {
  # Load time series data from CSV file
  ohlc <- read.zoo(file=filen, header=TRUE, sep=",")
  colnames(ohlc)[8] <- colnamev[5]
  ohlc <- ohlc[, colnamev]
  # Coerce data to numeric and make xts
  ohlc <- xts(apply(ohlc, 2, as.numeric), index(ohlc))
  # Create sym bol from file name
  symboln <- strsplit(filen, split="/")[[1]]
  symboln <- strsplit(last(symboln), split="[.]")[[1]][1]
  assign(x=symboln, value=ohlc, envir=env_vixn)
}  # end for

# Copy from env_vixn to env_vix
rm(VXN8, envir=env_vixn)
symbolv <- ls(env_vixn)
for (symboln in symbolv) {
  rm(symboln, envir=env_vix)
  assign(x=symboln, value=get(symboln, envir=env_vixn), envir=env_vix)
}  # end for

save(env_vix, file="/Users/jerzy/Develop/data/vix_data/vix_cboe.RData")



## Load futures data from RData files

# Read the symbolv
symbolv <- scan(file="/Users/jerzy/Develop/data/bar_data/etf_symbolv.txt", what=character(), sep=",")
# Specify the file directory
dirin <- "/Users/jerzy/Develop/data/bar_data/"
# Specify new environment for data
etfenv <- new.env()
# Specify the file names
# filev <- paste0(dirin, symbolv, ".RData")

# Load data in a loop and copy into etfenv
for (symboln in symbolv) {
  # Specify the file name
  filen <- paste0(dirin, symboln, ".RData")
  loadv <- load(file=filen)
  assign(x=symboln, value=get(loadv), envir=etfenv)
}  # end for


## Combine the ETF series of prices into a single xts series and save it into etfenv

# Extract only first 4 OHLC price columns from each ETF series
assign(x="ohlc",
       value=rutils::do_call(cbind, eapply(etfenv, function(xtes) xtes[, 1:4])),
       envir=etfenv)
# ohlc <- rutils::do_call(cbind, eapply(etfenv, function(xtes) xtes[, 1:4]))
etfenv$ohlc <- na.omit(etfenv$ohlc)
# Subset to trading hours
etfenv$ohlc <- etfenv$ohlc["T09:00:00/T16:30:00"]
# Save the OHLC prices to binary file
save(etfenv, file=paste0(dirin, "etf_series.RData"))



## Load futures data from binary files and combine into a single xts series
# First load ES1 data and extract only first 4 OHLC price columns
load(file="/Users/jerzy/Develop/data/ES1.RData")
com_bo <- ohlc[, 1:4]
colnames(com_bo) <- paste0("ES1.", colnames(com_bo))
# Next load TU1 data and cbind it to ES1 data
load(file="/Users/jerzy/Develop/data/TU1UST2yr.RData")
ohlc <- ohlc[, 1:4]
colnames(ohlc) <- paste0("TU1.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)
# Next load TY1 data and cbind it to ES1 data
load(file="/Users/jerzy/Develop/data/TY1UST10yr.RData")
ohlc <- ohlc[, 1:4]
colnames(ohlc) <- paste0("TY1.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)
# Next load UX1 data and cbind it to ES1 data
load(file="/Users/jerzy/Develop/data/UX1_VIX.RData")
ohlc <- ohlc[, 1:4]
colnames(ohlc) <- paste0("UX1.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)
load(file="/Users/jerzy/Develop/data/UX2_VIX.RData")
ohlc <- ohlc[, 1:4]
# Next load UX1 data and cbind it to ES1 data
colnames(ohlc) <- paste0("UX2.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)

# Combine into a single xts series
ohlc <- na.omit(com_bo)
# Save the OHLC prices to binary file
save(com_bo, file="/Users/jerzy/Develop/data/combined.RData")
# load(file="/Users/jerzy/Develop/data/combined.RData")

# plot dygraph
label_s <- c("TY1.Close", "TU1.Close")
# dygraphs::dygraph(cbind(closep, datenv())["2018-02-09"], main="OHLC Technicals Strategy") %>%
dygraphs::dygraph(ohlc[endpoints(ohlc, on="hours"), label_s], main="OHLC Data") %>%
  dyAxis("y", label=label_s[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=label_s[2], independentTicks=TRUE) %>%
  dySeries(label_s[2], axis="y2", col=c("blue", "red"))



##############
# Read daily futures prices from CSV files and save
# them into an environment.
# The OHLC futures prices were collected from IB on
# consecutive days and were saved into CSV files.

# Set parameters for directory with CSV files
symboln <- "ES"
dirin <- "/Users/jerzy/Develop/data"
setwd(dir=dirin)

# Get all CSV file names in the dirin directory
filev <- Sys.glob(paste(dirin, "*.csv", sep="/"))
# Subset to only symbol file names
filev <- filev[grep(symboln, filev)]
# Subset to only files created after "2018-10-01"
cutoff <- as.POSIXct("2018-10-01", tz="America/New_York", origin="1970-01-01")
filev <- filev[file.info(filev)$mtime > cutoff]
# Subset to only files created before "2019-06-30"
cutoff <- as.POSIXct("2019-06-30", tz="America/New_York", origin="1970-01-01")
filev <- filev[file.info(filev)$mtime < cutoff]
# Exclude "ESTSY"
filev <- filev[-1]

# Create new environment for data
datenv <- new.env()

# Loop over the filev, load data from CSV files,
# and save the OHLC prices to binary files
countn <- 1
for (filen in filev) {
  cat("Counter: ", countn, "\n")
  # Load time series data from CSV file
  ohlc <- data.table::fread(filen)
  data.table::setDF(ohlc)
  if (!is.numeric(ohlc[1, 1])) {
    # Remove rows with non-numeric datestamps
    ohlc <- ohlc[!is.na(as.numeric(ohlc[, 1])), ]
    ohlc <- sapply(ohlc, as.numeric)
  }  # end if
  ohlc <- xts::xts(ohlc[, -1], order.by=as.POSIXct(ohlc[, 1], tz="America/New_York", origin="1970-01-01"))
  # name <- paste0("ES", countn)
  assign(x=paste0(symboln, countn), value=ohlc, envir=datenv)
  countn <- countn + 1
}  # end for

# Inspect the data
ls(datenv)
do.call(rbind, eapply(datenv, dim))
save(datenv, file="datenv.RData")



##############
# Chain together futures prices and save them into
# an .RData file.

# Define function for chaining futures prices from
# consecutive days.
# It adjusts prices by adding the difference in Close
# prices, instead of multiplying them by a ratio.
# No need for this: Fix chain2() by using ratio, as in slide Chaining Together Futures Prices in markets_trading.Rnw
# https://www.interactivebrokers.com/en/software/tws/usersguidebook/technicalanalytics/continuous.htm

chain2 <- function(ohlc1, ohlc2) {
  if (end(ohlc1) < start(ohlc2)) {
    diffv <- as.numeric(ohlc2[start(ohlc2), 4]) - as.numeric(ohlc1[end(ohlc1), 4])
    ohlc1[, c(1:4, 6)] <- ohlc1[, c(1:4, 6)] + diffv
    return(rbind(ohlc1, ohlc2))
  } else if (end(ohlc2) < start(ohlc1)) {
    diffv <- as.numeric(ohlc1[start(ohlc1), 4]) - as.numeric(ohlc2[end(ohlc2), 4])
    ohlc2[, c(1:4, 6)] <- ohlc2[, c(1:4, 6)] + diffv
    return(rbind(ohlc2, ohlc1))
  } else {
    warning("Overlapping data")
    return(NULL)
  } # end if
}  # end chain2

chaind <- chain2(datenv$ES1, datenv$ES2)

# Chain the data
ohlc <- rutils::do_call(chain2, as.list(datenv)[order(sapply(datenv, start))])
dim(ohlc)
colnames(ohlc) <- paste(symboln, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count"), sep=".")

# Create new time index
indeks <- seq.int(from=5*((as.numeric(Sys.time())-NROW(ohlc)) %/% 5), by=5, length.out=NROW(ohlc))
indeks <- as.POSIXct(indeks, tz="America/New_York", origin="1970-01-01")
ohlc <- xts::xts(coredata(ohlc), indeks)
sum(is.na(ohlc))

library(dygraphs)
dygraphs::dygraph(xts::to.minutes(ohlc)[, 1:4]) %>% dyCandlestick()

datenv$ohlc <- ohlc
save(datenv, file=paste0(paste(symboln, "datenv", sep="_"), ".RData"))
save(ohlc, file=paste0(symboln, "_ohlc.RData"))



##############
# Load futures OHLC prices from RData file.

# Set parameters for directory with CSV files
symboln <- "ES"
dirin <- "/Users/jerzy/Develop/data/ib_data"
setwd(dir=dirin)

load(file=paste0("/Users/jerzy/Develop/data/ib_data/", symboln, "_ohlc.RData"))
dim(ohlc)



##############
# Chain together VIX futures prices in a loop.
# Perform a for() loop, and one-by-one add to chaind
# the VIX futures prices given by the remaining symbolv.
# Hint: Adapt code from the slide: Chaining Together Futures Prices.

## This code is for chaining by starting at the last symbolv.
# It gives a slightly different answer from the homework.

chaind <- get(last(symbolv), envir=env_vixn)

for (symboln in rev(symbolv)[-1]) {
  cat("Chaining the symbol: ", symboln, "\n")
  # Get data for symboln
  ohlc <- get(symboln, envir=env_vixn)
  # Calculate end date of ohlc
  endd <- end(ohlc)
  startd <- (endd-30)
  # Calculate start date of chaind
  # startd <- start(chaind)
  # cbind overlapping volume data of ohlc and chaind, between startd and endd
  overlp <- paste0(startd, "/", endd)
  # volumes <- cbind(Vo(chaind), Vo(ohlc))[paste0(startd, "/", endd)]
  # volumes <- na.omit(volumes)
  # Find date when volume of ohlc first exceeds chaind
  exceedv <- (Vo(ohlc[overlp]) < Vo(chaind[overlp]))
  if (sum(exceedv) > 0) {
    indeks <- match(TRUE, exceedv)
    indeks <- zoo::index(exceedv[indeks])
    # Scale the prices
    factv <- as.numeric(quantmod::Cl(chaind[indeks])/quantmod::Cl(ohlc[indeks]))
  } else {
    indeks <- NROW(exceedv)
    indeks <- zoo::index(exceedv[indeks])
    # Scale the prices
    factv <- 1
  }  # end if
  ohlc[, 1:4] <- factv*ohlc[, 1:4]
  # Chain ohlc to chaind
  chaind <- rbind(ohlc[index(ohlc) <= indeks],
                    chaind[index(chaind) > indeks])
}  # end for

# Rename the column names
colnames(chaind) <- c("Open", "High", "Low", "Close", "Volume")



##############
# Download ETF OHLC prices from Alpha Vantage and save them
# to an .RData file.

library(rutils)  # Load package rutils
# Select ETF symbolv for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
             "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
             "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
             "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV", "AIEQ", "QQQ")

# Create environment for data
etfenv <- new.env()

# Copy symbolv into etfenv
# symbolv <- ls(etfenv)
etfenv$symbolv <- symbolv

# Boolean vector of symbolv already downloaded
isdown <- symbolv %in% ls(etfenv)
# Download data for symbolv using single command - creates pacing error - not with new key
getSymbols.av(symbolv, adjust=TRUE, env=etfenv, from="1990-01-01",
              auto.assign=TRUE, output.size="full", api.key="BDOPARDCGRT7C5JZ")

# Alpha Vantage doesn't adjust VTI, VXX, and SVXY prices - Download from Tiingo
ohlc <- quantmod::getSymbols.tiingo("VTI", adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
etfenv$VTI <- ohlc
ohlc <- quantmod::getSymbols.tiingo("VXX", adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
etfenv$VXX <- ohlc
ohlc <- quantmod::getSymbols.tiingo("SVXY", adjust=TRUE, from="1990-01-01", auto.assign=FALSE, output.size="full", api.key="d84fc2a9c5bde2d68e33034f65a838092c6b9f10")
etfenv$SVXY <- ohlc

# Or
# Download data from Alpha Vantage using while loop
nattempts <- 0  # number of download attempts
while (((sum(!isdown)) > 0) & (nattempts<10)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  cat("Download attempt = ", nattempts, "\n")
  for (symboln in na.omit(symbolv[!isdown][1:5])) {
    cat("Processing: ", symboln, "\n")
    tryCatch(  # With error handler
      quantmod::getSymbols.av(symboln, adjust=TRUE, env=etfenv, from="1990-01-01", auto.assign=TRUE, output.size="full", api.key="BDOPARDCGRT7C5JZ"),
      # Error handler captures error condition
      error=function(error_cond) {
        print(paste("error handler: ", error_cond))
      },  # end error handler
      finally=print(paste("symbol=", symboln))
    )  # end tryCatch
  }  # end for
  # Update vector of symbolv already downloaded
  isdown <- symbolv %in% ls(etfenv)
  cat("Pausing 1 minute to avoid pacing...\n")
  Sys.sleep(65)
}  # end while


# Adjust OHLC prices if needed
for (symboln in symbolv) {
  cat("Processing: ", symboln, "\n")
  ohlc <- etfenv[[symboln]]
  # Calculate price adjustment vector
  factv <- as.numeric(Ad(ohlc)/Cl(ohlc))
  # Adjust OHLC prices
  ohlc[, 1:4] <- factv*ohlc[, 1:4]
  assign(symboln, ohlc, etfenv)
}  # end for

# Extract Close prices
prices <- eapply(etfenv, quantmod::Cl)
# Or
# prices <- lapply(mget(etfenv$symbolv, etfenv), quantmod::Cl)
prices <- do.call(cbind, prices)

# Drop ".Close" from colnamev
colnames(prices) <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]

# Calculate the log returns
returns <- xts::diff.xts(log(prices))
returns[1, ] <- 0
# Or
# returns <- lapply(prices, function(xtes) {
#   xts::diff.xts(log(xtes))
# })  # end lapply
# returns <- do.call(cbind, returns)

# Copy prices and returns into etfenv
etfenv$prices <- prices
etfenv$returns <- returns

# Calculate the risk-return statistics
riskstats <- PerformanceAnalytics::table.Stats(returns)
# Transpose the data frame
riskstats <- as.data.frame(t(riskstats))
# Add Name column
riskstats$Name <- rownames(riskstats)
# Copy riskstats into etfenv
etfenv$riskstats <- riskstats

# Calculate the beta, alpha, Treynor ratio, and other performance statistics
capmstats <- PerformanceAnalytics::table.CAPM(Ra=returns[, symbolv],
                         Rb=returns[, "VTI"], scale=252)
colnamev <- strsplit(colnames(capmstats), split=" ")
colnamev <- do.call(cbind, colnamev)[1, ]
colnames(capmstats) <- colnamev
capmstats <- t(capmstats)
capmstats <- capmstats[, -1]
colnamev <- colnames(capmstats)
whichv <- match(c("Annualized Alpha", "Information Ratio", "Treynor Ratio"), colnamev)
colnamev[whichv] <- c("Alpha", "Information", "Treynor")
colnames(capmstats) <- colnamev
capmstats <- capmstats[order(capmstats[, "Alpha"], decreasing=TRUE), ]
# Copy capmstats into etfenv
etfenv$capmstats <- capmstats

# Save ETF data to .RData file
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
# Copy file to C:\Develop\R\rutils\data\etf_data.RData



##############
# Download multiple symbols from Bloomberg

# install.packages("Rblpapi")
library(Rblpapi)
# Connect R to Bloomberg
bbg_connect <- blpConnect()

## Download daily historical OHLC prices and volume for SPX Index and XLU ETF
# bbg_symbolv <- "SPX Index"
bbg_symbolv <- c("SPX Index", "XLU US Equity")
bbg_fields <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST", "VOLUME")
startd <- as.Date("2017-08-01")
dirin <- "/Users/jerzy/Develop/data/bbg_data"
filev <- file.path(dirin,
  paste0(gsub(bbg_symbolv, pattern=" ", replacement="_"), ".csv"))

# bbg_data <- bdh(securities = bbg_symbolv,
#                 fields = bbg_fields,
#                 start.date = startd)

# Download data from Bloomberg in a loop
lapply(seq_along(bbg_symbolv), function(indeks) {
  symboln <- bbg_symbolv[indeks]
  bbg_data <- xts::as.xts(Rblpapi::bdh(securities = symboln,
                                       fields = bbg_fields,
                                       start.date = startd))
  filen <- file.path(dirin, paste0(gsub(symboln, pattern=" ", replacement="_"), ".csv"))
  zoo::write.zoo(bbg_data, file = filen, sep=",")
  symboln
})  # end lapply



##############
# Load data from CSV files

## Load time series data from a single CSV file

pricev <- xts::as.xts(zoo::read.zoo(
  file=file.path("/Users/jerzy/Develop/data", "data prices close 2017-08-31.csv"),
  header=TRUE, sep=",", FUN=as.Date, format="%m/%d/%Y"))
# overwrite NA values
pricev <- rutils::na_locf(pricev)
pricev <- rutils::na_locf(pricev, from_last=TRUE)
symbolv <- c("XLP", "XLU")
pricev <- pricev[, symbolv]



## Load time series data from CSV files into an environment.

# Create new environment for data
datenv <- new.env()
dirin <- "/Users/jerzy/Develop/data/bbg_records"
# symbolv <- c("SPX", "VIX")
# filev <- paste0(symbolv, ".csv")
filev <- dir(dirin)
symbolv <- rutils::get_name(filev)

# Subset symbolv by removing currency symbolv
sub_symbolv <- symbolv[-grep("USD", symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("EUR", sub_symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("UST", sub_symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("JGB", sub_symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("GDB", sub_symbolv, ignore.case=TRUE)]


# Load data from CSV files into the environment
out <- rutils::get_data(symbolv=sub_symbolv,
                        data_dir=dirin,
                        data_env=datenv,
                        echo=FALSE)


## Extract the closing prices into a single xts time series

# prices <- lapply(as.list(datenv)[symbolv], quantmod::Cl)
# Flatten (cbind) prices into single xts series
# prices <- rutils::do_call(cbind, prices)

prices <- rutils::get_col(ohlc=ls(datenv), data_env=datenv)
# overwrite NA values
prices <- rutils::na_locf(prices)
prices <- rutils::na_locf(prices, from_last=TRUE)
# Save column names
colnamev <- rutils::get_name(colnames(prices))




##############
# Load and save OHLC prices

library(HighFreq)

## Load ES1 futures data from binary file
load(file="/Users/jerzy/Develop/data/ES1.RData")
# or
# Load ES1 futures data from CSV file
ohlc <- read.zoo(file="/Users/jerzy/Develop/data/bar_data/ES1.csv",
                  header=TRUE, sep=",",
                  drop=FALSE, format="%Y-%m-%d %H:%M",
                  FUN=as.POSIXct, tz="America/New_York")
# Coerce to xts series
ohlc <- as.xts(ohlc)
# Subset to trading hours
ohlc <- ohlc["T09:00:00/T16:30:00"]
# Save the OHLC prices to binary file
save(ohlc, file="/Users/jerzy/Develop/data/ES1.RData")



## Read IB futures data from CSV files

# Read file names
filev <- scan(file="/Users/jerzy/Develop/data/bar_data/etf_filev.txt", what=character(), sep=",")

# Remember the cwd
dirname <- getwd()
# Set the cwd to the file directory
dirin <- strsplit(filev[1], split="/")[[1]]
dirin <- dirin[-NROW(dirin)]
dirin <- paste(dirin, collapse="/")
# or
# dirin <- do.call(file.path, as.list(dirin))
setwd(dir=dirin)

# Loop over the filev, load data from CSV files,
# and save the OHLC prices to binary files
for (filen in filev) {
  filen <- strsplit(filen, split="/")[[1]]
  filen <- filen[NROW(filen)]
  # Load time series data from CSV file
  ohlc <- read.zoo(file=filen,
                    header=TRUE, sep=",",
                    drop=FALSE, format="%Y-%m-%d %H:%M",
                    FUN=as.POSIXct, tz="America/New_York")
  # Coerce to xts series
  ohlc <- as.xts(ohlc)
  symboln <- strsplit(filen, split="[.]")[[1]][1]
  # Rename column names
  colnames(ohlc) <- paste(symboln, colnames(ohlc), sep=".")
  # Subset to trading hours
  # ohlc <- ohlc["T09:00:00/T16:30:00"]
  # Save the OHLC prices to binary file
  save(ohlc, file=paste0(symboln, ".RData"))
}  # end for

# Restore the cwd
setwd(dir=dirname)



##############
# Download TAP ETF OHLC prices from WRDS to CSV file, read
# it, and format into xts series.
# WRDS query name EMM_OHLC
# https://wrds-web.wharton.upenn.edu/wrds/ds/compd/secd/index.cfm
# Column names:
# datadate ajexdi cshtrd  prccd  prchd  prcld prcod trfd
# ajexdi and trfd columns are adjustment factors.

# Read CSV file with TAP OHLC prices
ohlc <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/TAP.csv", stringsAsFactors=FALSE)
# ohlc contains cusips not in cusipv
cusips <- unique(ohlc$cusip)
cusips %in% cusipv
# Select data only for cusipv
ohlc <- ohlc[ohlc$cusip %in% cusipv, ]
# ohlc contains tickers not in tickv
tickers <- unique(ohlc$tic)
tickers %in% tickv
# Select data only for tickv
ohlc <- ohlc[ohlc$tic %in% tickv, ]
# Select ticker from sp500table
symboln <- sp500table$co_tic[match(ohlc$gvkey[1], sp500table$gvkey)]
# Plot adjusted close prices
# plot(ohlc[, "prcld"]/ohlc[, "ajexdi"]*ohlc[, "trfd"], t="l")
# Adjustment factor
adjfact <- drop(ohlc[, c("ajexdi")])
# Daily total return factor
trfact <- drop(ohlc[, "trfd"])
# Extract dates index
indeks <- drop(ohlc[, "datadate"])
indeks <- lubridate::ymd(indeks)
# ohlc <- ohlc[, -match(c("ajexdi", "trfd"), colnames(ohlc))]
# Select only OHLCV data
ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
colnames(ohlc) <- paste(symboln, c("Open", "High", "Low", "Close", "Volume"), sep=".")
# Coerce to xts series
ohlc <- xts::xts(ohlc, indeks)
# Fill initial Open NA prices
is_na <- is.na(ohlc[, 1])
ohlc[is_na, 1] <- (ohlc[is_na, 2] + ohlc[is_na, 3])/2
sum(is.na(ohlc))
# Adjust all the prices
ohlc[, 1:4] <- trfact*ohlc[, 1:4]/adjfact/trfact[NROW(trfact)]
ohlc <- na.omit(ohlc)
xts::isOrdered(zoo::index(ohlc))
plot(quantmod::Cl(ohlc))
head(ohlc)
tail(ohlc)



##############
# Define formatting function for OHLC prices.
# Download from WRDS into CSV files.
# Output is an OHLCV xts series.

format_ohlc <- function(ohlc, environ_ment) {
  # symboln <- ohlc[1, "tic"]
  symboln <- sp500table$co_tic[match(ohlc$gvkey[1], sp500table$gvkey)]
  # ohlc <- ohlc[ohlc$tic == symboln, ]
  # Adjustment factor
  adjfact <- drop(ohlc[, c("ajexdi")])
  # Daily total return factor
  trfact <- drop(ohlc[, "trfd"])
  # Fill NA values
  trfact <- ifelse(is.na(trfact), 1, trfact)
  # Extract dates index
  indeks <- drop(ohlc[, "datadate"])
  indeks <- lubridate::ymd(indeks)
  # Select only OHLCV data
  ohlc <- ohlc[, c("prcod", "prchd", "prcld", "prccd", "cshtrd")]
  colnames(ohlc) <- paste(symboln, c("Open", "High", "Low", "Close", "Volume"), sep=".")
  # Coerce to xts series
  ohlc <- xts::xts(ohlc, indeks)
  # Fill NA prices
  is_na <- is.na(ohlc[, 1])
  ohlc[is_na, 1] <- (ohlc[is_na, 2] + ohlc[is_na, 3])/2
  # Adjust the prices
  ohlc[, 1:4] <- trfact*ohlc[, 1:4]/adjfact/trfact[NROW(trfact)]
  # Copy the OHLCV data to environ_ment
  ohlc <- na.omit(ohlc)
  assign(x=symboln, value=ohlc, envir=environ_ment)
  symboln
}  # end format_ohlc


# Load OHLC prices from CSV file downloaded from CRSP
pricer <- read.csv(file="/Users/jerzy/Develop/data/WRDS/etf_prices_crsp.csv", stringsAsFactors=FALSE)

# Create new data environment
etfenv <- new.env()
# Perform OHLC aggregations by ticker symbol column
pricep <- lapply(split(pricer, pricer$tic), format_ohlc, environ_ment=etfenv)
plot(quantmod::Cl(etfenv$USO))
# Save OHLC prices to .RData file
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
# Load OHLC prices from .RData file
load("/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")



##############
# Download from WRDS the Compustat names table using package rWRDS

devtools::install_github("davidsovich/rwrds")
library(rwrds)
library(dplyr)

wrds_con <- rwrds::wrds_connect(username="jp3900", password="Wtinlke18")
# Downloads Compustat names table as dplyr
names_table <- rwrds::compustat_names(wrds=wrds_con, subset=FALSE, dl=TRUE)
dim(names_table)
write.csv(names_table, file="/Users/jerzy/Develop/data/WRDS/compustat_table.csv", row.names=FALSE)
# rm(compustat_table)
names_table <- read.csv(file="/Users/jerzy/Develop/data/WRDS/compustat_table.csv", stringsAsFactors=FALSE)
# symboln <- "VTI"
# match(symboln, names_table$tic)
# Get cusips of symbolv tickers
indeks <- match(symbolv, names_table$tic)
names(indeks) <- symbolv
etf_cusips <- names_table$cusip[indeks]
names(etf_cusips) <- symbolv
# Save cusips into text file
cat(etf_cusips, file="/Users/jerzy/Develop/data/WRDS/etf_cusips.txt", sep="\n")
# Save gvkeys into text file
etf_gvkeys <- names_table$gvkey[indeks]
names(etf_gvkeys) <- symbolv
cat(etf_gvkeys, file="/Users/jerzy/Develop/data/WRDS/etf_gvkeys.txt", sep="\n")



####################################
# Ignore below


# Coerce bbg_data from data frame to xts
# bbg_data <- xts::as.xts(bbg_data)
# need to verify date format
bbg_data <- xts::xts(bbg_data[, bbg_fields],
                     order.by = as.Date(bbg_data[, "date"], format="%Y-%m-%d"))

# Coerce bbg_data from data frame to xts
# bbg_data <- xts::as.xts(bbg_data)
# bbg_data <- xts::xts(bbg_data, order.by = as.Date(bbg_data[, "date"]))
# bbg_data
# write bbg_data to CSV file
zoo::write.zoo(bbg_data, file = filen, sep=",")


# write bbg_data to CSV files
zoo::write.zoo(bbg_data, file="bbg_data.csv", sep=",")


## Download daily historical close prices and daily volume for SPX Index and XLU ETF



# Bloomberg script for a list of symbolv

