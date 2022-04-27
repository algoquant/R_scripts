####################################
# Scripts for downloading data from external sources,
# and for loading data from files.

# Suppress spurious timezone warning message: "timezone of object is different than current timezone"
Sys.setenv(TZ=Sys.timezone())
# Load HighFreq
library(HighFreq)


###############
# Download from WRDS the Compustat names table using package rWRDS

devtools::install_github("davidsovich/rwrds")
library(rwrds)
library(dplyr)

wrds_con <- rwrds::wrds_connect(username="jp3900", password="Wtinlke18")
# Downloads Compustat names table as dplyr
names_table <- rwrds::compustat_names(wrds=wrds_con, subset=FALSE, dl=TRUE)
dim(names_table)
write.csv(names_table, file="C:/Develop/data/WRDS/compustat_table.csv", row.names=FALSE)
# rm(compustat_table)
names_table <- read.csv(file="C:/Develop/data/WRDS/compustat_table.csv", stringsAsFactors=FALSE)
# symbol <- "VTI"
# match(symbol, names_table$tic)
# Get cusips of symbolv tickers
indeks <- match(symbolv, names_table$tic)
names(indeks) <- symbolv
etf_cusips <- names_table$cusip[indeks]
names(etf_cusips) <- symbolv
# Save cusips into text file
cat(etf_cusips, file="C:/Develop/data/WRDS/etf_cusips.txt", sep="\n")
# Save gvkeys into text file
etf_gvkeys <- names_table$gvkey[indeks]
names(etf_gvkeys) <- symbolv
cat(etf_gvkeys, file="C:/Develop/data/WRDS/etf_gvkeys.txt", sep="\n")


###############
# Combine different CSV files with S&P500 constituents

# Load old CSV files with S&P500 constituents from the past
sp500old <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents2020.csv")
dim(sp500old)
# Check if there are missing tickers
sum(is.na(sp500old$co_tic))
sum(sp500old$co_tic == "")
# Remove rows with missing tickers
sp500old <- sp500old[!(sp500old$co_tic == ""), ]
# Replace dots with hyphens in tickers
sp500old$co_tic <- gsub("[.]", "-", sp500old$co_tic)
# Remove suffixes from tickers
# sp500old$co_tic <- rutils::get_name(sp500old$co_tic)
# (needs recalculating) Load unavailable tickers from Tiingo download - mergers, etc.
tickna <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/tickna.csv")
tickna <- unname(unlist(tickna))
# Select available tickers
sp500old <- sp500old[!(sp500old$co_tic %in% tickna), ]
# Extract all the old tickers
tickold <- unique(sp500old$co_tic)


# Load CSV file with current SPY holdings
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/spyholdings.csv")
# Replace dots with hyphens in tickers
sp500table$Ticker <- gsub("[.]", "-", sp500table$Ticker)
tickers <- sp500table$Ticker
# Select only those old tickers that are not new
tickold <- tickold[!(tickold %in% tickers)]
# Select rows with old tickers
sp500old <- sp500old[sp500old$co_tic %in% tickold, c("co_conm","co_tic")]
# Rename columns
colnames(sp500old) <- c("Name", "Ticker")
sp500old$Name <- stringr::str_to_title(sp500old$Name)
# Add columns
sp500old$Identifier <- rep_len(NA, NROW(sp500old))
sp500old$SEDOL <- rep_len(NA, NROW(sp500old))
# Combine the tickers
sp500table <- rbind(sp500table[, 1:4], sp500old)
sp500table <- sp500table[, c("Ticker", "Name", "Identifier", "SEDOL")]

# Save the tickers
write.csv(sp500table, file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv", row.names=FALSE)



###############
# (old) Read CSV file with S&P500 constituents

sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
duplicates <- table(sp500table$gvkey)
duplicates <- duplicates[duplicates > 1]
duplicates <- sp500table[match(as.numeric(names(duplicates)), sp500table$gvkey), ]
# Select unique gvkeys
sp500gvkeys <- unique(sp500table$gvkey)
# foo <- sp500table[match(sp500gvkeys, sp500table$gvkey), ]
# Save gvkeys into text file
cat(sp500gvkeys, file="C:/Develop/data/WRDS/sp500gvkeys.txt", sep="\n")
# Select unique cusips and remove empty cusips
sp500cusips <- unique(sp500table$co_cusip)
sp500cusips <- sp500cusips[-which(sp500cusips == "")]
# Some cusips are empty
which(sp500cusips == "")
# Remove empty cusips
sp500cusips <- sp500cusips[-which(sp500cusips == "")]
cat(sp500cusips, file="C:/Develop/data/WRDS/sp500_cusips.txt", sep="\n")
# Find the rows corresponding to the sp500cusips
rows_cusips <- sp500table[match(sp500cusips, sp500table$co_cusip), ]
# Find the rows corresponding to duplicate gvkeys
duplicates <- table(rows_cusips$gvkey)
duplicates <- duplicates[duplicates > 1]
duplicates <- rows_cusips[rows_cusips$gvkey %in% as.numeric(names(duplicates)), ]
# Select unique sp500 tickers
sp500tickers <- unique(sp500table$co_tic)



###############
# Download TAP ETF OHLC prices from WRDS to CSV file, read
# it, and format into xts series.
# WRDS query name EMM_OHLC
# https://wrds-web.wharton.upenn.edu/wrds/ds/compd/secd/index.cfm
# Column names:
# datadate ajexdi cshtrd  prccd  prchd  prcld prcod trfd
# ajexdi and trfd columns are adjustment factors.

# Read CSV file with TAP OHLC prices
ohlc <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/TAP.csv", stringsAsFactors=FALSE)
# ohlc contains cusips not in sp500cusips
cusips <- unique(ohlc$cusip)
cusips %in% sp500cusips
# Select data only for sp500cusips
ohlc <- ohlc[ohlc$cusip %in% sp500cusips, ]
# ohlc contains tickers not in sp500tickers
tickers <- unique(ohlc$tic)
tickers %in% sp500tickers
# Select data only for sp500tickers
ohlc <- ohlc[ohlc$tic %in% sp500tickers, ]
# Select ticker from sp500table
symbol <- sp500table$co_tic[match(ohlc$gvkey[1], sp500table$gvkey)]
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
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
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



###############
# Define formatting function for OHLC prices
# download from WRDS into CSV files.
# Output is an OHLCV xts series.

format_ohlc <- function(ohlc, environ_ment) {
  # symbol <- ohlc[1, "tic"]
  symbol <- sp500table$co_tic[match(ohlc$gvkey[1], sp500table$gvkey)]
  # ohlc <- ohlc[ohlc$tic == symbol, ]
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
  colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume"), sep=".")
  # Coerce to xts series
  ohlc <- xts::xts(ohlc, indeks)
  # Fill NA prices
  is_na <- is.na(ohlc[, 1])
  ohlc[is_na, 1] <- (ohlc[is_na, 2] + ohlc[is_na, 3])/2
  # Adjust the prices
  ohlc[, 1:4] <- trfact*ohlc[, 1:4]/adjfact/trfact[NROW(trfact)]
  # Copy the OHLCV data to environ_ment
  ohlc <- na.omit(ohlc)
  assign(x=symbol, value=ohlc, envir=environ_ment)
  symbol
}  # end format_ohlc


# Load OHLC prices from CSV file downloaded from CRSP
etf_prices <- read.csv(file="C:/Develop/data/WRDS/etf_prices_crsp.csv", stringsAsFactors=FALSE)

# Create new data environment
etfenv <- new.env()
# Perform OHLC aggregations by ticker symbol column
process_ed <- lapply(split(etf_prices, etf_prices$tic), format_ohlc, environ_ment=etfenv)
plot(quantmod::Cl(etfenv$USO))
# Save OHLC prices to .RData file
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")
# Load OHLC prices from .RData file
load("/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")




###############
### Download OHLC bars from Polygon
# Only 15 years of data with current plan

# Setup code
symbol <- "SPY"
startd <- as.Date("2019-01-01")
# startd <- as.Date("2021-05-01")
endd <- Sys.Date()
tspan <- "minute"
# tspan <- "day"
# Polygon API key
apikey <- "0Q2f8j8CwAbdY4M8VYt_8pwdP0V4TunxbvRVC_"

# Create url for download
urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symbol, "/range/1/", tspan, "/", startd, "/", endd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
# old - no need to Download OHLC bars from Polygon into JSON format file
# download.file(urll, destfile="/Volumes/external/Develop/data/polygon/data.json")
# Read OHLC bars from json file
# ohlc <- jsonlite::read_json("/Volumes/external/Develop/data/polygon/data.json")
# Download OHLC prices in JSON format from Polygon
ohlc <- jsonlite::read_json(urll)
class(ohlc)
NROW(ohlc)
names(ohlc)
# Extract list of prices from json object
ohlc <- ohlc$results
# Coerce from list to matrix
ohlc <- lapply(ohlc, function(x) unlist(x)[c("t","o","h","l","c","v","vw")])
ohlc <- do.call(rbind, ohlc)
# Coerce time from milliseconds to date-time
dates <- ohlc[, "t"]/1e3
dates <- as.POSIXct(dates, origin="1970-01-01")
head(dates)
tail(dates)
# Coerce from matrix to xts
ohlc <- ohlc[, -1]
colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
ohlc <- xts::xts(ohlc, order.by=dates)
head(ohlc)
tail(ohlc)
# Save to file
spyohlc <- ohlc
save(spyohlc, file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")
rm(spyohlc)


# Or using rutils::getpoly()

ohlc <- getpoly(symbol=symbol, startd=startd, apikey=apikey)
colnames(ohlc) <- paste0(symbol, ".", colnames(ohlc))


# Candlestick plot of OHLC prices
dygraphs::dygraph(ohlc["2019/", 1:4], main=paste("Candlestick Plot of", symbol, "OHLC prices")) %>% 
  dygraphs::dyCandlestick()
# Dygraphs plot of Close prices
dygraphs::dygraph(ohlc[, 4], main=paste(symbol, "Close prices")) %>%
  dySeries(colnames(ohlc[, 4]), label=symbol) %>%
  dyOptions(colors="blue", strokeWidth=1) %>%
  dyLegend(show="always", width=500)


###############
### Download daily OHLC bars for multiple symbols in a loop

## Setup code
startd <- as.Date("1990-01-01")
# startd <- as.Date("2021-05-01")
endd <- Sys.Date()
tspan <- "day"
apikey <- "UJcr9ctoMBXEBK1Mqu_KQAkUuBxLvEtE"


### Download daily OHLC bars for multiple ETF symbols in a loop

# Select ETF symbols for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
              "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
              "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
              "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")
# Create new environment for ETF data
etfenv <- new.env()


# Initialize Boolean vector of the symbols that were already downloaded
isdownloaded <- symbolv %in% ls(etfenv)

# Download data from Polygon using while loop
while (sum(!isdownloaded) > 0) {
  for (symbol in symbolv[!isdownloaded]) {
    cat("Processing:", symbol, "\n")
    tryCatch({  # With error handler
      # Download OHLC bars from Polygon
      ohlc <- getpoly(symbol=symbol, startd=startd, apikey=apikey)
      colnames(ohlc) <- paste0(symbol, ".", colnames(ohlc))
      # Save to environment
      assign(symbol, ohlc, envir=etfenv)
      Sys.sleep(1)
    },
    error={function(error_cond) print(paste("Error handler:", error_cond))},
    finally=print(paste0("symbol=", symbol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbolv already downloaded
  isdownloaded <- symbolv %in% ls(etfenv)
}  # end while

# Save OHLC prices to .RData file
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")

### Add stats to etfenv

# Extract Close prices
prices <- eapply(etfenv, quantmod::Cl)
prices <- do.call(cbind, prices)
# Drop ".Close" from colnamev
colnames(prices) <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
# Calculate the log returns
returns <- xts::diff.xts(log(prices))
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
save(etfenv, file="/Users/jerzy/Develop/lecture_slides/data/etf_data.RData")



### Download from Polygon daily OHLC bars for multiple SP500 symbols in a loop

# Select SP500 symbols
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv")
symbolv <- sp500table$Ticker
# Create new environment for SP500 data
sp500env <- new.env()

# Initialize Boolean vector of the symbols that were already downloaded
isdownloaded <- symbolv %in% ls(sp500env)

# Download data from Polygon using while loop
while (sum(!isdownloaded) > 0) {
  for (symbol in symbolv[!isdownloaded]) {
    cat("Processing:", symbol, "\n")
    tryCatch({  # With error handler
      # Download OHLC bars from Polygon
      ohlc <- getpoly(symbol=symbol, startd=startd, apikey=apikey)
      colnames(ohlc) <- paste0(symbol, ".", colnames(ohlc))
      # Save to environment
      assign(symbol, ohlc, envir=sp500env)
      Sys.sleep(1)
    },
    error={function(error_cond) print(paste("Error handler:", error_cond))},
    finally=print(paste0("symbol=", symbol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbolv already downloaded
  isdownloaded <- symbolv %in% ls(sp500env)
}  # end while

# Save OHLC prices to .RData file
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")



### Download minute bars for single symbol in a loop
# Has to be performed in batches since data limit=50000

# Setup code
symbol <- "SPY"
startd <- as.Date("2019-01-01")
# startd <- as.Date("2021-05-01")
endd <- Sys.Date()
tspan <- "minute"
# tspan <- "day"
apikey <- "SDpnrBpiRzONMJdl48r6dOo0_mjmCu6r"
dataa <- new.env()
iter <- 1

# Run loop
while (startd < endd) {
  urll <- paste0("https://api.polygon.io/v2/aggs/ticker/", symbol, "/range/1/", tspan, "/", startd, "/", endd, "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
  download.file(urll, destfile="/Volumes/external/Develop/data/polygon/data.json")
  ohlc <- jsonlite::read_json("/Volumes/external/Develop/data/polygon/data.json")
  ohlc <- ohlc$results
  # ohlc <- lapply(ohlc, unlist)
  ohlc <- lapply(ohlc, function(x) unlist(x)[c("t","o","h","l","c","v","vw")])
  ohlc <- do.call(rbind, ohlc)
  dates <- ohlc[, "t"]/1e3
  # Polygon seconds are moment in time in UTC
  dates <- as.POSIXct(dates, origin="1970-01-01", tz="America/New_York")
  ohlc <- ohlc[, -1]
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
  ohlc <- xts::xts(ohlc, order.by=dates)
  startd <- as.Date(end(ohlc))-1
  # ohlc <- rbind(ohlc, dataa)
  # ohlc <- ohlc[!duplicated(index(ohlc)), ]
  # dataa <- ohlc
  assign(paste0("ohlc", iter), ohlc, envir=dataa)
  iter <- iter + 1
  cat("Sleeping for 1 sec ... \n")
  Sys.sleep(1)
}  # end while

# Combine xts
ohlc <- do.call(rbind, as.list(dataa))
ohlc <- ohlc[!duplicated(index(ohlc)), ]
# Save xts to file
spyohlc <- ohlc
save(spyohlc, file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")
rm(spyohlc)


# Load existing minute bars of SPY prices
loadd <- load(file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")
# Or
loadd <- load(file="/Volumes/external/Develop/data/polygon/spyvxx_minutes.RData")
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



###############
# Download ETF OHLC prices from Alpha Vantage and save them
# to an .RData file.

library(rutils)  # Load package rutils
# Select ETF symbolv for asset allocation
symbolv <- c("VTI", "VEU", "EEM", "XLY", "XLP", "XLE", "XLF",
              "XLV", "XLI", "XLB", "XLK", "XLU", "VYM", "IVW", "IWB", "IWD",
              "IWF", "IEF", "TLT", "VNQ", "DBC", "GLD", "USO", "VXX", "SVXY",
              "MTUM", "IVE", "VLUE", "QUAL", "VTV", "USMV")

# Create environment for data
etfenv <- new.env()

# Copy symbolv into etfenv
# symbolv <- ls(etfenv)
etfenv$symbolv <- symbolv

# Boolean vector of symbolv already downloaded
isdownloaded <- symbolv %in% ls(etfenv)
# Download data for symbolv using single command - creates pacing error
getsymbolv.av(symbolv, adjust=TRUE, env=etfenv,
              output.size="full", api.key="T7JPW54ES8G75310")
# Download data from Alpha Vantage using while loop
nattempts <- 0  # number of download attempts
while (((sum(!isdownloaded)) > 0) & (nattempts<10)) {
  # Download data and copy it into environment
  nattempts <- nattempts + 1
  cat("Download attempt = ", nattempts, "\n")
  for (symbol in na.omit(symbolv[!isdownloaded][1:5])) {
    cat("Processing: ", symbol, "\n")
    tryCatch(  # With error handler
      quantmod::getsymbolv.av(symbol, adjust=TRUE, env=etfenv, auto.assign=TRUE, output.size="full", api.key="T7JPW54ES8G75310"),
      # Error handler captures error condition
      error=function(error_cond) {
        print(paste("error handler: ", error_cond))
      },  # end error handler
      finally=print(paste("symbol=", symbol))
    )  # end tryCatch
  }  # end for
  # Update vector of symbolv already downloaded
  isdownloaded <- symbolv %in% ls(etfenv)
  cat("Pausing 1 minute to avoid pacing...\n")
  Sys.sleep(65)
}  # end while


# Adjust OHLC prices if needed
for (symbol in symbolv) {
  cat("Processing: ", symbol, "\n")
  ohlc <- etfenv[[symbol]]
  # Calculate price adjustment vector
  ratio <- as.numeric(Ad(ohlc)/Cl(ohlc))
  # Adjust OHLC prices
  ohlc[, 1:4] <- ratio*ohlc[, 1:4]
  assign(symbol, ohlc, etfenv)
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




###############
# Read CSV file with S&P500 OHLC prices

# Read CSV file with S&P500 constituents
sp500table <- read.csv(file="/Users/jerzy/Develop/lecture_slides/data/sp500_constituents.csv", stringsAsFactors=FALSE)
# Select unique cusips and remove empty cusips
sp500cusips <- unique(sp500table$co_cusip)
sp500cusips <- sp500cusips[-which(sp500cusips == "")]
# Load OHLC prices from CSV file downloaded from WRDS by cusip
sp500_prices <- read.csv(file="C:/Develop/data/WRDS/sp500_prices_bycusip.csv", stringsAsFactors=FALSE)
# sp500_prices contains cusips not in sp500cusips
cusips <- unique(sp500_prices$cusip)
NROW(sp500cusips); NROW(cusips)
# Select data only for sp500cusips
sp500_prices <- sp500_prices[sp500_prices$cusip %in% sp500cusips, ]
# sp500_prices contains tickers not in sp500tickers
tickers <- unique(sp500_prices$tic)
NROW(sp500tickers); NROW(tickers)
# Select data only for sp500tickers
sp500_prices <- sp500_prices[sp500_prices$tic %in% sp500tickers, ]
# Create new data environment
sp500env <- new.env()
# Read names table from csv file
names_table <- read.csv(file="C:/Develop/data/WRDS/compustat_table.csv", stringsAsFactors=FALSE)
# Perform OHLC aggregations by cusip column
sp500_prices <- split(sp500_prices, sp500_prices$cusip)
process_ed <- lapply(sp500_prices, format_ohlc, environ_ment=sp500env)
plot(quantmod::Cl(sp500env$MSFT))
# Check if time indices are OK
foo <- eapply(sp500env, function(x) xts::isOrdered(index(x)))
sum(!unlist(foo))
# Save OHLC prices to .RData file
save(sp500env, file="/Users/jerzy/Develop/lecture_slides/data/sp500.RData")
# Load OHLC prices from .RData file
load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")





###############
# Load S&P500 constituent stock prices from .RData file
# and calculate the percentage daily returns scaled
# by their intraday range.

load("/Users/jerzy/Develop/lecture_slides/data/sp500.RData")

## Calculate prices from OHLC data
prices <- eapply(sp500env, quantmod::Cl)
prices <- rutils::do_call(cbind, prices)
# Carry forward and backward non-NA prices - no only forward because backward skews volatility
sum(is.na(prices))
prices <- zoo::na.locf(prices, na.rm=FALSE)
# prices <- zoo::na.locf(prices, fromLast=TRUE)
# Modify column names
colnamev <- rutils::get_name(colnames(prices))
# colnamev <- do.call(rbind, strsplit(colnames(prices), split="[.]"))[, 1]
colnames(prices) <- colnamev

## Calculate percentage returns of the S&P500 constituent stocks
returns <- xts::diff.xts(log(prices))
# Or
# returns <- lapply(prices, function(x)
#   xts::diff.xts(x)/rutils::lagit(x))
# returns <- rutils::do_call(cbind, returns)
# Calculate percentage returns by accounting for extra time over weekends
# returns <- (24*3600)*xts::diff.xts(prices)/rutils::lagit(prices)/xts::diff.xts(.index(prices))

## Select a random sample of 100 prices and returns of the S&P500 constituent stocks
set.seed(1121)
samplev <- sample(NCOL(returns), s=100, replace=FALSE)
prices100 <- prices[, samplev]
returns100 <- returns[, samplev]

## Calculate scaled returns using price range - experimental
returns_scaled <- eapply(sp500env, function(ohlc) {
  ohlc <- log(ohlc)
  # openp <- quantmod::Op(ohlc)
  highp <- quantmod::Hi(ohlc)
  lowp <- quantmod::Lo(ohlc)
  closep <- quantmod::Cl(ohlc)
  # Scale returns using price range
  returns <- xts::diff.xts(closep)
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

## Save the data
save(prices, prices100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_prices.RData")

## Save the returns
save(returns, returns100,
     file="/Users/jerzy/Develop/lecture_slides/data/sp500_returns.RData")



###############
# Read minute OHLC stock prices from CSV files and coerce them into xts.

# Load time series data from CSV file
ohlc <- data.table::fread("/Volumes/external/Develop/data/polygon/spy_minutes.csv")
dates <- unname(unlist(ohlc[, 2]/1e3))
dates <- as.POSIXct(dates, tz="UTC", origin="1970-01-01")
dates <- lubridate::force_tz(dates, "America/New_York")
ohlc <- xts::xts(ohlc[, -(1:2)], order.by=dates)
save(ohlc, file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")


###############
# Coerce time zones
# Index had America/New_York time zone while the clock time was actually UTC

load("/Volumes/external/Develop/data/polygon/spy_minutes.RData")
dates <- zoo::index(spyohlc)
# Get same moment of time in UTC time zone
dates <- lubridate::with_tz(dates, "UTC")
# Get same clock time in America/New_York time zone
dates <- lubridate::force_tz(dates, "America/New_York")
zoo::index(spyohlc) <- dates
save(spyohlc, file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")


###############
# Read daily futures prices from CSV files and save
# them into an environment.
# The OHLC futures prices were collected from IB on
# consecutive days and were saved into CSV files.

# Set parameters for directory with CSV files
symbol <- "ES"
data_dir <- "C:/Develop/data/ib_data"
setwd(dir=data_dir)

# Get all CSV file names in the data_dir directory
file_names <- Sys.glob(paste(data_dir, "*.csv", sep="/"))
# Subset to only symbol file names
file_names <- file_names[grep(symbol, file_names)]
# Subset to only files created after "2018-10-01"
cutoff <- as.POSIXct("2018-10-01", tz="America/New_York", origin="1970-01-01")
file_names <- file_names[file.info(file_names)$mtime > cutoff]
# Subset to only files created before "2019-06-30"
cutoff <- as.POSIXct("2019-06-30", tz="America/New_York", origin="1970-01-01")
file_names <- file_names[file.info(file_names)$mtime < cutoff]
# Exclude "ESTSY"
file_names <- file_names[-1]

# Create new environment for data
data_env <- new.env()

# Loop over the file_names, load data from CSV files,
# and save the bar data to binary files
count_er <- 1
for (file_name in file_names) {
  cat("count_er: ", count_er, "\n")
  # Load time series data from CSV file
  ohlc <- data.table::fread(file_name)
  data.table::setDF(ohlc)
  if (!is.numeric(ohlc[1, 1])) {
    # Remove rows with non-numeric datestamps
    ohlc <- ohlc[!is.na(as.numeric(ohlc[, 1])), ]
    ohlc <- sapply(ohlc, as.numeric)
  }  # end if
  ohlc <- xts::xts(ohlc[, -1], order.by=as.POSIXct(ohlc[, 1], tz="America/New_York", origin="1970-01-01"))
  # name <- paste0("ES", count_er)
  assign(x=paste0(symbol, count_er), value=ohlc, envir=data_env)
  count_er <- count_er + 1
}  # end for

# Inspect the data
ls(data_env)
do.call(rbind, eapply(data_env, dim))
save(data_env, file="data_env.RData")



###############
# Chain together futures prices and save them into
# an .RData file.

# Define function for chaining futures prices from
# consecutive days.
# It adjusts prices by adding the difference in Close
# prices, instead of multiplying them by a ratio.
# No need for this: Fix chain_ohlc() by using ratio, as in slide Chaining Together Futures Prices in markets_trading.Rnw
# https://www.interactivebrokers.com/en/software/tws/usersguidebook/technicalanalytics/continuous.htm

chain_ohlc <- function(ohlc1, ohlc2) {
  if (end(ohlc1) < start(ohlc2)) {
    di_ff <- as.numeric(ohlc2[start(ohlc2), 4]) - as.numeric(ohlc1[end(ohlc1), 4])
    ohlc1[, c(1:4, 6)] <- ohlc1[, c(1:4, 6)] + di_ff
    return(rbind(ohlc1, ohlc2))
  } else if (end(ohlc2) < start(ohlc1)) {
    di_ff <- as.numeric(ohlc1[start(ohlc1), 4]) - as.numeric(ohlc2[end(ohlc2), 4])
    ohlc2[, c(1:4, 6)] <- ohlc2[, c(1:4, 6)] + di_ff
    return(rbind(ohlc2, ohlc1))
  } else {
    warning("Overlapping data")
    return(NULL)
  } # end if
}  # end chain_ohlc

chain_ed <- chain_ohlc(data_env$ES1, data_env$ES2)

# Chain the data
ohlc <- rutils::do_call(chain_ohlc, as.list(data_env)[order(sapply(data_env, start))])
dim(ohlc)
colnames(ohlc) <- paste(symbol, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count"), sep=".")

# Create new time index
in_deks <- seq.int(from=5*((as.numeric(Sys.time())-NROW(ohlc)) %/% 5), by=5, length.out=NROW(ohlc))
in_deks <- as.POSIXct(in_deks, tz="America/New_York", origin="1970-01-01")
ohlc <- xts::xts(coredata(ohlc), in_deks)
sum(is.na(ohlc))

library(dygraphs)
dygraphs::dygraph(xts::to.minutes(ohlc)[, 1:4]) %>% dyCandlestick()

data_env$ohlc <- ohlc
save(data_env, file=paste0(paste(symbol, "data_env", sep="_"), ".RData"))
save(ohlc, file=paste0(symbol, "_ohlc.RData"))



###############
# Load futures OHLC prices from RData file.

# Set parameters for directory with CSV files
symbol <- "ES"
data_dir <- "C:/Develop/data/ib_data"
setwd(dir=data_dir)

load(file=paste0("C:/Develop/data/ib_data/", symbol, "_ohlc.RData"))
dim(ohlc)


###############
# Chain together VIX futures prices in a loop.
# Perform a for() loop, and one-by-one add to chain_ed
# the VIX futures prices given by the remaining symbolv.
# Hint: Adapt code from the slide: Chaining Together Futures Prices.

## This code is for chaining by starting at the last symbolv.
# It gives a slightly different answer from the homework.

chain_ed <- get(last(symbolv), envir=vix_env_new)

for (symbol in rev(symbolv)[-1]) {
  cat("Chaining the symbol: ", symbol, "\n")
  # Get data for symbol
  ohlc <- get(symbol, envir=vix_env_new)
  # Calculate end date of ohlc
  endd <- end(ohlc)
  startd <- (endd-30)
  # Calculate start date of chain_ed
  # startd <- start(chain_ed)
  # cbind overlapping volume data of ohlc and chain_ed, between startd and endd
  over_lap <- paste0(startd, "/", endd)
  # volumes <- cbind(Vo(chain_ed), Vo(ohlc))[paste0(startd, "/", endd)]
  # volumes <- na.omit(volumes)
  # Find date when volume of ohlc first exceeds chain_ed
  exceeds <- (Vo(ohlc[over_lap]) < Vo(chain_ed[over_lap]))
  if (sum(exceeds) > 0) {
    indeks <- match(TRUE, exceeds)
    indeks <- zoo::index(exceeds[indeks])
    # Scale the prices
    ratio <- as.numeric(quantmod::Cl(chain_ed[indeks])/quantmod::Cl(ohlc[indeks]))
  } else {
    indeks <- NROW(exceeds)
    indeks <- zoo::index(exceeds[indeks])
    # Scale the prices
    ratio <- 1
  }  # end if
  ohlc[, 1:4] <- ratio*ohlc[, 1:4]
  # Chain ohlc to chain_ed
  chain_ed <- rbind(ohlc[index(ohlc) <= indeks],
                    chain_ed[index(chain_ed) > indeks])
}  # end for

# Rename the column names
colnames(chain_ed) <- c("Open", "High", "Low", "Close", "Volume")



###########
# Download multiple symbolv from Bloomberg

# install.packages("Rblpapi")
library(Rblpapi)
# Connect R to Bloomberg
bbg_connect <- blpConnect()

## Download daily historical OHLC prices and volume for SPX Index and XLU ETF
# bbg_symbolv <- "SPX Index"
bbg_symbolv <- c("SPX Index", "XLU US Equity")
bbg_fields <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST", "VOLUME")
startd <- as.Date("2017-08-01")
data_dir <- "C:/Develop/data/bbg_data"
file_names <- file.path(data_dir,
  paste0(gsub(bbg_symbolv, pattern=" ", replacement="_"), ".csv"))

# bbg_data <- bdh(securities = bbg_symbolv,
#                 fields = bbg_fields,
#                 start.date = startd)

# Download data from Bloomberg in a loop
lapply(seq_along(bbg_symbolv), function(indeks) {
  symbol <- bbg_symbolv[indeks]
  bbg_data <- xts::as.xts(Rblpapi::bdh(securities = symbol,
                                       fields = bbg_fields,
                                       start.date = startd))
  file_name <- file.path(data_dir,
                         paste0(gsub(symbol, pattern=" ", replacement="_"), ".csv"))
  zoo::write.zoo(bbg_data, file = file_name, sep=",")
  symbol
})  # end lapply



###########
# Load data from CSV files

## Load time series data from a single CSV file

prices_ts <- xts::as.xts(zoo::read.zoo(
  file=file.path("C:/Develop/data",
                 "data prices close 2017-08-31.csv"),
  header=TRUE, sep=",", FUN=as.Date, format="%m/%d/%Y"))
# overwrite NA values
prices_ts <- rutils::na_locf(prices_ts)
prices_ts <- rutils::na_locf(prices_ts, from_last=TRUE)
symbolv <- c("XLP", "XLU")
prices_ts <- prices_ts[, symbolv]



## Load time series data from CSV files into an environment.

# Create new environment for data
data_env <- new.env()
data_dir <- "C:/Develop/data/bbg_records"
# symbolv <- c("SPX", "VIX")
# file_names <- paste0(symbolv, ".csv")
file_names <- dir(data_dir)
symbolv <- rutils::get_name(file_names)

# Subset symbolv by removing currency symbolv
sub_symbolv <- symbolv[-grep("USD", symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("EUR", sub_symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("UST", sub_symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("JGB", sub_symbolv, ignore.case=TRUE)]
sub_symbolv <- sub_symbolv[-grep("GDB", sub_symbolv, ignore.case=TRUE)]


# Load data from CSV files into the environment
out <- rutils::get_data(symbolv=sub_symbolv,
                        data_dir=data_dir,
                        data_env=data_env,
                        echo=FALSE)


## Extract the closing prices into a single xts time series

# prices <- lapply(as.list(data_env)[symbolv], quantmod::Cl)
# Flatten (cbind) prices into single xts series
# prices <- rutils::do_call(cbind, prices)

prices <- rutils::get_col(ohlc=ls(data_env),
                           data_env=data_env)
# overwrite NA values
prices <- rutils::na_locf(prices)
prices <- rutils::na_locf(prices, from_last=TRUE)
# Save column names
colnamev <- rutils::get_name(colnames(prices))




###############
# Load and save OHLC bar data

library(HighFreq)

## Load ES1 futures data from binary file
load(file="C:/Develop/data/ES1.RData")
# or
# Load ES1 futures data from CSV file
ohlc <- read.zoo(file="C:/Develop/data/bar_data/ES1.csv",
                  header=TRUE, sep=",",
                  drop=FALSE, format="%Y-%m-%d %H:%M",
                  FUN=as.POSIXct, tz="America/New_York")
# Coerce to xts series
ohlc <- as.xts(ohlc)
# Subset to trading hours
ohlc <- ohlc["T09:00:00/T16:30:00"]
# Save the bar data to binary file
save(ohlc, file="C:/Develop/data/ES1.RData")



## Read IB futures data from CSV files

# Read file names
file_names <- scan(file="C:/Develop/data/bar_data/etf_file_names.txt", what=character(), sep=",")

# Remember the cwd
c_wd <- getwd()
# Set the cwd to the file directory
file_dir <- strsplit(file_names[1], split="/")[[1]]
file_dir <- file_dir[-NROW(file_dir)]
file_dir <- paste(file_dir, collapse="/")
# or
# file_dir <- do.call(file.path, as.list(file_dir))
setwd(dir=file_dir)

# Loop over the file_names, load data from CSV files,
# and save the bar data to binary files
for (file_name in file_names) {
  file_name <- strsplit(file_name, split="/")[[1]]
  file_name <- file_name[NROW(file_name)]
  # Load time series data from CSV file
  ohlc <- read.zoo(file=file_name,
                    header=TRUE, sep=",",
                    drop=FALSE, format="%Y-%m-%d %H:%M",
                    FUN=as.POSIXct, tz="America/New_York")
  # Coerce to xts series
  ohlc <- as.xts(ohlc)
  symbol <- strsplit(file_name, split="[.]")[[1]][1]
  # Rename column names
  colnames(ohlc) <- paste(symbol, colnames(ohlc), sep=".")
  # Subset to trading hours
  # ohlc <- ohlc["T09:00:00/T16:30:00"]
  # Save the bar data to binary file
  save(ohlc, file=paste0(symbol, ".RData"))
}  # end for

# Restore the cwd
setwd(dir=c_wd)



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
dates <- read.csv(file="C:/Develop/R/data/futures_expiration_dates_codes.csv",
                   stringsAsFactors=FALSE, row.names=1)
data_dir <- "C:/Users/Jerzy/Downloads/vix_data"
# dir.create(data_dir)
symbolv <- rownames(dates)
# Select only some symbolv
se_lect <- 19:NROW(symbolv)
symbolv <- symbolv[se_lect]
file_names <- file.path(data_dir, paste0(symbolv, ".csv"))
log_file <- file.path(data_dir, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
urls <- paste0(cboe_url, dates[se_lect, 1])
# Download files in a loop
for (it in seq_along(urls)) {
  tryCatch(  # Warning and error handler
    download.file(urls[it],
                  destfile=file_names[it], quiet=TRUE),
    # Warning handler captures warning condition
    warning=function(warning_cond) {
      cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
    },  # end warning handler
    # Error handler captures error condition
    error=function(error_cond) {
      cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
    },  # end error handler
    finally=cat(paste("Processing file name =", file_names[it], "\n"), append=TRUE)
  )  # end tryCatch
}  # end for



## Read CBOE futures data from CSV files

# Define utility function
# last <- function(x) x[NROW(x)]

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
colnamev <- colnames(vix_env$VXF8)

# Get all CSV file names in the data_dir directory
# data_dir <- "C:/Users/Jerzy/Downloads/vix_data"
data_dir <- "C:/Develop/data/vix_data/"
file_names <- Sys.glob(paste(data_dir, "VX*.csv", sep="/"))

vix_env_new <- new.env()

# Loop over the file_names, load data from CSV files,
# and copy the bar data to vix_env_new
for (file_name in file_names) {
  # Load time series data from CSV file
  ohlc <- read.zoo(file=file_name,
                    header=TRUE, sep=",")
  colnames(ohlc)[8] <- colnamev[5]
  ohlc <- ohlc[, colnamev]
  # Coerce data to numeric and make xts
  ohlc <- xts(apply(ohlc, 2, as.numeric), index(ohlc))
  # Create sym bol from file name
  symbol <- strsplit(file_name, split="/")[[1]]
  symbol <- strsplit(last(symbol), split="[.]")[[1]][1]
  assign(x=symbol, value=ohlc, envir=vix_env_new)
}  # end for

# Copy from vix_env_new to vix_env
rm(VXN8, envir=vix_env_new)
symbolv <- ls(vix_env_new)
for (symbol in symbolv) {
  rm(symbol, envir=vix_env)
  assign(x=symbol, value=get(symbol, envir=vix_env_new), envir=vix_env)
}  # end for

save(vix_env, file="C:/Develop/data/vix_data/vix_cboe.RData")



## Load futures data from RData files

# Read the symbolv
symbolv <- scan(file="C:/Develop/data/bar_data/etf_symbolv.txt", what=character(), sep=",")
# Specify the file directory
file_dir <- "C:/Develop/data/bar_data/"
# Specify new environment for data
etfenv <- new.env()
# Specify the file names
# file_names <- paste0(file_dir, symbolv, ".RData")

# Load data in a loop and copy into etfenv
for (symbol in symbolv) {
  # Specify the file name
  file_name <- paste0(file_dir, symbol, ".RData")
  load_ed <- load(file=file_name)
  assign(x=symbol, value=get(load_ed), envir=etfenv)
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
# Save the bar data to binary file
save(etfenv, file=paste0(file_dir, "etf_series.RData"))



## Load futures data from binary files and combine into a single xts series
# First load ES1 data and extract only first 4 OHLC price columns
load(file="C:/Develop/data/ES1.RData")
com_bo <- ohlc[, 1:4]
colnames(com_bo) <- paste0("ES1.", colnames(com_bo))
# Next load TU1 data and cbind it to ES1 data
load(file="C:/Develop/data/TU1UST2yr.RData")
ohlc <- ohlc[, 1:4]
colnames(ohlc) <- paste0("TU1.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)
# Next load TY1 data and cbind it to ES1 data
load(file="C:/Develop/data/TY1UST10yr.RData")
ohlc <- ohlc[, 1:4]
colnames(ohlc) <- paste0("TY1.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)
# Next load UX1 data and cbind it to ES1 data
load(file="C:/Develop/data/UX1_VIX.RData")
ohlc <- ohlc[, 1:4]
colnames(ohlc) <- paste0("UX1.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)
load(file="C:/Develop/data/UX2_VIX.RData")
ohlc <- ohlc[, 1:4]
# Next load UX1 data and cbind it to ES1 data
colnames(ohlc) <- paste0("UX2.", colnames(ohlc))
com_bo <- cbind(com_bo, ohlc)

# Combine into a single xts series
ohlc <- na.omit(com_bo)
# Save the bar data to binary file
save(com_bo, file="C:/Develop/data/combined.RData")
# load(file="C:/Develop/data/combined.RData")

# plot dygraph
label_s <- c("TY1.Close", "TU1.Close")
# dygraphs::dygraph(cbind(closep, dataa())["2018-02-09"], main="OHLC Technicals Strategy") %>%
dygraphs::dygraph(ohlc[endpoints(ohlc, on="hours"), label_s], main="OHLC Data") %>%
  dyAxis("y", label=label_s[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=label_s[2], independentTicks=TRUE) %>%
  dySeries(label_s[2], axis="y2", col=c("blue", "red"))



####################################
# ignore below


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
zoo::write.zoo(bbg_data, file = file_name, sep=",")


# write bbg_data to CSV files
zoo::write.zoo(bbg_data, file="bbg_data.csv", sep=",")


## Download daily historical close prices and daily volume for SPX Index and XLU ETF



# Bloomberg script for a list of symbolv

