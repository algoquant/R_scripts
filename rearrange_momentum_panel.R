### Rearrange panel data of momentum trend logs into wide data frames
### Produces three data frames: EMA prices, volatilities, and z-scores
### Columns: timestamp followed by the superset of symbols in alphabetical order

library(data.table)

dir_name <- "/Users/jerzy/Develop/trading/logs/"
file_name <- paste0(dir_name, "Momentum_Trend_zscore_20260904.csv")

## Load the panel data (long format)
panel_data <- data.table::fread(file_name, colClasses=list(character="date_time"))
data.table::setnames(panel_data,
  old=c("date_time", "symbol", "price", "EMA price", "volatility", "z-score"),
  new=c("timestamp", "symbol", "price", "ema", "volatility", "zscore"))

## Drop the interleaved summary rows which have numbers instead of a symbol
panel_data <- panel_data[grepl("^[A-Za-z][A-Za-z.]*$", symbol)]

panel_data[, timestamp := as.POSIXct(timestamp, tz="America/New_York")]
panel_data[, c("price", "ema", "volatility", "zscore") :=
  lapply(.SD, as.numeric), .SDcols=c("price", "ema", "volatility", "zscore")]

## Superset of the symbols in alphabetical order
symbolv <- sort(unique(panel_data$symbol))

## Cast a single value column from long to wide format
cast_wide <- function(value_col) {
  wide_data <- data.table::dcast(panel_data, timestamp ~ symbol,
                                 value.var=value_col,
                                 fun.aggregate=function(x) x[length(x)],
                                 fill=NA)
  ## Add any symbols missing from this value column
  miss_ing <- setdiff(symbolv, colnames(wide_data))
  if (length(miss_ing) > 0)
    wide_data[, (miss_ing) := NA_real_]
  data.table::setcolorder(wide_data, c("timestamp", symbolv))
  data.table::setorder(wide_data, timestamp)
  as.data.frame(wide_data)
} # end cast_wide

ema_prices <- cast_wide("ema")
volatilities <- cast_wide("volatility")
zscores <- cast_wide("zscore")

# Set first row to zero for all symbols
ema_prices[1, 2:ncol(ema_prices)] <- 0
volatilities[1, 2:ncol(volatilities)] <- 0
zscores[1, 2:ncol(zscores)] <- 0
# Carry forward the last non-NA value for each symbol
ema_prices[, 2:ncol(ema_prices)] <- zoo::na.locf(ema_prices[, 2:ncol(ema_prices)], na.rm=FALSE)
volatilities[, 2:ncol(volatilities)] <- zoo::na.locf(volatilities[, 2:ncol(volatilities)], na.rm=FALSE)
zscores[, 2:ncol(zscores)] <- zoo::na.locf(zscores[, 2:ncol(zscores)], na.rm=FALSE)

cat("Rows:", NROW(ema_prices), " Symbols:", length(symbolv), "\n")
print(head(ema_prices[, 1:min(6, NCOL(ema_prices))]))
print(head(volatilities[, 1:min(6, NCOL(volatilities))]))
print(head(zscores[, 1:min(6, NCOL(zscores))]))

# Save to CSV files
data.table::fwrite(ema_prices, "/Users/jerzy/Develop/trading/logs/Momentum_Trend_ema_20260904.csv", row.names=FALSE)
data.table::fwrite(volatilities, "/Users/jerzy/Develop/trading/logs/Momentum_Trend_vol_20260904.csv", row.names=FALSE)
data.table::fwrite(zscores, "/Users/jerzy/Develop/trading/logs/Momentum_Trend_zscores_20260904.csv", row.names=FALSE)


### Plotting the data using dygraphs
# Plot the z-scores for a specific symbol using dygraphs
zscores <- xts::xts(zscores[, -1], order.by=zscores$timestamp)
symboln <- "MSFT"
dygraph(zscores[, symboln], main=paste(symboln, "Z-Scores")) %>%
  dyOptions(colors=c("red"), strokeWidth=1)

# Plot the ema_prices for a specific symbol using dygraphs
ema_prices <- xts::xts(ema_prices[, -1], order.by=ema_prices$timestamp)
dygraph(ema_prices[, symboln], main=paste(symboln, "EMA")) %>%
  dyOptions(colors=c("red"), strokeWidth=1)

# Load the minute prices for the same symbol
dir_name <- "/Users/jerzy/Develop/data/minutes/live/"
file_name <- paste0(dir_name, symboln, "_20260904.csv")
ohlc <- data.table::fread(file_name, tz="")
datev <- as.POSIXct(ohlc$timestamp, FUN = as.POSIXct, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
pricev <- xts::xts(ohlc$close, order.by=datev)

# Plot the minute prices with the z-scores
colnames(pricev) <- symboln
# pricev <- pricev[index(zscores)]
# zscores <- zscores[index(pricev)]
datav <- na.omit(cbind(pricev, zscores[, symboln]))
colnames(datav)[2] <- "zscores"

# Plot the minute prices with the EMA
datav <- na.omit(cbind(pricev, ema_prices[, symboln]))
colnames(datav)[2] <- "EMA"

## Load the PnL data
dir_name <- "/Users/jerzy/Develop/trading/logs/"
file_name <- paste0(dir_name, "Momentum_Trend_total_pnl_20260904.csv")
# Read the PnL data with timestamp as POSIXct and NY timezone
pnls <- data.table::fread(file_name)
pnls$timestamp <- lubridate::force_tz(pnls$timestamp, "America/New_York")
pnls <- as.xts(pnls)
# Plot the PnLs for the symbol
dygraph(pnls[, symboln], main=paste(symboln, "PnL"))
# Plot the combined PnL for all symbols
pnlc <- rowSums(pnls)
pnlc <- as.xts(pnlc, order.by=index(pnls))
dygraph(pnlc)
# Find the symbol with the highest PnL at some point in time
symboln <- names(which.max(sapply(pnls, max)))
dygraph(pnls[, symboln], main=paste(symboln, "PnL"))


# dygraph with two y-axes
colv <- colnames(datav)
dygraphs::dygraph(datav[-(1:33)], main=paste(symboln, "Minute Prices")) %>%
  dyAxis("y", label=symboln, independentTicks=TRUE) %>%
  dyAxis("y2", label="Z-Score", independentTicks=TRUE) %>%
  dySeries(name=colv[1], axis="y", strokeWidth=1, col="blue") %>%
  dySeries(name=colv[2], axis="y2", strokeWidth=1, col="red") %>%
  dyLegend(show="always", width=300)

