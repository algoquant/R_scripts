##############################
# 
# This is an R script for simulating a contrarian strategy 
# using intraday returns scaled by the price range.
# 
##############################

# Load R packages
library(dygraphs)
library(HighFreq)

## Data setup
# Uncomment the data you want to load

symbol <- "ES"  # S&P500 Emini futures
# symbol <- "QM"  # oil
data_dir <- "C:/Develop/data/ib_data/"
# Load the 5-second futures bar data collected from IB
# load(paste0(data_dir, symbol, "_ohlc.RData"))


# Write zoo series to CSV file, and then read it back
file_name <- paste0(data_dir, symbol, "_ohlc.csv")
# write.zoo(ohlc, file=file_name, sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
ohlc <- read.csv.zoo(file=file_name)
# Coerce data frame into xts series
ohlc <- xts::xts(coredata(ohlc), order.by=index(ohlc), tz="America/New_York")

# Or:
# Load 1-minute SPY bars
# symbol <- "SPY"
# ohlc <- HighFreq::SPY

indeks <- index(ohlc)
nrows <- NROW(ohlc)
endpoints <- xts::endpoints(ohlc, on="hours")
closep <- Cl(ohlc)[endpoints]
# returns <- HighFreq::diff_vec(log(drop(coredata(Cl(ohlc)))))
# returns <- c(0, returns)
returns <- log(drop(coredata(Cl(ohlc))))
returns <- (returns - c(returns[1], returns[-NROW(returns)]))

## Scale the returns using the price range
rangev <- log(drop(coredata(Hi(ohlc)/Lo(ohlc))))
# Average with the price range from previous bar
rangev <- (rangev + c(0, rangev[-NROW(rangev)]))/2
returns_scaled <- ifelse(rangev>0, returns/rangev, 0)

## Scale the returns using the volume
# volumes <- drop(coredata(Vo(ohlc)))
# Average with the price range from previous bar
# volumes <- (volumes + c(0, volumes[-NROW(volumes)]))/2
# returns_scaled <- ifelse(volumes>0, returns/sqrt(volumes), 0)
# returns_scaled <- returns_scaled/sd(returns_scaled)

lagg <- 4
threshold <- 0.8
bid_offer <- 0.000001


# Rerun the model
## Backtest strategy for flipping if two consecutive positive and negative returns
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
# Flip position if the scaled returns exceed threshold 
posit[returns_scaled > threshold] <- (-1)
posit[returns_scaled < (-threshold)] <- 1
# LOCF
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit, lagg=lagg)
# Calculate position turnover
turn_over <- abs(rutils::diffit(posit)) / 2
# Calculate number of trades
# sum(turn_over)/NROW(posit)
# Calculate strategy pnls
pnls <- (posit*returns)
# Calculate transaction costs
costs <- bid_offer*turn_over
pnls <- (pnls - costs)
pnls <- cumsum(pnls)

## Coerce pnls to xts
# pnls <- xts(pnls, indeks)
pnls <- cbind(pnls[endpoints], closep)
colnamev <- c("Strategy", symbol)
colnames(pnls) <- colnamev

# Return to the output argument a dygraph plot with two y-axes
colnamev <- colnames(pnls)
cap_tion <- paste("Contrarian Strategy for", symbol, "Using the Returns Scaled by the Price Range")

dygraphs::dygraph(pnls, main=cap_tion) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")


