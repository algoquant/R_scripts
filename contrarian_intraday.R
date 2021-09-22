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

sym_bol <- "ES"  # S&P500 Emini futures
# sym_bol <- "QM"  # oil
data_dir <- "C:/Develop/data/ib_data/"
# Load the 5-second futures bar data collected from IB
# load(paste0(data_dir, sym_bol, "_ohlc.RData"))


# Write zoo series to CSV file, and then read it back
file_name <- paste0(data_dir, sym_bol, "_ohlc.csv")
# write.zoo(oh_lc, file=file_name, sep=",", col.names=TRUE)
# Read from CSV file using read.csv.zoo()
oh_lc <- read.csv.zoo(file=file_name)
# Coerce data frame into xts series
oh_lc <- xts::xts(coredata(oh_lc), order.by=index(oh_lc), tz="America/New_York")

# Or:
# Load 1-minute SPY bars
# sym_bol <- "SPY"
# oh_lc <- HighFreq::SPY

in_dex <- index(oh_lc)
n_rows <- NROW(oh_lc)
end_points <- xts::endpoints(oh_lc, on="hours")
clos_e <- Cl(oh_lc)[end_points]
# re_turns <- HighFreq::diff_vec(log(drop(coredata(Cl(oh_lc)))))
# re_turns <- c(0, re_turns)
re_turns <- log(drop(coredata(Cl(oh_lc))))
re_turns <- (re_turns - c(re_turns[1], re_turns[-NROW(re_turns)]))

## Scale the returns using the price range
rang_e <- log(drop(coredata(Hi(oh_lc)/Lo(oh_lc))))
# Average with the price range from previous bar
rang_e <- (rang_e + c(0, rang_e[-NROW(rang_e)]))/2
returns_scaled <- ifelse(rang_e>0, re_turns/rang_e, 0)

## Scale the returns using the volume
# vol_ume <- drop(coredata(Vo(oh_lc)))
# Average with the price range from previous bar
# vol_ume <- (vol_ume + c(0, vol_ume[-NROW(vol_ume)]))/2
# returns_scaled <- ifelse(vol_ume>0, re_turns/sqrt(vol_ume), 0)
# returns_scaled <- returns_scaled/sd(returns_scaled)

lagg <- 4
thresh_old <- 0.8
bid_offer <- 0.000001


# Rerun the model
## Backtest strategy for flipping if two consecutive positive and negative returns
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
# Flip position if the scaled returns exceed thresh_old 
position_s[returns_scaled > thresh_old] <- (-1)
position_s[returns_scaled < (-thresh_old)] <- 1
# LOCF
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- rutils::lag_it(position_s, lagg=lagg)
# Calculate position turnover
turn_over <- abs(rutils::diff_it(position_s)) / 2
# Calculate number of trades
# sum(turn_over)/NROW(position_s)
# Calculate strategy pnl_s
pnl_s <- (position_s*re_turns)
# Calculate transaction costs
cost_s <- bid_offer*turn_over
pnl_s <- (pnl_s - cost_s)
pnl_s <- cumsum(pnl_s)

## Coerce pnl_s to xts
# pnl_s <- xts(pnl_s, in_dex)
pnl_s <- cbind(pnl_s[end_points], clos_e)
col_names <- c("Strategy", sym_bol)
colnames(pnl_s) <- col_names

# Return to the output argument a dygraph plot with two y-axes
col_names <- colnames(pnl_s)
cap_tion <- paste("Contrarian Strategy for", sym_bol, "Using the Returns Scaled by the Price Range")

dygraphs::dygraph(pnl_s, main=cap_tion) %>%
  dyAxis("y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", label=col_names[1], strokeWidth=1, col="red") %>%
  dySeries(name=col_names[2], axis="y2", label=col_names[2], strokeWidth=1, col="blue")


