####################################
### Scripts for PAPER trading through Interactive Brokers
### using package IBrokers.
#
### THESE SCRIPTS ARE for PAPER TRADING ONLY.
### DO NOT USE THEM FOR LIVE TRADING WITH REAL CAPITAL!


####################################
### Script for trading a simple market-making strategy in a callback loop.

library(HighFreq)
library(IBrokers)

# Define the contract for trading
con_tract <- IBrokers::twsFuture(symbol="ES", exch="GLOBEX", expiry="201812")

# Load the trading function written as an eWrapper:
source("C:/Develop/R/ibrokers/ibrokers_eWrapper.R")

# The simple market-making strategy is defined as follows:
#  Place limit buy order at previous bar Low price minus buy_spread,
#  Place limit sell order at previous bar High price plus sell_spread.
#
# The trading strategy functions are defined inside the function
# realtimeBars(), which is part of the trade wrapper.
# The user can customize the strategy or create a new strategy by
# modifying the trading function code in the function realtimeBars()
# in the file trade_wrapper.R

# Open the file for storing the bar data
data_dir <- "C:/Develop/data/ib_data"
file_name <- file.path(data_dir, paste0("ESohlc_live_", format(Sys.time(), format="%m_%d_%Y_%H_%M"), ".csv"))
file_connect <- file(file_name, open="w")

# Open the IB connection
ib_connect <- IBrokers::twsConnect(port=7497)

# Run the strategy:
IBrokers::reqRealTimeBars(conn=ib_connect, useRTH=FALSE,
                          Contract=con_tract, barSize="10",
                          eventWrapper=trade_wrapper(1),
                          CALLBACK=twsCALLBACK,
                          file=file_connect,
                          buy_spread=0.75, sell_spread=0.75)


# Close IB connection
IBrokers::twsDisconnect(ib_connect)
# Close file
close(file_connect)


