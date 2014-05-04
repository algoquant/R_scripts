
# http://www.programmingr.com/content/installing-quantstrat-r-forge-and-source/
# 
install.packages("blotter", repos="http://R-Forge.R-project.org")
install.packages("quantstrat", repos="http://R-Forge.R-project.org")

library(quantstrat)

###############################
### blotter package example ###
###############################
# Mebane Faber trend following strategy using a 200-day simple moving average
# from "A Quantitative Approach to Tactical Asset Allocation." 
# this demo uses monthly SP500 data from Yahoo Finance, instead of total return data.

# Load required libraries
require(quantmod)
require(TTR)
require(blotter)

Sys.setenv(TZ="UTC")

# Try to clean up in case the demo was run previously
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "equity", "GSPC", "i", "initDate", "initEq", "Posn", "UnitSize", "verbose"), silent=TRUE)


# Set initial values
initDate <- '1997-12-31'
initEq <- 100000

# Load data with quantmod
print("Loading data")
# define instruments
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
# load data
getSymbols('^GSPC', src='yahoo', index.class=c("POSIXt", "POSIXct"), from='1998-01-01')
# create monthly data
GSPC <- to.monthly(GSPC, indexAt='endof', drop.time=FALSE)

# add indicator column using TTR function
print("Setting up indicators")
GSPC$SMA10m <- SMA(GSPC[, grep('Adj', colnames(GSPC))], 10)

print("Initializing portfolio and account structure")

# assign portfolio and account names
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
# create portfolio and account objects in blotter
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
# verbose=TRUE


# main loop to create trades
for (date.bar in 10:NROW(GSPC)) {
# browser()
  CurrentDate <- time(GSPC)[date.bar]
  cat(".")
# get last equity
  equity <- getEndEq(ltaccount, CurrentDate)
# get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
# calc number of shares
  UnitSize <- as.numeric(trunc(equity/ClosePrice))
# get last position
  Posn <- getPosQty(ltportfolio, Symbol='GSPC', Date=CurrentDate)

# Position Entry (assume fill at close)
  if (Posn == 0 ) {
# No position, so test to initiate Long position
    if (as.numeric(Ad(GSPC[date.bar, ])) > as.numeric(GSPC[date.bar, 'SMA10m'])) { 
      cat('\n')
# Store trade with blotter
      addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=UnitSize , TxnFees=0, verbose=TRUE)
    }
  } else {
# Have a position, so check exit
    if (as.numeric(Ad(GSPC[date.bar, ])) < as.numeric(GSPC[date.bar, 'SMA10m'])) { 
      cat('\n')
# Store trade with blotter
      addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=-Posn , TxnFees=0, verbose=TRUE)
    }
  } # end if Posn

# Calculate P&L and resulting equity with blotter
  updatePortf(ltportfolio, Dates=CurrentDate)
  updateAcct(ltaccount, Dates=CurrentDate)
  updateEndEq(ltaccount, Dates=CurrentDate)
} # End dates loop

cat('\n')

# Chart results with quantmod
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=10, col='darkgreen', on=1))

#look at a transaction summary
getTxns(Portfolio="longtrend", Symbol="GSPC")

# Copy the results into the local environment
print("Retrieving resulting portfolio and account")
ltportfolio <- getPortfolio("longtrend")
ltaccount <- getAccount("longtrend")

