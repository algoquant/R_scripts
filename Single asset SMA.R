###############################
### blotter package example ###
###############################
# Jeremy Siegel and Mebane Faber trend following strategy using a 200-day simple moving average
# from "A Quantitative Approach to Tactical Asset Allocation." 
# adapted from script longtrend.R in package blotter
# this demo uses monthly SP500 data from Yahoo Finance, instead of total return data.

# Load required libraries
require(quantmod)
require(TTR)
require(blotter)
# set TZ
Sys.setenv(TZ="UTC")

# load data (quantmod)
getSymbols('^GSPC', src='yahoo', index.class=c("POSIXt", "POSIXct"), from='1998-01-01')
GSPC.daily <- GSPC


##############################################
### version slightly modified longtrend.R  ###
##############################################

# this version is path-dependent because number of shares traded 
# is scaled to the time-dependent equity

# xts objects are stored in the global environment
# portfolio and account objects are stored in .blotter environment
# currency and trading instrument objects are stored in the .instrument environment

# remove objects in case demo was run previously
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "equity", "date.bar", "initDate", "initEq", "Posn", "UnitSize"), silent=TRUE)

# Set initial values
initDate <- '1997-12-31'
initEq <- 10000

# define instruments (FinancialInstrument)
print("Loading data")
currency("USD")
stock("GSPC", currency="USD", multiplier=1)

# create monthly data
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
# add SMA indicator column (TTR)
print("Setting up indicators")
ma.window <- 8
GSPC$indicator <- SMA(GSPC[, grep('Adjusted', colnames(GSPC))], ma.window)

print("Initializing portfolio and account structure")

# assign portfolio and account names
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
# create portfolio and account objects (blotter)
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)

# initialize position to some small size (blotter)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=time(GSPC)[ma.window], TxnPrice=1, TxnQty=1, TxnFees=0, verbose=TRUE)

# main loop to create trades
for (date.bar in ma.window:nrow(GSPC)) {
# browser()
  CurrentDate <- time(GSPC)[date.bar]
# get last equity (blotter)
  equity <- getEndEq(ltaccount, CurrentDate)
# get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
# calc number of shares
  UnitSize <- as.numeric(trunc(equity/ClosePrice))
# get last position (blotter)
  Posn <- getPosQty(ltportfolio, Symbol='GSPC', Date=CurrentDate)

# perform trade if current position is in opposite direction to indicator
  UnitSize <- -sign(Posn)*UnitSize
  flip.indic <- (Posn*(as.numeric(Ad(GSPC[date.bar, ])) - as.numeric(GSPC[date.bar, 'indicator']))) < 0
  cat('.')
#  cat(paste('.', Posn, UnitSize, '\n'))
# Flip position if indicator is opposite to current position
  if (flip.indic) {
    cat('\n')
# store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=UnitSize-Posn, TxnFees=0, verbose=TRUE)
  } # end if

# Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
  updateAcct(ltaccount, Dates=CurrentDate)
  updateEndEq(ltaccount, Dates=CurrentDate)
} # End dates loop

cat('\n')

##############################################
### end slightly modified version ###
##############################################


##############################################
### longtrend.R faster simplified version  ###
##############################################

# this version trades a fixed number of shares, so it's path-independent and faster
# it doesn't Calculate equity amount in account object

# xts objects are stored in the global environment
# portfolio and account objects are stored in .blotter environment
# currency and trading instrument objects are stored in the .instrument environment

# remove objects in case demo was run previously
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)


# Set initial values
initDate <- '1997-12-31'
initEq <- 10000

# define instruments (FinancialInstrument)
print("Loading data")
currency("USD")
stock("GSPC", currency="USD", multiplier=1)

# create monthly data
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
# add SMA indicator column (TTR)
print("Setting up indicators")
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']


print("Initializing portfolio and account structure")

### assign portfolio and account names
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
# create portfolio and account objects (blotter)
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)

### initialize all the variables
# trade one unit of stock
UnitSize <- 1
# get time index
index.GSPC <- index(GSPC)
# initialize posn.indic and flip.indic
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
# initialize position to UnitSize (blotter)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=index.GSPC[1], TxnPrice=as.numeric(Ad(GSPC[1, ])), TxnQty=posn.indic[1]*UnitSize, TxnFees=0, verbose=TRUE)

# main loop to create trades
for (date.bar in 2:nrow(GSPC)) {
# browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
# get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
# Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
# store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if

# Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop

cat('\n')

##############################################
### end simplified version ###
##############################################


##############################################
### performance charts and statistics ###
##############################################

# chart strategy (blotter)
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
# Add MA (quantmod)
plot(add_SMA(n=ma.window, col='darkgreen', on=1))

# optional copy the results into the local environment (blotter)
print("Retrieving resulting portfolio and account")
ltportfolio <- getPortfolio("longtrend")
ltaccount <- getAccount("longtrend")

# get transaction summary (blotter)
getTxns(Portfolio="longtrend", Symbol="GSPC")

# get strategy statistics
tradeStats(Portfolio=ltportfolio)
trade.stats <- tradeStats(Portfolio=ltportfolio)

# arrange strategy statistics
# trade statistics
tab.trades <- cbind(
  c("Trades","Win Percent","Loss Percent","W/L Ratio"),
  c(trade.stats[,"Num.Trades"],trade.stats[,c("Percent.Positive","Percent.Negative")],
    trade.stats[,"Percent.Positive"]/trade.stats[,"Percent.Negative"])
  )
# profit statistics
tab.profit <- cbind(
  c("Net Profit","Gross Profits","Gross Losses","Profit Factor"),
  c(trade.stats[,c("Net.Trading.PL","Gross.Profits","Gross.Losses",
              "Profit.Factor")])
  )
# averages
tab.wins <- cbind(
  c("Avg Trade","Avg Win","Avg Loss","Avg W/L Ratio"),
  c(trade.stats[,c("Avg.Trade.PL","Avg.Win.Trade","Avg.Losing.Trade",
              "Avg.WinLoss.Ratio")])
  )
# combine statistics (needs work)
trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)


# PerformanceAnalytics statistics
ts.rets <- PortfReturns(Account=ltportfolio)
rownames(ts.rets) <- NULL
tail(ts.rets)
charts.PerformanceSummary(ts.rets, colorset=bluefocus)
table.Arbitrary(ts.rets)

tab.perf <- table.Arbitrary(ts.rets,
                            metrics=c(
                              "Return.cumulative",
                              "Return.annualized",
                              "SharpeRatio.annualized",
                              "CalmarRatio"),
                            metricsNames=c(
                              "Cumulative Return",
                              "Annualized Return",
                              "Annualized Sharpe Ratio",
                              "Calmar Ratio"))
tab.perf

tab.risk <- table.Arbitrary(ts.rets,
                            metrics=c(
                              "StdDev.annualized",
                              "maxDrawdown",
                              "VaR",
                              "ES"),
                            metricsNames=c(
                              "Annualized StdDev",
                              "Max DrawDown",
                              "Value-at-Risk",
                              "Conditional VaR"))
tab.risk

performance.stats.tab <- data.frame(
  rownames(tab.perf),tab.perf[,1],
  rownames(tab.risk),tab.risk[,1])
performance.stats.tab

