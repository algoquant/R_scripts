#####################################################
# A quantstrat script for a simple moving average strategy
# Adapted from "maCross.R" example 
# R (http://r-project.org/) Quantitative Strategy Model Framework
#####################################################

library(TTR)
library(PerformanceAnalytics)
library(RTisean)
require(quantstrat)
library(ROneTick)
oneTickLib()

# Remove some objects from specified environments
suppressWarnings(rm("order_book.macross", pos=.strategy))
suppressWarnings(rm("account.macross", "portfolio.macross", pos=.blotter))
suppressWarnings(rm("account.st", "portfolio.st", "symbol.cds", "stratMACROSS", "initDate", "initEq", 'start_t', 'end_t'))

# Construct ts instruments
symbol.cds <- 'LEN'
currency('USD')
stock(symbol.cds, currency='USD', multiplier=1)
initDate <- '1999-12-31'
initEq <- 1000000

# Initialize portfolio object and account object
portfolio.st <- 'macross'
account.st <- 'macross'
initPortf(portfolio.st, symbols=symbol.cds,  initDate=initDate)
initAcct(account.st, portfolios=portfolio.st, initDate=initDate)
initOrders(portfolio=portfolio.st, initDate=initDate)

# Construct strategy object
stratMACROSS <- strategy(portfolio.st)

# Add indicators to the strategy
stratMACROSS <- add.indicator(strategy=stratMACROSS, name="SMA", arguments=list(x=quote(mktdata[,1]), n=50), label= "ma50" )
stratMACROSS <- add.indicator(strategy=stratMACROSS, name="SMA", arguments=list(x=quote(mktdata[,1]), n=200), label= "ma200")

# Add signal to the strategy
stratMACROSS <- add.signal(strategy=stratMACROSS, name="sigCrossover", arguments=list(columns=c("ma50","ma200"), relationship="gte"), label="ma50.gt.ma200")
stratMACROSS <- add.signal(strategy=stratMACROSS, name="sigCrossover", arguments=list(column=c("ma50","ma200"), relationship="lt"), label="ma50.lt.ma200")

# Add rules to the strategy
stratMACROSS <- add.rule(strategy=stratMACROSS, name='ruleSignal', arguments=list(sigcol="ma50.gt.ma200", sigval=TRUE, orderqty=100, ordertype='market', orderside='long'), type='enter')
stratMACROSS <- add.rule(strategy=stratMACROSS, name='ruleSignal', arguments=list(sigcol="ma50.lt.ma200", sigval=TRUE, orderqty=-100, ordertype='market', orderside='long'), type='exit')

# Add two more rules for the short side: to reverse MA cross strategy
stratMACROSS <- add.rule(strategy=stratMACROSS, name='ruleSignal', arguments=list(sigcol="ma50.lt.ma200", sigval=TRUE, orderqty=-100, ordertype='market', orderside='short'), type='enter')
stratMACROSS <- add.rule(strategy=stratMACROSS, name='ruleSignal', arguments=list(sigcol="ma50.gt.ma200", sigval=TRUE, orderqty=100, ordertype='market', orderside='short'), type='exit')


# Get market data
source("c:/Devel/src/sysCredit/R/JP/funcDevelop.R")
LEN <- loadCDS(symbol.cds)
colnames(LEN) <- "Close"
# getSymbols(symbol.cds, from=initDate)
# for(i in symbol.cds)
#   assign(i, adjustOHLC(get(i), use.Adjusted=TRUE))


# Apply the strategy and calculate positions
out <- try(applyStrategy(strategy=stratMACROSS, portfolios=portfolio.st))

# Calculate P&Ls
updatePortf(Portfolio='macross', Dates=paste('::', as.Date(Sys.time()), sep=''))

# Chart
chart.Posn(Portfolio='macross', Symbol=symbol.cds)
add_SMA(n=50 , on=1,col='blue')
add_SMA(n=200, on=1)


