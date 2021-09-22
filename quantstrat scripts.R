# store trade in blotter
addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
} # end if
# Calculate P&L and resulting equity (blotter)
updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
for (date.bar in ma.window:nrow(GSPC)) {
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
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=time(GSPC)[ma.window], TxnPrice=1, TxnQty=1 , TxnFees=0, verbose=TRUE)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
for (date.bar in ma.window:nrow(GSPC)) {
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
posn.indic[11]*UnitSize
posn.indic[11]
unclass(posn.indic[11])
as.numeric(posn.indic[11])
as.vector(posn.indic[11])
posn.indic <- sign(as.numeric(Ad(GSPC) - GSPC[, 'indicator']))
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
for (date.bar in ma.window:nrow(GSPC)) {
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
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
tail(posn.indic)
posn.indic[2]
head(posn.indic)
tail(flip.indic)
head(flip.indic)
?lag
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
for (date.bar in ma.window:nrow(GSPC)) {
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
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=time(GSPC)[ma.window], TxnPrice=1, TxnQty=UnitSize, TxnFees=0, verbose=TRUE)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=time(GSPC)[ma.window], TxnPrice=1, TxnQty=posn.indic[1]*UnitSize, TxnFees=0, verbose=TRUE)
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=index.GSPC[ma.window], TxnPrice=1, TxnQty=posn.indic[ma.window]*UnitSize, TxnFees=0, verbose=TRUE)
for (date.bar in ma.window:nrow(GSPC)) {
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
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
head(posn.indic)
tail(posn.indic)
posn.indic[ma.window]
posn.indic[ma.window]*UnitSize
head(cbind(posn.indic, flip.indic), 11)
cbind(posn.indic, flip.indic)[1:22, ]
cbind(posn.indic, flip.indic)[ma.window]
posn.indic[ma.window]*UnitSize
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=index.GSPC[ma.window], TxnPrice=as.numeric(Ad(GSPC[ma.window, ])), TxnQty=posn.indic[ma.window]*UnitSize, TxnFees=0, verbose=TRUE)
for (date.bar in ma.window:nrow(GSPC)) {
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
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=index.GSPC[1], TxnPrice=as.numeric(Ad(GSPC[1, ])), TxnQty=posn.indic[1]*UnitSize, TxnFees=0, verbose=TRUE)
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
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
tradeStats(Portfolio=ltportfolio)
trade.stats <- tradeStats(Portfolio=ltportfolio)
tab.trades <- cbind(
c("Trades","Win Percent","Loss Percent","W/L Ratio"),
c(trade.stats[,"Num.Trades"],trade.stats[,c("Percent.Positive","Percent.Negative")],
trade.stats[,"Percent.Positive"]/trade.stats[,"Percent.Negative"])
)
tab.trades
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
# combine
trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)
trade.stats.tab
objects()
search()
ts.rets <- PortfReturns(Account=ltportfolio)
rownames(ts.rets)
head(ts.rets)
class(ts.rets)
colnames(ts.rets)
rownames(ts.rets) <- NULL
rownames(ts.rets)
tail(ts.rets)
head(ts.rets)
charts.PerformanceSummary(ts.rets, colorset=bluefocus)
table.Arbitrary(ts.rets)
?table.Arbitrary
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
"Calmar Ratio")
)
tab.perf
tab.risk <- table.Arbitrary(rets,
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
R.Version()
################################################
read.clip <- function(header=TRUE,...) {
read.table("clipboard",sep="\t",header=header,...)
}  # end read.clip
data.xls <- read.clip()
data.xls
data.xls <- read.clip()
data.xls
?read.table
read.clip <- function(file="clipboard", sep="\t", header=TRUE, ...) {
read.table(file=file, sep=sep, header=header, ...)
}  # end read.clip
data.xls <- read.clip()
data.xls <- read.clip()
data.xls
?write.table
write.clip <- function(data, row.names=FALSE, col.names=TRUE, ...) {
write.table(x=data, file="clipboard", sep="\t", row.names=row.names, col.names=col.names, ...)
}  # end write.clip
write.clip(data=data.xls)
edit(data.xls)
data.xls
edit(data.xls)
data.xls
rm(data.xls)
data.xls <- read.clip()
data.xls
data.xls[,1]
class(data.xls[,1])
edit(data.xls)
data.xls
blah <- edit(data.xls)
blah
library(ggplot2)
.libPaths()
UBS_Op18Nov <-(data.frame(read.table("C:/Develop/R/FRE6811/option_data.csv",header=TRUE, sep=",")))
summary(UBS_Op18Nov)
UBS_Op18Nov
rm(list=ls())  # remove all
options(digits.secs=6)
options(stringsAsFactors=FALSE)
require(quantstrat)
require(utils)
loadInstruments(file_name='E:/mktdata/instruments.rda')
ls()
search()
ls(FinancialInstrument)
ls(package:FinancialInstrument)
ls_instruments()
tail(ls(.instrument))
search()
tail(ls(FinancialInstrument:::.instrument))
getInstrument(ZW_Z9)
getInstrument(ZW)
getInstrument("IBM")
getInstrument("TSLA")
getInstrument("FSLR")
find.instrument("bond")
find.instrument("computer)
""
"
find.instrument("computer")
?setDefaults
?getSymbols
data_source <- "E:/mktdata/"
setDefaults(getSymbols.FI,
extension="RData",
dir=data_source,
days_to_omit="Saturday",
use_identifier="X.RIC")
setDefaults(getSymbols, verbose=FALSE, dir=data_source, src="rda")
getSymbols.rda
?getSymbolLookup
getSymbols("MSFT", verbose=FALSE, dir=data_source, src="rda")
getSymbols("ESH0", verbose=FALSE, dir=data_source, src="rda")
getInstrument("MSFT")
sink("C:/Develop/R/research/instruments.csv")
ls_instruments()  # vary large list!!!
sink()
?sink
ls()
load("E:/mktdata/ZQF3.rda")
load("E:/mktdata/ZQF3/2013.01.31.ZQF3.rda")
load("E:/mktdata/ZQF3/2013.01.31.ZQF3.rdata")
ls()
class(ZQF3)
tail(ZQF3)
head(ZQF3)
dim(ZQF3)
blah <- ZQF3[, 4:6]
dim(blah)
blah <- ZQF3[, 3:6]
dim(blah)
head(blah)
sum(is.na((blah)))
blah <- cbind((blah[, 1] + blah[, 3])/2, blah)
dim(blah)
head(blah)
colnames(blah) <- c("Bid.Price", "Bid.Price.1", "Bid.Size", "Ask.Price", "Ask.Size")
head(blah)
colnames(blah) <- c("Mid.Price", "Bid.Price", "Bid.Size", "Ask.Price", "Ask.Size")
head(blah)
plot(blah[, 1])
tail(blah)
setDefaults(getSymbols, verbose=FALSE, src="FI")
getSymbols("ESH3")
getSymbols.FI
getSymbols("ZQZ9")
loadInstruments(file_name='E:/mktdata/instruments.rda')
getSymbols("ZQZ9")
getSymbols("QZ9")
getSymbols("ESH3")
getSymbols("ZSK7")
getSymbols("ZSK5")
ls()
dim(ZSK5)
tail(ZSK5)
plot(ZSK5[, "Bid.Price"])
plot(tail(ZSK5[, "Bid.Price"], 1000))
plot(tail(ZSK5[, "Bid.Price"], 10000))
dim(ZSK5)[1]
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-100)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-1000):(dim(ZSK5)[1]-100)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-5000):(dim(ZSK5)[1]-100)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-1000)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-10000)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-5000)), "Bid.Price"])
?setDefaults
getDefaults(getSymbols)
getDefaults(getSymbols.FI)
getSymbols("MSFT", verbose=FALSE, dir=data_source, src="rda")
blah <- ls_instruments()
tail(blah)
length(blah)
write.csv(blah, file="C:/Develop/R/research/instruments.csv")
getSymbols("ZSK6")
getSymbols("ZSK5")
getSymbols("ZSK5.N5")
dim(ZSK5)
dim(ZSK5.N5)
tail(ZSK5.N5, 22)
dim(ZSK5)
tail(ZSK5.N5)
plot(ZSK5.N5[, "Bid.Price"])
plot(ZSK5.N5[(100:(dim(ZSK5.N5)[1]-100)), "Bid.Price"])
plot(ZSK5.N5[(100:(dim(ZSK5.N5)[1]-500)), "Bid.Price"])
plot(ZSK5.N5[(500:(dim(ZSK5.N5)[1]-500)), "Bid.Price"])
rm(list=ls())  # remove all
data_source <- "E:/mktdata/sec/"
setDefaults(getSymbols.FI,
extension="RData",
dir=data_source,
days_to_omit="Saturday",
use_identifier="X.RIC")
getSymbols("ES")
getSymbols("ESM9")
getDefaults(getSymbols.FI)
data_source
getSymbols("ESM5")
getSymbols("ESU9")
ls()
loadInstruments(file_name='E:/mktdata/instruments.rda')
getSymbols("ESU9")
getSymbols("ESM5")
ls()
load("E:/mktdata/sec/ESM9/2009.04.02.ESM9.rdata")
ls()
dim(ESM9)
tail(ESM9)
head(ESM9)
plot(ESM9[(1000:(dim(ESM9)[1]-1000)), "Bid.Price"])
plot(ESM9[(10000:(dim(ESM9)[1]-10000)), "Bid.Price"])
plot(ESM9[((dim(ESM9)[1]-20000):(dim(ESM9)[1]-10000)), "Bid.Price"])
plot(ESM9[((dim(ESM9)[1]-11000):(dim(ESM9)[1]-10000)), "Bid.Price"])
dim(ESM9)[1]
len_es <- dim(ESM9)[1]
ESM9[(len_es-1010):(len_es-10000), ]
ESM9[(len_es-10010):(len_es-10000), ]
plot(ESM9[((len_es-20000):(len_es-10000)), "Bid.Price"])
sink("C:/Develop/R/research/temp.txt")
getSymbols.FI
sink()
?get
savehistory("C:/Develop/R/scripts/temp.txt")
