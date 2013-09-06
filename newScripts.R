# Load generic libraries
library(xts)
library(TTR)
library(PerformanceAnalytics)

# Set the time-zone to UTC (UTC)
Sys.setenv(TZ="UTC")


# Load prices from CSV and remove NA's
ts.prices <- read.csv('C:/Data/Stock ETFs.csv', stringsAsFactors=FALSE)
ts.prices <- xts(sapply(ts.prices[,-1], as.numeric), order.by=as.POSIXlt(ts.prices[,1]) )
ts.prices <- na.locf(ts.prices)
ts.prices <- na.locf(ts.prices,fromLast=TRUE)
plot.zoo(ts.prices, main=paste(c('Stock ETFs ', format(Sys.time(),'%m-%d-%y', tz="UTC"))))

ts.rets <- diff(ts.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]


# Normalize returns
look.back <- 50
run.vol <- apply(ts.rets, 2, runSD, n=look.back)
run.vol <- xts(run.vol, order.by=index(ts.prices))
run.vol <- na.locf(run.vol, fromLast=TRUE)

ts.nrets <- ts.rets/run.vol
ts.nrets <- apply(ts.nrets, 2, runMean, n=look.back)
ts.nrets <- xts(ts.nrets, order.by=index(ts.prices))
ts.nrets <- na.locf(ts.nrets, fromLast=TRUE)


# Sort the assets according to recent performance
symbols.top <- t(apply(ts.nrets, 1, order))
symbols.top <- xts(symbols.top, order.by=index(ts.prices))
symbols.top <- lag(symbols.top)
symbols.top[1,] <- symbols.top[2,]


# Calculate PnLs
v.loadings <- 1:ncol(symbols.top)-ncol(symbols.top)/2
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- apply(ts.pnls, 1, sum)
ts.pnl <- cumsum(xts(ts.pnl, order.by=index(ts.prices)))
plot.zoo(ts.pnl)

# Trade EMA
symbol.name <- 'EEM'
ts.rets.ema <- EMA(coredata(ts.rets[,symbol.name]), n=look.back, ratio=0.2)
ts.rets.ema <- xts(ts.rets.ema, order.by=index(ts.rets))
na.lines <- is.na(ts.rets.ema)
ts.rets.ema[na.lines,] <- ts.rets[na.lines,symbol.name]
colnames(ts.rets.ema) <- paste(colnames(ts.rets[,symbol.name]),".ema",sep="")
dev.off()
chart.TimeSeries(cumsum(cbind(ts.rets[,symbol.name],ts.rets.ema)), main="EMA Smoothing", colorset=c(1,2), lty=c(1,1), ylab="", xlab="", legend.loc='topright')

ts.diff <- sign(cumsum(ts.rets[,1]-ts.rets.ema))
ts.diff <- lag(ts.diff)
ts.diff[1,] <- ts.diff[2,]
colnames(ts.diff) <- paste(colnames(ts.rets[,1]),".signals",sep="")

plot.zoo(cbind(ts.prices[,1],ts.diff,cumsum(ts.diff*ts.rets[,1])), main=paste(c('PnL ', format(Sys.time(),'%m-%d-%y', tz="EST"))) )


### VAR trading
library(vars)

ts.rets.ema <- cbind(ts.rets[,1:4],ts.rets.ema)
plot.zoo(cbind(cumsum(ts.rets.ema)))
sum(is.na(ts.rets.ema))
# ts.rets.ema <- ts.rets[,c(1:3,8:15)]

range.date <- matrix(look.back:nrow(ts.rets))
ts.predict <- ts.rets[1:(look.back-1),'EEM']
colnames(ts.predict) <- 'forecast'
predict.var <- apply(range.date, 1, function(n.row)
{
  var.model <- VAR(y=ts.rets[(n.row-look.back+1):n.row,1:4], lag.max=6)
#  var.model <- VAR(y=ts.rets.ema[(n.row-look.back+1):n.row,], lag.max=6)
  predict.var.model <- predict(var.model, n.ahead=1)
  predict.data <- predict.var.model$fcst$EEM[1,'fcst']
  ts.predict <<- rbind(ts.predict, xts(predict.data, order.by=index(ts.rets[n.row,])))
  predict.data
}
)
ts.positions <- lag(sign(ts.predict)*sqrt(abs(ts.predict)))
ts.positions[1:look.back,] <- 0.0

plot.zoo(cbind(ts.prices[,'EEM'],cumsum(ts.positions*ts.rets[,'EEM'])), main='Trading on signals from VAR forecasts')
plot.zoo(cbind(ts.prices[,'EEM'],cumsum(ts.positions*ts.rets[,'EEM']))["2012/"], main='Trading on signals from VAR forecasts')
