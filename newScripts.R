# Load generic libraries
library(xts)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)

# Set the time-zone to UTC (UTC)
Sys.setenv(TZ="UTC")


# Load prices from CSV and remove NA's
ts.prices <- read.csv('C:/Data/Stock ETFs.csv', stringsAsFactors=FALSE)
ts.prices <- xts(sapply(ts.prices[,-1], as.numeric), order.by=as.POSIXlt(ts.prices[,1]) )
ts.prices <- na.locf(ts.prices)
ts.prices <- na.locf(ts.prices,fromLast=TRUE)
plot.zoo(ts.prices, main=paste(c('Stock ETFs ', format(Sys.time(),'%m-%d-%y', tz="UTC"))), xlab="")

ts.rets <- diff(ts.prices,lag=1)
ts.rets <- diff(log(ts.prices[,c('XLY','IVV')]),lag=1)
ts.rets <- diff(log(ts.prices),lag=1)
ts.rets[1,] <- ts.rets[2,]
# Normalize returns (global)
vol.mat <- sqrt(as.matrix(apply(ts.rets, 2, var)))
colnames(vol.mat) <- "SD"
ts.rets <- xts(t(t(ts.rets)/as.numeric(vol.mat)), order.by=index(ts.rets) )


# Normalize returns (running)
look.back <- 50
# run.vol <- apply(ts.rets, 2, runSD, n=look.back, cumulative=TRUE)
run.vol <- apply(ts.rets, 2, runSD, n=look.back)
run.vol <- xts(run.vol, order.by=index(ts.prices))
run.vol <- na.locf(run.vol, fromLast=TRUE)

ts.nrets <- ts.rets/run.vol
ts.nrets <- apply(ts.nrets, 2, runMean, n=look.back)
ts.nrets <- xts(ts.nrets, order.by=index(ts.prices))
ts.nrets <- na.locf(ts.nrets, fromLast=TRUE)


### Momentum trading strategy
# Sort the assets according to recent performance
symbols.top <- t(apply(ts.nrets, 1, order))
symbols.top <- xts(symbols.top, order.by=index(ts.prices))
symbols.top <- lag(symbols.top)
symbols.top[1,] <- symbols.top[2,]

# Calculate PnLs
v.loadings <- 1:ncol(symbols.top)-ncol(symbols.top)/2
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- cumsum(xts(rowSums(ts.pnls), order.by=index(ts.prices)))
chart_Series(ts.pnl["2009/"], name="Momentum trading strategy", ylab="", xlab="", legend.loc='topright')
# End momentum strategy


### Momentum trading strategy over a range of look.back parameters
look.backs <- matrix(c(2*(1:4),10*(1:20)))
top.pnls <- apply(look.backs, 1,  function(n.look.back) {
  ts.mean.rets <- apply(ts.nrets, 2, runMean, n=n.look.back)
  ts.mean.rets <- xts(ts.mean.rets, order.by=index(ts.prices))
  ts.mean.rets <- na.locf(ts.mean.rets, fromLast=TRUE)
  top.symbols <- t(apply(ts.mean.rets, 1, order))
  top.symbols <- xts(top.symbols, order.by=index(ts.prices))
  top.symbols <- lag(top.symbols)
  top.symbols[1,] <- top.symbols[2,]
  top.loadings <- 1:ncol(top.symbols)-ncol(top.symbols)/2
  top.pnl <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  top.loadings*ts.rets[row.num,top.symbols[row.num,]]))
  top.pnl <- rowSums(top.pnl)
  top.pnl
}
)
top.pnls <- cumsum(xts(top.pnls, order.by=index(ts.prices)))
colnames(top.pnls) <- paste("look.back",look.backs,sep="-")
plot.zoo(top.pnls)
mean.pnls <- xts(rowMeans(top.pnls), order.by=index(ts.prices))
chart_Series(mean.pnls, name="Momentum trading strategy", ylab="", xlab="", legend.loc='topright')
# End momentum strategy


# Trade EMA
symbol.name <- 'EEM'
ts.rets.ema <- EMA(coredata(ts.rets[,symbol.name]), n=look.back, ratio=0.2)
ts.rets.ema <- xts(ts.rets.ema, order.by=index(ts.rets))
na.lines <- is.na(ts.rets.ema)
ts.rets.ema[na.lines,] <- ts.rets[na.lines,symbol.name]
colnames(ts.rets.ema) <- paste(colnames(ts.rets[,symbol.name]),".ema",sep="")
dev.off()
chart_Series(cumsum(cbind(ts.rets[,symbol.name],ts.rets.ema)), name="EMA Smoothing", colorset=c(1,2), lty=c(1,1), ylab="", xlab="", legend.loc='topright')

ts.diff <- sign(cumsum(ts.rets[,1]-ts.rets.ema))
ts.diff <- lag(ts.diff)
ts.diff[1,] <- ts.diff[2,]
colnames(ts.diff) <- paste(colnames(ts.rets[,1]),".signals",sep="")

plot.zoo(cbind(ts.diff,cumsum(cbind(ts.rets[,1],ts.diff*ts.rets[,1]))), main=paste(c('PnL ', format(Sys.time(),'%m-%d-%y', tz="EST"))), xlab="")


### VAR trading
library(vars)

ts.rets.ema <- cbind(ts.rets[,1:4],ts.rets.ema)
plot.zoo(cbind(cumsum(ts.rets.ema)), xlab="")
sum(is.na(ts.rets.ema))
# ts.rets.ema <- ts.rets[,c(1:3,8:15)]

range.date <- matrix(look.back:nrow(ts.rets))
ts.predict <- ts.rets[1:(look.back-1),1]
colnames(ts.predict) <- 'forecast'
predict.var <- apply(range.date, 1, function(n.row)
{
  var.model <<- VAR(y=ts.rets[(n.row-look.back+1):n.row,], lag.max=6)
  summary.var.model <- summary(var.model)
#  var.model <- VAR(y=ts.rets.ema[(n.row-look.back+1):n.row,], lag.max=6)
  predict.var.model <- predict(var.model, n.ahead=1)
  predict.data <- predict.var.model$fcst[[1]][1,'fcst']
  ts.predict <<- rbind(ts.predict, xts(predict.data, order.by=index(ts.rets[n.row,])))
  c(summary.var.model$varresult[[1]]$sigma, summary.var.model$varresult[[1]]$r.squared, summary.var.model$varresult[[1]]$fstatistic[1], lmp(var.model,1)[[1]])
}
)
ts.positions <- lag(sign(ts.predict)*sqrt(abs(ts.predict)))
ts.positions[1:look.back,] <- 0.0
predict.var <- xts(t(predict.var), order.by=index(ts.rets[look.back:nrow(ts.rets),]))
na.pad <- xts(matrix(rep(NA,4*(look.back-1)),ncol=4,byrow=TRUE), order.by=index(ts.rets[1:(look.back-1)]))
predict.var <- rbind(na.pad,predict.var)
predict.var <- na.locf(predict.var,fromLast=TRUE)
colnames(predict.var) <- c("RSE","R-squared","F-stat","p-value")
plot.zoo(predict.var, main='VAR diagnostics', xlab="")

plot.zoo(cumsum(cbind(ts.rets[,1],ts.positions*ts.rets[,1])), main='Trading on signals from VAR forecasts', xlab="")
plot.zoo(cbind(ts.prices[,1],cumsum(ts.positions*ts.rets[,1]))["2012/"], main='Trading on signals from VAR forecasts', xlab="")
