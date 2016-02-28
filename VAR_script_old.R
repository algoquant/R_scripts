tail(predict.var)
summary(var.model)
rm(var.model)
class(summary.var.model)
summary.var.model
rm(summary.var.model)
range.date <- matrix(look.back:nrow(ts.rets))
ts.predict <- ts.rets[1:(look.back-1),1]
colnames(ts.predict) <- 'forecast'
predict.var <- apply(range.date, 1, function(n.row)
{
var.model <- VAR(y=ts.rets[(n.row-look.back+1):n.row,], lag.max=6)
summary.var.model <- summary(var.model)
#  var.model <- VAR(y=ts.rets.ema[(n.row-look.back+1):n.row,], lag.max=6)
predict.var.model <- predict(var.model, n.ahead=1)
predict.data <- predict.var.model$fcst[[1]][1,'fcst']
ts.predict <<- rbind(ts.predict, xts(predict.data, order.by=index(ts.rets[n.row,])))
c(summary.var.model$varresult[[1]]$sigma, summary.var.model$varresult[[1]]$r.squared, summary.var.model$varresult[[1]]$fstatistic[1], lmp(var.model,1)[[1]])
}
)
summary.var.model
savehistory("C:/Develop/GIThub/Rmodels/temp.R")
blah <- apply(range.date, 1, function(n.row)
{
var.mod <- VAR(y=ts.rets[(n.row-look.back+1):n.row,], lag.max=6)
summ.var.mod <- summary(var.mod)
c(summary.var.mod$varresult[[1]]$sigma, summary.var.mod$varresult[[1]]$r.squared, summary.var.mod$varresult[[1]]$fstatistic[1], lmp(var.mod,1)[[1]])
}
)
blah <- apply(range.date, 1, function(n.row)
{
var.mod <- VAR(y=ts.rets[(n.row-look.back+1):n.row,], lag.max=6)
summ.var.mod <- summary(var.mod)
c(summ.var.mod$varresult[[1]]$sigma, summ.var.mod$varresult[[1]]$r.squared, summ.var.mod$varresult[[1]]$fstatistic[1], lmp(var.mod,1)[[1]])
}
)
rm(var.model)
blah <- apply(range.date, 1, function(n.row)
{
bs.mod <- VAR(y=ts.rets[(n.row-look.back+1):n.row,], lag.max=6)
summ.bs.mod <- summary(bs.mod)
c(summ.bs.mod$varresult[[1]]$sigma, summ.bs.mod$varresult[[1]]$r.squared, summ.bs.mod$varresult[[1]]$fstatistic[1], lmp(bs.mod,1)[[1]])
}
)
source('C:/Develop/GIThub/alphaLib/defaults.R', echo=TRUE)
ts.prices <- read.csv('C:/Data/Stock ETFs.csv', stringsAsFactors=FALSE)
ts.prices <- xts(sapply(ts.prices[,-1], as.numeric), order.by=as.POSIXlt(ts.prices[,1]) )
ts.prices <- na.locf(ts.prices)
ts.prices <- na.locf(ts.prices,fromLast=TRUE)
plot.zoo(ts.prices, main=paste(c('Stock ETFs ', format(Sys.time(),'%m-%d-%y', tz="UTC"))), xlab="")
ts.rets <- diff(ts.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]
ts.rets <- diff(ts.prices[,c('EEM','IVV')],lag=1)
ts.rets[1,] <- ts.rets[2,]
class(ts.rets)
library(vars)
spectral.frac
func.objective <- function(v.weights, ts.rets) {
spectral.frac(ts.rets %*% c(1,v.weights))
}
n.col <- ncol(ts.rets)
optim.obj <- optim(par=rep(0,n.col-1), fn=func.objective, ts.rets=ts.rets, method="L-BFGS-B", lower=-rep(100,n.col-1), upper=rep(100,n.col-1), control=list(trace=1, pgtol=1e-2, factr=1e7))
tail(ts.prices[,c('EEM','IVV')])
optim.obj
spectral.frac(ts.rets[,1])
spectral.frac(ts.rets[,2])
optim.obj <- optim(par=rep(0,n.col-1), fn=-func.objective, ts.rets=ts.rets, method="L-BFGS-B", lower=-rep(100,n.col-1), upper=rep(100,n.col-1), control=list(trace=1, pgtol=1e-2, factr=1e7))
?spectrum
ts.random <- rnorm(nrow(ts.prices))
spectral.frac(ts.random)
spectral.frac(ts.random,0.5)
spectral.frac(ts.random,0.95)
spectral.frac(ts.random,0.15)
ts.random <- sin(20*(1:nrow(ts.data))/nrow(ts.data))
ts.random <- sin(20*(1:nrow(ts.prices))/nrow(ts.prices))
spectral.frac(ts.random)
ts.random <- diff(sin(20*(1:nrow(ts.prices))/nrow(ts.prices)))
ts.random[1,] <- ts.random[2,]
ts.random[1] <- ts.random[2]
spectral.frac(ts.random)
spectral.frac(as.vector(ts.rets[,1]))
spectral.frac(ts.rets[,1])
spectral.frac(as.matrix(ts.rets[,1]))
blah <- spectrum(coredata(ts.rets[,1]))
blah <- spectrum(coredata(ts.rets[,2]))
blah <- spectrum(coredata(ts.random))
ts.random <- sin(20*(1:nrow(ts.prices))/nrow(ts.prices))+1.0*rnorm(nrow(ts.prices))
blah <- spectrum(coredata(ts.random))
ts.random <- sin(20*(1:nrow(ts.prices))/nrow(ts.prices))+0.1*rnorm(nrow(ts.prices))
blah <- spectrum(coredata(ts.random))
ts.random <- sin(20*(1:nrow(ts.prices))/nrow(ts.prices))+0.01*rnorm(nrow(ts.prices))
blah <- spectrum(coredata(ts.random))
ts.random <- sin(20*(1:nrow(ts.prices))/nrow(ts.prices))+0.001*rnorm(nrow(ts.prices))
blah <- spectrum(coredata(ts.random))
tail(blah$spec)
head(blah$spec)
spectral.frac <- function(ts.rets, cutoff=0.25) {
specDensity <- spectrum(coredata(ts.rets), plot=FALSE)$spec
hfreqEnergy <- sum(head(specDensity, cutoff*length(specDensity)))
hfreqFrac <- 100*hfreqEnergy/sum(specDensity)/cutoff
hfreqFrac
}
spectral.frac(as.matrix(ts.rets[,1]))
spectral.frac(as.matrix(ts.rets[,2]))
spectral.frac(ts.random)
ts.random <- rnorm(nrow(ts.prices))
spectral.frac(ts.random)
colnames(ts.rets)
ts.rets <- diff(ts.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]
data.spec <- as.matrix(apply(ts.rets, 2, function(ts.ret) tryCatch(spectral.frac(ts.ret), error=function(e) writeMessage(e)) ))
colnames(data.spec) <- 'Spectrum.Scores'
data.spec
blah <- spectrum(coredata(ts.rets[,'EWW']))
library(ForeCA)
blah <- SDF(coredata(ts.rets[,'EWW']),''wosa'')
blah <- SDF(coredata(ts.rets[,'EWW']),'wosa')
plot(blah)
ts.random <- sin(20*(1:nrow(ts.prices))/nrow(ts.prices))
blah <- SDF(coredata(ts.random),'wosa')
plot(blah)
ts.random <- rnorm(nrow(ts.prices))
length(ts.random)
nrow(ts.prices)
blah <- SDF(coredata(ts.random),'wosa')
plot(blah)
tail(blah)
length(blah)
class(blah)
min(blah)
max(blah)
sum(blah)
blah
spectral.frac <- function(ts.rets, cutoff=0.25) {
specDensity <- SDF(coredata(ts.rets),'wosa')
hfreqEnergy <- sum(head(specDensity, cutoff*length(specDensity)))
hfreqFrac <- 100*hfreqEnergy/sum(specDensity)/cutoff
hfreqFrac
}
data.spec <- as.matrix(apply(ts.rets, 2, function(ts.ret) tryCatch(spectral.frac(ts.ret), error=function(e) writeMessage(e)) ))
colnames(data.spec) <- 'Spectrum.Scores'
data.spec
savehistory("C:/Develop/GIThub/Rmodels/temp.R")
func.objective <- function(v.weights, ts.rets) {
-spectral.frac(ts.rets %*% c(1,v.weights))
}
n.col <- ncol(ts.rets)
ts.rets <- diff(ts.prices[,c('IWO','IVV')],lag=1)
ts.rets[1,] <- ts.rets[2,]
n.col <- ncol(ts.rets)
optim.obj <- optim(par=rep(0,n.col-1), fn=func.objective, ts.rets=ts.rets, method="L-BFGS-B", lower=-rep(100,n.col-1), upper=rep(100,n.col-1), control=list(trace=1, pgtol=1e-2, factr=1e7))
v.weights <- as.matrix(c(1,optim.obj$par))
rownames(v.weights) <- colnames(ts.rets)
colnames(v.weights) <- 'weights'
optim.rets <- xts(ts.rets %*% v.weights, order.by=index(ts.rets))
colnames(optim.rets) <- 'optim.rets'
chart_Series(cumsum(optim.rets), name="Optimal Portfolio", xlab="", ylab="", legend.loc='topright')
sum(is.na(optim.rets))
tail(optim.rets)
chart_Series(cumsum(ts.rets[,1]), name="Optimal Portfolio", xlab="", ylab="", legend.loc='topright')
v.weights
index(optim.rets) <- index(optim.rets)
chart_Series(cumsum(optim.rets), name="Optimal Portfolio", xlab="", ylab="", legend.loc='topright')
plot(cumsum(optim.rets),type='l')
optim.rets <- xts(ts.rets %*% v.weights, order.by=index(ts.rets))
class(optim.rets)
plot(cumsum(optim.rets),type='l')
plot(cumsum(ts.rets %*% v.weights),type='l')
length(ts.rets %*% v.weights)
optim.rets <- xts(as.vector(ts.rets %*% v.weights), order.by=index(ts.rets))
plot(cumsum(optim.rets),type='l')
dim(optim.rets)
tail(optim.rets)
head(optim.rets)
optim.rets <- xts(rnorm(nrow(ts.prices)), order.by=index(ts.rets))
plot(cumsum(optim.rets),type='l')
class(ts.rets)
plot(cumsum(ts.rets[,1]),type='l')
plot(cumsum(ts.rets[,1]),type='l')
index(ts.rets) <- index(ts.rets)
optim.rets <- xts(rnorm(nrow(ts.prices)), order.by=index(ts.rets))
plot(cumsum(optim.rets),type='l')
optim.rets <- xts(rnorm(nrow(ts.prices)), order.by=index(ts.prices))
plot(cumsum(optim.rets),type='l')
plot(optim.rets,type='l')
dim(optim.rets)
dim(ts.prices)
indexTZ(ts.prices)
Sys.setenv(TZ="UTC")
ts.prices <- read.csv('C:/Data/Stock ETFs.csv', stringsAsFactors=FALSE)
ts.prices <- xts(sapply(ts.prices[,-1], as.numeric), order.by=as.POSIXlt(ts.prices[,1]) )
ts.prices <- na.locf(ts.prices)
ts.prices <- na.locf(ts.prices,fromLast=TRUE)
plot.zoo(ts.prices, main=paste(c('Stock ETFs ', format(Sys.time(),'%m-%d-%y', tz="UTC"))), xlab="")
ts.rets <- diff(ts.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]
ts.rets <- diff(ts.prices[,c('EEM','IVV')],lag=1)
ts.rets[1,] <- ts.rets[2,]
optim.rets <- xts(rnorm(nrow(ts.prices)), order.by=index(ts.prices))
plot(cumsum(optim.rets),type='l')
optim.rets <- xts(as.vector(ts.rets %*% v.weights), order.by=index(ts.rets))
chart_Series(cumsum(optim.rets), name="Optimal Portfolio", xlab="", ylab="", legend.loc='topright')
savehistory("C:/Develop/GIThub/Rmodels/temp.R")
ts.rets <- diff(ts.prices[,c('IWO','IVV')],lag=1)
ts.rets[1,] <- ts.rets[2,]
optim.rets <- xts(as.vector(ts.rets %*% v.weights), order.by=index(ts.rets))
chart_Series(cumsum(optim.rets), name="Optimal Portfolio", xlab="", ylab="", legend.loc='topright')
func.objective(optim.rets)
func.objective
spectral.frac(optim.rets)
optim.obj
look.back <- 200
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
source('C:/Develop/GIThub/Rmodels/funcUtil.R', echo=TRUE)
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
ts.rets <- diff(ts.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]
run.vol <- apply(ts.rets, 2, runSD, n=look.back)
run.vol <- xts(run.vol, order.by=index(ts.prices))
run.vol <- na.locf(run.vol, fromLast=TRUE)
ts.nrets <- ts.rets/run.vol
ts.nrets <- apply(ts.nrets, 2, runMean, n=look.back)
ts.nrets <- xts(ts.nrets, order.by=index(ts.prices))
ts.nrets <- na.locf(ts.nrets, fromLast=TRUE)
symbols.top <- t(apply(ts.nrets, 1, order))
symbols.top <- xts(symbols.top, order.by=index(ts.prices))
symbols.top <- lag(symbols.top)
symbols.top[1,] <- symbols.top[2,]
v.loadings <- 1:ncol(symbols.top)-ncol(symbols.top)/2
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- apply(ts.pnls, 1, sum)
ts.pnl <- cumsum(xts(ts.pnl, order.by=index(ts.prices)))
plot.zoo(ts.pnl, xlab="")
look.back <- 50
run.vol <- apply(ts.rets, 2, runSD, n=look.back)
run.vol <- xts(run.vol, order.by=index(ts.prices))
run.vol <- na.locf(run.vol, fromLast=TRUE)
ts.nrets <- ts.rets/run.vol
ts.nrets <- apply(ts.nrets, 2, runMean, n=look.back)
ts.nrets <- xts(ts.nrets, order.by=index(ts.prices))
ts.nrets <- na.locf(ts.nrets, fromLast=TRUE)
symbols.top <- t(apply(ts.nrets, 1, order))
symbols.top <- xts(symbols.top, order.by=index(ts.prices))
symbols.top <- lag(symbols.top)
symbols.top[1,] <- symbols.top[2,]
v.loadings <- 1:ncol(symbols.top)-ncol(symbols.top)/2
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- apply(ts.pnls, 1, sum)
ts.pnl <- cumsum(xts(ts.pnl, order.by=index(ts.prices)))
plot.zoo(ts.pnl, xlab="")
look.back <- 20
run.vol <- apply(ts.rets, 2, runSD, n=look.back)
run.vol <- xts(run.vol, order.by=index(ts.prices))
run.vol <- na.locf(run.vol, fromLast=TRUE)
ts.nrets <- ts.rets/run.vol
ts.nrets <- apply(ts.nrets, 2, runMean, n=look.back)
ts.nrets <- xts(ts.nrets, order.by=index(ts.prices))
ts.nrets <- na.locf(ts.nrets, fromLast=TRUE)
symbols.top <- t(apply(ts.nrets, 1, order))
symbols.top <- xts(symbols.top, order.by=index(ts.prices))
symbols.top <- lag(symbols.top)
symbols.top[1,] <- symbols.top[2,]
v.loadings <- 1:ncol(symbols.top)-ncol(symbols.top)/2
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- apply(ts.pnls, 1, sum)
ts.pnl <- cumsum(xts(ts.pnl, order.by=index(ts.prices)))
plot.zoo(ts.pnl, xlab="")
look.back <- 10
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
plot.zoo(ts.pnl, xlab="")
look.back <- 5
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
plot.zoo(ts.pnl, xlab="")
?runSD
run.vol <- apply(ts.rets, 2, runSD, n=look.back, cumulative=TRUE)
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
plot.zoo(ts.pnl, xlab="")
look.back
look.back <- 2
run.vol <- apply(ts.rets, 2, runSD, n=look.back, cumulative=TRUE)
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
plot.zoo(ts.pnl, xlab="")
spectral.frac(ts.pnl)
spectral.frac(ts.pnl["2009/"])
spectral.frac(ts.pnl["2010/"])
spectral.frac(ts.pnl["2011/"])
spectral.frac(ts.pnl["2012/"])
ts.temp <- ts.pnl
ts.temp <- ts.pnl["2009/"]
head(ts.temp)
ts.signals <- lag(sign(ts.temp)*sqrt(abs(ts.temp)))
ts.signals[1] <- ts.signals[2]
colnames(ts.signals) <- 'signals'
ts.pnls <- ts.signals*ts.temp
colnames(ts.pnls) <- 'PnLs'
chart_Series(cumsum(ts.pnls), name="Simple Trending Strategy", xlab="", ylab="", legend.loc='topright')
plot.zoo(cumsum(cbind(ts.temp,ts.pnls)), xlab="", ylab="", main="Simple Trending Strategy")
ts.signals <- lag(sign(ts.temp)*sqrt(abs(ts.temp)))
ts.signals[1] <- ts.signals[2]
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- apply(ts.pnls, 1, sum)
spectral.frac(ts.pnl["2009/"])
spectral.frac
ts.pnl <- xts(ts.pnl, order.by=index(ts.prices))
plot.zoo(cumsum(ts.pnl), xlab="")
spectral.frac(ts.pnl["2009/"])
spectral.frac(ts.pnl)
look.back <- 200
run.vol <- apply(ts.rets, 2, runSD, n=look.back, cumulative=TRUE)
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
plot.zoo(ts.pnl, xlab="")
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
plot.zoo(ts.pnl, xlab="")
colnames(ts.rets)
ts.nrets <- ts.rets
symbols.top <- t(apply(ts.nrets, 1, order))
symbols.top <- xts(symbols.top, order.by=index(ts.prices))
symbols.top <- lag(symbols.top)
symbols.top[1,] <- symbols.top[2,]
# Calculate PnLs
v.loadings <- 1:ncol(symbols.top)-ncol(symbols.top)/2
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- apply(ts.pnls, 1, sum)
ts.pnl <- cumsum(xts(ts.pnl, order.by=index(ts.prices)))
plot.zoo(ts.pnl, xlab="")
ts.nrets <- diff(log(ts.prices),lag=1)
ts.nrets[1,] <- ts.nrets[2,]
symbols.top <- t(apply(ts.nrets, 1, order))
symbols.top <- xts(symbols.top, order.by=index(ts.prices))
symbols.top <- lag(symbols.top)
symbols.top[1,] <- symbols.top[2,]
# Calculate PnLs
v.loadings <- 1:ncol(symbols.top)-ncol(symbols.top)/2
ts.pnls <- t(apply(matrix(1:length(ts.prices[,1])), 1,  function(row.num)  v.loadings*ts.rets[row.num,symbols.top[row.num,]]))
ts.pnl <- apply(ts.pnls, 1, sum)
ts.pnl <- cumsum(xts(ts.pnl, order.by=index(ts.prices)))
plot.zoo(ts.pnl, xlab="")
plot.zoo(ts.pnl["2009/"], xlab="")
look.back
look.back <- 100
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
plot.zoo(ts.pnl["2009/"], xlab="")
look.back
look.back <- 200
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
plot.zoo(ts.pnl["2009/"], xlab="")
look.back <- 2
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
plot.zoo(ts.pnl["2009/"], xlab="")
look.back <- 100
savehistory("C:/Develop/GIThub/Rmodels/temp.R")
