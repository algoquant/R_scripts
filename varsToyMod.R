### Run alphaModel for VAR model
library(xts)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)
library(vars)

Sys.setenv(TZ="UTC")
data.dir <- "C:/Data/"
alpha.dir <- "C:/Develop/GIThub/alphaLib/"
rmodels.dir <- "C:/Develop/GIThub/Rmodels/"

# Load data
mex.prices <- read.csv(paste(data.dir, "MEX.csv", sep=""), stringsAsFactors=FALSE)
mex.prices <- xts(mex.prices[,-1], order.by=as.POSIXlt(mex.prices[,1]) )
colnames(mex.prices) <- c('mex.cds','EWW','IG','OIL','MXN','MXN.vol')
plot.zoo(mex.prices, main=paste(c('MEXICO ', format(Sys.time(),'%m-%d-%y', tz="GMT"))))
mex.rets <- diff(log(mex.prices))
mex.rets[1,] <- mex.rets[2,]

# Run alphaModel
source(paste(alpha.dir, "alphaModel.R", sep=""))
source(paste(alpha.dir, "utilLib.R", sep=""))
source(paste(rmodels.dir, "chartLib.R", sep=""))
source(paste(rmodels.dir, "alphaModelVAR.R", sep=""))
ts.bidoffers <- xts(rep(0.017, nrow(mex.prices)), order.by=index(mex.prices))
func.signal <- list(signal.func="signals.ancillary1.1.alphaModel", filter.func="VAR", filter.params=c(200,0.01), normalize.returns=FALSE, normalize.signal=FALSE)
trading.rules <- list(rules.func="fillOrders4.2.alphaModel", rules.params=0.0)
model.test <- alphaModel("MEXICO rets VAR Model")
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=mex.rets, ts.bidoffers=ts.bidoffers)
model.test <- recalc.alphaModel(model.test)
chart.data <- cbind(model.test)
chart_Series(chart.data[,'PnLs'], name="mex.prices")
