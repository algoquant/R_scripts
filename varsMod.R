### Vector autoregressive and vector error correction models, and cointegration analysis using package vars
library(vars)

# Prepare data
# Calculate cumsum(ts.rets)
ts.data <- cumsum(ts.rets)

# Perform cointegration analysis for IG versus PC1
# Calculate VECM model using Johansen (library urca)
johansen.model <- ca.jo(x=ts.data['2012-09/',c('IG','PC1')], spec="transitory")
summary(johansen.model)
# Convert Johansen VECM to VAR model
var.model <- vec2var(johansen.model)
summary(var.model)
# Calculate OLS regression from VECM
lm.model <- cajorls(var.model)
summary(lm.model)

# Calculate VAR model using OLS regression (library vars)
var.model <- VAR(y=ts.data['2012-09/',c('IG','PC1')], lag.max=3)
summary(var.model)

# Plot fitted VAR model values
plot(var.model)
fitted.values <- cbind(var.model$datamat[,'IG',drop=FALSE],var.model$varresult$IG$fitted.values)
colnames(fitted.values) <- c('IG','IG fitted')
chart.TimeSeries(fitted.values, main="IG fitted from VAR model", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')

# Plot cointegration residuals
plot.zoo(var.model$resid, main="Residuals of IG versus PC1")

# Check for causality
causality(var.model, cause='PC1')

# Forecast from VAR model and produce fanchart
predict.var.model <- predict(var.model, n.ahead=8, ci=0.95)
fanchart(predict.var.model)


look.back <- 120
range.date <- matrix((look.back+1):nrow(ts.rets))
# range.date <- index(ts.data[(look.back+1):nrow(ts.rets),])
# Perform cointegration over sliding window
data.var <- apply(range.date, 1, function(n.row)
                  {
#                           var.model <- ca.jo(x=ts.data[(n.row-look.back):n.row,c('IG','PC1')], spec="transitory")
                    var.model <- VAR(y=ts.data[(n.row-look.back):n.row,c('IG','PC1')], lag.max=3)
                    sum.var <- summary(var.model)$varresult$IG$coefficients
#                           browser()
#                           c(coint.vec=var.model@V[2,1], coint.lambda=var.model@lambda, johan.test=var.model@teststat[2])
                    c(vec=sum.var[,"Estimate"], t.value=sum.var[,"t value"])
#                           xts(t(as.matrix(x=c(vec=var.model@V[2,1],test=var.model@teststat[2]))), order.by=index(ts.data[n.row,]))
#                           c(vec=var.model@V[2,1],test=var.model@teststat[2])
                  }
                  )
# End apply
data.var <- do.call('rbind', data.var)
# Convert matrix into xts
data.var <- xts(data.var, order.by=index(ts.data[range.date,]))
data.var <- na.omit(cbind(ts.data[,c('IG','PC1')],data.var))
plot.zoo(data.var, main="data.var")


predictVAR <- function(look.back) {
range.date <- matrix(look.back:nrow(ts.data))
ts.predict <- ts.data[1:(look.back-1),'PC1']
colnames(ts.predict) <- 'forecast'
# Perform forecasting over sliding window
predict.var <- apply(range.date, 1, function(n.row)
                     {
                       var.model <- VAR(y=ts.data[(n.row-look.back+1):n.row,c('IG','PC1')], lag.max=3)
                       predict.var.model <- predict(var.model, n.ahead=1)
                       predict.data <- predict.var.model$fcst$IG[1,'fcst']
                       ts.predict <<- rbind(ts.predict, xts(predict.data, order.by=index(ts.data[n.row,])))
                       predict.data
                     }
                     )
# End apply
# ts.predict <- ts.predict[-1,]
}
# End predictVAR


### Run simple trading strategy
ts.positions <- diff(lag(ts.predict))
ts.positions[1:2,] <- 0.0
plot.zoo(cbind(ts.data,cumsum(ts.positions*ts.rets[,'IG'])), main='Trading on signals from VAR forecasts')


ts.predict <- na.omit(cbind(ts.data[,'IG'],ts.predict))
# ts.predict <- lag.xts(ts.predict)
# ts.predict[1,] <- 0.0
ts.resid <- ts.predict[,1]-ts.predict[,2]
pacf(ts.resid)
par(mfrow=c(2,1))
chart.TimeSeries(ts.predict, main="IG forecasts from VAR model", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')
chart.TimeSeries(ts.resid, main="IG forecast residuals", colorset=1, lty=1, ylab="", xlab="")


### Run alphaModel trading strategy
ts.predict <- ts.predict[-1,]
rets.predict <- diff(ts.predict)
rets.predict[1,] <- 0.0

model.test <- alphaModel("IG/PC1 Proxy Daily Model")

runalphaModel <- function(rets.predict) {
ts.bidoffers <- xts(rep(0.0025, nrow(rets.predict)), order.by=index(rets.predict))
names(ts.bidoffers) <- 'BID.OFFERS'
ts.ancillary <- na.omit(cbind(rets.predict, ts.bidoffers))
ts.prices.ig <- na.omit(cbind(rets.predict, ts.data[,'IG']))
ts.prices.ig <- ts.prices.ig[,2]

func.signal <- list(signal.func="signals.ancillary1.1.alphaModel")
trading.rules <- list(rules.func="fillOrders4.1.alphaModel", rules.params=c(0.77,0.2))

model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=ts.prices.ig, ts.bidoffers=ts.bidoffers, ts.ancillary=ts.ancillary)
model.test <- recalc.alphaModel(model.test)

# summary(model.test)
# plot.alphaModel(model.test, n.flips.max=500)

model.test$performance$CumPnL
}
# End runalphaModel


### Run loop over look-back windows
look.backs <- matrix(10*(5:20))
performance.var <- apply(look.backs, 1, function(look.back)
                         {
                           ts.predict <- predictVAR(look.back)
                           rets.predict <- diff(ts.predict)
                           rets.predict[1,] <- 0.0
                           runalphaModel(rets.predict)
                         }
                         )
# End apply
performance.var <- cbind(look.backs, performance.var)
colnames(performance.var) <- c('look-back','performance')

### Functions

# Get signals from ancillary prices (precalculated)
signals.ancillary1.1.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))
  model$signals <- model$ancillary$prices
  colnames(model$signals) <- "Signals"
  model
}


# Calculate signals using VAR model with either price or return inputs
signals.ancillary1.1.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))
  look.back <- model$signal.list$filter.params[1]
  ts.data <- model$prices
  name.forecast <- colnames(model$prices[,1])
  colnames(ts.data[,1]) <- 'indep.var'

  range.date <- matrix(look.back:nrow(ts.data))
  ts.predict <- 0.0*ts.data[1:(look.back-1),1]
  colnames(ts.data) <- c('indep.var',colnames(ts.data)[-1])
# Perform forecasting over sliding window
  predict.var <- apply(range.date, 1, function(n.row)
                       {
                         var.model <- VAR(y=ts.data[(n.row-look.back+1):n.row,], lag.max=3)
                         predict.var.model <- predict(var.model, n.ahead=1)
                         predict.data <- predict.var.model$fcst$indep.var[1,'fcst']
                         ts.predict <<- rbind(ts.predict, xts(predict.data, order.by=index(ts.data[n.row,])))
                       }
                       )
# End apply

#  model$signals <- diff(ts.predict)
  model$signals <- ts.predict
  model$signals[1,] <- 0.0
  model$signals[look.back,] <- 0.0
  colnames(model$signals) <- "Signals"
  model
}
# End signals.ancillary1.1.alphaModel


# The function fillOrders4.1 calculates positions from VAR signal
fillOrders4.1.alphaModel <- function(model, ...) {
  look.back <- model$signal.list$filter.params[1]
  model$positions <- diff(model$signals)
  model$positions[1:(look.back+1),] <- 0.0
  colnames(model$positions) <- "Positions"
  model
}
# End fillOrders4.1.alphaModel


# The function fillOrders4.2 calculates positions from VAR signal
fillOrders4.2.alphaModel <- function(model, ...) {
  look.back <- model$signal.list$filter.params[1]
  threshold <- model$signal.list$filter.params[2]
  #threshold=0.01
  pos <- (abs(model$signals) > threshold)*sign(model$signals)
  signal<-model$signals
  pos[pos==0]<-NA; pos[1]=0; pos<-na.locf(pos)
  # pos[pos<0&signal<threshold&signal>0]<-0  ### set position to zero if signal is weak the opposite way
  # pos[pos>0&signal>-threshold&signal<0]<-0 ### set position to zero if signal is weak the opposite way
  model$positions <-pos
  model$positions[1:(look.back+1),] <- 0.0
  
  colnames(model$positions) <- "Positions"
  model
}
# End fillOrders4.2.alphaModel


# This is a modified calcProfitLoss function for calculating PnLs from returns - without diff(model$prices)
calcProfitLoss.alphaModel <- function(model) {
  singlePnL <- function(returns, positions, bidoffers, betas) {
    pnl <- lag(positions*betas)*returns - abs(diff(positions*betas))*bidoffers/2
    pnl
  }
  asset.returns <- (model$prices[,1])
  asset.returns[1,] <- 0.0
  pnls.model <- singlePnL(asset.returns, model$positions, model$bidoffers, model$betas)
  pnls.model[1:2,] <- 0.0
  model$times.stop.loss <- xts(rep(FALSE, length(model$prices[,1])), order.by=index(model$prices))
  model$pnls <- pnls.model
  colnames(model$pnls) <- "PnLs"
  model
}
# End calcProfitLoss.alphaModel


