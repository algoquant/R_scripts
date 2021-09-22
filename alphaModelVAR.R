### Functions for running VAR models, using package vars
library(vars)

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


