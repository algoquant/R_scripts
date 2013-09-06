fillOrders4.1.alphaModel <- function(model, ...) {
model$positions <- sign(model$signals)
colnames(model$positions) <- "Positions"
model
}


signals.ancillary1.1.alphaModel <- function(model) {
stopifnot(inherits(model, "alphaModel"))
ts.prices <- model$ancillary$prices
func.name <- match.fun(model$signal.list$filter.func)
signal.list <- model$signal.list$filter.params
signal.list[3] <- 0
model$signals <- xts(
                     do.call(
                             func.name,
                             append(list(coredata(ts.prices)), signal.list)
                             ),
                     order.by=index(model$prices)
                     )
model$signals[1,] <- na.omit(model$signals)[1,]
model$signals <- na.locf(model$signals)
look.back <- model$signal.list$filter.params[3]
model$signals <- diff(model$signals, look.back)
model$signals[1:look.back,] <- 0.0
colnames(model$signals) <- "Signals"
model
}



model.values <- function(comb, model.input) {
  func.signal <- list(filter.params=c(model.input$signal.list$filter.params[1],comb[['width']],comb[['window']],model.input$signal.list$filter.params[4]))
  model <- update.alphaModel(model=model.input, func.signal=func.signal)
  model <- recalc.alphaModel(model)
  c(pnl=model$performance$CumPnL, width=comb[['width']], window=comb[['window']])
}
