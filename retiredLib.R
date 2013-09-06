
########################
### makeOrders
########################
# This can be deprecated - not called anymore
# makeOrders function calculates a vector of buy.sell.orders for a single threshold level, based on the signals vector
#' @export
makeOrders.alphaModel <- function(model, threshold) {

#  signals.slope <- diff(model$signals)
#  signals.slope <- filtSavGol(model$signals, M=2, nl=100, nr=0, ld=1)
#  signals.slope <- xts(signals.slope, order.by=index(model$signals))

#  buy.sell.orders <- ifelse((model$signals*lag(model$signals)<0) & (model$signals*model$thresholds[threshold,2])>0, model$thresholds[threshold,3], NA)
  buy.sell.orders <- ifelse((((model$signals-threshold[1])*(lag(model$signals)-threshold[1]))<0)&((model$signals-threshold[1])*threshold[2])>0, threshold[3], NA)

  buy.sell.orders
}
# End makeOrders


