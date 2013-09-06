###############################
### Optimization functions  ###
###############################
# Find the optimal portfolio over a period of time
optimPortf.alphaModel <- function(ts.prices, method="L-BFGS-B", control=list(trace=1, reltol=1e-4, factr=1e7, fnscale=-1), func.objective, time.window, lambda, ...) {

  if (length(ts.prices[,1])<(time.window+1))
    stop("optimPortf.alphaModel: Time series too short")

# Initialize the portfolio and temporal weights and other parameters
  portf.weights <- rep(1, ncol(ts.prices))
#  if (is.null(time.weights))
  time.weights <- c(1, sapply(1:(time.window-1), function(t.win) lambda**t.win))
  bounds <- 10

  funcObjective <- match.fun(func.objective)

#  ts.ret <- diff(ts.prices)
#  ts.ret[1,] <- ts.ret[2,]
#  ts.ret <- na.locf(ts.ret)
  ts.length <- length(ts.prices[,1])

  ts.portf.weights <- sapply((time.window+1):ts.length, 
                             function(time.loop) {
#                               writeMessage(c(time.loop, portf.weights), file.message="S:/Data/R_Data/Messages_opt.txt")
# Call optimizer
                               optim.results <- optim(portf.weights[-1],
                                                      function(optim.weights, first.weight, time.weights, ts.prices) funcObjective(ts.prices=ts.prices, portf.weights=c(first.weight, optim.weights), time.weights=time.weights),
                                                      first.weight=portf.weights[1],
                                                      time.weights=time.weights,
                                                      ts.prices=head(ts.prices, time.loop),
                                                      method=method,
                                                      lower=-bounds,
                                                      upper=bounds,
                                                      control=control
                                                      )

# Save optimized weights
                               if (length(optim.results$par)==length(portf.weights[-1]))
                                           portf.weights[-1] <- optim.results$par
# Return values
                               writeMessage(c(time.loop, optim.results$value, optim.results$counts, portf.weights), file.message="S:/Data/R_Data/Messages_opt.txt")
                               c(optim.results$value, optim.results$counts, portf.weights)
#    browser()
#  optimizeResults <- NULL
                             }
                             )
# End sapply
#    browser()

# Format and normalize
  ts.portf.weights <- aperm(ts.portf.weights, c(2,1))
  ts.optim.data <- ts.portf.weights[,1:3]
  ts.portf.weights <- ts.portf.weights[,-1:-3]
#  ts.portf.weights <- cbind(ts.portf.weights, ts.prices[,1])
#  ts.portf.weights <- ts.portf.weights[,-1]
#  ts.portf.weights[1,] <- 0.0
#  ts.portf.weights <- na.locf(ts.portf.weights)
#  portf.value <- ts.portf.weights[,1]
#  ts.portf.weights <- ts.portf.weights[,-1]
#  ts.portf.weights <- ts.portf.weights/pmax(0.0001, rowSums(abs(ts.portf.weights)))
#  colnames(portf.value) <- "Portf.value"
  colnames(ts.portf.weights) <- colnames(ts.prices)
#  portf.weights <- portf.weights / max(sum(portf.weights[portf.weights > 0]), -sum(portf.weights[portf.weights < 0]))

  ts.weights <- lag(ts.portf.weights)
  ts.weights[1,] <- 0.0
  ts.prices <- tail(ts.prices,ts.length-time.window)
  ts.prices <- xts(rowSums(coredata(ts.prices)*ts.weights), index(ts.prices))
  colnames(ts.prices) <- "Portf.value"

  optim.obj <- list(ts.prices=ts.prices, portf.weights=ts.portf.weights)
  class(optim.obj) <- c("optimPortf", "list")
  optim.obj

}
# End optimPortf


# Objective function for optimization
optimObjective.alphaModel <- function(ts.prices, portf.weights, time.weights=NULL, lambda=1.0, time.window=10) {

# Create an exponential vector of temporal weights
# and apply it as a one-sided moving average calculations, see help(filter)
#  movavg <- stats::filter(ts.prices, time.weights, method="convolution", sides=1)

#  if (is.null(time.weights))
#    time.weights <- c(1, sapply(1:(time.window-1), function(dta.win) lambda**t.win))
#  time.window <- length(time.weights)

  if (length(ts.prices[,1])>time.window)
    {
# Truncate time series to save calculations
      ts.prices <- tail(ts.prices, time.window)
# Multiply the multivariate portfolio time series by the portfolio weights to obtain a univariate time series
      ts.prices <- ts.prices %*% portf.weights
#  xts(ts.prices %*% c(first.weight, optim.weights), index(ts.prices))

# Calculate running SD
#      ts.sd <- runSD(ts.prices, n=time.window)
#      ts.sd[1] <- 0.0
#      ts.sd <- na.locf(ts.sd)
#      ts.prices <- tail(ts.prices, time.window)
#      ts.sd <- tail(ts.sd, time.window)
#  na.locf(ts.prices)
# Return WA return divided by WA SD
#      crossprod(ts.prices, time.weights)/crossprod(ts.sd, time.weights)
#      (last(ts.prices)-first(ts.prices))/(max(ts.prices)-min(ts.prices))
      (last(ts.prices)-first(ts.prices))/sd(diff(ts.prices)[-1])
    }
  else
    0.0

}
# End optimObjective


### Objective function for exploring model dependence on its parameters
objective.alphaModel <- function(model, func.signal) {

# Update the model
  model <- update.alphaModel(model=model, func.signal=func.signal)
# Run Model - calculate positions and pnls
  model <- recalc.alphaModel(model)
# Calculate performance statistics
  model.stat <- model$performance$CumPnL
#  model.stat <- coredata(model$pnls)
  model.stat

}
# End objective.alphaModel


# Calculate model values for a given combination of parameters - comb is a list
model.values <- function(comb, model.input) {
#  trading.rules <- list(rules.params=c(comb[['threshold1']], comb[['threshold2']]))
  trading.rules <- list(rules.params=c(comb[['threshold']], model.input$rules.list$rules.params[2]))
#  trading.rules <- list(rules.params=c(model.input$rules.list$rules.params[1], comb[['threshold']]))

  func.signal <- list(filter.params=c(model.input$signal.list$filter.params[1],comb[['width']],comb[['window']],model.input$signal.list$filter.params[4]))

  model <- update.alphaModel(model=model.input, func.signal=func.signal, trading.rules=trading.rules)
  model <- recalc.alphaModel(model)

#  print(paste('Running iteration: threshold =', model$rules.list$rules.params[1],'vol= ',model$rules.list$rules.params[2], 'width=', comb['width'], 'window =', comb['window'], 'pnl =', model$performance$CumPnL))
  c(pnl=model$performance$CumPnL, width=comb[['width']], window=comb[['window']], threshold=comb[['threshold']])
}

# Objective function for optim
objective.alphaModel <- function(param, model) {
  pnl <- model.values(list(width=param[1], window=param[2], threshold=param[3]), model)$pnl
  print(c(param,pnl))
  -pnl
}


# Calculate model value for a given window index
# The apply.windows and optim.params are lists of time windows and model parameters
model.apply <- function(window.index, apply.windows, optim.params, model.input) {
  time.window <- apply.windows[[window.index]]

  trading.rules <- list(rules.params=c(optim.params[[window.index,'threshold']], model.input$rules.list$rules.params[2]))
  func.signal <- list(filter.params=c(model.input$signal.list$filter.params[1],optim.params[[window.index,'width']],optim.params[[window.index,'window']],model.input$signal.list$filter.params[4]))

  model <- update.alphaModel(model=model.input, func.signal=func.signal, trading.rules=trading.rules)
  model <- recalc.alphaModel(model)

  c(width=optim.params[[window.index,'width']], window=optim.params[[window.index,'window']], threshold=optim.params[[window.index,'threshold']], pnl=sum(model$pnls[time.window]))
}
# End model.apply


### Calculate pnls for all parameter combinations, over a given time window
model.comb <- function(combs, time.window=NULL, model.input) {
# Update model with time series
#  time.window <- paste(start.date,"/",end.date, sep="")
  if (is.null(time.window))
    model <- model.input
  else
    {
      ts.ancillary <- cbind(model.input$ancillary$prices[time.window], model.input$ancillary$order.balance[time.window])
      model <- update.alphaModel(model=model.input, ts.prices=model.input$prices[time.window], ts.bidoffers=model.input$bidoffers[time.window], ts.ancillary=ts.ancillary)
    }

# Calculate optimal parameters using exhaustive search through all possible combinations
  profiles <- apply(combs, 1, model.values, model.input=model)
# Run apply on core cluster
#  profiles <- parApply(cl, combs, 1, model.values, model.input=model)
  profiles

}
# End model.comb



#############################
### MetaTrader functions  ###
#############################


### Function quickTrader performs a quick and simple test of a meta-trading model
quickMetaTrader.alphaModel <- function(func.signal, ts.prices) {

# The first element of func.signal is the filter function name
  func.name <- match.fun(func.signal$filter.func)
# The remaining elements of func.signal are the filter function parameters
  signal.list <- func.signal$filter.params
  meta.time.window <- signal.list[3]
# Set parameters for SavGol filter
  signal.list[3] <- 0
  signal.list[4] <- 0

# Apply the func.signal to the ts.prices
  ts.signals <- xts(
                    do.call(
                            func.name,
                            append(list(coredata(ts.prices)), signal.list)
                            ),
                    order.by=index(ts.prices)
                    )
  ts.signals[1,] <- na.omit(ts.signals)[1,]
  ts.signals <- na.locf(ts.signals)
  colnames(ts.signals) <- "Signals"

# Calculate positions
  ts.positions <- sign(diff(ts.signals))
  ts.positions[1,] <- ts.positions[2,]
  colnames(ts.positions) <- "Positions"

# Calculate PnLs
  ts.returns <- diff(ts.prices)
  ts.returns[1,] <- ts.returns[2,]
  ts.pnls <- lag(ts.positions)*ts.returns
  ts.pnls[1,] <- 0.0
  colnames(ts.pnls) <- "PnLs"
  ts.cumpnls <- cumsum(ts.pnls)

# Apply meta-trading model
# Set parameters for meta-trader SavGol filter
  signal.list[2] <- meta.time.window
# Apply the func.signal to the ts.cumpnls
  ts.signals <- xts(
                    do.call(
                            func.name,
                            append(list(coredata(ts.cumpnls)), signal.list)
                            ),
                    order.by=index(ts.cumpnls)
                    )
  ts.signals[1,] <- na.omit(ts.signals)[1,]
  ts.signals <- na.locf(ts.signals)
  colnames(ts.signals) <- "Meta-signals"

# Re-calculate positions and PnLs
  ts.metapositions <- sign(diff(ts.signals))
  ts.metapositions[1,] <- ts.metapositions[2,]
  colnames(ts.metapositions) <- "Meta-positions"
  ts.metapnls <- lag(ts.metapositions)*ts.pnls
  ts.metapnls[1,] <- 0.0
  colnames(ts.metapnls) <- "Meta-PnLs"

# Calculate performance
  CumPnL <- round(sum(ts.metapnls),4)
  n.flips <- sum(abs(na.omit(diff(sign(ts.metapositions*ts.positions)))))/2
  PnL.flip <- round(CumPnL/n.flips,4)
  c(CumPnL, n.flips, PnL.flip)

}
### End quickMetaTrader.alphaModel


### Function metaTrader trades a model based on its trailing performance
metaTrader.alphaModel <- function(model, func.signal) {

# The first element of func.signal is the filter function name
  func.name <- match.fun(func.signal$filter.func)
  signal.list <- func.signal$filter.params
  meta.time.window <- signal.list[3]
  func.signal$filter.params[3] <- model.test$signal.list$filter.params[3]

# Update the model with zero bidoffers, otherwise trading the model doesn't make sense
  ts.bidoffers <- xts(rep(0.0, length(model$prices[,1])), order.by=index(model$prices))
  model <- update.alphaModel(model=model, func.signal=func.signal, ts.bidoffers=ts.bidoffers)
  model <- recalc.alphaModel(model)
  ts.cumpnls <- cumsum(model$pnls)

# Set parameters for meta-trader SavGol filter
  signal.list[2] <- meta.time.window
  signal.list[3] <- 0
  signal.list[4] <- 0
# Apply the func.signal to the ts.cumpnls
  ts.signals <- xts(
                    do.call(
                            func.name,
                            append(list(coredata(ts.cumpnls)), signal.list)
                            ),
                    order.by=index(ts.cumpnls)
                    )
  ts.signals[1,] <- na.omit(ts.signals)[1,]
  ts.signals <- na.locf(ts.signals)
  colnames(ts.signals) <- "Meta-signals"

# Re-calculate positions and PnLs
  ts.metapositions <- sign(diff(ts.signals))
  ts.metapositions[1,] <- ts.metapositions[2,]
  colnames(ts.metapositions) <- "Meta-positions"
  ts.metapnls <- lag(ts.metapositions)*model$pnls
  ts.metapnls[1,] <- 0.0
  colnames(ts.metapnls) <- "Meta-PnLs"

# Calculate performance
  CumPnL <- round(sum(ts.metapnls),4)
  n.flips <- sum(abs(na.omit(diff(sign(ts.metapositions*model$positions)))))/2
  PnL.flip <- round(CumPnL/n.flips,4)
  c(CumPnL, n.flips, PnL.flip)


}
### End metaTrader.alphaModel


### Function metaPortfolioTrader dynamically re-allocates capital between different competing trading models based on their trailing performance
metaPortfolioTrader.alphaModel <- function(model, func.signals, time.window, file.data=NULL) {

### Calculate or load xts of model returns
  if (is.null(file.data)) {
# Run models
    pnls.model <- sapply(func.signals, function(func.signal) {
# Update the model
      model <- update.alphaModel(model=model, func.signal=func.signal)
# Run Model - calculate positions and pnls
      model <- recalc.alphaModel(model)
#    cat(paste(model$name, "/", date(), "\nSignal function:", paste(model$signal.list, collapse=" / "), "\n", paste(tail(model$pnls), collapse=" / "), "\n"))
#    cat(paste(model$name, "/", date(), "\nSignal function:", paste(model$signal.list, collapse=" / "), "\tNum:\t", length(ts.perf.scores), "\tNum of NAs:\t", length(ts.perf.scores[is.na(ts.perf.scores)]), "\n"))
    coredata(model$pnls)
    }
                         )
# End sapply

# Format PnL time series
    pnls.model <- xts(pnls.model, order.by=index(model$prices))
    names.model <- lapply(func.signals, paste, collapse="-")
    colnames(pnls.model) <- names.model

  }
else
# Load model returns from file
  load(file=file.data)


### Compile dataframe of performance scores
  perf.scores <- sapply(1:ncol(pnls.model), function(col.num) {
    ts.perf.scores <- statPersist(cumsum(pnls.model[,col.num]), time.window=time.window)
    coredata(ts.perf.scores)
  }
                       )
# End sapply

# Format performance scores
  perf.scores <- xts(perf.scores, order.by=index(model$prices))
  names.model <- lapply(func.signals, paste, collapse="-")
  colnames(perf.scores) <- names.model
  save(pnls.model, perf.scores, file="S:/Data/R_Data/meta_model.RData")
# Normalize performance scores
  perf.scores <- perf.scores/pmax(0.0001, rowSums(abs(perf.scores)))

#  cat(paste("Before xts\tNum:\t", dim(pnls.model), "\tNum of NAs:\t", length(pnls.model[is.na(pnls.model)]), "\n"))
#  cat(paste("After xts\tNum:\t", dim(pnls.model), "\tNum of NAs:\t", length(pnls.model[2][is.na(pnls.model[2])]), "\n"))

### Run meta-trading rules
# Calculate meta-trading pnls
  pnls.out <- rowSums(pnls.model*lag(perf.scores))

# Format PnL time series
  pnls.out <- xts(pnls.out, order.by=index(model$prices))
  pnls.out[1,] <- 0.0
  names.model <- paste(model$name, paste(model$signal.list, collapse="/"), paste("time.window:", time.window), sep="/")
  colnames(pnls.out) <- names.model
  pnls.out

#    cbind(ts.pnls, ts.perf.scores)

}
### End metaPortfolioTrader.alphaModel


### Function adaptivePortfolioTrader trades a portfolio of single-name credits, using an Adaptive Asset Allocation framework.
adaptivePortfolioTrader <- function(ts.prices, look.back=NULL, func.signal=NULL, cut.off=10, lag.trade=TRUE) {
# The strategy holds a portfolio of winning symbols equal to cut.off
# The strategy holds a portfolio of losing symbols equal to cut.off
### Calculate rate of change for a list of symbols
# Apply simpole diff to calculate rate of change
  if (!is.null(look.back))
    {
      ts.roc <- diff(ts.prices,lag=look.back)/lag(ts.prices,look.back)
      ts.roc[1:look.back,] <- ts.roc[look.back+1,]
    }

# Apply SavGol filter to calculate rate of change
  if (!is.null(func.signal))
    {
      func.name <- match.fun(func.signal$filter.func)
      ts.roc <- sapply(1:length(ts.prices[1,]),
                         function(col.num) do.call(func.name, append(list(coredata(ts.prices[,col.num])), func.signal$filter.params))/coredata(ts.prices[,col.num]))
      ts.roc <- xts(ts.roc, order.by=index(ts.prices))
      colnames(ts.roc) <- colnames(ts.prices)
      ts.roc[1,1] <- 0.0
      ts.roc <- na.locf(ts.roc)
    }

# Sort symbols by lowest to highest rate of change
  roc.order <- t(sapply(1:length(ts.roc[,1]), function(row.num) colnames(ts.roc)[order(coredata(ts.roc[row.num,]))]))
  roc.order <- xts(roc.order,order.by=index(ts.roc))
### Simulate trend-following strategy for a list of symbols
# Every period update the winner and loser portfolios by selling top losers and buying top winners from previous day
  top.losers <- 1:cut.off
  loser.symbols <- roc.order[,top.losers]
  top.winners <- (length(roc.order[1,])-cut.off+1):length(roc.order[1,])
  winner.symbols <- roc.order[,top.winners]
  if (lag.trade)
    {
      loser.symbols <- lag(loser.symbols)
      loser.symbols[1,] <- loser.symbols[2,]
      winner.symbols <- lag(winner.symbols)
      winner.symbols[1,] <- winner.symbols[2,]
    }
# Calculate pnls per symbol per period
  ts.rets <- diff(ts.prices)
  ts.rets[1,] <- ts.rets[2,]
  winner.pnls <- t(sapply(1:length(ts.roc[,1]), function(row.num) ts.rets[row.num,winner.symbols[row.num,]]))
  winner.pnls <- xts(winner.pnls,order.by=index(ts.rets))
  loser.pnls <- t(sapply(1:length(ts.roc[,1]), function(row.num) ts.rets[row.num,loser.symbols[row.num,]]))
  loser.pnls <- xts(loser.pnls,order.by=index(ts.rets))
# Calculate total pnls per period
  winner.pnl <- xts(rowSums(winner.pnls),order.by=index(winner.pnls))
  colnames(winner.pnl) <- "Winner.pnl"
  loser.pnl <- xts(rowSums(loser.pnls),order.by=index(loser.pnls))
  colnames(loser.pnl) <- "Loser.pnl"
# chart_Series(cumsum(winner.pnl-loser.pnl), name="Winners - Losers")
# Calculate transaction costs based on symbol turnover per period
# Calculate turnover of winner trades per period
  winner.turnover <- cut.off-c(0,sapply(2:length(ts.rets[,1]), function(row.num) length(intersect(winner.symbols[row.num-1,],winner.symbols[row.num,])) ))
  winner.turnover <- xts(winner.turnover,order.by=index(ts.rets))
# Calculate trading costs assuming full BO is 5% of spread (a single trade costs 2.5% of spread)
  winner.spreads <- t(sapply(1:length(ts.prices[,1]), function(row.num) ts.prices[row.num,winner.symbols[row.num,]]))
  winner.spreads <- xts(rowMeans(winner.spreads),order.by=index(ts.prices))
  win.costs <- 0.015*winner.spreads*winner.turnover
  colnames(win.costs) <- "Winner.trans.costs"
# plot(winner.turnover, type='l')
  loser.turnover <- cut.off-c(0,sapply(2:length(ts.rets[,1]), function(row.num) length(intersect(loser.symbols[row.num-1,],loser.symbols[row.num,])) ))
  loser.turnover <- xts(loser.turnover,order.by=index(ts.rets))
  loser.spreads <- t(sapply(1:length(ts.prices[,1]), function(row.num) ts.prices[row.num,loser.symbols[row.num,]]))
  loser.spreads <- xts(rowMeans(loser.spreads),order.by=index(ts.prices))
  lose.costs <- 0.015*loser.spreads*loser.turnover
  colnames(lose.costs) <- "Loser.trans.costs"
  total.pnl <- winner.pnl-loser.pnl-win.costs-lose.costs
  colnames(total.pnl) <- "Total.pnl"
# Format output
  list(sums=c(look.back=look.back, cut.off=cut.off, total.pnl=sum(total.pnl), winner.pnl=sum(winner.pnl), loser.pnl=sum(-loser.pnl), win.costs=sum(win.costs), lose.costs=sum(lose.costs)),
       time.series=cbind(total.pnl, winner.pnl, -loser.pnl, win.costs, lose.costs)
       )
}
### End adaptivePortfolioTrader


##########################
### Testing functions  ###
##########################

# test parsing trading.rules list
test.alphaModel <- function(model) {

  stopifnot(inherits(model, "alphaModel"))

# Apply trading rules
  func.name <- match.fun(model$rules.list$rules.func)
  func.name(model, model$rules.list$rules.params)

}
# End test.alphaModel

echo.alphaModel <- function(model, params) {
  stopifnot(inherits(model, "alphaModel"))
  cat(params)
}

