##################################
### Time Series Data Plotting  ###
##################################

# Plot xts object using chart_Series, and add vertical lines
#' @export
plotSeries <- function(ts.data, name.plot, ts.indeks=NULL) {

  stopifnot(inherits(ts.data, "xts"))
  theme <- chart_theme()
#  if(name=='Signals') theme$col$line.col = 'red' 

  invisible(chart_Series(ts.data, name=name.plot))
  if (!is.null(ts.indeks))
    {
      invisible(add_TA(ts.indeks$positions>0, on=-1, col="lightgreen", border=NA))
      invisible(add_TA(ts.indeks$positions<0, on=-1, col="lightgrey", border=NA))
      invisible(add_TA(ts.indeks$stop.loss>0, on=-1, col="red", border=NA))
    }
# Plot the charts and suppress warnings
  suppressWarnings(.chob)

}
# End plotSeries


### Plot an xts with multiple time series
#' @export
chart.xts <- function(ts.prices) {
  par(mfrow=c(dim(ts.prices)[2],1))
  sapply(1:(dim(ts.prices)[2]), function(n.col) plotSeries(ts.prices[,n.col], name.plot=paste(colnames(ts.prices[,n.col]), "/", date())))
# Plot the charts and suppress warnings
#  suppressWarnings(.chob)
}
### End chart.xts



### Plot a price time series and its PACF
chart.PACF <- function(ts.price, theme=NULL) {
  if (is.null(theme))
    {
      theme <- chart_theme()
      theme$col$up.col <- 'lightgreen'
      theme$col$up.border <- 'lightgreen'
      theme$col$dn.col <- 'pink'
      theme$col$dn.border <- 'pink'
    }
  par(mfrow = c(2,1))
#  plot.xts(ts.price, main=paste("Price ", symbolCDS), major.format="%b %y")
  plot(chart_Series(ts.price, theme=theme, name=paste(colnames(ts.price), "/", date())))
  pacf(na.locf(diff(ts.price)), lag=30, xlab="Lag ticks", main=paste("PACF ", colnames(ts.price), "/", date()))
}
# End chart.PACF


### Plot a price time series and its SavGol estimates using plot.xts
plot.SavGol <- function(ts.price, func.signal) {

# Extract function name
  func.name <- match.fun(func.signal$filter.func)

# Apply function and calculate signal (first derivative)
  func.signal$filter.params[4] <- 1
  ts.signal <- xts(do.call(func.name, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
  ts.signal[1,1] <- 0.0
  ts.signal <- na.locf(ts.signal)
# Set margins
  par(mar=c(5,4,4,5)+.1)
# Plot prices
  plot.xts(ts.price, main=paste(colnames(ts.price), "/", date(), "\nSignal function:", paste(func.signal$filter.func, paste(func.signal$filter.params, collapse="/"))))
# Add left y-axis title
  mtext("Price", side=2, line=2)
# Set par new=TRUE to plot over existing plot
  par(new=TRUE)
# Plot first derivative over existing plot
  tryCatch(plot.xts(x=ts.signal, lwd=2, col="red"), message=function(m) m, error=function(e) e)
# Add right y-axis and title
  axis(4)
  mtext("SavGol", side=4, line=2)

# Plot second derivative
  func.signal$filter.params[4] <- 2
# Apply function and calculate signal
  ts.signal <- xts(do.call(func.name, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
  ts.signal[1,1] <- 0.0
  ts.signal <- na.locf(ts.signal)
# Set par to plot over existing plot, and plot signal
  par(new=TRUE)
  tryCatch(plot.xts(x=ts.signal, lwd=2, col="green"), message=function(m) m, error=function(e) e)

# Add legend
  PerformanceAnalytics::legend("topleft", lty=1, col=c("black","red","green"), pt.lwd=2, legend=c(colnames(ts.price), paste(func.signal$filter.func, "First derivative"), paste(func.signal$filter.func, "Second derivative") ) )

}
### End plot.SavGol


### Plot a price time series and its SavGol estimates using plotSeries
#' @export
chart.SavGol <- function(ts.prices, func.signal, time.range=NULL, n.flips.max=150) {

### Calculate signals
# Parse the func.signal list
# The first element of func.signal is the func.signal name
  func.name <- match.fun(func.signal$filter.func)
# The remaining elements of func.signal are the signal function parameters
# Apply the func.signal to the ts.prices
  signal.list <- func.signal$filter.params
  signal.list[3] <- 0
  signal.list[4] <- 0
  mean.SavGol <- xts(
                     do.call(
                             func.name,
                             append(list(coredata(ts.prices)), signal.list)
                             ),
                     order.by=index(ts.prices)
                     )
  mean.SavGol[1,] <- na.omit(mean.SavGol)[1,]
  mean.SavGol <- na.locf(mean.SavGol)
  colnames(mean.SavGol) <- "Mean"

  signal.list[4] <- 1
  slope.SavGol <- xts(
                      do.call(
                              func.name,
                              append(list(coredata(ts.prices)), signal.list)
                              ),
                      order.by=index(ts.prices)
                      )
  slope.SavGol[1,] <- na.omit(slope.SavGol)[1,]
  slope.SavGol <- na.locf(slope.SavGol)
  colnames(slope.SavGol) <- "Slope"

  signal.list[4] <- 2
  convexity.SavGol <- xts(
                          do.call(
                                  func.name,
                                  append(list(coredata(ts.prices)), signal.list)
                                  ),
                          order.by=index(ts.prices)
                          )
  convexity.SavGol[1,] <- na.omit(convexity.SavGol)[1,]
  convexity.SavGol <- na.locf(convexity.SavGol)
  colnames(convexity.SavGol) <- "Convexity"

  chart.data <- cbind(ts.prices, mean.SavGol, slope.SavGol, convexity.SavGol)
  if (!is.null(time.range))
    chart.data <- chart.data[time.range]

# Calculate the number of times positions change sign
#  positions <- diff(mean.SavGol)
#  positions[1] <- 0
  positions <- sign(chart.data[,'Slope'])
  n.flips <- sum(abs(na.omit(diff(positions))))/2
# Shade the plot and show Signals and Positions only if the number of sign flips is small
  if (n.flips<n.flips.max)
    ts.indeks <- list(positions=positions, stop.loss=0*positions)
  else
    ts.indeks <- NULL

  par(mfrow=c(dim(chart.data)[2],1))
  sapply(1:(dim(chart.data)[2]), function(n.col) plotSeries(chart.data[,n.col], name.plot=paste(colnames(chart.data[,n.col]), "/", date()), ts.indeks=ts.indeks))
#  add_TA(model$positions>0, on=-1, col="lightgreen")
#  add_TA(model$positions<0, on=-1, col="lightgrey")
#  plot(.chob)

}
### End chart.SavGol



### Plot a price time series and its SavGol estimates using plotSeries
# filters.SavGol is an array of SavGol width parameters
#' @export
chart.SavGol.filters <- function(ts.prices, func.signal, widths.SavGol, time.range=NULL) {

### Calculate SavGol estimates
  chart.data <- filters.SavGol(ts.prices=ts.prices, func.signal=func.signal, widths.SavGol=widths.SavGol, time.range=time.range)

  par(mfrow=c(dim(chart.data)[2],1))
  sapply(1:(dim(chart.data)[2]), function(n.col) plotSeries(chart.data[,n.col], name.plot=paste(colnames(chart.data[,n.col]), "/", date())))

}
### End chart.SavGol.filters


### Plot a price time series and its SavGol estimates using plot.xts
# widths.SavGol is an array of SavGol width parameters
#' @export
plot.SavGol.filters <- function(ts.prices, func.signal, widths.SavGol, time.range=NULL) {
### Calculate SavGol estimates
  chart.data <- filters.SavGol(ts.prices=ts.prices, func.signal=func.signal, widths.SavGol=widths.SavGol, time.range=time.range)

# Plot
  t.colors <- c("red","blue","green","orange","yellow")
#  par(mfrow=c(dim(chart.data)[2],1))
  par(mar=c(5,4,4,5)+.1)
  tryCatch(plot.xts(x=chart.data[,1], lwd=2, col=t.colors[1]), message=function(m) m, error=function(e) e)
  sapply(2:(dim(chart.data)[2]), function(n.col)
         {
           plot.data <- chart.data[,n.col]
           plot.data[1,] <- na.omit(plot.data)[1,]
           plot.data <- na.locf(plot.data)
           par(new=TRUE)
           tryCatch(plot.xts(x=plot.data, lwd=2, col=t.colors[n.col]), message=function(m) m, error=function(e) e)
         }
         )

}
### End plot.SavGol.filters


########################################
### alphaModel Plotting and Writing  ###
########################################

summary.alphaModel <- function(model, ...) {

  stopifnot(inherits(model, "alphaModel"))

  cat(paste(title.alphaModel(model), "\n", sep=""), ...)

}
# End summary.alphaModel


### Calculate SavGol filter estimates, and cbind them with original time series
filters.SavGol <- function(ts.prices, func.signal, widths.SavGol, lag=1, time.range=NULL) {
# Parse the func.signal list
# The first element of func.signal is the func.signal name
  func.name <- match.fun(func.signal$filter.func)
# The remaining elements of func.signal are the signal function parameters
# Apply the func.signal to the ts.prices
  estimates.SavGol <- sapply(widths.SavGol, function(filter.SavGol)
                             {
                               signal.list <- func.signal$filter.params
                               signal.list[2] <- filter.SavGol
                               mean.SavGol <- do.call(func.name, append(list(coredata(ts.prices)), signal.list))
                             }
                             )
  estimates.SavGol <- xts(estimates.SavGol, order.by=index(ts.prices))
  s.name <- switch(func.signal$filter.params[4]+1, 'Mean', 'Slope', 'Convexity')
  colnames(estimates.SavGol) <- paste(s.name, widths.SavGol)
# Fix NAs
  for (n.col in 1:(dim(estimates.SavGol)[2]))
    {
      estimates.SavGol[1,n.col] <- na.omit(estimates.SavGol[,n.col])[1]
      estimates.SavGol[,n.col] <- na.locf(estimates.SavGol[,n.col])
    }
# Lag ts.prices if SavGol filter is not mean
  if(func.signal$filter.params[4]>0)
    {
      ts.prices <- lag(diff(ts.prices,lag),-lag)
      ts.prices <- na.locf(ts.prices)
    }
  chart.data <- cbind(ts.prices, estimates.SavGol)
  if (!is.null(time.range))
    chart.data <- chart.data[time.range]
  chart.data
}
# End filters.SavGol


# Plot all the internal time series
#' @export
plot.alphaModel <- function(model, time.range=NULL, n.flips.max=150) {

  stopifnot(inherits(model, "alphaModel"))

  if (is.null(time.range))
    chart.data <- cbind(model)
#    plot.zoo(cbind(model), main=title.alphaModel(model), major.format="%b %y")
#    plot.zoo(cbind(model)[time.range], main=title.alphaModel(model), major.format="%b %y")
  else
    chart.data <- cbind(model)[time.range]

# Calculate the number of times positions change sign
  n.flips <- sum(abs(na.omit(diff(sign(chart.data[,"Positions"])))))/2
# Shade the plot and show Signals and Positions only if the number of sign flips is small
  if (n.flips<n.flips.max)
    ts.indeks <- list(positions=model$positions, stop.loss=model$times.stop.loss)
  else
    {
      ts.indeks <- NULL
      chart.data <- chart.data[,c("Prices","PnLs")]
    }
  par(mfrow=c(dim(chart.data)[2],1))

  sapply(1:(dim(chart.data)[2]), function(n.col) plotSeries(chart.data[,n.col], name.plot=paste(colnames(chart.data[,n.col]), "/", date()), ts.indeks=ts.indeks))
#  add_TA(model$positions>0, on=-1, col="lightgreen")
#  add_TA(model$positions<0, on=-1, col="lightgrey")
#  plot(.chob)

}
# End plot.alphaModel


plot.zoo.alphaModel <- function(model, time.range=NULL) {

  stopifnot(inherits(model, "alphaModel"))

  if (is.null(time.range))
    plot.zoo(cbind(model), main=title.alphaModel(model), major.format="%b %y")
  else
    plot.zoo(cbind(model)[time.range], main=title.alphaModel(model), major.format="%b %y")

}
# End plot.zoo.alphaModel


### Plot prices, positions, and PnLs in a single chart over a small time range
plotPositions.alphaModel <- function(model, time.range) {

# Set margins
  par(mar=c(5,4,4,5)+0.1)
# Plot prices
  tryCatch(plot(coredata(model$prices[time.range]), type='l', xlab="", ylab=""), error=function(e) e)
# Add left y-axis title
  mtext("Price", side=2, line=2)
# Add right y-axis and title
  axis(4)
  mtext("Positions", side=4, line=2)
# Set par new=TRUE to plot over existing plot
  par(new=TRUE)
# Plot positions over existing plot
  tryCatch(plot(coredata(model$positions[time.range]), lwd=2, col="red", type='l', xlab="", ylab=""), error=function(e) e)
# Plot PnLs over existing plot
  par(new=TRUE)
  tryCatch(plot(cumsum(coredata(model$pnls[time.range])), axes=F, lwd=2, col="green", type='l', xlab="", ylab=""), error=function(e) e)
# Add index
  mtext("Time", side=1, line=2)
# Add title
  title(main=paste(title.alphaModel(model), "\nTime range:", time.range))
# Add legend
  PerformanceAnalytics::legend("topleft", lty=1, col=c("black","red","green"), pt.lwd=2, legend=c("Price","Positions","PnLs"))

}
# End plotPositions.alphaModel



