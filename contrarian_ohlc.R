###############
### Contrarian strategies using OHLC technical indicators

# The intuition is that if there are several consecutive 
# gains or losses, and if also the prices close at the 
# highs or lows, then it's best to reverse the strong trend.

library(HighFreq)

# The function HighFreq::roll_count() counts the number of 
# consecutive TRUE elements in a Boolean vector.
# Or compile Rcpp functions (old):
# Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/lm_arma.cpp")

## Load the 5-second ES futures bar data collected from IB.
data_dir <- "C:/Develop/data/ib_data/"
symbol <- "ES"  # S&P500
# symbol <- "QM"  # oil
load(paste0(data_dir, symbol, "_ohlc.RData"))
nrows <- NROW(ohlc)
indeks <- index(ohlc)
endpoints <- xts::endpoints(ohlc, on="hours")


## Load VTI bar data
ohlc <- rutils::etfenv$VTI
# openp <- quantmod::Op(ohlc)
# closep <- quantmod::Cl(ohlc)
# volumes <- quantmod::Vo(ohlc)
# startd <- as.numeric(closep[1])


## Calculate OHLC returns.
core_data <- zoo::coredata(ohlc)
openp <- log(core_data[, 1])
highp <- log(core_data[, 2])
lowp <- log(core_data[, 3])
closep <- log(core_data[, 4])
volumes <- core_data[, 5]
close_close <- rutils::diffit(closep)
returns_lag <- rutils::lagit(close_close, lagg=1)


## Calculate technical indicators.
open_close <- (closep - openp)
close_open <- (openp - rutils::lagit(closep, lagg=1, pad_zeros=FALSE))


# Select only those bars where there was a change in prices
high_low <- (highp > lowp)
returns_pos_count <- drop(HighFreq::roll_count(close_close > 0))
returns_neg_count <- drop(HighFreq::roll_count(close_close < 0))
open_high <- (openp == highp) & high_low
open_high_count <- drop(HighFreq::roll_count(open_high))
open_low <- (openp == lowp) & high_low
open_low_count <- drop(HighFreq::roll_count(open_low))
close_high <- (closep == highp) & high_low
close_high_count <- drop(HighFreq::roll_count(close_high))
close_low <- (closep == lowp) & high_low
close_low_count <- drop(HighFreq::roll_count(close_low))


## Analysis
x11(width=6, height=5)
# Calculate partial autocorrelations
pacf(close_close)

plot(cumsum(close_open), ylab="cum returns", t="l", main="close_open")
plot(cumsum(open_close), ylab="cum returns", t="l", main="open_close")
plot(cumsum(close_open - open_close), ylab="cum returns", t="l", main="open_close")



## Backtest strategies based on number of consecutive positive and negative returns
threshold_s <- 1:4
cum_pnls <- sapply(threshold_s, function(threshold) {
  cat("threshold=", threshold, "\n")
  # Initialize positions
  posit <- rep(NA_integer_, nrows)
  posit[1] <- 0
  # Flip position if several consecutive positive or negative returns
  posit[returns_pos_count > threshold] <- (-1)
  posit[returns_neg_count > threshold] <- 1
  # Flip position if several consecutive closes at high or low
  posit[close_high_count > threshold] <- (-1)
  posit[close_low_count > threshold] <- 1
  # LOCF
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- rutils::lagit(posit, lagg=1)
  cumsum(posit*close_close)
})  # end sapply

# Plot in panels
colnames(cum_pnls) <- paste0("thresh=", threshold_s)
x11()
plot.zoo(cum_pnls)


## Backtest strategy for flipping if two consecutive positive and negative returns
rm(datav)
# Initialize positions
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
# Flip position if several consecutive positive or negative returns
posit[returns_pos_count > 1] <- (-1)
posit[returns_neg_count > 1] <- 1
# Flip position if several consecutive closes at high or low
posit[close_high_count > 1] <- (-1)
posit[close_low_count > 1] <- 1
# LOCF
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit, lagg=1)
# Calculate number of trades
sum(abs(rutils::diffit(posit))) / NROW(posit) / 2
# Calculate strategy pnls
pnls <- cumsum(posit*close_close)
# tail(pnls)


## Backtest strategy for flipping if single close at the high or low
rm(datav)
# Initialize positions
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
# Flip position if close at high or low
posit[close_high] <- (-1)
posit[close_low] <- 1
# LOCF
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit, lagg=1)
# Calculate number of trades
sum(abs(rutils::diffit(posit))) / NROW(posit) / 2
# Calculate strategy pnls
pnls <- cumsum(posit*close_close)
# tail(pnls)


## Backtest strategy for flipping if returns scaled by the price range exceed threshold
threshold <- 0.9
lagg <- 2
rm(datav)
# Scale returns using price range
rangev <- (log(core_data[, 2]) - log(core_data[, 3]))
close_close <- rutils::diffit(log(closep))
returns_norm <- ifelse(rangev>0, close_close/rangev, 0)
# Initialize positions
posit <- rep(NA_integer_, nrows)
posit[1] <- 0
# Flip position if the scaled returns exceed threshold 
posit[returns_norm > threshold] <- (-1)
posit[returns_norm < (-threshold)] <- 1
# LOCF
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit, lagg=lagg)
# Calculate number of trades
sum(abs(rutils::diffit(posit))) / NROW(posit) / 2
# Calculate strategy pnls
pnls <- cumsum(posit*close_close)
# tail(pnls)


# Plot pnls
plot(pnls, t="l")


## Coerce pnls to xts
pnls <- xts(pnls, indeks)
datav <- cbind(pnls[endpoints], closep[endpoints])
colnamev <- c("Strategy", symbol)
colnames(datav) <- colnamev


## Plot using zoo with two y-axes
zoo::plot.zoo(datav[, 1], lwd=3, col="orange", xlab=NA, ylab=NA, xaxt="n")
# Create X-axis date labels
index_pretty <- pretty(index(datav))
# Add X-axis
axis(side=1, at=index_pretty, labels=format(index_pretty, "%b-%d-%y"))
# Plot second time series without y-axis
par(new=TRUE)  # Allow new plot on same chart
zoo::plot.zoo(datav[, 2], xlab=NA, ylab=NA,
              lwd=3, yaxt="n", col="blue", xaxt="n")
# Plot second y-axis on right
axis(side=4, lwd=2, col="blue")
# Add axis labels
mtext(colnamev[1], cex=1.5, lwd=3, side=2, las=2, adj=(-0.5), padj=(-10), col="orange")
mtext(colnamev[2], cex=1.5, lwd=3, side=4, las=2, adj=1.5, padj=(-10), col="blue")
# Add title and legend
title(main=paste0("Contrarian Strategy for ", symbol, " Using OHLC Technical Indicators"),
      line=0.5)
legend("top", legend=colnamev, cex=1.5, 
       bg="white", lty=1, lwd=6,
       col=c("orange", "blue"), bty="n")


## Plot dygraph with two y-axes
library(dygraphs)
dygraphs::dygraph(datav, main=paste0("Contrarian Strategy for ", symbol, " Using OHLC Technical Indicators")) %>%
  dyAxis(name="y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis(name="y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", strokeWidth=3, col="orange") %>%
  dySeries(name=colnamev[2], axis="y2", strokeWidth=3, col="blue")


## Doesn't plot with two y-axes: Plot chart_Series pnls with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(datav, theme=plot_theme,
             name=paste0("Contrarian Strategy for ", symbol, " Using OHLC Technical Indicators"))
legend("bottomright", legend=colnames(agg_regations),
       bg="white", lty=c(1, 1), lwd=c(2, 2),
       col=plot_theme$col$line.col, bty="n")

