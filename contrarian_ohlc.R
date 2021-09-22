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
sym_bol <- "ES"  # S&P500
# sym_bol <- "QM"  # oil
load(paste0(data_dir, sym_bol, "_ohlc.RData"))
n_rows <- NROW(oh_lc)
in_dex <- index(oh_lc)
end_points <- xts::endpoints(oh_lc, on="hours")


## Load VTI bar data
oh_lc <- rutils::etf_env$VTI
# op_en <- quantmod::Op(oh_lc)
# clos_e <- quantmod::Cl(oh_lc)
# vol_ume <- quantmod::Vo(oh_lc)
# star_t <- as.numeric(clos_e[1])


## Calculate OHLC returns.
core_data <- zoo::coredata(oh_lc)
op_en <- log(core_data[, 1])
hi_gh <- log(core_data[, 2])
lo_w <- log(core_data[, 3])
clos_e <- log(core_data[, 4])
vol_ume <- core_data[, 5]
close_close <- rutils::diff_it(clos_e)
returns_lag <- rutils::lag_it(close_close, lagg=1)


## Calculate technical indicators.
open_close <- (clos_e - op_en)
close_open <- (op_en - rutils::lag_it(clos_e, lagg=1, pad_zeros=FALSE))


# Select only those bars where there was a change in prices
high_low <- (hi_gh > lo_w)
returns_pos_count <- drop(HighFreq::roll_count(close_close > 0))
returns_neg_count <- drop(HighFreq::roll_count(close_close < 0))
open_high <- (op_en == hi_gh) & high_low
open_high_count <- drop(HighFreq::roll_count(open_high))
open_low <- (op_en == lo_w) & high_low
open_low_count <- drop(HighFreq::roll_count(open_low))
close_high <- (clos_e == hi_gh) & high_low
close_high_count <- drop(HighFreq::roll_count(close_high))
close_low <- (clos_e == lo_w) & high_low
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
cum_pnls <- sapply(threshold_s, function(thresh_old) {
  cat("thresh_old=", thresh_old, "\n")
  # Initialize positions
  position_s <- rep(NA_integer_, n_rows)
  position_s[1] <- 0
  # Flip position if several consecutive positive or negative returns
  position_s[returns_pos_count > thresh_old] <- (-1)
  position_s[returns_neg_count > thresh_old] <- 1
  # Flip position if several consecutive closes at high or low
  position_s[close_high_count > thresh_old] <- (-1)
  position_s[close_low_count > thresh_old] <- 1
  # LOCF
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s <- rutils::lag_it(position_s, lagg=1)
  cumsum(position_s*close_close)
})  # end sapply

# Plot in panels
colnames(cum_pnls) <- paste0("thresh=", threshold_s)
x11()
plot.zoo(cum_pnls)


## Backtest strategy for flipping if two consecutive positive and negative returns
rm(da_ta)
# Initialize positions
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
# Flip position if several consecutive positive or negative returns
position_s[returns_pos_count > 1] <- (-1)
position_s[returns_neg_count > 1] <- 1
# Flip position if several consecutive closes at high or low
position_s[close_high_count > 1] <- (-1)
position_s[close_low_count > 1] <- 1
# LOCF
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- rutils::lag_it(position_s, lagg=1)
# Calculate number of trades
sum(abs(rutils::diff_it(position_s))) / NROW(position_s) / 2
# Calculate strategy pnl_s
pnl_s <- cumsum(position_s*close_close)
# tail(pnl_s)


## Backtest strategy for flipping if single close at the high or low
rm(da_ta)
# Initialize positions
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
# Flip position if close at high or low
position_s[close_high] <- (-1)
position_s[close_low] <- 1
# LOCF
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- rutils::lag_it(position_s, lagg=1)
# Calculate number of trades
sum(abs(rutils::diff_it(position_s))) / NROW(position_s) / 2
# Calculate strategy pnl_s
pnl_s <- cumsum(position_s*close_close)
# tail(pnl_s)


## Backtest strategy for flipping if returns scaled by the price range exceed thresh_old
thresh_old <- 0.9
lagg <- 2
rm(da_ta)
# Scale returns using price range
rang_e <- (log(core_data[, 2]) - log(core_data[, 3]))
close_close <- rutils::diff_it(log(clos_e))
returns_norm <- ifelse(rang_e>0, close_close/rang_e, 0)
# Initialize positions
position_s <- rep(NA_integer_, n_rows)
position_s[1] <- 0
# Flip position if the scaled returns exceed thresh_old 
position_s[returns_norm > thresh_old] <- (-1)
position_s[returns_norm < (-thresh_old)] <- 1
# LOCF
position_s <- zoo::na.locf(position_s, na.rm=FALSE)
position_s <- rutils::lag_it(position_s, lagg=lagg)
# Calculate number of trades
sum(abs(rutils::diff_it(position_s))) / NROW(position_s) / 2
# Calculate strategy pnl_s
pnl_s <- cumsum(position_s*close_close)
# tail(pnl_s)


# Plot pnl_s
plot(pnl_s, t="l")


## Coerce pnl_s to xts
pnl_s <- xts(pnl_s, in_dex)
da_ta <- cbind(pnl_s[end_points], clos_e[end_points])
col_names <- c("Strategy", sym_bol)
colnames(da_ta) <- col_names


## Plot using zoo with two y-axes
zoo::plot.zoo(da_ta[, 1], lwd=3, col="orange", xlab=NA, ylab=NA, xaxt="n")
# Create X-axis date labels
index_pretty <- pretty(index(da_ta))
# Add X-axis
axis(side=1, at=index_pretty, labels=format(index_pretty, "%b-%d-%y"))
# Plot second time series without y-axis
par(new=TRUE)  # Allow new plot on same chart
zoo::plot.zoo(da_ta[, 2], xlab=NA, ylab=NA,
              lwd=3, yaxt="n", col="blue", xaxt="n")
# Plot second y-axis on right
axis(side=4, lwd=2, col="blue")
# Add axis labels
mtext(col_names[1], cex=1.5, lwd=3, side=2, las=2, adj=(-0.5), padj=(-10), col="orange")
mtext(col_names[2], cex=1.5, lwd=3, side=4, las=2, adj=1.5, padj=(-10), col="blue")
# Add title and legend
title(main=paste0("Contrarian Strategy for ", sym_bol, " Using OHLC Technical Indicators"),
      line=0.5)
legend("top", legend=col_names, cex=1.5, 
       bg="white", lty=1, lwd=6,
       col=c("orange", "blue"), bty="n")


## Plot dygraph with two y-axes
library(dygraphs)
dygraphs::dygraph(da_ta, main=paste0("Contrarian Strategy for ", sym_bol, " Using OHLC Technical Indicators")) %>%
  dyAxis(name="y", label=col_names[1], independentTicks=TRUE) %>%
  dyAxis(name="y2", label=col_names[2], independentTicks=TRUE) %>%
  dySeries(name=col_names[1], axis="y", strokeWidth=3, col="orange") %>%
  dySeries(name=col_names[2], axis="y2", strokeWidth=3, col="blue")


## Doesn't plot with two y-axes: Plot chart_Series pnl_s with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(da_ta, theme=plot_theme,
             name=paste0("Contrarian Strategy for ", sym_bol, " Using OHLC Technical Indicators"))
legend("bottomright", legend=colnames(agg_regations),
       bg="white", lty=c(1, 1), lwd=c(2, 2),
       col=plot_theme$col$line.col, bty="n")

