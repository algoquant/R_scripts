############## homework
# Summary: Simulate a volatility regime switching 
# strategy for SPY.  
# Use the level of VIX to determine high and low 
# volatility regimes.
# Buy and hold SPY in the low volatility regime.
# Trade the AR strategy in the high volatility 
# regime.

# Conclusions:
# Very promising.  Performs very well.
# Achieves a Sharpe ratio of 0.84 using the range 
# volatility.
# Doesn't perform well when stock prices decline
# gradually with low volatility.


# To-do:
# Use range volatility instead of VIX.
# 
# Combine the AR strategy with buy and hold SPY 
# using the logistic function.  Fit the lambda 
# parameter by maximizing the Sharpe ratio.
# 
# Use logistic regression to determine the 
# volatility regimes.
# 
# Forecast the volatility using an ARIMA-GARCH model, 
# and use the forecasted volatility to determine the 
# regimes.
# 
# Forecast the volatility regimes using an LSTM 
# neural network.


## Run the setup code below

library(rutils)

# Calculate the SPY daily percentage returns
symboln <- "SPY"
retp <- na.omit(get(symboln, rutils::etfenv$returns))
nrows <- NROW(retp)
datev <- zoo::index(retp)

ohlc <- log(get(symboln, rutils::etfenv))
retp <- rutils::diffit(quantmod::Cl(ohlc))
colnames(retp) <- symboln
nrows <- NROW(retp)
datev <- zoo::index(retp)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
hilo <- (highp - lowp) ##  range of daily prices


# Get the VIX index prices
load(file="/Users/jerzy/Develop/data/vix/vix_data.RData")
vixohlc <- vixenv$vixohlc[datev]
datev <- zoo::index(vixohlc)
retp <- retp[datev]
vixindex <- quantmod::Cl(vixohlc)
volt <- 21 ##  Volatility threshold for detecting high and low volatility regimes
dygraph(vixindex, main="VIX Index") %>%
  dyOptions(colors="blue", strokeWidth=1)
dygraphs::dygraph(vixohlc["2019/"], main="VIX Index") %>%
  dyCandlestick()


# Calculate the high volatility AR coefficients
respv <- retp
orderp <- 5
predm <- lapply(1:orderp, rutils::lagit, input=respv)
predm <- rutils::do_call(cbind, predm)
colnames(predm) <- paste0("lag", 1:orderp)
# predm <- cbind(rep(1, NROW(predm)), predm)
# vixl <- rutils::lagit(vixindex)
# Select the high volatility regime
vixl <- rutils::lagit(hilo) # Lagged range of daily prices
vixt <- rutils::diffit(vixl) # Difference of the lagged range of daily prices
volt <- 0.012
# High volatility regime is when the range is high and the range differences are positive
selv <- ((vixl > 2.5*volt) & (vixt > 0.25*volt))
predinv <- MASS::ginv(predm[selv])
coeff <- drop(predinv %*% respv[selv])
coeff <- coeff*sign(coeff[1])/sum(coeff)
names(coeff) <- paste0("lag", 1:orderp)
barplot(coeff, main="High Volatility AR Coefficients", 
        col="grey", xlab="", ylab="coefficient")

# Calculate the in-sample forecasts of VTI (fitted values)
fcasts <- (predm %*% coeff)/sd(respv)/2
# Calculate the AR strategy PnLs
pnlar <- retp*fcasts
# Scale the PnL volatility to that of VTI
# pnlar <- pnlar*0.02/sd(pnlar[selv])

# Combine the buy and hold SPY strategy with the AR strategy
pnls <- rbind(pnlar[selv], retp[!selv])
colnames(pnls) <- "Strategy"

# Combine using the logistic function
lambdaf <- 400
logisv <- 1/(1 + exp(-lambdaf*(vixl - volt)))
dygraph(logisv)
pnls <- (logisv*pnlar + (1-logisv)*retp)
colnames(pnls) <- "Strategy"

# Calculate the Sharpe and Sortino ratios
wealthv <- cbind(retp, pnls)
# colnames(wealthv) <- c(symboln, "Strategy")
sqrt(252)*sapply(wealthv, function(x) 
  c(Sharpe=mean(x)/sd(x), Sortino=mean(x)/sd(x[x<0])))
sharper <- sqrt(252)*sapply(wealthv, function(x) mean(x)/sd(x))
sharper <- round(sharper, 3)
captiont <- paste("Regime Switching Strategy, Sharpe", "\n",
                  paste(paste(names(sharper), sharper, sep="="), collapse=", "))
# Plot dygraph of the strategy
endw <- rutils::calc_endpoints(wealthv, interval="weeks")
dygraphs::dygraph(cumsum(wealthv)[endw], main=captiont) %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=300)

# Plot dygraph of the strategy with shading for the high and low volatility regimes
# Create colors for background shading
crossd <- (rutils::diffit(!selv) != 0)
shadev <- !selv[crossd]
crossd <- c(zoo::index(shadev), end(selv))
shadev <- ifelse(drop(zoo::coredata(shadev)) == 1, "lightgreen", "antiquewhite")
# Plot Optimal Dual EMA strategy
# captiont <- paste("Regime Switching Strategy, Sharpe", "\n",
#                   paste(paste(names(sharper), sharper, sep="="), collapse=", "))
dyplot <- dygraphs::dygraph(cumsum(wealthv), main=captiont) %>% 
  dyOptions(colors=c("blue", "red"), strokeWidth=1)
# Add shading to dygraph object
for (i in 1:NROW(shadev)) {
  dyplot <- dyplot %>% dyShading(from=crossd[i], to=crossd[i+1], color=shadev[i])
}  ##  end for
# Plot the dygraph object
dyplot


# Find the best volt by looping over volv
volv <- seq(20, 40, by=1)
volv <- seq(0.005, 0.02, by=0.001)
lambdav <- seq(10, 25, by=2)^2
# sharpev <- sqrt(252)*sapply(volv, function(volt) {
sharpev <- sqrt(252)*sapply(lambdav, function(lambdaf) {
  logisv <- 1/(1 + exp(-lambdaf*(vixl - volt)))
  dygraph(logisv)
  pnls <- (logisv*pnlar + (1-logisv)*retp)
  return(mean(pnls)/sd(pnls))
}) ## end sapply

sharpev <- sqrt(252)*sapply(volv, function(volt) {
  selv <- (vixl > volt)
  predinv <- MASS::ginv(predm[selv])
  coeff <- drop(predinv %*% respv[selv])
  fcasts <- (predm %*% coeff)/0.005
  pnlar <- retp*fcasts
  # pnlar <- pnlar*0.02/sd(pnlar[selv])
  pnls <- rbind(pnlar[selv], retp[!selv])
  return(mean(pnls)/sd(pnls))
}) ## end sapply
plot(volv, sharpev, type="b", main="Sharpe Ratios of AR Strategy for Different Volatility Thresholds",
     xlab="Volatility Threshold", ylab="Sharpe Ratio")
volt <- volv[which.max(sharpev)]
lambdaf <- lambdav[which.max(sharpev)]

## End of setup code


# 1. (10pts)
# Plot a scatterplot of the SPY returns versus the VIX returns, 
# and calculate the correlation between them.




