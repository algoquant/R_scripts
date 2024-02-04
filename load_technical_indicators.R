###############
### Calculate matrix of OHLC technical indicators

library(rutils)

# Define OHLC data
ohlc <- log(rutils::etfenv$VTI)
openp <- Op(ohlc)
highp <- Hi(ohlc)
lowp <- Lo(ohlc)
closep <- Cl(ohlc)
variance <- (highp - lowp)^2
colnames(variance) <- "variance"
volat <- sqrt(variance)
colnames(volat) <- "volat"
volumes <- Vo(ohlc)
colnames(volumes) <- "volume"

# Define current and future returns
returns <- rutils::diffit(closep)
colnames(returns) <- "returns"
# returns_adv <- rutils::lagit(returns, lagg=-1)
# or
# returns_adv <- 0.5*(returns_adv + rutils::lagit(returns_adv, lagg=-1))
lookb <- 2
returns_adv <- rutils::lagit(HighFreq::roll_sum(returns, lookb=lookb), lagg=-lookb)/lookb
returns_adv <- xts(returns_adv, index(ohlc))
colnames(returns_adv) <- "returns_adv"
# scale returns using sigmoid
# returns_adv <- plogis(returns_adv, scale=-quantile(returns_adv, 0.01))
# returns_adv <- (returns_adv - median(returns_adv))
# colnames(returns_adv) <- "returns_adv"

# Define OHLC technical indicators
# residuals of the regression of the time series of closep prices
dates <- xts::.index(ohlc)
lookb <- 11
zscores <- HighFreq::roll_zscores(response=closep, 
                                   design=matrix(as.numeric(dates), nc=1), 
                                   lookb=lookb)
colnames(zscores) <- "zscores"
zscores[1:3] <- 0
close_open <- (closep-openp)
colnames(close_open) <- "close_open"
close_high <- (closep-highp)
colnames(close_high) <- "close_high"
close_low <- (closep-lowp)
colnames(close_low) <- "close_low"
# skew <- ((highp+lowp) - (openp+closep))
skew <- ((highp+lowp) - (openp+closep))
colnames(skew) <- "skew"
# moment_um <- ((closep-openp) - (highp-lowp))
moment_um <- ((closep-openp) - (highp-lowp)) + 1.0
colnames(moment_um) <- "moment_um"

# close_high <- (highp - rutils::lagit(highp))
# close_low <- (lowp - rutils::lagit(lowp))
# Select only independent indicators

indicator_s <- cbind(returns, volat, skew)
# scale indicator_s using roll_scale()
lookb <- 60
indicator_s <- roll::roll_scale(indicator_s, width=lookb, min_obs=1)
indicator_s[1, ] <- 0
indicator_s <- cbind(indicator_s, zscores)
indicator_s[1:3, ] <- 0
colnamev <- colnames(indicator_s)

