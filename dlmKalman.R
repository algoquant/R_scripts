### Kalman Filter Poly and ARIMA Models Using Package dlm
library(dlm)

### Prepare data
# Create a sine-wave price series plus noise
ts.synth <- sin(20*(1:nrow(ts.pca))/nrow(ts.pca)) + 0.05*rnorm(nrow(ts.pca)) + 2.0
ts.synth <- xts(ts.synth, order.by=index(ts.pca))
# Create a step-wise price series plus noise
ts.synth <- c(rep(0.5,times=100),rep(1.0,times=100),rep(1.5,times=100),rep(1.0,times=100),rep(0.5,times=(nrow(rets.pca)-400))) + 0.05*rnorm(nrow(ts.pca))
ts.synth <- xts(ts.synth, order.by=index(ts.pca))
colnames(ts.synth) <- 'synth.prices'
ts.data <- ts.synth
ts.data <- ts.pca[,'PC1']
ts.data <- ts.10min.ig[-(1:10),'MEDIAN.1MIN']
ts.data <- ts.1min.ig['2013-01-03/','MEDIAN']
rets.data <- diff(ts.data)
rets.data[1,] <- 0.0
# Plot
plot(ts.synth,type='l')
rets.synth <- diff(log(ts.synth))
rets.synth[1,] <- 0.0


### Apply Kalman filter in-sample to prices using polynomial dlm model
# dV=observation variance
# dW=system noise variance matrix
# The bigger the dV (observation variance) the stronger the Kalman filtering (smoothing)
# The bigger the dW (system variance) the weaker the Kalman filtering (smoothing)

# Define the Kalman filter
order.model <- 2
dlm.poly <- dlmModPoly(order=order.model, dV=10.1)
dlm.poly <- dlmModPoly(order=order.model, dV=10.1, dW=c(rep(0,order.model-1),1), C0=1e+07*diag(nrow=order.model))

# Apply the Kalman filter
filter.dlm.poly <- dlmFilter(y=coredata(ts.data), mod=dlm.poly)
ts.filter.poly <- xts(filter.dlm.poly$m[-1], order.by=index(ts.data))
colnames(ts.filter.poly) <- 'filtered.values'
index(ts.filter.poly) <- index(ts.filter.poly)

# Calculate in-sample residuals (these are different from filter.dlm.poly$m[,2])
resid.filter.poly <- ts.data-ts.filter.poly[,1]
colnames(resid.filter.poly) <- 'residuals'
# pacf(resid.filter.poly, 10)
# Standardize the residuals
std.resid <- standard.xts(resid.filter.poly, look.back=10)

# Plot
plot.zoo(cbind(std.resid,resid.filter.poly,ts.data[,1]))
plot.zoo(cbind(ts.data,ts.filter.poly)[-(1:10),])
chart.TimeSeries(cbind(ts.data,ts.filter.poly[,1])[-(1:10),], main="Prices and Kalman filtered prices", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')
chart.TimeSeries(cbind(ts.data,ts.filter.poly[,1])[190:250,], main="Prices and Kalman filtered prices", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')
pacf(ts.filter.poly[-(1:10),2], 10)
# pacf(na.omit(diff(ts.filter.poly[-(1:10),2])),10)

### Run simple trading strategy
resid.forecast <- lag(resid.filter.poly)
resid.forecast[1] <- 0.0
plot.zoo(cbind(ts.data,cumsum(rets.data*resid.forecast))[-(1:10),], main='IG resid.forecast')


# Perform in-sample filtering and run simple trading strategy
filterKalman <- function(dV) {
  dlm.poly <- dlmModPoly(order=3, dV=dV)
  filter.dlm.poly <- dlmFilter(y=ts.data, mod=dlm.poly)
  ts.filter.poly <- xts(filter.dlm.poly$m[-1,], order.by=index(ts.data))
  resid.filter.poly <- ts.data-ts.filter.poly[,1]
  resid.forecast <- lag(resid.filter.poly)
  resid.forecast[1] <- 0.0
  sum(rets.data*resid.forecast)
}
# End filterKalman

# Apply a list of variance parameters to filterKalman
apply(matrix(1:20), 1, filterKalman)


# Forecast dlm model out-of-sample
# Create extra dates, and bind them to existing data (to create NA values)
forecast.dates <- seq.Date(from=index(last(ts.data)), len=11, by='days')
forecast.dates <- forecast.dates[-1]
forecast.dates <- xts(rep(NA, times=length(forecast.dates)), order.by=forecast.dates)
forecast.data <- rbind(ts.data,forecast.dates)
# forecast.data <- forecast.data[,1]
# Filter the extended time series
forecast.data <- dlmFilter(y=forecast.data, mod=dlm.poly)
forecast.data <- as.xts(forecast.data$m[-1,])
forecast.data <- cbind(ts.data,ts.filter.poly[,1],forecast.data[index(forecast.dates),1])
colnames(forecast.data) <- c('prices','filtered','forecast')
chart.TimeSeries(forecast.data['2013-01/'], main="Kalman filtered and forecast prices", colorset=c(1,2,3), lty=c(1,2,3), ylab="", xlab="", legend.loc='topright')

# dlmForecast below doesn't appear to work as expected (produces different forecast than from dlmFilter above)
forecast.poly <- dlmForecast(filter.dlm.poly, nAhead=10)
forecast.data <- seq.Date(from=index(last(ts.data)), len=11, by='days')
forecast.data <- forecast.data[-1]
forecast.data <- xts(forecast.poly$f, order.by=forecast.data)
forecast.data <- xts(rep(NA, times=10), order.by=forecast.data)
colnames(forecast.data) <- 'forecast'
forecast.data <- dlmFilter(y=ts.data, mod=dlm.poly)


### Calculate out-of-sample residuals over sliding window
look.back <- 100
range.date <- matrix((look.back+1):nrow(rets.pca))
ts.residuals <- 0.0*(rets.pca[1:look.back,1])
colnames(ts.residuals) <- 'residuals'
forecast.poly <- apply(range.date, 1, function(n.row)
                       {
                         ts.data <- ts.pca[1:n.row,'PC1']
                         filter.dlm.poly <- dlmFilter(y=ts.data, mod=dlm.poly)
                         ts.filter.poly <- xts(filter.dlm.poly$m[-1,1], order.by=index(ts.data))
                         resid.filter.poly <- ts.data-ts.filter.poly
                         ts.residuals <<- rbind.xts(ts.residuals, tail(resid.filter.poly,1))
                       }
                       )
# End apply
plot.zoo(cbind(ts.residuals,ts.data))

chart.TimeSeries(cbind(ts.pca[,'PC1'],ts.forecast), main="Kalman out-of-sample forecast", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')
plot(ts.pca[,'PC1']-ts.forecast,type='l')


# Perform out-of-sample forecasting over sliding window
forcastKalman <- function(dV) {
dlm.poly <- dlmModPoly(order=2, dV=dV)

### Perform out-of-sample forecasting over sliding window
look.back <- 100
range.date <- matrix(look.back:(nrow(ts.data)-1))
ts.forecast <- 0.0*ts.data[1:look.back,1]
colnames(ts.forecast) <- 'forecast'
forecast.poly <- apply(range.date, 1, function(n.row)
                       {
                         input.data <- ts.data[(n.row-look.back+1):(n.row+1),1]
                         input.data[(look.back+1),] <- NA
                         forecast.data <- dlmFilter(y=input.data, mod=dlm.poly)
                         forecast.data <- as.xts(tail(forecast.data$m,1))
                         ts.forecast <<- rbind.xts(ts.forecast, forecast.data[,1])
                       }
                       )
# End apply

#  diff.forecast <- diff(ts.forecast)
#  diff.forecast[1,] <- 0.0
  sum(ts.forecast*ts.rets[,'IG'])
}
# End forcastKalman

# Apply a list of variance parameters to forcastKalman
apply(matrix(1:20), 1, forcastKalman)

chart.TimeSeries(cbind(ts.data,ts.forecast), main="Kalman out-of-sample forecast", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')
plot(ts.data-ts.forecast,type='l')

### Run simple trading strategy
diff.forecast <- diff(ts.forecast)
diff.forecast[1,] <- 0.0
plot.zoo(cbind(ts.data,cumsum(diff.forecast*rets.pca[,'PC1'])), main='Trading on Kalman forecasts')
# Compare trading on signals from forecasts and from residuals
plot.zoo(cbind(ts.data,cumsum(diff.forecast*rets.pca[,'PC1']),cumsum(lag.residuals*rets.pca[,'PC1'])), main='Trading on signals from forecasts and residuals')
# Compare trading on signals from KF and VAR Models
plot.zoo(cbind(ts.data,cumsum(diff.forecast*rets.pca[,'IG']),cumsum(ts.positions*rets.pca[,'IG'])), main='Trading on forecasts from KF and VAR Models')


### Fit returns into standard ARIMA model
arima.synth <- auto.arima(x=as.vector(rets.synth))
arima.synth <- Arima(x=as.vector(rets.synth), order=c(2,0,2))
summary(arima.synth)
# Plot fitted values
fitted.synth <- xts(fitted(arima.synth), order.by=index(ts.pca))
colnames(fitted.synth) <- 'fitted.synth'
chart.TimeSeries(cumsum(cbind(rets.synth,fitted.synth)), main="Sine-wave prices and fitted ARIMA model", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')
# Forecast next few PC1 ticks
forecast.synth <- forecast(arima.synth)
plot(forecast.synth)


### Build dlm ARIMA model
# sigma2=ARIMA innovations variance
# dV=observation variance
# For synthetic prices
build.arima <- function(var.param)  return( dlmModARMA(ar=as.vector(var.param[1:2]), ma=as.vector(var.param[3:4]), sigma2=0.1, dV=500.1) )
init.param <- arima.synth$coef[1:4]
fit.arima.synth <- dlmMLE(y=rets.synth, parm=init.param, build=build.arima, hessian=T)
dlm.arima.synth <- build.arima(var.param=fit.arima.synth$par)
filter.arima.synth <- dlmFilter(y=rets.synth, mod=dlm.arima.synth)
filter.arima.synth$m <- xts(filter.arima.synth$m[-1,], order.by=index(ts.pca))
colnames(filter.arima.synth$m) <- c('filtered.values','residuals1','residuals2')
plot.zoo(cumsum(cbind(rets.synth,nrow(filter.arima.synth$m)*filter.arima.synth$m[,1])[-10:0]))
plot.zoo(cumsum(filter.arima.synth$m[-(1:10),]))
chart.TimeSeries(cumsum(cbind(rets.synth,filter.arima.synth$m[,1])[-10:0]), main="Sine-wave prices and fitted ARIMA model", colorset=c(1,2), lty=c(1,2), ylab="", xlab="", legend.loc='topright')


# Build dlm ARIMA model for IG and PC1 returns
arima.pc1 <- dlmModARMA(ar=as.vector(c(0.2,0.01)), sigma2=0.1, dV=10.1)
filter.arima.pc1 <- dlmFilter(y=rets.pca[,'PC1'], mod=arima.pc1)
ts.filter.arima.pc1 <- xts(filter.arima.pc1$m[-1,], order.by=index(rets.pca))
colnames(ts.filter.arima.pc1) <- paste(c('filter','smooth'),colnames(rets.pca[,'PC1']),sep='.')
# Ignore the first few filtered returns
ts.filter.arima.pc1[1:5,] <- 0.0
# Cbind PC1 with filtered returns
ts.filter.arima.pc1 <- cbind(rets.pca[,'PC1'],ts.filter.arima.pc1)
# plot.zoo(cumsum(ts.filter.arima.pc1))


### Perform regressions
# Regress PC1 versus filtered returns
formula.lm <- PC1 ~ filter.PC1 + smooth.PC1
lm.pc1 <- lm(formula.lm, data=ts.filter.arima.pc1)
# Regress PC1 versus filtered cumulative returns
formula.lm <- PC1 ~ filter.PC1
lm.pc1 <- lm(formula.lm, data=cumsum(ts.filter.arima.pc1))
# summary(lm.pc1)
# Copy fitted values
ts.filter.arima.pc1[,'filter.PC1'] <- as.xts(lm.pc1$fitted.values)
# ts.filter.arima.pc1[,'filter.PC1'] <- lm.pc1$coefficients[2]*ts.filter.arima.pc1[,'filter.PC1']
# ts.filter.arima.pc1[1,'filter.PC1'] <- lm.pc1$coefficients[1]+ts.filter.arima.pc1[1,'filter.PC1']
formula.lm <- PC1 ~ smooth.PC1
lm.pc1 <- lm(formula.lm, data=cumsum(ts.filter.arima.pc1))
ts.filter.arima.pc1[,'smooth.PC1'] <- as.xts(lm.pc1$fitted.values)
ts.filter.arima.pc1[,'PC1'] <- cumsum(ts.filter.arima.pc1[,'PC1'])
# ts.filter.arima.pc1[,'smooth.PC1'] <- lm.pc1$coefficients[2]*ts.filter.arima.pc1[,'smooth.PC1']
# ts.filter.arima.pc1[1,'smooth.PC1'] <- lm.pc1$coefficients[1]+ts.filter.arima.pc1[1,'smooth.PC1']


vec.lags <- c(1,3,5)
formula.lm <- as.formula(paste(colnames(ts.filter.arima.pc1)[1], " ~ ", paste(paste('lag.xts(', colnames(ts.filter.arima.pc1)[2], ',', vec.lags, ')', sep=""), collapse=" + "), sep=""))
lm.pc1 <- lm(formula.lm, data=ts.filter.arima.pc1)


# Plot
chart.TimeSeries(ts.filter.arima.pc1, main="PC1 Kalman filter", colorset=c(1,2,3), lty=c(1,1,1),ylab="",xlab="", legend.loc='topright')
chart.TimeSeries(ts.filter.arima.pc1["2012-06/"], main="PC1 Kalman filter", colorset=c(1,2,3), lty=c(1,1,1),ylab="",xlab="", legend.loc='topright')

resid.arima.pc1 <- ts.filter.arima.pc1[,1]-ts.filter.arima.pc1[,2]
plot(cumsum(cbind(ts.filter.arima.pc1[,1],resid.arima.pc1)))


# update()
# predict()
# rollapplyr()


### Apply Kalman filter for a range of ARIMA parameters
var.params <- 0.2*(1:4)
filter.params <- sapply(var.params, function(var.param)
                       {
                         arima.pc1 <- dlmModARMA(ar=as.vector(c(var.param,0.01)), sigma2=0.1, dV=10.1)
                         filter.arima.pc1 <- dlmFilter(y=rets.pca[,'PC1'], mod=arima.pc1)
                         filter.arima.pc1$m[-1,1]
                       }
                       )
filter.params <- xts(filter.params, order.by=index(rets.pca))
colnames(filter.params) <- paste('param.var',var.params,sep='=')
plot.zoo(cumsum(cbind(rets.pca[,'PC1'],filter.params)['2011-02/']))


### Function to build ARIMA model
build.arima <- function(var.param){
  var.param <- exp(var.param)
  return( dlmModARMA(ar=as.vector(c(var.param,0.01)), sigma2=0.1, dV=10.1) )
}

# lns2v is the observation variance: higher observation noise - more filtering
# lns2a and lns2b are the variance of the system noise: higher system noise - less filtering
init.param <- 0.0
names(init.param) <- 'lnvp'
# Estimate variances by maximizing likelihood
fit.arima.pc1 <- dlmMLE(y=rets.pca[,'PC1'], parm=init.param, build=build.arima, hessian=T)
names(fit.arima.pc1)

### Filter and smooth
# Build ARIMA dlm model using the fitted vol parameters
arima.pc1 <- build.arima(var.param=fit.arima.pc1$par)
# Kalman filter the dlm model
filter.arima.pc1 <- dlmFilter(y=rets.pca[,'PC1'], mod=arima.pc1)
class(filter.arima.pc1)
names(filter.arima.pc1)
# Kalman smooth the dlm model
smooth.arima.pc1 <- dlmSmooth(filter.arima.pc1)
class(smooth.arima.pc1)
names(smooth.arima.pc1)
# Plot
smooth.filter.param <- xts(cbind(smooth.arima.pc1$s[,2],filter.arima.pc1$m[,2])[-1,], order.by=index(rets.pca))
colnames(smooth.filter.param) <- c('smooth','filter')
chart.TimeSeries(smooth.filter.param, main="Smoothed and Filtered estimates of beta", colorset=c(1,2), lty=c(1,2), ylab=expression(beta), xlab="")

### Apply Kalman filter over expanding apply window
list.filter.params <- apply(matrix(100:nrow(rets.pca)), 1, function(n.row)
                           {
                             arima.pc1 <- build.arima(var.param=fit.arima.pc1$par, design.mat=rets.pca[1:n.row,'PC1'])
                             filter.arima.pc1 <- dlmFilter(y=rets.pca[1:n.row,'IG.MEDIAN'], mod=arima.pc1)
                             tmp <- cbind(filter.arima.pc1$m[-1,2],rets.pca[,'PC1'])
                             tmp[,1]
                           }
                           )
# End apply




### Extract smoothed states (intercept and slope)
smooth.alpha = xts(smooth.arima.pc1$s[-1,1,drop=FALSE], as.Date(rownames(smooth.arima.pc1$s[-1,])))
colnames(smooth.alpha) = "alpha"
smooth.beta = xts(smooth.arima.pc1$s[-1,2,drop=FALSE], as.Date(rownames(smooth.arima.pc1$s[-1,])))
colnames(smooth.beta) = "beta"
# Extract std errors and confidence intervals - dlmSvd2var returns list of MSE matrices
mse.list = dlmSvd2var(smooth.arima.pc1$U.S, smooth.arima.pc1$D.S)
se.mat = t(sapply(mse.list, FUN=function(x) sqrt(diag(x))))
se.xts = xts(se.mat[-1, ], index(smooth.beta))
colnames(se.xts) = c("alpha", "beta")
alpha.upper = smooth.alpha + 1.96*se.xts[,"alpha"]
alpha.lower = smooth.alpha - 1.96*se.xts[,"alpha"]
beta.upper = smooth.beta + 1.96*se.xts[,"beta"]
beta.lower = smooth.beta - 1.96*se.xts[, "beta"]


### Plot smoothed estimates with +/- 2*SE bands
chart.TimeSeries(cbind(smooth.beta, beta.lower, beta.upper), main="Smoothed estimates of beta", colorset=c(1,2,2), lty=c(1,2,2),ylab=expression(beta),xlab="")


### Out-of-sample forecasting alpha and beta using dlmFilter
# add 10 missing values to end of sample
ts.future = xts(rep(NA, 10), seq.Date(from=end(rets.pca), by="days", length.out=11)[-1])
rets.pca.ext = rbind(rets.pca[,'IG.MEDIAN',drop=FALSE], ts.future)
forecast.arima.pc1 = dlmFilter(rets.pca.ext, arima.pc1)
# extract h-step ahead forecasts of state vector
forecast.arima.pc1$m[as.character(index(ts.future)),]

