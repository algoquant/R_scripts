### Zivot Code for Dynamic Regression Model Using Package dlm (2012)
library(dlm)

### Prepare data
# Create a step-wise beta vector plus noise
vec.beta <- c(rep(0.5,times=100),rep(1.0,times=100),rep(1.5,times=100),rep(1.0,times=100),rep(0.5,times=(nrow(rets.pca)-400)))+0.1*rnorm(nrow(rets.pca))
vec.beta <- xts(vec.beta, order.by=index(rets.pca))
colnames(vec.beta) <- 'betas'
# Create random walk vector bound with its own beta multiple
ts.random <- xts(rnorm(nrow(rets.pca)), order.by=index(rets.pca))
ts.random <- cbind(ts.random,vec.beta*ts.random)
colnames(ts.random) <- c('rand','rand.beta')
plot.zoo(cbind(cumsum(ts.random),vec.beta))
# Create PC1 vector bound with its own beta multiple
ts.data <- cbind(rets.pca[,'PC1'],vec.beta*rets.pca[,'PC1'])
colnames(ts.data) <- c('PC1','PC1.beta')
plot.zoo(cbind(cumsum(ts.data),vec.beta))

### Function to build time-varying regression model
build.dyn.reg <- function(param.reg, design.mat){
  param.reg <- exp(param.reg)
  return( dlmModReg(X=design.mat, dV=param.reg[1], dW=c(param.reg[2], param.reg[3])) )
}

# lns2v is the observation variance: higher observation variance - more filtering
# lns2a and lns2b are the system variance: higher system variance - less filtering
init.param = c(0,0,0)
names(init.param) = c("lns2v", "lns2a", "lns2b")
# Estimate variances by maximizing likelihood
fit.reg <- dlmMLE(y=rets.pca[,'IG'], parm=init.param, design.mat=rets.pca[,'PC1'], build=build.dyn.reg, hessian=T)
names(fit.reg)

### Filter and smooth
# Get STDEV parameter estimates
sd.reg <- sqrt(exp(fit.reg$par))
names(sd.reg) = c("sv", "sa", "sb")
# Build dynamic regression dlm model using the fitted vol parameters
dyn.reg <- build.dyn.reg(param.reg=fit.reg$par, design.mat=rets.pca[,'PC1'])
# Kalman filter the dlm model
filter.dyn.reg <- dlmFilter(y=rets.pca[,'IG'], mod=dyn.reg)
class(filter.dyn.reg)
names(filter.dyn.reg)
# Kalman smooth the dlm model
smooth.dyn.reg <- dlmSmooth(filter.dyn.reg)
class(smooth.dyn.reg)
names(smooth.dyn.reg)
# Plot
beta.dyn.reg <- xts(filter.dyn.reg$m[-1,], order.by=index(ts.data))
colnames(beta.dyn.reg) <- c('alpha','beta')
chart.TimeSeries(cbind(vec.beta,beta.dyn.reg[,2])[-1,], main="Beta and Filtered estimates", colorset=c(1,2), lty=c(1,2), ylab=expression(beta), xlab="", legend.loc='topright')

chart.TimeSeries(beta.dyn.reg[,2], main="Filtered estimates of beta", colorset=c(1), lty=c(1), ylab=expression(beta), xlab="")
beta.dyn.reg <- xts(cbind(smooth.dyn.reg$s[,2],filter.dyn.reg$m[,2])[-1,], order.by=index(rets.pca))
colnames(beta.dyn.reg) <- c('alpha','beta')
chart.TimeSeries(beta.dyn.reg, main="Smoothed and Filtered estimates of beta", colorset=c(1,2), lty=c(1,2), ylab=expression(beta), xlab="", legend.loc='topright')

### Apply Kalman filter to random data for a range of variance parameters
var.betas <- -5*(-1:6)
par(mfcol=c(4,2))
beta.dyn.reg <- sapply(var.betas, function(var.beta)
                       {
                         fit.reg$par['lns2v'] <- var.beta
                         dyn.reg <- build.dyn.reg(param.reg=fit.reg$par, design.mat=ts.data[,'PC1'])
                         filter.dyn.reg <- dlmFilter(y=ts.data[,'PC1.beta'], mod=dyn.reg)
                         beta.dyn.reg <- xts(filter.dyn.reg$m[-1,2], order.by=index(ts.data))
                         colnames(beta.dyn.reg) <- paste('KF',var.beta,sep='=')
                         chart.TimeSeries(cbind(beta.dyn.reg,vec.beta)[-1,], main="", colorset=c(1,2), lty=c(1,3), ylab="", xlab="", legend.loc='topright')
#                         browser()
                         coredata(beta.dyn.reg)
                       }
                       )
beta.dyn.reg <- xts(beta.dyn.reg, order.by=index(ts.data))
colnames(beta.dyn.reg) <- paste('beta.ovar',var.betas,sep='=')
plot.zoo(cbind(cumsum(ts.data[,1:2]),beta.dyn.reg)[-1,],ylab="")


### Apply Kalman filter to pca data for a range of variance parameters
var.betas <- -5*(-1:6)
par(mfcol=c(4,2))
beta.dyn.reg <- sapply(var.betas, function(var.beta)
                       {
                         fit.reg$par['lns2v'] <- var.beta
                         dyn.reg <- build.dyn.reg(param.reg=fit.reg$par, design.mat=rets.pca[,'PC1'])
                         filter.dyn.reg <- dlmFilter(y=rets.pca[,'IG'], mod=dyn.reg)
                         beta.dyn.reg <- xts(filter.dyn.reg$m[-1,2], order.by=index(rets.pca))
                         colnames(beta.dyn.reg) <- paste('KF',var.beta,sep='=')
                         chart.TimeSeries(beta.dyn.reg[-1,], main="", colorset=c(1,2), lty=c(1,3), ylab="", xlab="")
#                         browser()
                         coredata(beta.dyn.reg)
                       }
                       )
beta.dyn.reg <- xts(beta.dyn.reg, order.by=index(ts.data))
colnames(beta.dyn.reg) <- paste('beta.ovar',var.betas,sep='=')
plot.zoo(cbind(cumsum(rets.pca[,1:2]),beta.dyn.reg)[-1,],ylab="")


### Apply Kalman filter over expanding apply window
list.betas.dyn.reg <- apply(matrix(100:nrow(rets.pca)), 1, function(n.row)
                            {
                              dyn.reg <- build.dyn.reg(param.reg=fit.reg$par, design.mat=rets.pca[1:n.row,'PC1'])
                              filter.dyn.reg <- dlmFilter(y=rets.pca[1:n.row,'IG'], mod=dyn.reg)
                              tmp <- cbind(filter.dyn.reg$m[-1,2],rets.pca[,'PC1'])
                              tmp[,1]
                            }
                            )
# End lapply




### Extract smoothed states (intercept and slope)
smooth.alpha = xts(smooth.dyn.reg$s[-1,1,drop=FALSE], as.Date(rownames(smooth.dyn.reg$s[-1,])))
colnames(smooth.alpha) = "alpha"
smooth.beta = xts(smooth.dyn.reg$s[-1,2,drop=FALSE], as.Date(rownames(smooth.dyn.reg$s[-1,])))
colnames(smooth.beta) = "beta"
# Extract std errors and confidence intervals - dlmSvd2var returns list of MSE matrices
mse.list = dlmSvd2var(smooth.dyn.reg$U.S, smooth.dyn.reg$D.S)
se.mat = t(sapply(mse.list, FUN=function(x) sqrt(diag(x))))
se.xts = xts(se.mat[-1, ], index(smooth.beta))
colnames(se.xts) = c("alpha", "beta")
alpha.upper = smooth.alpha + 1.96*se.xts[,"alpha"]
alpha.lower = smooth.alpha - 1.96*se.xts[,"alpha"]
beta.upper = smooth.beta + 1.96*se.xts[,"beta"]
beta.lower = smooth.beta - 1.96*se.xts[, "beta"]


### Plot smoothed estimates with +/- 2*SE bands
chart.TimeSeries(cbind(smooth.beta, beta.lower, beta.upper), main="Smoothed estimates of beta", colorset=c(1,2,2), lty=c(1,2,2),ylab=expression(beta),xlab="", legend.loc='topright')


### Out-of-sample forecasting alpha and beta using dlmFilter
# add 10 missing values to end of sample
ts.future = xts(rep(NA, 10), seq.Date(from=end(rets.pca), by="days", length.out=11)[-1])
rets.pca.ext = rbind(rets.pca[,'IG',drop=FALSE], ts.future)
forecast.dyn.reg = dlmFilter(rets.pca.ext, dyn.reg)
# extract h-step ahead forecasts of state vector
forecast.dyn.reg$m[as.character(index(ts.future)),]

