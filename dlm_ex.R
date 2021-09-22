# Sebastian Fossati
# 1/2013
# SS Models with DLM package


rm(list=ls(all=TRUE))
ls()

library(dlm)
library(forecast)

# Set directory
# Check you have the right file path!!
filepath <- '/Users/sfossati/Dropbox/UofA/E509/R files/SS Models'
#filepath <- '/Users/seba/Dropbox/UofA/E509/R files/SS Models'
setwd(filepath)

# Set seed
set.seed(123)


##################################################################
# SS Model
# 
# y(t) = FF a(t) + v(t)		v(t)~N(0,V(t))
# a(t) = GG a(t-1) + w(t)	w(t)~N(0,W(t))
#							a(0)~N(m0,C0)
# 
##################################################################


##################################################################
# Example 1: AR(1) specifying SS model directly
##################################################################

# simulate AR(1) process
# phi = .8, sig2 = .25
nobs <- 250
yt <- arima.sim(n=nobs,list(ar=.8,ma=0),sd=.5)

# estimate AR(1) for comparison
model10 <- Arima(yt,order=c(1,0,0),method="ML",include.mean=FALSE)
model10

# set parameter restrictions 
parm_rest <- function(parm){
 	return( c(exp(parm[1])/(1+exp(parm[1])),exp(parm[2])) ) 
	}

# set up SS model
ssm1 <- function(parm){
	parm <- parm_rest(parm)
	return( dlm(FF=1,V=0,GG=parm[1],W=parm[2],
				m0=0,C0=solve(1-parm[1]^2)*parm[2]) )
	}
# estimate parameters
fit1 <- dlmMLE(y=yt,parm=c(0,1),build=ssm1,hessian=T)

# get estimates 
coef <- parm_rest(fit1$par)
# get standard errors using delta method
dg1 <- exp(fit1$par[1])/(1+exp(fit1$par[1]))^2
dg2 <- exp(fit1$par[2])
dg <- diag(c(dg1,dg2))
var <- dg%*%solve(fit1$hessian)%*%dg
# print results
coef; sqrt(diag(var))



##################################################################
# Example 2: AR(1) using dlmModARMA
##################################################################

# set parameter restrictions 
parm_rest <- function(parm){
 	return( c(exp(parm[1])/(1+exp(parm[1])),exp(parm[2])) ) 
	}

# a simpler option is to use DLM package functions
# set up SS model
ssm2 <- function(parm){
	parm <- parm_rest(parm)
	dlm <- dlmModARMA(ar=parm[1], ma=NULL, sigma2=parm[2])
	dlm$C0 <- solve(1-parm[1]^2)*parm[2] 
	return(dlm)
	}
# estimate parameters
fit2 <- dlmMLE(y=yt,parm=c(0,1),build=ssm2,hessian=T)

# get estimates 
coef <- parm_rest(fit2$par)
# get standard errors using delta method
dg1 <- exp(fit2$par[1])/(1+exp(fit2$par[1]))^2
dg2 <- exp(fit2$par[2])
dg <- diag(c(dg1,dg2))
var <- dg%*%solve(fit2$hessian)%*%dg
# print results
coef; sqrt(diag(var))



##################################################################
# Example 3: MA(1) using dlmModARMA
##################################################################

# simulated MA(1) process
# theta = .6, sig2 = .25
nobs <- 250
yt <- arima.sim(n=nobs,list(ar=0,ma=0.6),sd=.5)

# estimate MA(1) for comparison
model01 <- Arima(yt,order=c(0,0,1),method="ML",include.mean=FALSE)
model01

# set parameter restrictions 
parm_rest <- function(parm){
 	return( c(exp(parm[1])/(1+exp(parm[1])),exp(parm[2])) ) 
	}

# set up SS model
ssm3 <- function(parm){
	parm <- parm_rest(parm)
	return( dlmModARMA(ar=NULL, ma=parm[1], sigma2=parm[2]) )
	}
# estimate parameters
fit3 <- dlmMLE(y=yt,parm=c(0,1),build=ssm3,hessian=T)

# get estimates 
coef <- parm_rest(fit3$par)
# get standard errors using delta method
dg1 <- exp(fit3$par[1])/(1+exp(fit3$par[1]))^2
dg2 <- exp(fit3$par[2])
dg <- diag(c(dg1,dg2))
var <- dg%*%solve(fit3$hessian)%*%dg
# print results
coef; sqrt(diag(var))



##################################################################
# Example 4: Linear reg using dlmModReg
##################################################################

# model set up
# a = 4, b = 2, sigma2 = .25
nobs <- 250
a <- 4; b <- 2
# simulate data
xt <- rnorm(nobs,mean=0,sd=1)
yt <- a + b * xt + rnorm(nobs,mean=0,sd=.5) 

# plot data
plot(xt,yt)

# fit linear model
fit4 <- lm(yt~xt)
summary(fit4)

# set parameter restrictions 
parm_rest <- function(parm){
 	return( exp(parm[1]) ) 
	}

# set up SS model
ssm4 <- function(parm,x.mat){
	parm <- parm_rest(parm)
	return( dlmModReg(X=x.mat, dV=parm[1]) )
	}
# estimate parameters
fit4 <- dlmMLE(y=yt,parm=1,x.mat=xt,build=ssm4,hessian=T)

# get residual variance estimate
se2 <- parm_rest(fit4$par)
sqrt(se2)

# get parameter estimates (slope and intercept)
# these are the last filtered values of a
LSMod <- ssm4(fit4$par,xt)
LSf <- dlmFilter(yt,LSMod)

# get parameters
coef <- LSf$m[nobs+1,]
covar <- dlmSvd2var(LSf$U.C[[nobs+1]],LSf$D.C[nobs+1,])
coef.se <- c(sqrt(covar[1,1]),sqrt(covar[2,2]))
coef; coef.se



##################################################################
# Example 5: TVP Reg using dlmModReg
##################################################################

# model set up
# a0 = 1, b0 = 2, a1 = 2, b1 = 3, sigma2 = .25
nobs <- 500
a <- as.ts(c(rep(1,nobs/2),rep(2,nobs/2)))
b <- as.ts(c(rep(2,nobs/2),rep(3,nobs/2)))
# simulate data
xt <- rnorm(nobs,mean=0,sd=1)
yt <- a + b * xt + rnorm(nobs,mean=0,sd=.5) 

# fit linear model for full sample (misspecified model)
fit5 <- lm(yt~xt)
summary(fit5)
# fit linear model to the first half of the sample
fit5a <- lm(yt[1:nobs/2]~xt[1:nobs/2])
summary(fit5a)
# fit linear model to the second half of the sample
fit5b <- lm(yt[nobs/2+1:nobs]~xt[nobs/2+1:nobs])
summary(fit5b)

# set parameter restrictions 
parm_rest <- function(parm){
 	return( exp(parm) ) 
	}

# set up SS model
ssm5 <- function(parm,x.mat){
	parm <- parm_rest(parm)
	return( dlmModReg(X=x.mat, dV=parm[1], dW=c(parm[2],parm[3])) )
	}
# estimate parameters
fit5 <- dlmMLE(y=yt,parm=c(1,1,1),x.mat=xt,build=ssm5,hessian=T)

# get variance estimates
se2 <- parm_rest(fit5$par)
sqrt(se2)

# get parameter estimates over time
# these are the smoothed values of a
TVPMod <- ssm5(fit5$par,xt)
TVPf <- dlmFilter(yt,TVPMod)
TVPs <- dlmSmooth(TVPf)

# plot parameters
plot(cbind(a,TVPf$m[-1,1],TVPs$s[-1,1]),plot.type='s',
		col=c("red","black","blue"),
		main="TVP Model - Intercept",lwd=c(1,2,2))
plot(cbind(b,TVPf$m[-1,2],TVPs$s[-1,2]),plot.type='s',
		col=c("red","black","blue"),
		main="TVP Model - Slope",lwd=c(1,2,2))



##################################################################
# Example 6: ARMA model + time trend using dlmModReg + dlmModARMA
##################################################################

# simulate AR(2) process + time trend
# phi1 = .6, phi2 = .2, sig2 = 1
nobs <- 250
a <- 1; b <- .1
tt <- as.numeric(seq(1,nobs))
yt <- a + b*tt + arima.sim(n=nobs,list(ar=c(.6,.2),ma=0),sd=1)

# set parameter restrictions (only variances here)
parm_rest <- function(parm){
 	return( c(parm[1],parm[2],exp(parm[3])) ) 
	}

# set up SS model
ssm6 <- function(parm,x.mat){
	parm <- parm_rest(parm)
	dlm <- dlmModReg(X=x.mat, addInt=TRUE, dV=1e-7) +
		dlmModARMA(ar=c(parm[1],parm[2]), ma=NULL, sigma2=parm[3])
	# get distribution variance of initial state
	tmp0 <- matrix(c(parm[1],parm[2],1,0),nr=2)
	tmp1 <- matrix(c(parm[3],0,0,0),nc=1)
	tmp <- solve(diag(4)-tmp0%x%tmp0)%*%tmp1
	dlm$C0[3:4,3:4] <- matrix(tmp,nr=2)
	return( dlm )
	}
# estimate parameters
fit6 <- dlmMLE(y=yt,parm=c(0,0,1),x.mat=tt,build=ssm6,hessian=T)

# get estimates and standard errors for AR(2) part
coef <- parm_rest(fit6$par)
# get standard errors using delta method
dg1 <- 1
dg2 <- 1
dg3 <- exp(fit6$par[3])
dg <- diag(c(dg1,dg2,dg3))
var <- dg%*%solve(fit6$hessian)%*%dg
# print results
coef; sqrt(diag(var))

# get parameter estimates (slope and intercept)
# these are the last filtered values of a
LSARMod <- ssm6(fit6$par,tt)
LSARf <- dlmFilter(yt,LSARMod)

# get parameters
coef <- LSARf$m[nobs+1,1:2]
covar <- dlmSvd2var(LSARf$U.C[[nobs+1]],LSARf$D.C[nobs+1,])
coef.se <- c(sqrt(covar[1,1]),sqrt(covar[2,2]))
coef; coef.se



##################################################################
# Example 7: ARMA model + time trend using dlmModPoly + dlmModARMA
##################################################################

# set parameter restrictions (only variances here)
parm_rest <- function(parm){
 	return( c(parm[1],parm[2],exp(parm[3])) ) 
	}

# set up SS model
ssm7 <- function(parm){
	parm <- parm_rest(parm)
	dlm <- dlmModPoly(2,dV=1e-7,dW=c(0,0)) + 
		dlmModARMA(ar=c(parm[1],parm[2]), ma=NULL, sigma2=parm[3])
	# get distribution variance of initial state
	tmp0 <- matrix(c(parm[1],parm[2],1,0),nr=2)
	tmp1 <- matrix(c(parm[3],0,0,0),nc=1)
	tmp <- solve(diag(4)-tmp0%x%tmp0)%*%tmp1
	dlm$C0[3:4,3:4] <- matrix(tmp,nr=2)
	return( dlm )
	}
# estimate parameters
fit7 <- dlmMLE(y=yt,parm=c(0,0,1),build=ssm7,hessian=T)

# get estimates and standard errors for AR(2) part
coef <- parm_rest(fit7$par)
# get standard errors using delta method
dg1 <- 1
dg2 <- 1
dg3 <- exp(fit7$par[3])
dg <- diag(c(dg1,dg2,dg3))
var <- dg%*%solve(fit7$hessian)%*%dg
# print results
coef; sqrt(diag(var))

# get parameter estimates (slope and intercept)
# these are the last filtered values of a
LSARMod2 <- ssm7(fit7$par)
LSARf2 <- dlmFilter(yt,LSARMod2)

# get parameters
coef <- LSARf2$m[nobs+1,1:2]
coef[1] <- coef[1]-nobs*coef[2]
covar <- dlmSvd2var(LSARf2$U.C[[nobs+1]],LSARf2$D.C[nobs+1,])
coef.se <- c(sqrt(covar[1,1]),sqrt(covar[2,2]))
coef; coef.se



##################################################################
# Example 8: Random walk plus noise
##################################################################

# simulate random walk + noise process
nobs <- 250
vt <- sqrt(3)*rnorm(nobs) 		# var(v) = 3
wt <- sqrt(.5)*rnorm(nobs) 		# var(w) = .5
# simulate time series
xt <- rep(nobs)
xt[1] <- wt[1]
for(i in 2:nobs){ xt[i] <- xt[i-1]+wt[i] }
yt <- as.ts(xt) + vt

# plot process
plot(cbind(yt,xt),plot.type='s',col=c("black","blue"),
			main="Random walk plus noise",lwd=c(1,2))

# set parameter restrictions (only variances here)
parm_rest <- function(parm){
 	return( exp(parm) ) 
	}

# set up SS model
ssm8 <- function(parm){
	parm <- parm_rest(parm)
	return( dlm(FF=1,V=parm[2],GG=1,W=parm[1],m0=0,C0=10^7) )
	}
# estimate parameters
fit8 <- dlmMLE(y=yt,parm=c(1,1),build=ssm8,hessian=T)

# get estimates
se2 <- parm_rest(fit8$par)
se2

# filter and smooth
mod8 <- ssm8(fit8$par)
mod8f <- dlmFilter(yt,mod8)
mod8s <- dlmSmooth(mod8f)

# get filtered and smoothed estimates
# and standard errors

# filtered values
xtfilt <- ts(mod8f$m[-1],start=1)
# width of 90% confidence interval
sefilt <- dlmSvd2var(mod8f$U.C,mod8f$D.C)[-1]
width <- qnorm(.95) * sqrt(sapply(sefilt,diag))
# put filtered results together
xtfiltered <- cbind(xtfilt,as.vector(xtfilt)+width%o%c(1,-1))

# smoothed values
xtsmooth <- ts(mod8s$s[-1],start=1)
# width of 90% confidence interval
sesmooth <- dlmSvd2var(mod8s$U.S,mod8s$D.S)[-1]
width <- qnorm(.95) * sqrt(sapply(sesmooth,diag))
# put smoothed results together
xtsmoothed <- cbind(xtsmooth,as.vector(xtsmooth)+width%o%c(1,-1))

# plot fitered state + 90% interval
plot(cbind(yt,xtfiltered),plot.type='s',
		col=c("black","blue","red","red"),
		main="Random walk plus noise - Filtered state",
		lwd=c(1,2,2,2))
# plot smoothed state + 90% interval
plot(cbind(yt,xtsmoothed),plot.type='s',
		col=c("black","blue","red","red"),
		main="Random walk plus noise - Smoothed state",
		lwd=c(1,2,2,2))



##################################################################
# Example 9: Forecasting using dlmForecast or as missing values
##################################################################

# simulate AR(1) process
# phi = .8, sig2 = .25
nobs <- 250
yt <- arima.sim(n=nobs,list(ar=.8,ma=0),sd=.5)

# set parameter restrictions 
parm_rest <- function(parm){
 	return( c(exp(parm[1])/(1+exp(parm[1])),exp(parm[2])) ) 
	}

# set up SS model
ssm9 <- function(parm){
	parm <- parm_rest(parm)
	dlm <- dlmModARMA(ar=parm[1], ma=NULL, sigma2=parm[2])
	dlm$C0 <- solve(1-parm[1]^2)*parm[2] 
	return(dlm)
	}
	

# Case 1: fit model using 230 observations and 
# forecast next 20 observations using dlmForecast

# estimate parameters
xt <- yt[1:230]
fit9 <- dlmMLE(y=xt,parm=c(0,1),build=ssm9,hessian=T)

# filter and smooth
mod9 <- ssm9(fit9$par)
mod9f <- dlmFilter(xt,mod9)

# forecast using SS model
fore9 <- dlmForecast(mod9f,nAhead=20,method="plain")


# Case 2: fit model using 250 observations where 
# the last 20 are missing

# estimate parameters
zt <- matrix(NA,250,1)
zt[1:230] <- xt
fit10 <- dlmMLE(y=zt,parm=c(0,1),build=ssm9,hessian=T)

# filter and smooth
mod10 <- ssm1(fit10$par)
mod10f <- dlmFilter(zt,mod10)

# forecast are last observations
fore10 <- mod10f$f[231:250]


# compare results
fore.all <- cbind(fore9$f,fore10,fore9$f-fore10)
fore.all


