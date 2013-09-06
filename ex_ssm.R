# Sebastian Fossati
# 1/2013
# SS Models in detail


rm(list=ls(all=TRUE))
ls()

# Set directory
# Check you have the right file path!!
filepath <- '/Users/sfossati/Dropbox/UofA/E509/R files/SS Models'
#filepath <- '/Users/seba/Dropbox/UofA/E509/R files/SS Models'
setwd(filepath)

# Set seed
set.seed(123)


# Functions

kfilter <- function(SS,out=0){
	
	# Set up SS model
	yt <- as.matrix(SS$y)
	# measurement equation
	Ft <- SS$F; Vt <- SS$V
	# transition equation
	Gt <- SS$G; Wt <- SS$W
	# Get dimensions
	n.obs <- nrow(yt); n.var <- ncol(yt); n.dim <- nrow(Gt)

	# Kalman filter and prediction error decomposition
	# set up storage matrices
	SigmaQt <- SigmaetQtet <- 0
	mtt <- mt <- matrix(0,n.dim,(n.obs+1))
	Ctt <- Ct <- array(0,dim=c(n.dim,n.dim,n.obs+1))
	et <- matrix(0,n.var,(n.obs+1))
	Qt <- array(0,dim=c(n.var,n.var,(n.obs+1)))
	Lt <- array(0,dim=c(n.dim,n.var,(n.obs+1)))
	# set initial conditions
	mtt[,1] <- SS$m0; Ctt[,,1] <- SS$C0

	# filter
	for (i in 1:n.obs){
		# Prediction equation
		mt[,i+1] <- Gt%*%mtt[,i]
		Ct[,,i+1] <- Gt%*%Ctt[,,i]%*%t(Gt) + Wt
		# Prediction error
		et[,i+1] <- t(yt[i,]) - Ft%*%mt[,i+1]
		Qt[,,i+1] <- Ft%*%Ct[,,i+1]%*%t(Ft) + Vt
		# Updating equations
		Lt[,,i+1] <- Ct[,,i+1]%*%t(Ft)%*%solve(Qt[,,i+1])
		mtt[,i+1] <- mt[,i+1] + Lt[,,i+1]%*%as.matrix(et[,i+1])
		Ctt[,,i+1] <- Ct[,,i+1] - Lt[,,i+1]%*%Ft%*%Ct[,,i+1]
		# Log likelihood
		SigmaQt <- SigmaQt + log(det(as.matrix(Qt[,,i+1])))
		SigmaetQtet <- SigmaetQtet + t(et[,i+1])%*%solve(Qt[,,i+1])%*%et[,i+1]
		}
	# compute log-likelihood
	loglike <- -0.5*n.obs*n.var*log(2*pi) - 0.5*SigmaQt - 0.5*SigmaetQtet
	
	# Output
	if (out==0){ return(-loglike) }
	else {
		return( list(loglike=loglike,et=et,Qt=Qt,mtt=mtt,Ctt=Ctt,												mt=mt,Ct=Ct,n.obs=n.obs,n.var=n.var,n.dim=n.dim) )
		}
	}


getFit <- function(SS,yt,parm, ...){
	# set function for optimization
	loglike <- function(parm,yt,SSmodel=SS){ 
		return(kfilter(SSmodel(parm,yt)))
		}
	# run optimization
	res <- optim(parm,loglike,yt=yt,method="L-BFGS-B", ...)
	return(res)
	}


ssm1 <- function(parm,yt){
	# Set up SS model
	# measurement equation
	Ft = matrix(1)
	Vt = matrix(0)
	# transition equation
	Gt = matrix(parm[1])
	Wt = matrix(parm[2])
	# Initial conditions
	m0 <- matrix(0,nr=1); C0 <- parm[2]/(1-parm[1]^2)
	# return list
	SS <- list(F=Ft,V=Vt,G=Gt,W=Wt,m0=m0,C0=C0,y=yt)
	return(SS)
}


# Simulated AR(1) process
y0 <- arima.sim(n=250,list(ar=.6,ma=0),sd=1)

# estimate AR(1)
model10 <- arima(y0,order=c(1,0,0),method="ML",include.mean=FALSE)
model10

	
# estimate model using SS model
fit1 <- getFit(SS=ssm1,y=y0,parm=c(.5,.8),hessian=T)

# get estimates and standard errors
fit1$par
avar <- solve(fit1$hessian)
sqrt(diag(avar)) 
	

