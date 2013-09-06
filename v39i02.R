###################################################
### chunk number 1: 
###################################################
set.seed(1)
options(warn=-1,width=65,prompt="R> ")
rm(list=ls())
library("xtable")
#
#   With multiple cores available, some of the code
#   below may run in parallel, for faster execution.
#   With only one core, the code will run as is.
#
library("multicore")
library("doMC")
registerDoMC(cores = 1)  #  Can be changed on multicore
                         #  machines.
library("foreach")
require("weaver")        #  Required to cache expensive
                         #  computations.
require("dse")
require("sspir")
require("KFAS")
require("FKF")
require("dlm")
require("xts")           #  Loads package zoo
require("lattice")
require("latticeExtra")


###################################################
### chunk number 2: DataInput
###################################################
#
#   These are datasets used in the paper, some part
#   of R, some included alongside the paper.
#
data("Nile", package = "datasets")
Nilo <- as.numeric(Nile)
#
#   First time the code is run, a zoo object is created;
#   subsequent executions use it without recreation
#   each time.
#
if (!file.exists("divisas.rda")) {
  divisas <- read.zoo(file="divisas.txt",
                      format="%d/%m/%Y")
  save(divisas,file="divisas.rda")
} else {
  load("divisas.rda")}
divisas.zoo <- divisas
divisas <- coredata(divisas)
colnames(divisas) <- c("AUD","BEF","CHF","DEM",
                       "DKK","ESP","FRF","GBP","ITL",
                       "JPY","NGL","SEK")


###################################################
### chunk number 3: Versiones
###################################################
#
#   Record versions used. The paper used several
#   versions of some packages, which have experienced
#   some changes (partly because of my requests).
#   This helps ensure the version really used is
#   cited.
#
aaa      <- installed.packages()
paq <- c("dse","sspir","dlm","FKF","KFAS")
filas    <- match(paq,aaa)
paq <- aaa[filas,c("Package","Version")]


###################################################
### chunk number 4: DseModConstruction1
###################################################
m1.dse <- dse::SS(F=matrix(1.0,1,1),Q=matrix(40,1,1),
                  H=matrix(1.0,1,1),R=matrix(130,1,1),
                  z0=matrix(0,1,1),P0=matrix(10^5,1,1))


###################################################
### chunk number 5: DseModConstruction1
###################################################
m1b.dse <- dse::SS(F=matrix(1.0,1,1),Q=matrix(40,1,1),
                  H=matrix(1.0,1,1),R=matrix(130,1,1),
                  constants=list(Q=matrix(TRUE,1,1),P0=matrix(TRUE,1,1)),
                  z0=matrix(0,1,1),P0=matrix(10^5,1,1))


###################################################
### chunk number 6: DseNileLocalLevel
###################################################
data("Nile", package = "datasets")
m1b.dse.est <- estMaxLik(m1b.dse,TSdata(output=Nile))


###################################################
### chunk number 7: SspirConstruction
###################################################
data("Nile", package = "datasets")
m1.sspir <- sspir::SS(Fmat=function(tt,x,phi) { return(matrix(1)) },
                 Gmat=function(tt,x,phi) { return(matrix(1)) },
                 Vmat=function(tt,x,phi) { return(matrix(exp(phi[1]))) },
                 Wmat=function(tt,x,phi) { return(matrix(exp(phi[2]))) },
                 y=as.matrix(Nile,ncol=1))
str(m1.sspir)                                  


###################################################
### chunk number 8: SspirManipulation
###################################################
C0(m1.sspir) <- matrix(10^7,1,1)
phi(m1.sspir) <- c(9,7)


###################################################
### chunk number 9: SspirInspecting
###################################################
phi(m1.sspir)


###################################################
### chunk number 10: KalmanFiltSspir
###################################################
m1.sspir.f <- kfilter(m1.sspir)


###################################################
### chunk number 11: FormulaInterfaceSspir
###################################################
mod2.sspir <- ssm(Nilo ~ tvar(1), family="gaussian", C0=diag(1)*10^7,
                  phi=exp(c(9,7)))
class(mod2.sspir)


###################################################
### chunk number 12: DlmConstruction
###################################################
m1.dlm <- dlm(FF = 1, V = 0.8, GG = 1, W = 0.1, m0 = 0, C0 = 100)


###################################################
### chunk number 13: DlmStructure
###################################################
str(m1.dlm)


###################################################
### chunk number 14: 
###################################################
N <- 100
X <- matrix(rnorm(2*N),N,2)
y <- 3 + 2*X[,1] + 5*X[,2] + 2*rnorm(N)


###################################################
### chunk number 15: ExampleDlmTimeVaryingRegression
###################################################
m2.dlm <- dlm(FF=matrix(c(1,0,0),1,3),JFF=matrix(c(0,1,2),1,3),
              GG=diag(3),W=0.1*diag(3),V=4,m0=c(0,0,0),
              C0=10^9*diag(3),X=X)


###################################################
### chunk number 16: ExampleDlmFunctions
###################################################
m3.dlm <- dlmModARMA(ar=c(0.3,0.2,0.4),ma=c(0.9,0.1))
m3.dlm


###################################################
### chunk number 17: ExampleDlmFunctions
###################################################
m4.dlm <- dlmModPoly(order=1) + dlmModSeas(4)


###################################################
### chunk number 18: ExampleDlmOuterSum
###################################################
m3Plusm4.dlm <- m4.dlm %+% m3.dlm


###################################################
### chunk number 19: dlmDemoOuterSum1
###################################################
GG(m3Plusm4.dlm)


###################################################
### chunk number 20: dlmDemoOuterSum2
###################################################
FF(m3Plusm4.dlm)


###################################################
### chunk number 21: CurrenciesPlot
###################################################
#
#   A subset of the divisas data set is selected and
#   displayed (Figure 1).
#
divisas.zoo <- divisas.zoo[,1:4]
colnames(divisas.zoo) <- c("AUD","BEF","CHF","DEM")
pdf(file="curr.pdf") # ,paper="a4",horizontal=FALSE,height=8.70,width=6)
aaa <- xyplot(divisas.zoo[,4:1],
              data=divisas.zoo,#strip.levels=c(TRUE,TRUE),
              #strip.left=TRUE,strip.names=c(TRUE,TRUE),var.name=c("a","b","c","d"),
              # strip=FALSE,
              scales=list(y=list(tick.number=3)),
              type="l",
              # auto.key=list(columns=2,lines=TRUE,points=FALSE,
              #              text=c("Rolling window estimate","Local level estimate")),
              # main="Exchange rate against the US\$ of four currencies",
              xlab="Date",ylab="Units per US dollar" 
       )
print(aaa)
aaa <- dev.off()


###################################################
### chunk number 22: NiloExample
###################################################
InitialValues <- c(1000,160)
Tolerance <- 10^(-12)
C0 <- diag(1)*10^7
#
#  Nile local level model with sspir
#
Nile.sspir.like  <- function(params) {
  fit <- ssm(Nilo ~ tvar(1), family="gaussian", C0=C0,
                  phi=params, fit=TRUE)
  return(getFit(fit)$likelihood)
}
Nile.sspir.time <- system.time(Nile.sspir.fit <- 
                               optim(par=InitialValues,fn=Nile.sspir.like,method="L-BFGS-B",
                                     lower=c(0,0),upper=c(Inf,Inf),
                                     control=list(fnscale=-1,reltol=Tolerance)))[3]
Nile.sspir.est   <- ssm(Nilo ~ tvar(1), family="gaussian",C0=C0,
                     phi = Nile.sspir.fit$par)
phi(Nile.sspir.est)
#
#  Nile local level model with dlm
#
buildFun <- function(params) {
  dlmModPoly(1,dV=params[1], dW=params[2])
}
Nile.dlm.time <- system.time(Nile.dlm.fit <- dlmMLE(Nilo, parm = InitialValues, lower=c(0,0),
                             upper=c(Inf,Inf),build = buildFun,control=list(reltol=Tolerance)))[3]
Nile.dlm.fit$par
#
#  Nile local level model with FKF
#
yt <- matrix(Nilo,nrow=1)
Nile.fkf.like <- function(params) {
  fit <- fkf(a0=c(0),P0=C0,dt=matrix(0,ncol=1),
             ct=matrix(0,ncol=1),Tt=matrix(1,ncol=1),Zt=matrix(1,ncol=1),
             HHt=matrix(params[2],ncol=1),GGt=matrix(params[1],ncol=1),yt=yt)
  return(fit$logLik)
}
Nile.fkf.time <- system.time(Nile.fkf.fit <- optim(par=InitialValues,fn=Nile.fkf.like,method="L-BFGS-B",
                    control=list(fnscale=-1,reltol=Tolerance)))[3]
Nile.fkf.fit$par
#
#  Nile local level model with KFAS
#
yt <- t(data.matrix(Nile))
Nile.kfas.like <- function(params) {
  fit <- kf(yt,Zt=1,Tt=1,Rt=1,Ht=params[1],Qt=params[2],
            a1=0, P1=C0)
  return(fit$lik)
}
Nile.kfas.time <- system.time(Nile.kfas.fit <- optim(par=InitialValues,fn=Nile.kfas.like,
                                                     method="L-BFGS-B",
                                                     control=list(fnscale=-1,reltol=Tolerance)))[3]
Nile.kfas.fit$par
#
#   With diffuse initialization
#
Nile.kfas.like.d <- function(params) {
  fit <- kf(yt,Zt=1,Tt=1,Rt=1,Ht=params[1],Qt=params[2],
            a1=0, P1=0, P1inf=1)
  return(fit$lik)
}
Nile.kfas.time.d <- system.time(Nile.kfas.fit.d <- 
                                  optim(par=InitialValues,fn=Nile.kfas.like.d,method="L-BFGS-B",
                                        control=list(fnscale=-1,reltol=Tolerance)))[3]
Nile.kfas.fit.d$par
#


###################################################
### chunk number 23: RegressionExample
###################################################
set.seed(12345)                     # to make things replicable.
reg.res.C0 <- 10^3                  # initial variances state
reg.res <- matrix(NA,1,8)
reg.res.times <- matrix(NA,1,8)
N <- 1000 ; p <- 5
X <- matrix(rnorm(p*N),N,p)
Betas.true <- 1:p                   # "True" betas.
y <- X %*% Betas.true + rnorm(N)    # The y's are generated.
#
#   Betas estimated by ordinary LS to check Kalman filter results
#
scratch <- lsfit(X,y,intercept=FALSE)
betas.MCO <- scratch$coefficients
sigma2.MCO <- sum(scratch$residuals^2) / (N-p)
Xy <- as.data.frame(cbind(X,y))
colnames(Xy) <- c("X1","X2","X3","X4","X5","y")
#
#  
C0 <- diag(p)*reg.res.C0
InitialValues <- c(2)
#
#  Estimation with sspir
#
Reg.sspir.model <- sspir::SS(y=y,x=list(X=X),
                      Fmat=function(tt,x,phi) { return(matrix(x$X[tt,],ncol=1)) },
                      Gmat=function(tt,x,phi) { return(diag(p)) },
                      Vmat=function(tt,x,phi) { return(matrix(phi[1])) },
                      Wmat=function(tt,x,phi) { return(matrix(0,p,p)) },
                      m0= matrix(0,1,p),
                      C0=C0,
                      phi = InitialValues
                      )
Reg.sspir.like <- function(parms) {
  phi(Reg.sspir.model) <- parms[1]
  return(kfilter(Reg.sspir.model)$loglik)
}
sspir.result <- class(try(reg.res.times[1,4]<- 
                          system.time(Reg.sspir.fit <- optim(par=InitialValues,
                                                             fn=Reg.sspir.like,
                                                             method="L-BFGS-B",
                                                             control=list(fnscale=-1,reltol=Tolerance),
                                                             lower=0,upper=Inf))[3]))
#
#  Now we run the Kalman filter with the estimated variance of the measurements.
#
if (sspir.result != "try-error") {
   phi(Reg.sspir.model) <- Reg.sspir.fit$par
   aaa <- kfilter(Reg.sspir.model)
   betas.sspir <- aaa$m[N,]
 } else {
   betas.sspir <- rep(NA,p)
   Reg.sspir.fit$par <- NA
 }

#
#  Same with dlm
#
buildFun <- function(parm) {
  N <- N ; p <- p ; X <- X ; y <- y ; C0=C0
  return(dlm(FF = matrix(1,1,p),JFF=matrix(1:p,1,p),GG=diag(p),
             V=matrix(parm[1],1,1),W=matrix(0,p,p),X=X,
             m0=rep(0,p),C0=C0))
}
reg.res.times[1,5] <- system.time(Reg.dlm.fit <- dlmMLE(y, parm = InitialValues,lower=0,upper=Inf,
                      build = buildFun,control=list(reltol=Tolerance)))[3]
Reg.dlm.model <- buildFun(Reg.dlm.fit$par)
aaa <- dlmFilter(y,mod=Reg.dlm.model)
betas.dlm <- aaa$m[N+1,]
#
#  Same with FKF
#
yt <- t(y)
Zt <- array(t(X),dim=c(1,p,N))
Reg.fkf.like <- function(parm) {
   yt <- yt ; N <- N ; p <- p ; Zt <- Zt ; C0=C0 ; parm <- parm
   fit <- fkf(a0=rep(0,p),P0=C0,dt=array(0,dim=c(p,N)),
              ct=matrix(0,nrow=1,ncol=N),Tt=array(diag(p),dim=c(p,p,N)),Zt=Zt,
              HHt=array(0,dim=c(p,p,N)),
              GGt=array(matrix(parm[1],1,1),dim=c(1,1,N)),
              yt=yt)
   return(fit$logLik)
 }
reg.res.times[1,6] <- system.time(Reg.fkf.fit <- optim(par=InitialValues,fn=Reg.fkf.like,method="L-BFGS-B",
                         lower=0, upper=Inf,control=list(fnscale=-1,reltol=Tolerance)))[3]
Reg.fkf.model <- fkf(a0=rep(0,p),P0=C0,dt=array(0,dim=c(p,N)),
              ct=matrix(0,nrow=1,ncol=N),Tt=array(diag(p),dim=c(p,p,N)),Zt=Zt,
              HHt=array(0,dim=c(p,p,N)),
              GGt=array(matrix(Reg.fkf.fit$par,1,1),dim=c(1,1,N)),
              yt=yt)
betas.FKF <- Reg.fkf.model$att[,N]
#
#  Same with KFAS (proper initialization)
#
Reg.kfas.like <- function(parm) {
  yt <- yt ; p <- p ; Zt <- Zt
  fit <- kf(yt,Zt=Zt,Tt=diag(p),Rt=diag(p),Ht=matrix(parm[1],1,1),
            Qt=matrix(0,p,p),a1=rep(0,p), optcal=c(FALSE,FALSE,FALSE,FALSE), P1=C0)
   return(fit$lik)
 }
reg.res.times[1,7] <- system.time(Reg.kfas.fit <- optim(par=InitialValues,fn=Reg.kfas.like,
                                                        method="L-BFGS-B",lower=0, upper=Inf,
                                                        control=list(fnscale=-1,reltol=Tolerance)))[3]
Reg.kfas.model <-  kf(yt,Zt=Zt,Tt=diag(p),Rt=diag(p),Ht=matrix(Reg.kfas.fit$par,1,1),
            Qt=matrix(0,p,p),a1=rep(0,p), P1=C0)
betas.kfas <- Reg.kfas.model$at[,N+1]
#
#   With diffuse initialization
#
Reg.kfas.like.d <- function(parm) {
  yt <- yt ; p <- p ; Zt <- Zt
  fit <- kf(yt,Zt=Zt,Tt=diag(p),Rt=diag(p),Ht=matrix(parm[1],1,1),
            Qt=matrix(0,p,p),a1=rep(0,p), P1=0, P1inf=diag(p), optcal=c(FALSE,FALSE,FALSE,FALSE))
   return(fit$lik)
 }
reg.res.times[1,8] <- system.time(Reg.kfas.fit.d <- optim(par=InitialValues,fn=Reg.kfas.like,method="L-BFGS-B",
                      control=list(fnscale=-1,reltol=Tolerance)))[3]
Reg.kfas.model.d <-  kf(yt,Zt=Zt,Tt=diag(p),Rt=diag(p),Ht=matrix(Reg.kfas.fit.d$par,1,1),
            Qt=matrix(0,p,p),a1=rep(0,p), P1=0, P1inf=diag(p))
betas.kfas.d <- Reg.kfas.model.d$at[,N+1]
res.betas <- cbind(betas.MCO,betas.sspir,betas.dlm,betas.FKF,betas.kfas,betas.kfas.d)
res.sigma2 <- c(sigma2.MCO,exp(c(Reg.sspir.fit$par,
        Reg.dlm.fit$par,
        Reg.fkf.fit$par,
        Reg.kfas.fit$par,
        Reg.kfas.fit.d$par)))
reg.res.times[1,1:3] <- c(1,p,N)


###################################################
### chunk number 24: BefChfDemSetup
###################################################
set.seed(12345)
N   <- nrow(divisas)
#
#  Three currencies selected, Belgian franc, Swiss franc and Deutsche mark.
#
y   <- scale(log(as.matrix(divisas[,c(2,3,4)])),scale=FALSE,center=TRUE)
nv  <- ncol(y)
InitialValues <- c(rep(0.0001,nv),rep(1,nv-1),0.0001)
C0 <- diag(1)*10^2
m0 <- matrix(0,1,1)
#
#    Next function creates a matrix nv x nv with
#    (possibly different) variances "vars" 
#    in the main diagonal and covariances such that
#    the correlation coefficient for any two
#    variables is always "rho".
#
equicorrel <- function(nv,vars,rho) {
  sd <- sqrt(vars)
  m1 <- sd %o% sd
  m2 <- matrix(rho,nv,nv)
  diag(m2) <- 1
  return(m1*m2)
}

lowerlimits <- c(rep(0.000000001,2*nv))
upperlimits <- c(rep(8,2*nv))

#
#   Below, the single factor model is fitted. All
#   functions with sufix .like define likelihoods
#   as required by the different packages.
#


###################################################
### chunk number 25: DefChfDemSspir
###################################################
Curr.sspir.like  <- function(phi) {
   nv <- nv ; y <- y
   Fmat <- function(tt,x,phi) { return( matrix(c(1,phi[(nv+1):(2*nv-1)]),1,nv)) }
   Gmat <- function(tt,x,phi) { return( matrix(1,1,1)) }
   Wmat <- function(tt,x,phi) { return( matrix(phi[2*nv],1,1)) }
   Vmat <- function(tt,x,phi) { return( diag(phi[1:nv])) }
   SS.object <- sspir::SS(Fmat=Fmat,Gmat=Gmat,Vmat=Vmat,Wmat=Wmat,
                   m0=m0,C0=C0,y=y,phi=phi)
   aaa <- kfilter(SS.object)$loglik
   return(aaa)
 }


###################################################
### chunk number 26: BefChfDemDlm
###################################################
#
#  Currencies single factor model with dlm
#
Curr.dlm.like <- function(parm) {
  nv <- nv ; y <- y
  dlm(FF = matrix(c(1,parm[(nv+1):(2*nv-1)]),nv,1), GG=1, W= parm[2*nv], V=diag(parm[1:nv]),
      m0=as.vector(m0),C0=C0)
}


###################################################
### chunk number 27: BefChfDemFKF
###################################################
#
#  Currencies single factor model with FKF
#
yt <- t(y)
Curr.fkf.like <- function(parm) {
   yt <- yt
   nv <- nv 
   W <- matrix(parm[2*nv],1,1)
   V <- diag(parm[1:nv])
   fit <- fkf(a0=c(0),P0=C0,dt=matrix(0,nrow=1,ncol=1),
              ct=matrix(0,nrow=nv,ncol=1),Tt=matrix(1,1,1),Zt=matrix(c(1,parm[(nv+1):(2*nv-1)]),nv,1),
              HHt=W,GGt=V,yt=yt)
   return(fit$logLik)
}


###################################################
### chunk number 28: BefChfDemKFAS
###################################################
#
#  Currencies single factor model with KFAS
#
Curr.kfas.like <- function(parm) {
  yt <- yt
  nv <- nv
  W <- matrix(parm[2*nv],1,1)
  V <- diag(parm[1:nv])
  fit <- kf(yt=yt,Zt=matrix(c(1,parm[(nv+1):(2*nv-1)]),nv,1),Tt=matrix(1,1,1),Rt=matrix(1,1,1),Ht=V,Qt=W,
             a1=c(0), P1=C0, optcal=c(FALSE,FALSE,FALSE,FALSE))
   return(fit$lik)
 }
#
#   With diffuse initialization
#
Curr.kfas.like.d <- function(parm) {
  yt <- yt
  nv <- nv
  W <- matrix(parm[2*nv],1,1)
  V <- diag(parm[1:nv])
  fit <- kf(yt=yt,Zt=matrix(c(1,parm[(nv+1):(2*nv-1)]),nv,1),Tt=matrix(1,1,1),Rt=matrix(1,1,1),Ht=V,Qt=W,
             a1=c(0), P1=0, P1inf=matrix(1,1,1), optcal=c(FALSE,FALSE,FALSE,FALSE))
   return(fit$lik)
 }


###################################################
### chunk number 29: RunInParallel
###################################################
Curr.sspir.time <- system.time(Curr.sspir.fit <- 
                               optim(par=InitialValues,
                                     fn=Curr.sspir.like,
                                     method="L-BFGS-B",
                                     lower=lowerlimits,
                                     upper=upperlimits,
                                     control=list(fnscale=-1,parscale=InitialValues)))[3]
         
Curr.fkf.time <- system.time(Curr.fkf.fit <- 
                             optim(par=InitialValues, 
                                   lower=lowerlimits,
                                   upper=upperlimits,
                                   fn=Curr.fkf.like,method="L-BFGS-B",
                                   control=list(fnscale=-1,parscale=InitialValues)))[3]
         
Curr.kfas.time <- system.time(Curr.kfas.fit <- 
                              optim(par=InitialValues,
                                    lower=lowerlimits,upper=upperlimits,
                                    fn=Curr.kfas.like,
                                    method="L-BFGS-B",
                                    control=list(fnscale=-1,
                                    parscale=InitialValues)))[3]
        
Curr.kfas.time.d <- system.time(Curr.kfas.fit.d <- 
                                   optim(par=InitialValues,fn=Curr.kfas.like.d,method="L-BFGS-B",
                                          lower=lowerlimits,upper=upperlimits,
                                          control=list(fnscale=-1,parscale=InitialValues)))[3]
      
Curr.dlm.time <- system.time(Curr.dlm.fit <- 
                             dlmMLE(as.matrix(y), parm = InitialValues,
                                    method="L-BFGS-B",
                                    lower=lowerlimits,
                                    upper=upperlimits,
                                    build = Curr.dlm.like,
                                    control=list(parscale=InitialValues)))[3] 
                          
# collect(list(Curr.sspir.time,Curr.fkf.time,Curr.kfas.time,Curr.kfas.time.d,Curr.dlm.time))
Curr.dlm.estimated <- Curr.dlm.like(Curr.dlm.fit$par)
## Curr.dlm.filtered  <- dlmFilter(y=as.ts(y),mod=Curr.dlm.estimated)
## filtered <- zoo(as.vector(Curr.dlm.filtered$a) %o% as.vector(Curr.dlm.estimated$F),order.by=index(divisas))
## par(mfrow=c(2,2))
## for (j in 1:3) {
## z <- zoo(y[,j],order.by=index(divisas))
## plot(cbind(filtered[,j],z)[-1,],type="n",plot.type="single")
## lines(filtered[-1,j])
## lines(z[-1],col=2)
## }
## par(mfrow=c(1,1))


###################################################
### chunk number 30: CurrenciesExample
###################################################
#
#   Here we fit the local level equicorrelated model to 2, 3
#   and 4 currencies at a time. The code allows (by changing
#   "reps" and "nvs") to fit models with other numbers of
#   currencies. For instance, rep=5 would also fit the
#   equicorrelation model to sets of 8 and 12 currencies
#   (takes too long and gives convergence problems).
#
set.seed(12345)
reps <- 3
res   <- rep(0,8)
resultados <- vector("list",reps)
#
#   Code takes advantage of multiple cores if they exist;
#   next commented line is a remnant of former sequential
#   execution.
#
# for (i in 1:reps) {
resultados <- foreach(i=1:reps) %dopar% {
  nvs <- c(2,3,4,8,12)
  N   <- nrow(divisas)
  
nv <- nvs[i]                   # Number of currencies to consider
InitialValues <- c(rep(0.000002,2*nv),0.5)
C0 <- diag(nv)*10^5
m0 <- matrix(0,1,nv)
equicorrel <- function(nv,vars,rho) {
  sd <- sqrt(vars)
  m1 <- sd %o% sd
  m2 <- matrix(rho,nv,nv)
  diag(m2) <- 1
  return(m1*m2)
}   
y     <- log(divisas[,1:nv])
yt <- t(y)
lowerlimits <- c(rep(0.000001,2*nv),-0.99)
upperlimits <- c(rep(Inf,2*nv),+0.99)
#
#  Curr local level model with sspir
#
Curr2.sspir.like  <- function(phi) {
  nv <- nv
  y    <- y
  Fmat <- function(tt,x,phi) { return(diag(nv)) }
  Gmat <- function(tt,x,phi) { return(diag(nv)) }
  Wmat <- function(tt,x,phi) { return(diag(phi[(nv+1):(2*nv)])) }
  Vmat <- function(tt,x,phi) { equicorrel(nv,phi[1:nv],phi[2*nv+1]) }
  SS.object <- sspir::SS(Fmat=Fmat,Gmat=Gmat,Vmat=Vmat,Wmat=Wmat,
                  m0=m0,C0=C0,y=y,phi=phi)
  aaa <- kfilter(SS.object)$loglik
  print(aaa)
}
Curr2.sspir.time <- system.time(Curr2.sspir.fit <- 
                               try(optim(par=InitialValues,fn=Curr2.sspir.like,method="L-BFGS-B",
                                     lower=lowerlimits,upper=upperlimits,
                                     control=list(fnscale=-1,parscale=InitialValues))))[3]
#
#  Currencies local level model with dlm
#
buildFun <- function(parm) {
  nv <- nv
  Vvars <- parm[1:nv]
  Wvars <- parm[(nv+1):(2*nv)]
  rho  <-  parm[2*nv+1] 
  y    <- y
  dlm(FF = diag(nv), GG=diag(nv), W=diag(Wvars), V=equicorrel(nv,Vvars,rho),
      m0=as.vector(m0),C0=C0)
}
Curr2.dlm.time <- system.time(Curr2.dlm.fit <- 
                            try(dlmMLE(y, parm = InitialValues,method="L-BFGS-B",
                                   lower=lowerlimits,upper=upperlimits,
                                   build = buildFun,control=list(parscale=InitialValues))))[3]
Curr2.dlm.estimated <- buildFun(Curr2.dlm.fit$par)
## #
## #  Currencies local level model with FKF
## #
Curr2.fkf.like <- function(parm) {
   yt <- yt
   nv <- nv
   rho  <- parm[2*nv+1] 
   Vvars <- parm[1:nv]
   Wvars <- parm[(nv+1):(2*nv)]
   W <- diag(Wvars)
   V <- equicorrel(nv,Vvars,rho)
   fit <- fkf(a0=rep(0,nv),P0=C0,dt=matrix(0,nrow=nv,ncol=1),
              ct=matrix(0,nrow=nv,ncol=1),Tt=diag(nv),Zt=diag(nv),
              HHt=W,GGt=V,yt=yt)
   return(fit$logLik)
 }
Curr2.fkf.time <- system.time(Curr2.fkf.fit <- 
                             try(optim(par=InitialValues, lower=lowerlimits,upper=upperlimits,
                                   fn=Curr2.fkf.like,method="L-BFGS-B",
                                   control=list(fnscale=-1,parscale=InitialValues))))[3]
#
#  Currency local level model with KFAS
#
Curr2.kfas.like <- function(parm) {
  yt <- yt
  nv <- nv
  rho  <- parm[2*nv+1]  # / ( 1 +  abs(parm[2*nv+1]) )
  Vvars <- parm[1:nv]
  Wvars <- parm[(nv+1):(2*nv)]
  W <- diag(Wvars)
  V <- equicorrel(nv,Vvars,rho)
  fit <- kf(yt=yt,Zt=diag(nv),Tt=diag(nv),Rt=diag(nv),Ht=V,Qt=W,
             a1=rep(0,nv), P1=C0,optcal=c(FALSE,FALSE,FALSE,FALSE))
   return(fit$lik)
 }
Curr2.kfas.time <- system.time(Curr2.kfas.fit <- 
                              try(optim(par=InitialValues,
                                    lower=lowerlimits,upper=upperlimits,
                                    fn=Curr2.kfas.like,
                                    method="L-BFGS-B",
                                    control=list(fnscale=-1,
                                      parscale=InitialValues))))[3]
#
#   With diffuse initialization
#
Curr2.kfas.like.d <- function(parm) {
   yt <- yt
   nv <- nv
   rho  <- parm[2*nv+1]  #  / ( 1 +  abs(parm[2*nv+1]) )
   Vvars <- parm[1:nv]
   Wvars <- parm[(nv+1):(2*nv)]
   W <- diag(Wvars)
   V <- equicorrel(nv,Vvars,rho)
   fit <- kf(yt=yt,Zt=diag(nv),Tt=diag(nv),Rt=diag(nv),Ht=V,Qt=W,
             a1=rep(0,nv), P1=0, P1inf=diag(nv), optcal=c(FALSE,FALSE,FALSE,FALSE))
   return(fit$lik)
 }
Curr2.kfas.time.d <- system.time(Curr2.kfas.fit.d <- 
                                   try(optim(par=InitialValues,fn=Curr2.kfas.like.d,method="L-BFGS-B",
                                          lower=lowerlimits,upper=upperlimits,
                                          control=list(fnscale=-1,parscale=InitialValues))))[3]
#
res[1] <- nv
res[2] <- nv
res[3] <- N
res[4] <- Curr2.sspir.time
res[5] <- Curr2.dlm.time
res[6] <- Curr2.fkf.time
res[7] <- Curr2.kfas.time
res[8] <- Curr2.kfas.time.d
#
#  What each iteration of the loop returns is a list with:
#   1) The dimensions of the model and the times taken by
#      each package.
#   2) The fitted parameters by MLE (in Curr2.*.fit) as
#      computed with each package.
#  The list is called scratch; the sentence "foreach" takes
#  care to collect the results of its parallel runs in a list,
#  so after "foreach" exits, scratch is a list of lists, named
#  "resultados".
#
scratch <-  list(res=res,sspir=Curr2.sspir.fit,dlm=Curr2.dlm.fit,
     fkf=Curr2.fkf.fit,kfas=Curr2.kfas.fit,
     kfas.d=Curr2.kfas.fit.d)
scratch
}



###################################################
### chunk number 31: ExtraerResults
###################################################
#
#   Here we collect the results obtained in the last
#   chunk and those previously obtained for the single
#   factor model.
#
 results <- matrix(0,4,8)
 for(i in 1:length(resultados)) {
   results[i,] <- (resultados[[i]])$res
 }
#
#  Now results from the single factor model
#
results[4,] <- c(3,1,N,Curr.sspir.time,Curr.dlm.time,Curr.fkf.time,
                 Curr.kfas.time,Curr.kfas.time.d)
#
#   The matrix of times "results" is not used further
#   in this R script, but is included in the paper
#   vía \Sexpr{} to form one of the tables.
#

###################################################
### chunk number 32: SspirNa
###################################################
#
#   This code writes some figures in one of the tables.
#
cat("& ")
if (!is.numeric(reg.res.times[1,4]) ) {
    cat("\\multicolumn{1}{c}{---}\n")
} else  {
    cat(sprintf("%4.2f",reg.res.times[1,4]))
    cat("\n")
  }


###################################################
### chunk number 33: StructTS
###################################################
fit <- StructTS(Nile,type="level")


###################################################
### chunk number 34: RawRunningTimes
###################################################
#
#   The following code computes the times which serve
#   as the basis for Figure 2.
#
d <- c(2, 4, 8, 10)        #  Obs. dimensions
p <- c(2, 4, 8, 10, 12)    #  State dimensions
N <- 100 + 200*(1:9)       #  Sample lengths
reps  <- 25                #  Replications for each model
                           #  (times computed as the average of
                           #   all replications).
speed <- expand.grid(p=p,d=d,N=NA,dse=NA,dlm=NA,FKF=NA,KFAS=NA)
l     <- 0
scratch <- expand.grid(p=p,d=d,N=NA,dse=NA,dlm=NA,FKF=NA,KFAS=NA)
#
#   The list "res" ends up with the timings which are then used
#   to produce the graph. "foreach" runs things in parallel and
#   collects results bu "rbinding" them.
#
res <- foreach(i=N, .combine=rbind) %dopar% {
  gc()
  speed <- scratch
  for (j in p) {
    y <- matrix(rnorm(i*j),i,j) # coredata(divisas)[1:i,1:j]
    for (k in d) {
      l <- l + 1
      speed[l,c("p","d","N")] <- c(j,k,i)
      #
      #  Set up system matrices
      #
      TM <- diag(k)                              # Transition matrix.
      OM <- diag(max(j,k))[1:j,1:k,drop=FALSE]   # Observation matrix
      OM[,1] <- 1
      NS <- diag(k)                              # Noise state equation
      NO <- diag(j)                              # Noise observation equation
      c0 <- rep(0.0,k)                           # Initial state
      C0 <- 10^7*diag(k)                         # Initial covariance state
      #
      #   Construct model for dse
      #
      mod   <- dse::SS(F=TM, Q=NS, H=OM, R=NO, z0=c0, P0=C0)
      datos <- TSdata(output=y)
      speed[l,"dse"] <- system.time( 
                                    for (r in 1:reps) {
                                      invisible(l(mod,datos,compiled=TRUE))
                                    }
                                    )[1] / reps
      #
      #   Construct model for dlm
      #
      mod   <- dlm(FF=OM, GG=TM, V=NO, W=NS, m0=c0,C0=C0)
      speed[l,"dlm"] <- system.time( 
                                    for (r in 1:reps) {
                                      invisible(dlmFilter(y,mod,debug=FALSE,simplify=TRUE))
                                    }
                                    )[1] / reps
      #
      #   Construct model for FKF
      #
      yt <- t(y)
      dt <- rep(0,k)
      ct <- rep(0,j)
      speed[l,"FKF"] <- system.time( 
                                    for (r in 1:reps) {
                                      invisible(fkf(a0=c0, P0=C0, dt=dt, ct=ct,
                                                     Tt=TM, Zt=OM, HHt=NS, GGt=NO,
                                                     yt=yt))
                                    }
                                    )[1] / reps
      #
      #  Construct model for KFAS
      #
      speed[l,"KFAS"] <- system.time( 
                                    for (r in 1:reps) {
                                      invisible(kf(yt=yt, Zt=OM, Tt=TM, Rt=NS, 
                                                    Qt=NS, Ht=NO, a1=c0, P1=C0,
                                                    optcal=c(FALSE,FALSE,FALSE,FALSE)
                                                    ))
                                    }
                                    )[1] / reps    
    }
  }
  speed
}


###################################################
### chunk number 35: ComparisonFigure
###################################################
pdf(file="Comp.pdf",paper="special",height=10.7,width=8)
strip.combined <- function(which.given, which.panel, factor.levels, ...) {
  if (which.given == 1) {
    panel.rect(0, 0, 1, 1, col = "#ffe5cc", border = 1)
    panel.text(x = 0, y = 0.5, pos = 4,
               lab = paste("d=",factor.levels[which.panel[which.given]],sep=""))
  }
  if (which.given == 2) {
    panel.text(x = 1, y = 0.5, pos = 2,
               lab = paste("p=",factor.levels[which.panel[which.given]],sep=""))
  }
}
bbb <- xyplot( dse + dlm + FKF + KFAS ~ N | factor(d)*factor(p) ,
              data=res,
              scales=list(y=list(relation="same",log=TRUE,at=c(0.01,0.05,0.10,0.30)),
                cex=0.8,alternating=TRUE),
              type="l",
              auto.key=list(columns=4,lines=TRUE,points=FALSE,space="bottom"),
              xlab="N", strip=strip.combined, par.strip.text = list(lines = 0.5),
              layout=layout,
              ylab="Seconds (log scale)") 
print(bbb)
dev.off()


###################################################
### chunk number 36: ResultsBetas
###################################################
res.betas <- cbind(1:5,res.betas)


###################################################
### chunk number 37: EstimationComparisonSingleFactorModel
###################################################
aaa <- cbind(Curr.sspir.fit$par,Curr.dlm.fit$par,Curr.fkf.fit$par,Curr.kfas.fit$par)
prop.fac <- c(10^5,10^3,10^6,1,1,10^5)
bbb <- matrix(0,4,4)
y <- as.matrix(scale(log(as.matrix(divisas[,c(2,3,4)])),scale=FALSE,center=TRUE))
for (i in 1:4) {
  bbb[1,i] <- Curr.sspir.like(aaa[,i])   
  bbb[2,i] <- dlmLL(y=y,mod=Curr.dlm.like(aaa[,i]),debug=FALSE)
  bbb[3,i] <- Curr.fkf.like(aaa[,i])
  bbb[4,i] <- Curr.kfas.like(aaa[,i])
  res.thetas <- rbind(diag(prop.fac) %*% aaa,bbb)
}


