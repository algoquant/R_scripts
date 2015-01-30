# from:
# https://github.com/erbas/ApticReports/blob/master2/R%20src/AdaptiveAssetAllocation.R
# Feb 13, 2014 First optimisation efforts

library(quantmod)
library(DEoptim)
library(PerformanceAnalytics)
library(BurStFin)
library(quadprog)

#-------------------------------------------------------------------------
# load equity data
#-------------------------------------------------------------------------

sp500 <- read.csv("sp500.csv",stringsAsFactors=FALSE)

syms <- sp500$Ticker.symbol

stock.data <- new.env()
for (x in syms) {
  print(x)
  if (exists(x)) next
  try(getSymbols(x,from='2001-01-02',env=stock.data),silent=FALSE)
}

getSymbols("SPY",from='2001-01-02',env=stock.data)
sizes <- sapply(stock.data,nrow)
valid.set <- names(sizes[sizes>3000])

save(stock.data,file="sp500_prices.RData")



hist.prices <- NULL
for (x in valid.set) {
  print(x)
  hist.prices <- merge(Ad(stock.data[[x]]),hist.prices,all=TRUE)
}
dim(hist.prices)

colnames(hist.prices) = gsub(".Adjusted","",names(hist.prices))
plot.zoo(hist.prices,plot.type='single',col=rainbow(ncol(hist.prices)))


#-------------------------------------------------------------------------
#  objective functions
#-------------------------------------------------------------------------

obj.min.var <- function(w,rtns,cov.mat) {
  # portfolio covariance
  p.cov <- t(w) %*% cov.mat %*% w
  constr <- length(w)^2*(sum(w)-1)^2   # weights sum to one
  return(p.cov + constr)  
}

obj.var.rtns <- function(w,rtns,cov.mat) {
  # variance of returns - note cov.mat in params only for consistency
  p.rtns <- rtns %*% w
  p.var <- sd(p.rtns)
  constr <- length(w)^2*(sum(w)-1)^2   # weights sum to one
  return(p.var + constr)  
}

obj.capm <- function(w,rtns,cov.mat) {
  # maximise returns while minimising variance
  mu <- sum(rtns %*% w)
  p.cov <- t(w) %*% cov.mat %*% w
  constr <- length(w)^2*(sum(w)-1)^2   # weights sum to one
  return(constr - mu + 0.5*p.cov)  
}

obj.sharpe <- function(w,rtns,cov.mat) {
  # maximise returns while minimising variance
  mu <- sum(rtns %*% w)
  p.cov <- t(w) %*% cov.mat %*% w
  constr <- length(w)^2*(sum(w)-1)^2   # weights sum to one
  return(constr - mu/p.cov)  
}

obj.geom <- function(w,rtns,cov.mat) {
  # objective for Kelly criteria, ie geometric mean maximisation
  mu <- sum(rtns %*% w)
  a <- 1 + mu
  b <- t(w) %*% cov.mat %*% w
  A <- log(a) - b/(2*a^2) 
  constr <- length(w)^2*(sum(w)-1)^2 
  return(constr - A)  # note we ignore the exp for maximisation purposes
}
  

#-------------------------------------------------------------------------
# portfolio optimisation wrappers
#-------------------------------------------------------------------------

ptf.optim <- function(rtns,fn=obj.capm,meth='pearson') {
  n <- ncol(rtns)
  w <- rep(1/n,n) + runif(n,-0.1/n,0.1/n)
  cov.mat <- cov(rtns,method=meth)
  cor.mat <- cov2cor(cov.mat)
  ctrl <- list(maxit=1.e3,reltol=1.e-8,trace=3)
  opt.obj <- optim(w,fn,gr=NULL,rtns,cor.mat, method="L-BFGS-B",lower=0,upper=1,control=ctrl)
  print(opt.obj$message)
  w.final <- opt.obj$par
  return(w.final/sum(w.final))
}

ptf.nlminb <- function(rtns,fn=obj.capm,meth='pearson') {
  n <- ncol(rtns)
  w <- rep(1/n,n) + runif(n,-0.1/n,0.1/n)
  cov.mat <- cov(rtns,method=meth)
  #cor.mat <- cov2cor(cov.mat)
  cntrl <- list(eval.max=1.e3,iter.max=1.e3,rel.tol=1.e-8,trace=1)
  opt.obj <- nlminb(w,fn,gradient=NULL,hessian=NULL,rtns,cov.mat,control=cntrl,lower=0,upper=1)
  print(opt.obj$message)
  w.final <- opt.obj$par
  return(w.final/sum(w.final))
}

ptf.nlminb.shrink <- function(rtns,fn=obj.capm) {
  n <- ncol(rtns)
  w <- rep(1/n,n) + runif(n,-0.1/n,0.1/n)
  cov.mat <- var.shrink.eqcor(rtns,shrink=NULL,vol.shrink=0.0,tol=1.e-4)
  #cor.mat <- cov2cor(cov.mat)
  cntrl <- list(eval.max=1.e3,iter.max=1.e3,rel.tol=1.e-8,trace=1)
  opt.obj <- nlminb(w,fn,gradient=NULL,hessian=NULL,rtns,cov.mat,control=cntrl,lower=0,upper=1)
  print(opt.obj$message)
  w.final <- opt.obj$par
  return(w.final/sum(w.final))
}

ptf.qp <- function(rtns,obj="capm",V=cov(rtns)) {
  stopifnot(obj %in% c("capm","minvar"))
  n <- ncol(rtns)
  stopifnot(dim(V)==c(n,n))
  # calculate appropriate covariance
  # calculate appropriate mean
  if (obj == "capm") {
    mu <- colSums(rtns)
  } else if (obj == "minvar") {
    mu = rep(0,n)
  }
  A <- cbind(                 # One constraint per column
    matrix( rep(1,n), nr=n ), # The weights sum up to 1
    diag(n)                   # No short-selling
  )
  b <- c(1, rep(0,n))
  w <- solve.QP(V, mu, A, b, meq=1) 
  return(w$solution)
}


ptf.deopt <- function(rtns,fn=obj.capm,meth='pearson') {
  n <- ncol(rtns)
  NP <- 10*n
  w <- matrix(runif(n*NP,min=1.e-3,max=1),nrow=NP,ncol=n)
  w <- apply(w,1,function(x) x/sum(x))
  cov.mat <- cov(rtns,method=meth)
#   cor.mat <- cov2cor(cov.mat)
  ctrl <- DEoptim.control(itermax=50000,trace=200,strategy=6,reltol=1.e-5, steptol=200,c=0.1,p=0.3,initialpop=w,NP=NP)
  opt.obj <- DEoptim(fn,lower=rep(0,n),upper=rep(1,n),ctrl,rtns,cov.mat)
  return(opt.obj$optim$bestmem)
}

#-------------------------------------------------------------------------
#  tests of optimisation routines
#-------------------------------------------------------------------------

n.cov <- 20
n.mom <- 20
n.top <- 5

daily.rtns <- ROC(hist.prices,1)
mom <- ROC(hist.prices,n.mom)

rr <- tail(daily.rtns[1200:1300,],n.cov)
rr.mom <- tail(mom[index(rr)],n.cov)
idx <- order(tail(rr.mom,1),decreasing=TRUE)[1:n.top]
colnames(hist.prices)[idx]

plot.zoo(rr[,idx],plot.type='single',col=rainbow(n.top))

svd(cov(rr[,idx],method='pearson'))$d

w0 <- ptf.optim(rr[,idx],fn=obj.sharpe,meth='pearson')
w1 <- ptf.nlminb(rr[,idx],fn=obj.sharpe,meth='pearson')
w2 <- ptf.nlminb(rr[,idx],fn=obj.sharpe,meth='spearman')
w3 <- ptf.nlminb.shrink(rr[,idx],fn=obj.sharpe)
t(cbind(w0,w1,w2,w3))

w0 <- ptf.optim(rr[,idx],fn=obj.capm,meth='pearson')
w1 <- ptf.nlminb(rr[,idx],fn=obj.capm,meth='pearson')
#w2 <- ptf.deopt(rr[,idx],fn=obj.capm,meth='spearman')
w2 <- ptf.nlminb(rr[,idx],fn=obj.geom,meth='pearson')
w3 <- ptf.nlminb.shrink(rr[,idx],fn=obj.capm)
w4 <- ptf.qp.capm(rr[,idx])
t(cbind(w0,w1,w2,w3,w4))

w0 <- ptf.optim(rr[,idx],fn=obj.min.var,meth='pearson')
w1 <- ptf.nlminb(rr[,idx],fn=obj.min.var,meth='pearson')
#w2 <- ptf.deopt(rr[,idx],fn=obj.min.var,meth='pearson')
w2 <- ptf.nlminb.shrink(rr[,idx],fn=obj.min.var)
w3 <- ptf.qp.minvar(rr[,idx])
t(cbind(w0,w1,w2,w3))

v0 <- ptf.optim(rr[,idx],fn=obj.min.var,meth='spearman')
v1 <- ptf.nlminb(rr[,idx],fn=obj.min.var,meth='spearman')
v2 <- ptf.nlminb(rr[,idx],fn=obj.var.rtns,meth='spearman')
cbind(v0,v1,v2)

#-------------------------------------------------------------------------
# portfolio optimisation a.k.a. asset allocation
#-------------------------------------------------------------------------
n.cov <- 120
n.mom <- 120
n.top <- 10
my.hist.prices <- na.omit(hist.prices)
rebalance <- "weeks"
period.ends <- endpoints(my.hist.prices,rebalance) 

daily.rtns <- ROC(my.hist.prices,1)
daily.rtns[is.na(daily.rtns)] <- 0
mom <- ROC(my.hist.prices,n.mom)
mom[is.na(mom)] <- 0

period.rtns <- ROC(my.hist.prices[period.ends,])
period.rtns[is.na(period.rtns)] <- 0

zmat <- xts(matrix(0,ncol=ncol(period.rtns),nrow=nrow(period.rtns)), index(period.rtns))
wts.aaa <- zmat
wts.capm <- zmat
wts.capm.all <- zmat
wts.kelly <- zmat
wts.aaa.shrink <- zmat
wts.capm.shrink <- zmat
wts.minvar <- zmat

z <- Sys.time()
for (i in period.ends[period.ends > n.cov]) {
  k <- which(period.ends==i)
  # find largest movers, in n.mom sense
  top.mom <- order(mom[i-1,],decreasing=TRUE)[1:n.top]
  rr.all <- daily.rtns[(i-n.cov):(i-1),]
  rr <- rr.all[,top.mom]
  # find weights for high momentum names
#   wts.aaa[k,top.mom] <- ptf.nlminb(rr,fn=obj.min.var,meth='pearson')
#   wts.capm[k,top.mom] <- ptf.nlminb(rr,fn=obj.capm,meth='pearson')
#   wts.aaa.shrink[k,top.mom] <- ptf.nlminb.shrink(rr,fn=obj.min.var)
#   wts.capm.shrink[k,top.mom] <- ptf.nlminb.shrink(rr,fn=obj.capm)
#   # find weights over all names
#   wts.minvar[k,] <- ptf.nlminb.shrink(rr.all,fn=obj.min.var)
#   wts.capm.all[k,] <- ptf.nlminb.shrink(rr.all,fn=obj.capm)
#   wts.kelly[k,] <- ptf.nlminb.shrink(rr.all,fn=obj.geom)
  cov.mat <- cov(rr)
  wts.aaa[k,top.mom] <- ptf.qp(rtns=rr,obj="minvar",V=cov.mat)
  wts.capm[k,top.mom] <- ptf.qp(rtns=rr,obj="capm",V=cov.mat)
  cov.mat.shrink <- var.shrink.eqcor(rr)
  wts.aaa.shrink[k,top.mom] <- ptf.qp(rtns=rr,obj="minvar",V=cov.mat.shrink)
  wts.capm.shrink[k,top.mom] <- ptf.qp(rtns=rr,obj="capm",V=cov.mat.shrink)
  cov.mat.shrink.all <- var.shrink.eqcor(rr.all)
  wts.minvar[k,] <- ptf.qp(rtns=rr.all,obj="minvar",V=cov.mat.shrink.all)
  wts.capm.all[k,] <- ptf.qp(rtns=rr.all,obj="capm",V=cov.mat.shrink.all)
  print(index(daily.rtns[i,]))
}
Sys.time() - z

aaa <- xts(rowSums(wts.aaa * period.rtns),index(period.rtns))
capm <- xts(rowSums(wts.capm * period.rtns),index(period.rtns))
kelly <- xts(rowSums(wts.kelly * period.rtns),index(period.rtns))
aaa.shrink <- xts(rowSums(wts.aaa.shrink * period.rtns),index(period.rtns))
capm.shrink <- xts(rowSums(wts.capm.shrink * period.rtns),index(period.rtns))
capm.all <- xts(rowSums(wts.capm.all * period.rtns),index(period.rtns))
minvar <- xts(rowSums(wts.minvar * period.rtns),index(period.rtns))

buy.hold <- period.rtns$SPY
colnames(buy.hold) <- "buy.hold SPY"
oracle.long.spy <- ifelse(buy.hold >0,buy.hold,0)
oracle.spy <- ifelse(buy.hold >0,buy.hold,-buy.hold)
colnames(oracle.long.spy) <- "oracle.long.spy"
colnames(oracle.spy) <- "oracle.spy"

# results <- merge(aaa,capm,kelly,aaa.shrink,capm.shrink,capm.all,minvar,all=TRUE) #- 1.e-3
results <- merge(aaa,capm,aaa.shrink,capm.shrink,capm.all,kelly,minvar,all=TRUE) #- 1.e-3
results <- merge(buy.hold,results,all=FALSE)

charts.PerformanceSummary(results["2002::"],geometric=F,wealth.index=1,main=paste(rebalance,"n.cov", n.cov, "n.mom", n.mom,"n.top", n.top,sep=" "))

table.AnnualizedReturns(results["2002::"],scale=12,geometric=F,digits=3)
table.Drawdowns(results["2010::",1])
table.Drawdowns(results["2010::",2])
table.Drawdowns(results["2010::",3])
table.Drawdowns(results["2010::",4])
table.Drawdowns(results["2010::",5])
table.Drawdowns(results["2010::",6])
table.Drawdowns(results["2010::",8])


table.DownsideRisk(results["2006::",-7],scale=52,MAR=0.05,digits=3)
table.CAPM(results["2007::"],buy.hold)
table.CalendarReturns(results["2002::",-7],digits=1,as.perc=T,geometric=F)


# unused symbols...
colnames(hist.prices)[which(apply(wts.kelly,2,max) < 0.01)]

cc <- c('black',rainbow(ncol(results)-1))
plot.zoo(results["2010::"],plot.type='single',col=cc,lwd=2)
legend('topleft',legend=colnames(results),lty=1,cex=0.6,col=cc,lwd=2)
