library(lubridate)
library(quantmod)
library(plyr)

rm(list = ls())

##############################################################################################

####################
### GET THE DATA ###
####################

v <- c("XLE","XLU","XLK","XLB","XLP","XLY","XLI","XLV","XLF")
t1 <- "1996-01-01"
P.list <- lapply(v,function(x) get(getSymbols(x,from = t1)) )
P <- lapply(P.list,function(x) x[,grep("Adjusted",names(x))])
P <- na.omit(Reduce(function(...) merge(..., all = T),P  ))
names(P) <- v
P <- apply.monthly(P,function(x) x[nrow(x),] )
R <- P/lag(P) - 1; R <- R[-1,];
R <- R[date(R) < "2017-10-01",]

# rolling window example
plot(R[,"XLF"])
lines(rollapply(R[,"XLF"],20,sd), col = 2)

###########################
### BUDGET CONSTRAINTS ####
###########################

# CONSTRAINTS FOR SUM TO ONE
d <- ncol(R)
A <- matrix(1,1,d)
A <- rbind(A,-A)
B <- c(0.999,-1.001)

# SHORT-SALES CONSTRAINTS
A2 <- diag(rep(1,d))
B2 <- rep(0,d)
A2 <- rbind(A,A2)
B2 <- c(B,B2)

# STACK CONSTRAINTS IN A LIST FOR LATER ON
BC1 <- list(A,B)
BC2 <- list(A2,B2)


##############################################################################################

################################
### DYNAMIC ASSET ALLOCATION ###
################################

# recall that 
gmv_portfolio <- function(S,BC) {
  # check for any missing values due to rolling window
  if (any(is.na(S))) return(rep(NA,ncol(S)))
  
  d <- ncol(S) # number of assets
  
  # objective function
  f <- function(X) c(t(X)%*%S%*%X)
  g <- function(X) 2*S%*%X # and gradient
  X0 <- rep(1/d,d) # an initial guess
  
  # use budget constraints
  A <- BC[[1]]
  B <- BC[[2]]
  
  # use constrained optimization
  X_gmv <- constrOptim(X0,f,g,ui = A,ci = B)$par
  return(X_gmv)
}

N <- 100
# covariance matrix on a rolling window
cov_roll <- rollapply(R,N, function(x) as.vector(var(x)), by.column = F, align = "right")
cov_roll <- alply(cov_roll, 1, function(x) matrix(x, nrow = ncol(R) )   )
names(cov_roll) <- date(R)

# look at correlation over time
cor_roll <- rollapply(R,N, function(x) as.vector(cor(x)), by.column = F, align = "right")
cor_roll <- alply(cor_roll, 1, function(x) matrix(x, nrow = ncol(R) )   )
names(cor_roll) <- date(R)
cor_mean <- na.omit(as.xts(sapply(cor_roll, function(x) mean(x[upper.tri(x)]) )))
cor_min <- na.omit(as.xts(sapply(cor_roll, function(x) min(x[upper.tri(x)]) )))

plot(cor_mean, ylim = range(c(cor_mean,cor_min))   )
lines(cor_min, col = 2)

# GMV
X_gmv1 <- t(sapply(cov_roll, function(S)  gmv_portfolio(S,BC1)))
X_gmv2 <- t(sapply(cov_roll, function(S)  gmv_portfolio(S,BC2)))
colnames(X_gmv1) <- colnames(X_gmv2)<- names(R)

X_gmv1 <- as.xts(X_gmv1)
X_gmv2 <- as.xts(X_gmv2)

round(tail(X_gmv1*100,1),2)
round(tail(X_gmv2*100,1),2)

plot(na.omit(X_gmv1[,"XLF"]))
abline(h = 0, lty = 2, col = 2)

##############################################################################################

####################################
######## BACKTESTING ###############
####################################
R_test <- tail(R,nrow(R) - N + 1)

X_gmv1 <- X_gmv1[date(R_test),]
X_gmv2 <- X_gmv2[date(R_test),]

R_test <- R_test[-1,]
X_gmv1 <- X_gmv1[-nrow(X_gmv1),]
X_gmv2 <- X_gmv2[-nrow(X_gmv2),]

start(X_gmv1);end(X_gmv1)
start(R_test);end(R_test)

ret_gmv1 <- sapply(1:nrow(R_test), function(i) (X_gmv1[i,]) %*% t(R_test[i,]) )
ret_gmv2 <- sapply(1:nrow(R_test), function(i) (X_gmv2[i,]) %*% t(R_test[i,]) )
ret_naive <- apply(R_test,1,mean)

y1_plot <- cumprod(ret_gmv1+1)
y2_plot <- cumprod(ret_gmv2+1)
y3_plot <- cumprod(ret_naive+1)
x_plot <- date(R_test)

plot(y1_plot ~ x_plot , type = "l", ylab = "",xlab = "", main = "Cumulative Return", ylim = range(c(y1_plot,y2_plot,y3_plot))   )
lines(y2_plot~x_plot , col  = 2)
lines(y3_plot~x_plot , col  = 3) # equal weighting requires balancing as well
legend( "topleft",c("GMV","GMV without short-sales","Naive"),col = 1:3, lty = rep(1,3))
grid(20)

###########################
### SUMMMARY STATISTICS ###
###########################

portfolio_ret <- list(ret_gmv1,ret_gmv2,ret_naive)
summary_ret <- function(x) c(100*mean(x)*12,100*sd(x)*sqrt(12),(mean(x)/sd(x))*sqrt(12))

# summarize returns
M <- round(sapply(portfolio_ret,summary_ret),2)
colnames(M) <- c("GMV","GMV_no_short","Naive")

# summarize TO for GMV
portfolio_weights <- list(X_gmv1,X_gmv2)
TO_f <- function(X) apply(abs(as.matrix(X[-1,]) - as.matrix(X[-nrow(X),])),1,sum)
TO_list <- lapply(portfolio_weights,TO_f)

# NAIVE TO
TO_naive <- apply(R_test[-nrow(R_test),],1,function(x)  sum(abs((x+1)/sum(x+1) - 1/length(x)))  )     
TO_list[3] <- list(TO_naive)

TO <- sapply(TO_list,mean)*100
M <- rbind(M,TO)

# summarize TC
TC_f <- function(TC,i) {
  R_i <- portfolio_ret[[i]][-1] - TO_list[[i]]*TC
  return(mean(R_i)/sd(R_i))
  }

# solve for TC that makes it equal
TC1 <- uniroot(function(TC) TC_f(TC,1)  - TC_f(TC,3) ,c(-1,1))$root
TC2 <- uniroot(function(TC) TC_f(TC,2)  - TC_f(TC,3),c(-1,1))$root
TC <- c(TC1,TC2,NA)*100

# finally summarize results altogether
M <- rbind(M,TC)
rownames(M) <- c("Mean","Std","SR","TO","TC")
round(M,2)

##########################################################################################

###################
### ROBUSTNESS ####
###################

backtesting_N <- function(N) {
  if(N > 100) stop("sample size should not be greater than 100")

  # covariance matrix on a rolling window
  cov_roll <- rollapply(R,N, function(x) as.vector(var(x)), by.column = F, align = "right")
  cov_roll <- alply(cov_roll, 1, function(x) matrix(x, nrow = ncol(R) )   )
  names(cov_roll) <- date(R)

  # GMV
  X_gmv1 <- t(sapply(cov_roll, function(S)  gmv_portfolio(S,BC1)))
  X_gmv2 <- t(sapply(cov_roll, function(S)  gmv_portfolio(S,BC2)))
  colnames(X_gmv1) <- colnames(X_gmv2)<- names(R)

  X_gmv1 <- as.xts(X_gmv1)
  X_gmv2 <- as.xts(X_gmv2)
  
  R_test <- tail(R,nrow(R) - 100 + 1)
  X_gmv1 <- X_gmv1[date(R_test),]
  X_gmv2 <- X_gmv2[date(R_test),]
  R_test <- R_test[-1,]
  X_gmv1 <- X_gmv1[-nrow(X_gmv1),]
  X_gmv2 <- X_gmv2[-nrow(X_gmv2),]

  ret_gmv1 <- sapply(1:nrow(R_test), function(i) (X_gmv1[i,]) %*% t(R_test[i,]) )
  ret_gmv2 <- sapply(1:nrow(R_test), function(i) (X_gmv2[i,]) %*% t(R_test[i,]) )
  ret_naive <- apply(R_test,1,mean)

  portfolio_ret <- list(ret_gmv1,ret_gmv2,ret_naive)
  summary_ret <- function(x) c(100*mean(x)*12,100*sd(x)*sqrt(12),(mean(x)/sd(x))*sqrt(12))

  # summarize returns
  M <- round(sapply(portfolio_ret,summary_ret),2)
  colnames(M) <- c("GMV","GMV_no_short","Naive")

  # summarize TO for GMV
  portfolio_weights <- list(X_gmv1,X_gmv2)
  TO_f <- function(X) apply(abs(as.matrix(X[-1,]) - as.matrix(X[-nrow(X),])),1,sum)
  TO_list <- lapply(portfolio_weights,TO_f)

  # NAIVE TO
  TO_naive <- apply(R_test[-nrow(R_test),],1,function(x)  sum(abs((x+1)/sum(x+1) - 1/length(x)))  )     
  TO_list[3] <- list(TO_naive)

  TO <- sapply(TO_list,mean)*100
  M <- rbind(M,TO)

  # summarize TC
  TC_f <- function(TC,i) {
    R_i <- portfolio_ret[[i]][-1] - TO_list[[i]]*TC
    return(mean(R_i)/sd(R_i))
  }
  
  # solve for TC that makes it equal
  TC1 <- uniroot(function(TC) TC_f(TC,1)  - TC_f(TC,3) ,c(-1,1))$root
  TC2 <- uniroot(function(TC) TC_f(TC,2)  - TC_f(TC,3),c(-1,1))$root
  TC <- c(TC1,TC2,NA)*100
  
  # finally summarize results altogether
  M <- rbind(M,TC)
  rownames(M) <- c("Mean","Std","SR","TO","TC")
  
  return(M)
  }

# finally the above function for different Ns
N_seq <- seq(20,100,by = 5)
backtest_results <- lapply(N_seq,backtesting_N)

SR2 <- sapply(backtest_results , function(x) x["SR","GMV_no_short"])
TC2 <- sapply(backtest_results , function(x) x["TC","GMV_no_short"])

plot(SR2~N_seq, pch = 20, ylab = "SR", xlab = "N")
lines(SR2~N_seq)

par(new = T)
plot(TC2~N_seq, pch = 15, col = 2, ylab = NA,xlab = NA,axes = F)
lines(TC2~N_seq, col = 2)
axis(side = 4)
mtext(side = 4, line = 0.5, 'TC')
legend("topleft",c("SR","TC"), col = 1:2,pch = c(20,15))
dev.off()

