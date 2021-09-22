library(lubridate)
library(quantmod)

rm(list = ls())

####################
### GET THE DATA ###
####################

v <- c("XLE","XLU","XLK","XLB","XLP","XLY","XLI","XLV","XLF")
t1 <- "1996-01-01"
P.list <- lapply(v,function(x) get(getSymbols(x,from = t1)) )
P <- lapply(P.list,function(x) x[,grep("Adjusted",names(x))])
P <- Reduce(function(...) merge(...),P  )
names(P) <- v
P <- apply.monthly(P,function(x) x[nrow(x),] )
R <- na.omit(P/lag(P)-1)
R <- R[date(R) < "2017-07-01",]

# in-sample and out-of-sample
in_index <- 1:floor(nrow(R)/2)
out_index <- (1:nrow(R))[!1:nrow(R) %in% in_index]
R_in <- R[in_index,]
R_out <- R[out_index,]

M1 <- apply(R_in,2,mean)
M2 <- apply(R_out,2,mean)
mean((M1-M2)^2)*100

S1 <- var(R_in);
S2 <- var(R_out)
mean((S1 - S2)^2)*100


#####################################
### SOLVING FOR THE MEAN-VARIANCE ###
#####################################

# EXPECTED UTILITY
EU <- function(X,M,S,k) c(t(X)%*%M) - 0.5*k*c(t(X)%*%S%*%X)

# need to start with an initial portfolio weights
X0 <- rep(1/d,d)
# which also satisfies the BC
all(A%*%X0 >= B)
all(A2%*%X0 >= B2)



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

# CHECK CONSTRAINTS
X0 <- rep(1/d,d)
all(A%*%X0 >= B)
all(A2%*%X0 >= B2)

MV_portfolio <- function(M,S,BC,k) {
  eps <- 10^-3
  f <- function(x) -EU(x,M,S,k)
  g <- function(X) -M + k*S%*%X
  A <- BC[[1]]
  B <- BC[[2]]
  X1 <- constrOptim(X0,f,grad = g,ui = A,ci = B)$par
  return(X1)
}


# FUNCTION FOR GMV PORTFOLIO
gmv_portfolio <- function(S,BC) {
  d <- ncol(S)
  portfolio_variance <- function(X) c(t(X)%*%S%*%X)
  X0 <- rep(1/d,d)
  A <- BC[[1]]
  B <- BC[[2]]
  # solving this should give the minimum variance portfolio (GMV)
  X_gmv <- constrOptim(X0,portfolio_variance,grad = NULL,ui = A,ci = B)
  return(X_gmv)
}

# FUNCTION TO PRODUCE A LIST OF PORTFOLIOS FOR DIFFERENT k VALUES
MV_portfolios <- function(M,S,BC) {
    d <- ncol(S) # number of assets
    e <- as.matrix(rep(1,d)) # vector of ones
    MV_portfolio_k <- function(k) MV_portfolio(M,S,BC,k)
    k.seq <- c(seq(2.5,5,length = 10),seq(5,10,length = 10),seq(10,100,length = 100))
    X.list <- lapply(k.seq,MV_portfolio_k)
    return(X.list)
  }


# FUNCTION TO PLOT THE MVE FRONTIER FOR A GIVEN BC
MVE_function <- function(BC) {  
  # OUT-OF-SAMPLE CONSTRUCTED PORTFOLIOS
  X.list <- MV_portfolios(M2,S2,BC)
  # IN-SAMPLE CONSTRUCTED PORTFOLIOS
  X2.list <- MV_portfolios(M1,S1,BC)

  # OUT-OF-SAMPLE FRONTIER (HYPOTHETICAL CASE)
  M_p <- sapply(X.list,function(X) t(X)%*%M2 )
  
  
  V_p <- sqrt(sapply(X.list,function(X) t(X)%*%S2%*%X))
  MVE <- data.frame(M = M_p,V = V_p)

  # IN-SAMPLE FRONTIER (REALISTIC CASE)
  M2_p <- sapply(X2.list,function(X) t(X)%*%M2 )
  V2_p <- sqrt(sapply(X2.list,function(X) t(X)%*%S2%*%X))
  MVE2 <- data.frame(M = M2_p,V = V2_p)

  
  X_gmv1 <- gmv_portfolio(S2,BC)$par
  X_gmv2 <- gmv_portfolio(S1,BC)$par

  # HIGHLIGHT THE GMV POINT
  M_0 <- t(X_gmv1)%*%M2
  V_0 <- sqrt(t(X_gmv1)%*%S2%*%X_gmv1)
  MVE <- rbind(MVE,c(M_0,V_0))

  M_02 <- t(X_gmv2)%*%M2
  V_02 <- sqrt(t(X_gmv2)%*%S2%*%X_gmv2)
  MVE2 <- rbind(MVE2,c(M_02,V_02))

  # ORDER THE FRONTIER
  MVE <- MVE[order(MVE$M),]
  MVE2 <- MVE2[order(MVE2$M),]

  # ADD THE NAIVE PORTFOLIO
  X_N <- rep(1/ncol(R),ncol(R))
  M_N <- t(X_N)%*%M2
  V_N <- sqrt(t(X_N)%*%S2%*%X_N)
  
 list(MVE2 = MVE2,MVE = MVE,NAIVE = c(M_N,V_N), GMV = c(M_02,V_02) )
}



#####################################
######## PLOT WITH SHORT-SALES ######
#####################################

# USE BUDGET CONSTRAINTS BC1
MVE_BC1 <- MVE_function(BC1)

# FINALLY PRODUCE PLOT
MVE <- MVE_BC1$MVE
MVE2 <- MVE_BC1$MVE2
M_N <- MVE_BC1$NAIVE[1]
V_N <- MVE_BC1$NAIVE[2]
M_02 <- MVE_BC1$GMV[1]
V_02 <- MVE_BC1$GMV[2]

y.range <- range(c(MVE$M,MVE2$M))
x.range <-  range(c(MVE$V,MVE2$V))

plot(M~V,data = MVE,  ylim = y.range, xlim = x.range, pch = 1, cex = 0.3, col = 1, ylab = expression(mu[p]), xlab = expression(sigma[b]),type = "l", lwd = 2)
lines(M~V,data = MVE2, lty = 2,lwd = 2)
points(M_N~V_N,pch = 4,lwd = 2)
points(M_02~V_02,pch = 1,lwd = 2)
legend("topright",c("MVE Out","MVE In", "Naive","GMV") , lty = c(1:2,0,0),lwd = 2, pch = c(NA,NA,4,1)) 

########################################
######## PLOT WITHOUT SHORT-SALES ######
########################################

# USE BUDGET CONSTRAINTS BC2
MVE_BC2 <- MVE_function(BC2)

# FINALLY PRODUCE PLOT
MVE <- MVE_BC2$MVE
MVE2 <- MVE_BC2$MVE2
M_N <- MVE_BC2$NAIVE[1]
V_N <- MVE_BC2$NAIVE[2]
M_02 <- MVE_BC2$GMV[1]
V_02 <- MVE_BC2$GMV[2]

lines(M~V,data = MVE, lty = 1,lwd = 2, col = 2)
lines(M~V,data = MVE2, lty = 2,lwd = 2, col = 2)
points(M_N~V_N,pch = 4,lwd = 2, col = 2)
points(M_02~V_02,pch = 1,lwd = 2, col = 2)




  