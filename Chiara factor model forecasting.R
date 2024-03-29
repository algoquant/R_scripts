# Code E.1: Screening
# ##### a) Hard - thresholding rule
hard.thr <- function(sX, Y, sZ, h) {
  N <- ncol(sX)
  sX <- as.ts(sX)
  Y <- as.ts(coredata(Y))
  sZ <- as.ts(sZ)

  stat <- sapply(1:N, function(i)
    {
    dat <- ts.intersect(lY=lag(Y, k=h), sX=sX[,i], dframe=T)
# get the t-values for sX[,i], use the Newey - West correction
    return(coeftest(lm(lY ~ sX, data=dat), vcov.= NeweyWest(lm(lY ~ sX, data=dat),prewhite=F))[2,3])
  }
                 )

  a <- data.frame(cbind(abs(stat),1:N))
  a <- a[(order(a[,1], decreasing=T)),]
  return(ord <- a[,2])
}


# ###### b) soft thresholding rule: LARS
soft.lars <- function(sX, Y, h) {
  n <- length(Y)
  selec <- lars(x=as.matrix(sX[1:(n-h),]), y=as.matrix(Y[h +1:n])[1:(n-h)], type="lar", normalize=F)
  return(unique(unlist(selec$actions)))
}


# ######## c) soft thresholding: TS - LARS
soft.tslars <- function(sX, Y, h) {
  selec <- tslars(as.matrix(coredata(Y)) ~ as.matrix(sX), h=h, p.max=5, max.x= length(sX))
  if (length(selec$active)>ncol(sX)) {
    b <- (length(selec$active) - ncol(sX))+1
    return(selec$active[b:length(selec$active)])
  } else return(selec$active)
}


# Code E.2: Factor Estimation
factorstr <- function(osX) {
  X <- as.matrix(osX) # sX must be data.frame or matrix (NOT zoo or xts)
  N <- ncol(X)
  T <- nrow(X)
  if(T>N) { # Ng, Ludvigson (2009): asymptotic PCA NxN matrix
    omega <- t(X) %*% X
    decomp <- eigen(omega, symmetric=FALSE)
    lambda <- sqrt(N) * decomp$vectors
    fhat <- (1/N)*X %*% lambda
    return(list(fhat=fhat, lambda=lambda, values=decomp$values))
  }
  else { # Ng, Ludvigson (2009): asymptotic PCA TxT matrix
    omega <- X %*% t(X)
    decomp <- eigen(omega, symmetric=FALSE)
    fhat <- sqrt(T) * decomp$vectors
    lambda <- (1/T) * t(X) %*% fhat
    return(list(fhat=fhat, lambda=lambda, values=decomp$values))
  }
}


# Code E.3: Factor Selection
# No.factor selection
sigmasq <- function(osX, decomp, r, N, T) {
  F <- as.matrix(decomp$fhat[,1:r])
  L <- as.matrix(decomp$lambda[,1:r])
  return(sum((osX -(F %*% t(L))) ^2) /(N*T))
}

# find the maximum number of factors allowed
findkmax <- function(decomp) {
  for (j in 1:length(decomp$values)) { # choose kmax to compute sigmasq
    if(sum(decomp$values[1:j])/sum(decomp$values) >= 0.85) {
      kmax <- j
      break }
  }
  return(kmax)
}

# factor selection using modified and standard BN criteria
fselect <- function(decomp, osX, Y, sZ, h, fmod) { # see Bai, Ng (2002)
  T <- nrow(osX)
  N <- ncol(osX)
  g <- ((N+T)/(N*T))* log((N*T)/(N+T)) # penalization
  # standard BN criteria with g1 penalization 
  if(fmod==0) {
    kmax <- findkmax(decomp)
    PC <- sapply(1:kmax, function(r) {
      V <- sigmasq(osX, decomp, r=r, N, T)
      return(log(V)+r*g)
    })
    bestk <- which(PC==min(PC), arr.ind=TRUE)
    if(length(bestk)==1) return(list(Fhat=decomp$fhat[,1:bestk], ord=(1:bestk))
    )
  } else {
# modified BN criteria with g3 penalization 
    ordfactors <- hard.thr(decomp$fhat, Y, sZ, h)
    for(d in 1:3) {
      if(d != 3) decomp[[d]] <- decomp[[d]][, ordfactors] else
        decomp[[d]] <- decomp[[d]][ordfactors]
    }
    kmax <- findkmax(decomp)
    sigmahat <- sigmasq(osX, decomp, r=kmax, N, T)
    PC <- sapply(1:kmax, function(r) {
      V <- sigmasq(osX, decomp, r=r, N, T)
      return(V+r* sigmahat *g)
    })

    bestk <- which(PC==min(PC), arr.ind=TRUE)
    if(length(bestk)==1) return(list(Fhat=decomp$fhat[,1:bestk],
                                     ord= ordfactors[1:bestk]))
  }
}


# Code E.4: FAR Model Selection
modselect <- function(dataset, newdata, h) {
# 1) find complete lasso solution path
  mod.lars <- lars(x=as.matrix(dataset[,-1]), y=as.matrix(dataset[,1]),
                   type="lasso", trace=F, normalize=F, max.steps=1000)
  # 2) find the best solution computing in - sample mse for each solution computed
  # using the last window.test observations
  window.test <- 8
  if(nrow(mod.lars$beta)<50) quante.s <- nrow(mod.lars$beta) else quante.s <- 41
# dataset.mse add the last observation to the "dataset" in include it in the CV
  dataset.mse <- rbind(as.matrix(dataset[,1]), as.matrix(newdata[,1]))
  mse.lars <- sapply(2:quante.s, function(s) {
    error <- sapply((nrow(dataset)-window.test -h):(nrow(dataset)-h+1), function(x) {
                      predict(mod.lars, newx=as.matrix(dataset[x, -1]), s=s, type="fit", mode="step")$fit[[1]] - dataset.mse[x+h] 
    }
                    )
    return(mean(error ^2))
    }
                     )

  best.s <- which(mse.lars==min(mse.lars), arr.ind=TRUE)+1
# "best.s" is the index of the best solution in mod.lars$beta[best.s,]
# 3) get the estimated LASSO coefficients
  coeff <- mean(as.matrix(dataset[,1])) # intercept is the mean of y
  nam <- "intercept" 
  for(a in 1:ncol(dataset[,-1])) {
    if(mod.lars$beta[best.s,][[a]] != 0) {
      coeff <- c(coeff, mod.lars$beta[best.s,][[a]])
      nam <- c(nam, names(dataset)[a])
    }
  }
  coeff <- data.frame(t(as.matrix(coeff)))
  names(coeff) <- nam
  forecast <- predict(mod.lars, newx=as.matrix(newdata), s= best.s, type="fit",
                      mode="step")$fit[[1]]
  return(list(forecast=forecast, coefficients=coeff))
}
# End modselect

# Code E.5: Pseudo Real Time Forecasting Exercise
# cut window
initiate <- function(start, end, Y, sZ, sX) {
  Y <- Y[start:end]
  sZ <- data.frame(sp500dy=sZ$sp500dy[start:end],
                   sp500PE=sZ$sp500PE[start:end],
                   Tbill=sZ$Tbill[start:end],
                   spread=sZ$spread[start:end])
  sX <- sX[start:end,]
  return(list(Y=Y, sZ=sZ, sX=sX))
}


COOK <- function(i, sX, init, sZ, Y, preselec, k, h, fmod) { #sX is a data.frame
  
# initiate
  train <- initiate(start=i, end=i+init -1, Y, sZ, sX)
  Y <- train$Y
  sZ <- train$sZ
  sX <- train$sX

# the predictors are ordered
  if(preselec==0) ord <- 1:ncol(sX) # no screening
  if(preselec==1) ord <- hard.thr(sX, Y, sZ, h) # hard thresholding
  if(preselec==2) ord <- soft.lars(sX, coredata(Y), h) # LAR
  if(preselec==3) ord <- soft.tslars(sX, Y, h) # TS - LARS
  
  osX <- sX[,ord[1:k]] # choose k
  
# construct factors
  decomp <- factorstr(osX) # PCA
  factselection <- fselect(decomp, osX, Y, sZ, h, fmod) # factor selection
  Fhat <- factselection$Fhat
  nof <- ncol(Fhat) # no.of selected factors
  whichf <- factselection$ord
  
# factor augmented regression
  sZ <- as.ts(sZ) # must be "ts" in order to use "ts.intersect"
  Fhat2 <- as.ts(Fhat^2) 
  Fhat <- as.ts(Fhat)
  Y <- as.ts(coredata(Y))
  
  dataset <- ts.intersect(lY=lag(Y, k=h),
                          Y,
                          Fhat,
                          lFhat=lag(Fhat, -1),
                          Fhat2, 
                          sZ,
                          lsZ= lag(sZ, -1), dframe=T) # used in estimation lm()
  
  maxlag <- 1
  newdata <- ts.intersect(Y, # used for final prediction
                          Fhat,
                          lFhat=lag(Fhat, -1),
                          Fhat2, 
                          sZ,
                          lsZ= lag(sZ, -1), dframe=T)[length(Y)-maxlag,]
  
  result <- modselect(dataset, newdata, h)
  forecast <- result$forecast
  coeff <- result$coefficients
  
  return(list(forecast=forecast, coefficients=coeff, nofactors=nof,
              whichfactors=whichf))
}
