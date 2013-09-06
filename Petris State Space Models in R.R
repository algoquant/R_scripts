####################################
## 2.1 The local level model in R ##
####################################

fitNile <- StructTS(Nile, "level")
fitNile

plot(Nile, type = "o")
lines(fitted(fitNile), lty = "dashed", lwd = 2)
lines(tsSmooth(fitNile), lty = "dotted", lwd = 2)

library("forecast")
plot(forecast(fitNile, level = c(50, 90), h = 10), xlim = c(1950, 1980))


#################################################
## 2.2. The local level model with package dlm ##
#################################################

library("dlm")
mod <- dlmModPoly(1, dV = 0.3, dW = 0.01)

buildNile <- function(theta) {
  dlmModPoly(order = 1, dV = theta[1], dW = theta[2])
}

fit <- dlmMLE(Nile, parm = c(100, 2), buildNile, lower = rep(1e-4, 2))

modNile <- buildNile(fit$par)
drop(V(modNile)) # sigma2_epsilon
drop(W(modNile)) # sigma2_xi

library("numDeriv")
hs <- hessian(function(x) dlmLL(Nile, buildNile(x)), fit$par)
all(eigen(hs, only.values = TRUE)$values > 0)

aVar <- solve(hs)
sqrt(diag(aVar))

smoothNile <- dlmSmooth(Nile, modNile)

hwidth <- qnorm(0.05, lower = FALSE) *
  sqrt(unlist(dlmSvd2var(smoothNile$U.S, smoothNile$D.S)))
sm <- cbind(smoothNile$s, as.vector(smoothNile$s) + hwidth %o% c(-1, 1))

filterNile <- dlmFilter(Nile, modNile)
plot(residuals(filterNile, sd = FALSE), type = "o",
  ylab = "Standardized prediction error")
abline(h = 0)

foreNile <- dlmForecast(filterNile, nAhead = 10)
attach(foreNile)
hwidth <- qnorm(0.25, lower = FALSE) * sqrt(unlist(Q))
fore <- cbind(f, as.vector(f) + hwidth %o% c(-1, 1))
rg <- range(c(fore, window(Nile, start = c(1951, 1))))
plot(fore, type = "o", pch = 16, plot.type = "s", lty = c(1, 3, 3),
  ylab = "Nile level", xlab = "", xlim = c(1951, 1980), ylim = rg)
lines(window(Nile, start = c(1951, 1)), type = 'o')
lines(window(smoothNile$s, start = c(1951,1)), lty = 5)
abline(v = mean(c(time(f)[1], tail(time(Nile), 1))),
  lty = "dashed", col = "darkgrey")
legend("topleft", lty = c(1, 5, 1, 3), pch = c(1, NA, 16, 16), bty = "n",
  legend = c("observed level", "smoothed level", "forecasted level",
    "50% probability limits"))
detach(foreNile)


#############################################
## The local level model with package KFAS ##
#############################################

library("KFAS")
logLik <- function(theta) {
  lik <- kf(yt = Nile, Zt = 1, Tt = 1, Rt = 1, Ht = theta[1],
    Qt = theta[2], a1 = 0, P1 = 1e7)
  return(-lik$lik)
}
fit <- optim(par = c(100, 2), fn = logLik, lower = rep(1e-4, 2))

filterNile <- kf(yt = Nile, Zt = 1, Tt = 1, Rt = 1, Ht = fit$par[1],
  Qt = fit$par[2], a1 = 0, P1 = 1e7)
smoothNile <- ks(filterNile)
attach(smoothNile)
hwidth <- qnorm(0.05, lower = FALSE) * sqrt(drop(Vt))
sm <- cbind(drop(ahat), as.vector(ahat) + hwidth %o% c(-1, 1))
sm <- ts(sm, start = start(Nile))
plot(sm, plot.type = "s", type = "l", lty = c(1, 5, 5),
  ylab = "Level", xlab = "", ylim = range(Nile))
lines(Nile, type = "o", col = "darkgrey")
legend("bottomleft", col = c("darkgrey", rep("black", 2)),
  lty = c(1, 1, 5), pch = c(1, NA, NA), bty = "n", legend =
  c("data", "smoothed level", "90% probability limits"))
detach(smoothNile)

residNile <- drop(filterNile$vtuni / sqrt(filterNile$Ftuni))

foreNile <- forecast(filterNile, fc = 9)
attach(foreNile)
hwidth <- qnorm(0.25, lower = FALSE) * sqrt(drop(Pt.fc))
fore <- ts(cbind(drop(at.fc), drop(at.fc) + hwidth %o% c(-1, 1)),
  start = 1 + end(Nile)[1])
rg <- range(c(fore, window(Nile, start = c(1951, 1))))
detach(foreNile)


#######################################################
## 2.4. Bayesian inference for the local level model ##
#######################################################

set.seed(123)
gibbsOut <- dlmGibbsDIG(Nile, mod = dlmModPoly(1), shape.y = 0.1,
  rate.y = 0.1, shape.theta = 0.1, rate.theta = 0.1, n.sample = 10000,
  thin = 9)

burn <- 1:1000
attach(gibbsOut)
ts.plot(ergMean(dV[-burn]), ylab = "sample mean", xlab = "iterations",
  main = "obs variance")
ts.plot(ergMean(dW[-burn]), ylab = "sample mean", xlab = "iterations",
  main = "evolution variance")
acf(dV[-burn])
acf(dW[-burn])

plot(density(dV[-burn]), xlim = c(2000, 34000), ylab = "", main = "")
hist(dV[-burn], prob = TRUE, add = TRUE)
curve(dgamma(1/x, shape = 0.1, rate = 0.1) / x^2, lty = "dashed",
  add = TRUE)
plot(density(dW[-burn]), ylab = "", xlim = c(0, 16000),  main = "")
hist(dW[-burn], prob = TRUE, add = TRUE)
curve(dgamma(1/x, shape = 0.1, rate = 0.1) / x^2, lty = "dashed",
  add = TRUE)
plot(dV[-burn], dW[-burn], pch = ".", cex = 1.5, ylab = "")

mcmcMean(dV[-burn])
mcmcMean(dW[-burn])
quantile(dV[-burn], c(0.025, 0.975))
quantile(dW[-burn], c(0.025, 0.975))

lastTheta <- theta[length(Nile) + 1, , ]
levelSim <- matrix(0, nr = 10, nc = 10000)
for (it in 1:10000) {
  innovSim <- rnorm(10, sd = sqrt(dW[it]))
  levelSim[, it] <- cumsum(innovSim) + lastTheta[it]
}
ySim <- matrix(0, nr = 10, nc = 10000)
for (it in 1:10000) {
  innov <- rnorm(10, sd = sqrt(dV[it]))
  ySim[, it] <- innov + levelSim[, it]
}
yInts <- apply(ySim, 1, function(x) quantile(x, c(0.25, 0.75)))


#################################
## 3.1. Intervention variables ##
#################################

x <- matrix(c(rep(0, 27), rep(1, length(Nile) - 27)), ncol = 1)
modNileReg <- dlmModReg(x, dW = c(1, 0))
buildFun <- function(theta) {
  V(modNileReg) <- exp(theta[1])
  diag(W(modNileReg))[1] <- exp(theta[2])
  return(modNileReg)
}
fit <- dlmMLE(Nile, parm = rep(0, 2), build = buildFun)
modNileReg <- buildFun(fit$par)
drop(V(modNileReg))
W(modNileReg)[1]

modSmooth <- dlmSmooth(Nile, mod = modNileReg)
plot(Nile, type = "o")
lines(ts(modSmooth$s[-1, 1] + modSmooth$s[-1, 2] * x, start = 1871), lty = 2)


#######################################################
## 3.2. Structural time series and model composition ##
#######################################################

lGas <- log(UKgas)
dlmGas <- dlmModPoly() + dlmModSeas(4)
buildFun <- function(x) {
  diag(W(dlmGas))[2:3] <- exp(x[1:2])
  V(dlmGas) <- exp(x[3])
  return(dlmGas)
}
fit <- dlmMLE(lGas, parm = rep(0, 3), build = buildFun)
dlmGas <- buildFun(fit$par)
drop(V(dlmGas))
diag(W(dlmGas))[2:3]

gasSmooth <- dlmSmooth(lGas, mod = dlmGas)
x <- cbind(lGas, dropFirst(gasSmooth$s[, c(1, 3)]))
colnames(x) <- c("Gas", "Trend", "Seasonal")
plot(x, type = "o", main = "UK Gas Consumption")


##############################
## 3.3. Multivariate models ##
##############################

modUni <- dlmModPoly(1)
modUni %+% modUni %+% modUni
do.call(dlmSum, rep(list(modUni), 13))

tmp <- ts(read.table("P.txt", header = TRUE), start = c(1978, 1), frequency = 12) * 100
y <- tmp[, 1:4] - tmp[, "RKFREE"]
colnames(y) <- colnames(tmp)[1:4]
market <- tmp[, "MARKET"] - tmp[, "RKFREE"]
rm("tmp")
m <- NCOL(y)

Zt <- sapply(seq_along(market), function(i) market[i] %x% diag(m))
dim(Zt) <- c(m, m, length(market))
Rt <- diag(nr = m)
logLik <- function(theta) {
  a <- diag(exp(0.5 * theta[1:m]), nr = m)
  a[upper.tri(a)] <- theta[(m + 1):k]
  Ht <- crossprod(a)
  a <- diag(exp(0.5 * theta[1:m + k]), nr = m)
  a[upper.tri(a)] <- theta[-(1:(k + m))]
  Qt <- crossprod(a)
  lik <- kf(yt = t(y), Zt = Zt, Tt = diag(nr = m), Rt = Rt, Ht = Ht,
    Qt = Qt, a1 = rep(0, m), P1 = matrix(0, m, m),
    P1inf = diag(rep(1, m)), optcal = c(FALSE, FALSE, FALSE, FALSE))
  return(-lik$lik)
}
fit <- optim(par = rep(0, 2 * k), fn = logLik, method = "BFGS",
  control = list(maxit = 500))
fit$conv

theta <- fit$par
a <- diag(exp(0.5 * theta[1:m]), nr=m)
a[upper.tri(a)] <- theta[(m+1):k]
Ht <- crossprod(a)
a <- diag(exp(0.5 * theta[1:m + k]), nr=m)
a[upper.tri(a)] <- theta[-(1:(k + m))]
Qt <- crossprod(a)
smoothCAPM <- ks(kf(yt = t(y), Zt = Zt, Tt = diag(nr = m), Rt = Rt,
  Ht = Ht, Qt = Qt, a1 = rep(0, m), P1 = matrix(0, m, m),
  P1inf = diag(rep(1, m))))
betas <- ts(t(smoothCAPM$ahat), start = start(market),
  freq = frequency(market))
