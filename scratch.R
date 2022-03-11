# Calculate trailing z-scores of SVXY
predictor <- cbind(sqrt(variance), vx_x, vti_close)
response <- svx_y
rollzscores <- drop(HighFreq::roll_zscores(response=response, predictor=predictor, look_back=look_back))
rollzscores[is.infinite(rollzscores)] <- 0

rollreg <- HighFreq::roll_reg(response=response, predictor=predictor, intercept=TRUE, look_back=look_back)
rollregscores <- rollreg[, NCOL(rollreg), drop=TRUE]
all.equal(rollregscores, rollzscores)

library(microbenchmark)
# HighFreq::roll_reg() is faster than roll_zscores()
summary(microbenchmark(
  roll_zscores=HighFreq::roll_zscores(response=response, predictor=predictor, look_back=look_back),
  roll_reg=HighFreq::roll_reg(response=response, predictor=predictor, intercept=TRUE, look_back=look_back),
  times=10))[, c(1, 4, 5)]

runreg <- HighFreq::run_reg(response=response, predictor=predictor, lambda=lambdav, method="scale")
runzscores <- HighFreq::run_zscores(response=response, predictor=predictor, lambda=lambdav, demean=FALSE)
# runzscores <- runreg[, 1, drop=TRUE]

runreg <- rutils::lagit(rutils::diffit(cbind(response, predictor)))

runreg[is.infinite(runreg)] <- 0
runreg[abs(runreg) > 1e8] <- 0
runreg[1:11, ] <- 0


# Portfolio objective function
calc_perf <- function(x) {
  forecasts <- sign(runreg %*% x)
  pnls <- (returns*forecasts)["/2017"]
  -mean(pnls)/sd(pnls[pnls < 0])
}  # end calc_perf

# 2-dim case
optimd <- optim(fn=calc_perf, 
                par=rep(1, NCOL(runreg)),
                method="L-BFGS-B",
                upper=rep(100, NCOL(runreg)),
                lower=rep(-100, NCOL(runreg)))

# Portfolio weights - static dollars not shares
weights <- optimd$par

# Calculate out-of-sample pnls
forecasts <- runreg %*% weights
pnls <- (returns*forecasts)
pnls <- xts::xts(cumsum(pnls), zoo::index(returns))
dygraphs::dygraph(pnls, main="VTI Stategy Using RunReg")
forecasts <- xts::xts(forecasts, zoo::index(returns))
dygraphs::dygraph(forecasts, main="VTI RunReg Forecasts")


###############
### Benchmark of HighFreq::run_reg() against R code

library(HighFreq)
# Load ETF returns
returns <- na.omit(rutils::etfenv$returns[, c("XLF", "VTI", "IEF")])
# Response equals XLF returns
response <- returns[, 1]
# Predictor matrix equals VTI and IEF returns
predictor <- returns[, -1]
# Calculate the rolling regressions
lambdav <- 0.9
regs <- HighFreq::run_reg(response=response, predictor=predictor, lambda=lambdav)
# Plot the rolling alphas
datav <- cbind(cumsum(response), regs[, 2])
colnames(datav) <- c("XLF", "alphas")
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="Alphas of XLF Versus VTI and IEF") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

response <- zoo::coredata(response)
predictor <- zoo::coredata(predictor)
numrows <- NROW(predictor)
ncols <- NCOL(predictor)
means_resp <- matrix(nrow.n_rows)
means_pred <- matrix(nrow.n_rows, ncol.n_cols)
vars <- predictor^2
covars <- matrix(rep(0,.n_rows.n_cols), nrow.n_rows, ncol.n_cols)
betas <- matrix(rep(0,.n_rows.n_cols), nrow.n_rows, ncol.n_cols)
alphas <- matrix(rep(0,.n_rows), nrow.n_rows)
resids <- matrix(rep(0,.n_rows), nrow.n_rows)
# varz <- matrix(rep(1,.n_rows), nrow.n_rows)
# meanz <- matrix(rep(0,.n_rows), nrow.n_rows)
lambda1 <- 1-lambdav

# Perform loop over rows
means_resp[1, ] <- response[1, ]
means_pred[1, ] <- predictor[1, ]
for (it in 2.n_rows) {
  # Calculate the mean as the weighted sum
  means_resp[it, ] <- lambda1*response[it, ] + lambdav*means_resp[it-1, ]
  means_pred[it, ] <- lambda1*predictor[it, ] + lambdav*means_pred[it-1, ]
  vars[it, ] <- lambda1*(predictor[it, ]-means_pred[it, ])^2 + lambdav*vars[it-1, ]
  covars[it, ] <- lambda1*((response[it, ]-means_resp[it, ])*(predictor[it, ]-means_pred[it, ])) + lambdav*covars[it-1, ]
  betas[it, ] <- lambda1*covars[it, ]/vars[it, ] + lambdav*betas[it-1, ]
  alphas[it, ] <- lambda1*(means_resp[it, drop=FALSE] - betas[it, ] %*% means_pred[it, ]) + lambdav*alphas[it-1, ]
  # Calculate the z-score as the weighted sum of products of returns.
  resids[it, ] <- lambda1*(response[it, drop=FALSE] - betas[it, ] %*% predictor[it, ]) + lambdav*resids[it-1, ]
  # Calculate the mean and variance of the z-scores.
  # meanz[it, ] <- lambda1*resids[it, ] + lambdav*meanz[it-1, ]
  # varz[it, ] <- lambda1*(resids[it, ] - resids[it-1, ])^2 + lambdav*varz[it-1, ]
}  # end for

# regdatar <- cbind(alphas, betas, vars, (resids - meanz)/sqrt(varz))
regdatar <- cbind(resids, alphas, betas)
regdata <- HighFreq::run_reg(response=response, predictor=predictor, lambda=lambdav)
all.equal(regdatar, regdata, check.attributes=FALSE)



###############
### Portfolio optimization of VTI, VXX, and SVXY

# Select ETFs
symbolv <- c("VTI", "VXX", "SVXY")
returns <- na.omit(rutils::etfenv$returns[, symbolv])
dates <- zoo::index(returns)
prices <- na.omit(rutils::etfenv$prices[, symbolv])

# dygraph plot of VXX prices versus SVXY
dygraphs::dygraph(prices["2018-01-01/", 2:3], main="Returns of VXX and SVXY") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# dygraph plot of VXX returns versus SVXY
dygraphs::dygraph(cumsum(cbind(returns["2018-01-01/", 2], -2*returns["2018-01-01/", 3])), 
                  main="Returns of VXX and SVXY") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# In-sample returns
returns_in <- rbind(returns["/2018-02-01"], returns["2018-02-07/"])
returns_in <- returns["2018-03-01/"]

# Portfolio objective function
calc_perf <- function(x) {
  weights <- c(1, x[1], x[2])
  pnls <- (returns_in %*% weights)
  -mean(pnls)/sd(pnls[pnls < 0])
}  # end calc_perf

## Calculate in-sample weights
# 1-dim case
optimd <- optimize(calc_perf, c(-2, 1))
weights <- c(-1, optimd$minimum)

# 2-dim case
optimd <- optim(fn=calc_perf, 
                par=c(-1, -1),
                method="L-BFGS-B",
                upper=c(2, 2),
                lower=c(-5, -5))

# Portfolio weights - static dollars not shares
weights <- c(1, optimd$par)

weights <- c(1, -2, -2)

weights <- c(1, -2.1, -4.6)

# Calculate out-of-sample pnls
pnls <- (returns %*% weights)
pnls <- xts::xts(cumsum(pnls), zoo::index(returns))
dygraphs::dygraph(pnls, main="Static Portfolio of ETFs")


## VTI AR strategy with VXX and SVXY as predictors

# Define response as the rolling sum of returns
# response <- returns[, "VTI"]
numagg <- 5
response <- roll::roll_mean(returns[, "VTI"], width=numagg, min_obs=1)
response <- rutils::lagit(response, lagg=(-numagg+1))

# Define predictor
# predictor <- rutils::lagit(returns)
order_max <- 2
predictor <- roll::roll_mean(returns, width=numagg, min_obs=1)
predictor <- lapply(1:order_max, rutils::lagit, input=predictor)
predictor <- do.call(cbind, predictor)
# colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
numrows <- NROW(predictor)
predictor <- cbind(rep(1,.n_rows), predictor)
colnames(predictor)[1] <- "intercept"
model <- lm(response ~ predictor - 1)
model_sum <- summary(model)


# In-Sample
eigen_max <- 6
# inverse <- MASS::ginv(predictor)
inverse <- HighFreq::calc_inv(predictor, eigen_max=eigen_max)
coeff <- drop(inverse %*% response)
forecasts <- (predictor %*% coeff)
pnls <- sign(forecasts)*returns[, "VTI"]
wealth <- cbind(returns[, "VTI"], pnls)
colnames(wealth) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealth, function(x) mean(x)/sd(x[x<0]))
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="VIX Strategy In-Sample") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)


# Out-of-Sample
in_sample <- (dates < as.Date("2018-01-01"))
out_sample <- (dates >= as.Date("2018-01-01"))
# inverse <- MASS::ginv(predictor[in_sample, ])
eigen_max <- 4
inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
coeff <- drop(inverse %*% response[in_sample, ])
forecasts <- (predictor[out_sample, ] %*% coeff)
pnls <- sign(forecasts)*returns[out_sample, "VTI"]
wealth <- cbind(returns[out_sample, "VTI"], pnls)
colnames(wealth) <- c("VTI", "Strategy")
sqrt(252)*sapply(wealth, function(x) mean(x)/sd(x[x<0]))
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="VIX Strategy Out-of-Sample") %>%
dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
dyLegend(show="always", width=500)


eigen_maxs <- 2:7
pnls <- lapply(eigen_maxs, function(eigen_max) {
cat("eigen_max =", eigen_max, "\n")
inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
coeff <- drop(inverse %*% response[in_sample, ])
forecasts <- (predictor[out_sample, ] %*% coeff)
sign(forecasts)*response[out_sample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_max)
colnames(pnls) <- paste0("eigen", eigen_maxs)
colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
dyOptions(colors=colors, strokeWidth=1) %>%
dyLegend(show="always", width=500)



###############
### Tune parameters of AR strategies

eigen_maxs <- 2:7
pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max =", eigen_max, "\n")
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*response[out_sample, ]
})
pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  pnls <- lapply(3:(NROW(years)-1), function(i) {
    in_sample <- (dates > years[i-1]) & (dates < years[i])
    out_sample <- (dates >= years[i]) & (dates < years[i+1])
    inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
    coeff <- drop(inverse %*% response[in_sample, ])
    forecasts <- (predictor[out_sample, ] %*% coeff)
    sign(forecasts)*returns[out_sample, ]
  })  # end lapply
  do.call(rbind, pnls)
})  # end lapply


pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  pnls <- lapply(12:(NROW(months)-1), function(i) {
    in_sample <- (dates > months[i-3]) & (dates < months[i])
    out_sample <- (dates > months[i]) & (dates < months[i+1])
    inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
    coeff <- drop(inverse %*% response[in_sample, ])
    forecasts <- (predictor[out_sample, ] %*% coeff)
    sign(forecasts)*returns[out_sample, ]
  })  # end lapply
  do.call(rbind, pnls)
})  # end lapply

pnls <- lapply(eigen_maxs, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    in_sample <- (dates > weeks[i-3]) & (dates < weeks[i])
    out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
    inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
    coeff <- drop(inverse %*% response[in_sample, ])
    forecasts <- (predictor[out_sample, ] %*% coeff)
    sign(forecasts)*returns[out_sample, ]
  })  # end lapply
  do.call(rbind, pnls)
})  # end lapply

pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", eigen_maxs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)

# Loop over aggregations - aggregations don't improve the forecasts!
numaggs <- 2:11
pnls <- lapply(numaggs, function(numagg) {
  cat("numagg=", numagg, "\n")
  predictor <- roll::roll_mean(rates_diff, width=numagg, min_obs=1)
  predictor <- cbind(rep(1, NROW(predictor)), predictor)
  predictor <- rutils::lagit(predictor)
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    # Define in-sample and out-of-sample intervals
    in_sample <- (dates > weeks[i-look_back]) & (dates < weeks[i])
    out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
    # Calculate forecasts and pnls out-of-sample
    inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
    coeff <- drop(inverse %*% response[in_sample, ])
    forecasts <- (predictor[out_sample, ] %*% coeff)
    sign(forecasts)*returns[out_sample, ]
  })  # end lapply
  pnls <- do.call(rbind, pnls)
})  # end sapply

pnls <- do.call(cbind, pnls)
colnames(pnls) <- paste0("eigen", numaggs)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


###############

in_sample <- 1:.n_rows %/% 2)
out_sample <- .n_rows %/% 2 + 1).n_rows
# Calculate forecasts as function of eigen_max
forecasts <- lapply(2:5, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  # Calculate fitted coefficients
  # inverse <- MASS::ginv(predictor[in_sample, 1:eigen_max])
  inverse <- HighFreq::calc_inv(predictor[in_sample,], eigen_max=eigen_max)
  coeff <- drop(inverse %*% response[in_sample])
  # Calculate out-of-sample forecasts of vtis
  drop(predictor[out_sample, ] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:5)


###############
## Multifactor AR rates

load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
rates <- do.call(cbind, as.list(rates_env))
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]
closep <- log(quantmod::Cl(rutils::etfenv$VTI))
colnames(closep) <- "VTI"
numrows <- NROW(closep)
dates <- zoo::index(closep)
rates <- na.omit(rates[dates])
closep <- closep[zoo::index(rates)]
dates <- zoo::index(closep)
returns <- rutils::diffit(closep)
rates_diff <- rutils::diffit(log(rates))

order_max <- 5
predictor <- sapply(1:order_max, rutils::lagit, input=as.numeric(returns))
colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
predictor <- cbind(predictor, rutils::lagit(rates_diff))
predictor <- cbind(rep(1, NROW(predictor)), predictor)
colnames(predictor)[1] <- "intercept"
response <- returns

# Calculate forecasts as function of eigen_max
forecasts <- lapply(2:5, function(eigen_max) {
  cat("eigen_max=", eigen_max, "\n")
  inverse <- HighFreq::calc_inv(predictor, eigen_max=eigen_max)
  coeff <- drop(inverse %*% response)
  drop(predictor %*% coeff)
})  # end lapply
names(forecasts) <- paste0("n=", 2:5)

pnls <- lapply(forecasts, function(x) {
  sign(x)*returns
})  # end lapply
pnls <- do.call(cbind, pnls)
colnames(pnls) <- names(forecasts)

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


# Define weekly dates
weeks <- seq.Date(from=as.Date("2001-05-01"), to=as.Date("2021-04-01"), by="weeks")
# Perform loop over weekly dates
look_back <- 9
eigen_max <- 2
# Or
look_back <- 8
eigen_max <- 4
pnls <- lapply(51:(NROW(weeks)-1), function(i) {
  # Define in-sample and out-of-sample intervals
  in_sample <- (dates > weeks[i-look_back]) & (dates < weeks[i])
  out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
  # Calculate forecasts and pnls out-of-sample
  inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=eigen_max)
  coeff <- drop(inverse %*% response[in_sample, ])
  forecasts <- (predictor[out_sample, ] %*% coeff)
  sign(forecasts)*returns[out_sample, ]
})  # end lapply
pnls <- do.call(rbind, pnls)

vtis <- rutils::diffit(closep[zoo::index(pnls),])
wealth <- cbind(vtis, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Rolling Weekly Yield Curve Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)


###############


foo <- sapply(2:11, function(x) {
  pnls <- lapply(12:(NROW(months)-1), function(i) {
    # Define in-sample and out-of-sample intervals
    in_sample <- (dates > months[i-x]) & (dates < months[i])
    out_sample <- (dates > months[i]) & (dates < months[i+1])
    # Calculate forecasts and pnls out-of-sample
    inverse <- MASS::ginv(predictor[in_sample, ])
    # inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=3)
    coeff <- drop(inverse %*% response[in_sample, ])
    forecasts <- (predictor[out_sample, ] %*% coeff)
    sign(forecasts)*returns[out_sample, ]
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  mean(pnls)/sd(pnls)
})
(2:11)[which.max(foo)]


foo <- sapply(2:11, function(x) {
  pnls <- lapply(51:(NROW(weeks)-1), function(i) {
    # Define in-sample and out-of-sample intervals
    in_sample <- (dates > weeks[i-x]) & (dates < weeks[i])
    out_sample <- (dates > weeks[i]) & (dates < weeks[i+1])
    # Calculate forecasts and pnls out-of-sample
    inverse <- MASS::ginv(predictor[in_sample, ])
    # inverse <- HighFreq::calc_inv(predictor[in_sample, ], eigen_max=3)
    coeff <- drop(inverse %*% response[in_sample, ])
    forecasts <- (predictor[out_sample, ] %*% coeff)
    sign(forecasts)*returns[out_sample, ]
  })  # end lapply
  pnls <- do.call(rbind, pnls)
  mean(pnls)/sd(pnls)
})
(2:11)[which.max(foo)]


###############

# Define backtest functional for daily momentum strategy
# If trend=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, hold_period=5, bid_offer=0.001, trend=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back, min_obs=1)
  variance[1, ] <- 1
  variance[variance <= 0] <- 1
  # Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back, min_obs=1)
  weights <- past/sqrt(variance)
  weights <- weights/sqrt(rowSums(weights^2))
  weights <- rutils::lagit(weights)
  weights <- roll::roll_mean(weights, width=hold_period, min_obs=1)
  # Calculate momentum profits and losses
  pnls <- trend*rowMeans(weights*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  (pnls - costs)
}  # end momentum_daily

look_backs <- seq(50, 150, by=10)
pnls <- sapply(look_backs, momentum_daily, hold_period=9,
                  returns=returns, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", look_backs)
pnls <- xts::xts(pnls, index(returns))

hold_periods <- seq(9, 21, by=2)
pnls <- sapply(hold_periods, momentum_daily, look_back=120,
                  returns=returns, bid_offer=bid_offer)
colnames(pnls) <- paste0("look_back=", hold_periods)
pnls <- xts::xts(pnls, index(returns))

colors <- colorRampPalette(c("blue", "red"))(NCOL(pnls))
dygraphs::dygraph(cumsum(pnls), main="Cumulative Returns of AR Strategies") %>%
  dyOptions(colors=colors, strokeWidth=1) %>%
  dyLegend(show="always", width=500)


###############
### Interest rate butterfly strategy
# Doesn't work well

load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
rates <- mget(ls(rates_env), envir=rates_env)
rates <- rutils::do_call(cbind, rates)
rates <- zoo::na.locf(rates, na.rm=FALSE)
rates <- zoo::na.locf(rates, fromLast=TRUE)

# Sort the columns of rates according bond maturity
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]

returns <- rutils::diffit(log(rates))

covmat <- cov(returns["2000/"])
cormat <- cor(returns["2000/"])
eigend <- eigen(cormat)
eigend$vectors

bfly <- rates["2000/"] %*% eigend$vectors[, 3]
dates <- zoo::index(rates["2000/"])
bfly <- xts::xts(bfly, dates)
dygraphs::dygraph(bfly, main="IR Butterfly") %>% 
  dyOptions(colors="blue", strokeWidth=2)

# ADF test


# Calculate the volatility
look_back <- 21
returns <- rutils::diffit(bfly)
volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
volat[1, ] <- 1
# Calculate the z-scores of prices
mean_s <- roll::roll_mean(bfly, width=look_back, min_obs=1)
z_scores <- ifelse(volat > 0, (bfly - mean_s)/volat, 0)
z_scores <- z_scores/sqrt(look_back)
sd(z_scores)
hist(z_scores)

# Calculate positions
po_s <- rep(NA_integer_, NROW(bfly))
po_s[z_scores < (-1)] <- 1
po_s[z_scores > 1] <- (-1)
# Carry forward and backward non-NA po_s
po_s <- zoo::na.locf(po_s, na.rm=FALSE)
po_s <- zoo::na.locf(po_s, fromLast=TRUE)
po_s <- rutils::lagit(po_s)
pnls <- returns*po_s

# Plot dygraph of in-sample VTI strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("BFLY", "Strategy")
dygraphs::dygraph(cumsum(wealth), main="Butterfly Strategy In-sample") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(show="always", width=500)



###############
### Test for app_zscore_returns_strat.R
# Find optimal parameters for SPY minutes data

load(file="/Volumes/external/Develop/data/polygon/spy_minutes.RData")
ohlc <- ohlc["T09:00:00/T16:30:00"]
returns <- rutils::diffit(log(Cl(ohlc)))
numrows <- NROW(returns)

lambdav <- 0.15
lagg <- 1
thresh_old <- 1.0
long_back <- 100

refvar <- rep(1, NROW(returns))
tseries <- cbind(returns, refvar)

# Run the model
calc_sharpe <- function(lambdav, thresh_old) {
  z_scores <- HighFreq::run_zscore(tseries, lambda=lambdav)
  z_scores <- z_scores[, 1, drop=FALSE]
  z_scores <- HighFreq::roll_scale(z_scores, look_back=long_back, use_median=TRUE)
  z_scores[is.na(z_scores) | is.infinite(z_scores)] <- 0
  z_scores <- HighFreq::lagit(z_scores, pad_zeros=TRUE)
  indic <- rep(0,.n_rows)
  indic <- ifelse(z_scores > thresh_old, -1, indic)
  indic <- ifelse(z_scores < (-thresh_old), 1, indic)
  indic_sum <- HighFreq::roll_vec(tseries=indic, look_back=lagg)
  indic_sum[1:lagg] <- 0
  position_s <- rep(NA_integer_,.n_rows)
  position_s[1] <- 0
  position_s <- ifelse(indic_sum >= lagg, 1, position_s)
  position_s <- ifelse(indic_sum <= (-lagg), -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s <- rutils::lagit(position_s, lagg=1)
  pnls <- position_s*returns
  mean(pnls)/sd(pnls[pnls<0])
}  # end calc_sharpe

calc_sharpe(0.3)


# Calculate heatmaps 
lambdavs <- seq(0.1, 0.3, 0.01)
thresh_old <- 1.0
sharpes <- sapply(lambdavs, calc_sharpe, thresh_old=thresh_old)
plot(lambdavs, sharpes)

thresh_olds <- seq(0.5, 4, 0.2)
lambdav <- 0.25
sharpes <- sapply(thresh_olds, calc_sharpe, lambdav=lambdav)
plot(thresh_olds, sharpes)



###############
### Stock Forecasting Using Interest Rate Data

# Load FRED data from csv file
# datav <- data.table::fread(file="C:/Develop/predictive/data/FRED_data.csv", stringsAsFactors=FALSE)

datav <- read.csv(file="C:/Develop/predictive/data/FRED_data.csv")
colnamev <- colnames(datav)[-1]
sapply(datav, class)
datav <- lapply(datav[, -1], as.numeric)
datav <- rutils::do_call(cbind, datav)
apply(datav, 2, class)

num_nona <- apply(datav, 2, function(x) sum(!is.na(x)))
colnamev <- colnamev[num_nona > 0]
datav <- lapply(1:NCOL(datav), function(x) {
  if (sum(!is.na(datav[, x])) > 0)
    datav[, x]
  else
    NULL
})  # end lapply
datav <- rutils::do_call(cbind, datav)
head(datav)
tail(datav)

diff_data <- rutils::diffit(datav, pad_zeros=FALSE)
bar <- var(diff_data, na.rm=TRUE)


corr_el <- sapply(2:NCOL(diff_data), function(x) {
  di_ff <- na.omit(diff_data[, c(1, x)])
  cor(di_ff[, 1], rutils::lagit(di_ff[, 2], pad_zeros=FALSE))
})  # end lapply
names(corr_el) <- colnamev[-1]
sort(corr_el)



###############
sum(!is.na(datav[, 1]))

which(!is.null(datav[, BAMLH0A0HYM2SY]))



datav <- lapply(datav[, -1], rutils::diffit)
datav <- rutils::do_call(cbind, datav)



returns <- lapply(datav[, -1], rutils::diffit)
returns <- rutils::do_call(cbind, returns)
colnames(returns) <- c("sentiment", "SPY")
cor(rutils::lagit(returns[, 1]), returns[, 2])


datav <- datav[, c("date", "sentiment", "close")]
colnames(datav) <- c("date", "sentiment", "SPY")
datav <- xts::xts(datav[, 2:3], as.Date.IDate(datav[, date]))



############### - copied to slides
### Simulate an AR strategy for VTI using IR yield curve
# Comment: PCA of yield curve have some forecasting power for VTI

wealth <- cbind(foo1, foo2, foo3)
color_s <- colorRampPalette(c("blue", "red"))(NCOL(wealth))
colnames(wealth) <- c("NoReg", "Reg=3", "Reg=2")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Yield Curve Strategy In-sample") %>%
  dyOptions(colors=color_s, strokeWidth=1) %>% dyLegend(show="always", width=500)

# Load packages
library(rutils)

# Load constant maturity Treasury rates
load(file="/Users/jerzy/Develop/lecture_slides/data/rates_data.RData")
# Combine rates into single xts series
rates <- do.call(cbind, as.list(rates_env))
# Sort the columns of namesv according bond maturity
namesv <- colnames(rates)
namesv <- substr(namesv, start=4, stop=10)
namesv <- as.numeric(namesv)
indeks <- order(namesv)
rates <- rates[, indeks]
tail(rates)
# Align rates dates with VTI prices
closep <- quantmod::Cl(rutils::etfenv$VTI)
numrows <- NROW(closep)
dates <- zoo::index(closep)
rates <- na.omit(rates[dates])
closep <- closep[zoo::index(rates)]
dates <- zoo::index(closep)
all.equal(dates, zoo::index(rates))
returns <- rutils::diffit(log(closep))
# Calculate VTI returns

# Calculate change in rates
rates_diff <- rutils::diffit(log(rates))

# Calculate eigen decomposition of correlation/covariance matrix
# eigend <- eigen(cov(rates_diff))
eigend <- eigen(cor(rates_diff))
rates_pca <- rates_diff %*% eigend$vectors

foo <- apply(rates_pca, 2, function(x) {
  pacfd <- pacf(x, lag=10, plot=FALSE)
  sum(pacfd$acf)
})  # end sapply
barplot(foo, main="PACF of Interest Rate PCs")

# Plot principal components of rates
# rates_pca <- rates %*% eigend$vectors[, 1:3]
# rates_pca <- apply(rates_pca, 2, cumsum)

datav <- cbind(returns, rates_pca[, 1:3])
colnames(datav) <- c("VTI", "FirstPC", "Steepener", "Butterfly")
colnamev <- colnames(datav)[c(1, 3)]
dygraphs::dygraph(cumsum(datav[, c(1, 3)]), main=paste(colnames(datav)[1], "and IR", colnames(datav)[3])) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")


# Pure in-sample with aggregations
numagg <- 40
# Define response as the rolling sum of returns
# response <- returns
response <- roll::roll_mean(returns, width=numagg, min_obs=1)
response <- rutils::lagit(response, lagg=(-numagg+1))
# Calculate predictor as the rolling rates PCAs
# predictor <- rutils::lagit(rates_pca[, 1:2])
predictor <- roll::roll_mean(rates_pca, width=numagg, min_obs=1)
predictor <- rutils::lagit(predictor)
# Calculate inverse of predictor
inverse <- MASS::ginv(predictor)
coeff <- drop(inverse %*% response)
# coeff <- coeff/sqrt(sum(coeff^2))
forecasts <- (predictor %*% coeff)
pnls <- forecasts*response
# pnls <- pnls*sd(returns)/sd(pnls)
# Plot dygraph of in-sample VTI strategy
wealth <- cbind(returns, pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Autoregressive Strategy Using Yield Curve") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Find best numagg
numaggs <- 5:50
foo <- sapply(numaggs, function(numagg) {
  response <- roll::roll_mean(returns, width=numagg, min_obs=1)
  response <- rutils::lagit(response, lagg=(-numagg+1))
  predictor <- roll::roll_mean(rates_pca, width=numagg, min_obs=1)
  predictor <- rutils::lagit(predictor)
  inverse <- MASS::ginv(predictor)
  coeff <- drop(inverse %*% response)
  forecasts <- (predictor %*% coeff)
  pnls <- forecasts*returns
  sum(pnls)/sd(pnls)
})  # end sapply
numaggs[which.max(foo)]
plot(numaggs, foo, t="l", col="blue", lwd=2)


# Fit in-sample logistic regression
glmod <- glm((sign(response)+1)/2 ~ predictor, family=binomial(logit))
summary(glmod)
coeff <- glmod$coefficients
predictv <- drop(design[, -1] %*% coeff)
ordern <- order(predictv)
# Calculate in-sample forecasts from logistic regression model
forecasts <- 1/(1+exp(-predictv))


# Define in-sample and out-of-sample intervals
in_sample <- (dates < as.Date("2020-01-01"))
out_sample <- (dates > as.Date("2020-01-01"))
# Or without 2008 and 2009
in_sample <- ((dates < as.Date("2008-01-01")) | ((dates > as.Date("2010-01-01")) & (dates < as.Date("2012-01-01"))))
out_sample <- (dates > as.Date("2012-01-01"))


# Calculate in-sample fitted coefficients
inverse <- MASS::ginv(predictor[in_sample, ])
coeff <- drop(inverse %*% response[in_sample, ])
names(coeff) <- colnames(predictor)


# Calculate regularized inverse using RcppArmadillo
eigen_max <- 3
inverse <- HighFreq::calc_inv(predictor[in_sample], eigen_max=eigen_max)
coeff <- drop(inverse %*% response[in_sample])
names(coeff) <- colnames(predictor)

## Define predictor as a rolling mean
numagg <- 10
predictor <- roll::roll_mean(rates_pca, width=numagg, min_obs=1)
predictor <- rutils::lagit(predictor)
# Shift the response forward out-of-sample
response <- roll::roll_mean(returns, width=numagg, min_obs=1)
response <- rutils::lagit(response, lagg=(-numagg+1))
# Calculate in-sample fitted coefficients
inverse <- MASS::ginv(predictor[in_sample])
coeff <- drop(inverse %*% response[in_sample])
names(coeff) <- colnames(predictor)

# Calculate in-sample PnLs
forecasts <- predictor[in_sample] %*% coeff
pnls <- cumsum(forecasts*returns[in_sample])
pnls <- xts::xts(pnls, dates[in_sample])
colnames(pnls) <- "VTI"

# Calculate out-of-sample PnLs
forecasts <- (predictor[out_sample, ] %*% coeff)
pnls <- forecasts*response[out_sample, ]
# Plot dygraph of out-of-sample VTI strategy
wealth <- cbind(returns[out_sample, ], pnls)
colnames(wealth) <- c("VTI", "Strategy")
colnamev <- colnames(wealth)
dygraphs::dygraph(cumsum(wealth), main="Autoregressive Strategy Using Yield Curve") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue", strokeWidth=1) %>%
  dySeries(name=colnamev[2], axis="y2", col="red", strokeWidth=2) %>%
  dyLegend(show="always", width=500)

# Trade the strategy
response <- pnls
predictor <- lapply(1:3, function(n) rutils::lagit(pnls, n))
predictor <- do.call(cbind, predictor)
# Calculate inverse of predictor
inverse <- MASS::ginv(predictor)
coeff <- drop(inverse %*% response)
# coeff <- coeff/sqrt(sum(coeff^2))
forecasts <- rutils::lagit(predictor %*% coeff)
pnls <- forecasts*response



##### some optim stuff

u_til <- function(coeff) {
  forecasts <- predictor %*% coeff
  -sum(sign(forecasts)*returns)
}  # end u_til

# Optimize with respect to vector argument
optimd <- optim(fn=u_til, 
                par=rnorm(6),
                method="L-BFGS-B",
                upper=rep(10, 6),
                lower=rep(-10, 6))
# Weights
coeff <- optimd$par


# Calculate eigen decomposition of covariance matrix
eigend <- eigen(cov(rates_diff))
pcad <- eigend$vectors[, 1:2]
pcad %*% c(1, -1)

u_til <- function(coeffpc) {
  coeff <- pcad %*% coeffpc
  forecasts <- predictor %*% coeff
  -sum(sign(forecasts)*returns)
}  # end u_til

# Optimize with respect to vector argument
optimd <- optim(fn=u_til, 
                par=rnorm(2),
                method="L-BFGS-B",
                upper=rep(10, 2),
                lower=rep(-10, 2))
coeffpc <- optimd$par
coeff <- pcad %*% coeffpc
forecasts <- drop(predictor %*% coeff)
pnls <- cumsum(sign(forecasts)*returns)
pnls <- xts::xts(pnls, dates)
colnames(pnls) <- "VTI"
dygraphs::dygraph(pnls, main="Autoregressive Strategy Using Yield Curve") %>%
  dyOptions(colors="blue", strokeWidth=2) %>%
  dyLegend(width=500)


## Define predictor as a rolling mean
numagg <- 5
predictor <- roll::roll_mean(predictor, width=numagg, min_obs=1)
# Shift the response forward out-of-sample
response <- roll::roll_mean(returns, width=numagg, min_obs=1)
response <- rutils::lagit(response, lagg=(-numagg+1))
# Calculate in-sample fitted coefficients
inverse <- MASS::ginv(predictor[in_sample])
coeff <- drop(inverse %*% returns[in_sample])
names(coeff) <- colnames(predictor)

foo <- sapply(1*(1:20), function(numagg) {
  predictor <- roll::roll_mean(predictor, width=numagg, min_obs=1)
  response <- roll::roll_mean(returns, width=numagg, min_obs=1)
  response <- rutils::lagit(response, lagg=(-numagg+1))
  inverse <- MASS::ginv(predictor)
  coeff <- drop(inverse %*% response)
  forecasts <- predictor %*% coeff
  sum(sign(forecasts)*returns)
})  # end sapply




###############
### Backtest an AR strategy with design as z-scores.
# Comment: The z-scores have low predictive power.

# Load packages
library(rutils)

# Calculate SVXY and VXX prices
svx_y <- log(rutils::etfenv$SVXY)
svxy_close <- quantmod::Cl(svx_y)
numrows <- NROW(svx_y)
dates <- zoo::index(svx_y)
vx_x <- log(rutils::etfenv$VXX[dates])
vxx_close <- quantmod::Cl(vx_x)

look_back <- 21

# Extract log OHLC prices
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)[dates]
closep <- log(quantmod::Cl(ohlc))
volumes <- quantmod::Vo(ohlc)
returns <- rutils::diffit(closep)


# Define response as a rolling mean
numagg <- 5
response <- roll::roll_mean(returns, width=numagg, min_obs=1)
# Shift the response forward out-of-sample
response <- rutils::lagit(response, lagg=(-numagg+1))

# Calculate SVXY z-scores
indeks <- matrix(1.n_rows, nc=1)
svxy_scores <- drop(HighFreq::roll_zscores(response=svxy_close, design=indeks, look_back=look_back))
svxy_scores[1:look_back] <- 0
svxy_scores[is.infinite(svxy_scores)] <- 0
svxy_scores[is.na(svxy_scores)] <- 0
svxy_scores <- svxy_scores/sqrt(look_back)
# roll_svxy <- roll::roll_mean(svxy_close, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var_ohlc(svx_y, look_back=look_back, scale=FALSE))
# svxy_scores <- (svxy_close - roll_svxy)/var_rolling

# Calculate VXX z-scores
vxx_scores <- drop(HighFreq::roll_zscores(response=vxx_close, design=indeks, look_back=look_back))
vxx_scores[1:look_back] <- 0
vxx_scores[is.infinite(vxx_scores)] <- 0
vxx_scores[is.na(vxx_scores)] <- 0
vxx_scores <- vxx_scores/sqrt(look_back)
# roll_vxx <- roll::roll_mean(vxx_close, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var_ohlc(vx_x, look_back=look_back, scale=FALSE))
# vxx_scores <- (vxx_close - roll_vxx)/var_rolling

# Calculate price z-scores
pricescores <- drop(HighFreq::roll_zscores(response=closep, design=indeks, look_back=look_back))
pricescores[1:look_back] <- 0
pricescores[is.infinite(pricescores)] <- 0
pricescores[is.na(pricescores)] <- 0
pricescores <- pricescores/sqrt(look_back)
# roll_stock <- roll::roll_mean(closep, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
# pricescores <- (closep - roll_stock)/var_rolling

# Calculate volatility z-scores
volat <- log(quantmod::Hi(ohlc))-log(quantmod::Lo(ohlc))
volat_scores <- drop(HighFreq::roll_zscores(response=volat, design=indeks, look_back=look_back))
volat_scores[1:look_back] <- 0
volat_scores[is.infinite(volat_scores)] <- 0
volat_scores[is.na(volat_scores)] <- 0
volat_scores <- volat_scores/sqrt(look_back)
# roll_vol <- roll::roll_mean(volat, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var(rutils::diffit(volat), look_back=look_back))
# volat_scores <- (volat - roll_vol)/var_rolling

# Calculate volume z-scores
volume_scores <- drop(HighFreq::roll_zscores(response=volumes, design=indeks, look_back=look_back))
volume_scores[1:look_back] <- 0
volume_scores[is.infinite(volume_scores)] <- 0
volume_scores[is.na(volume_scores)] <- 0
volume_scores <- volume_scores/sqrt(look_back)
# volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
# var_rolling <- sqrt(HighFreq::roll_var(rutils::diffit(volumes), look_back=look_back))
# volume_scores <- (volumes - volume_mean)/var_rolling

# Define design matrix
design <- cbind(vxx_scores - svxy_scores, volat_scores, pricescores, volume_scores)
colnames(design) <- c("vxx", "stock", "volat", "volume")

# Invert the predictor matrix
design_inv <- MASS::ginv(design)
# Calculate fitted coefficients
coeff <- drop(design_inv %*% response)
names(coeff) <- c("vxx", "stock", "volat", "volume")
barplot(coeff)
# Calculate forecast
forecasts <- drop(design %*% coeff)
forecasts <- rutils::lagit(forecasts)
pnls <- sign(forecasts)*returns
# forecasts <- xts::xts(forecasts, dates)
dygraph(cumsum(pnls))

# Define in-sample and out-of-sample intervals
in_sample <- 1:.n_rows %/% 2)
out_sample <- .n_rows %/% 2 + 1).n_rows

# Invert the predictor matrix in-sample
design_inv <- MASS::ginv(design[in_sample, ])
# Calculate in-sample fitted coefficients
coeff <- drop(design_inv %*% response[in_sample, ])
names(coeff) <- c("vxx", "stock", "volat", "volume")
barplot(coeff)
# Calculate out-of-sample forecasts
forecasts <- drop(design[out_sample, ] %*% coeff)
forecasts <- rutils::lagit(forecasts)
pnls <- sign(forecasts)*returns[out_sample, ]
# forecasts <- xts::xts(forecasts, dates)
dygraph(cumsum(pnls))



###############
### Backtest a strategy trading at oversold and overbought 
# extreme price points using weights optimization.
# Comment: The z-scores have low predictive power.

# Define z-score weights
weights <- c(15, -15, 0, 0)
names(weights) <- c("vxx", "stock", "volat", "volume")

coeff <- 1
lagg <- 1
thresh_top <- 0.3
thresh_bot <- (-0.1)

# Define back_test as function of thresholds
back_test <- function(weights=c(25, 0, 0, 0),
                     .n_rows,
                      thresh_top=0.1,
                      thresh_bot=0.1,
                      design,
                      returns,
                      lagg,
                      coeff=coeff) {
  sig_nal <- drop(design %*% weights)
  top_s <- (sig_nal > thresh_top)
  bottom_s <- (sig_nal < thresh_bot)
  indic <- rep(NA_integer_,.n_rows)
  indic[1] <- 0
  indic[bottom_s] <- coeff
  indic[top_s] <- (-coeff)
  indic <- zoo::na.locf(indic, na.rm=FALSE)
  indic_sum <- roll::roll_sum(indic, width=lagg, min_obs=1)
  indic_sum[1:lagg] <- 0
  position_s <- rep(NA_integer_,.n_rows)
  position_s[1] <- 0
  position_s <- ifelse(indic_sum == lagg, 1, position_s)
  position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s[1:lagg] <- 0
  position_s <- rutils::lagit(position_s, lagg=1)
  position_s
}  # end back_test

# Objective function equal to minus strategy returns
object <- function(weights, FUN, returns, ...) {
  -sum(returns*FUN(weights, ...))
}  # end object

# Perform weights optimization
optimd <- optim(par=weights,
                fn=object,
                FUN=back_test,
               .n_rows.n_rows,
                # thresh_top=thresh_top,
                # thresh_bot=thresh_bot,
                design=design,
                returns=returns,
                lagg=lagg,
                coeff=coeff,
                method="L-BFGS-B",
                upper=rep(25, 4),
                lower=rep(-25, 4))
weights <- optimd$par
names(weights) <- c("vxx", "stock", "volat", "volume")
back_test(weights, 
         .n_rows.n_rows,
          # thresh_top=thresh_top,
          # thresh_bot=thresh_bot,
          design=design,
          returns=returns,
          lagg=lagg,
          coeff=coeff)

# Objective function equal to minus strategy returns
# For DEoptim only way to make it work.
object <- function(weights=c(25, 0, 0, 0),
                     .n_rows,
                      thresh_top=0.1,
                      thresh_bot=0.1,
                      design,
                      returns,
                      lagg,
                      coeff=coeff) {
  sig_nal <- drop(design %*% weights)
  top_s <- (sig_nal > thresh_top)
  bottom_s <- (sig_nal < thresh_bot)
  indic <- rep(NA_integer_,.n_rows)
  indic[1] <- 0
  indic[bottom_s] <- coeff
  indic[top_s] <- (-coeff)
  indic <- zoo::na.locf(indic, na.rm=FALSE)
  indic_sum <- roll::roll_sum(indic, width=lagg, min_obs=1)
  indic_sum[1:lagg] <- 0
  position_s <- rep(NA_integer_,.n_rows)
  position_s[1] <- 0
  position_s <- ifelse(indic_sum == lagg, 1, position_s)
  position_s <- ifelse(indic_sum == (-lagg), -1, position_s)
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s[1:lagg] <- 0
  position_s <- rutils::lagit(position_s, lagg=1)
  -sum(position_s*returns)
}  # end object

# Perform portfolio optimization using DEoptim
optimd <- DEoptim::DEoptim(fn=object,
                          .n_rows.n_rows,
                           # thresh_top=thresh_top,
                           # thresh_bot=thresh_bot,
                           design=design,
                           returns=returns,
                           lagg=lagg,
                           coeff=coeff,
                           upper=rep(25, 4),
                           lower=rep(-25, 4),
                           control=list(trace=FALSE, itermax=1e3, parallelType=1))
weights <- optimd$optim$bestmem/sum(abs(optimd$optim$bestmem))
names(weights) <- c("vxx", "stock", "volat", "volume")
# names(weights) <- c("vxx", "svxy", "volat", "volume")

# Calculate strategy positions
position_s <- back_test(weights, 
         .n_rows.n_rows,
          # thresh_top=thresh_top,
          # thresh_bot=thresh_bot,
          design=design,
          returns=returns,
          lagg=lagg,
          coeff=coeff)

# Number of trades
sum(abs(rutils::diffit(position_s)))
# Calculate strategy returns
pnls <- position_s*returns
dygraph(cumsum(pnls))



###############
### Label the turning points in prices.

# Extract log OHLC prices
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)
closep <- log(quantmod::Cl(ohlc))
volumes <- quantmod::Vo(ohlc)
returns <- rutils::diffit(closep)
numrows <- NROW(ohlc)

# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
volat <- rutils::lagit(volat, lagg=(-half_back))

# Calculate the z-scores of prices
mid_p <- 1.n_rows  # mid point
startp <- (mid_p - half_back)  # start point
startp[1:half_back] <- 1
endp <- (mid_p + half_back)  # end point
endp[.n_rows-half_back+1).n_rows] <-.n_rows
closep <- as.numeric(closep)
pricescores <- (2*closep[mid_p] - closep[startp] - closep[endp])
pricescores <- ifelse(volat > 0, pricescores/volat, 0)
dygraph(pricescores)
hist(pricescores)

# Plot dygraph of z-scores of VTI prices
prices <- cbind(closep, pricescores)
colnames(prices) <- c(symbol, paste(symbol, "Z-Score"))
colnamev <- colnames(prices)
dygraphs::dygraph(prices["2009"], main=paste(symbol, "Z-Score")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")

# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(pricescores, c(0.1, 0.9))
# Calculate the vectors of tops and bottoms
top_s <- (pricescores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- (pricescores < threshold_s[1])
colnames(bottom_s) <- "bottoms"

# Backtest in-sample VTI strategy
position_s <- rep(NA_integer_,.n_rows)
position_s[1] <- 0
position_s[top_s] <- (-1)
position_s[bottom_s] <- 1
position_s <- zoo::na.locf(position_s)
# position_s <- rutils::lagit(position_s, 1)
pnls <- cumsum(returns*position_s)
# Number of trades
sum(abs(rutils::diffit(position_s))) / NROW(position_s)

# Plot dygraph of in-sample VTI strategy
prices <- cbind(closep, pnls)
colnames(prices) <- c(symbol, paste(symbol, "Strategy"))
colnamev <- colnames(prices)
dygraphs::dygraph(prices, main=paste(symbol, "In-sample Strategy")) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=2, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=2, col="red")



###############
### Apply logistic regression using bar labels as the response.
# Comment: The z-scores have low predictive power.

# Calculate log VTI prices
ohlc <- log(rutils::etfenv$VTI)
closep <- quantmod::Cl(ohlc)
returns <- rutils::diffit(closep)

# Calculate the centered volatility
look_back <- 21
half_back <- look_back %/% 2
returns <- rutils::diffit(closep)
volat <- roll::roll_sd(returns, width=look_back, min_obs=1)
volat <- rutils::lagit(volat, lagg=(-half_back))
# Calculate the z-scores of prices
pricescores <- (2*closep - rutils::lagit(closep, half_back, pad_zeros=FALSE) - 
                   rutils::lagit(closep, -half_back, pad_zeros=FALSE))
pricescores <- ifelse(volat > 0, pricescores/volat, 0)

# Calculate thresholds for labeling tops and bottoms
threshold_s <- quantile(pricescores, c(0.2, 0.8))
# Calculate the vectors of tops and bottoms
top_s <- (pricescores > threshold_s[2])
colnames(top_s) <- "tops"
bottom_s <- (pricescores < threshold_s[1])
colnames(bottom_s) <- "bottoms"

# Calculate SVXY and VXX prices
svx_y <- log(quantmod::Cl(rutils::etfenv$SVXY))
numrows <- NROW(svx_y)
dates <- zoo::index(svx_y)
vx_x <- log(quantmod::Cl(rutils::etfenv$VXX))
vx_x <- vx_x[dates]

# Calculate rolling VTI volatility
volat <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
volat <- xts::xts(sqrt(volat), zoo::index(ohlc))
colnames(volat) <- "volat"

# Calculate trailing z-scores of SVXY
design <- cbind(volat[dates], vx_x, closep[dates])
response <- svx_y
z_scores <- drop(HighFreq::roll_zscores(response=response, design=design, look_back=look_back))
z_scores[1:look_back] <- 0
z_scores[is.infinite(z_scores)] <- 0
z_scores[is.na(z_scores)] <- 0
# z_scores <- z_scores/sqrt(look_back)

# Calculate SVXY medians
# medi_an <- roll::roll_median(svx_y, width=look_back, min_obs=1)
# Calculate SVXY MAD
# ma_d <- HighFreq::roll_var(svx_y, look_back=look_back, method="nonparametric")
# Calculate SVXY Hampel z-scores
# svxy_scores <- ifelse(ma_d > 0, (svx_y - medi_an)/ma_d, 0)
# svxy_scores[1:look_back, ] <- 0
# Calculate SVXY z-scores
roll_svxy <- roll::roll_mean(svx_y, width=look_back, min_obs=1)
svxy_sd <- roll::roll_sd(rutils::diffit(svx_y), width=look_back, min_obs=1)
svxy_sd[1] <- 0
svxy_scores <- ifelse(svxy_sd > 0, (svx_y - roll_svxy)/svxy_sd, 0)

# Calculate VXX z-scores
roll_vxx <- roll::roll_mean(vx_x, width=look_back, min_obs=1)
vxx_sd <- roll::roll_sd(rutils::diffit(vx_x), width=look_back, min_obs=1)
vxx_sd[1] <- 0
vxx_scores <- ifelse(vxx_sd > 0, (vx_x - roll_vxx)/vxx_sd, 0)

# Calculate volatility z-scores
volat <- log(quantmod::Hi(ohlc))-log(quantmod::Lo(ohlc))
roll_vol <- roll::roll_mean(volat, width=look_back, min_obs=1)
volat_scores <- (volat - roll_vol)/roll_vol

# Calculate volume z-scores
volumes <- quantmod::Vo(ohlc)
volume_mean <- roll::roll_mean(volumes, width=look_back, min_obs=1)
volume_sd <- roll::roll_sd(rutils::diffit(volumes), width=look_back, min_obs=1)
volume_scores <- (volumes - volume_mean)/volume_sd

# Define design matrix
design <- cbind(vxx_scores, svxy_scores, volat_scores[dates], volume_scores[dates])
colnames(design) <- c("vxx", "svxy", "volat", "volume")
design <- zoo::coredata(design)
top_s <- top_s[dates]

response <- as.numeric(top_s[dates])


# Define in-sample and out-of-sample intervals
in_sample <- 1:.n_rows %/% 2)
out_sample <- .n_rows %/% 2 + 1).n_rows

# Fit in-sample logistic regression for tops
# Only volat is significant
response <- as.numeric((top_s[dates][in_sample]))

glm_tops <- glm(response[in_sample] ~ design[in_sample], family=binomial(logit))
glm_tops <- glm(response ~ design[in_sample], 
                data=design[in_sample],
                family=binomial(logit))
summary(glm_tops)

####################
# by hand

coeff <- glm_bottoms$coefficients
prob_s <- function(coeff) {
  plogis(drop(cbind(intercept=rep(1, NROW(in_sample)), design[in_sample]) %*% coeff))
}  # end prob_s
prob_s(rep(1, 5))



# Define likelihood function
likeli_hood <- function(coeff, response, design) {
  probs <- plogis(drop(design %*% coeff))
  -sum(response*log(probs) + (1-response)*log((1-probs)))
}  # end likeli_hood
# Run likelihood function
likeli_hood(rep(1, 5), 
            response=response, 
            design=cbind(intercept=rep(1, NROW(in_sample)), design[in_sample]))


# Initial parameters
par_init <- rep(1, 5)
# Find max likelihood parameters using steepest descent optimizer
optim_fit <- optim(par=par_init,
                   fn=likeli_hood, # Log-likelihood function
                   method="L-BFGS-B", # Quasi-Newton method
                   response=response,
                   design=cbind(intercept=rep(1, NROW(in_sample)), design[in_sample]), 
                   upper=rep(2, 5), # Upper constraint
                   lower=rep(-2, 5), # Lower constraint
                   hessian=TRUE)

# Optimal and actual parameters
optim_fit$par
unname(glm_tops$coefficients)

# Standard errors of parameters
sqrt(diag(solve(optim_fit$hessian)))
model_sum <- summary(glm_tops)
model_sum$coefficients[, 2]



# Find max likelihood parameters using DEoptim
optimd <- DEoptim::DEoptim(fn=likeli_hood,
                           upper=rep(2, 5), # Upper constraint
                           lower=rep(-2, 5), # Lower constraint
                           response=response,
                           design=cbind(intercept=rep(1, NROW(in_sample)), design[in_sample]), 
                           control=list(trace=FALSE, itermax=1000, parallelType=1))
# Optimal and actual parameters
optimd$optim$bestmem
unname(glm_tops$coefficients)



####################

# Fit in-sample logistic regression for bottoms
# Nothing is significant!
response <- bottom_s[dates][in_sample]
glm_bottoms <- glm(response ~ design[in_sample], family=binomial(logit))
summary(glm_bottoms)

# Find best in-sample thresholds
numrows <- NROW(response)
dates <- zoo::index(response)
returns <- returns[dates]
fitted_bottoms <- glm_bottoms$fitted.values
fitted_tops <- glm_tops$fitted.values

# Define runsimu as function of confidence levels
runsimu <- function(confi=c(0.1, 0.9)) {
  thresh_old <- quantile(fitted_bottoms, confi[1])
  bottom_s <- (fitted_bottoms < thresh_old)
  thresh_old <- quantile(fitted_tops, confi[2])
  top_s <- (fitted_tops > thresh_old)
  position_s <- rep(NA_integer_,.n_rows)
  position_s[1] <- 0
  position_s[top_s] <- (-1)
  position_s[bottom_s] <- 1
  position_s <- zoo::na.locf(position_s)
  -sum(returns*position_s)
}  # end runsimu

runsimu()
confi <- c(0.5, 0.6)
names(confi) <- c("bottom", "top")
# Find weights with maximum variance
optimd <- optim(par=confi,
                fn=runsimu,
                method="L-BFGS-B",
                upper=rep(1, 2),
                lower=rep(0, 2))
confi <- optimd$par


# Run out-of-sample
position_s <- rep(NA_integer_, NROW(out_sample))
position_s[1] <- 0

# Forecast over test data out-of-sample
coeff <- glm_bottoms$coefficients
forecasts <- plogis(drop(cbind(intercept=rep(1, NROW(out_sample)), design[out_sample]) %*% coeff))
thresh_old <- quantile(fitted_bottoms, confi[1])
bottom_s <- (fitted_bottoms < thresh_old)

coeff <- glm_tops$coefficients
forecasts <- plogis(drop(cbind(intercept=rep(1, NROW(out_sample)), design[out_sample]) %*% coeff))
thresh_old <- quantile(fitted_tops, confi[2])
top_s <- (forecasts > thresh_old)

position_s[top_s] <- (-1)
position_s[bottom_s] <- 1
position_s <- zoo::na.locf(position_s)




###############
### Create trending portfolios of similar ETFs.

# Load packages
library(rutils)

# Calculate ETF returns and volumes
# VXX with SVXY

# Calculate ETF returns and volumes
symbolv <- c("VTI", "SVXY", "VXX")
returns <- na.omit(rutils::etfenv$returns[, symbolv])
dates <- zoo::index(returns)

look_back <- 11
# Scale the volume by the rolling average volume
rets_scaled <- lapply(symbolv, function(symbol) {
  ohlc <- get(symbol, rutils::etfenv)
  returns <- rutils::diffit(log(quantmod::Cl(ohlc)))
  volumes <- quantmod::Vo(ohlc)
  volume_rolling <- roll::roll_mean(volumes, width=look_back)
  volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
  volumes <- volumes/volume_rolling
  # Divide  the returns by the volume - use trading time (volume clock)
  returns/volumes
})  # end lapply

# Scale by the High minus Low range
rets_scaled <- lapply(symbolv, function(symbol) {
  ohlc <- log(get(symbol, rutils::etfenv))
  returns <- rutils::diffit(quantmod::Cl(ohlc))
  rangev <- (quantmod::Hi(ohlc) - quantmod::Lo(ohlc))
  returns/rangev
})  # end lapply

# Scale by the rolling volatility
rets_scaled <- lapply(symbolv, function(symbol) {
  ohlc <- log(get(symbol, rutils::etfenv))
  returns <- rutils::diffit(quantmod::Cl(ohlc))
  variance <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
  colnames(variance) <- "variance"
  volat <- sqrt(variance)
  returns/volat
})  # end lapply

rets_scaled <- do.call(cbind, rets_scaled)
colnames(rets_scaled) <- do.call(rbind,(strsplit(colnames(rets_scaled), split="[.]")))[, 1]
rets_scaled <- na.omit(rets_scaled)
rets_scaled <- rets_scaled[dates]

weights <- 1/sapply(rets_scaled, sd)/100
port_f <- rets_scaled %*% weights

# PACF of AR(1) process
x11(width=6, height=5)
pacfd <- pacf(port_f, lag=10, xlab="", ylab="", main="")
abs(sum(pacfd$acf))

sum_pacf <- function(weights) {
  port_f <- rets_scaled %*% weights
  pacfd <- pacf(port_f, lag=10, plot=FALSE)
  -sum(pacfd$acf) - sum(port_f) + (1-sum(weights^2))^2
}  # end sum_pacf


# Calculate end points
interval <- 21
numrows <- NROW(rets_scaled)
num_agg <-.n_rows %/% interval
endp <- c(0,.n_rows - num_agg*interval + (0:num_agg)*interval)

# Calculate Hurst
hurs_t <- function(weights) {
  port_f <- rets_scaled %*% weights
  prices <- cumsum(port_f)
  r_s <- sapply(2:NROW(endp), function(ep) {
    indeks <- endp[ep-1]:endp[ep]
    diff(range(prices[indeks]))/sd(port_f[indeks])
  })  # end sapply
  # Calculate Hurst from single data point
  -log(mean(r_s))/log(interval) - sum(port_f) + (1-sum(weights^2))^2
}  # end sum_pacf

optimd <- optim(par=c(0.1, 0.1, 0.1), 
                fn=hurs_t,
                method="L-BFGS-B",
                upper=c(10, 10, 10),
                lower=c(0, 0, 0))
# Optimal parameters and value
weights <- optimd$par
port_f <- rets_scaled %*% weights
pacf(port_f, lag=10, xlab="", ylab="", main="")
port_f <- xts::xts(port_f, dates)

datav <- cbind(returns$VTI, port_f)
sharp_e <- sqrt(252)*sapply(datav, function(x) mean(x)/sd(x[x<0]))

colnames(pnls) <- c(paste(symbol, "Returns"), "Strategy", "Buy", "Sell")

cap_tion <- paste("Strategy for", symbol, "Returns Scaled by the Trading Volumes / \n", 
                  paste0(c("Index SR=", "Strategy SR="), sharp_e, collapse=" / "), "/ \n",
                  "Number of trades=", n_trades)

colnamev <- colnames(datav)
dygraphs::dygraph(cumsum(datav), main="Autoregressive Portfolio") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="red")

dygraphs::dygraph(datav, main="Autoregressive Portfolio") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)



###############
### Autoregressive strategy using the principal components  
# of average returns as predictors.
# Mostly in app_ar_pca_strat.R

# Load packages
library(rutils)

# Calculate ETF returns and volumes
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)
dates <- zoo::index(ohlc)
numrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)
cum_rets <- cumsum(returns)
volumes <- quantmod::Vo(ohlc)

in_sample <- 1:.n_rows %/% 4)
out_sample <- .n_rows %/% 4 + 1).n_rows

# Scale the volume by the rolling average volume
look_back <- 11
volume_rolling <- roll::roll_mean(volumes, width=look_back)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
volumes <- volumes/volume_rolling

# Divide  the returns by the volume - use trading time (volume clock)
# rets_scaled <- ifelse(volumes > 0, returns/volumes, 0)
rets_scaled <- returns/volumes

## Version using only recent returns

# First version
look_backs <- 2:25
design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x))
design <- do.call(cbind, design)
design[1, ] <- 0
design <- zoo::na.locf(design)
# sum(is.na(design))
design <- cbind(rets_scaled, design)
max_back <- last(look_backs)
# response <- rutils::lagit(design[, max_back], lagg=(-max_back))
response <- sqrt(max_back)*roll::roll_mean(returns, max_back)
response[1:(max_back-1)] <- 0
response <- rutils::lagit(response, lagg=(-max_back))

# Second version
# Define predictor matrix for forecasting
order_max <- 10
design <- lapply(1:order_max, rutils::lagit, input=returns)
design <- do.call(cbind, design)
colnames(design) <- paste0("pred_", 1:NCOL(design))
# response <- returns
response <- rutils::lagit(design[, order_max], lagg=(-order_max))


###########
## Old version
# Calculate design equal to the rolling means
look_backs <- c(5, 20, 80, 250)
# design <- lapply(look_backs, roll::roll_mean, x=returns)
# Scale the rolling means so they have similar volatities
design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(returns, x))
# design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(returns, x)/sqrt(roll::roll_var(returns, x)))
design <- do.call(cbind, design)
design[1, ] <- 0
design <- zoo::na.locf(design)
# sum(is.na(design))
colnames(design) <- paste0("back_", look_backs)
sapply(design, sd)
# Standardize (de-mean and scale) the design
# design <- lapply(design, function(x) {(x - mean(x))/sd(x)})
# design <- rutils::do_call(cbind, design)

# Define response as a rolling sum and shift it forward out-of-sample
response <- rutils::lagit(design[, 1], lagg=(-look_backs[1]))

###########
## End Old version

# Calculate covariance matrix of design
covmat <- cov(design)
# Calculate eigenvectors and eigenvalues
eigend <- eigen(covmat)

# Define predictors as the principal components of design
# eigen_vec <- eigend$vectors
predictor <- xts::xts(design %*% eigend$vectors, order.by=dates)
colnames(predictor) <- paste0("pc", 1:NCOL(predictor))
# round(cov(rates_diff), 3)
predictor <- rutils::lagit(predictor)
predictor <- cbind(rep(1,.n_rows), predictor)
colnames(predictor)[1] <- "unit"

# Calculate in-sample fitted coefficients
max_eigen <- 4
inverse <- MASS::ginv(predictor[in_sample, 1:max_eigen])
coeff <- drop(inverse %*% response[in_sample])
# Calculate out-of-sample forecasts of returns
# forecasts <- drop(predictor[out_sample, 1:3] %*% coeff[1:3])
forecasts <- drop(predictor[out_sample, 1:max_eigen] %*% coeff)
mean((returns[out_sample, ] - forecasts)^2)
drop(cor(returns[out_sample, ], forecasts))

# Lag the positions to trade in next period
position_s <- sign(rutils::lagit(forecasts))

# Calculate strategy pnls
pnls <- cumsum(position_s*returns[out_sample])
pnls <- cbind(cumsum(returns[out_sample]), pnls)
colnames(pnls) <- c(symbol, "Strategy")
dygraphs::dygraph(pnls, main="Autoregressive Strategy Performance") %>%
  dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
  dyLegend(width=500)


###############
# Case for look_backs = 2

# Calculate ETF returns and volumes
symbol <- "VTI"
ohlc <- get(symbol, rutils::etfenv)
dates <- zoo::index(ohlc)
numrows <- NROW(ohlc)
closep <- log(quantmod::Cl(ohlc))
returns <- rutils::diffit(closep)
cum_rets <- cumsum(returns)
volumes <- quantmod::Vo(ohlc)

in_sample <- 1:.n_rows %/% 2)
out_sample <- .n_rows %/% 2 + 1).n_rows

# Scale the volume by the rolling average volume
look_back <- 11
volume_rolling <- roll::roll_mean(volumes, width=look_back)
volume_rolling <- zoo::na.locf(volume_rolling, fromLast=TRUE)
volumes <- volumes/volume_rolling
# Divide  the returns by the volume - use trading time (volume clock)
rets_scaled <- returns/volumes

look_backs <- 2:5
design <- lapply(look_backs, function(x) sqrt(x)*roll::roll_mean(rets_scaled, x))
design <- do.call(cbind, design)
# look_backs <- 2
# design <- sqrt(2)*roll::roll_mean(rets_scaled, 2)
design[1, ] <- 0
design <- zoo::na.locf(design)
design <- cbind(rets_scaled, design)
max_back <- last(look_backs)

response <- sqrt(max_back)*roll::roll_mean(returns, max_back)
response[1:(max_back-1)] <- 0
response <- rutils::lagit(response, lagg=(-max_back))
covmat <- cov(design)
eigend <- eigen(covmat)
predictor <- xts::xts(design %*% eigend$vectors, order.by=dates)
predictor <- rutils::lagit(predictor)
# predictor <- cbind(rep(1,.n_rows), predictor)
inverse <- MASS::ginv(predictor[in_sample, ])
coeff <- drop(inverse %*% response[in_sample])
forecasts <- drop(predictor[out_sample, ] %*% coeff)
forecasts <- roll::roll_mean(sign(forecasts), width=max_back)
forecasts[1:(max_back-1)] <- 1
pnls <- cumsum(forecasts*returns[out_sample])
pnls <- cbind(cumsum(returns[out_sample]), pnls)
colnames(pnls) <- c(symbol, "Strategy")
dygraphs::dygraph(pnls, main="Autoregressive Strategy Performance") %>%
dyOptions(colors=c("blue", "red"), strokeWidth=2) %>%
dyLegend(width=500)



###############
### PTS Sentiment SPY trading strategy
# Load packages

# Load packages
library(rutils)
library(data.table)

# Load data with SPY sentiment from csv file
datav <- data.table::fread(file="C:/Develop/predictive/data/correlation_news_to_price_change.csv", stringsAsFactors=FALSE)
datav <- datav[, c("date", "sentiment", "close")]
colnames(datav) <- c("date", "sentiment", "SPY")
sapply(datav, class)
datav <- xts::xts(datav[, 2:3], as.Date.IDate(datav[, date]))


returns <- lapply(datav[, -1], rutils::diffit)
returns <- rutils::do_call(cbind, returns)
colnames(returns) <- c("sentiment", "SPY")
cor(rutils::lagit(returns[, 1]), returns[, 2])

x11(width=6, height=5)
plot(SPY ~ sentiment, data=returns)

colnamev <- colnames(datav)
cap_tion <- paste(colnamev, collapse=" vs ")
dygraphs::dygraph(datav, main=cap_tion) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")




###############
### Benchmark of rolling functions.

# Benchmark the speed of TTR::runMedian()
ohlc <- HighFreq::SPY["2011"]
closep <- log(drop(coredata(Cl(ohlc))))
ro_ll <- roll::roll_median(closep, width=look_back)[-(1:(look_back-1))]
rcpp_roll <- RcppRoll::roll_median(closep, n=look_back)
tt_r <- TTR::runMedian(closep, n=look_back)[-(1:(look_back-1))]
all.equal(rcpp_roll, coredata(tt_r), check.attributes=FALSE)
all.equal(ro_ll, tt_r, check.attributes=FALSE)
library(microbenchmark)
# roll::roll_median() is several times faster than the other two
summary(microbenchmark(
  ro_ll=roll::roll_median(closep, width=look_back),
  rcpp_roll=RcppRoll::roll_median(closep, n=look_back),
  tt_r=TTR::runMedian(closep, n=look_back),
  times=10))[, c(1, 4, 5)]


# Benchmark the speed of HighFreq::roll_scale()
# Use roll and TTR
returns <- rutils::diffit(closep)
med_rets <- roll::roll_median(returns, width=look_back)
mad_rets <- TTR::runMAD(returns, n=look_back)
re_scaled <- (returns - med_rets)/mad_rets
# Use HighFreq::roll_scale()
re_scaledh <- HighFreq::roll_scale(matrix(returns, ncol=1), look_back=look_back, use_median=TRUE)
re_scaledh <- drop(re_scaledh)
# Same result up to factor of qnorm(0.75)
tail(re_scaled)/tail(re_scaledh)
library(microbenchmark)
# HighFreq::roll_scale() is over twice as fast
summary(microbenchmark(
  h_freq=HighFreq::roll_scale(matrix(returns, ncol=1), look_back=look_back, use_median=TRUE),
  rcpp_roll={
    (returns - roll::roll_median(returns, width=look_back))/TTR::runMAD(returns, n=look_back)
  },
  times=10))[, c(1, 4, 5)]





###############
### Prototype of function get_data() for rutils

get_data <- function(symbolv,
                     data_dir = NULL, # the directory containing csv files
                     data_env = NULL, # the environment for writing xts into
                     startd = "2000-01-01",
                     endd = Sys.Date(),
                     date_fun = match.fun("as.Date"),
                     formatv = "%Y-%m-%d",
                     header = TRUE,
                     echo = TRUE,
                     scrub = TRUE,
                     api.key = NULL) {
  if (is.null(data_dir)) {
    # download prices from Tiingo
    output <- quantmod::getSymbols.tiingo(symbolv,
                                           env = data_env,
                                           from = startd,
                                           to = endd,
                                           adjust = TRUE,
                                           auto.assign = TRUE,
                                           api.key = api.key)
    # Adjust the OHLC prices and save back to data_env
    # output <- lapply(symbolv,
    #                   function(symbol) {
    #                     assign(symbol,
    #                            value = adjust_ohlc(get(symbol, envir = data_env)),
    #                            envir = data_env)
    #                     symbol
    #                   }
    # )  # end lapply
    invisible(output)
  } else {
    # load from csv files
    file_names <- file.path(data_dir, paste0(symbolv, ".csv"))
    invisible(sapply(file_names, function(file_name) {
      if (echo)
        cat("Loading instrument: \t", file_name, "\n")
      datav <- xts::as.xts(zoo::read.zoo(file = file_name,
                                         header = header, sep = ",",
                                         drop = FALSE,
                                         FUN = date_fun,
                                         format = formatv))
      if (scrub) {
        # overwrite NA values
        datav <- rutils::na_locf(datav)
        datav <- rutils::na_locf(datav, from_last = TRUE)
      }  # end if
      assign(rutils::get_name(colnames(datav)[1]),
             datav,
             envir = data_env)
      file_name
    }))  # end sapply
  }  # end if
}  # end get_data



###############
### PTS AAPL tick data trading strategy

# Load packages
library(rutils)
library(data.table)

## Load data with AAPL stock features from csv file
raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl20201021.csv", sep="\t")
# tail(raw_ticks)
# class(raw_ticks)
# sapply(raw_ticks, class)
# unlist(sapply(raw_ticks, function(x) if (is.numeric(x)) sum(x))) == 0
# Remove empty columns
raw_ticks <- raw_ticks[, .(timestamp=V10, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 10)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Or more recent data
raw_ticks <- data.table::fread(file="C:/Develop/predictive/data/aapl20201102.csv", sep=",")
raw_ticks <- raw_ticks[, .(timestamp=V8, seconds=V3, price=V1, volume=V2)]
# raw_ticks <- raw_ticks[, c(1:3, 8)]
# colnames(raw_ticks) <- c("timestamp", "seconds", "price", "volume")

## Bind additional pieces of data together
foo <- data.table::fread(file="C:/Develop/predictive/data/aapl20201030.csv", sep="\t")
foo <- foo[, c(1:3, 8)]
colnames(foo) <- c("price", "volume", "seconds", "timestamp")
foo <- foo[, .(timestamp, seconds, price, volume)]
bar <- (last(raw_ticks)$price - first(foo)$price)
foo[, price := (price + bar)]
raw_ticks <- rbind(raw_ticks, foo)



## Apply Hampel filter to remove price jumps

win_dow <- 111
half_window <- win_dow %/% 2
medi_an <- TTR::runMedian(raw_ticks$price, n=win_dow)
medi_an <- rutils::lagit(medi_an, lagg=-half_window, pad_zeros=FALSE)
ma_d <- TTR::runMAD(raw_ticks$price, n=win_dow)
ma_d <- rutils::lagit(ma_d, lagg=-half_window, pad_zeros=FALSE)
ma_d[1:half_window] <- 1
ma_d[ma_d == 0] <- 1

z_scores <- (raw_ticks$price - medi_an)/ma_d
z_scores[is.na(z_scores)] <- 0
z_scores[!is.finite(z_scores)] <- 0
sum(is.na(z_scores))
sum(!is.finite(z_scores))
range(z_scores)
mad(z_scores)
foo <- hist(z_scores, breaks=1000, xlim=c(-5*mad(z_scores), 5*mad(z_scores)))

thresh_old <- 3
bad_ticks <- (abs(z_scores) > thresh_old)
good_ticks <- raw_ticks[!bad_ticks]
good_ticks <- raw_ticks[volume > 10]

## Calculate a vector of returns
returns <- rutils::diffit(good_ticks$price)
numrows <- NROW(returns)


## Simple big tick contrarian strategy - trade on next tick after large volume tick

# Trade on large volume and non-zero return
big_ticks <- (good_ticks$volume >= 2000) & (abs(returns) > 0)
# Or: Trade on large volume and go flat if zero return
# big_ticks <- (good_ticks$volume >= 100)
position_s <- rep(NA_integer_,.n_rows)
position_s[1] <- 0
position_s[big_ticks] <- -sign(returns[big_ticks])
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s, 3)
pnls <- cumsum(returns*position_s)
x11(width=6, height=5)
plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(position_s))) / NROW(position_s)
# Plot dygraph
dates <- as.POSIXct(good_ticks$seconds, origin="1970-01-01")
# There are many duplicate dates:
NROW(good_ticks$seconds) == NROW(unique(good_ticks$seconds))
# Make dates unique:
dates <- xts::make.index.unique(dates)
pnls <- xts::xts(pnls, dates)
# dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, good_ticks$price)
colnamev <- c("Strategy", "AAPL")
colnames(pnls) <- colnamev
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)

datav <- cbind(good_ticks, position_s, pnls$Strategy)
data.table::fwrite(datav, file="C:/Develop/predictive/data/aapl_strategy.csv")


## Simple big tick contrarian strategy - trade on large volume ticks only

big_ticks <- raw_ticks[volume >= 400]
returns <- rutils::diffit(big_ticks$price)
# Flip position or flatten if returns == 0
position_s <- (-rutils::lagit(sign(returns)))
pnls <- cumsum(returns*position_s)
plot(pnls, t="l")
datav <- cbind(big_ticks, position_s, pnls$Strategy)
data.table::fwrite(datav, file="C:/Develop/predictive/data/aapl_strategy.csv")
# Number of trades
sum(abs(rutils::diffit(position_s))) / NROW(position_s)
# Plot dygraph
dates <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnls <- xts::xts(pnls, dates)
dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, big_ticks$price)
colnamev <- c("Strategy", "AAPL")
colnames(pnls) <- colnamev
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)


# Or always carry a position - doesn't work so well
position_s <- rep(NA_integer_, NROW(returns))
position_s[1] <- 0
position_s <- ifelse(returns > 0, -1, position_s)
position_s <- ifelse(returns < (-1), 1, position_s)
position_s <- zoo::na.locf(position_s)
# position_s <- (-rutils::lagit(sign(returns)))
position_s <- rutils::lagit(position_s)


# temp stuff
foo <- tail(raw_ticks, 33)
dates <- as.POSIXct(foo$V3, origin="1970-01-01")
ohlc <- foo[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=seconds]
sum(foo[foo$seconds == foo[33]$seconds]$volume)


# Aggregate to OHLC
# tail(raw_ticks)
ohlc <- raw_ticks[, .(open=first(price), high=max(price), low=min(price), close=last(price), volume=sum(volume)), by=seconds]
# all.equal(ohlc$seconds, unique(raw_ticks$seconds))
startd <- as.numeric(as.POSIXct("2020-10-21 09:30:00"))
endd <- as.numeric(as.POSIXct("2020-10-21 16:00:00"))
ohlc <- ohlc[seconds >= startd & seconds <= endd]
foo <- hist(ohlc$volume[-which(ohlc$volume > max(ohlc$volume)/1000)], breaks=1e3, xlim=c(0, 1000))


## Simple OHLC contrarian strategy - trade on large volume only

foo <- ohlc[volume >= 200]
returns <- rutils::diffit(foo$close)
numrows <- NROW(returns)
# foo <- hist(returns, breaks=500, xlim=c(-0.1, 0.1))
position_s <- (-rutils::lagit(sign(returns)))
pnls <- cumsum(returns*position_s)
plot(pnls, t="l")

foo <- lapply(100*(2:10), function(x) {
  returns <- rutils::diffit(ohlc[volume >= x]$close)
  position_s <- (-rutils::lagit(sign(returns)))
  cumsum(returns*position_s)
})  # end lapply
sapply(foo, NROW)
sapply(foo, last)


## AR strategy for OHLC - too complicated?

# Calculate a vector of returns
returns <- rutils::diffit(ohlc$close)
numrows <- NROW(returns)
hist(returns, breaks=500, xlim=c(-0.1, 0.1))

in_sample <- 1:.n_rows %/% 2)
out_of_sample <- .n_rows %/% 2 + 1).n_rows

order_max <- 5  # Define maximum order parameter
look_back <- 5

predictor <- rutils::roll_sum(returns, look_back=look_back)
# Shift the response forward into out-of-sample
response <- rutils::lagit(predictor, lagg=(-look_back))
# Define predictor matrix for forecasting
predictor <- sapply(1+look_back*(0:order_max), rutils::lagit,
                     input=predictor)
predictor <- cbind(rep(1,.n_rows), predictor)
colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
# Calculate forecasts as function of ordern
forecasts <- lapply(2:NCOL(predictor), function(ordern) {
  # Calculate fitted coefficients
  inverse <- MASS::ginv(predictor[in_sample, 1:ordern])
  coeff <- drop(inverse %*% response[in_sample])
  # Calculate out-of-sample forecasts of returns
  drop(predictor[out_of_sample, 1:ordern] %*% coeff)
})  # end lapply
names(forecasts) <- paste0("p=", 2:NCOL(predictor))

# Calculate out-of-sample PnLs
pnls <- sapply(forecasts, function(x) {
  cumsum(sign(x)*returns[out_of_sample])
})  # end sapply
colnames(pnls) <- names(forecasts)
pnls <- xts::xts(pnls, index(prices[out_of_sample]))
plot(pnls[, 1], t="l")
plot.zoo(pnls)



## Simple contrarian strategy using Hampel filter - doesn't work too well

# Calculate a time series of rolling z-scores
win_dow <- 5
# prices <- big_ticks$price
returns <- rutils::diffit(big_ticks$price)
medi_an <- TTR::runMedian(returns, n=win_dow)
medi_an[1:win_dow] <- 1
# sum(is.na(medi_an))
ma_d <- TTR::runMAD(returns, n=win_dow)
ma_d[1:win_dow] <- 1
ma_d[ma_d < 1e-6] <- 1
# sum(is.na(ma_d))
z_scores <- ifelse(ma_d!=0, (returns-medi_an)/ma_d, 0)
z_scores[1:win_dow] <- 0
# sum(is.na(z_scores))
# ma_d <- zoo::na.locf(z_scores)
# mad_zscores <- TTR::runMAD(z_scores, n=win_dow)
# mad_zscores[1:win_dow, ] <- 0

tail(z_scores)
mad(z_scores)
range(z_scores)
x11(width=6, height=5)
hist(z_scores, breaks=200, xlim=c(-5, 5), freq=FALSE)

# Calculate position_s and pnls from z-scores
position_s <- rep(NA_integer_, NROW(returns))
position_s[1] <- 0
# thresh_old <- 3*mad(z_scores)
thresh_old <- 1
position_s <- ifelse(z_scores > thresh_old, -1, position_s)
position_s <- ifelse(z_scores < (-thresh_old), 1, position_s)
# position_s <- ifelse(z_scores > 2*mad_zscores, -1, position_s)
# position_s <- ifelse(z_scores < (-2*mad_zscores), 1, position_s)
position_s <- zoo::na.locf(position_s)
# Number of trades
# sum(abs(rutils::diffit(position_s))) / NROW(position_s)
positions_lag <- rutils::lagit(position_s, lagg=2)
pnls <- cumsum(positions_lag*returns)

plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(position_s))) / NROW(position_s)
# Plot dygraph
dates <- as.POSIXct(big_ticks$seconds, origin="1970-01-01")
pnls <- xts::xts(pnls, dates)
dygraphs::dygraph(pnls)
# Combine index with AAPL
pnls <- cbind(pnls, big_ticks$price)
colnamev <- c("Strategy", "AAPL")
colnames(pnls) <- colnamev
dygraphs::dygraph(pnls, main="AAPL Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)




############### temp

indic <- rutils::diffit(position_s)
indic_buy <- (indic > 0)
indic_sell <- (indic < 0)
cumsumv <- cumsum(returns)


pnls <- cbind(pnls, cumsumv[indic_buy], cumsumv[indic_sell])
colnames(pnls)[3:4] <- c("Buy", "Sell")

colnamev <- colnames(pnls)
dygraphs::dygraph(pnls, main=cap_tion) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue") %>%
  dySeries(name=colnamev[3], axis="y2", label=colnamev[3], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="orange") %>%
  dySeries(name=colnamev[4], axis="y2", label=colnamev[4], drawPoints=TRUE, strokeWidth=0, pointSize=5, col="green")




############### homework
# Summary: Study how the dispersion of the Hampel z-scores 
# depends on the level of volatility in the interval.
# Yes, z_scores have higher dispersion on more volatile days.
# But so what?

ohlc <- HighFreq::SPY["T09:31:00/T15:59:00"]
# ohlc <- rutils::etfenv$VTI
numrows <- NROW(ohlc)
ohlc <- log(ohlc[, 1:4])
closep <- Cl(ohlc)
# Calculate the z_scores
look_back <- 11
medi_an <- TTR::runMedian(closep, n=look_back)
medi_an[1:look_back, ] <- 1
z_scores <- (closep-medi_an)
z_scores[1:look_back, ] <- 0
mad_zscores <- TTR::runMAD(z_scores, n=10*look_back)
mad_zscores[1:(10*look_back), ] <- 0
z_scores <- ifelse(mad_zscores != 0, z_scores/mad_zscores, 0)

# Calculate the log variance for SPY
variance <- xts::apply.daily(ohlc, HighFreq::calc_var_ohlc)
# For VTI
# variance <- sapply((2*look_back).n_rows, function(ro_w) {
#   HighFreq::calc_var_ohlc(ohlc[(ro_w-look_back+1):ro_w, ], scale=FALSE)
# })  # end sapply
# variance <- c(variance[1]+numeric(2*look_back-1), variance)
# x11(width=6, height=5)
# plot(variance)
# Plot the VTI volatility
vo_l <- sqrt(variance)
# vo_l <- xts::xts(sqrt(variance), index(ohlc))
dygraphs::dygraph(vo_l, main="VTI Volatility")


# Plot z_scores versus volatility for VTI
plot(as.numeric(z_scores) ~ as.numeric(vo_l))


# Calculate dates with high volatility
is_high <- (vo_l > max(vo_l)/10)
is_high <- is_high[is_high]
high_days <- index(is_high)
high_days <- as.Date(high_days)
# low_days <- high_days[!high_days]

dates <- index(ohlc)
dates <- as.Date(dates)

is_high <- dates %in% high_days
high_scores <- z_scores[is_high]
low_scores <- z_scores[!is_high]

# Plot histogram of z_scores
range(low_scores)
low_scores <- low_scores[low_scores > quantile(low_scores, 0.05)]
low_scores <- low_scores[low_scores < quantile(low_scores, 0.95)]
x11(width=6, height=5)
hist(low_scores, xlim=c(quantile(low_scores, 0.05), quantile(low_scores, 0.95)), breaks=50, main=paste("low_scores", "look_back =", look_back))

range(high_scores)
high_scores <- high_scores[high_scores > quantile(high_scores, 0.05)]
high_scores <- high_scores[high_scores < quantile(high_scores, 0.95)]
x11(width=6, height=5)
hist(high_scores, xlim=c(quantile(high_scores, 0.05), quantile(high_scores, 0.95)), breaks=50, main=paste("high_scores", "look_back =", look_back))



###############
### PTS AAPL features PCA dimension reduction trading strategy

# Load packages
library(rutils)

# Load data with AAPL stock features from csv file
datav <- data.table::fread(file="C:/Develop/predictive/data/jerzy_aapl20200720.csv", stringsAsFactors=FALSE)
returns <- datav$price_change_plus5min
datav <- datav[, -"price_change_plus5min"]
datav <- as.matrix(datav)
cor_vec <- drop(cor(returns, datav))
barplot(cor_vec, main="Correlations of Features to the AAPL Returns")
data_scaled <- scale(datav, center=TRUE, scale=TRUE)
sd_data <- apply(datav, MARGIN=2, sd)
meandata <- apply(datav, MARGIN=2, mean)

# Calculate correlation matrix
cormat <- cor(data_scaled)
# Reorder correlation matrix based on clusters
library(corrplot)
ordern <- corrMatOrder(cormat,
                       order="hclust",
                       hclust.method="complete")
cormat <- cormat[ordern, ordern]
# Plot the correlation matrix
colors <- colorRampPalette(c("red", "white", "blue"))
x11()
corrplot(cormat, title="AAPL Features Correlation Matrix",
         tl.col="black", tl.cex=0.8, mar=c(0,0,1,0),
         method="square", col=colors(8),
         cl.offset=0.75, cl.cex=0.7,
         cl.align.text="l", cl.ratio=0.25)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(cormat, k=NROW(cormat) %/% 2,
                method="complete", col="red")


# Perform PCA
pcad <- prcomp(data_scaled, scale=FALSE)
# Plot barplots with PCA vectors weights in multiple panels
x11()
nweights <- 6
par(mfrow=c(nweights/2, 2))
par(mar=c(2, 2, 2, 1), oma=c(0, 0, 0, 0))
for (ordern in 1:nweights) {
  barplot(pcad$rotation[, ordern], las=3, xlab="", ylab="", main="")
  title(paste0("PC", ordern), line=-2.0, col.main="red")
} # end for

# Inspect principal component time series
round(cor(pcad$x), 4)
plot(pcad$x[, 1], t="l")

# Calculate correlations of principal component time series and returns
returns_std <- (returns - mean(returns))/sd(returns)
s_d <- apply(pcad$x, MARGIN=2, sd)
# pcats <- scale(pcad$x, center=TRUE, scale=TRUE)
cor_vec <- cor(returns, pcad$x)
# apply(returns_std*pcats, MARGIN=2, sum)/NROW(returns_std)
# Calculate weights equal to correlations
weights <- cor_vec/s_d
x11()
barplot(weights)

# Invert all the principal component time series
inv_rotation <- solve(pcad$rotation)
weights_solved <- drop(weights %*% inv_rotation)
weights_solved <- weights_solved/sd_data
foo <- drop(datav %*% weights_solved)
cor(returns, foo)
barplot(weights_solved)
barplot(weights_solved, main="Weights of Features in New Feature")


# Simulate trading strategy
position_s <- rep(NA_integer_, NROW(returns))
position_s[1] <- 0
# Long positions
# indica_tor <- (datav[, feature4] + datav[, feature5])
indica_tor <- (datav[, feature4] + datav[, feature5])
position_s <- indica_tor
# position_s <- ifelse(indica_tor >= lagg, 1, position_s)
# Short positions
# indica_tor <- ((closep - vwapv) < (-thresh_old*rangev))
# indica_tor <- HighFreq::roll_count(indica_tor)
# position_s <- ifelse(indica_tor >= lagg, -1, position_s)
# position_s <- zoo::na.locf(position_s, na.rm=FALSE)
# Lag the positions to trade in next period
position_s <- rutils::lagit(position_s, lagg=1)
pnls <- cumsum(coeff*returns*position_s)
plot(pnls[(1e3*(1:(NROW(returns) %/% 1e3)))], t="l")




###############
### Backtest rescaled range strategy

# Load packages
library(HighFreq)

# Calculate rolling rescaled cumulative returns from OHLC data
roll_range <- function(ohlc, look_back=11) {
  returns <- rutils::diffit(ohlc[, 4])
  rangev <- HighFreq::roll_sum(returns, look_back=look_back)
  var_rolling <- sqrt(HighFreq::roll_var_ohlc(ohlc, look_back=look_back, scale=FALSE))
  look_back <- sqrt(look_back)
  hurst_rolling <- ifelse((var_rolling==0) | (rangev==0),
                          0.0,
                          rangev/var_rolling/look_back)
  # Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  # end roll_range


# Calculate rolling rescaled cumulative returns from returns data
roll_range <- function(returns, cum_returns, look_back=11) {
  rangev <- HighFreq::roll_sum(returns, look_back=look_back)
  var_rolling <- sqrt(HighFreq::roll_var(returns, look_back=look_back))
  look_back <- sqrt(look_back)
  hurst_rolling <- ifelse((var_rolling==0) | (rangev==0),
                          0.0,
                          rangev/var_rolling/look_back)
  # Colnames(hurst_rolling) <- paste0(rutils::get_name(colnames(ohlc)[1]), ".Hurst")
  rutils::na_locf(hurst_rolling)
}  # end roll_range


returns <- rutils::diffit(drop(zoo::coredata(quantmod::Cl(HighFreq::SPY))))
numrows <- NROW(returns)
dim(returns) <- c.n_rows, 1)
# cum_returns <- cumsum(returns)



# Calculate rolling Hurst for SPY
hurst_rolling <- HighFreq::roll_hurst(ohlc=HighFreq::SPY, look_back=7)
# Calculate rolling rescaled range
hurst_rolling <- roll_range(ohlc=HighFreq::SPY, look_back=5)
hurst_rolling <- roll_range(returns=returns, look_back=5)
# chart_Series(hurst_rolling["2009-03-10/2009-03-12"], name="SPY hurst_rolling")

# thresh_old <- 0.5

x11(width=6, height=5)
quantiles <- quantile(hurst_rolling, c(0.01, 0.99))
hist(hurst_rolling, xlim=quantiles, breaks=1e2)


# Trade on rescaled range

quantiles <- quantile(hurst_rolling, c(0.2, 0.8))
position_s <- rep(NA_integer_,.n_rows)
position_s[1] <- 0
# Flip only if two consecutive signals in same direction
position_s <- ifelse((re_scaled > quantiles[2]) & (rescaled_lag > quantiles[2]), -1, position_s)
position_s <- ifelse((re_scaled < quantiles[1]) & (rescaled_lag < quantiles[1]), 1, position_s)
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lagit(position_s, 2)
pnls <- cumsum(returns*position_s)
x11(width=6, height=5)
plot(pnls, t="l")
# Number of trades
sum(abs(rutils::diffit(position_s))) / NROW(position_s)



###############
### Backtest momentum strategy

# Load packages
library(rutils)

load("C:/Develop/lecture_slides/data/sp500_returns.RData")
returns100[1, ] <- 0
returns100 <- zoo::na.locf(returns100, na.rm=FALSE)
indeks <- cumsum(rowMeans(returns100))

lagg <- 5
sum_roll <- rutils::roll_sum(returns100, look_back=lagg)
weights <- matrixStats::rowRanks(sum_roll)
weights <- (weights - 50)
weights <- rutils::lagit(weights, lagg=1)

wealth <- -weights*returns100
wealth <- -weights*rutils::lagit(sum_roll, lagg=(-lagg))
wealth <- cumsum(rowMeans(wealth))
x11()
plot(wealth, t="l")
wealth <- xts::xts(wealth, index(returns100))
dygraphs::dygraph(wealth)

# Combine index with AAPL
wealth <- cbind(wealth, indeks)
# wealth <- xts(wealth, index(returns100))
colnamev <- c("Strategy", "Index")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)




# Define backtest functional for daily momentum strategy
# If tre_nd=(-1) then it backtests a mean reverting strategy
momentum_daily <- function(returns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- roll::roll_var(returns, width=look_back)
  variance <- zoo::na.locf(variance, na.rm=FALSE)
  # variance[is.na(variance)] <- 1
  variance[variance <= 0] <- 1
  # Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back)
  past[1:look_back, ] <- 1
  weights <- past/sqrt(variance)
  # weights <- ifelse(variance > 0, past/sqrt(variance), 0)
  # weights[variance == 0] <- 0
  weights[1:look_back, ] <- 1
  weights <- weights/sqrt(rowSums(weights^2))
  weights[is.na(weights)] <- 0
  weights <- rutils::lagit(weights, 2)
  # Calculate momentum profits and losses
  future <- rutils::lagit(past, (-look_back))
  pnls <- tre_nd*rowMeans(weights*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  cumsum(pnls - costs)
}  # end momentum_daily


wealth <- momentum_daily(returns=returns100, look_back=5, bid_offer=0, tre_nd=(-1))

# Combine index with AAPL
wealth <- cbind(wealth, indeks)
wealth <- xts(wealth, index(returns100))
colnamev <- c("Strategy", "Index")
colnames(wealth) <- colnamev
dygraphs::dygraph(wealth, main="Momentum S&P500 Mean Reverting Strategy") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)



###############
### Relationship between liquidity (turnover) and linear dependence (correlation).

# Do illiquid stocks with a lower dollar turnover have significant autocorrelation?

# The answer is that there's no significant relationship between liquidity (turnover) 
# and linear dependence (correlation).

# Load packages
library(rutils)

load("C:/Develop/lecture_slides/data/sp500.RData")

startd <- "2000-01-01"
lagg <- 25
datav <- lapply(sp500env, function(ohlc) {
  if (start(ohlc) < startd) {
    prices <- quantmod::Cl(ohlc)
    volumes <- quantmod::Vo(ohlc)
    returns <- rutils::diffit(log(prices))
    # Calculate autocorrelations from PACF
    p_acf <- pacf(na.omit(returns), plot=FALSE)
    c(turnover=sum(volumes*prices), dependence=sum(p_acf$acf))
    # Calculate autocorrelations from Hurst
    # endp <- rutils::calc_endpoints(ohlc, lagg)
    # c(turnover=sum(volumes*prices), dependence=calc_hurst_hilo(Hi(ohlc), Lo(ohlc), endp))
  } else NULL
})  # end lapply

# Bind and sort
datav <- do.call(rbind, datav)
datav <- datav[order(datav[, "dependence"], decreasing=FALSE), ]

# Plot
x11()
hist(datav)
plot(datav)
chart_Series(quantmod::Cl(sp500env$TYL))
dygraphs::dygraph(quantmod::Cl(sp500env$TYL))



###############
### Test autoregressive strategy for all ETFs in rutils::etfenv

library(rutils)
ordern <- 3
look_back <- 5

back_test <- function(symbol) {
  prices <- log(quantmod::Cl(get(symbol, rutils::etfenv)))
  returns <- rutils::diffit(prices)
  returns <- as.numeric(returns)
 .n_rows <- NROW(returns)
  in_sample <- 1:.n_rows %/% 2)
  out_of_sample <- .n_rows %/% 2 + 1).n_rows
  predictor <- rutils::roll_sum(returns, look_back=look_back)
  response <- rutils::lagit(predictor, lagg=(-look_back))
  predictor <- sapply(1+look_back*(0:ordern), rutils::lagit, input=predictor)
  predictor <- cbind(rep(1,.n_rows), predictor)
  colnames(predictor) <- paste0("pred_", 1:NCOL(predictor))
  inverse <- MASS::ginv(predictor[in_sample, 1:ordern])
  coeff <- drop(inverse %*% response[in_sample])
  forecasts <- drop(predictor[out_of_sample, 1:ordern] %*% coeff)
  sign(forecasts)*returns[out_of_sample]
}  # end back_test


test_s <- lapply(rutils::etfenv$symbolv, back_test)
names(test_s) <- rutils::etfenv$symbolv
sort(sapply(test_s, sum))

forecasts <- test_s[[1]]
pnls <- xts(cumsum(sign(forecasts)*returns[out_of_sample]), index(prices[out_of_sample]))



###############
### Variance ratios

# Find stocks with largest variance ratios
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
symbolv <- colnames(returns)
nweights <- NROW(symbolv)
lagg <- 25
vr_s <- sapply(returns, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 500)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)

# Find ETFs with largest variance ratios
returns <- rutils::etfenv$returns
symbolv <- colnames(returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "IEF"))]
returns <- returns[, symbolv]
vr_s <- sapply(returns, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)
# symbolv <- names(vr_s)

# Find PCAs with largest variance ratios
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
pcad <- prcomp(returns, scale=TRUE)
pcarets <- xts(pcad$x/100, order.by=index(returns))
vr_s <- sapply(pcarets, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)
pcarets <- pcarets[, names(vr_s)]
save(pcarets, file="/Volumes/external/Develop/data/pcarets.RData")
x11()
dygraphs::dygraph(cumsum(pcarets[, "PC2"]))
barplot(sort(pcad$rotation[, "PC2"]))

# Second PCA
cumsumv <- cumsum(pcarets)
endp <- rutils::calc_endpoints(pcarets, interval=5)
cumsumv <- cumsumv[endp, ]
cumsumv <- rutils::diffit(cumsumv)
pcad <- prcomp(cumsumv, scale=TRUE)
pcarets <- xts(pcad$x/100, order.by=index(cumsumv))
vr_s <- sapply(pcarets, function(return) {
  return <- na.omit(return)
  if (NROW(return) > 100)
    calc_var(return, lagg)/calc_var(return)/lagg
  else NULL
})  # end sapply
vr_s <- sort(unlist(vr_s), decreasing=TRUE)


## optim
object_ive <- function(weights, returns, lagg) {
  returns <- (returns %*% weights)
  -calc_var(returns, lagg)/calc_var(returns)/lagg
}  # end object_ive

symbolv <- colnames(returns)
nweights <- NROW(symbolv)
optimd <- optim(par=rep(1/nweights, nweights),
                fn=object_ive,
                returns=returns,
                lagg=lagg,
                method="L-BFGS-B",
                upper=rep(10, nweights),
                lower=rep(-10, nweights))
# Optimal parameters
weights <- optimd$par
# weights <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weights)
names(weights) <- colnames(returns)
object_ive(weights, returns, lagg)
optimd$value
pnls <- cumsum(returns %*% weights)
pnls <- xts::xts(pnls, zoo::index(returns))
dygraphs::dygraph(pnls)


# DEoptim
optimd <- DEoptim::DEoptim(object_ive,
                           returns=returns,
                           lagg=lagg,
                           upper=rep(10, nweights),
                           lower=rep(-10, nweights),
                           control=list(trace=FALSE, itermax=500))

# Extract optimal parameters into weights vector
weights <- optimd$optim$bestmem
# weights <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weights)
names(weights) <- colnames(returns)
object_ive(weights, returns, lagg)
pnls <- cumsum(returns %*% weights)
pnls <- xts::xts(pnls, zoo::index(returns))
dygraphs::dygraph(pnls)



###############
### Hurst stuff

lagg <- 25
ohlc <- rutils::etfenv$VTI
endp <- rutils::calc_endpoints(ohlc, lagg)
highp <- Hi(ohlc)
lowp <- Lo(ohlc)
calc_hurst_hilo(highp, lowp, endp)

# Find ETFs with largest Hurst
hurst_s <- sapply(rutils::etfenv$symbolv, function(symbol) {
  ohlc <- get(symbol, rutils::etfenv)
  endp <- rutils::calc_endpoints(ohlc, lagg)
  highp <- Hi(ohlc)
  lowp <- Lo(ohlc)
  calc_hurst_hilo(highp, lowp, endp)
})  # end eapply
hurst_s <- sort(hurst_s, decreasing=TRUE)

plot(hurst_s, vr_s)
text(x=hurst_s, y=vr_s, labels=names(vr_s))


# Calculate Hurst from returns
endp <- rutils::calc_endpoints(returns, lagg)
hurst_s <- sapply(returns, calc_hurst_rets, endp)
hurst_s <- sort(hurst_s, decreasing=TRUE)

# Find stocks with largest Hurst
load("C:/Develop/lecture_slides/data/sp500.RData")
startd <- "2000-01-01"
hurst_s <- eapply(sp500env, function(ohlc) {
  # ohlc <- get(symbol, rutils::etfenv)
  # Check if data starts before 2000
  if (start(ohlc) < startd) {
    ohlc <- ohlc[paste0("/", startd)]
    endp <- rutils::calc_endpoints(ohlc, lagg)
    highp <- Hi(ohlc)
    lowp <- Lo(ohlc)
    calc_hurst_hilo(highp, lowp, endp)
  } else NULL
})  # end eapply
hurst_s <- sort(unlist(hurst_s), decreasing=TRUE)
symbolv <- names(hurst_s[1:100])
load("C:/Develop/lecture_slides/data/sp500_returns.RData")
returns <- returns[, symbolv]
save(returns, file="C:/Develop/lecture_slides/data/sp100_rets_hurst.RData")



## Find portfolio with largest Hurst

# Vector of initial portfolio weights
weights <- rep(1/nweights, nweights)

# object_ive with shrinkage
object_ive <- function(weights, returns, endp) {
  -calc_hurst_rets(returns %*% weights, endp)
}  # end object_ive


# Portfolio optimization using principal components
# Perform PCA
pcad <- prcomp(returns, center=TRUE, scale=TRUE)
# eigend <- eigen(cor(returns))
# all.equal(abs(pcad$rotation), abs(eigend$vectors), check.attributes=FALSE)
# Calculate principal component time series
rets_pca <- scale(returns) %*% pcad$rotation
# all.equal(pcad$x, rets_pca, check.attributes=FALSE)
round(cor(rets_pca), 4)
# Calculate the returns from the principal component
# time series rets_pca:
rot_inv <- solve(pcad$rotation)
solved <- rets_pca %*% rot_inv

hurst_pca <- apply(rets_pca, 2, calc_hurst_rets, endp=endp)
sort(hurst_pca, decreasing=TRUE)

optimd <- optim(par=rep(1/nweights, nweights),
                fn=object_ive,
                returns=rets_pca,
                endp=endp,
                method="L-BFGS-B",
                upper=rep(10, nweights),
                lower=rep(-10, nweights))
# Optimal parameters
weights <- optimd$par
weights <- 0.01*weights/sd(rets_pca %*% weights)
# weights <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weights)
names(weights) <- colnames(rets_pca)
object_ive(weights, rets_pca, endp)
optimd$value


# Find portfolio with largest Hurst
object_ive <- function(weights, returns, endp) {
  -calc_hurst_rets(returns %*% weights, endp)
  + (sum(weights^2) - 1)^2
}  # end object_ive
# library(parallel)
# num_cores <- detectCores()
# cluster <- makeCluster(num_cores-1)
# clusterExport(cluster, varlist=c("calc_hurst_rets"))
optimd <- DEoptim::DEoptim(object_ive,
                           returns=rets_pca,
                           endp=endp,
                           upper=rep(10, nweights),
                           lower=rep(-10, nweights),
                           # cluster=cluster,
                           # parVar=c("calc_hurst_rets"),
                           control=list(trace=FALSE, itermax=500, parVar=c("calc_hurst_rets"), parallelType=1))
# Stop R processes over cluster under Windows
# stopCluster(cluster)

# Extract optimal parameters into weights vector
weights <- optimd$optim$bestmem
weights <- 0.01*weights/sd(rets_pca %*% weights)
# weights <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weights)
names(weights) <- colnames(rets_pca)
object_ive(weights, rets_pca, endp)


# Plot wealth
portf_hurst <- drop(rets_pca %*% weights)
calc_hurst_rets(portf_hurst, endp)
portf_hurst <- xts::xts(portf_hurst, index(returns))
colnames(portf_hurst) <- "max_hurst_deopt"
wealth <- cumsum(portf_hurst)
# wealth <- cumprod(1 + portf_hurst)
datav <- cbind(Cl(rutils::etfenv$VEU)[index(returns)], wealth)
colnames(datav)[1] <- "VEU"
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red")



## Calculate autocorrelations
returns <- rutils::etfenv$returns
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
returns_lag <- rutils::lagit(returns)
auto_cor <- sapply(colnames(returns), function(symbol) {
  returns <- returns[, symbol]
  mean(returns*returns_lag[, symbol])/var(returns)
})  # end sapply
auto_cor <- sort(auto_cor, decreasing=TRUE)


## Find portfolio with largest autocorrelations

# Vector of initial portfolio weights
returns <- rutils::etfenv$returns
symbolv <- colnames(returns)
symbolv <- symbolv[!(symbolv %in% c("VXX", "SVXY", "MTUM", "IEF"))]
returns <- returns[, symbolv]
returns[1, is.na(returns[1, ])] <- 0
returns <- zoo::na.locf(returns, na.rm=FALSE)
returns_lag <- rutils::lagit(returns)

nweights <- NCOL(returns)
weights <- rep(1/nweights, nweights)

# object_ive with shrinkage
object_ive <- function(weights, returns, returns_lag) {
  returns <- returns %*% weights
  -drop(mean(returns*(returns_lag %*% weights))/var(returns))
}  # end object_ive
object_ive(weights, returns, returns_lag)


optimd <- optim(par=rep(1/nweights, nweights),
                fn=object_ive,
                returns=returns,
                returns_lag=returns_lag,
                method="L-BFGS-B",
                upper=rep(10, nweights),
                lower=rep(-10, nweights))
# Optimal parameters
weights <- optimd$par
names(weights) <- colnames(returns)
weights <- sort(weights, decreasing=TRUE)
object_ive(weights, returns, returns_lag)
optimd$value
# wippp
pnls <- xts::xts(cumsum(returns %*% weights), zoo::index(returns))
dygraphs::dygraph(pnls)

# DEoptim
optimd <- DEoptim::DEoptim(object_ive,
                           returns=returns,
                           returns_lag=returns_lag,
                           upper=rep(10, nweights),
                           lower=rep(-10, nweights),
                           # cluster=cluster,
                           # parVar=c("calc_hurst_rets"),
                           control=list(trace=FALSE, itermax=500, parallelType=1))
# Extract optimal parameters into weights vector
weights <- optimd$optim$bestmem
weights <- 0.01*weights/sd(rets_pca %*% weights)
# weights <- weights*sd(rowMeans(rets_pca))/sd(rets_pca %*% weights)
names(weights) <- colnames(returns)
sort(weights, decreasing=TRUE)
object_ive(weights, returns, returns_lag)
pnls <- xts::xts(cumsum(returns %*% weights), zoo::index(returns))
dygraphs::dygraph(pnls)


# Plot wealth
portf_hurst <- drop(rets_pca %*% weights)
calc_hurst_rets(portf_hurst, endp)
portf_hurst <- xts::xts(portf_hurst, index(returns))
colnames(portf_hurst) <- "max_hurst_deopt"
wealth <- cumsum(portf_hurst)
# wealth <- cumprod(1 + portf_hurst)
datav <- cbind(Cl(rutils::etfenv$VEU)[index(returns)], wealth)
colnames(datav)[1] <- "VEU"
colnamev <- colnames(datav)
dygraphs::dygraph(datav, main="Max Hurst vs VEU") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="blue") %>%
  dySeries(name=colnamev[2], axis="y2", col="red")




# More stuff below

datav <- exp(cumsum(rnorm(1e7)/100))
dates <- seq.POSIXt(from=Sys.time(), by="sec", length.out=NROW(datav))
datav <- xts(datav, dates)

interval_s <- seq.int(from=1e2, to=1e3, by=1e2)
vol_s <- sapply(interval_s, function(interval) {
  # spy_agg <- rutils::to_period(ohlc=datav, k=interval)
  # HighFreq::calc_var_ohlc(spy_agg)
  endp <- rutils::calc_endpoints(datav, interval=interval)
  sd(rutils::diffit(log(datav[endp])))
})  # end sapply

interval_s <- c("seconds", "minutes", "hours", "days")
inter_log <- log(c(1, 60, 3600, 6.5*3600))
inter_log <- log(c(1, 60, 3600, 24*3600))
vol_s <- sapply(interval_s, function(interval) {
  spy_agg <- rutils::to_period(ohlc=HighFreq::SPY, k=interval)
  # spy_agg <- rutils::to_period(ohlc=HighFreq::SPY, period=interval)
  # HighFreq::calc_var_ohlc(spy_agg)
  returns <- rutils::diffit(spy_agg[, 4])
  sd(returns)
})  # end sapply


names(vol_s) <- paste0("agg_", interval_s)
vol_log <- log(vol_s)
inter_log <- log(interval_s)
inter_log <- inter_log - mean(inter_log)
vol_log <- vol_log - mean(vol_log)
model <- lm(vol_log ~ inter_log)
hurst_lm <- summary(model)$coeff[2, 1]
hurs_t <- sum(vol_log*inter_log)/sum(inter_log^2)
all.equal(hurst_lm, hurs_t)



###############
### Backtests

# Define backtest functional
backtest_rolling <- function(returns, look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- HighFreq::roll_var(returns_weighted, look_back=look_back)
  variance <- zoo::na.locf(variance, na.rm=FALSE)
  variance[is.na(variance)] <- 0
  # Calculate rolling Sharpe
  past <- roll::roll_mean(returns, width=look_back)
  weights <- past/sqrt(variance)
  weights[variance == 0] <- 0
  weights[1:look_back, ] <- 1
  weights <- weights/sqrt(rowSums(weights^2))
  weights[is.na(weights)] <- 0
  weights <- rutils::lagit(weights)
  # Calculate momentum profits and losses
  pnls <- tre_nd*rowMeans(weights*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  cumprod(1 + pnls - costs)
}  # end backtest_rolling


# Define backtest functional
backtest_weighted <- function(returns, returns_weighted,
                              look_back=252, bid_offer=0.001, tre_nd=1, ...) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  # Calculate rolling variance
  variance <- HighFreq::roll_var(returns_weighted, look_back=look_back)
  variance <- zoo::na.locf(variance, na.rm=FALSE)
  variance[is.na(variance)] <- 0
  # Calculate rolling Sharpe
  past <- roll::roll_mean(returns_weighted, width=look_back)
  weights <- past/sqrt(variance)
  weights[variance == 0] <- 0
  weights[1:look_back, ] <- 1
  weights <- weights/sqrt(rowSums(weights^2))
  weights[is.na(weights)] <- 0
  weights <- rutils::lagit(weights)
  # Calculate momentum profits and losses
  pnls <- tre_nd*rowMeans(weights*returns)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*rowSums(abs(rutils::diffit(weights)))
  cumprod(1 + pnls - costs)
}  # end backtest_weighted


# Define backtest functional
backtestmomentum <- function(returns, returns_weighted,
                              objfunc=function(returns) (sum(returns)/sd(returns)),
                              look_back=12, re_balance="months", bid_offer=0.001,
                              endp=rutils::calc_endpoints(returns, interval=re_balance),
                              with_weights=FALSE, ...) {
  stopifnot("package:rutils" %in% search() || require("rutils", quietly=TRUE))
  # Define look-back and look-forward intervals
 .n_rows <- NROW(endp)
  startp <- c(rep_len(1, look_back-1), endp[1:.n_rows-look_back+1)])
  # Calculate look-back intervals
  look_backs <- cbind(startp, endp)
  # Calculate look-forward intervals
  look_fwds <- cbind(endp + 1, rutils::lagit(endp, -1))
  look_fwds.n_rows, 1] <- endp.n_rows]
  # Calculate past performance over look-back intervals
  past <- t(apply(look_backs, 1, function(ep) sapply(returns_weighted[ep[1]:ep[2]], objfunc)))
  past[is.na(past)] <- 0
  # Calculate future performance
  future <- t(apply(look_fwds, 1, function(ep) sapply(returns[ep[1]:ep[2]], sum)))
  future[is.na(future)] <- 0
  # Scale weights so sum of squares is equal to 1
  weights <- past
  weights <- weights/sqrt(rowSums(weights^2))
  weights[is.na(weights)] <- 0  # Set NA values to zero
  # Calculate momentum profits and losses
  pnls <- rowSums(weights*future)
  # Calculate transaction costs
  costs <- 0.5*bid_offer*cumprod(1 + pnls)*rowSums(abs(rutils::diffit(weights)))
  pnls <- (pnls - costs)
  if (with_weights)
    rutils::lagit(cbind(pnls, weights))
  else
    rutils::lagit(pnls)
}  # end backtestmomentum


# Perform sapply loop over look_backs
endp <- rutils::calc_endpoints(returns, interval="weeks")
look_backs <- seq(3, 15, by=1)
objfunc <- function(returns) sum(returns)/sd(returns)
profilevs <- sapply(look_backs, function(look_back) {
  pnls <- backtestmomentum(returns=returns, returns_weighted=returns_weighted,
                             endp=endp,
                             look_back=look_back, objfunc=objfunc)
  last(cumprod(1 + pnls))
})  # end sapply
x11(width=6, height=4)
plot(x=look_backs, y=profilevs, t="l",
     main="Strategy PnL as function of look_back",
     xlab="look_back (months)", ylab="pnl")

dates <- index(returns[endp])
weightsaw <- c(0.30, 0.55, 0.15)
retsaw <- returns %*% weightsaw
wealthaw <- cumprod(1 + retsaw)
wealthaw <- xts::xts(wealthaw[endp], dates)

look_back <- look_backs[which.max(profilevs)]
pnls <- backtestmomentum(returns=returns, returns_weighted=returns_weighted,
                           look_back=look_back, endp=endp,
                           objfunc=objfunc, with_weights=TRUE)
tail(pnls)
retsmom <- as.numeric(pnls[, 1])
wealth <- cumprod(1 + retsmom)

datav <- cbind(wealth, wealthaw)
colnames(datav) <- c("Momentum Strategy", "All_weather")

dygraphs::dygraph(datav, main="Momentum Strategy") %>%
  dyAxis("y", label="All_weather", independentTicks=TRUE) %>%
  dyAxis("y2", label="Momentum Strategy", independentTicks=TRUE) %>%
  dySeries(name="Momentum Strategy", axis="y2", label="Momentum Strategy", strokeWidth=2, col="red") %>%
  dySeries(name="All_weather", axis="y", label="All_weather", strokeWidth=2, col="blue")


# Plot multiple wealth curves
# Perform sapply loop over look_backs
wealth <- sapply(look_backs, backtestmomentum,
                  returns=returns,
                  returns_weighted=returns_weighted,
                  endp=endp,
                  objfunc=objfunc)
wealth <- apply(wealth, 2, function(x) cumprod(1 + x))
colnames(wealth) <- paste0("look_back=", look_backs)
wealth <- xts(wealth, dates)
tail(wealth)

plot_theme <- chart_theme()
plot_theme$col$line.col <-
  colorRampPalette(c("blue", "red"))(NCOL(wealth))
chart_Series(wealth,
             theme=plot_theme, name="Cumulative Returns of Daily ETF Momentum Strategies")
legend("bottomleft", legend=colnames(wealth),
       inset=0.02, bg="white", cex=0.7, lwd=rep(6, NCOL(returns)),
       col=plot_theme$col$line.col, bty="n")



###############
### VXX and SVXY

# returns <- -etfenv$returns$VXX
returns <- na.omit(etfenv$returns$SVXY)
returns <- cbind(returns, -na.omit(etfenv$returns$VXX))
which_na <- which(is.na(returns$VXX))
returns$VXX[which_na] <- returns$SVXY[which_na]
returns <- cumprod(1+rowMeans(returns))
sum(is.na(returns))
head(returns)
volumes <- cbind(quantmod::Vo(etfenv$SVXY), quantmod::Vo(etfenv$VXX))
volumes$VXX.Volume[which_na] <- volumes$SVXY.Volume[which_na]
volumes <- rowMeans(volumes)

roll_vwap <- rutils::roll_sum(xtes=returns*volumes, look_back=look_back)
volume_rolling <- rutils::roll_sum(xtes=volumes, look_back=look_back)
roll_vwap <- roll_vwap/volume_rolling
roll_vwap[is.na(roll_vwap)] <- 0
roll_vwap

plot(returns, t="l", lwd=2)
lines(roll_vwap, col="red", lwd=2)



###############
### Wilcoxon tests

# Data
sample1 <- rnorm(200)
sample2 <- rnorm(100, mean=0.1)
# Mann-Whitney-Wilcoxon rank sum test
wilcoxt <- wilcox.test(sample1, sample2, paired=FALSE)
wilcoxt$statistic
# Calculate U statistic of Mann-Whitney-Wilcoxon test
datav <- c(sample1, sample2)
ranks <- rank(datav)
sum(ranks[1:200]) - 100*201
sum(ranks[201:300]) - 50*101

# Data
sample1 <- rnorm(100)
sample2 <- rnorm(100, mean=0.1)
# Wilcoxon signed rank test
wilcoxt <- wilcox.test(sample1, sample2, paired=TRUE)
wilcoxt$statistic
# Calculate V statistic of Wilcoxon test
datav <- (sample1 - sample2)
sum(rank(abs(datav))[datav>0])


###############
### C:/Develop/predictive/data

library(rutils)
datav <- read.zoo(file="C:/Develop/predictive/data/predictions_long_account.csv", header=TRUE, sep=",")
datav <- as.xts(datav)
dates <- index(datav)
colnamev <- colnames(datav)
datav <- lapply(datav, as.numeric)
datav <- rutils::do_call(cbind, datav)
datav <- xts(datav, dates)
colnames(datav) <- colnamev

core_data <- datav[, 8:9]
colnames(core_data) <- c("actual", "predicted")
core_data <- core_data["2020-01-10/"]
core_data <- na.omit(core_data)
dates <- index(core_data)
dates <- as.Date(dates)
dates <- unique(dates)
dates <- as.Date(dates)
foo <- sapply(dates, function(dat_e) {
  foo <- core_data[(dates == dat_e), ]
  sum(foo[foo[, 2] > 0.15, 1])
})  # end sapply
names(foo) <- dates


tail(core_data, 33)
dim(core_data)
head(core_data, 33)
core_data <- coredata(core_data)
core_data <- cbind(core_data[, 2, drop=FALSE], core_data[, 1, drop=FALSE])
plot(core_data)

ma_x <- max(core_data[, 1])
sapply((1:8)*ma_x/10, function(x) {
  sum(core_data[core_data[, 1]>x, 2])
})  # end sapply
sum(core_data[core_data[, 1]>0.15, 2])


foo <- datav["2020-01-10/"]
foo <- na.omit(foo)
sum(foo[, 8])
sum(foo[foo[, 9]>0.15, 8])




###############
### Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/test.cpp")


indeks <- rutils::etfenv$returns[, "VTI", drop=FALSE]
indeks <- na.omit(indeks)
dates <- index(indeks)

returns <- rutils::etfenv$returns[, "XLF", drop=FALSE]
returns <- na.omit(returns)
returns <- returns[dates]
calc_alpha(returns, indeks, typ_e="wilcoxon")

symbolv <- colnames(rutils::etfenv$returns)
symbolv <- symbolv[-grep("VTI", symbolv, ignore.case=TRUE)]

foo <- sapply(symbolv, function(symbol) {
  cat(symbol, "\n")
  returns <- rutils::etfenv$returns[, symbol, drop=FALSE]
  returns <- na.omit(returns)
  returns <- returns[dates]
  dates <- dates[index(returns)]
  calc_alpha(returns, dates, typ_e="wilcoxon")
})  # end sapply




###############
### Scale minutely returns by the volume to make them closer to normal or stationary.
# Use trading time (volume clock)

library(HighFreq)
ohlc <- HighFreq::SPY
prices <- drop(coredata(quantmod::Cl(ohlc)))
volumes <- drop(coredata(quantmod::Vo(ohlc)))
returns <- rutils::diffit(log(prices))
returns <- returns/sd(returns)
prices <- cumsum(returns)

# Scale the volume by the rolling average volume
look_back <- 111
volume_rolling <- rutils::roll_sum(xtes=volumes, look_back=look_back)
volumes <- look_back*volumes/volume_rolling

## wippp
## Add to homeworks
## Statistics for different volume scaling exponents.
# Dividing by the square root of the volume works better
# than dividing by the volume itself.
statis_tic <- sapply((1:20)/20, function(ex_po) {
  rets_scaled <- ifelse(volumes > 0, returns/(volumes^ex_po), 0)
  rets_scaled <- rets_scaled/sd(rets_scaled)
  # Calculate moments and perform JB normality tests
  # tseries::jarque.bera.test(rets_scaled)$statistic
  # moments::moment(rets_scaled, order=4)
  # Calculate autocorrelations from PACF
  pa_cf <- pacf(as.numeric(rets_scaled), lag=10, plot=FALSE)
  sum(pa_cf$acf)
  # Standard deviation of square returns is proxy for kurtosis and stationarity
  # sd(rets_scaled^2)
  # calc_hurst(rets_scaled, endp)
})  # end sapply
x11(width=6, height=5)
plot(statis_tic, t="l")


# Divide returns by the volume (volume clock).
rets_scaled <- ifelse(volumes > 0, returns/volumes, 0)
rets_scaled <- rets_scaled/sd(rets_scaled)
ma_d <- mad(rets_scaled)

# Plot densities of the returns
x11(width=6, height=5)
par(mar=c(3, 3, 2, 1), oma=c(1, 1, 1, 1))
plot(density(returns), xlim=5*c(-ma_d, ma_d), 
     lwd=3, mgp=c(2, 1, 0), col="blue", 
     xlab="returns (standardized)", ylab="frequency", 
     main="Density of Volume-scaled High Frequency SPY Returns")
lines(density(rets_scaled, bw=0.4), lwd=3, col="red")
curve(expr=dnorm, add=TRUE, lwd=3, col="green")
# Add legend
legend("topright", inset=0.05, bty="n",
       leg=c("minutely", "scaled", "normal"),
       lwd=6, lty=1, col=c("blue", "red", "green"))

# Plot the cumulative scaled returns
prices_scaled <- cumsum(rets_scaled)
prices_scaled <- xts(prices_scaled, index(ohlc))
dygraphs::dygraph(prices_scaled[60*(1:(NROW(prices_scaled) %/% 60))], main="SPY Prices")


# Plot the cumulative scaled returns with close prices
datav <- cbind(prices, prices_scaled)
colnamev <- c("SPY Prices", "Scaled by Volume")
# datav <- xts::to.hourly(datav)
colnames(datav) <- colnamev
dygraphs::dygraph(datav[60*(1:(NROW(datav) %/% 60))], main="SPY Prices") %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", col="red", strokeWidth=2) %>%
  dySeries(name=colnamev[2], axis="y2", col="blue", strokeWidth=2) %>%
  dyLegend(width=500)


## Calculate Hurst

# Define a single aggregation interval.
interval <- 35^2
inter_log <- log(interval)
# Calculate index of end points spaced apart by interval.
numrows <- NROW(prices)
num_agg <-.n_rows %/% interval
endp <- c(0,.n_rows - num_agg*interval + (0:num_agg)*interval)

# Calculate Hurst from single data point
calc_hurst <- function(returns, endp) {
  prices <- cumsum(returns)
  r_s <- sapply(2:NROW(endp), function(ep) {
    indeks <- endp[ep-1]:endp[ep]
    diff(range(prices[indeks]))/sd(returns[indeks])
  })  # end sapply
  log(mean(r_s))/inter_log
}  # end calc_hurst

calc_hurst(returns, endp)



###############
### Scale minutely returns by the price range to make them closer to normal or stationary

library(HighFreq)

# symbol <- "SPY"
ohlc <- HighFreq::SPY

## Calculate price range
rangev <- log(drop(coredata(Hi(ohlc)/Lo(ohlc))))
# Around 1.8% of bars have zero range
sum(rangev==0)/NROW(rangev)
# Remove bars with zero range
# ohlc <- ohlc[!(rangev==0)]
# Remove bars with zero returns
returns <- rutils::diffit(log(drop(coredata(Cl(ohlc)))))
# zero_rets <- (returns==0)
# ohlc <- ohlc[!zero_rets]

dates <- index(ohlc)
numrows <- NROW(ohlc)
endp <- xts::endpoints(ohlc, on="hours")
closep <- Cl(ohlc)[endp]
rangev <- log(drop(coredata(Hi(ohlc)/Lo(ohlc))))
rangev <- (rangev + c(0, rangev[-NROW(rangev)]))/2
returns <- rutils::diffit(log(drop(coredata(Cl(ohlc)))))
# returns <- c(0, returns)
sum(is.na(returns))
sum(is.infinite(returns))

## Distribution of raw returns is bimodal
x11()
# Standardize raw returns to make later comparisons
returns <- returns/sd(returns)
ma_d <- mad(returns)
range(returns)
# Standard deviation of square returns is proxy for kurtosis and stationarity
sd((returns/sd(returns))^2)
hist(returns, breaks=1111, xlim=c(-3, 3), freq=FALSE)
lines(density(returns, bw=0.2), col='red', lwd=2)

## Calculate moments and perform JB normality tests
sapply(1:4, moments::moment, x=returns)
tseries::jarque.bera.test(returns)
shapiro.test(returns)


# Regress volume on range
datav <- cbind(volume=log(volumes), range=log(rangev))
# datav[!is.finite(datav)] <- NA
datav <- na.omit(is.finite(datav)*datav)
# foo <- lm(paste(colnames(datav), collapse=" ~ "), data=as.data.frame(datav))
foo <- lm(volume ~ range, as.data.frame(datav))
plot(volume ~ range, as.data.frame(datav[samplev, ]))
abline(foo, lwd=3, col="red")


## Apply Manly transformation to make returns more normal
lambdav <- 1.5

# Modulus
# Divide returns by square root of volume (volume clock)


## Calculate moments and perform JB normality test
sum(is.na(rets_scaled))
sum(is.infinite(rets_scaled))
range(rets_scaled)
sapply(1:4, moments::moment, x=rets_scaled)
tseries::jarque.bera.test(rets_scaled)
x11()
ma_d <- mad(rets_scaled)
hist(rets_scaled, breaks=11111, xlim=10*c(-ma_d, ma_d), freq=FALSE)
pacf(rets_scaled)


## Scale returns using price range
rets_scaled <- ifelse(rangev>0, returns/rangev, 0)
# rets_scaled <- returns/rangev
rets_scaled <- rets_scaled/sd(rets_scaled)
ma_d <- mad(rets_scaled)
hist(rets_scaled, breaks=1111, xlim=c(-3, 3), freq=FALSE)
lines(density(rets_scaled, bw=0.2), col='blue', lwd=2)

# JB statistic for different range scaling exponents
statis_tic <- sapply((1:6)/2, function(ex_po) {
  rets_scaled <- ifelse(rangev>0, returns/(rangev^ex_po), 0)
  rets_scaled <- rets_scaled/sd(rets_scaled)
  tseries::jarque.bera.test(rets_scaled)$statistic
})  # end sapply


# Stationarity statistic for different scaling exponents
statis_tic <- sapply((1:20)/10, function(ex_po) {
  # rets_scaled <- ifelse(volumes>0, returns/(volumes^x), 0)
  rets_scaled <- ifelse(rangev>0, returns/(rangev^ex_po), 0)
  # rets_scaled <- returns/(rangev^ex_po)
  # Remove zero returns
  # rets_scaled <- rets_scaled[!(rets_scaled==0)]
  # rets_scaled <- rets_scaled/sd(rets_scaled)
  # Standard deviation of square returns is proxy for kurtosis and stationarity
  sd((rets_scaled/sd(rets_scaled))^2)
})  # end sapply



###############
### Fractional differencing




############### homework
# Summary: Strategy using weekly and monthly stock returns.
# It's implemented in app_roll_portf9.R

library(HighFreq)

# Aggregate the VTI returns to monthly and run pacf()

closep <- Cl(rutils::etfenv$VTI)
returns <- rutils::diffit(log(closep))
returns_adv <- rutils::lagit(returns, lagg=(-1))
returns_adv <- as.numeric(returns_adv)

# endp <- rutils::calc_endpoints(closep, interval="weeks")
# week_ly <- closep[endp]
# week_ly <- rutils::diffit(log(week_ly))
week_ly <- rutils::diffit(log(closep), lagg=5)
week_ly <- as.numeric(week_ly)
returns_adv <- rutils::lagit(week_ly, lagg=(-1))
returns_adv <- as.numeric(returns_adv)
# endp <- rutils::calc_endpoints(closep, interval="months")
# month_ly <- closep[endp]
# month_ly <- rutils::diffit(log(month_ly))
month_ly <- rutils::diffit(log(closep), lagg=25)
month_ly <- as.numeric(month_ly)
returns_adv <- rutils::lagit(month_ly, lagg=(-1))
returns_adv <- as.numeric(returns_adv)

# Objective function for simple optimization
object_ive <- function(wei_ght) {
  # weights <- c(wei_ght, 1-wei_ght)
  sum((returns_adv - (wei_ght*week_ly + (1-wei_ght)*month_ly))^2)
}  # end object_ive
object_ive(0.5)
foo <- optimize(f=object_ive, interval=c(-10, 10))
unlist(foo)
wei_ght <- unlist(foo)[1]
position_s <- sign(wei_ght*week_ly + (1-wei_ght)*month_ly)
positions_lag <- rutils::lagit(position_s, lagg=2)
pnls <- cumsum(positions_lag*returns)
x11()
end_days <- rutils::calc_endpoints(returns, "days")
plot.zoo(-pnls[end_days], main="pnls", xlab=NA, ylab=NA)




############### homework
# Summary: Create a contrarian strategy using
# the Hampel filter.
# It's implemented in app_roll_portf10.R

library(rutils)


# 1. (10pts)
# Define a rolling look-back window and a half window:
# Use the %/% operator.

win_dow <- 11
half_window <- win_dow %/% 2

# Calculate a time series of rolling z-scores
# (called z_scores), using the Hampel filter code
# from the lecture slides.

prices <- Cl(HighFreq::SPY)["T09:31:00/T15:59:00"]
returns <- rutils::diffit(log(prices))

medi_an <- TTR::runMedian(prices, n=win_dow)
medi_an[1:win_dow, ] <- 1
sum(is.na(medi_an))
ma_d <- TTR::runMAD(prices, n=win_dow)
ma_d[1:win_dow, ] <- 1
sum(is.na(ma_d))
z_scores <- ifelse(ma_d!=0, (prices-medi_an)/ma_d, 0)
z_scores[1:win_dow, ] <- 0
ma_d <- zoo::na.locf(z_scores)
sum(is.na(z_scores))
mad_zscores <- TTR::runMAD(z_scores, n=win_dow)
mad_zscores[1:win_dow, ] <- 0

# You should get the following output:
tail(z_scores)
mad(z_scores)
range(z_scores)
hist(z_scores, breaks=30, xlim=c(-10, 10), freq=FALSE)

# Calculate position_s and pnls from z-scores
position_s <- rep(NA_integer_, NROW(prices))
position_s[1] <- 0
# thresh_old <- 3*mad(z_scores)
# position_s <- ifelse(z_scores > thresh_old, -1, position_s)
# position_s <- ifelse(z_scores < (-thresh_old), 1, position_s)
position_s <- ifelse(z_scores > 2*mad_zscores, -1, position_s)
position_s <- ifelse(z_scores < (-2*mad_zscores), 1, position_s)
position_s <- zoo::na.locf(position_s)
positions_lag <- rutils::lagit(position_s, lagg=2)
pnls <- cumsum(positions_lag*returns)
x11()
end_days <- rutils::calc_endpoints(prices, "days")
plot.zoo(pnls[end_days], main="pnls", xlab=NA, ylab=NA)



############### homework - Hurst exponents almost random
# Summary: Calculate a time series of monthly Hurst
# exponents and the volatility for the SPY series.
# Demonstrate that the changes of the Hurst exponent
# have negative autocorrelations, that Hurst exponent
# is anti-persistent over time.
# Calculate the correlation between the SPY Hurst
# exponent and the level of volatility.
#
# Regress the Hurst exponent versus the standard
# deviation, and create plots and perform a regression
# of the two.

# Observation:
# The Hurst exponent is low when the volatility is high.
# When the volatility is low then the Hurst exponent
# can be both high and low.


library(HighFreq)

# This is best version
# Calculate Hurst exponent using median of range ratios
calc_hurst_hilo <- function(highp, lowp, endp) {
  range_ratios <- sapply(seq_along(endp)[-1], function(it) {
    startpoint <- endp[it-1]
    endpoint <- endp[it]
    highp <- highp[startpoint:endpoint]
    lowp <- lowp[startpoint:endpoint]
    log((max(highp) - min(lowp))/mean(highp - lowp))/log(endpoint-startpoint)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_hilo

# Calculate Hurst exponent using median of range ratios
calc_hursto <- function(highp, lowp, endp) {
  range_ratios <- sapply(seq_along(endp)[-1], function(it) {
    highp <- highp[endp[it-1]:endp[it]]
    lowp <- lowp[endp[it-1]:endp[it]]
    (max(highp) - min(lowp))/mean(highp - lowp)
  })  # end sapply
  log(median(na.omit(range_ratios)))/log(median(rutils::diffit(endp)))
}  # end calc_hursto

# Calculate Hurst exponent from returns
calc_hurst_rets <- function(rets, endp) {
  cumsumv <- cumsum(rets)
  range_ratios <- sapply(seq_along(endp)[-1], function(it) {
    startpoint <- endp[it-1]
    endpoint <- endp[it]
    rets <- rets[startpoint:endpoint]
    cumsumv <- cumsumv[startpoint:endpoint]
    log((max(cumsumv) - min(cumsumv))/sd(rets))/log(endpoint-startpoint)
  })  # end sapply
  median(na.omit(range_ratios))
}  # end calc_hurst_rets


ohlc <- log(HighFreq::SPY)
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days"))

library(microbenchmark)
summary(microbenchmark(
  calc_hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")),
  calc_hursto=calc_hursto(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



# 2. (20pts)
# Calculate a vector of monthly end points from
# the ohlc, and call it endp.
# use the function rutils::calc_endpoints().

endp <- rutils::calc_endpoints(ohlc, interval="months")

# Perform an sapply() loop over the length of endp.
# Inside the loop calculate the standard deviation of
# returns and the cumulative trading volumes.
# The output should be a matrix called volat_hurst.

volat_hurst <- sapply(seq_along(endp)[-1],
  function(it) {
    ohlc <- ohlc[endp[it-1]:endp[it]]
    highp <- quantmod::Hi(ohlc)
    lowp <- quantmod::Lo(ohlc)
    c(volatility=HighFreq::calc_var_ohlc(ohlc),
      hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="days")))
  })  # end sapply
# rbind list into single xts or matrix
volat_hurst <- t(volat_hurst)

x11()
plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diffit(volat_hurst)
plot(foo)
pacf(foo[, 1])


# wippp
############### homework
# Summary: Calculate a time series of annual Hurst
# exponents for S&P500 stocks.
# Plot a scatterplot of Hurst for the years 2008 and 2009.

# Set up the data as follows:
library(HighFreq)

# Load S&P500 constituent stock prices
load("C:/Develop/lecture_slides/data/sp500.RData")

ohlc <- log(sp500env$SIG)
quantmod::chart_Series(Cl(ohlc))
highp <- quantmod::Hi(ohlc)
lowp <- quantmod::Lo(ohlc)
calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="months"))

endp <- rutils::calc_endpoints(ohlc, interval="years")
volat_hurst <- sapply(seq_along(endp)[-1],
                      function(it) {
                        ohlc <- ohlc[endp[it-1]:endp[it]]
                        highp <- quantmod::Hi(ohlc)
                        lowp <- quantmod::Lo(ohlc)
                        c(volatility=HighFreq::calc_var_ohlc(ohlc),
                          hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="months")))
                      })  # end sapply
# Transpose the matrix
volat_hurst <- t(volat_hurst)

plot(volat_hurst)
plot.zoo(volat_hurst)
foo <- rutils::diffit(volat_hurst)
plot(foo)
plot(cbind(foo[, 1], volat_hurst[, 2]))
pacf(foo[, 1])

model <- lm(volat_hurst[, 2] ~ volat_hurst[, 1])
model_sum <- summary(model)
model_sum$coefficients[2, 3]
plot(volat_hurst[, 2] ~ volat_hurst[, 1])
abline(model)

###

# Scrub data in sp500env

x[is.infinite(x)] <- NA
x <- zoo::na.locf(x)
x <- zoo::na.locf(x, fromLast=TRUE)


###


hurst_prof <- eapply(sp500env, function(ohlc) {
  endp <- rutils::calc_endpoints(ohlc, interval="years")
  if (NROW(endp) > 3) {
    ohlc <- log(ohlc)
    volat_hurst <- sapply(seq_along(endp)[-1],
                          function(it) {
                            ohlc <- ohlc[endp[it-1]:endp[it]]
                            highp <- quantmod::Hi(ohlc)
                            lowp <- quantmod::Lo(ohlc)
                            c(volatility=HighFreq::calc_var_ohlc(ohlc),
                              hurst=calc_hurst(highp, lowp, rutils::calc_endpoints(ohlc, interval="months")))
                          })  # end sapply
    # Transpose the matrix
    volat_hurst <- t(volat_hurst)
    # Scrub the data
    volat_hurst <- zoo::na.locf(volat_hurst)
    zoo::na.locf(volat_hurst, fromLast=TRUE)
  } else {
    c("Not enough years for Hurst\n")
    cbind(volatility=rep(1, NROW(endp)), hurst=rep(0.5, NROW(endp)))
  }
})  # end eapply

hurst_prof <- lapply(hurst_prof, function(volat_hurst) {
  # Scrub the data
  volat_hurst[is.infinite(volat_hurst)] <- NA
  volat_hurst <- zoo::na.locf(volat_hurst)
  zoo::na.locf(volat_hurst, fromLast=TRUE)
})  # end lapply

save(hurst_prof, file="C:/Develop/lecture_slides/data/sp500_perf.RData")

bar <- sapply(hurst_prof, function(x) sum(is.na(x) | is.infinite(x)))
max(bar)
which.max(bar)
hurst_prof[[names(which.max(bar))]]

get_tval <- function(x) {
  cat("NROW(x) = ", NROW(x), "\n")
  x <- na.omit(x)
  if ((NROW(x) > 3) & (sum(is.na(x))==0)) {
    model <- lm(x[, 2] ~ x[, 1])
    summary(model)$coefficients[2, 3]
  } else 1
}  # end get_tval
bar <- sapply(hurst_prof, get_tval)

bar <- sapply(hurst_prof, function(x) {
  # cat("dim(x) = ", dim(x), "\n")
  if (NROW(x) > 3) {
    model <- lm(x[, 2] ~ x[, 1])
    summary(model)$coefficients[2, 3]
  } else 0
})  # end sapply

bar <- sort(bar)
hist(bar, freq=FALSE)
which.max(bar)
bar[[names(which.max(bar))]]
hurst_prof[[names(which.max(bar))]]
which.min(bar)
bar[[names(which.min(bar))]]
hurst_prof[[names(which.min(bar))]]
plot(hurst_prof[[names(which.min(bar))]])

bar <- sapply(hurst_prof, function(x) {
  max(x[, 2])
})  # end sapply
returns <- prices[, names(tail(bar, 100))]
returns <- rutils::diffit(log(returns))
save(returns, file="C:/Develop/lecture_slides/data/sp100_rets.RData")

colnamev <- colnames(hurst_prof$AAPL)
bar <- lapply(hurst_prof, function(x) {
  x <- cbind((x[, 1]-min(x[, 1]))/(max(x[, 1]-min(x[, 1]))), (x[, 2]-min(x[, 2]))/(max(x[, 2])-min(x[, 2])))
  colnames(x) <- colnamev
  x
})  # end lapply
foo <- NULL
unlist(sapply(hurst_prof, function(x) {
  foo <<- rbind(foo, x)
  NULL
}))  # end sapply
plot(foo)



###############
### Forecasting for univariate regression

library(HighFreq)
source("C:/Develop/R/scripts/market_making.R")

data_dir <- "/Volumes/external/Develop/data/ib_data/"
symbol <- "ES"
load(paste0(data_dir, symbol, "_ohlc.RData"))
numrows <- NROW(ohlc)
ohlc <- coredata(ohlc)

look_back <- 500
design <- cbind(rep(1, look_back), 1:look_back)
deg_free <- (look_back - NCOL(design))
design_inv <- MASS::ginv(design)
design2 <- MASS::ginv(crossprod(design))
lagg <- 1
oo_s <- cbind(1, look_back + lagg)
oos_t <- t(oo_s)

# influ_ence <- design %*% design_inv
# fit_ted <- drop(influ_ence %*% se_ries)

# se_ries <- ohlc[(look_back+1):(look_back+100), 4]
# val_ue <- ohlc[201, 4]
# calc_zscore(val_ue, se_ries, design, design_inv, design2, oo_s, oos_t, deg_free)

z_scores <- sapply((look_back+1).n_rows, function(x) {
  se_ries <- ohlc[(x-look_back):(x-1), 4]
  val_ue <- ohlc[x, 4]
  calc_zscore(val_ue, se_ries, design, design_inv, design2, oo_s, oos_t, deg_free)
})  # end sapply
z_scores <- c(rep(0, look_back), z_scores)

hist(z_scores, breaks=100, freq=FALSE)
quantile(z_scores, 0.9)

foo <- which((z_scores < quantile(z_scores, 0.1)) & (z_scores > quantile(z_scores, 0.01)))
foo <- which((z_scores > quantile(z_scores, 0.9)) & (z_scores < quantile(z_scores, 0.99)))
foo <- foo[(foo>(30)) & (foo<.n_rows-100))]
foo <- lapply(foo, function(x) {
  ohlc[(x-30):(x+100), 4]/ohlc[x, 4]
})
foo <- rutils::do_call(cbind, foo)
foo <- rowMeans(foo)
plot(foo, t="l")




###############
### Tests for HighFreq functions

library(rutils)
# detach("package:HighFreq")
library(dygraphs)

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/calc_weights.cpp")


## Tests for sorting and ranking

datav <- round(runif(7), 2)
all.equal(datav, drop(sort_back(datav)))
all.equal(rank(datav), drop(calc_ranks(datav)))
drop(calc_ranks_m(datav))
sum(calc_ranks_m(datav))
datav <- xts::xts(runif(7), seq.Date(Sys.Date(), by=1, length.out=7))
all.equal(rank(drop(coredata(datav))), drop(calc_ranks(datav)))



## Load S&P500 stock returns
# returns <- na.omit(rutils::etfenv$returns[, 1:9])
load(file="C:/Develop/lecture_slides/data/sp500_returns.RData")
ret_s <- returns100["2000/"]
ret_s[1, is.na(ret_s[1, ])] <- 0
ret_s <- zoo::na.locf(ret_s, na.rm=FALSE)
numrows <- NROW(ret_s)
ncols <- NCOL(ret_s)


## Tests for HighFreq::calc_weights()

## Calculate ranksharpe weights using R
weights_r <- sapply(ret_s, function(x) mean(x)/sd(x))
weights_r <- rank(weights_r)
weights_r <- (weights_r - mean(weights_r))

# weights_r <- 0.01*weights/stddev(returns*weights)

## Calculate weights using RcppArmadillo
weights <- drop(calc_weights(ret_s, model_type="ranksharpe", scale=FALSE))
all.equal(weights, weights_r, check.attributes=FALSE)


## Calculate max_sharpe weights using R

# Calculate covariance matrix of ETF returns
# ret_s <- na.omit(rutils::etfenv$returns[, 1:16])
eigend <- eigen(cov(ret_s))
# Calculate regularized inverse of covariance matrix
max_eigen <- 3
eigen_vec <- eigend$vectors[, 1:max_eigen]
eigen_val <- eigend$values[1:max_eigen]
inverse <- eigen_vec %*% (t(eigen_vec) / eigen_val)
# Define shrinkage intensity and apply shrinkage to the mean returns
alpha <- 0.5
col_means <- colMeans(ret_s)
col_means <- ((1-alpha)*col_means + alpha*mean(col_means))

weights_r <- inverse %*% col_means
n_col <- NCOL(ret_s)
weights_r <- weights_r*sd(ret_s %*% rep(1/n_col, n_col))/sd(ret_s %*% weights_r)

## Calculate weights using RcppArmadillo
weights <- drop(calc_weights(ret_s, model_type="max_sharpe", alpha=alpha, max_eigen=3, scale=FALSE))
all.equal(weights, drop(weights_r), check.attributes=FALSE)



## Calculate returns on equal weight portfolio
indeks <- rowMeans(ret_s)
stdev <- sd(indeks[indeks<0])
indeks <- xts(indeks, index(ret_s))

foo <- weight_returns(ret_s, weights)
bar <- ret_s %*% weights
all.equal(foo, bar)


# Define maximum Sharpe portfolio weights
calc_weights <- function(returns) {
  # eigend <- eigen(cov(returns))
  # # set tolerance for determining zero eigenvalues
  # precision <- sqrt(.Machine$double.eps)
  # # check for zero eigenvalues
  # not_zero <- (eigend$values > (precision * eigend$values[1]))
  # inverse <- eigend$vectors[, not_zero] %*% (t(eigend$vectors[, not_zero])/eigend$values[not_zero])
  # weights <- inverse %*% apply(returns, 2, mean)
  # weights/sum(abs(weights))

  weights <- sapply(returns[(.n_rows-500).n_rows)], median)
  # weights <- (order(order(weights)-1)-1)
  # weights <- order(order(weights))
  # weights <- order(weights)
  weights <- (order(weights)-1)
  # weights <- order(order(weights, decreasing=TRUE), decreasing=TRUE)
  # weights <- ordern[ordern]

} # end calc_weights
weights <- calc_weights(ret_sub)


foo <- drop(calc_weights(returns[(.n_rows-500).n_rows)], typ_e="rankrob", alpha=0, scale=FALSE))


all.equal(weights, foo)
foo <- cbind(weights, foo)
head(foo, 11)
tail(foo, 11)


weights <- colMeans(returns)
weights <- sapply(returns, function(x) mean(x)/sd(x))
weights <- sapply(returns, moments::skewness)
weights <- drop(HighFreq::calc_ranks(weights))
weights <- (weights - mean(weights))
names(weights) <- colnames(returns)


weights <- drop(calc_weights(returns, typ_e="max_sharpe", alpha=0))
pnls <- (returns %*% weights)
pnls <- xts(cumsum(pnls), order.by=index(returns))
prices <- cumsum(rowMeans(returns))
pnls <- cbind(pnls, prices)
colnames(pnls) <- c("Strategy", "Index")

colnamev <- colnames(pnls)
cap_tion <- paste("Momentum Strategy for S&P500 Stocks")
dygraphs::dygraph(pnls, main=cap_tion) %>%
  dyAxis("y", label=colnamev[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=colnamev[2], independentTicks=TRUE) %>%
  dySeries(name=colnamev[1], axis="y", label=colnamev[1], strokeWidth=1, col="red") %>%
  dySeries(name=colnamev[2], axis="y2", label=colnamev[2], strokeWidth=1, col="blue")




###############
### Strategy for market making using limit orders

data_dir <- "/Volumes/external/Develop/data/ib_data"
setwd(dir=data_dir)
load("ohlc.RData")
ohlc_data <- coredata(ohlc)
numrows <- NROW(ohlc_data)

ohlc_lag <- rutils::lagit(ohlc_data)
prices <- ohlc_data[, 4]

buy_spread <- 0.25
sell_spread <- 0.25

# Vectorized version

buy_price <- (ohlc_lag[, 3] - buy_spread)
sell_price <- (ohlc_lag[, 2] + sell_spread)

buy_ind <- (ohlc_data[, 3] < buy_price)
n_buy <- cumsum(buy_ind)
sell_ind <- (ohlc_data[, 2] > sell_price)
n_sell <- cumsum(sell_ind)

buy_s <- numeric.n_rows)
buy_s[buy_ind] <- buy_price[buy_ind]
buy_s <- cumsum(buy_s)
sell_s <- numeric.n_rows)
sell_s[sell_ind] <- sell_price[sell_ind]
sell_s <- cumsum(sell_s)

pnls <- ((sell_s-buy_s) - prices*(n_sell-n_buy))


# Loop version

buy_price <- numeric.n_rows)
sell_price <- numeric.n_rows)
n_buy <- numeric.n_rows)
n_sell <- numeric.n_rows)
buy_s <- numeric.n_rows)
sell_s <- numeric.n_rows)
pnls <- numeric.n_rows)

for (it in 2.n_rows) {
  buy_price[it] <- (ohlc_lag[it, 3] - buy_spread)
  sell_price[it] <- (ohlc_lag[it, 2] + sell_spread)

  buy_ind <- (ohlc_data[it, 3] < buy_price[it])
  sell_ind <- (ohlc_data[it, 2] > sell_price[it])
  n_buy[it] <- n_buy[it-1] + buy_ind
  n_sell[it] <- n_sell[it-1] + sell_ind
  buy_s[it] <- buy_s[it-1] + buy_ind*buy_price[it]
  sell_s[it] <- sell_s[it-1] + sell_ind*sell_price[it]
  pnls[it] <- ((sell_s[it] - buy_s[it]) - prices[it]*(n_sell[it] - n_buy[it]))
}  # end for


plot(pnls[c(1, rutils::calc_endpoints(ohlc, interval="minutes"))], t="l", main="Market Making Strategy")




###############
### Strategy using OHLC technical indicators

library(HighFreq)

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/lm_arma.cpp")


# Source the backtest functions
source("C:/Develop/R/scripts/backtest_functions.R")


# load OHLC data
# ohlc <- HighFreq::SPY
# ohlc <- HighFreq::SPY["2010-10/2010-11"]
# ohlc <- rutils::etfenv$VTI
# load recent ES1 futures data
# load(file="/Volumes/external/Develop/data/ES1.RData")
# or
# ohlc <- read.zoo(file="/Volumes/external/Develop/data/new_bar/ES1.csv", header=TRUE, sep=",",
#                   drop=FALSE, format="%Y-%m-%d %H:%M",
#                   FUN=as.POSIXct, tz="America/New_York")
# ohlc <- as.xts(ohlc)
# ohlc <- ohlc["T09:00:00/T16:30:00"]
# save(ohlc, file="/Volumes/external/Develop/data/ES1.RData")
# load recent combined futures data
load(file="/Volumes/external/Develop/data/combined.RData")

# set up data for signal
symbol <- "UX1"
ohlc <- com_bo[, paste(symbol, c("Open", "High", "Low", "Close"), sep=".")]

ohlc_log <- log(ohlc)
# sum(is.na(ohlc))
# sapply(ohlc, class)
# tail(ohlc, 11)
closep <- Cl(ohlc_log)
close_num <- drop(coredata(closep))
returns <- rutils::diffit(closep)
# regression with closep prices as response requires closep to be a vector
# closep <- drop(coredata(closep)
# plot dygraph
dygraphs::dygraph(xts::to.hourly(closep), main=symbol)
# random data
# returns <- xts(rnorm(NROW(ohlc), sd=0.01), index(ohlc))
# closep <- drop(coredata(cumsum(returns))

# Define OHLC data
openp <- Op(ohlc_log)
highp <- Hi(ohlc_log)
high_num <- as.numeric(highp)
lowp <- Lo(ohlc_log)
low_num <- as.numeric(lowp)
close_high <- (close_num == high_num)
close_high_count <- drop(HighFreq::roll_count(close_high))
close_low <- (closep == lowp)
close_low_count <- drop(HighFreq::roll_count(close_low))
open_high <- (openp == highp)
open_high_count <- drop(HighFreq::roll_count(open_high))
open_low <- (openp == lowp)
open_low_count <- drop(HighFreq::roll_count(open_low))


# Set up data for trading
symbol <- "ES1"
returns <- rutils::diffit(log(com_bo[, paste(symbol, "Close", sep=".")]))



# variance <- (highp - lowp)^2
look_back <- 11
variance <- HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE)
colnames(variance) <- "variance"
volat <- sqrt(variance)
colnames(volat) <- "volat"
volumes <- Vo(ohlc)
colnames(volumes) <- "volume"

# Define current and future returns
# returns <- rutils::diffit(closep)
# trailing average returns
returns <- rutils::diffit(closep, lagg=look_back)/sqrt(look_back)
colnames(returns) <- "returns"
# returns_adv <- rutils::lagit(returns, lagg=-1)
# or
# returns_adv <- 0.5*(returns_adv + rutils::lagit(returns_adv, lagg=-1))
returns_adv <- rutils::lagit(rutils::diffit(closep, lagg=look_back), lagg=-look_back)/sqrt(look_back)
# returns_adv <- rutils::lagit(HighFreq::roll_sum(returns, look_back=look_back), lagg=-look_back)/look_back
# returns_adv <- xts(returns_adv, index(ohlc))
colnames(returns_adv) <- "returns_adv"
# Scale returns using sigmoid
# returns_adv <- plogis(returns_adv, scale=-quantile(returns_adv, 0.01))
# returns_adv <- (returns_adv - median(returns_adv))
# colnames(returns_adv) <- "returns_adv"



# Begin old stuff

###############
### Strategy using rolling z-scores over OHLC technical indicators
# with regression and dimension reduction


# colnames(returns) <- "returns"
# create design matrix
# dates <- xts::.index(ohlc)
indeks <- 1:NROW(ohlc)
design <- matrix(indeks, nc=1)

model <- HighFreq::calc_lm(response=as.numeric(returns_adv), design=cbind(returns, variance))
model$coefficients

# old: calculate sig_nal as the residual of the regression of the time series of closep prices
look_back <- 11
sig_nal <- HighFreq::roll_zscores(response=close_num, design=design, look_back=look_back)
colnames(sig_nal) <- "sig_nal"
sig_nal[1:look_back] <- 0
# or
sig_nal <- calc_signal(look_back, close_num, design)
hist(sig_nal, freq=FALSE)
# hist(sig_nal, xlim=c(-10, 10), freq=FALSE)

# old: perform parallel loop over look_backs
look_backs <- 15:35
library(parallel)
num_cores <- detectCores()
cluster <- makeCluster(num_cores-1)
# clusterExport(cluster, varlist=c("closep", "design"))
signal_s <- parLapply(cluster, X=look_backs, fun=calc_signal, closep=close_num, design=design)


# trade entry and exit levels
en_ter <- 1.0
ex_it <- 0.5
pnls <- calc_revert(signal_s[[1]], returns, en_ter, ex_it)
quantmod::chart_Series(pnls[endpoints(pnls, on="days")])

# old uses calc_revert(): run strategies over a vector of trade entry levels
run_strategies <- function(sig_nal, returns, en_ters, ex_it, return_series=TRUE) {
  # sapply(en_ters, calc_revert, sig_nal=sig_nal, returns=returns, ex_it=ex_it)
  pnls <- lapply(en_ters, calc_revert, sig_nal=sig_nal, returns=returns, ex_it=ex_it)
  pnls <- rutils::do_call(cbind, pnls)
  if (return_series) {
    pnls <- rowSums(pnls)
  } else {
    pnls <- as.numeric(pnls[NROW(pnls)])
  }  # end if
  return(pnls)
}  # end run_strategies

# define vector of trade entry levels
en_ters <- (5:30)/10
# pnls <- run_strategies(signal_s[[1]], returns, en_ters, ex_it=ex_it)
# pnls <- xts(pnls, index(ohlc))
# quantmod::chart_Series(pnls)
clusterExport(cluster, varlist=c("calc_revert"))


## old uses run_strategies(): simulate ensemble of strategies and return heatmap of final pnls
pnls <- parLapply(cluster, X=signal_s, fun=run_strategies, returns=returns, en_ters=en_ters, ex_it=ex_it, return_series=FALSE)
pnls <- rutils::do_call(rbind, pnls)
colnames(pnls) <- paste0("en_ter=", en_ters)
rownames(pnls) <- paste0("look_back=", look_backs)
heatmap(pnls, Colv=NA, Rowv=NA, col=c("red", "blue"))
rgl::persp3d(z=pnls, col="green")
plot(colSums(pnls), t="l", xlab="")


## Simulate ensemble of strategies and return the average pnls
pnls <- parLapply(cluster, X=signal_s[1:10], fun=run_strategies, returns=returns, en_ters=en_ters, ex_it=ex_it, return_series=TRUE)
pnls <- rutils::do_call(cbind, pnls)
pnls <- xts(pnls, index(ohlc))
colnames(pnls) <- paste0("look_back=", look_backs[1:10])
# plot matrix using plot.zoo()
colors <- colorRampPalette(c("red", "blue"))(NCOL(pnls))
plot.zoo(pnls[endpoints(pnls, on="days")], main="pnls", lwd=2,
         plot.type="single", xlab="", ylab="pnls", col=colors)
# Add legend
legend("bottomright", legend=colnames(pnls), col=colors, lty=1, lwd=4, inset=0.05, cex=0.8)
# plot single dygraph
pnls <- rowSums(pnls)
pnls <- xts(pnls, index(ohlc))
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls)[endpoints(pnls, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


stopCluster(cluster)  # Stop R processes over cluster under Windows

# Count the number of consecutive TRUE elements, and reset to zero after every FALSE element
roll_countr <- function(sig_nal) {
  count_true <- integer(NROW(sig_nal))
  count_true[1] <- sig_nal[1]
  for (it in 2:NROW(sig_nal)) {
    if (sig_nal[it])
      count_true[it] <- count_true[it-1] + sig_nal[it]
    else
      count_true[it] <- sig_nal[it]
  }  # end for
  return(count_true)
}  # end roll_countr
foo <- logical(21)
foo[sample(NROW(foo), 12)] <- TRUE
barr <- roll_countr(foo)
foo <- roll_countr(close_high)
bar <- hist(foo, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar <- hist(close_high_count, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar <- hist(close_low_count, breaks=0:15, xlim=c(0, 4), freq=FALSE)
bar$counts
all.equal(roll_countr(close_high), drop(HighFreq::roll_count(close_high)), check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_countr(close_high),
  rcpp=HighFreq::roll_count(close_high),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# Contrarian strategy
numrows <- NROW(ohlc)
po_sit <- rep(NA_integer_,.n_rows)
po_sit[1] <- 0
po_sit[close_high] <- (-1)
po_sit[close_low] <- 1
po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
po_sit <- rutils::lagit(po_sit, lagg=1)

# Contrarian strategy using HighFreq::roll_count()
po_sit <- rep(NA_integer_,.n_rows)
po_sit[1] <- 0
po_sit[close_high_count>2] <- (-1)
po_sit[close_low_count>2] <- 1
po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
po_sit <- rutils::lagit(po_sit, lagg=1)

# Contrarian strategy using roll_cum()
po_sit <- rep(0,.n_rows)
po_sit[close_high] <- (-1)
po_sit[close_low] <- 1
po_sit <- roll_cum(po_sit, 2)
po_sit <- rutils::lagit(po_sit, lagg=1)


# wipp
# Contrarian strategy using roll_maxmin()
look_back <- 11
max_min <- roll_maxmin(close_num, look_back)
close_max <- (close_num == max_min[, 1])
close_min <- (close_num == max_min[, 2])
volat <- HighFreq::roll_var_ohlc(ohlc=ohlc_log, look_back=5*look_back, scale=FALSE)
volat <- sqrt(volat)
volat[1] <- volat[2]
colnames(volat) <- "volat"
dra_w <- rutils::diffit(close_num, lagg=look_back)
dra_w <- as.numeric(dra_w/volat)
max_min <- roll_maxmin(dra_w, look_back)
draw_max <- (dra_w == max_min[, 1])
draw_min <- (dra_w == max_min[, 2])

po_sit <- rep(NA_integer_,.n_rows)
po_sit[1] <- 0
# po_sit[close_max] <- (-1)
# po_sit[close_min] <- 1
po_sit[(dra_w>4) & draw_max & close_max] <- (-1)
po_sit[(dra_w<(-4)) & draw_min & close_min] <- 1
po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
po_sit <- rutils::lagit(po_sit, lagg=1)

# Number of trades
sum(abs(rutils::diffit(po_sit))) / NROW(po_sit)

# Calculate strategy pnls
pnls <- cumsum(po_sit*returns)
colnames(pnls) <- "strategy"


# dygraphs plot
endp <- xts::endpoints(pnls, on="days")
dygraphs::dygraph(pnls[endp], main="ES1 strategy")
# data for plot
datav <- cbind(closep, pnls)[endp]
colnames(datav) <- c(symbol, "pnls")
# or
datav <- cbind(closep, po_sit)
colnames(datav) <- c(symbol, "position")
# dygraphs plot with two "y" axes
second_series <- colnames(datav)[2]
dygraphs::dygraph(datav, main=paste(symbol, "Strategy Using OHLC Technical Indicators")) %>%
  dyAxis("y", label=symbol, independentTicks=TRUE) %>%
  dyAxis("y2", label=second_series, independentTicks=TRUE) %>%
  dySeries(second_series, axis="y2", col=c("blue", "red"))


x11()
# dates <- index(ohlc)
po_sit <- xts::xts(po_sit, index(ohlc))
# rangev <- "2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"
rangev <- "2018-02-05/2018-02-07"
dygraphs::dygraph(pnls[rangev], main="ES1 strategy")
# Calculate integer index of date range
# rangev <- index(ohlc["2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"])
# rangev <- index(ohlc[rangev])
# rangev <- (which(dates==min(rangev)):which(dates==max(rangev)))
# plot prices
chart_Series(x=Cl(ohlc[rangev]))
# Add background shading of areas
add_TA(po_sit[rangev] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(po_sit[rangev] < 0, on=-1,
       col="lightgrey", border="lightgrey")

# Calculate integer index of date range
dates <- xts::.index(ohlc)
rangev <- xts::.index(ohlc[rangev])
rangev <- (which(dates==min(rangev)):which(dates==max(rangev)))
# Add vertical lines
# close_high_count <- xts::xts(close_high_count, index(ohlc))
# close_low_count <- xts::xts(close_low_count, index(ohlc))
close_high_count <- drop(close_high_count)
close_low_count <- drop(close_low_count)
abline(v=which(close_high_count[rangev]>0), col='red')
abline(v=which(close_low_count[rangev]>0), col='blue')

draw_max <- xts::xts(draw_max, index(ohlc))
draw_min <- xts::xts(draw_min, index(ohlc))
abline(v=draw_max[rangev], col='blue')
abline(v=draw_min[rangev], col='red')
# Add background shading of areas
chart_Series(x=Cl(ohlc[rangev]))
add_TA(draw_max[rangev], on=-1,
       col="blue", border="blue")
add_TA(draw_min[rangev], on=-1,
       col="red", border="red")

# wipp
# dygraphs plot with max_min lines
datav <- xts::xts(cbind(close_num, max_min), index(ohlc))[rangev]
colnames(datav) <- c(symbol, "max", "min")
colors <- c("blue", "red", "green")
dygraphs::dygraph(datav, main=paste(symbol, "max and min lines")) %>%
  dyOptions(colors=colors)

# Standard plot with max_min lines
# plot(as.numeric(datav[, 1]), type="l", col="blue",
#      main=paste(symbol, "max and min lines"),
#      xlab="", ylab="")
# lines(datav[, 2], col="red")
# lines(datav[, 3], col="green")
plot_theme <- chart_theme()
plot_theme$col$line.col <- colors
quantmod::chart_Series(datav, theme=plot_theme, name=paste(symbol, "max and min lines"))
legend(x="left", title=NULL, legend=colnames(datav),
       inset=0.1, cex=1.2, bg="white", bty="n",
       lwd=6, lty=1, col=colors)


# Calculate the rolling maximum and minimum over a vector of data
roll_maxminr <- function(vectorv, look_back) {
 .n_rows <- NROW(vectorv)
  max_min <- matrix(numeric(2.n_rows), nc=2)
  # Startup periods
  max_min[1, 1] <- vectorv[1]
  max_min[1, 2] <- vectorv[1]
  for (it in 2:(look_back-1)) {
    sub_vec <- vectorv[1:it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  # remaining periods
  for (it in look_back.n_rows) {
    sub_vec <- vectorv[(it-look_back+1):it]
    max_min[it, 1] <- max(sub_vec)
    max_min[it, 2] <- min(sub_vec)
  }  # end for
  return(max_min)
}  # end roll_maxminr

max_min <- roll_maxmin(close_num, look_back)
max_minr <- roll_maxminr(close_num, look_back)
all.equal(max_min, max_minr)
bar <- TTR::runMax(x=close_num, n=look_back)
all.equal(max_min[-(1:look_back), 1], bar[-(1:look_back)])
bar <- TTR::runMin(x=close_num, n=look_back)
all.equal(max_min[-(1:look_back), 2], bar[-(1:look_back)])
max_min <- xts(max_min, index(closep["2014-05"]))
dygraphs::dygraph(max_min[, 1]-closep["2014-05"])

library(microbenchmark)
summary(microbenchmark(
  tt_r=TTR::runMax(x=closep["2014-05"], n=look_back),
  rcpp=roll_maxmin(as.numeric(closep["2014-05"]), look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# end of old stuff


###############
### Strategy using static betas over OHLC technical indicators
# with regression and dimension reduction

# Load OHLC futures data
load(file="/Volumes/external/Develop/data/combined.RData")

# Define OHLC technical indicators
# residuals of the regression of the time series of closep prices
dates <- xts::.index(ohlc)
# foo <- unique(dates)
design <- matrix(dates, nc=1)
# foo <- MASS::ginv(design)
look_back <- 11
z_scores <- HighFreq::roll_zscores(response=closep,
                                   design=design,
                                   look_back=look_back)
colnames(z_scores) <- "z_scores"
z_scores[1:3] <- 0
close_open <- (closep-openp)
colnames(close_open) <- "close_open"
close_high <- (closep-highp)
colnames(close_high) <- "close_high"
close_low <- (closep-lowp)
colnames(close_low) <- "close_low"
# skew <- ((highp+lowp) - (openp+closep))
skew <- ((highp+lowp) - (openp+closep))
colnames(skew) <- "skew"
# moment_um <- ((closep-openp) - (highp-lowp))
moment_um <- ((closep-openp) - (highp-lowp)) + 1.0
colnames(moment_um) <- "moment_um"

# close_high <- (highp - rutils::lagit(highp))
# close_low <- (lowp - rutils::lagit(lowp))
# Select only independent indicators

indicator_s <- cbind(returns, volat, skew)

# indicator_s <- cbind(returns, close_open, close_high, close_low, volat, skew, moment_um, z_scores)
# colnames(indicator_s) <- c("close_high", "openp_highp", "closep_highp")
# indicator_s <- cbind(returns, volat, skew, moment_um, indicator_s)
# indicator_s <- cbind(openp-highp, openp-lowp, openp-closep, closep-highp, closep-lowp, highp-lowp)
# colnames(indicator_s) <- c("open_high", "open_low", "open_close", "close_high", "close_low", "high_low")
# indicator_s <- cbind(skew, moment_um, indicator_s)
# Select only independent indicators
# indicator_s <- cbind(openp-highp, closep-highp)
# colnames(indicator_s) <- c("openp_highp", "closep_highp")
# indicator_s <- cbind(returns, skew, indicator_s)
colnamev <- colnames(indicator_s)

# Scale indicator_s using roll_scale()
look_back <- 11
indicator_s <- roll::roll_scale(data=indicator_s, width=look_back, min_obs=1)
indicator_s[1, ] <- 0
round(cor(indicator_s), 3)
indicator_s <- cbind(indicator_s, z_scores)
indicator_s[1:3, ] <- 0
colnamev <- colnames(indicator_s)


# Scale indicator_s using sigmoid
indicator_s <- lapply(1:NCOL(indicator_s), function(colnum) {
  x <- plogis(indicator_s[, colnum], scale=-quantile(indicator_s[, colnum], 0.01))
  (x - median(x))
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)

round(cor(indicator_s), 3)

# Calculate PCA of technical indicators
pcad <- prcomp(indicator_s)
pcad$sdev
pcad$rotation


## Create design matrix for SPY or ES1
# design <- cbind(returns, close_high, close_low, rutils::diffit(variance), rutils::diffit(volumes))
# design from pcad
# rolling average
indicator_s <- lapply(1:NCOL(indicator_s), function(colnum) {
  HighFreq::roll_sum(indicator_s[, colnum], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- colnamev
# design <- as.data.frame(cbind(returns_adv, returns, variance))
# colnames(design) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
design <- as.data.frame(cbind(returns_adv, indicator_s))
colnames(design)[1] <- "returns_adv"
# or
design <- cbind(HighFreq::roll_sum(returns, look_back=look_back),
                 HighFreq::roll_sum(moment_um, look_back=look_back),
                 HighFreq::roll_sum(skew, look_back=look_back))
design <- as.data.frame(cbind(returns_adv, design))
# design <- cbind(returns_adv>0, design)
colnames(design) <- c("indic", "returns", "momentum", "skew")

# design <- cbind(design, rutils::lagit(design, lagg=1), rutils::lagit(design, lagg=2), rutils::lagit(design, lagg=3), rutils::lagit(design, lagg=4))
# design <- cbind(returns, close_high, close_low, returns/sqrt(variance), close_high/sqrt(variance), variance, volumes)
# colnames(design)[4:5] <- c("returns_s", "close_high_s")
## Apply rolling centering and scaling to the design matrix
design <- lapply(design, function(x) (x-mean(x))/sd(x))
design <- rutils::do_call(cbind, design)
sum(is.na(design))



## Create design matrix for ES1, TY1, UX1
# design <- cbind(returns, close_high, close_low, rutils::diffit(variance), rutils::diffit(volumes))
# design from pcad
# define indicators
look_back <- 5
indicator_s <- c("ES1.Close", "TY1.Close", "TU1.Close", "UX1.Close", "UX2.Close")
dygraphs::dygraph(ohlc[, indicator_s[2]]-ohlc[, indicator_s[3]])
indicator_s <- lapply(indicator_s, function(colnum) {
  colnum <- ohlc[, colnum]
  sig_nal <- rutils::diffit(closep, lagg=look_back)/sqrt(look_back)/sqrt(HighFreq::roll_var_ohlc(ohlc=ohlc, look_back=look_back, scale=FALSE))
  HighFreq::roll_sum(indicator_s[, colnum], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- colnamev
design <- as.data.frame(cbind(returns_adv, design))
# colnames(design) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
design <- as.data.frame(cbind(returns_adv, indicator_s))
colnames(design)[1] <- "returns_adv"
# or
design <- cbind(HighFreq::roll_sum(returns, look_back=look_back),
                 HighFreq::roll_sum(moment_um, look_back=look_back),
                 HighFreq::roll_sum(skew, look_back=look_back))
design <- as.data.frame(cbind(returns_adv, design))
# design <- cbind(returns_adv>0, design)
colnames(design) <- c("indic", "returns", "momentum", "skew")

# design <- cbind(design, rutils::lagit(design, lagg=1), rutils::lagit(design, lagg=2), rutils::lagit(design, lagg=3), rutils::lagit(design, lagg=4))
# design <- cbind(returns, close_high, close_low, returns/sqrt(variance), close_high/sqrt(variance), variance, volumes)
# colnames(design)[4:5] <- c("returns_s", "close_high_s")
## Apply rolling centering and scaling to the design matrix
design <- lapply(design, function(x) (x-mean(x))/sd(x))
design <- rutils::do_call(cbind, design)
sum(is.na(design))





## run regressions of future returns against different indicators

# lm formula
colnamev <- colnames(design)
formulav <- as.formula(paste(colnamev[1], paste(colnamev[-1], collapse=" + "), sep="~"))
formulav <- as.formula(paste(colnamev[1], paste(paste(colnamev[-1], collapse=" + "), "- 1"), sep="~"))


# find extreme returns
excess <- which(returns_adv > quantile(returns_adv, 0.99) | returns_adv < quantile(returns_adv, 0.01))
excess <- sort(unique(c(excess, excess+1, excess-1)))

# perform regression
model <- lm(formulav, data=design)
# model <- lm(returns_adv[-excess] ~ design[-excess, ])
model_sum <- summary(model)
model_sum$coefficients
weights <- model_sum$coefficients[, 1]
weights <- model_sum$coefficients[, 1][-1]
sig_nal <- xts(as.matrix(design)[, -1] %*% weights, order.by=index(ohlc))
sig_nal <- rutils::lagit(sig_nal)


# Signal from z-scores (t-values) of trailing slope
design <- matrix(xts::.index(ohlc), nc=1)
look_back <- 3
sig_nal <- HighFreq::roll_zscores(response=closep, design=design, look_back=look_back)
sig_nal <- roll::roll_scale(data=sig_nal, width=look_back, min_obs=1)
sig_nal[1:look_back, ] <- 0
sig_nal[is.infinite(sig_nal)] <- NA
sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
sum(is.infinite(sig_nal))
sum(is.na(sig_nal))
sd(sig_nal)
hist(sig_nal, freq=FALSE)
plot(sig_nal, t="l")


# wipp
# Simulate ensemble of strategies using slope as technical indicator
# mean-reverting strategies
# par_am <- cbind(6:10, rep((3:12)/10, each=NROW(6:10)))
posit_mat <- sapply(4:8, function(look_short) {
  # mean reverting signal
  sig_nal_short <- calc_signal(ohlc=ohlc_log,
                               closep=close_num,
                               design=design,
                               look_short=look_short, high_freq=FALSE)
  # Simulate the positions of mean reverting strategy
  sim_revert(sig_nal_short, returns, close_high, close_low, en_ter, ex_it, trade_lag=1)
})  # end sapply
par_am <- cbind(8:12, rep((3:12)/10, each=NROW(8:12)))
posit_mat <- sapply(1:NROW(par_am), function(it) {
  look_short <- par_am[it, 1]
  en_ter <- par_am[it, 2]
  sig_nal <- HighFreq::roll_zscores(response=closep, design=design, look_back=look_short)
  sig_nal[1:look_short, ] <- 0
  # Scale sig_nal using roll_scale()
  sig_nal <- roll::roll_scale(data=sig_nal, width=look_short, min_obs=1)
  sig_nal[1:look_short, ] <- 0
  # sig_nal <- rutils::lagit(sig_nal, lagg=1)
  # Calculate positions, either: -1, 0, or 1
  po_sit <- rep(NA_integer_,.n_rows)
  po_sit[1] <- 0
  po_sit[sig_nal < (-en_ter)] <- 1
  po_sit[sig_nal > en_ter] <- (-1)
  zoo::na.locf(po_sit, na.rm=FALSE)
})  # end sapply
po_sit <- rowMeans(posit_mat)
po_sit[is.na(po_sit)] <- 0
po_sit <- rutils::lagit(po_sit, lagg=1)
# plot(po_sit, t="l")
pnls <- cumsum(po_sit*returns)
pnls <- closep + 2*pnls
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls)[endpoints(closep, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# Scale returns using MAD median
num_returns <- as.numeric(returns)
foo <- sapply((look_back+1):NROW(num_returns), function(it) {
  sub_vec <- num_returns[(it-look_back+1):it]
  (num_returns[it]-median(sub_vec))/mad(sub_vec, constant=1.0)
})  # end sapply
tail(foo)
bar <- HighFreq::roll_scale(matrixv=returns, look_back=look_back, use_median=TRUE)
bar[is.infinite(bar), ] <- 0
tail(drop(bar))
summary(microbenchmark(
  roll=roll::roll_scale(data=num_returns, width=look_back, min_obs=1),
  rcpp=roll_scale(matrixv=num_returns, look_back=look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

foo <- (num_returns-median(num_returns))/mad(num_returns, constant=1.0)
bar <- HighFreq::calc_scaled(matrixv=num_returns)
all.equal(foo, drop(bar))

library(microbenchmark)
summary(microbenchmark(
  pure_r=(num_returns-median(num_returns))/mad(num_returns, constant=1.0),
  rcpp=HighFreq::calc_scaled(matrixv=num_returns),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# wipp
# Newer code: optimize strategies using slope as technical indicator

# OHLC data setup
ohlc <- log(HighFreq::SPY["2010-10/2010-11"])
ohlc_log <- log(ohlc)
openp <- Op(ohlc_log)
highp <- Hi(ohlc_log)
lowp <- Lo(ohlc_log)
closep <- Cl(ohlc_log)
returns <- rutils::diffit(closep)
# colnames(returns) <- "returns"
close_num <- as.numeric(closep)
close_high <- (close_num == high_num)
close_low <- (close_num == low_num)
indeks <- 1:NROW(ohlc)
design <- matrix(indeks, nc=1)

look_back <- 15
run_signal <- function(look_back, returns) {
  sig_nal <- HighFreq::roll_scale(matrixv=returns, look_back=look_back, use_median=TRUE)
  sig_nal[1:look_back, ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
  rutils::lagit(sig_nal, lagg=1)
}  # end run_signal
sig_nal <- run_signal(look_back, returns)
run_signal <- function(look_back, closep, design) {
  sig_nal <- HighFreq::roll_zscores(response=closep, design=design, look_back=look_back)
  sig_nal[1:look_back, ] <- 0
  # sig_nal <- HighFreq::roll_scale(matrixv=sig_nal, look_back=look_back, use_median=TRUE)
  # sig_nal[1:look_back, ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal, na.rm=FALSE)
  rutils::lagit(sig_nal, lagg=1)
}  # end run_signal
sig_nal <- run_signal(look_back, closep, design)
hist(sig_nal, freq=FALSE)
hist(sig_nal, xlim=c(-10, 10), freq=FALSE)

# perform parallel loop over look_backs under Windows
look_backs <- 15:35
library(parallel)
cluster <- makeCluster(num_cores-1)
clusterExport(cluster, varlist=c("closep", "design"))
signal_s <- parLapply(cluster, X=look_backs, fun=run_signal, closep=closep, design=design)


# close_high and close_low are Boolean vectors which are TRUE if the close price is at the high or low price
run_strategy <- function(sig_nal, returns, en_ter, ex_it, close_high=TRUE, close_low=TRUE) {
  po_sit <- rep(NA_integer_, NROW(sig_nal))
  po_sit[1] <- 0
  # po_sit[sig_nal < (-en_ter)] <- 1
  po_sit[(sig_nal < (-en_ter)) & close_low] <- 1
  # po_sit[sig_nal > en_ter] <- (-1)
  po_sit[(sig_nal > en_ter) & close_high] <- (-1)
  po_sit[abs(sig_nal) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
  po_sit <- po_sit + rutils::lagit(po_sit, lagg=1)
  pnls <- cumsum(po_sit*returns)
  pnls[NROW(pnls)]
  # colnames(pnls) <- "strategy"
}  # end run_strategy
# trade entry and exit levels
en_ter <- 2.0
ex_it <- 0.5
pnls <- run_strategy(signal_s[[1]], returns, en_ter, ex_it, close_high, close_low)


run_strategies <- function(sig_nal, returns, en_ters, ex_it, close_high=TRUE, close_low=TRUE) {
  sapply(en_ters, run_strategy, sig_nal=sig_nal, returns=returns, ex_it=ex_it, close_high=close_high, close_low=close_low)
  # pnls <- lapply(en_ters, run_strategy, sig_nal=sig_nal, returns=returns, ex_it=ex_it)
  # pnls <- rutils::do_call(cbind, pnls)
  # rowSums(pnls)
}  # end run_strategies
# trade entry levels
en_ters <- (5:40)/10
foo <- run_strategies(signal_s[[1]], returns, en_ters, ex_it=ex_it, close_high, close_low)
clusterExport(cluster, varlist=c("run_strategy"))
pnls <- parLapply(cluster, X=signal_s, fun=run_strategies, returns=returns, en_ters=en_ters, ex_it=ex_it, close_high=close_high, close_low=close_low)

stopCluster(cluster)  # Stop R processes over cluster under Windows
pnls <- rutils::do_call(cbind, pnls)
rownames(pnls) <- paste0("en_ter=", en_ters)
colnames(pnls) <- paste0("look_back=", look_backs)
heatmap(pnls, Colv=NA, Rowv=NA, col=c("red", "blue"))
pnls <- rowSums(pnls)
pnls <- xts(pnls, index(ohlc))
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls)[endpoints(pnls, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# trade ensemble of strategies using slope as technical indicator
# mean-reverting strategies
foo <- sapply(2:15, function(look_back) {
  sig_nal <- HighFreq::roll_zscores(response=closep,
                          design=design,
                          look_back=look_back)
  sig_nal[1:3, ] <- 0
  # sig_nal <- rutils::lagit(sig_nal)
  -sign(sig_nal)
})  # end sapply
# trending strategies
bar <- sapply(10*(10:15), function(look_back) {
  sig_nal <- HighFreq::roll_zscores(response=closep,
                          design=design,
                          look_back=look_back)
  sig_nal[1:3, ] <- 0
  # sig_nal <- rutils::lagit(sig_nal)
  sign(sig_nal)
})  # end sapply
po_sit <- cbind(foo, bar)
po_sit <- rowMeans(po_sit)
po_sit[is.na(po_sit)] <- 0
po_sit <- rutils::lagit(po_sit)
plot(po_sit, t="l")
pnls <- cumsum(po_sit*returns)
pnls <- closep + 3*pnls
colnames(pnls) <- "strategy"
dygraphs::dygraph(cbind(closep, pnls), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# regress returns_adv versus moment_um indicator
# moment_um <- moment_um[abs(moment_um)>0.9]
foo <- sapply(1:10, function(look_back) {
  if (look_back>1)
    returns_adv <- rutils::lagit(HighFreq::roll_sum(returns, look_back=look_back), lagg=-look_back)/look_back
  else
    returns_adv <- rutils::lagit(returns, lagg=-look_back)
  model <- lm(returns_adv ~ moment_um)
  model_sum <- summary(model)
  model_sum$coefficients[2, 3]
})  # end sapply

# use average returns as predictor
foo <- sapply(1:11, function(look_back) {
  if (look_back>1)
    returns <- HighFreq::roll_sum(returns, look_back=look_back)/look_back
  else
    returns <- returns
  model <- lm(returns_adv ~ returns)
  model_sum <- summary(model)
  model_sum$coefficients[2, 3]
})  # end sapply

foo <- cbind(returns_adv, returns)
colnamev <- colnames(foo)
formulav <- as.formula(paste(colnamev[1], paste(colnamev[-1], collapse=" + "), sep="~"))
# perform regression
# returns_adv <- plogis(returns_adv, scale=-quantile(foo, 0.1))
excess <- ((foo[, 2]>quantile(foo[, 2], 0.05)) & (foo[, 2]<quantile(foo[, 2], 0.95)))
model <- lm(formulav, data=foo[excess])
model_sum <- summary(model)
model_sum
plot(formulav, data=foo[excess])
abline(model, lwd=2, col="red")

# perform optimization
# objective function equal to the strategy Sharpe ratio
# plus a penalty term for the weight constraint:
# sum(weights) == 1.
object_ive <- function(weights, indicator_s, returns) {
  sig_nal <- rutils::lagit(indicator_s %*% weights)
  pnls <- sig_nal*returns
  se_lect <- ((pnls>quantile(pnls, 0.05)) & (pnls<quantile(pnls, 0.95)))
  pnls <- pnls[se_lect]
  -mean(pnls)/sd(pnls) + (sum(weights) - 1)^2
}  # end object_ive

# perform parameter optimization using function optim()
optimd <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s,
                returns=returns)
weights <- optimd$par
names(weights) <- colnames(indicator_s)
sig_nal <- xts(indicator_s %*% weights, order.by=index(ohlc))
sig_nal <- rutils::lagit(sig_nal)


# perform logistic regression
glmod <- glm(formulav, data=design, family=binomial)
summary(glmod)
glm_predict <- predict(glmod, newdata=design, type="response")
en_ter <- 0.58
forecastvs <- data.frame((glm_predict>en_ter), coredata(design[, 1]))
colnames(forecastvs) <- c("lm_pred", "realized")
table(forecastvs)
sig_nal <- xts(design %*% rota_tion, order.by=index(ohlc))


# perform lda
l_da <- MASS::lda(formulav, data=design)
summary(l_da)
lda_predict <- predict(l_da, newdata=design)
forecastvs <- data.frame(lda_predict$class, coredata(design[, 1]))
colnames(forecastvs) <- c("lda_pred", "realized")
table(forecastvs)

# perform qda
q_da <- MASS::qda(formulav, data=design)
summary(q_da)
qda_predict <- predict(q_da, newdata=design)
forecastvs <- data.frame(qda_predict$class, coredata(design[, 1]))
colnames(forecastvs) <- c("qda_pred", "realized")
table(forecastvs)


# Calculate PCA of design
pcad <- prcomp(design)
pcad$sdev
pcad$rotation
# lm
model <- lm(returns_adv ~ pcad$x - 1)
model_sum <- summary(model)
model_sum$coefficients


# Curated PCs
rota_tion <- cbind(PC1=rep(0.2, 5),
                   PC2=c(-2, -1, 0, 1, 2),
                   PC3=c(-1, 0.5, 1, 0.5, -1))
pcats <- xts(design %*% rota_tion, order.by=index(design))

model <- lm(returns_adv ~ pcats - 1)
model_sum <- summary(model)
model_sum$coefficients


## Perform in-sample
in_sample <- 1:2000
# Define OHLC technical indicators
indic_in <- indicator_s[in_sample]
# Scale indic_in using sigmoid
indic_in <- lapply(1:NCOL(indic_in), function(colnum) {
  x <- plogis(indic_in[, colnum], scale=-quantile(indic_in[, colnum], 0.01))
  (x - median(x))
})  # end lapply
indic_in <- rutils::do_call(cbind, indic_in)
design_in <- as.data.frame(cbind(returns_adv[in_sample], indic_in))
colnames(design_in)[1] <- "returns_adv"

# perform optimization
optimd <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s[in_sample],
                returns=returns[in_sample])
weights <- optimd$par
names(weights) <- colnames(indicator_s)
sig_nal <- xts(indicator_s %*% weights, order.by=index(ohlc))
sig_nal <- rutils::lagit(sig_nal)


# perform regression
model <- lm(formulav, data=design_in)
# model <- lm(returns_adv[-excess] ~ design[-excess, ])
model_sum <- summary(model)
weights <- model_sum$coefficients[, 1][-1]

# or
pcad <- prcomp(design[in_sample, ])
pcad$sdev
pcad$rotation
model <- lm(returns_adv[in_sample] ~ pcad$x - 1)
model_sum <- summary(model)
model_sum$coefficients

# or
model <- lm(returns_adv[in_sample] ~ pcats[in_sample] - 1)
model_sum <- summary(model)

# or
model <- lm(returns_adv[in_sample] ~ design[in_sample, ] - 1)
model_sum <- summary(model)


weights <- model_sum$coefficients[, 1]
# weights <- weights[-1]
t_vals <- rep(TRUE, NROW(weights))
t_vals <- (abs(model_sum$coefficients[, 3]) > 2)
weights[!t_vals] <- 0


## Perform out-of-sample
out_sample <- 2001:NROW(ohlc)
# Define OHLC technical indicators
indic_out <- indicator_s[out_sample, ]
# Scale indic_in using sigmoid
indic_out <- lapply(1:NCOL(indic_out), function(colnum) {
  x <- plogis(indic_out[, colnum], scale=-quantile(indicator_s[in_sample, colnum], 0.01))
  (x - median(indicator_s[in_sample, colnum]))
})  # end lapply
indic_out <- rutils::do_call(cbind, indic_out)
sig_nal <- xts(indic_out %*% weights, order.by=index(ohlc[out_sample]))
# Simulate strategy
pnls <- cumsum(sig_nal*returns[out_sample])
colnames(pnls) <- "strategy"


# or
sig_nal <- xts(as.matrix(design)[, t_vals] %*% weights[t_vals], order.by=index(ohlc))
sig_nal <- rutils::lagit(sig_nal)

# or
sig_nal <- xts(design %*% rota_tion, order.by=index(ohlc))
# sig_nal <- xts(design %*% pcad$rotation[, t_vals], order.by=index(ohlc))
sig_nal <- xts(as.matrix(design)[, -1] %*% weights, order.by=index(ohlc))
sig_nal <- xts(sig_nal[, t_vals] %*% weights[t_vals], order.by=index(ohlc))
sig_nal <- rutils::lagit(sig_nal)


# Simulate strategy
pnls <- cumsum(sig_nal*returns)
colnames(pnls) <- "strategy"

# plot
library(dygraphs)
dygraphs::dygraph(cbind(closep, pnls), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))



# design <- cbind(rets_lag2, z_scores[[3]], hu_rst, sharpe_rolling)
# colnames(design) <- c("returns", "variance", "skew", "hurst")
endp <- xts::endpoints(design, "years")

## Apply rolling centering and scaling to the design matrix
# library(roll)
design <- roll::roll_scale(data=design, width=100*look_back, min_obs=1)
# remove NAs
design[is.na(design)] <- 0
sum(is.na(design))

## perform regressions of future returns against different indicators

# Single indicator
returns_adv <- returns + close_high + close_low
model <- lm(returns_adv ~ returns_adv)
summary(model)

# three indicators - lower lows is most significant
model <- lm(returns_adv ~ returns + close_high + close_low)
summary(model)

# Single indicator
# lower lows indicator works well in bearish periods
returns_adv <- -returns - close_high + close_low
returns_adv <- sign(returns_adv)
model <- lm(returns_adv ~ returns_adv)
summary(model)

mo_del <- lm(rets_adv2 ~ design)
summary(mo_del)
coef(summary(mo_del))
betas <- -coef(summary(mo_del))[-1, 1]

max_eigen <- 2
covmat <- cov(excess)

# Calculate eigen decomposition
eigend <- eigen(matrixv)
eigen_values <- eigend$values
eigen_vec <- eigend$vectors

# Check for zero singular values
# Set tolerance for determining zero singular values
precision <- sqrt(.Machine$double.eps)
not_zero <- (eigen_values > (precision*eigen_values[1]))

# Calculate generalized inverse from eigen decomposition
eigen_inverse <- eigen_vec[, not_zero] %*%
  (t(eigen_vec[, not_zero]) / eigen_values[not_zero])

# perform eigen decomposition and calculate eigenvectors and eigenvalues
eigend <- eigen(covmat)
eigen_vec <- eigend$vectors
# Calculate regularized inverse
inverse <- eigen_vec[, 1:max_eigen] %*% (t(eigen_vec[, 1:max_eigen]) / eigend$values[1:max_eigen])
# Calculate the maximum Sharpe ratio portfolio weights
# weights <- inverse %*% colMeans(excess)
# weights <- rep(mean(colMeans(excess)), NCOL(excess))
weights <- colMeans(excess)
weights <- inverse %*% weights
weights <- drop(weights/sqrt(sum(weights^2)))


# Simulate strategy
pnls <- cumsum(sig_nal*returns)
colnames(pnls) <- "strategy"




###############
### State space model and Kalman filter

## Simulate state space model

# Length of data
#.n_rows <- NROW(endp)
numrows <- 100  # Number of time points
# n <- 5    # Number of observations at each time point
# p <- 2    # Number of covariates


# True parameter values

rho_v <- 2.0
rho_w <- 1.0
gg <- 1.0
hh <- 1.0

# Allocate state vector xx
xx <- numeric.n_rows)
# Transition equation for state vector under AR(1) process
set.seed(1121)
# xx[1] <- rnorm(1, sd=rho_w)
for (it in 2.n_rows) {
  xx[it] <- gg*xx[it-1] + rnorm(1, sd=rho_w)
}  # end for

# Measurement equation for measured vector
yy <- hh*xx + rnorm.n_rows, sd=rho_v)

# Plot
x11()
matplot(cbind(xx, yy), type="l", xlab="", ylab="", lty=1, col=1:2, lwd=2,
        main="State space model \nand Kalman filter")
legend("top", leg=c("state", "observed"), lty=1, lwd=6, col=1:2, inset=0.05)


## Apply Kalman filter

# process matrix
aa <- 1.0
# process variance
qq <- 1.0
# measurement variance
rr <- 1.0

# Allocate predicted vector zz
zz <- numeric.n_rows)
zz[1] <- aa*yy[1]
# Allocate process variance vector pp
pp <- numeric.n_rows)
pp[1] <- 1
# Allocate predicted variance vector ppp
ppp <- numeric.n_rows)
ppp[1] <- aa^2*pp[1] + qq
# Allocate Kalman gain vector kk
kk <- numeric.n_rows)
kk[1] <- ppp[1]*hh/(ppp[1]*hh^2+rr)

# Apply Kalman filter recursivelly
for (it in 2.n_rows) {
  # Prediction equations for predicted vector zz and predicted variance pp
  zz[it] <- aa*yy[it-1]
  ppp[it] <- aa^2 + qq
  # Correction (measurement) equations
  kk[it] <- ppp[it]*hh/(ppp[it]*hh^2+rr)
  zz[it] <- zz[it] + kk[it]*(yy[it] - hh*zz[it])
  pp[it] <- (1-kk[it]*hh)*ppp[it]
}  # end for


# Plot
x11()
matplot(cbind(xx, yy, zz), type="l", xlab="", ylab="", lty=1, col=1:3, lwd=2,
        main="State space model \nand Kalman filter")
legend("top", leg=c("state", "observed", "Kalman filter"), lty=1, lwd=6, col=1:3, inset=0.05)



set.seed(1121)
d_lm <- dlm::dlm(FF=hh, V=rho_v, GG=gg, W=rho_w, m0=0, C0=100)

a_r <- dlm::dlmModARMA(ar=gg, ma=0.0, sigma2=rho_w)

# Generate data
X <- array(rnorm.n_rows*n*p), c(n, p,.n_rows))
X[, 1, ] <- 1


Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/kalman_filter.cpp")




# Calculate ETF prices
symbolv <- colnames(rutils::etfenv$prices)
symbolv <- symbolv[!(symbolv=="VXX")]
prices <- rutils::etfenv$prices[, symbolv]
# Carry forward non-NA prices
prices <- zoo::na.locf(prices, na.rm=FALSE)
prices <- na.omit(prices)
# Calculate simple ETF returns
returns <- rutils::diffit(prices)
# Calculate the daily excess returns
# riskf is the daily risk-free rate
riskf <- 0.03/260
excess <- returns - riskf


# Define monthly endp without initial warmpup period
endp <- rutils::calc_endpoints(returns, interval="months")
endp <- endp[endp>50]
numrows <- NROW(endp)
# Define 12-month look_back interval and startp over sliding window
look_back <- 12
startp <- c(rep_len(1, look_back-1), endp[1:.n_rows-look_back+1)])

# Define the shrinkage intensity
alpha <- 0.5
max_eigen <- 3

# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(endp),
                     function(i) {
                       # Subset the excess returns
                       excess <- excess[startp[i-1]:endp[i-1], ]
                       eigend <- eigen(cov(excess))
                       # Calculate regularized inverse of covariance matrix
                       max_eigen <- 3
                       eigen_vec <- eigend$vectors[, 1:max_eigen]
                       eigen_val <- eigend$values[1:max_eigen]
                       inverse <- eigen_vec %*% (t(eigen_vec) / eigen_val)
                       # Apply shrinkage to the mean returns
                       col_means <- colMeans(excess)
                       col_means <- ((1-alpha)*col_means + alpha*mean(col_means))
                       # Calculate weights using R
                       weights <- inverse %*% col_means
                       weights <- weights/sum(weights)
                       # Subset the returns to out-of-sample returns
                       returns <- returns[(endp[i-1]+1):endp[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(returns %*% weights, index(returns))
                     }  # end anonymous function
)  # end lapply

# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"


# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(endp),
                     function(i) {
                       # Subset the excess returns
                       excess <- excess[startp[i-1]:endp[i-1], ]
                       # Apply regularized inverse to mean of excess
                       weights <- HighFreq::calc_weights(excess, max_eigen, alpha)
                       # Subset the returns to out-of-sample returns
                       returns <- returns[(endp[i-1]+1):endp[i], ]
                       # Calculate the out-of-sample portfolio returns
                       xts(returns %*% weights, index(returns))
                     }  # end anonymous function
)  # end lapply
# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"

indicator_s <- HighFreq::roll_portf(excess, returns, startp-1, endp-1, max_eigen, alpha)
indicator_s <- xts(indicator_s, index(returns))
colnames(indicator_s) <- "strat_rets"

# Compare RcppArmadillo with R
all.equal(strat_rets, indicator_s[index(strat_rets)])

# Plot dygraph
dygraphs::dygraph(cumsum(indicator_s),
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")



###############
### Benchmark eigen decomposition function in RcppArmadillo

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/calc_eigen.cpp")

eigend <- calc_eigen(scale(prices_ts, scale=FALSE))
model <- prcomp(prices_ts)
all.equal(model$sdev^2, drop(eigend$values))
all.equal(unname(model$rotation), -eigend$vectors)

library(microbenchmark)
summary(microbenchmark(
  rcpp=calc_eigen(prices_ts),
  pure_r=prcomp(prices_ts),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### ARIMA simulation

library(rutils)

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/sim_arima.cpp")

coeff <- -0.8
innov <- rnorm(100)

arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
for (i in 2:NROW(innov)) arimav[i] <- coeff*arimav[i-1] + innov[i]
foo <- arimav

arimav <- filter(c(innov), filter=coeff, method="recursive")
all.equal(as.numeric(arimav), foo)


# two coeff
coeff <- c(-0.8, 0.2)

# old
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
arimav[2] <- coeff[1]*arimav[1] + innov[2]
for (i in 3:NROW(innov)) {
  arimav[i] <- coeff[1]*arimav[i-1] + coeff[2]*arimav[i-2] + innov[i]
}  # end for
foo <- arimav

# vectorized
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
for (i in 2:NROW(coeff)) {
  arimav[i] <- coeff[1:(i-1)] %*% arimav[(i-1):1] + innov[i]
}  # end for

for (i in (NROW(coeff)+1):NROW(innov)) {
  arimav[i] <- coeff %*% arimav[(i-1):(i-NROW(coeff))] + innov[i]
}  # end for
foo <- arimav

arimav <- filter(innov, filter=coeff, method="recursive")
all.equal(as.numeric(arimav), foo)


# vectorized vector of coeff
coeff <- c(-0.8, 0.2)
arimav <- numeric(NROW(innov))
arimav[1] <- innov[1]
for (i in 2:NROW(coeff)) {
  arimav[i] <- coeff[1:(i-1)] %*% arimav[(i-1):1] + innov[i]
}  # end for

for (i in (NROW(coeff)+1):NROW(innov)) {
  arimav[i] <- coeff %*% arimav[(i-1):(i-NROW(coeff))] + innov[i]
}  # end for
foo <- arimav

arimav <- filter(innov, filter=coeff, method="recursive")
all.equal(as.numeric(arimav), foo)

arimav <- sim_arima(innov, rev(coeff))
all.equal(as.numeric(arimav), foo)

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::sim_arima(innov, rev(coeff)),
  pure_r=filter(innov, filter=coeff, method="recursive"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### Convolutions and filtering

# Rcpp::sourceCpp(file="C:/Develop/lecture_slides/assignments/roll_wsum.cpp")
library(HighFreq)

weights <- c(1, rep(1e-5, 10))

weights <- exp(-0.2*1:11)
weights <- weights/sum(weights)
vectorv <- as.numeric(rutils::etfenv$VTI[, 6])
weight_ed <- HighFreq::roll_wsum(vectorv=vectorv, weights=rev(weights))
filter_ed <- filter(x=vectorv, filter=weights, method="convolution", sides=1)

all.equal(as.numeric(vectorv), as.numeric(weight_ed))

all.equal(as.numeric(filter_ed[-(1:10)]), as.numeric(weight_ed))


library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vectorv=vectorv, weights=weights),
  pure_r=filter(x=vectorv, filter=weights, method="convolution", sides=1, circular=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


filter_ed <- roll_wsum_arma(vectorv, rev(weights))
all.equal(as.numeric(filter_ed[-(1:10)]), as.numeric(weight_ed))

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vectorv=vectorv, weights=weights),
  arma=roll_wsum_arma(vectorv, rev(weights)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### Stitching and Updating Data for S&P500 Constituents
# in tests already

library(rutils)

# load new data
load("C:/Develop/lecture_slides/data/sp5002018.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_new are the new symbols
symbols_new <- ls(sp500env)
# sp500env_new is the new data
sp500env_new <- sp500env


# load old data
load("C:/Develop/lecture_slides/data/sp5002017.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_old are the old symbols
symbols_old <- ls(sp500env)
# sp500env_old is the old data
sp500env_old <- sp500env
rm(sp500env)

# find the new symbols that are not in the old symbols
is_in <- symbols_new %in% symbols_old
symbols_new[!is_in]

# find the old symbols that are in the new symbols
is_in <- symbols_old %in% symbols_new
symbolv <- symbols_old[is_in]
# find the old symbols that are not in the new symbols
symbols_old[!is_in]

# Create a new environment to store the updated data
sp500env <- new.env()
# Copy the old symbols that are also in the new symbols from sp500env_old to sp500env
# for (symbol in symbolv) {
#   assign(symbol, get(symbol, envir=sp500env_old), envir=sp500env)
#   # sp500env$symbol <- sp500env_old$symbol
# }  # end for

# Stitch the old and new data and copy it into sp500env
for (symbol in symbolv) {
  # get old data
  old_data <- get(symbol, envir=sp500env_old)
  endd <- end(old_data)
  # get new data
  new_data <- get(symbol, envir=sp500env_new)
  # Stitch the old and new data only if old is older
  if (start(old_data) < start(new_data)) {
    closep <- new_data[, 4]
    # diff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] - as.numeric(closep))
    # diff the Close prices
    closep <- rutils::diffit(log(closep))
    # Calculate new extended Close prices
    new_close <- as.numeric(old_data[endd, 4])*exp(cumsum(closep[index(new_data)>endd]))
    # foo <- as.numeric(new_data[endd, 4])*exp(cumsum(closep[index(new_data)>endd]))
    # all.equal(new_data[index(new_data)>endd, 4], foo)
    # Stitch the Close prices
    new_close <- rbind(old_data[, 4], new_close)
    # all.equal(NROW(index(new_close)), NROW(unique(index(new_close))))
    # new_data <- (new_data[, 1:3] + as.numeric(new_close))
    # undiff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] + as.numeric(new_close[index(new_data)]))
    new_data[, 4] <- new_close[index(new_data)]
    new_data[, 6] <- new_close[index(new_data)]
    # Stitch all the prices
    new_data <- rbind(old_data, new_data[index(new_data)>endd])
  }  # end if
  # Copy the data
  assign(symbol, new_data, envir=sp500env)
}  # end for

# verify that all symbols were stitched
all.equal(symbolv, ls(sp500env))
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(sp500env, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))

save(sp500env, file="C:/Develop/lecture_slides/data/sp500.RData")



###############
### sweep() for matrix multiplication

matrixv1 <- matrix(rnorm(1e6), ncol=100)
vectorv <- rnorm(1e2)
matrixv2 <- diag(vectorv)
product <- matrixv1 %*% matrixv2
product2 <- sweep(matrixv1, 2, vectorv, FUN="*")
all.equal(product, product2)

# sweep() is about 5 times faster
library(microbenchmark)
summary(microbenchmark(
  matrix_mult=(matrixv1 %*% matrixv2),
  sweep=sweep(matrixv1, 2, vectorv, FUN="*"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### Portfolio optimization with constraints

# This is an objective function equal to the portfolio
# variance plus a penalty term for the weight constraint:
# sum(weights) == 1.

object_ive <- function(weights, returns) {
  var(returns %*% weights) +
    (sum(weights) - 1)^2
}  # end object_ive

# Perform portfolio optimization with the same two weight
# constraints as in p.1
# You must use function optim().

optimd <- optim(par=rep(1.0, NCOL(returns)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(returns)),
                lower=rep(-1, NCOL(returns)),
                returns=returns)

weights <- optimd$par

var(returns %*% weights)


# You should get output similar to the following:
# > optimd$par


object_ive <- function(weights, returns, confl, portfolio_sub) {
  # pnls <- returns %*% weights
  # var(pnls) +
  t(weights) %*% covmat %*% weights +
    1000*(sum(weights) - 1)^2 +
    1000*(sum(weights*portfolio_sub[-1]) - portfolio_sub[1])^2
}  # end object_ive



###############
### plot multiple dygraphs in the same RStudio window

# Create the time series
temperature <- ts(frequency = 12, start = c(1980, 1),
                  data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5,
                           25.2, 26.5, 23.3, 18.3, 13.9, 9.6))
rainfall <- ts(frequency = 12, start = c(1980, 1),
               data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0,
                        135.6, 148.5, 216.4, 194.1, 95.6, 54.4))

# Create a list of dygraphs objects
library(dygraphs)
dyplot <- list(
  dygraphs::dygraph(temperature, group="temp_rain", main="temperature", width=400, height=200),
  dygraphs::dygraph(rainfall, group="temp_rain", main="rainfall", width=400, height=200)
)  # end list

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dyplot))



###############
### plot multiple dygraphs in the same RStudio window

# load packages
library(quantmod)
library(dygraphs)

# download time series into an environment
symbolv <- c("VTI", "EEM")
data_env <- new.env()
quantmod::getSymbols(symbolv, from="2017-01-01", env=data_env)

dygraphs::dygraph(data_env$EEM[, 1:4]) %>% dygraphs::dyCandlestick()

# Create a list of dygraphs objects in a loop
dyplot <- eapply(data_env, function(xtes) {
  dygraphs::dygraph(xtes[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(xtes)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
})  # end eapply

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dyplot))

## perform same plotting as above using pipes syntax
# Create a list of dygraphs objects in a loop
eapply(data_env, function(xtes) {
  dygraphs::dygraph(xtes[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(xtes)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
}) %>% # end eapply
  # render the dygraphs objects using htmltools
  htmltools::tagList() %>% htmltools::browsable()




###############
### dygraph plot with highlighting of specific points

library(xts)
library(dygraphs)
# Convert numeric time index of ldeaths into class 'Date' (approximately)
dates <- as.Date(365*(zoo::index(ldeaths)-1970))
# Convert time index from class 'Date' to 'POSIXct'
dates <- as.POSIXct.Date(dates)

# Convert ldeaths into xts time series
l_deaths <- xts::xts(as.numeric(ldeaths), order.by=dates)
# Calculate number of years
n_years <- NROW(dates)/12
# Calculate the January dates
jan_dates <- dates[1 + 12*(0:(n_years - 1))]
# or
# jan_dates <- dates[which(months(dates)=="January")]
# or
# jan_dates <- dates[grep("Jan", months(dates), ignore.case=TRUE)]
# Calculate the July dates
jul_dates <- dates[7 + 12*(0:(n_years - 1))]
# or
# jul_dates <- dates[which(months(dates)=="July")]

# Create dygraph object
dyplot <- dygraphs::dygraph(l_deaths, main="Dygraph of ldeaths with Annotations") %>%
  dyHighlight(highlightCircleSize=5)

# Add annotations for the January and July dates to dygraph object
for (i in 1:NROW(jan_dates)) {
  dyplot <- dygraphs::dyAnnotation(dyplot, x=jan_dates[i], text="Jan")
}  # end for
for (i in 1:NROW(jul_dates)) {
  dyplot <- dygraphs::dyAnnotation(dyplot, x=jul_dates[i], text="Jul")
}  # end for

# plot dygraph object
dyplot



###############
### exponentiation operator is a function:

'^'(3, 2)
"^"(3, 2)



###############
### Split random xts time series into daily list and rbind it back into the original xts

xtes <- xts(x=rnorm(100), order.by=(Sys.time()-3600*(1:100)))
# Split time series into daily list
listv <- xts::split.xts(xtes, "days")
# rbind the list back into a time series and compare with the original
all.equal(xtes, rutils::do_call(rbind, listv))



###############
### Code to check for duplicate dates

# Create xts with duplicate dates
x <- xts(rnorm(5), order.by=c(Sys.Date() + 1:4, Sys.Date() + 2))
diff(index(x))

rutils::diffit(index(x))

as.POSIXct(make.index.unique(.index(x)), origin="1970-01-01")


###############
### Example of an apply() error.
# When apply() parses a data frame with a Boolean column by rows,
# then it adds a leading space to TRUE.
# This may be because FALSE has 5 letters, while TRUE has only 4 letters.
# Below is example of how it works.

# Create data frame with two columns: Boolean column and character column
data_frame <- data.frame(
  new_instr=(rnorm(10)>0),
  instr_name=paste0("instrument", 1:10),
  stringsAsFactors=FALSE)
# perform apply() loop - requires gsub() to remove leading space in TRUE
apply(data_frame, MARGIN=1, function(instru_ment) {
  cat("processing instrument:", instru_ment["instr_name"], "\n")
  cat(instru_ment["new_instr"], "\n")
  if (as.logical(gsub(" ", "", instru_ment["new_instr"])))
    # below doesn't work
    # if (as.logical(instru_ment["new_instr"]))
    1
  else
    0
})  # end apply


# sapply() loop doesn't introduce leading space to TRUE
sapply(1:NROW(data_frame), function(i_ter) {
  instru_ment <- unlist(data_frame[i_ter, ])
  cat("processing instrument:", instru_ment["instr_name"], "\n")
  cat(instru_ment["new_instr"], "\n")
  # if (as.logical(gsub(" ", "", instru_ment["new_instr"])))
    if (as.logical(instru_ment["new_instr"]))
    1
  else
    0
})  # end sapply



###############
### Brownian bridge puzzle: given deck of 52 cards, every time you randomly choose a red card you're account increases by $1, but if you choose black card you're account decreases by -$1
# At any point you can choose to continue playing, or to stop and keep your net wins.
# The optimal strategy is to stop playing if the current net wins are greater than the expected value of wins from continuing to play.
# Calculate the expected value of the optimal strategy, assuming you start with zero in your account.

# stra_tegy <- matrix(nrow=4, ncol=4)
n_pos <- 26
stra_tegy <- outer(n_pos:0, n_pos:0, function(positive, negative)
  (negative - positive))
stra_tegy[, n_pos+1] <- 0
stra_tegy[n_pos+1, ] <- n_pos:0

prob_s <- outer(n_pos:0, n_pos:0, function(positive, negative)
  positive/(positive + negative))
prob_s[, n_pos+1] <- 0

for (i in n_pos:1) {
  for (j in n_pos:1)
    stra_tegy[i, j] <- max(stra_tegy[i, j],
                           prob_s[i, j]*stra_tegy[i+1, j] + (1-prob_s[i, j])*stra_tegy[i, j+1])
  for (j in n_pos:1)
    stra_tegy[j, i] <- max(stra_tegy[j, i],
                           prob_s[j, i]*stra_tegy[j+1, i] + (1-prob_s[j, i])*stra_tegy[j, i+1])
}  # end for

stra_tegy[1, 1]

stra_tegy <- function(cash, positive, negative) {
  # cat(paste("args=", cash, positive, negative, "\n"))
  pro_b <- positive/(positive + negative)
  if (positive==0)
    max(cash, 0)
  else if (negative==0)
    max(cash + positive, 0)
  else
    max(cash,
        pro_b*stra_tegy(cash+1, positive-1, negative) +
          (1-pro_b)*stra_tegy(cash-1, positive, negative-1))
}  #end stra_tegy

# stra_tegy(0, 26, 26)
stra_tegy(0, 3, 3)
stra_tegy(3, 0, 3)
stra_tegy(2, 1, 0)
stra_tegy(-3, 3, 0)
stra_tegy(0, 3, 3)

sapply(3:1, function(positive, negative)
  stra_tegy(negative-positive, positive, negative),
  negative=3:1)



###############
### Simulating several managers, with only one manager with skill,
# and try to determine how much data is needed to determine which manager has skill

library(rutils)
num_managers <- 11
# Daily probability as function of Sharpe ratio
sharpe_ratio <- 0.4
pro_b <- (sharpe_ratio/sqrt(250)+1)/2
# Adjust probability to account for multiple managers
p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
mean_s <- c(2*p1-1, rep(2*p2-1, num_managers-1))

# length of Brownian motion
n_row <- 5000

# Simulate Brownian motion
volat <- 0.01
set.seed(1121)  # reset random number generator
returns <- sapply(mean_s, rnorm, n=n_row, sd=volat)
returns <- apply(returns, 2, cumsum)
# apply(returns, 2, mean)
# plot.zoo(returns, plot.type="single")

# length of lookback window
look_back <- 100
# define endp with beginning stub
num_agg <- n_row %/% look_back
endp <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
numrows <- NROW(endp)
# startp are single-period lag of endp
startp <- endp[c(1, 1:.n_rows-1))] + 1
fix_points <- (startp > endp)
startp[fix_points] <- endp[fix_points]

# total returns aggregated over non-overlapping windows
agg_rets <- apply(returns, 2, function(x) (x[endp]-x[startp]))


# Switch to best manager with biggest total returns
be_st <- apply(agg_rets, 1, which.max)
be_st <- rutils::lagit(be_st)
be_st[1] <- 1
# be_st <- c(rep(1, NROW(endp)-NROW(be_st)), be_st)
pnls <- agg_rets[cbind(1:NROW(agg_rets), be_st)]
# pnls <- lapply(seq_along(endp), function(dates) {
#   returns[startp[dates]:endp[dates], be_st[dates]]
# })  # end lapply
# pnls <- rutils::do_call(c, pnls)
plot.zoo(cumsum(pnls))


## cum_pnl for multi-manager strategy
cum_pnl <- function(sharpe_ratio, returns=NULL, mean_s=NULL, num_managers, n_row, look_back, volat=0.01) {
  # Simulate Brownian motion
  if(is.null(returns)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- c(2*p1-1, rep(2*p2-1, num_managers-1))
    set.seed(1121)  # reset random number generator
    returns <- sapply(mean_s, rnorm, n=n_row, sd=volat)
    returns <- apply(returns, 2, cumsum)
  } else {
    num_managers <- NCOL(returns)
    n_row <- NROW(returns)
  }  # end if

  # define endp with beginning stub
  num_agg <- n_row %/% look_back
  endp <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
 .n_rows <- NROW(endp)
  # startp are single-period lag of endp
  startp <- endp[c(1, 1:.n_rows-1))] + 1
  fix_points <- (startp > endp)
  startp[fix_points] <- endp[fix_points]

  # total returns over non-overlapping windows
  agg_rets <- apply(returns, 2, function(x) (x[endp]-x[startp]))

  # Switch to best manager with biggest total returns
  be_st <- apply(agg_rets, 1, which.max)
  be_st <- rutils::lagit(be_st)
  be_st[1] <- 1
  be_st <- c(rep(1, NROW(endp)-NROW(be_st)), be_st)
  pnls <- lapply(seq_along(endp), function(dates) {
    returns[startp[dates]:endp[dates], be_st[dates]]
  })  # end lapply
  # return total expected pnl
  pnls <- rutils::do_call(c, pnls)
  sum(pnls)
}  # end cum_pnl

cum_pnl(returns=returns, look_back=100)

cum_pnl(sharpe_ratio=0.4, num_managers=11, look_back=100, n_row=5000)



## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, n_row=NULL, sharpe_ratio=NULL, returns=NULL, mean_s=NULL, num_managers=NULL, volat=0.01) {
  # Calculate drifts
  if(is.null(mean_s)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- volat*look_back*c(2*p1-1, rep(2*p2-1, num_managers-1))
  } else {
    num_managers <- NROW(mean_s)
  }  # end if
  # Simulate Brownian motion
  if(is.null(returns)) {
    # set.seed(1121)  # reset random number generator
    num_agg <- n_row %/% look_back
    returns <- sapply(mean_s, rnorm, n=num_agg, sd=sqrt(look_back)*volat)
  } else {
    num_managers <- NCOL(returns)
    n_row <- NROW(returns)
  }  # end if

  # Switch to best manager with biggest total returns
  be_st <- apply(returns, 1, which.max)
  be_st <- rutils::lagit(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnls <- returns[cbind(1:NROW(returns), be_st)]
  sum(returns[cbind(1:NROW(returns), be_st)])
}  # end cum_pnl



## cum_pnl for multi-manager strategy (simplest version)
cum_pnl <- function(look_back, n_row, sharpe_ratio=NULL, returns=NULL, mean_s=NULL, num_managers=NULL, volat=0.01) {
  # Calculate drifts
  if(is.null(mean_s)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- volat*look_back*c(2*p1-1, rep(2*p2-1, num_managers-1))
  } else {
    num_managers <- NROW(mean_s)
  }  # end if

  # Calculate probability of selecting the best manager
  pro_b <- integrate(function(x, ...)
    dnorm(x, mean=mean_s[1], ...)*pnorm(x, mean=mean_s[2], ...)^(num_managers-1),
            low=-3.0, up=3.0,
            sd=sqrt(look_back)*volat)$value
  # return total expected pnl
  num_agg <- n_row %/% look_back
  num_agg*(pro_b*mean_s[1] + (1-pro_b)*mean_s[2])
}  # end cum_pnl

cum_pnl(sharpe_ratio=0.4, num_managers=11, mean_s=mean_s, look_back=100, n_row=5000)
cum_pnl(sharpe_ratio=0.4, num_managers=11, look_back=100, n_row=5000)
cum_pnl(look_back=100, sharpe_ratio=0.4, num_managers=11, n_row=5000)

# Calculate average pnl
foo <- mean(sapply(1:10000, function(x)
  cum_pnl(mean_s=mean_s, look_back=100, n_row=5000)))

foo <- mean(sapply(1:10000, function(x)
  cum_pnl(look_back=100, sharpe_ratio=0.4, num_managers=11, n_row=500000)))

# perform loop over lookback windows
look_backs <- 100*(1:20)
foo <- sapply(look_backs, cum_pnl,
              sharpe_ratio=0.4, num_managers=11, n_row=50000)
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(cumsum(pnls), t="l")

# perform loop over number of managers
num_managers <- 2*(1:50)
foo <- sapply(num_managers, cum_pnl,
              returns=NULL, sharpe_ratio=0.4, look_back=100, n_row=50000, mean_s=NULL, volat=0.01)
foo <- cbind(num_managers, foo)
plot(foo, t="l")



###############
### Simulation of asset returns, with a time-dependent drift (skill) plus a random noise.

# define daily volatility: daily prices change by volat units
volat <- 0.01
n_row <- 50000
num_managers <- 3
# rate of drift (skill) change
ra_te <- 2*pi
# Daily probability as function of Sharpe ratio
sharpe_ratio <- 0.4
pro_b <- (sharpe_ratio/sqrt(250)+1)/2
# Adjust probability to account for two managers
pro_b <- 0.5 + (pro_b-0.5)/2
# define growth rate
mea_n <- volat*(2*pro_b-1)
# time-dependent drift (skill)
# drift <- 0.01*sin(ra_te*(1:n_row)/n_row)
# drift <- rutils::do_call(c, lapply(1:num_managers, function(x) (drift + 2*pi*x/num_managers)))
drift <- sapply(1:num_managers, function(x)
  mea_n*sin(ra_te*(1:n_row)/n_row + 2*pi*x/num_managers))

# Simulate multiple price paths

# returns <- xts(volat*rnorm(n_row) + drift - volat^2/2,
#                 order.by=seq.Date(Sys.Date()-n_row+1, Sys.Date(), by=1))
# chart_Series(x=returns, name="Multiple price paths")

set.seed(1121)  # reset random number generator
returns <- matrix(volat*rnorm(num_managers*n_row) - volat^2/2, nc=num_managers) + drift
# returns <- exp(matrixStats::colCumsums(returns))
# Create zoo time series
# returns <- xts(returns, order.by=seq.Date(Sys.Date()-NROW(returns)+1, Sys.Date(), by=1))
# plot zoo time series
colors <- colorRampPalette(c("red", "blue"))(NCOL(returns))
# colors <- colors[order(order(returns[NROW(returns), ]))]
par(mfrow=c(2, 2))
par(mar=c(3, 1, 1, 1), oma=c(1, 1, 1, 1))
plot.zoo(drift, main="time-dependent growth rates", lwd=3, xlab=NA, ylab=NA, plot.type="single", col=colors)
plot.zoo(returns, main="simulated returns", xlab=NA, ylab=NA, plot.type="single", col=colors)
plot.zoo(apply(returns, 2, cumsum),
         main="simulated prices", xlab=NA, ylab=NA, plot.type="single", col=colors)
# plot_theme <- chart_theme()
# plot_theme$col$line.col <- colors
# chart_Series(returns, theme=plot_theme, name="Multiple price paths")


## Calculate pnl over lookback window
# Calculate cumulative returns
pnls <- apply(returns, 2, cumsum)
# length of lookback window
look_back <- 100
# define endp with beginning stub
num_agg <- n_row %/% look_back
endp <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
numrows <- NROW(endp)
# startp are single-period lag of endp
startp <- endp[c(1, 1:.n_rows-1))] + 1
fix_points <- (startp > endp)
startp[fix_points] <- endp[fix_points]

# total returns aggregated over non-overlapping windows
agg_rets <- apply(pnls, 2, function(x) (x[endp]-x[startp]))

# Switch to best manager with biggest total returns
be_st <- apply(agg_rets, 1, which.max)
be_st <- rutils::lagit(be_st)
be_st[1] <- 1
# be_st <- c(rep(1, NROW(endp)-NROW(be_st)), be_st)
pnls <- agg_rets[cbind(1:NROW(agg_rets), be_st)]
plot.zoo(cumsum(pnls))


## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, returns) {
  n_row <- NROW(returns)
  # define endp with beginning stub
  num_agg <- n_row %/% look_back
  endp <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
 .n_rows <- NROW(endp)
  # startp are single-period lag of endp
  startp <- endp[c(1, 1:.n_rows-1))] + 1
  fix_points <- (startp > endp)
  startp[fix_points] <- endp[fix_points]
  # total returns aggregated over non-overlapping windows
  returns <- apply(returns, 2, function(x) (x[endp]-x[startp]))
  # Switch to best manager with biggest total returns
  be_st <- apply(returns, 1, which.max)
  be_st <- rutils::lagit(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnls <- returns[cbind(1:NROW(returns), be_st)]
  sum(returns[cbind(1:NROW(returns), be_st)])
}  # end cum_pnl

cum_pnl(look_back=100, returns=pnls)


## cum_pnl for trend-following multi-manager strategy (without endp)
cum_pnl <- function(look_back, returns, pnls) {
  # n_row <- NROW(returns)
  # total returns aggregated over overlapping windows
  agg_rets <- apply(pnls, 2, rutils::diffit, lag=look_back)
  # Switch to best manager with biggest total returns
  be_st <- apply(agg_rets, 1, which.max)
  be_st <- rutils::lagit(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnls <- returns[cbind(1:NROW(returns), be_st)]
  sum(returns[cbind(1:NROW(returns), be_st)])
}  # end cum_pnl

# Calculate cumulative returns
pnls <- apply(returns, 2, cumsum)
cum_pnl(look_back=100, returns=returns, pnls=pnls)


## perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
pnls <- sapply(look_backs, cum_pnl, se_lect=1, returns=returns, pnls=pnls)
pnls <- cbind(look_backs, pnls)
plot(pnls, t="l")
# plot(cumsum(pnls), t="l")


## pre-calculate row order indices for a vector of look_backs
# perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
orderstats <- lapply(look_backs, function(look_back) {
  # total returns aggregated over overlapping windows
  agg_rets <- apply(pnls, 2, rutils::diffit, lag=look_back)
  ordern <- t(apply(agg_rets, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  # end lapply
names(orderstats) <- look_backs


## cum_pnl for long-short multi-manager strategy (without endp)
cum_pnl <- function(select_best=NULL, select_worst=NULL, returns, ordern) {
  n_row <- NROW(returns)
  if(!is.null(select_best)) {
    n_col <- NCOL(returns)
    be_st <- ordern[, (n_col-select_best+1):n_col]
    be_st <- cbind(1:n_row, be_st)
  } else {
    be_st <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    wor_st <- ordern[, 1:select_worst]
    wor_st <- cbind(1:n_row, wor_st)
  } else {
    wor_st <- NULL
  }  # end if
  # return total expected pnl
  # pnls <- returns[be_st]-returns[wor_st]
  sum(returns[be_st])/select_best-sum(returns[wor_st])/(if(is.null(select_worst)) 1)
}  # end cum_pnl

# Calculate pnl for long-short multi-manager strategy
cum_pnl(select_best=1, select_worst=1, returns=returns, ordern=orderstats[[5]])


## perform loop over lookback windows
pnls <- sapply(orderstats, cum_pnl, select_best=1, select_worst=1, returns=returns)
pnls <- cbind(look_backs, pnls)
plot(pnls, t="l")
# plot(cumsum(pnls), t="l")


num_managers <- 5
drift <- sapply(1:num_managers, function(x)
  mea_n*sin(ra_te*(1:n_row)/n_row + 2*pi*x/num_managers))
set.seed(1121)  # reset random number generator
returns <- matrix(volat*rnorm(num_managers*n_row) - volat^2/2, nc=num_managers) + drift
# Calculate cumulative returns
pnls <- apply(returns, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
look_backs <- 20*(1:50)
orderstats <- lapply(look_backs, function(look_back) {
  # total returns aggregated over overlapping windows
  agg_rets <- apply(pnls, 2, rutils::diffit, lag=look_back)
  ordern <- t(apply(agg_rets, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  # end lapply
names(orderstats) <- look_backs

## cum_pnl for long-short multi-manager strategy (without endp)
cum_pnl <- function(select_best=NULL, select_worst=NULL, returns, ordern) {
  n_row <- NROW(returns)
  if(!is.null(select_best)) {
    n_col <- NCOL(returns)
    be_st <- ordern[, (n_col-select_best+1):n_col]
    be_st <- cbind(1:n_row, be_st)
  } else {
    be_st <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    wor_st <- ordern[, 1:select_worst]
    wor_st <- cbind(1:n_row, wor_st)
  } else {
    wor_st <- NULL
  }  # end if
  # return total expected pnl
  # pnls <- returns[be_st]-returns[wor_st]
  sum(returns[be_st])-sum(returns[wor_st])
}  # end cum_pnl

# Calculate pnl for long-short multi-manager strategy
# cum_pnl(select_best=1, select_worst=1, returns=returns, ordern=orderstats[[5]])

# perform loop over lookback windows
pnls <- sapply(orderstats, cum_pnl, select_best=1, select_worst=NULL, returns=returns)
pnls <- cbind(look_backs, pnls)
# par(mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1))
# plot(pnls, t="l", main="Trend-following PnL, as function of lookback window")


## double the drift
set.seed(1121)  # reset random number generator
returns <- matrix(volat*rnorm(num_managers*n_row) - volat^2/2, nc=num_managers) + 2*drift
# Calculate cumulative returns
pnls <- apply(returns, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
orderstats2x <- lapply(look_backs, function(look_back) {
  # total returns aggregated over overlapping windows
  agg_rets <- apply(pnls, 2, rutils::diffit, lag=look_back)
  ordern <- t(apply(agg_rets, 1, order))
  ordern <- rutils::lagit(ordern)
  ordern[1, ] <- 1
  ordern
})  # end lapply
names(orderstats2x) <- look_backs

plot.zoo(cbind(pnls[, 2], pnls2x), main="Long-short Ensemble PnL, as function of lookback window",
         lwd=2, xaxt="n", xlab="lookback windows", ylab="PnL", plot.type="single", col=c("black", "red"))
# Add x-axis
axis(1, seq_along(look_backs), look_backs)
# Add legend
legend(x="top", legend=paste0("SR=", c(0.4, 0.8)),
       inset=0.0, cex=0.8, bg="white",
       lwd=6, lty=c(1, 1), col=c("black", "red"))




## parallel version with loops - much slower and more complicated
# initialize compute cluster under Windows
library(parallel)
cluster <- makeCluster(num_cores-1)

foo <- sapply(look_backs, function(look_back) {
  # define endp with beginning stub
  num_agg <- n_row %/% look_back
  endp <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
 .n_rows <- NROW(endp)
  # startp are single-period lag of endp
  startp <- endp[c(1, 1:.n_rows-1))] + 1
  # redefine endp
  endp <- cbind(startp, endp)

  # perform parallel loop over returns
  clusterExport(cluster, varlist=c(.n_rows", "endp"))
  sharper <- parApply(cluster, MARGIN=2, returns, function(returns) {
    sapply(2.n_rows, function(dates) {
      xtes <- returns[endp[dates, 1]:endp[dates, 2]]
      # Calculate annualized Sharpe ratio of returns
      sum(xtes)/sd(xtes)
    })  # end sapply
  })  # end parApply

  # sharper <- rutils::do_call(cbind, sharper)
  sharper[which(is.na(sharper), arr.ind=TRUE)] <- 1

  # Calculate dispersion of SRs
  # c(by_strategy=mean(apply(sharper, 1, sd)),
  #   by_period=mean(apply(sharper, 2, sd)))
  # diff_sr <- apply(sharper, 2, rutils::diffit) / sharper
  # mean(abs(diff_sr))
  # c(by_strategy=mean(apply(sharper, 1, sd)),
  #   by_period=mean(apply(diff_sr, 1, sd)))
  cum_pnl(sharper, returns, endp)
})  # end sapply

foo <- t(foo)
dim(foo)
foo
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(foo[, 1]/foo[, 2], t="l")

## end perform loop over lookback windows




###############
### Portfolio optimization using quadratic solver

load("/Volumes/external/Develop/data/etf_data.RData")
ls(etfenv)
dim(etfenv$returns)
colnames(etfenv$returns)



## perform standard calibration over ohlc interval
optimd <- optim(par=rep(0.5, 2*NCOL(design)),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=rep(2, 2*NCOL(design)),
                lower=rep(-2, 2*NCOL(design)),
                design=design[dates],
                returns=returns_running[dates],
                lambdav=lambdav)

betas <- optimd$par
names(betas) <- c(paste0(colnames(design), "_long"), paste0(colnames(design), "_short"))


## cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(betas, la_g=15, design=design, returns=returns_running, lambdav=0) {
  n_col <- NCOL(design)
  position_s <- rep.int(NA, NROW(design))
  position_s[1] <- 0
  # buy signal
  bu_y <- (design %*% betas[1:n_col] < -1)
  position_s[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lagit(bu_y, lag=la_g))
  # Sell signal
  position_s[se_ll] <- -1.0
  position_s[bu_y] <- 1.0
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s <- c(0, position_s[-NROW(position_s)])
  # pnls <- position_s*returns
  # betav <- (sum(pnls*returns) - sum(pnls)*sum(returns)) / (sum(pnls*pnls) - sum(pnls)^2 )
  # -(exp(sum(pnls) - betav*sum(returns)) - 1)
  # -(exp(sum(position_s*returns))-1) # / (sum(abs(rutils::diffit(position_s))) / 2/ 1e5) / abs(sum(position_s>0) - sum(position_s<0))
  -((exp(sum(position_s*returns))-1) - lambdav*sum(abs(betas)))
}  # end cum_pnl

cum_pnl(betas=betas, design=design[dates], returns=returns_running[dates])

# perform calibration over ohlc interval
optimd <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, NCOL(design)),
                           lower=rep(-2, NCOL(design)),
                           design=design[dates],
                           returns=returns_running[dates],
                           lambdav=lambdav,
                           control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))


betas <- optimd$optim$bestmem
names(betas) <- colnames(design)
# names(betas) <- colnames(design)
optimd$optim$bestval
cum_pnl(betas, design=design[dates])


bu_y <- (design %*% betas[1:n_col] < -1)

cum_pnl <- function(interval) {
  position_s <- rep.int(NA, NROW(design))
  position_s[1] <- 0
  position_s[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lagit(bu_y, lag=interval))
  position_s[se_ll] <- -1.0
  position_s[bu_y] <- 1.0
  position_s <- zoo::na.locf(position_s, na.rm=FALSE)
  position_s <- c(0, position_s[-NROW(position_s)])
  exp(sum((position_s*returns_running)))-1
}  # end cum_pnl

cum_pnl(200)
sapply(20*(1:30), cum_pnl)



###############
### Convert correlation matrix into distance object
dis_tance <- xts::.index(vol_spikes)
dis_tance <- abs(outer(X=dis_tance, Y=dis_tance, FUN="-"))
# dis_tance <- rutils::diffit(xts::.index(vol_spikes))
dis_tance <- as.dist(dis_tance)
# Perform hierarchical clustering analysis
cluster <- hclust(dis_tance)
plot(cluster, ann=FALSE, xlab="", ylab="")
title("clustering of vol_spikes", line=0.0)
foo <- cutree(cluster, h=2000)
# foo <- cutree(cluster, k=100)
NROW(vol_spikes)
NROW(foo)
NROW(unique(foo))
tail(foo)
tail(vol_spikes)
bar <- match(index(vol_spikes), index(variance))
tail(bar)



hc <- hclust(dist(USArrests))
plot(hc)
cutree(hc, k=5)
cutree(hc, h=50)

returns_future <- rutils::roll_sum(returns_running, look_back=5)
returns_future <- rutils::lagxts(returns_running, lag=-5)
colnames(returns_future) <- "returns_future"
foo <- lm(returns_future["2008"] ~ design["2008"] - 1)
summary(foo)


##

16*sd(rutils::etfenv$returns[, "VTI"])
sqrt(250)
250/5

# Summary: Create a functional which aggregates
# Asset returns over lookback and look-forward
# intervals.


# define functional

# 1. (20pts) Create a functional called roll_agg(),

# Should perform only a single


###############
###
# 4. (20pts) Create a scatterplot of returns and forward returns
# Create a scatterplot of alphas for "2008" and "2009",
# and add labels with ETF names,
# use columns of "alphas_capm" and functions plot() and text(),

dim(fwd_rets)
dim(pnls)

foo <- na.omit(merge(fwd_rets[, 5], pnls[, 5]))
colnames(foo) <- c("forward_returns", "past_returns")
foo <- as.data.frame(foo)
head(foo)
dim(foo)

x11()
# perform regression
reg_formula <- paste(colnames(foo), collapse=" ~ ")
mo_del <- lm(reg_formula, data=foo)
summary(mo_del)
# plot scatterplot using formula
plot(foo[, 2], foo[, 1], xlab="past returns", ylab="forward returns")
# plot(foo)
title(main="Simple Regression", line=-1)
# Add regression line
abline(mo_del, lwd=2, col="red")


# Select weights proportional to pnls
dim(pnls)
weights <- coredata(pnls[index(fwd_rets)])
weights <- weights/sqrt(rowSums(weights^2))

# bar <- matrixStats::rowMaxs(weights)
bar <- coredata(fwd_rets)
dim(bar)

# Select best and worst models in each period
bes_t <- apply(weights, 1, which.max)
wors_t <- apply(weights, 1, function(x) {which.min(x)})
bes_t <- apply(weights, 1, which.max)
wors_t <- apply(weights, 1, which.min)

back_test <- rowSums(weights*bar)
x11()
plot(cumsum(back_test), t="l")

# back_test <- t(weights) %*% bar
back_test <- rowSums(weights*bar)
back_test <- xts(back_test, order.by=index(fwd_rets))
x11()
chart_Series(x=cumsum(back_test), name="Back-test of EWMA strategies")

plot(cumsum(back_test), t="l")
NROW(back_test)


#########

# define lookback windows


# Create a functional for performing rolling
# aggregations over overlapping intervals.
# Apply the functional to roll the function simu_ewma()
# over overlapping 12-month intervals in the past.

# 1. (20pts) Create a functional called roll_agg(),
# which should accept four arguments:
#  xtes - an xts series containing one or more columns of data,
#  endp - integer vector of end points,
#  look_back - number of intervals in the lookback window,
#  FUN - name of of an aggregation function,
#  "..." - optional dots arguments to FUN.

# The functional roll_agg() should perform an lapply()
# loop over endp, subset the xtes series, and pass
# it to FUN, together with the dots "..." argument.
# roll_agg() should return an xts series, with each
# row equal to the vector returned by FUN.
# hint: You can adapt code from the slide:
# Performing Aggregations Over Overlapping Intervals.

roll_agg <- function(xtes, endp, look_back, FUN, ...) {
 .n_rows <- NROW(endp)
  # startp are multi-period lag of endp
  startp <-  endp[c(rep_len(1, look_back-1), 1:.n_rows-look_back+1))]
  # perform lapply() loop over length of endp
  agg_s <- lapply(2.n_rows,
                  function(indeks) {
                    FUN(xtes[startp[indeks]:endp[indeks]], ...)
                  })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=index(xtes[endp]))
  agg_s
}  # end roll_agg


# 2. (20pts) Create an aggregation function called
# agg_regate(), which calls the function simu_ewma()
# and calculates the Sharpe ratios of the EWMA strategy,
# for a given vector of lambdas.
# agg_regate() should accept three arguments:
#  ohlc - an OHLC series containing four columns of data,
#  lambdavs - integer vector of lambda parameters,
#  "..." - additional dots arguments to be passed to simu_ewma().
# hint: You can adapt code from the slide:
# Simulating Multiple EWMA Strategies

agg_regate <- function(ohlc, lambdavs, ...) {
  sapply(lambdavs, function(lambdav) {
    # Simulate EWMA strategy and calculate Sharpe ratio
    returns <- simu_ewma(ohlc=ohlc, lambdav=lambdav, ...)[, "returns"]
    sqrt(260)*sum(returns)/sd(returns)/NROW(returns)
  })  # end sapply
}  # end agg_regate

# Source the function simu_ewma() from the file
# ewma_model.R, using function source().
# Download the latest version from NYU Classes.

source("C:/Develop/R/scripts/ewma_model.R")

# Define ohlc series, the EWMA look_back, and lambdavs.

library(HighFreq)
ohlc <- rutils::etfenv$VTI["/2011"]
look_back <- 51
lambdavs <- seq(0.001, 0.01, 0.001)

# Call agg_regate() as follows:
agg_regate(ohlc, lambdavs, look_back=look_back)

# You should get the following output:
#  [1] 0.1220623 0.1620571 0.1887122 0.2399056 0.2308350 0.1594881 0.1702486 0.1539695 0.1136539
# [10] 0.1180002


# 3. (20pts) Apply the functional roll_agg() to roll
# the function simu_ewma() over overlapping 12-month
# intervals in the past.

# Define end points at the end of each month.
# Use function endpoints() from package xts.

endp <- xts::endpoints(ohlc, on="months")
numrows <- NROW(endp)

# Define number of monthly intervals per lookback interval:
look_back <- 12

# Note that there are two different windows in this simulation.
# The first window is the EWMA window, called look_back and equal
# to 51 by default.
# The second window is the lookback interval, called look_back.
# To avoid an error, the endp should be greater than
# the EWMA look_back, except for the first endp, which
# should be equal to zero.
# Adjust the endp so that they are greater than the
# EWMA look_back.

endp[(endp > 0) & (endp <= look_back)] <- look_back+1

# Run roll_agg() as follows:

sharper <- roll_agg(xtes=ohlc,
                          endp=endp,
                          look_back=look_back,
                          FUN=agg_regate,
                          lambdavs=lambdavs,
                          look_back=look_back)

# You should get the following output:
# > sharper[1:6, 1:5]
#                  [,1]       [,2]       [,3]       [,4]       [,5]
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-19 -1.7531927 -1.7531927 -1.7531927 -1.7531927 -1.7531927
# 2007-03-30 -2.2223479 -2.2223479 -2.2223479 -2.2223479 -2.2223479
# 2007-04-30 -0.9446608 -0.9446608 -0.9446608 -0.9446608 -0.9446608
# 2007-05-31  0.0550219  0.0550219  0.0550219  0.0550219  0.0550219
# 2007-06-29 -0.3290286 -0.3290286 -0.3290286 -0.3290286 -0.3290286


#########

bar <- "foo"
bar <- 10

bar <- "foo"
assign(bar, 10)



###

var1 <- sum(pc1*pc1)
# make returns orthogonal to pc1
dates <- index(returns)
returns <- apply(returns, MARGIN=2,
                  function(x) {x - sum(pc1*x)*pc1/var1})
# apply(returns, MARGIN=2, function(x) sum(pc1*x)) # verify orthogonality

###

x11()
foo <- seq(0, 2*pi, length.out=24)
plot(x=cos(foo), y=sin(foo), asp=1)
abline(a=0, b=-0.1, col="red")
abline(a=0, b=10, col="blue")


###

heatmap(sharper, col=colorRampPalette(c("blue", "red"))(22))

summary(microbenchmark(
  tee=-t(pnls) %*% pnls,
  sumv=-sum(pnls*pnls),
  times=10))[, c(1, 4, 5)]


###

w1 <- sqrt(0.5); w2 <- w1
foo <- matrix(c(w1, w2, -w2, w1), nc=2)
t(foo) %*% foo
# bar <- returns %*% t(solve(foo))
(t(bar) %*% bar) / NROW(bar)

covmat <- function(returns, an_gle=0) {
  w1 <- cos(an_gle)
  w2 <- sin(an_gle)
  matrixv <- matrix(c(w1, -w2, w2, w1), nc=2)
  compo_nents <- returns %*% t(matrixv)
  (t(compo_nents) %*% compo_nents) / NROW(compo_nents)
}  # end covmat

bar <- covmat(returns, an_gle=pi/4)
(t(returns) %*% returns) / NROW(returns)
(t(bar) %*% bar) / NROW(bar)

angle_s <- seq(0, pi/2, by=pi/24)
covmat <- sapply(angle_s, function(an_gle)
  covmat(returns, an_gle=an_gle)[1, 1])
plot(x=angle_s, y=covmat, t="l")

optimd <- optimize(
  f=function(an_gle)
    -covmat(returns, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- optimd$minimum
bar <- covmat(returns, an_gle=an_gle)
tan(an_gle)

w1 <- cos(an_gle)
w2 <- sin(an_gle)
matrixv <- matrix(c(w1, -w2, w2, w1), nc=2)
compo_nents <- returns %*% t(matrixv)
(t(compo_nents) %*% compo_nents) / NROW(compo_nents)

plot(x=compo_nents[, 1], y=compo_nents[, 2],
     xlim=c(-10, 10), ylim=c(-10, 10))

mo_del <- lm(reg_formula, data=returns)
# get regression coefficients
coef(summary(mo_del))

foo <- cbind(rnorm(1000, sd=0.2), rnorm(1000)) %*% t(matrixv)
(t(foo) %*% foo) / NROW(foo)
plot(x=foo[, 1], y=foo[, 2])
summary(lm(foo[, 1] ~ foo[, 2]))

optimd <- optimize(
  f=function(an_gle)
    -covmat(foo, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- optimd$minimum
tan(an_gle)

###

library(plotly)

df <- data.frame(Date=seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by="days"),
                 Value=sample(100:200, size=244, replace=T))

plot_ly(data=df, x=df$Date, y=df$Value, type="scatter", mode="lines") %>%
  add_trace(x=~df$Date, y=~df$Value, name="20yr Treasury rate") %>%
  layout(xaxis=list(range=c( as.numeric(max(df$Date)-30)*86400000,
                                 as.numeric(max(df$Date))*86400000   ),
                      rangeslider=list(type="date")  ))

###


