####################################
### Scripts for forecasting of turning points
### using logistic regression with static betas over design matrix
####################################

library(HighFreq)
options(max.print=40)
ohlc <- HighFreq::SPY["/2008"]
ohlc <- HighFreq::SPY["2009-03"]
ohlc <- HighFreq::SPY["2009-03-12"]
dates <- index(ohlc)

# load design matrix called SPY_design containing columns of data aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)

## calculate close to close percentage returns
returns_running <- 6.5*60^2*HighFreq::run_returns(xtes=HighFreq::SPY)
returns <- returns_running[dates]
# calculate returns advanced in time
returns_adv <- rutils::lagit(returns_running, lag=-1)
colnames(returns_adv) <- "returns_adv"


###############
### Define strategy which trades at turning points.
# Perform PCA of design matrix.
# Calculate signal as weighted average of principal components.
# Calculate total PnL and add penalty for over-trading.
# Optimize betas with LASSO penalty.

# Calculate score using average returns times variance plus hurst
# SPY_design <- SPY_design[, c("returns", "returns.roll", "variance", "hurst", "rets_var")]
score <- SPY_design[, "returns.roll"] * (SPY_design[, "variance"] + SPY_design[, "hurst"])
# apply rolling centering and scaling to the design matrix
score <- roll::roll_scale(data=score, width=6.5*60, min_obs=1)
score[1] <- 0
score <- zoo::na.locf(score, na.rm=FALSE)
colnames(score) <- "signal"
hist(score, breaks=30)
hist(score[abs(score)<2], breaks=30)

# trade out-of-sample
threshold <- 14.0
posit <- rep.int(NA, NROW(SPY))
posit[1] <- 0
posit[score > threshold] <- -1
posit[score < -threshold] <- 1
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit)
pnls <- exp(cumsum(posit*returns_running))
colnames(pnls) <- "SPY contrarian"
# average number of trades per day
sum(abs(rutils::diffit(posit))) / 2 / NROW(endpoints(SPY_design, on="days"))
# average holding period (minutes)
2*NROW(posit) / sum(abs(rutils::diffit(posit)))
# average PnL per trade
last(pnls)/(sum(abs(rutils::diffit(posit))) / 2)

cum_pnl <- function(threshold) {
  posit <- rep.int(NA, NROW(SPY))
  posit[1] <- 0
  posit[score > threshold] <- -1
  posit[score < -threshold] <- 1
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- rutils::lagit(posit)
  exp(sum(posit*returns_running))
}  # end cum_pnl

cum_pnl(threshold)

# calculate PnLs
sapply(seq(10.0, 30.0, by=2.0), cum_pnl)
sapply(seq(0.1, 1.0, by=0.1), cum_pnl)



###############
### identify turning points by calculating maxima and minima

# The function max_min() calculates a vector of dates 
# at which xtes reaches its local maximum or minimum.
# The function max_min() is recursive: it finds the max or min,
# then divides the xtes in two halves, and calls max_min() on them.
# max_min <- function(xtes, nrows=100, FUN="which.max") {
#   ndata <- NROW(xtes)
#   if (ndata < nrows)
#     return(NULL)
#   # if (FUN=="which.max")
#   #   factorv <- 2*sd(xtes)
#   # else if (FUN=="which.min")
#   #   factorv <- -2*sd(xtes)
#   # else
#   #   stop("FUN must be equal to either which.max or which.min")
#   # factorv <- ifelse(FUN=="which.max", 4*sd(xtes), -4*sd(xtes))
#   # adjust xtes so that its last value is equal to its first value
#   xtes <- xtes - (seq_along(xtes)-1) * (as.numeric(last(xtes))-as.numeric(first(xtes)))/(ndata-1)
#   # xtes <- xtes - 4*factorv*(seq_along(xtes)-(ndata %/% 2))^2 / (ndata^2) + factorv
#   # find maximum or minimum
#   whichv <- match.fun(FUN)(xtes)
#   if ((whichv==1) | (whichv==ndata))
#     return(NULL)
#   # calculate dates: first, last, and maximum or minimum
#   dates <- index(rbind(xtes[1], xtes[ndata], xtes[whichv]))
#   c(max_min(xtes=xtes[paste0(dates[1], "/", dates[2])], nrows=nrows, FUN=FUN), 
#     max_min(xtes=xtes[paste0(dates[2], "/", dates[3])], nrows=nrows, FUN=FUN), 
#     index(xtes[whichv]))
# }  # end max_min


# The function max_min() calculates a vector of dates 
# at which xtes reaches its maxima and minima.
max_min <- function(xtes, nrows=100) {
  ndata <- NROW(xtes)
  if (ndata < nrows)
    return(NULL)
  # adjust xtes so that its last value is equal to its first value
  xtes <- xtes - (seq_along(xtes)-1) * (as.numeric(last(xtes))-as.numeric(first(xtes)))/(ndata-1)
  # calculate dates: first, last, maximum, and minimum
  which_max <- which.max(xtes)
  which_min <- which.min(xtes)
  dates <- index(rbind(xtes[1], xtes[ndata], xtes[which_max], xtes[which_min]))
  c(max_min(xtes=xtes[paste0(dates[1], "/", dates[2])], nrows=nrows),
    max_min(xtes=xtes[paste0(dates[2], "/", dates[3])], nrows=nrows),
    max_min(xtes=xtes[paste0(dates[3], "/", dates[4])], nrows=nrows),
    max=index(xtes[which_max]), min=index(xtes[which_min]))
}  # end max_min

# calculate the maximum and minimum prices of ohlc.
se_ll <- max_min(ohlc[, 4], nrows=180)
bu_y <- se_ll[names(se_ll)=="min"]
se_ll <- se_ll[names(se_ll)=="max"]

se_ll <- sort(unique(se_ll))
se_ll <- as.POSIXct(se_ll[se_ll>0], origin="1970-01-01")
NROW(se_ll)

# bu_y <- max_min(ohlc[, 4], nrows=60, FUN="which.min")
bu_y <- sort(unique(bu_y))
bu_y <- as.POSIXct(bu_y[bu_y>0], origin="1970-01-01")
NROW(bu_y)

## plot optimal bu_y and se_ll
chart_Series(ohlc[, 4])
abline(v=match(se_ll, dates), col="red", lwd=1)
abline(v=match(bu_y, dates), col="blue", lwd=1)


## trade optimal strategy in-sample
posit <- rep.int(NA, NROW(ohlc))
posit[1] <- 0
posit[match(bu_y, dates)] <- 1.0
posit[match(se_ll, dates)] <- -1.0
posit <- zoo::na.locf(posit, na.rm=FALSE)
pnls <- exp(cumsum((posit * returns_running[dates])))-1
colnames(pnls) <- "SPY Optimal"
last(pnls)
chart_Series(pnls)
# chart_Series(pnls[endpoints(pnls, on="days")])


## calculate average future returns and identify points with biggest future returns
returns_adv <- rutils::roll_sum(returns_running, look_back=5)
returns_adv <- rutils::lagit(returns_running, lag=-5)
colnames(returns_adv) <- "returns_adv"
returns_adv <- returns_adv[returns_adv < quantile(returns_adv, 0.05), ]

# regress large future returns against SPY_design
model <- lm(returns_adv ~ SPY_design[index(returns_adv), ] - 1)
summary(model)
plot(y=as.numeric(returns_adv), ylab="returns_adv", 
     x=as.numeric(SPY_design[index(returns_adv), "variance.roll"]), 
     xlab="variance.roll")

# identify points with biggest variance
var_running <- SPY_design[dates, "variance.roll"]
# var_running <- 6.5*60^3*HighFreq::run_variance(ohlc=HighFreq::SPY)
var_running <- var_running[var_running > quantile(var_running, 0.9), ]
# regress future returns against SPY_design for points with large variance 
model <- lm(returns_adv[index(var_running), ] ~ SPY_design[index(var_running), ] - 1)
summary(model)

plot(y=as.numeric(returns_adv), ylab="returns_adv", 
     x=as.numeric(SPY_design[index(returns_adv), "variance.roll"]), 
     xlab="variance.roll")


## identify points with biggest future draw-downs and run-ups

future <- rutils::lagit(ohlc[, 4], lag=-optimd$optim$bestmem[1]) - ohlc[, 4]
threshold <- optimd$optim$bestmem[2]
bu_y <- which(future > threshold)
se_ll <- which(future < threshold)


## cum_pnl function
cum_pnl <- function(param_s, xtes=ohlc) {
  future <- rutils::lagit(xtes[, 4], lag=-param_s[1]) - xtes[, 4]
  bu_y <- which(future > param_s[2])
  se_ll <- which(future < param_s[2])
  posit <- NA*xtes[, 4]
  posit[1] <- 0
  posit[bu_y] <- 1
  posit[se_ll] <- -1
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  -exp(sum(posit*returns_running[index(xtes)]))
}  # end cum_pnl

-cum_pnl(param_s=c(30, 6))
-cum_pnl(param_s=optimd$optim$bestmem)

# calculate PnLs
sapply(seq(10.0, 30.0, by=2.0), cum_pnl)
sapply(seq(0.1, 1.0, by=0.1), cum_pnl)

optimd <- optim(par=c(60, 3),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=c(360, 11),
                lower=c(30, 1))
optimd <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=c(360, 6),
                           lower=c(30, 1),
                           control=list(storepopfrom=1, trace=FALSE))
# optimal parameters and value
optimd$par
optimd$value
optimd$optim$bestmem
-cum_pnl(optimd$optim$bestmem)



## create buy / sell series for logit model
posit <- xts(logical(NROW(ohlc)), order.by=dates)
# se_ll <- index(returns_adv[dates])
posit[bu_y] <- TRUE
posit[se_ll] <- TRUE
# posit <- posit + rutils::lagit(posit) + rutils::lagit(posit, lag=-1)
# posit <- xts(as.logical(posit), order.by=dates)
colnames(posit) <- "positions"

# fit logistic regression into buy or sell series
design <- cbind(posit, SPY_design[index(posit)])
# colnames(design)[1] <- "positions"
colnamev <- colnames(design)
formulav <- as.formula(paste(colnamev[1],
                             paste0(paste(colnamev[-1], collapse=" + "), " - 1"), 
                             sep=" ~ "))
glmod <- glm(formulav, data=design, family=binomial(link="logit"))
summary(glmod)
betas <- summary(glmod)$coefficients[, "Estimate"]

# list logit models
ls(pattern=glob2rx("log*"))
# load("C:/Develop/data/logit_models.RData")
# save logit models
# save(list=ls(pattern=glob2rx("log*")), file="C:/Develop/data/logit_models.RData")

# calculate in-sample confusion matrix
forecastvs <- predict(glmod, type="response")
hist(forecastvs)
# forecastvs <- 1 / (1 + exp(-SPY_design[dates] %*% glmod$coefficients))
threshold <- 0.5
# threshold <- threshold_s[7]
confu_sion <- table(posit, (forecastvs < threshold))
dimnames(confu_sion) <- list(hypothesis=rownames(confu_sion),
                             forecast=colnames(confu_sion))
confu_sion
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])

con_fuse(posit, forecastvs, threshold=threshold)
# define vector of discrimination thresholds
threshold_s <- seq(0.45, 0.55, by=0.01)
# calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
                      response=posit,
                      forecastvs=forecastvs)  # end sapply
error_rates <- t(error_rates)
which.min(abs(error_rates[, 1]-error_rates[, 2]))
which.min(rowSums(error_rates))


# calculate in-sample confusion matrix using simple regression
# forecastvs <- SPY_design[dates] %*% betas_buy
# threshold <- 100
# confu_sion <- table(posit, (forecastvs > threshold))
# dimnames(confu_sion) <- list(hypothesis=rownames(confu_sion),
#                              forecast=colnames(confu_sion))
# confu_sion



## trade out-of-sample
# new_data <- SPY_design["2010"]
posit <- rep.int(NA, NROW(SPY))
posit[1] <- 0
# buy_prob <- predict(logit_buy, newdata=SPY_design, type="response")
buy_prob <- 1 / (1 + exp(-SPY_design %*% logit_buy$coefficients))
# buy_prob <- as.numeric(SPY_design %*% logit_buy$coefficients)
posit[buy_prob > threshold] <- 1
# sell_prob <- predict(logit_sell, newdata=SPY_design, type="response")
sell_prob <- 1 / (1 + exp(-SPY_design %*% logit_sell$coefficients))
posit[sell_prob > threshold] <- -1
posit <- zoo::na.locf(posit, na.rm=FALSE)
posit <- rutils::lagit(posit)
# returns <- 6.5*60^2*HighFreq::run_returns(xtes=HighFreq::SPY[index(SPY_design)])
pnls <- exp(cumsum(posit*returns_running)) - 1
colnames(pnls) <- "SPY logit"
# average number of trades per day
# posit <- xts(posit, order.by=index(SPY_design))
sum(abs(rutils::diffits(posit))) / 2 / NROW(endpoints(posit, on="days"))
# average holding period (minutes)
2*NROW(posit) / sum(abs(rutils::diffits(posit)))

cum_pnl <- function(threshold) {
  posit <- rep.int(NA, NROW(SPY))
  posit[1] <- 0
  posit[buy_prob > threshold] <- 1
  posit[sell_prob > threshold] <- -1
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  posit <- rutils::lagit(posit)
  exp(sum(posit*returns_running))
}  # end cum_pnl

cum_pnl(threshold)

threshold_s <- seq(0.49, 0.52, by=0.002)
# calculate PnLs
sapply(threshold_s, cum_pnl)


# plot
chart_Series(ohlc[, 4])
abline(v=match(se_ll, dates), col="red", lwd=1)
abline(v=match(bu_y, dates), col="blue", lwd=1)

rangev <- "2010-05-05/2010-05-07"
pnls <- exp(cumsum(posit[rangev]*returns[rangev]))
colnames(pnls) <- "SPY logit"
bench_mark <- cbind(exp(cumsum(returns[rangev])), pnls)

bench_mark <- cbind(exp(cumsum(returns)), pnls)[endpoints(pnls, on="days")]
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(bench_mark, theme=plot_theme,
             name="Optimal SPY strategy")
legend("topleft", legend=colnames(bench_mark),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")
add_TA(posit > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(posit < 0, on=-1,
       col="lightgrey", border="lightgrey")



###############
### identify turning points by maximizing cumulative PnL - very compute intensive

posit <- rep.int(NA, NROW(ohlc))
posit[1] <- 0

pnls <- function(poin_t=NA, directio_n=NA, posit=NA, returns=NA) {
  posit[poin_t] <- directio_n
  posit <- zoo::na.locf(posit, na.rm=FALSE)
  exp(cumsum(posit*returns))
}  # end pnls

foo <- pnls(NROW(ohlc) %/% 4, directio_n=1, posit=posit, returns=returns)

# foo <- sapply(seq_along(posit), pnls, directio_n=1, posit=posit, returns=returns)
which.max(foo)


## calculate drawdowns and use them as turning points
# doesn't work well because returns neighboring dates 
# just before the max drawdown.

library(PerformanceAnalytics)
PerformanceAnalytics::table.Drawdowns(dailyReturn(ohlc[, 4]))
chart_Series(ohlc["2009-09", ])
foo <- diffits(log(ohlc["2009-09", 4]))
PerformanceAnalytics::table.Drawdowns(foo)

# The function draw_downs() calculates a vector of dates 
# at which the troughs of drawdowns are reached.
# xtes is a time series of prices.

draw_downs <- function(xtes, min_draw=-15.0) {
  if (NROW(xtes) < 5)
    return(0)
  # adjust xtes so that its last value is equal to its first value
  xtes <- xtes - (seq_along(xtes)-1) * (as.numeric(last(xtes))-as.numeric(first(xtes)))/(NROW(xtes)-1)
  draw_down <- xtes - cummax(xtes)
  if (min(draw_down) < min_draw) {
    dates <- index(xtes)
    date_trough <- dates[which.min(draw_down)]
    c(draw_downs(xtes=draw_down[dates<date_trough], min_draw=min_draw),
      as.numeric(date_trough),
      draw_downs(xtes=draw_down[dates>date_trough], min_draw=min_draw))
  }
  else 0  # stop recursion if drawdown is too small
}  # end draw_downs

# calculate the drawdowns of ohlc.
xtes <- xts(NROW(ohlc), order.by=dates)
foo <- draw_downs(ohlc[, 4], min_draw=-1.0)
foo <- unique(foo)
foo <- as.POSIXct(foo[foo>0], origin="1970-01-01")
NROW(foo)

chart_Series(ohlc[, 4])
abline(v=match(foo, dates), col="blue", lwd=1)



## calculate draw-ups to identify turning points

foo <- sapply(c(1, 10*(1:(NROW(ohlc) %/% 1000))), function(lag) {
  core_data <- coredata(ohlc[, 4])
  core_data <- lagit(core_data, lag=-lag) - core_data
  which.max(core_data)
})
foo <- sort(unique(foo))

chart_Series(ohlc[, 4])
abline(v=foo, col="blue", lwd=2)

bar <- last(foo)
chart_Series(ohlc[(bar-100):(bar+100), ])
abline(v=which(index(ohlc[(bar-100):(bar+100), ])==index(ohlc[bar])), col="blue", lwd=2)


## perform regression of returns_adv versus design in first quarter of data
# negative coefficients indicate that this is a contrarian strategy
rangev <- 1:(NROW(design) %/% 4)
formulav <- returns_adv[rangev, ] ~ design[rangev, ] - 1
model <- lm(formulav)
betas <- summary(model)$coefficients[, "t value"]
names(betas)[-1] <- sapply(names(betas)[-1], function(x) strsplit(x, split="]")[[1]][2])


## calculate the forecast score by applying betas out-of-sample to the remaining data
score <- design %*% betas
colnames(score) <- "signal"
# lag score by one period
score <- rutils::lagit(score)
# calculate average of score over past, to improve forecasts
score <- rutils::roll_sum(score, look_back=3) / 3


## calculate hit rates by signal quantiles
hit_s <- sign(score * returns_running)[-rangev, ]
hit_s <- cbind(score[-rangev, ], hit_s)
colnames(hit_s) <- c("signal", "hits")
x11()
# histogram of score
histo_gram <- hist(hit_s[, "signal"], breaks=100, xlim=c(-10, 15))
histo_gram$breaks
# quantiles of score
quantiles <- quantile(hit_s[, "signal"], probs=seq(0.05, 0.95, 0.1))
# extreme quantiles have higher hit rates
sapply(seq_along(quantiles)[-1], function(x)
  NROW(hit_s[(hit_s[, "signal"]>=quantiles[x-1]) & (hit_s[, "signal"]<quantiles[x]), "hits"]))
sapply(seq_along(quantiles)[-1], function(x)
  sum(hit_s[(hit_s[, "signal"]>=quantiles[x-1]) & (hit_s[, "signal"]<quantiles[x]), "hits"]))


## backtest: invest proportional to score - but it trades too much
# calculate out-of-sample pnls
# pnls <- exp(cumsum(sign(score) * returns_running[-rangev, ]))
pnls <- (score * returns_running)[-rangev, ]
# scale pnls to SPY volatility
pnls <- pnls*sd(diffits(log(HighFreq::SPY[index(pnls), 4])))/sd(pnls)
pnls <- exp(cumsum(pnls))


## backtest the contrarian strategy with threshold:
# sell when score exceeds threshold, hold, and buy when score is below -threshold
threshold <- 1.0
posit <- rep.int(NA, NROW(score))
posit[score > threshold] <- -1.0
posit[score < (-threshold)] <- 1.0
posit[rangev] <- 0.0
posit <- zoo::na.locf(posit, na.rm=FALSE)
# lag the posit ?
# posit <- c(0, posit[-NROW(posit)])
# posit <- xts(posit, order.by=index(returns_running))
# posit <- cbind(score, posit)
# colnames(posit)[2] <- "positions"
# calculate cumulative PnL
pnls <- exp(cumsum((posit * returns_running)[-rangev, ]))
last(pnls)
# pnls <- cumsum(posit[, 2]*returns)
# chart_Series(pnls)


## backtest: sell when score exceeds threshold, hold, and buy when score is below -threshold
pnls <- back_test(design=SPY_design[-rangev, ], betas=betas, returns=returns_running[-rangev, ], bid_offer=0.0, lag=1)

pnls <- back_test(design=SPY_design[-rangev, ], betas=betas, threshold=threshold, returns=returns_running[-rangev, ], bid_offer=0.0, lag=4)

# loop over threshold_s
threshold_s <- seq(0.1, 3.0, by=0.1)
foo <- sapply(threshold_s, function(threshold)
  last(back_test(design=SPY_design[-rangev, ], betas=betas, threshold=threshold, returns=returns_running[-rangev, ], bid_offer=0.0, lag=4)))
names(foo) <- threshold_s


## plotting

x11()
# bench_mark <- HighFreq::SPY[index(pnls), 4]
# bench_mark <- bench_mark / as.numeric(bench_mark[1, ])
# bench_mark <- cbind(pnls, bench_mark)[endpoints(bench_mark, on="days"), ]
tail(bench_mark)
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(bench_mark, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
legend("topleft", legend=colnames(bench_mark),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

# plot using dygraphs
library(dygraphs)
dygraph(bench_mark, main="Backtest of PCR strategy for SPY") %>%
  dyOptions(colors=c("orange", "blue")) %>%
  dyRangeSelector()

# plot using plotly
library(plotly)
data.frame(dates=index(bench_mark), coredata(bench_mark)) %>%
  plot_ly(x=~dates, y=~backtest, type="scatter", mode="lines + markers", name="backtest") %>%
  add_trace(x=~dates, y=~SPY.Close, name="SPY") %>%
  layout(title="Backtest of PCR strategy for SPY",
         xaxis=list(title="Time"),
         yaxis=list(title="PnL"),
         legend=list(x=0.1, y=0.9))


## back_test function
back_test <- function(design=NULL, betas=NULL, threshold=NULL, returns=NULL, lag=1, bid_offer=0.0) {
  score <- design %*% betas
  score <- rutils::lagit(score, lag=lag)
  if (lag > 1)
    score <- rutils::roll_sum(score, look_back=lag) / lag
  # calculate returns
  if (is.null(threshold)) {
    # calculate returns proportional to score and scale them to SPY volatility
    posit <- score
    pnls <- (posit * returns)
    factorv <- sd(diffits(log(HighFreq::SPY[index(pnls), 4])))/sd(pnls)
    pnls <- factorv*pnls
  }
  else {
    # calculate returns of contrarian strategy with threshold
    posit <- rep.int(NA, NROW(score))
    posit[1] <- 0.0
    posit[score > threshold] <- 1.0
    posit[score < (-threshold)] <- -1.0
    posit <- zoo::na.locf(posit, na.rm=FALSE)
    pnls <- posit * returns
    factorv <- 1
  }
  # calculate transaction costs
  costs <- factorv*bid_offer*abs(rutils::diffit(posit))
  # calculate cumulative PnL
  pnls <- exp(cumsum(pnls - costs))
  colnames(pnls) <- "backtest"
  pnls
}  # end back_test


## older


colnames(pnls) <- "backtest"
bench_mark <- SPY[index(pnls), 4]
bench_mark <- bench_mark / as.numeric(bench_mark[1, ])
bench_mark <- cbind(pnls, bench_mark)[endpoints(bench_mark, on="days"), ]
chart_Series(bench_mark, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
legend("topleft", legend=colnames(bench_mark),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

rangev <- "2010-05-05/2010-05-07"
foo <- bench_mark[rangev]
# foo <- sapply(foo, function(x) {x - as.numeric(x[1])})
foo[, 1] <- foo[, 1] - as.numeric(foo[1, 1])
foo[, 2] <- foo[, 2] - as.numeric(foo[1, 2])
chart_Series(foo, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
add_TA(posit[rangev] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(posit[rangev] < 0, on=-1,
       col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(foo),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

bar <- xts(score, order.by=index(returns_running))[rangev]
bench_mark <- SPY[index(bar), 4]
bench_mark <- bench_mark - as.numeric(bench_mark[1, ])
bar <- cbind(bench_mark, bar)
chart_Series(bar, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
legend("topleft", legend=colnames(bar),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")
add_TA(bar[rangev], lwd=2, on=1, col='blue')


## below are scratch scripts

# simple contrarian strategy works better than PCR?
foo <- -sign(rutils::lagit(returns_running))
sum(foo*returns_running)
chart_Series(x=cumsum(foo*returns_running), name="strategy cumulative returns")

