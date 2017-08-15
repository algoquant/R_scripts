####################################
### Scripts for forecasting of turning points
### using logistic regression with static betas over design matrix
####################################

library(HighFreq)
options(max.print=40)
oh_lc <- HighFreq::SPY["/2008"]
oh_lc <- HighFreq::SPY["2009-03"]
oh_lc <- HighFreq::SPY["2009-03-12"]
in_dex <- index(oh_lc)

# load design matrix called SPY_design containing columns of data aggregations
load("C:/Develop/data/SPY_design.RData")
head(SPY_design)

### calculate close to close percentage returns
returns_running <- 6.5*60^2*HighFreq::run_returns(x_ts=HighFreq::SPY)
re_turns <- returns_running[in_dex]
# calculate returns advanced in time
returns_advanced <- rutils::lag_xts(returns_running, lag=-1)
colnames(returns_advanced) <- "returns_advanced"


###############
### Define strategy which trades at turning points.
# Perform PCA of design matrix.
# Calculate signal as weighted average of principal components.
# Calculate total PnL and add penalty for over-trading.
# Optimize betas with LASSO penalty.

# Calculate sig_nal using average returns times variance plus hurst
# SPY_design <- SPY_design[, c("returns", "returns.roll", "variance", "hurst", "rets_var")]
sig_nal <- SPY_design[, "returns.roll"] * (SPY_design[, "variance"] + SPY_design[, "hurst"])
# apply rolling centering and scaling to the design matrix
sig_nal <- roll::roll_scale(data=sig_nal, width=6.5*60, min_obs=1)
sig_nal[1] <- 0
sig_nal <- zoo::na.locf(sig_nal)
colnames(sig_nal) <- "signal"
hist(sig_nal, breaks=30)
hist(sig_nal[abs(sig_nal)<2], breaks=30)

# trade out-of-sample
thresh_old <- 14.0
position_s <- rep.int(NA, NROW(SPY))
position_s[1] <- 0
position_s[sig_nal > thresh_old] <- -1
position_s[sig_nal < -thresh_old] <- 1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
pnl_s <- exp(cumsum(position_s*returns_running))
colnames(pnl_s) <- "SPY contrarian"
# average number of trades per day
sum(abs(rutils::diff_it(position_s))) / 2 / NROW(endpoints(SPY_design, on="days"))
# average holding period (minutes)
2*NROW(position_s) / sum(abs(rutils::diff_it(position_s)))
# average PnL per trade
last(pnl_s)/(sum(abs(rutils::diff_it(position_s))) / 2)

cum_pnl <- function(thresh_old) {
  position_s <- rep.int(NA, NROW(SPY))
  position_s[1] <- 0
  position_s[sig_nal > thresh_old] <- -1
  position_s[sig_nal < -thresh_old] <- 1
  position_s <- zoo::na.locf(position_s)
  position_s <- rutils::lag_it(position_s)
  exp(sum(position_s*returns_running))
}  # end cum_pnl

cum_pnl(thresh_old)

# calculate PnLs
sapply(seq(10.0, 30.0, by=2.0), cum_pnl)
sapply(seq(0.1, 1.0, by=0.1), cum_pnl)



###############
### identify turning points by calculating maxima and minima

# The function max_min() calculates a vector of dates 
# at which x_ts reaches its local maximum or minimum.
# The function max_min() is recursive: it finds the max or min,
# then divides the x_ts in two halves, and calls max_min() on them.
# max_min <- function(x_ts, n_rows=100, FUN="which.max") {
#   n_data <- NROW(x_ts)
#   if (n_data < n_rows)
#     return(NULL)
#   # if (FUN=="which.max")
#   #   fac_tor <- 2*sd(x_ts)
#   # else if (FUN=="which.min")
#   #   fac_tor <- -2*sd(x_ts)
#   # else
#   #   stop("FUN must be equal to either which.max or which.min")
#   # fac_tor <- ifelse(FUN=="which.max", 4*sd(x_ts), -4*sd(x_ts))
#   # adjust x_ts so that its last value is equal to its first value
#   x_ts <- x_ts - (seq_along(x_ts)-1) * (as.numeric(last(x_ts))-as.numeric(first(x_ts)))/(n_data-1)
#   # x_ts <- x_ts - 4*fac_tor*(seq_along(x_ts)-(n_data %/% 2))^2 / (n_data^2) + fac_tor
#   # find maximum or minimum
#   whi_ch <- match.fun(FUN)(x_ts)
#   if ((whi_ch==1) | (whi_ch==n_data))
#     return(NULL)
#   # calculate dates: first, last, and maximum or minimum
#   date_s <- index(rbind(x_ts[1], x_ts[n_data], x_ts[whi_ch]))
#   c(max_min(x_ts=x_ts[paste0(date_s[1], "/", date_s[2])], n_rows=n_rows, FUN=FUN), 
#     max_min(x_ts=x_ts[paste0(date_s[2], "/", date_s[3])], n_rows=n_rows, FUN=FUN), 
#     index(x_ts[whi_ch]))
# }  # end max_min


# The function max_min() calculates a vector of dates 
# at which x_ts reaches its maxima and minima.
max_min <- function(x_ts, n_rows=100) {
  n_data <- NROW(x_ts)
  if (n_data < n_rows)
    return(NULL)
  # adjust x_ts so that its last value is equal to its first value
  x_ts <- x_ts - (seq_along(x_ts)-1) * (as.numeric(last(x_ts))-as.numeric(first(x_ts)))/(n_data-1)
  # calculate dates: first, last, maximum, and minimum
  which_max <- which.max(x_ts)
  which_min <- which.min(x_ts)
  date_s <- index(rbind(x_ts[1], x_ts[n_data], x_ts[which_max], x_ts[which_min]))
  c(max_min(x_ts=x_ts[paste0(date_s[1], "/", date_s[2])], n_rows=n_rows),
    max_min(x_ts=x_ts[paste0(date_s[2], "/", date_s[3])], n_rows=n_rows),
    max_min(x_ts=x_ts[paste0(date_s[3], "/", date_s[4])], n_rows=n_rows),
    max=index(x_ts[which_max]), min=index(x_ts[which_min]))
}  # end max_min

# calculate the maximum and minimum prices of oh_lc.
se_ll <- max_min(oh_lc[, 4], n_rows=180)
bu_y <- se_ll[names(se_ll)=="min"]
se_ll <- se_ll[names(se_ll)=="max"]

se_ll <- sort(unique(se_ll))
se_ll <- as.POSIXct(se_ll[se_ll>0], origin="1970-01-01")
NROW(se_ll)

# bu_y <- max_min(oh_lc[, 4], n_rows=60, FUN="which.min")
bu_y <- sort(unique(bu_y))
bu_y <- as.POSIXct(bu_y[bu_y>0], origin="1970-01-01")
NROW(bu_y)

## plot optimal bu_y and se_ll
chart_Series(oh_lc[, 4])
abline(v=match(se_ll, in_dex), col="red", lwd=1)
abline(v=match(bu_y, in_dex), col="blue", lwd=1)


## trade optimal strategy in-sample
position_s <- rep.int(NA, NROW(oh_lc))
position_s[1] <- 0
position_s[match(bu_y, in_dex)] <- 1.0
position_s[match(se_ll, in_dex)] <- -1.0
position_s <- zoo::na.locf(position_s)
pnl_s <- exp(cumsum((position_s * returns_running[in_dex])))-1
colnames(pnl_s) <- "SPY Optimal"
last(pnl_s)
chart_Series(pnl_s)
# chart_Series(pnl_s[endpoints(pnl_s, on="days")])


### calculate average future returns and identify points with biggest future returns
returns_future <- rutils::roll_sum(returns_running, win_dow=5)
returns_future <- rutils::lag_xts(returns_running, lag=-5)
colnames(returns_future) <- "returns_future"
returns_future <- returns_future[returns_future < quantile(returns_future, 0.05), ]

# regress large future returns against SPY_design
mod_el <- lm(returns_future ~ SPY_design[index(returns_future), ] - 1)
summary(mod_el)
plot(y=as.numeric(returns_future), ylab="returns_future", 
     x=as.numeric(SPY_design[index(returns_future), "variance.roll"]), 
     xlab="variance.roll")

# identify points with biggest variance
var_running <- SPY_design[in_dex, "variance.roll"]
# var_running <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY)
var_running <- var_running[var_running > quantile(var_running, 0.9), ]
# regress future returns against SPY_design for points with large variance 
mod_el <- lm(returns_future[index(var_running), ] ~ SPY_design[index(var_running), ] - 1)
summary(mod_el)

plot(y=as.numeric(returns_future), ylab="returns_future", 
     x=as.numeric(SPY_design[index(returns_future), "variance.roll"]), 
     xlab="variance.roll")


### identify points with biggest future draw-downs and run-ups

fu_ture <- rutils::lag_xts(oh_lc[, 4], lag=-op_tim$optim$bestmem[1]) - oh_lc[, 4]
thresh_old <- op_tim$optim$bestmem[2]
bu_y <- which(fu_ture > thresh_old)
se_ll <- which(fu_ture < thresh_old)


### cum_pnl function
cum_pnl <- function(param_s, x_ts=oh_lc) {
  fu_ture <- rutils::lag_xts(x_ts[, 4], lag=-param_s[1]) - x_ts[, 4]
  bu_y <- which(fu_ture > param_s[2])
  se_ll <- which(fu_ture < param_s[2])
  position_s <- NA*x_ts[, 4]
  position_s[1] <- 0
  position_s[bu_y] <- 1
  position_s[se_ll] <- -1
  position_s <- zoo::na.locf(position_s)
  -exp(sum(position_s*returns_running[index(x_ts)]))
}  # end cum_pnl

-cum_pnl(param_s=c(30, 6))
-cum_pnl(param_s=op_tim$optim$bestmem)

# calculate PnLs
sapply(seq(10.0, 30.0, by=2.0), cum_pnl)
sapply(seq(0.1, 1.0, by=0.1), cum_pnl)

op_tim <- optim(par=c(60, 3),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=c(360, 11),
                lower=c(30, 1))
op_tim <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=c(360, 6),
                           lower=c(30, 1),
                           control=list(storepopfrom=1, trace=FALSE))
# optimal parameters and value
op_tim$par
op_tim$value
op_tim$optim$bestmem
-cum_pnl(op_tim$optim$bestmem)



### create buy / sell series for logit model
position_s <- xts(logical(NROW(oh_lc)), order.by=in_dex)
# se_ll <- index(returns_future[in_dex])
position_s[bu_y] <- TRUE
position_s[se_ll] <- TRUE
# position_s <- position_s + lag_xts(position_s) + lag_xts(position_s, lag=-1)
# position_s <- xts(as.logical(position_s), order.by=in_dex)
colnames(position_s) <- "positions"

# fit logistic regression into buy or sell series
de_sign <- cbind(position_s, SPY_design[index(position_s)])
# colnames(de_sign)[1] <- "positions"
col_names <- colnames(de_sign)
for_mula <- as.formula(paste(col_names[1],
                             paste0(paste(col_names[-1], collapse=" + "), " - 1"), 
                             sep=" ~ "))
log_it <- glm(for_mula, data=de_sign, family=binomial(link="logit"))
summary(log_it)
beta_s <- summary(log_it)$coefficients[, "Estimate"]

# list logit models
ls(pattern=glob2rx("log*"))
# load("C:/Develop/data/logit_models.RData")
# save logit models
# save(list=ls(pattern=glob2rx("log*")), file="C:/Develop/data/logit_models.RData")

# calculate in-sample confusion matrix
fore_casts <- predict(log_it, type="response")
hist(fore_casts)
# fore_casts <- 1 / (1 + exp(-SPY_design[in_dex] %*% log_it$coefficients))
thresh_old <- 0.5
# thresh_old <- threshold_s[7]
confu_sion <- table(position_s, (fore_casts < thresh_old))
dimnames(confu_sion) <- list(hypothesis=rownames(confu_sion),
                             forecast=colnames(confu_sion))
confu_sion
confu_sion <- confu_sion / rowSums(confu_sion)
c(typeI=confu_sion[2, 1], typeII=confu_sion[1, 2])

con_fuse(position_s, fore_casts, thresh_old=thresh_old)
# define vector of discrimination thresholds
threshold_s <- seq(0.45, 0.55, by=0.01)
# calculate error rates
error_rates <- sapply(threshold_s, con_fuse,
                      res_ponse=position_s,
                      fore_casts=fore_casts)  # end sapply
error_rates <- t(error_rates)
which.min(abs(error_rates[, 1]-error_rates[, 2]))
which.min(rowSums(error_rates))


# calculate in-sample confusion matrix using simple regression
# fore_casts <- SPY_design[in_dex] %*% betas_buy
# thresh_old <- 100
# confu_sion <- table(position_s, (fore_casts > thresh_old))
# dimnames(confu_sion) <- list(hypothesis=rownames(confu_sion),
#                              forecast=colnames(confu_sion))
# confu_sion



### trade out-of-sample
# new_data <- SPY_design["2010"]
position_s <- rep.int(NA, NROW(SPY))
position_s[1] <- 0
# buy_prob <- predict(logit_buy, newdata=SPY_design, type="response")
buy_prob <- 1 / (1 + exp(-SPY_design %*% logit_buy$coefficients))
# buy_prob <- as.numeric(SPY_design %*% logit_buy$coefficients)
position_s[buy_prob > thresh_old] <- 1
# sell_prob <- predict(logit_sell, newdata=SPY_design, type="response")
sell_prob <- 1 / (1 + exp(-SPY_design %*% logit_sell$coefficients))
position_s[sell_prob > thresh_old] <- -1
position_s <- zoo::na.locf(position_s)
position_s <- rutils::lag_it(position_s)
# re_turns <- 6.5*60^2*HighFreq::run_returns(x_ts=HighFreq::SPY[index(SPY_design)])
pnl_s <- exp(cumsum(position_s*returns_running)) - 1
colnames(pnl_s) <- "SPY logit"
# average number of trades per day
# position_s <- xts(position_s, order.by=index(SPY_design))
sum(abs(rutils::diff_xts(position_s))) / 2 / NROW(endpoints(position_s, on="days"))
# average holding period (minutes)
2*NROW(position_s) / sum(abs(rutils::diff_xts(position_s)))

cum_pnl <- function(thresh_old) {
  position_s <- rep.int(NA, NROW(SPY))
  position_s[1] <- 0
  position_s[buy_prob > thresh_old] <- 1
  position_s[sell_prob > thresh_old] <- -1
  position_s <- zoo::na.locf(position_s)
  position_s <- rutils::lag_it(position_s)
  exp(sum(position_s*returns_running))
}  # end cum_pnl

cum_pnl(thresh_old)

threshold_s <- seq(0.49, 0.52, by=0.002)
# calculate PnLs
sapply(threshold_s, cum_pnl)


# plot
chart_Series(oh_lc[, 4])
abline(v=match(se_ll, in_dex), col="red", lwd=1)
abline(v=match(bu_y, in_dex), col="blue", lwd=1)

ran_ge <- "2010-05-05/2010-05-07"
pnl_s <- exp(cumsum(position_s[ran_ge]*re_turns[ran_ge]))
colnames(pnl_s) <- "SPY logit"
bench_mark <- cbind(exp(cumsum(re_turns[ran_ge])), pnl_s)

bench_mark <- cbind(exp(cumsum(re_turns)), pnl_s)[endpoints(pnl_s, on="days")]
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(bench_mark, theme=plot_theme,
             name="Optimal SPY strategy")
legend("topleft", legend=colnames(bench_mark),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")
add_TA(position_s > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
       col="lightgrey", border="lightgrey")



###############
### identify turning points by maximizing cumulative PnL - very compute intensive

position_s <- rep.int(NA, NROW(oh_lc))
position_s[1] <- 0

pnl_s <- function(poin_t=NA, directio_n=NA, position_s=NA, re_turns=NA) {
  position_s[poin_t] <- directio_n
  position_s <- zoo::na.locf(position_s)
  exp(cumsum(position_s*re_turns))
}  # end pnl_s

foo <- pnl_s(NROW(oh_lc) %/% 4, directio_n=1, position_s=position_s, re_turns=re_turns)

# foo <- sapply(seq_along(position_s), pnl_s, directio_n=1, position_s=position_s, re_turns=re_turns)
which.max(foo)


### calculate drawdowns and use them as turning points
# doesn't work well because returns neighboring dates 
# just before the max drawdown.

library(PerformanceAnalytics)
PerformanceAnalytics::table.Drawdowns(dailyReturn(oh_lc[, 4]))
chart_Series(oh_lc["2009-09", ])
foo <- diff_xts(log(oh_lc["2009-09", 4]))
PerformanceAnalytics::table.Drawdowns(foo)

# The function draw_downs() calculates a vector of dates 
# at which the troughs of drawdowns are reached.
# x_ts is a time series of prices.

draw_downs <- function(x_ts, min_draw=-15.0) {
  if (NROW(x_ts) < 5)
    return(0)
  # adjust x_ts so that its last value is equal to its first value
  x_ts <- x_ts - (seq_along(x_ts)-1) * (as.numeric(last(x_ts))-as.numeric(first(x_ts)))/(NROW(x_ts)-1)
  draw_down <- x_ts - cummax(x_ts)
  if (min(draw_down) < min_draw) {
    in_dex <- index(x_ts)
    date_trough <- in_dex[which.min(draw_down)]
    c(draw_downs(x_ts=draw_down[in_dex<date_trough], min_draw=min_draw),
      as.numeric(date_trough),
      draw_downs(x_ts=draw_down[in_dex>date_trough], min_draw=min_draw))
  }
  else 0  # stop recursion if drawdown is too small
}  # end draw_downs

# calculate the drawdowns of oh_lc.
x_ts <- xts(NROW(oh_lc), order.by=in_dex)
foo <- draw_downs(oh_lc[, 4], min_draw=-1.0)
foo <- unique(foo)
foo <- as.POSIXct(foo[foo>0], origin="1970-01-01")
NROW(foo)

chart_Series(oh_lc[, 4])
abline(v=match(foo, in_dex), col="blue", lwd=1)



### calculate draw-ups to identify turning points

foo <- sapply(c(1, 10*(1:(NROW(oh_lc) %/% 1000))), function(lag) {
  core_data <- coredata(oh_lc[, 4])
  core_data <- lag_it(core_data, lag=-lag) - core_data
  which.max(core_data)
})
foo <- sort(unique(foo))

chart_Series(oh_lc[, 4])
abline(v=foo, col="blue", lwd=2)

bar <- last(foo)
chart_Series(oh_lc[(bar-100):(bar+100), ])
abline(v=which(index(oh_lc[(bar-100):(bar+100), ])==index(oh_lc[bar])), col="blue", lwd=2)


### perform regression of returns_advanced versus de_sign in first quarter of data
# negative coefficients indicate that this is a contrarian strategy
ran_ge <- 1:(NROW(de_sign) %/% 4)
for_mula <- returns_advanced[ran_ge, ] ~ de_sign[ran_ge, ] - 1
mod_el <- lm(for_mula)
beta_s <- summary(mod_el)$coefficients[, "t value"]
names(beta_s)[-1] <- sapply(names(beta_s)[-1], function(x) strsplit(x, split="]")[[1]][2])


### calculate the forecast sig_nal by applying beta_s out-of-sample to the remaining data
sig_nal <- de_sign %*% beta_s
colnames(sig_nal) <- "signal"
# lag sig_nal by one period
sig_nal <- rutils::lag_xts(sig_nal)
# calculate average of sig_nal over past, to improve forecasts
sig_nal <- rutils::roll_sum(sig_nal, win_dow=3) / 3


### calculate hit rates by signal quantiles
hit_s <- sign(sig_nal * returns_running)[-ran_ge, ]
hit_s <- cbind(sig_nal[-ran_ge, ], hit_s)
colnames(hit_s) <- c("signal", "hits")
x11()
# histogram of sig_nal
histo_gram <- hist(hit_s[, "signal"], breaks=100, xlim=c(-10, 15))
histo_gram$breaks
# quantiles of sig_nal
quantile_s <- quantile(hit_s[, "signal"], probs=seq(0.05, 0.95, 0.1))
# extreme quantiles have higher hit rates
sapply(seq_along(quantile_s)[-1], function(x)
  NROW(hit_s[(hit_s[, "signal"]>=quantile_s[x-1]) & (hit_s[, "signal"]<quantile_s[x]), "hits"]))
sapply(seq_along(quantile_s)[-1], function(x)
  sum(hit_s[(hit_s[, "signal"]>=quantile_s[x-1]) & (hit_s[, "signal"]<quantile_s[x]), "hits"]))


### backtest: invest proportional to sig_nal - but it trades too much
# calculate out-of-sample pnl_s
# pnl_s <- exp(cumsum(sign(sig_nal) * returns_running[-ran_ge, ]))
pnl_s <- (sig_nal * returns_running)[-ran_ge, ]
# scale pnl_s to SPY volatility
pnl_s <- pnl_s*sd(diff_xts(log(HighFreq::SPY[index(pnl_s), 4])))/sd(pnl_s)
pnl_s <- exp(cumsum(pnl_s))


### backtest the contrarian strategy with threshold:
# sell when sig_nal exceeds threshold, hold, and buy when sig_nal is below -threshold
thresh_old <- 1.0
position_s <- rep.int(NA, NROW(sig_nal))
position_s[sig_nal > thresh_old] <- -1.0
position_s[sig_nal < (-thresh_old)] <- 1.0
position_s[ran_ge] <- 0.0
position_s <- zoo::na.locf(position_s)
# lag the position_s ?
# position_s <- c(0, position_s[-NROW(position_s)])
# position_s <- xts(position_s, order.by=index(returns_running))
# position_s <- cbind(sig_nal, position_s)
# colnames(position_s)[2] <- "positions"
# calculate cumulative PnL
pnl_s <- exp(cumsum((position_s * returns_running)[-ran_ge, ]))
last(pnl_s)
# pnl_s <- cumsum(position_s[, 2]*re_turns)
# chart_Series(pnl_s)


### backtest: sell when sig_nal exceeds threshold, hold, and buy when sig_nal is below -threshold
pnl_s <- back_test(de_sign=SPY_design[-ran_ge, ], beta_s=beta_s, re_turns=returns_running[-ran_ge, ], bid_offer=0.0, lag=1)

pnl_s <- back_test(de_sign=SPY_design[-ran_ge, ], beta_s=beta_s, thresh_old=thresh_old, re_turns=returns_running[-ran_ge, ], bid_offer=0.0, lag=4)

# loop over threshold_s
threshold_s <- seq(0.1, 3.0, by=0.1)
foo <- sapply(threshold_s, function(thresh_old)
  last(back_test(de_sign=SPY_design[-ran_ge, ], beta_s=beta_s, thresh_old=thresh_old, re_turns=returns_running[-ran_ge, ], bid_offer=0.0, lag=4)))
names(foo) <- threshold_s


### plotting

x11()
# bench_mark <- HighFreq::SPY[index(pnl_s), 4]
# bench_mark <- bench_mark / as.numeric(bench_mark[1, ])
# bench_mark <- cbind(pnl_s, bench_mark)[endpoints(bench_mark, on="days"), ]
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


### back_test function
back_test <- function(de_sign=NULL, beta_s=NULL, thresh_old=NULL, re_turns=NULL, lag=1, bid_offer=0.0) {
  sig_nal <- de_sign %*% beta_s
  sig_nal <- rutils::lag_it(sig_nal, lag=lag)
  if (lag > 1)
    sig_nal <- rutils::roll_sum(sig_nal, win_dow=lag) / lag
  # calculate returns
  if (is.null(thresh_old)) {
    # calculate returns proportional to sig_nal and scale them to SPY volatility
    position_s <- sig_nal
    pnl_s <- (position_s * re_turns)
    fac_tor <- sd(diff_xts(log(HighFreq::SPY[index(pnl_s), 4])))/sd(pnl_s)
    pnl_s <- fac_tor*pnl_s
  }
  else {
    # calculate returns of contrarian strategy with threshold
    position_s <- rep.int(NA, NROW(sig_nal))
    position_s[1] <- 0.0
    position_s[sig_nal > thresh_old] <- 1.0
    position_s[sig_nal < (-thresh_old)] <- -1.0
    position_s <- zoo::na.locf(position_s)
    pnl_s <- position_s * re_turns
    fac_tor <- 1
  }
  # calculate transaction costs
  cost_s <- fac_tor*bid_offer*abs(rutils::diff_it(position_s))
  # calculate cumulative PnL
  pnl_s <- exp(cumsum(pnl_s - cost_s))
  colnames(pnl_s) <- "backtest"
  pnl_s
}  # end back_test


### older


colnames(pnl_s) <- "backtest"
bench_mark <- SPY[index(pnl_s), 4]
bench_mark <- bench_mark / as.numeric(bench_mark[1, ])
bench_mark <- cbind(pnl_s, bench_mark)[endpoints(bench_mark, on="days"), ]
chart_Series(bench_mark, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
legend("topleft", legend=colnames(bench_mark),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

ran_ge <- "2010-05-05/2010-05-07"
foo <- bench_mark[ran_ge]
# foo <- sapply(foo, function(x) {x - as.numeric(x[1])})
foo[, 1] <- foo[, 1] - as.numeric(foo[1, 1])
foo[, 2] <- foo[, 2] - as.numeric(foo[1, 2])
chart_Series(foo, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
add_TA(position_s[ran_ge] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(position_s[ran_ge] < 0, on=-1,
       col="lightgrey", border="lightgrey")
legend("topleft", legend=colnames(foo),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")

bar <- xts(sig_nal, order.by=index(returns_running))[ran_ge]
bench_mark <- SPY[index(bar), 4]
bench_mark <- bench_mark - as.numeric(bench_mark[1, ])
bar <- cbind(bench_mark, bar)
chart_Series(bar, theme=plot_theme,
             name="Backtest of PCR strategy for SPY")
legend("topleft", legend=colnames(bar),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")
add_TA(bar[ran_ge], lwd=2, on=1, col='blue')


### below are scratch scripts

# simple contrarian strategy works better than PCR?
foo <- -sign(rutils::lag_xts(returns_running))
sum(foo*returns_running)
chart_Series(x=cumsum(foo*returns_running), name="strategy cumulative returns")

