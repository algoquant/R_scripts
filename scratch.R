###############
### Load and save OHLC bar data

library(HighFreq)
# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/lm_arma.cpp")

# Source the strategy functions
source("C:/Develop/R/scripts/calc_strategy.R")


# load OHLC data
# oh_lc <- HighFreq::SPY
# oh_lc <- HighFreq::SPY["2010-10/2010-11"]
# oh_lc <- rutils::env_etf$VTI
# load recent ES1 futures data
# load(file="C:/Develop/data/ES1.RData")
# or
# oh_lc <- read.zoo(file="C:/Develop/data/bar_data/ES1.csv", header=TRUE, sep=",",
#                   drop=FALSE, format="%Y-%m-%d %H:%M",
#                   FUN=as.POSIXct, tz="America/New_York")
# oh_lc <- as.xts(oh_lc)
# oh_lc <- oh_lc["T09:00:00/T16:30:00"]
# save(oh_lc, file="C:/Develop/data/ES1.RData")
# load recent combined futures data
load(file="C:/Develop/data/combined.RData")
com_bo <- oh_lc

# set up data for signal
sym_bol <- "UX1"
oh_lc <- oh_lc[, paste(sym_bol, c("Open", "High", "Low", "Close"), sep=".")]

log_ohlc <- log(oh_lc)
# sum(is.na(oh_lc))
# sapply(oh_lc, class)
# tail(oh_lc, 11)
clo_se <- Cl(log_ohlc)
close_num <- as.numeric(clo_se)
re_turns <- rutils::diff_it(clo_se)
# regression with clo_se prices as response requires clo_se to be a vector
# clo_se <- as.numeric(clo_se)
# plot dygraph
dygraphs::dygraph(xts::to.hourly(clo_se), main=sym_bol)
# random data
# re_turns <- xts(rnorm(NROW(oh_lc), sd=0.01), index(oh_lc))
# clo_se <- as.numeric(cumsum(re_turns))

# Define OHLC data
op_en <- Op(log_ohlc)
hi_gh <- Hi(log_ohlc)
high_num <- as.numeric(hi_gh)
lo_w <- Lo(log_ohlc)
low_num <- as.numeric(lo_w)
close_high <- (close_num == high_num)
close_high_count <- roll_count(close_high)
close_low <- (close_num == low_num)
close_low_count <- roll_count(close_low)
open_num <- as.numeric(op_en)
open_high <- (open_num == high_num)
open_high_count <- roll_count(open_high)
open_low <- (open_num == low_num)
open_low_count <- roll_count(open_low)


# set up data for trading
sym_bol <- "ES1"
re_turns <- rutils::diff_it(log(com_bo[, paste(sym_bol, "Close", sep=".")]))



# vari_ance <- (hi_gh - lo_w)^2
look_back <- 11
vari_ance <- HighFreq::roll_variance(oh_lc=oh_lc, look_back=look_back, sca_le=FALSE)
colnames(vari_ance) <- "variance"
vol_at <- sqrt(vari_ance)
colnames(vol_at) <- "volat"
vol_ume <- Vo(oh_lc)
colnames(vol_ume) <- "volume"

# Define current and future returns
# re_turns <- rutils::diff_it(clo_se)
# trailing average returns
re_turns <- rutils::diff_it(clo_se, lagg=look_back)/sqrt(look_back)
colnames(re_turns) <- "returns"
# returns_adv <- rutils::lag_it(re_turns, lagg=-1)
# or
# returns_adv <- 0.5*(returns_adv + rutils::lag_it(returns_adv, lagg=-1))
returns_adv <- rutils::lag_it(rutils::diff_it(clo_se, lagg=look_back), lagg=-look_back)/sqrt(look_back)
# returns_adv <- rutils::lag_it(HighFreq::roll_sum(re_turns, look_back=look_back), lagg=-look_back)/look_back
# returns_adv <- xts(returns_adv, index(oh_lc))
colnames(returns_adv) <- "returns_adv"
# scale returns using sigmoid
# returns_adv <- plogis(returns_adv, scale=-quantile(returns_adv, 0.01))
# returns_adv <- (returns_adv - median(returns_adv))
# colnames(returns_adv) <- "returns_adv"


# begin old stuff

###############
### strategy using rolling zscores over OHLC technical indicators
# with regression and dimensionality reduction


# colnames(re_turns) <- "returns"
# create design matrix
# date_s <- xts::.index(oh_lc)
date_s <- 1:NROW(oh_lc)
de_sign <- matrix(date_s, nc=1)

mod_el <- HighFreq::calc_lm(res_ponse=as.numeric(returns_adv), de_sign=cbind(re_turns, vari_ance))
mod_el$coefficients

# old: calculate sig_nal as the residual of the regression of the time series of clo_se prices
look_back <- 11
sig_nal <- HighFreq::roll_zscores(res_ponse=close_num, de_sign=de_sign, look_back=look_back)
colnames(sig_nal) <- "sig_nal"
sig_nal[1:look_back] <- 0
# or
sig_nal <- calc_signal(look_back, close_num, de_sign)
hist(sig_nal)
# hist(sig_nal, xlim=c(-10, 10))

# old: perform parallel loop over look_backs
look_backs <- 15:35
library(parallel)
num_cores <- detectCores()
clus_ter <- makeCluster(num_cores-1)
# clusterExport(clus_ter, varlist=c("clo_se", "de_sign"))
signal_s <- parLapply(clus_ter, X=look_backs, fun=calc_signal, clo_se=close_num, de_sign=de_sign)


# trade entry and exit levels
en_ter <- 1.0
ex_it <- 0.5
pnl_s <- calc_revert(signal_s[[1]], re_turns, en_ter, ex_it)
quantmod::chart_Series(pnl_s[endpoints(pnl_s, on="days")])

# old uses calc_revert(): run strategies over a vector of trade entry levels
run_strategies <- function(sig_nal, re_turns, en_ters, ex_it, return_series=TRUE) {
  # sapply(en_ters, calc_revert, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it)
  pnl_s <- lapply(en_ters, calc_revert, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it)
  pnl_s <- rutils::do_call(cbind, pnl_s)
  if (return_series) {
    pnl_s <- rowSums(pnl_s)
  } else {
    pnl_s <- as.numeric(pnl_s[NROW(pnl_s)])
  }  # end if
  return(pnl_s)
}  # end run_strategies

# define vector of trade entry levels
en_ters <- (5:30)/10
# pnl_s <- run_strategies(signal_s[[1]], re_turns, en_ters, ex_it=ex_it)
# pnl_s <- xts(pnl_s, index(oh_lc))
# quantmod::chart_Series(pnl_s)
clusterExport(clus_ter, varlist=c("calc_revert"))


## old uses run_strategies(): simulate ensemble of strategies and return heatmap of final pnls
pnl_s <- parLapply(clus_ter, X=signal_s, fun=run_strategies, re_turns=re_turns, en_ters=en_ters, ex_it=ex_it, return_series=FALSE)
pnl_s <- rutils::do_call(rbind, pnl_s)
colnames(pnl_s) <- paste0("en_ter=", en_ters)
rownames(pnl_s) <- paste0("look_back=", look_backs)
heatmap(pnl_s, Colv=NA, Rowv=NA, col=c("red", "blue"))
rgl::persp3d(z=pnl_s, col="green")
plot(colSums(pnl_s), t="l", xlab="")


## simulate ensemble of strategies and return the average pnls
pnl_s <- parLapply(clus_ter, X=signal_s[1:10], fun=run_strategies, re_turns=re_turns, en_ters=en_ters, ex_it=ex_it, return_series=TRUE)
pnl_s <- rutils::do_call(cbind, pnl_s)
pnl_s <- xts(pnl_s, index(oh_lc))
colnames(pnl_s) <- paste0("look_back=", look_backs[1:10])
# plot matrix using plot.zoo()
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(pnl_s))
plot.zoo(pnl_s[endpoints(pnl_s, on="days")], main="pnls", lwd=2, 
         plot.type="single", xlab="", ylab="pnls", col=col_ors)
# add legend
legend("bottomright", legend=colnames(pnl_s), col=col_ors, lty=1, lwd=4, inset=0.05, cex=0.8)
# plot single dygraph
pnl_s <- rowSums(pnl_s)
pnl_s <- xts(pnl_s, index(oh_lc))
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clo_se, pnl_s)[endpoints(pnl_s, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


stopCluster(clus_ter)  # stop R processes over cluster under Windows

# count the number of consecutive TRUE elements, and reset to zero after every FALSE element
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
bar <- hist(foo, breaks=0:15, xlim=c(0, 4))
bar <- hist(close_high_count, breaks=0:15, xlim=c(0, 4))
bar <- hist(close_low_count, breaks=0:15, xlim=c(0, 4))
bar$counts
all.equal(roll_countr(close_high), drop(roll_count(close_high)), check.attributes=FALSE)
library(microbenchmark)
summary(microbenchmark(
  pure_r=roll_countr(close_high),
  rcpp=roll_count(close_high),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# wipp
# contrarian strategy
po_sit <- rep(NA_integer_, NROW(oh_lc))
po_sit[1] <- 0
po_sit[close_high] <- (-1)
po_sit[close_low] <- 1
po_sit <- zoo::na.locf(po_sit)
po_sit <- rutils::lag_it(po_sit, lagg=1)

# contrarian strategy using roll_count()
po_sit <- rep(NA_integer_, NROW(oh_lc))
po_sit[1] <- 0
po_sit[close_high_count>2] <- (-1)
po_sit[close_low_count>2] <- 1
po_sit <- zoo::na.locf(po_sit)
po_sit <- rutils::lag_it(po_sit, lagg=1)

# contrarian strategy using roll_cum()
po_sit <- rep(0, NROW(oh_lc))
po_sit[close_high] <- (-1)
po_sit[close_low] <- 1
po_sit <- roll_cum(po_sit, 2)
po_sit <- rutils::lag_it(po_sit, lagg=1)

# number of trades
sum(abs(rutils::diff_it(po_sit))) / NROW(po_sit)

# calculate strategy pnl_s
pnl_s <- cumsum(po_sit*re_turns)
colnames(pnl_s) <- "strategy"

# plot dygraphs
dygraphs::dygraph(pnl_s[xts::endpoints(pnl_s, on="days")], main="ES1 strategy")
# plot dygraphs with two "y" axes
da_ta <- cbind(clo_se, po_sit)
colnames(da_ta) <- c(symbol_asset, "position")
dygraphs::dygraph(da_ta, main=paste(symbol_asset, "Strategy Using OHLC Technical Indicators")) %>%
  dyAxis("y", label=symbol_asset, independentTicks=TRUE) %>%
  dyAxis("y2", label="position", independentTicks=TRUE) %>%
  dySeries("position", axis="y2", col=c("blue", "red"))


x11()
# date_s <- index(oh_lc)
po_sit <- xts::xts(po_sit, index(oh_lc))
# rang_e <- "2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"
rang_e <- "2018-02-05/2018-02-07"
dygraphs::dygraph(pnl_s[rang_e], main="ES1 strategy")
# calculate integer index of date range
# rang_e <- index(oh_lc["2018-02-06 10:00:00 EST/2018-02-06 11:00:00 EST"])
# rang_e <- index(oh_lc[rang_e])
# rang_e <- (which(date_s==min(rang_e)):which(date_s==max(rang_e)))
# plot prices
chart_Series(x=Cl(oh_lc[rang_e]))
# add background shading of areas
add_TA(po_sit[rang_e] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(po_sit[rang_e] < 0, on=-1,
       col="lightgrey", border="lightgrey")

# calculate integer index of date range
date_s <- xts::.index(oh_lc)
rang_e <- xts::.index(oh_lc[rang_e])
rang_e <- (which(date_s==min(rang_e)):which(date_s==max(rang_e)))
# add vertical lines
# close_high_count <- xts::xts(close_high_count, index(oh_lc))
# close_low_count <- xts::xts(close_low_count, index(oh_lc))
close_high_count <- drop(close_high_count)
close_low_count <- drop(close_low_count)
abline(v=which(close_high_count[rang_e]>0), col='red')
abline(v=which(close_low_count[rang_e]>0), col='blue')



# calculate the rolling maximum and minimum over a vector of data
roll_maxmin <- function(vec_tor, look_back) {
  len_gth <- NROW(vec_tor)
  max_min <- matrix(numeric(2*len_gth), nc=2)
  
  # startup periods
  max_min[1, 1] <- vec_tor[1];
  max_min[1, 2] <- vec_tor[1];
  for (it in 2:(look_back-1)) {
    sub_vec <- vec_tor[1:it];
    max_min[it, 1] <- max(sub_vec);
    max_min[it, 2] <- min(sub_vec);
  }  # end for
  
  # remaining periods
  for (it in look_back:len_gth) {
    sub_vec <- vec_tor[(it-look_back+1):it];
    max_min[it, 1] <- max(sub_vec);
    max_min[it, 2] <- min(sub_vec);
  }  # end for
  
  return(max_min)
}  # end roll_maxmin

max_min <- roll_maxmin(as.numeric(clo_se["2014-05"]), look_back)
bar <- TTR::runMax(x=clo_se["2014-05"], n=look_back)
all.equal(foo[-(1:look_back), 1], as.numeric(bar)[-(1:look_back)])
foo <- cbind(foo, bar)
bar <- TTR::runMin(x=clo_se["2014-05"], n=look_back)
all.equal(foo[-(1:look_back), 2], as.numeric(bar)[-(1:look_back)])
foo <- cbind(foo, bar)
tail(foo, 22)
head(foo, 22)
max_min <- xts(max_min, index(clo_se["2014-05"]))
dygraphs::dygraph(max_min[, 1]-clo_se["2014-05"])

library(microbenchmark)
summary(microbenchmark(
  tt_r=TTR::runMax(x=clo_se["2014-05"], n=look_back),
  rcpp=roll_maxmin(as.numeric(clo_se["2014-05"]), look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

# end of old stuff


###############
### strategy using static betas over OHLC technical indicators
# with regression and dimensionality reduction

# Load OHLC futures data
load(file="C:/Develop/data/combined.RData")

# Define OHLC technical indicators
# residuals of the regression of the time series of clo_se prices
date_s <- xts::.index(oh_lc)
# foo <- unique(date_s)
de_sign <- matrix(date_s, nc=1)
# foo <- MASS::ginv(de_sign)
look_back <- 11
z_scores <- HighFreq::roll_zscores(res_ponse=clo_se, 
                                   de_sign=de_sign, 
                                   look_back=look_back)
colnames(z_scores) <- "z_scores"
z_scores[1:3] <- 0
close_open <- (clo_se-op_en)
colnames(close_open) <- "close_open"
close_high <- (clo_se-hi_gh)
colnames(close_high) <- "close_high"
close_low <- (clo_se-lo_w)
colnames(close_low) <- "close_low"
# sk_ew <- ((hi_gh+lo_w) - (op_en+clo_se))
sk_ew <- ((hi_gh+lo_w) - (op_en+clo_se))
colnames(sk_ew) <- "sk_ew"
# moment_um <- ((clo_se-op_en) - (hi_gh-lo_w))
moment_um <- ((clo_se-op_en) - (hi_gh-lo_w)) + 1.0
colnames(moment_um) <- "moment_um"

# close_high <- (hi_gh - rutils::lag_it(hi_gh))
# close_low <- (lo_w - rutils::lag_it(lo_w))
# Select only independent indicators

indicator_s <- cbind(re_turns, vol_at, sk_ew)

# indicator_s <- cbind(re_turns, close_open, close_high, close_low, vol_at, sk_ew, moment_um, z_scores)
# colnames(indicator_s) <- c("close_high", "op_en_hi_gh", "clo_se_hi_gh")
# indicator_s <- cbind(re_turns, vol_at, sk_ew, moment_um, indicator_s)
# indicator_s <- cbind(op_en-hi_gh, op_en-lo_w, op_en-clo_se, clo_se-hi_gh, clo_se-lo_w, hi_gh-lo_w)
# colnames(indicator_s) <- c("op_en_hi_gh", "op_en_lo_w", "op_en_clo_se", "clo_se_hi_gh", "clo_se_lo_w", "hi_gh_lo_w")
# indicator_s <- cbind(sk_ew, moment_um, indicator_s)
# Select only independent indicators
# indicator_s <- cbind(op_en-hi_gh, clo_se-hi_gh)
# colnames(indicator_s) <- c("op_en_hi_gh", "clo_se_hi_gh")
# indicator_s <- cbind(re_turns, sk_ew, indicator_s)
col_names <- colnames(indicator_s)

# scale indicator_s using roll_scale()
look_back <- 11
indicator_s <- roll::roll_scale(data=indicator_s, width=look_back, min_obs=1)
indicator_s[1, ] <- 0
round(cor(indicator_s), 3)
indicator_s <- cbind(indicator_s, z_scores)
indicator_s[1:3, ] <- 0
col_names <- colnames(indicator_s)


# scale indicator_s using sigmoid
indicator_s <- lapply(1:NCOL(indicator_s), function(col_umn) {
  x <- plogis(indicator_s[, col_umn], scale=-quantile(indicator_s[, col_umn], 0.01))
  (x - median(x))
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)

round(cor(indicator_s), 3)

# Calculate PCA of technical indicators
pc_a <- prcomp(indicator_s)
pc_a$sdev
pc_a$rotation


## create design matrix for SPY or ES1
# de_sign <- cbind(re_turns, close_high, close_low, rutils::diff_it(vari_ance), rutils::diff_it(vol_ume))
# de_sign from pc_a
# rolling average
indicator_s <- lapply(1:NCOL(indicator_s), function(col_umn) {
  HighFreq::roll_sum(indicator_s[, col_umn], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- col_names
# de_sign <- as.data.frame(cbind(returns_adv, re_turns, vari_ance))
# colnames(de_sign) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
de_sign <- as.data.frame(cbind(returns_adv, indicator_s))
colnames(de_sign)[1] <- "returns_adv"
# or
de_sign <- cbind(HighFreq::roll_sum(re_turns, look_back=look_back), 
                 HighFreq::roll_sum(moment_um, look_back=look_back), 
                 HighFreq::roll_sum(sk_ew, look_back=look_back))
de_sign <- as.data.frame(cbind(returns_adv, de_sign))
# de_sign <- cbind(returns_adv>0, de_sign)
colnames(de_sign) <- c("indic", "returns", "momentum", "skew")

# de_sign <- cbind(de_sign, rutils::lag_it(de_sign, lagg=1), rutils::lag_it(de_sign, lagg=2), rutils::lag_it(de_sign, lagg=3), rutils::lag_it(de_sign, lagg=4))
# de_sign <- cbind(re_turns, close_high, close_low, re_turns/sqrt(vari_ance), close_high/sqrt(vari_ance), vari_ance, vol_ume)
# colnames(de_sign)[4:5] <- c("re_turns_s", "close_high_s")
## apply rolling centering and scaling to the design matrix
de_sign <- lapply(de_sign, function(x) (x-mean(x))/sd(x))
de_sign <- rutils::do_call(cbind, de_sign)
sum(is.na(de_sign))



## create design matrix for ES1, TY1, UX1
# de_sign <- cbind(re_turns, close_high, close_low, rutils::diff_it(vari_ance), rutils::diff_it(vol_ume))
# de_sign from pc_a
# define indicators
look_back <- 5
indicator_s <- c("ES1.Close", "TY1.Close", "TU1.Close", "UX1.Close", "UX2.Close")
dygraphs::dygraph(oh_lc[, indicator_s[2]]-oh_lc[, indicator_s[3]])
indicator_s <- lapply(indicator_s, function(col_umn) {
  col_umn <- oh_lc[, col_umn]
  sig_nal <- rutils::diff_it(clo_se, lagg=look_back)/sqrt(look_back)/sqrt(HighFreq::roll_variance(oh_lc=oh_lc, look_back=look_back, sca_le=FALSE))
  HighFreq::roll_sum(indicator_s[, col_umn], look_back=look_back)/look_back
})  # end lapply
indicator_s <- rutils::do_call(cbind, indicator_s)
colnames(indicator_s) <- col_names
de_sign <- as.data.frame(cbind(returns_adv, de_sign))
# colnames(de_sign) <- c("indic", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7")
# or
de_sign <- as.data.frame(cbind(returns_adv, indicator_s))
colnames(de_sign)[1] <- "returns_adv"
# or
de_sign <- cbind(HighFreq::roll_sum(re_turns, look_back=look_back), 
                 HighFreq::roll_sum(moment_um, look_back=look_back), 
                 HighFreq::roll_sum(sk_ew, look_back=look_back))
de_sign <- as.data.frame(cbind(returns_adv, de_sign))
# de_sign <- cbind(returns_adv>0, de_sign)
colnames(de_sign) <- c("indic", "returns", "momentum", "skew")

# de_sign <- cbind(de_sign, rutils::lag_it(de_sign, lagg=1), rutils::lag_it(de_sign, lagg=2), rutils::lag_it(de_sign, lagg=3), rutils::lag_it(de_sign, lagg=4))
# de_sign <- cbind(re_turns, close_high, close_low, re_turns/sqrt(vari_ance), close_high/sqrt(vari_ance), vari_ance, vol_ume)
# colnames(de_sign)[4:5] <- c("re_turns_s", "close_high_s")
## apply rolling centering and scaling to the design matrix
de_sign <- lapply(de_sign, function(x) (x-mean(x))/sd(x))
de_sign <- rutils::do_call(cbind, de_sign)
sum(is.na(de_sign))





## run regressions of future returns against different indicators

# lm formula
col_names <- colnames(de_sign)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse=" + "), sep="~"))
for_mula <- as.formula(paste(col_names[1], paste(paste(col_names[-1], collapse=" + "), "- 1"), sep="~"))


# find extreme returns
ex_treme <- which(returns_adv > quantile(returns_adv, 0.99) | returns_adv < quantile(returns_adv, 0.01))
ex_treme <- sort(unique(c(ex_treme, ex_treme+1, ex_treme-1)))

# perform regression
mod_el <- lm(for_mula, data=de_sign)
# mod_el <- lm(returns_adv[-ex_treme] ~ de_sign[-ex_treme, ])
model_sum <- summary(mod_el)
model_sum$coefficients
weight_s <- model_sum$coefficients[, 1]
weight_s <- model_sum$coefficients[, 1][-1]
sig_nal <- xts(as.matrix(de_sign)[, -1] %*% weight_s, order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# signal from z-scores (t-values) of trailing slope
de_sign <- matrix(xts::.index(oh_lc), nc=1)
look_back <- 3
sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_back)
sig_nal <- roll::roll_scale(data=sig_nal, width=look_back, min_obs=1)
sig_nal[1:look_back, ] <- 0
sig_nal[is.infinite(sig_nal)] <- NA
sig_nal <- zoo::na.locf(sig_nal)
sum(is.infinite(sig_nal))
sum(is.na(sig_nal))
sd(sig_nal)
hist(sig_nal)
plot(sig_nal, t="l")


# wipp
# simulate ensemble of strategies using slope as technical indicator
# mean-reverting strategies
# par_am <- cbind(6:10, rep((3:12)/10, each=NROW(6:10)))
posit_mat <- sapply(4:8, function(look_short) {
  # mean reverting signal
  sig_nal_short <- calc_signal(oh_lc=log_ohlc,
                               clo_se=close_num,
                               de_sign=de_sign,
                               look_short=look_short, high_freq=FALSE)
  # Simulate the positions of mean reverting strategy
  sim_revert(sig_nal_short, re_turns, close_high, close_low, en_ter, ex_it, trade_lag=1)
})  # end sapply
par_am <- cbind(8:12, rep((3:12)/10, each=NROW(8:12)))
posit_mat <- sapply(1:NROW(par_am), function(it) {
  look_short <- par_am[it, 1]
  en_ter <- par_am[it, 2]
  sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_short)
  sig_nal[1:look_short, ] <- 0
  # scale sig_nal using roll_scale()
  sig_nal <- roll::roll_scale(data=sig_nal, width=look_short, min_obs=1)
  sig_nal[1:look_short, ] <- 0
  # sig_nal <- rutils::lag_it(sig_nal, lagg=1)
  # calculate positions, either: -1, 0, or 1
  po_sit <- rep(NA_integer_, NROW(oh_lc))
  po_sit[1] <- 0
  po_sit[sig_nal < (-en_ter)] <- 1
  po_sit[sig_nal > en_ter] <- (-1)
  na.locf(po_sit)
})  # end sapply
po_sit <- rowMeans(posit_mat)
po_sit[is.na(po_sit)] <- 0
po_sit <- rutils::lag_it(po_sit, lagg=1)
# plot(po_sit, t="l")
pnl_s <- cumsum(po_sit*re_turns)
pnl_s <- clo_se + 2*pnl_s
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clo_se, pnl_s)[endpoints(clo_se, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# scale returns using MAD median
num_returns <- as.numeric(re_turns)
foo <- sapply((look_back+1):NROW(num_returns), function(it) {
  sub_vec <- num_returns[(it-look_back+1):it]
  (num_returns[it]-median(sub_vec))/mad(sub_vec, constant=1.0)
})  # end sapply
tail(foo)
bar <- HighFreq::roll_scale(mat_rix=re_turns, look_back=look_back, use_median=TRUE)
bar[is.infinite(bar), ] <- 0
tail(drop(bar))
summary(microbenchmark(
  roll=roll::roll_scale(data=num_returns, width=look_back, min_obs=1),
  rcpp=roll_scale(mat_rix=num_returns, look_back=look_back),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary

foo <- (num_returns-median(num_returns))/mad(num_returns, constant=1.0)
bar <- HighFreq::calc_scaled(mat_rix=num_returns)
all.equal(foo, drop(bar))

library(microbenchmark)
summary(microbenchmark(
  pure_r=(num_returns-median(num_returns))/mad(num_returns, constant=1.0),
  rcpp=HighFreq::calc_scaled(mat_rix=num_returns),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


# wipp
# newer code: optimize strategies using slope as technical indicator

# OHLC data setup
oh_lc <- log(HighFreq::SPY["2010-10/2010-11"])
log_ohlc <- log(oh_lc)
op_en <- Op(log_ohlc)
hi_gh <- Hi(log_ohlc)
lo_w <- Lo(log_ohlc)
clo_se <- Cl(log_ohlc)
re_turns <- rutils::diff_it(clo_se)
# colnames(re_turns) <- "returns"
close_num <- as.numeric(clo_se)
close_high <- (close_num == high_num)
close_low <- (close_num == low_num)
date_s <- 1:NROW(oh_lc)
de_sign <- matrix(date_s, nc=1)

look_back <- 15
run_signal <- function(look_back, re_turns) {
  sig_nal <- HighFreq::roll_scale(mat_rix=re_turns, look_back=look_back, use_median=TRUE)
  sig_nal[1:look_back, ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal)
  rutils::lag_it(sig_nal, lagg=1)
}  # end run_signal
sig_nal <- run_signal(look_back, re_turns)
run_signal <- function(look_back, clo_se, de_sign) {
  sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, de_sign=de_sign, look_back=look_back)
  sig_nal[1:look_back, ] <- 0
  # sig_nal <- HighFreq::roll_scale(mat_rix=sig_nal, look_back=look_back, use_median=TRUE)
  # sig_nal[1:look_back, ] <- 0
  # sig_nal[is.infinite(sig_nal), ] <- 0
  sig_nal[is.infinite(sig_nal)] <- NA
  sig_nal <- zoo::na.locf(sig_nal)
  rutils::lag_it(sig_nal, lagg=1)
}  # end run_signal
sig_nal <- run_signal(look_back, clo_se, de_sign)
hist(sig_nal)
hist(sig_nal, xlim=c(-10, 10))

# perform parallel loop over look_backs under Windows
look_backs <- 15:35
library(parallel)
clus_ter <- makeCluster(num_cores-1)
clusterExport(clus_ter, varlist=c("clo_se", "de_sign"))
signal_s <- parLapply(clus_ter, X=look_backs, fun=run_signal, clo_se=clo_se, de_sign=de_sign)


# close_high and close_low are Boolean vectors which are TRUE if the close price is at the high or low price
run_strategy <- function(sig_nal, re_turns, en_ter, ex_it, close_high=TRUE, close_low=TRUE) {
  po_sit <- rep(NA_integer_, NROW(sig_nal))
  po_sit[1] <- 0
  # po_sit[sig_nal < (-en_ter)] <- 1
  po_sit[(sig_nal < (-en_ter)) & close_low] <- 1
  # po_sit[sig_nal > en_ter] <- (-1)
  po_sit[(sig_nal > en_ter) & close_high] <- (-1)
  po_sit[abs(sig_nal) < ex_it] <- 0
  po_sit <- zoo::na.locf(po_sit)
  po_sit <- po_sit + rutils::lag_it(po_sit, lagg=1)
  pnl_s <- cumsum(po_sit*re_turns)
  pnl_s[NROW(pnl_s)]
  # colnames(pnl_s) <- "strategy"
}  # end run_strategy
# trade entry and exit levels
en_ter <- 2.0
ex_it <- 0.5
pnl_s <- run_strategy(signal_s[[1]], re_turns, en_ter, ex_it, close_high, close_low)


run_strategies <- function(sig_nal, re_turns, en_ters, ex_it, close_high=TRUE, close_low=TRUE) {
  sapply(en_ters, run_strategy, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it, close_high=close_high, close_low=close_low)
  # pnl_s <- lapply(en_ters, run_strategy, sig_nal=sig_nal, re_turns=re_turns, ex_it=ex_it)
  # pnl_s <- rutils::do_call(cbind, pnl_s)
  # rowSums(pnl_s)
}  # end run_strategies
# trade entry levels
en_ters <- (5:40)/10
foo <- run_strategies(signal_s[[1]], re_turns, en_ters, ex_it=ex_it, close_high, close_low)
clusterExport(clus_ter, varlist=c("run_strategy"))
pnl_s <- parLapply(clus_ter, X=signal_s, fun=run_strategies, re_turns=re_turns, en_ters=en_ters, ex_it=ex_it, close_high=close_high, close_low=close_low)

stopCluster(clus_ter)  # stop R processes over cluster under Windows
pnl_s <- rutils::do_call(cbind, pnl_s)
rownames(pnl_s) <- paste0("en_ter=", en_ters)
colnames(pnl_s) <- paste0("look_back=", look_backs)
heatmap(pnl_s, Colv=NA, Rowv=NA, col=c("red", "blue"))
pnl_s <- rowSums(pnl_s)
pnl_s <- xts(pnl_s, index(oh_lc))
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clo_se, pnl_s)[endpoints(pnl_s, on="days")], main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# trade ensemble of strategies using slope as technical indicator
# mean-reverting strategies
foo <- sapply(2:15, function(look_back) {
  sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se, 
                          de_sign=de_sign, 
                          look_back=look_back)
  sig_nal[1:3, ] <- 0
  # sig_nal <- rutils::lag_it(sig_nal)
  -sign(sig_nal)
})  # end sapply
# trending strategies
bar <- sapply(10*(10:15), function(look_back) {
  sig_nal <- HighFreq::roll_zscores(res_ponse=clo_se,
                          de_sign=de_sign,
                          look_back=look_back)
  sig_nal[1:3, ] <- 0
  # sig_nal <- rutils::lag_it(sig_nal)
  sign(sig_nal)
})  # end sapply
po_sit <- cbind(foo, bar)
po_sit <- rowMeans(po_sit)
po_sit[is.na(po_sit)] <- 0
po_sit <- rutils::lag_it(po_sit)
plot(po_sit, t="l")
pnl_s <- cumsum(po_sit*re_turns)
pnl_s <- clo_se + 3*pnl_s
colnames(pnl_s) <- "strategy"
dygraphs::dygraph(cbind(clo_se, pnl_s), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))


# regress returns_adv versus moment_um indicator
# moment_um <- moment_um[abs(moment_um)>0.9]
foo <- sapply(1:10, function(look_back) {
  if (look_back>1)
    returns_adv <- rutils::lag_it(HighFreq::roll_sum(re_turns, look_back=look_back), lagg=-look_back)/look_back
  else
    returns_adv <- rutils::lag_it(re_turns, lagg=-look_back)
  mod_el <- lm(returns_adv ~ moment_um)
  model_sum <- summary(mod_el)
  model_sum$coefficients[2, 3]
})  # end sapply

# use average re_turns as predictor
foo <- sapply(1:11, function(look_back) {
  if (look_back>1)
    re_turns <- HighFreq::roll_sum(re_turns, look_back=look_back)/look_back
  else
    re_turns <- re_turns
  mod_el <- lm(returns_adv ~ re_turns)
  model_sum <- summary(mod_el)
  model_sum$coefficients[2, 3]
})  # end sapply

foo <- cbind(returns_adv, re_turns)
col_names <- colnames(foo)
for_mula <- as.formula(paste(col_names[1], paste(col_names[-1], collapse=" + "), sep="~"))
# perform regression
# returns_adv <- plogis(returns_adv, scale=-quantile(foo, 0.1))
ex_treme <- ((foo[, 2]>quantile(foo[, 2], 0.05)) & (foo[, 2]<quantile(foo[, 2], 0.95)))
mod_el <- lm(for_mula, data=foo[ex_treme])
model_sum <- summary(mod_el)
model_sum
plot(for_mula, data=foo[ex_treme])
abline(mod_el, lwd=2, col="red")

# perform optimization
# objective function equal to the strategy Sharpe ratio
# plus a penalty term for the weight constraint:
# sum(weight_s) == 1.
object_ive <- function(weight_s, indicator_s, re_turns) {
  sig_nal <- rutils::lag_it(indicator_s %*% weight_s)
  pnl_s <- sig_nal*re_turns
  se_lect <- ((pnl_s>quantile(pnl_s, 0.05)) & (pnl_s<quantile(pnl_s, 0.95)))
  pnl_s <- pnl_s[se_lect]
  -mean(pnl_s)/sd(pnl_s) + (sum(weight_s) - 1)^2
}  # end object_ive

# perform parameter optimization using function optim()
op_tim <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s,
                re_turns=re_turns)
weight_s <- op_tim$par
names(weight_s) <- colnames(indicator_s)
sig_nal <- xts(indicator_s %*% weight_s, order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# perform logistic regression
g_lm <- glm(for_mula, data=de_sign, family=binomial)
summary(g_lm)
glm_predict <- predict(g_lm, newdata=de_sign, type="response")
en_ter <- 0.58
fore_casts <- data.frame((glm_predict>en_ter), coredata(de_sign[, 1]))
colnames(fore_casts) <- c("lm_pred", "realized")
table(fore_casts)
sig_nal <- xts(de_sign %*% rota_tion, order.by=index(oh_lc))


# perform lda
l_da <- MASS::lda(for_mula, data=de_sign)
summary(l_da)
lda_predict <- predict(l_da, newdata=de_sign)
fore_casts <- data.frame(lda_predict$class, coredata(de_sign[, 1]))
colnames(fore_casts) <- c("lda_pred", "realized")
table(fore_casts)

# perform qda
q_da <- MASS::qda(for_mula, data=de_sign)
summary(q_da)
qda_predict <- predict(q_da, newdata=de_sign)
fore_casts <- data.frame(qda_predict$class, coredata(de_sign[, 1]))
colnames(fore_casts) <- c("qda_pred", "realized")
table(fore_casts)


# Calculate PCA of de_sign
pc_a <- prcomp(de_sign)
pc_a$sdev
pc_a$rotation
# lm
mod_el <- lm(returns_adv ~ pc_a$x - 1)
model_sum <- summary(mod_el)
model_sum$coefficients


# curated PCs
rota_tion <- cbind(PC1=rep(0.2, 5), 
                   PC2=c(-2, -1, 0, 1, 2),
                   PC3=c(-1, 0.5, 1, 0.5, -1))
pca_ts <- xts(de_sign %*% rota_tion, order.by=index(de_sign))

mod_el <- lm(returns_adv ~ pca_ts - 1)
model_sum <- summary(mod_el)
model_sum$coefficients


## Perform in-sample
in_sample <- 1:2000
# Define OHLC technical indicators
indic_in <- indicator_s[in_sample]
# scale indic_in using sigmoid
indic_in <- lapply(1:NCOL(indic_in), function(col_umn) {
  x <- plogis(indic_in[, col_umn], scale=-quantile(indic_in[, col_umn], 0.01))
  (x - median(x))
})  # end lapply
indic_in <- rutils::do_call(cbind, indic_in)
design_in <- as.data.frame(cbind(returns_adv[in_sample], indic_in))
colnames(design_in)[1] <- "returns_adv"

# perform optimization
op_tim <- optim(par=rep(0.1, NCOL(indicator_s)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(indicator_s)),
                lower=rep(-1, NCOL(indicator_s)),
                indicator_s=indicator_s[in_sample],
                re_turns=re_turns[in_sample])
weight_s <- op_tim$par
names(weight_s) <- colnames(indicator_s)
sig_nal <- xts(indicator_s %*% weight_s, order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# perform regression
mod_el <- lm(for_mula, data=design_in)
# mod_el <- lm(returns_adv[-ex_treme] ~ de_sign[-ex_treme, ])
model_sum <- summary(mod_el)
weight_s <- model_sum$coefficients[, 1][-1]

# or
pc_a <- prcomp(de_sign[in_sample, ])
pc_a$sdev
pc_a$rotation
mod_el <- lm(returns_adv[in_sample] ~ pc_a$x - 1)
model_sum <- summary(mod_el)
model_sum$coefficients

# or
mod_el <- lm(returns_adv[in_sample] ~ pca_ts[in_sample] - 1)
model_sum <- summary(mod_el)

# or
mod_el <- lm(returns_adv[in_sample] ~ de_sign[in_sample, ] - 1)
model_sum <- summary(mod_el)


weight_s <- model_sum$coefficients[, 1]
# weight_s <- weight_s[-1]
t_vals <- rep(TRUE, NROW(weight_s))
t_vals <- (abs(model_sum$coefficients[, 3]) > 2)
weight_s[!t_vals] <- 0


## Perform out-of-sample
out_sample <- 2001:NROW(oh_lc)
# Define OHLC technical indicators
indic_out <- indicator_s[out_sample, ]
# scale indic_in using sigmoid
indic_out <- lapply(1:NCOL(indic_out), function(col_umn) {
  x <- plogis(indic_out[, col_umn], scale=-quantile(indicator_s[in_sample, col_umn], 0.01))
  (x - median(indicator_s[in_sample, col_umn]))
})  # end lapply
indic_out <- rutils::do_call(cbind, indic_out)
sig_nal <- xts(indic_out %*% weight_s, order.by=index(oh_lc[out_sample]))
# simulate strategy
pnl_s <- cumsum(sig_nal*re_turns[out_sample])
colnames(pnl_s) <- "strategy"


# or
sig_nal <- xts(as.matrix(de_sign)[, t_vals] %*% weight_s[t_vals], order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)

# or
sig_nal <- xts(de_sign %*% rota_tion, order.by=index(oh_lc))
# sig_nal <- xts(de_sign %*% pc_a$rotation[, t_vals], order.by=index(oh_lc))
sig_nal <- xts(as.matrix(de_sign)[, -1] %*% weight_s, order.by=index(oh_lc))
sig_nal <- xts(sig_nal[, t_vals] %*% weight_s[t_vals], order.by=index(oh_lc))
sig_nal <- rutils::lag_it(sig_nal)


# simulate strategy
pnl_s <- cumsum(sig_nal*re_turns)
colnames(pnl_s) <- "strategy"

# plot
library(dygraphs)
dygraphs::dygraph(cbind(clo_se, pnl_s), main="OHLC Technicals Strategy") %>%
  dyAxis("y", label="VTI", independentTicks=TRUE) %>%
  dyAxis("y2", label="strategy", independentTicks=TRUE) %>%
  dySeries("strategy", axis="y2", col=c("blue", "red"))



# de_sign <- cbind(rets_lag2, z_scores[[3]], hu_rst, sharpe_rolling)
# colnames(de_sign) <- c("returns", "variance", "skew", "hurst")
end_points <- xts::endpoints(de_sign, "years")

## apply rolling centering and scaling to the design matrix
# library(roll)
de_sign <- roll::roll_scale(data=de_sign, width=100*look_back, min_obs=1)
# remove NAs
de_sign[is.na(de_sign)] <- 0
sum(is.na(de_sign))

## perform regressions of future returns against different indicators

# single indicator
returns_adv <- re_turns + close_high + close_low
mod_el <- lm(returns_adv ~ returns_adv)
summary(mod_el)

# three indicators - lower lows is most significant
mod_el <- lm(returns_adv ~ re_turns + close_high + close_low)
summary(mod_el)

# single indicator
# lower lows indicator works well in bearish periods
returns_adv <- -re_turns - close_high + close_low
returns_adv <- sign(returns_adv)
mod_el <- lm(returns_adv ~ returns_adv)
summary(mod_el)

mo_del <- lm(rets_adv2 ~ de_sign)
summary(mo_del)
coef(summary(mo_del))
beta_s <- -coef(summary(mo_del))[-1, 1]

max_eigen <- 2
cov_mat <- cov(ex_cess)

# calculate eigen decomposition
ei_gen <- eigen(mat_rix)
eigen_values <- ei_gen$values
eigen_vec <- ei_gen$vectors

# check for zero singular values
# set tolerance for determining zero singular values
to_l <- sqrt(.Machine$double.eps)
not_zero <- (eigen_values > (to_l * eigen_values[1]))

# calculate generalized inverse from eigen decomposition
eigen_inverse <- eigen_vec[, not_zero] %*% 
  (t(eigen_vec[, not_zero]) / eigen_values[not_zero])

# perform eigen decomposition and calculate eigenvectors and eigenvalues
ei_gen <- eigen(cov_mat)
eigen_vec <- ei_gen$vectors
# calculate regularized inverse
in_verse <- eigen_vec[, 1:max_eigen] %*% (t(eigen_vec[, 1:max_eigen]) / ei_gen$values[1:max_eigen])
# calculate the maximum Sharpe ratio portfolio weights
# weight_s <- in_verse %*% colMeans(ex_cess)
# weight_s <- rep(mean(colMeans(ex_cess)), NCOL(ex_cess))
weight_s <- colMeans(ex_cess)
weight_s <- in_verse %*% weight_s
weight_s <- drop(weight_s/sqrt(sum(weight_s^2)))


# simulate strategy
pnl_s <- cumsum(sig_nal*re_turns)
colnames(pnl_s) <- "strategy"




###############
### State space model and Kalman filter

## Simulate state space model

# Length of data
# len_gth <- NROW(end_points)
len_gth <- 100  # number of time points
# n <- 5    # number of observations at each time point
# p <- 2    # number of covariates


# True parameter values

rho_v <- 2.0
rho_w <- 1.0
gg <- 1.0
hh <- 1.0

# Allocate state vector xx
xx <- numeric(len_gth)
# Transition equation for state vector under AR(1) process
set.seed(1121)
# xx[1] <- rnorm(1, sd=rho_w)
for (it in 2:len_gth) {
  xx[it] <- gg*xx[it-1] + rnorm(1, sd=rho_w)
}  # end for

# Measurement equation for measured vector
yy <- hh*xx + rnorm(len_gth, sd=rho_v)

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
zz <- numeric(len_gth)
zz[1] <- aa*yy[1]
# Allocate process variance vector pp
pp <- numeric(len_gth)
pp[1] <- 1
# Allocate predicted variance vector ppp
ppp <- numeric(len_gth)
ppp[1] <- aa^2*pp[1] + qq
# Allocate Kalman gain vector kk
kk <- numeric(len_gth)
kk[1] <- ppp[1]*hh/(ppp[1]*hh^2+rr)

# Apply Kalman filter recursivelly
for (it in 2:len_gth) {
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
X <- array(rnorm(len_gth*n*p), c(n, p, len_gth))
X[, 1, ] <- 1


Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/kalman_filter.cpp")




# Calculate ETF prices
sym_bols <- colnames(rutils::env_etf$price_s)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
price_s <- rutils::env_etf$price_s[, sym_bols]
# Carry forward non-NA prices
price_s <- zoo::na.locf(price_s)
price_s <- na.omit(price_s)
# Calculate simple ETF returns
re_turns <- rutils::diff_it(price_s)
# Calculate the daily excess returns
# risk_free is the daily risk-free rate
risk_free <- 0.03/260
ex_cess <- re_turns - risk_free


# Define monthly end_points without initial warmpup period
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points <- end_points[end_points>50]
len_gth <- NROW(end_points)
# Define 12-month look_back interval and start_points over sliding window
look_back <- 12
start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])

# Define the shrinkage intensity
al_pha <- 0.5
max_eigen <- 3

# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(end_points),
                     function(i) {
                       # subset the ex_cess returns
                       ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
                       ei_gen <- eigen(cov(ex_cess))
                       # Calculate regularized inverse of covariance matrix
                       max_eigen <- 3
                       eigen_vec <- ei_gen$vectors[, 1:max_eigen]
                       eigen_val <- ei_gen$values[1:max_eigen]
                       in_verse <- eigen_vec %*% (t(eigen_vec) / eigen_val)
                       # Apply shrinkage to the mean returns
                       col_means <- colMeans(ex_cess)
                       col_means <- ((1-al_pha)*col_means + al_pha*mean(col_means))
                       # Calculate weights using R
                       weight_s <- in_verse %*% col_means
                       weight_s <- weight_s/sum(weight_s)
                       # subset the re_turns to out-of-sample returns
                       re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
                       # calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply

# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"


# Simulate a monthly rolling portfolio optimization strategy
strat_rets <- lapply(2:NROW(end_points),
                     function(i) {
                       # subset the ex_cess returns
                       ex_cess <- ex_cess[start_points[i-1]:end_points[i-1], ]
                       # apply regularized inverse to mean of ex_cess
                       weight_s <- HighFreq::calc_weights(ex_cess, max_eigen, al_pha)
                       # subset the re_turns to out-of-sample returns
                       re_turns <- re_turns[(end_points[i-1]+1):end_points[i], ]
                       # calculate the out-of-sample portfolio returns
                       xts(re_turns %*% weight_s, index(re_turns))
                     }  # end anonymous function
)  # end lapply
# Flatten the list of xts into a single xts series
strat_rets <- rutils::do_call(rbind, strat_rets)
colnames(strat_rets) <- "strat_rets"

indicator_s <- HighFreq::roll_portf(ex_cess, re_turns, start_points-1, end_points-1, max_eigen, al_pha)
indicator_s <- xts(indicator_s, index(re_turns))
colnames(indicator_s) <- "strat_rets"

# Compare RcppArmadillo with R
all.equal(strat_rets, indicator_s[index(strat_rets)])

# Plot dygraph
dygraphs::dygraph(cumsum(indicator_s), 
                  main="Cumulative Returns of Max Sharpe Portfolio Strategy")



###############
### Benchmark eigen decomposition function in RcppArmadillo

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/calc_eigen.cpp")

ei_gen <- calc_eigen(scale(prices_ts, scale=FALSE))
mod_el <- prcomp(prices_ts)
all.equal(mod_el$sdev^2, drop(ei_gen$values))
all.equal(unname(mod_el$rotation), -ei_gen$vectors)

library(microbenchmark)
summary(microbenchmark(
  rcpp=calc_eigen(prices_ts),
  pure_r=prcomp(prices_ts),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### ARIMA simulation

library(rutils)

Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/sim_arima.cpp")

co_eff <- -0.8
in_nov <- rnorm(100)

ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
for (i in 2:NROW(in_nov)) ari_ma[i] <- co_eff*ari_ma[i-1] + in_nov[i]
foo <- ari_ma

ari_ma <- filter(c(in_nov), filter=co_eff, method="recursive")
all.equal(as.numeric(ari_ma), foo)


# two co_eff
co_eff <- c(-0.8, 0.2)

# old
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
ari_ma[2] <- co_eff[1]*ari_ma[1] + in_nov[2] 
for (i in 3:NROW(in_nov)) {
  ari_ma[i] <- co_eff[1]*ari_ma[i-1] + co_eff[2]*ari_ma[i-2] + in_nov[i]
}  # end for
foo <- ari_ma

# vectorized
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
for (i in 2:NROW(co_eff)) {
  ari_ma[i] <- co_eff[1:(i-1)] %*% ari_ma[(i-1):1] + in_nov[i]
}  # end for

for (i in (NROW(co_eff)+1):NROW(in_nov)) {
  ari_ma[i] <- co_eff %*% ari_ma[(i-1):(i-NROW(co_eff))] + in_nov[i]
}  # end for
foo <- ari_ma

ari_ma <- filter(in_nov, filter=co_eff, method="recursive")
all.equal(as.numeric(ari_ma), foo)


# vectorized vector of co_eff
co_eff <- c(-0.8, 0.2)
ari_ma <- numeric(NROW(in_nov))
ari_ma[1] <- in_nov[1]
for (i in 2:NROW(co_eff)) {
  ari_ma[i] <- co_eff[1:(i-1)] %*% ari_ma[(i-1):1] + in_nov[i]
}  # end for

for (i in (NROW(co_eff)+1):NROW(in_nov)) {
  ari_ma[i] <- co_eff %*% ari_ma[(i-1):(i-NROW(co_eff))] + in_nov[i]
}  # end for
foo <- ari_ma

ari_ma <- filter(in_nov, filter=co_eff, method="recursive")
all.equal(as.numeric(ari_ma), foo)

ari_ma <- sim_arima(in_nov, rev(co_eff))
all.equal(as.numeric(ari_ma), foo)

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::sim_arima(in_nov, rev(co_eff)),
  pure_r=filter(in_nov, filter=co_eff, method="recursive"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### convolutions and filtering

# Rcpp::sourceCpp(file="C:/Develop/R/lecture_slides/assignments/roll_wsum.cpp")
library(HighFreq)

wei_ghts <- c(1, rep(1e-5, 10))

wei_ghts <- exp(-0.2*1:11)
wei_ghts <- wei_ghts/sum(wei_ghts)
vec_tor <- as.numeric(rutils::env_etf$VTI[, 6])
weight_ed <- HighFreq::roll_wsum(vec_tor=vec_tor, wei_ghts=rev(wei_ghts))
filter_ed <- filter(x=vec_tor, filter=wei_ghts, method="convolution", sides=1)

all.equal(as.numeric(vec_tor), as.numeric(weight_ed))

all.equal(as.numeric(filter_ed[-(1:10)]), as.numeric(weight_ed))


library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vec_tor=vec_tor, wei_ghts=wei_ghts),
  pure_r=filter(x=vec_tor, filter=wei_ghts, method="convolution", sides=1, circular=TRUE),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary


filter_ed <- roll_wsum_arma(vec_tor, rev(wei_ghts))
all.equal(as.numeric(filter_ed[-(1:10)]), as.numeric(weight_ed))

library(microbenchmark)
summary(microbenchmark(
  rcpp=HighFreq::roll_wsum(vec_tor=vec_tor, wei_ghts=wei_ghts),
  arma=roll_wsum_arma(vec_tor, rev(wei_ghts)),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary



###############
### Stitching and Updating Data for S&P500 Constituents
# in tests already

library(rutils)

# load new data
load("C:/Develop/R/lecture_slides/data/sp500_2018.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(env_sp500, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_new are the new symbols
symbols_new <- ls(env_sp500)
# env_sp500_new is the new data
env_sp500_new <- env_sp500


# load old data
load("C:/Develop/R/lecture_slides/data/sp500_2017.RData")
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(env_sp500, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))
# symbols_old are the old symbols
symbols_old <- ls(env_sp500)
# env_sp500_old is the old data
env_sp500_old <- env_sp500
rm(env_sp500)

# find the new symbols that are not in the old symbols
is_in <- symbols_new %in% symbols_old
symbols_new[!is_in]

# find the old symbols that are in the new symbols
is_in <- symbols_old %in% symbols_new
sym_bols <- symbols_old[is_in]
# find the old symbols that are not in the new symbols
symbols_old[!is_in]

# create a new environment to store the updated data
env_sp500 <- new.env()
# copy the old symbols that are also in the new symbols from env_sp500_old to env_sp500
# for (sym_bol in sym_bols) {
#   assign(sym_bol, get(sym_bol, envir=env_sp500_old), envir=env_sp500)
#   # env_sp500$sym_bol <- env_sp500_old$sym_bol
# }  # end for

# stitch the old and new data and copy it into env_sp500
for (sym_bol in sym_bols) {
  # get old data
  old_data <- get(sym_bol, envir=env_sp500_old)
  end_date <- end(old_data)
  # get new data
  new_data <- get(sym_bol, envir=env_sp500_new)
  # stitch the old and new data only if old is older
  if (start(old_data) < start(new_data)) {
    cl_ose <- new_data[, 4]
    # diff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] - as.numeric(cl_ose))
    # diff the Close prices
    cl_ose <- rutils::diff_it(log(cl_ose))
    # calculate new extended Close prices
    new_close <- as.numeric(old_data[end_date, 4])*exp(cumsum(cl_ose[index(new_data)>end_date]))
    # foo <- as.numeric(new_data[end_date, 4])*exp(cumsum(cl_ose[index(new_data)>end_date]))
    # all.equal(new_data[index(new_data)>end_date, 4], foo)
    # stitch the Close prices
    new_close <- rbind(old_data[, 4], new_close)
    # all.equal(NROW(index(new_close)), NROW(unique(index(new_close))))
    # new_data <- (new_data[, 1:3] + as.numeric(new_close))
    # undiff the OHL prices
    new_data[, 1:3] <- (new_data[, 1:3] + as.numeric(new_close[index(new_data)]))
    new_data[, 4] <- new_close[index(new_data)]
    new_data[, 6] <- new_close[index(new_data)]
    # stitch all the prices
    new_data <- rbind(old_data, new_data[index(new_data)>end_date])
  }  # end if
  # copy the data
  assign(sym_bol, new_data, envir=env_sp500)
}  # end for

# verify that all symbols were stitched
all.equal(sym_bols, ls(env_sp500))
# verify that the Close and Adjusted price columns are equal for all symbols
sum(!unlist(eapply(env_sp500, function(x) {
  x <- unname(coredata(x))
  all.equal(x[, 4], x[, 6])
})))

save(env_sp500, file="C:/Develop/R/lecture_slides/data/sp500.RData")



###############
### sweep() for matrix multiplication

mat_rix1 <- matrix(rnorm(1e6), ncol=100)
vec_tor <- rnorm(1e2)
mat_rix2 <- diag(vec_tor)
pro_duct <- mat_rix1 %*% mat_rix2
pro_duct2 <- sweep(mat_rix1, 2, vec_tor, FUN="*")
all.equal(pro_duct, pro_duct2)

# sweep() is about 5 times faster
library(microbenchmark)
summary(microbenchmark(
  matrix_mult=(mat_rix1 %*% mat_rix2),
  sweep=sweep(mat_rix1, 2, vec_tor, FUN="*"),
  times=10))[, c(1, 4, 5)]  # end microbenchmark summary




###############
### Portfolio optimization with constraints

# This is an objective function equal to the portfolio 
# variance plus a penalty term for the weight constraint:
# sum(weight_s) == 1.

object_ive <- function(weight_s, re_turns) {
  var(re_turns %*% weight_s) + 
    (sum(weight_s) - 1)^2
}  # end object_ive

# Perform portfolio optimization with the same two weight 
# constraints as in p.1
# You must use function optim().

op_tim <- optim(par=rep(1.0, NCOL(re_turns)),
                fn=object_ive,
                method="L-BFGS-B",
                upper=rep(1, NCOL(re_turns)),
                lower=rep(-1, NCOL(re_turns)),
                re_turns=re_turns)

weight_s <- op_tim$par

var(re_turns %*% weight_s)


# You should get output similar to the following:
# > op_tim$par


object_ive <- function(weight_s, re_turns, conf_level, portfolio_sub) {
  # portf_rets <- re_turns %*% weight_s
  # var(portf_rets) + 
  t(weight_s) %*% cov_mat %*% weight_s +
    1000*(sum(weight_s) - 1)^2 +
    1000*(sum(weight_s * portfolio_sub[-1]) - portfolio_sub[1])^2
}  # end object_ive



###############
### plot multiple dygraphs in the same RStudio window

# create the time series
temperature <- ts(frequency = 12, start = c(1980, 1),
                  data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 
                           25.2, 26.5, 23.3, 18.3, 13.9, 9.6))
rainfall <- ts(frequency = 12, start = c(1980, 1),
               data = c(49.9, 71.5, 106.4, 129.2, 144.0, 176.0, 
                        135.6, 148.5, 216.4, 194.1, 95.6, 54.4))

# create a list of dygraphs objects
library(dygraphs)
dy_graph <- list(
  dygraphs::dygraph(temperature, group="temp_rain", main="temperature", width=400, height=200),
  dygraphs::dygraph(rainfall, group="temp_rain", main="rainfall", width=400, height=200)
)  # end list
  
# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dy_graph))



###############
### plot multiple dygraphs in the same RStudio window

# load packages
library(quantmod)
library(dygraphs)

# download time series into an environment
sym_bols <- c("VTI", "EEM")
data_env <- new.env()
quantmod::getSymbols(sym_bols, from="2017-01-01", env=data_env)

dygraphs::dygraph(data_env$EEM[, 1:4]) %>% dygraphs::dyCandlestick()

# create a list of dygraphs objects in a loop
dy_graph <- eapply(data_env, function(x_ts) {
  dygraphs::dygraph(x_ts[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(x_ts)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
})  # end eapply

# render the dygraphs objects using htmltools
htmltools::browsable(htmltools::tagList(dy_graph))

## perform same plotting as above using pipes syntax
# create a list of dygraphs objects in a loop
eapply(data_env, function(x_ts) {
  dygraphs::dygraph(x_ts[, 1:4], group="etfs",
                    main=paste("Plot of:", substring(colnames(x_ts)[1], 1, 3)),
                    width=400, height=200) %>% dygraphs::dyCandlestick()
}) %>% # end eapply
  # render the dygraphs objects using htmltools
  htmltools::tagList() %>% htmltools::browsable()




###############
### dygraph plot with highlighting of specific points

library(xts)
library(dygraphs)
# convert numeric time index of ldeaths into class 'Date' (approximately)
in_dex <- as.Date(365*(zoo::index(ldeaths)-1970))
# convert time index from class 'Date' to 'POSIXct'
in_dex <- as.POSIXct.Date(in_dex)

# convert ldeaths into xts time series
l_deaths <- xts::xts(as.numeric(ldeaths), order.by=in_dex)
# calculate number of years
n_years <- NROW(in_dex)/12
# calculate the January dates
jan_dates <- in_dex[1 + 12*(0:(n_years - 1))]
# or
# jan_dates <- in_dex[which(months(in_dex)=="January")]
# or
# jan_dates <- in_dex[grep("Jan", months(in_dex), ignore.case=TRUE)]
# calculate the July dates
jul_dates <- in_dex[7 + 12*(0:(n_years - 1))]
# or
# jul_dates <- in_dex[which(months(in_dex)=="July")]

# create dygraph object
dy_graph <- dygraphs::dygraph(l_deaths, main="Dygraph of ldeaths with Annotations") %>% 
  dyHighlight(highlightCircleSize=5)

# add annotations for the January and July dates to dygraph object
for (i in 1:NROW(jan_dates)) {
  dy_graph <- dygraphs::dyAnnotation(dy_graph, x=jan_dates[i], text="Jan")
}  # end for
for (i in 1:NROW(jul_dates)) {
  dy_graph <- dygraphs::dyAnnotation(dy_graph, x=jul_dates[i], text="Jul")
}  # end for

# plot dygraph object
dy_graph



###############
### exponentiation operator is a function:

'^'(3, 2)
"^"(3, 2)



###############
### Split random xts time series into daily list and rbind it back into the original xts

x_ts <- xts(x=rnorm(100), order.by=(Sys.time()-3600*(1:100)))
# split time series into daily list
li_st <- xts::split.xts(x_ts, "days")
# rbind the list back into a time series and compare with the original
all.equal(x_ts, rutils::do_call(rbind, li_st))



###############
### Code to check for duplicate dates

# create xts with duplicate dates
x <- xts(rnorm(5), order.by=c(Sys.Date() + 1:4, Sys.Date() + 2))
diff(index(x))

rutils::diff_it(index(x))

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

# simulate Brownian motion
vol_at <- 0.01
set.seed(1121)  # reset random number generator
re_turns <- sapply(mean_s, rnorm, n=n_row, sd=vol_at)
re_turns <- apply(re_turns, 2, cumsum)
# apply(re_turns, 2, mean)
# plot.zoo(re_turns, plot.type="single")

# length of lookback window
look_back <- 100
# define end_points with beginning stub
num_agg <- n_row %/% look_back
end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
start_points <- end_points[c(1, 1:(len_gth-1))] + 1
fix_points <- (start_points > end_points)
start_points[fix_points] <- end_points[fix_points]

# total re_turns aggregated over non-overlapping windows
agg_rets <- apply(re_turns, 2, function(x) (x[end_points]-x[start_points]))


# switch to best manager with biggest total re_turns
be_st <- apply(agg_rets, 1, which.max)
be_st <- rutils::lag_it(be_st)
be_st[1] <- 1
# be_st <- c(rep(1, NROW(end_points)-NROW(be_st)), be_st)
pnl_s <- agg_rets[cbind(1:NROW(agg_rets), be_st)]
# pnl_s <- lapply(seq_along(end_points), function(in_dex) {
#   re_turns[start_points[in_dex]:end_points[in_dex], be_st[in_dex]]
# })  # end lapply
# pnl_s <- rutils::do_call(c, pnl_s)
plot.zoo(cumsum(pnl_s))


## cum_pnl for multi-manager strategy
cum_pnl <- function(sharpe_ratio, re_turns=NULL, mean_s=NULL, num_managers, n_row, look_back, vol_at=0.01) {
  # simulate Brownian motion
  if(is.null(re_turns)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- c(2*p1-1, rep(2*p2-1, num_managers-1))
    set.seed(1121)  # reset random number generator
    re_turns <- sapply(mean_s, rnorm, n=n_row, sd=vol_at)
    re_turns <- apply(re_turns, 2, cumsum)
  } else {
    num_managers <- NCOL(re_turns)
    n_row <- NROW(re_turns)
  }  # end if

  # define end_points with beginning stub
  num_agg <- n_row %/% look_back
  end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  len_gth <- NROW(end_points)
  # start_points are single-period lag of end_points
  start_points <- end_points[c(1, 1:(len_gth-1))] + 1
  fix_points <- (start_points > end_points)
  start_points[fix_points] <- end_points[fix_points]

  # total re_turns over non-overlapping windows
  agg_rets <- apply(re_turns, 2, function(x) (x[end_points]-x[start_points]))

  # switch to best manager with biggest total re_turns
  be_st <- apply(agg_rets, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  be_st <- c(rep(1, NROW(end_points)-NROW(be_st)), be_st)
  pnl_s <- lapply(seq_along(end_points), function(in_dex) {
    re_turns[start_points[in_dex]:end_points[in_dex], be_st[in_dex]]
  })  # end lapply
  # return total expected pnl
  pnl_s <- rutils::do_call(c, pnl_s)
  sum(pnl_s)
}  # end cum_pnl

cum_pnl(re_turns=re_turns, look_back=100)

cum_pnl(sharpe_ratio=0.4, num_managers=11, look_back=100, n_row=5000)



## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, n_row=NULL, sharpe_ratio=NULL, re_turns=NULL, mean_s=NULL, num_managers=NULL, vol_at=0.01) {
  # calculate drifts
  if(is.null(mean_s)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- vol_at*look_back*c(2*p1-1, rep(2*p2-1, num_managers-1))
  } else {
    num_managers <- NROW(mean_s)
  }  # end if
  # simulate Brownian motion
  if(is.null(re_turns)) {
    # set.seed(1121)  # reset random number generator
    num_agg <- n_row %/% look_back
    re_turns <- sapply(mean_s, rnorm, n=num_agg, sd=sqrt(look_back)*vol_at)
  } else {
    num_managers <- NCOL(re_turns)
    n_row <- NROW(re_turns)
  }  # end if

  # switch to best manager with biggest total re_turns
  be_st <- apply(re_turns, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnl_s <- re_turns[cbind(1:NROW(re_turns), be_st)]
  sum(re_turns[cbind(1:NROW(re_turns), be_st)])
}  # end cum_pnl



## cum_pnl for multi-manager strategy (simplest version)
cum_pnl <- function(look_back, n_row, sharpe_ratio=NULL, re_turns=NULL, mean_s=NULL, num_managers=NULL, vol_at=0.01) {
  # calculate drifts
  if(is.null(mean_s)) {
    pro_b <- (sharpe_ratio/sqrt(250)+1)/2
    # Adjust probability to account for multiple managers
    p1 <- (0.5*num_managers + (pro_b - 0.5)*(num_managers-1)) / num_managers
    p2 <- (0.5*num_managers - (pro_b - 0.5)) / num_managers
    mean_s <- vol_at*look_back*c(2*p1-1, rep(2*p2-1, num_managers-1))
  } else {
    num_managers <- NROW(mean_s)
  }  # end if

  # calculate probability of selecting the best manager
  pro_b <- integrate(function(x, ...)
    dnorm(x, mean=mean_s[1], ...)*pnorm(x, mean=mean_s[2], ...)^(num_managers-1),
            low=-3.0, up=3.0,
            sd=sqrt(look_back)*vol_at)$value
  # return total expected pnl
  num_agg <- n_row %/% look_back
  num_agg*(pro_b*mean_s[1] + (1-pro_b)*mean_s[2])
}  # end cum_pnl

cum_pnl(sharpe_ratio=0.4, num_managers=11, mean_s=mean_s, look_back=100, n_row=5000)
cum_pnl(sharpe_ratio=0.4, num_managers=11, look_back=100, n_row=5000)
cum_pnl(look_back=100, sharpe_ratio=0.4, num_managers=11, n_row=5000)

# calculate average pnl
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
plot(cumsum(pnl_s), t="l")

# perform loop over number of managers
num_managers <- 2*(1:50)
foo <- sapply(num_managers, cum_pnl,
              re_turns=NULL, sharpe_ratio=0.4, look_back=100, n_row=50000, mean_s=NULL, vol_at=0.01)
foo <- cbind(num_managers, foo)
plot(foo, t="l")



###############
### simulation of asset returns, with a time-dependent drift (skill) plus a random noise.

# define daily volatility: daily prices change by vol_at units
vol_at <- 0.01
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
mea_n <- vol_at*(2*pro_b-1)
# time-dependent drift (skill)
# dri_ft <- 0.01*sin(ra_te*(1:n_row)/n_row)
# dri_ft <- rutils::do_call(c, lapply(1:num_managers, function(x) (dri_ft + 2*pi*x/num_managers)))
dri_ft <- sapply(1:num_managers, function(x)
  mea_n*sin(ra_te*(1:n_row)/n_row + 2*pi*x/num_managers))

# simulate multiple price paths

# re_turns <- xts(vol_at*rnorm(n_row) + dri_ft - vol_at^2/2,
#                 order.by=seq.Date(Sys.Date()-n_row+1, Sys.Date(), by=1))
# chart_Series(x=re_turns, name="Multiple price paths")

set.seed(1121)  # reset random number generator
re_turns <- matrix(vol_at*rnorm(num_managers*n_row) - vol_at^2/2, nc=num_managers) + dri_ft
# re_turns <- exp(matrixStats::colCumsums(re_turns))
# create zoo time series
# re_turns <- xts(re_turns, order.by=seq.Date(Sys.Date()-NROW(re_turns)+1, Sys.Date(), by=1))
# plot zoo time series
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(re_turns))
# col_ors <- col_ors[order(order(re_turns[NROW(re_turns), ]))]
par(mfrow=c(2, 2))
par(mar=c(3, 1, 1, 1), oma=c(1, 1, 1, 1))
plot.zoo(dri_ft, main="time-dependent growth rates", lwd=3, xlab=NA, ylab=NA, plot.type="single", col=col_ors)
plot.zoo(re_turns, main="simulated returns", xlab=NA, ylab=NA, plot.type="single", col=col_ors)
plot.zoo(apply(re_turns, 2, cumsum),
         main="simulated prices", xlab=NA, ylab=NA, plot.type="single", col=col_ors)
# plot_theme <- chart_theme()
# plot_theme$col$line.col <- col_ors
# chart_Series(re_turns, theme=plot_theme, name="Multiple price paths")


## calculate pnl over lookback window
# calculate cumulative returns
cum_pnls <- apply(re_turns, 2, cumsum)
# length of lookback window
look_back <- 100
# define end_points with beginning stub
num_agg <- n_row %/% look_back
end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
start_points <- end_points[c(1, 1:(len_gth-1))] + 1
fix_points <- (start_points > end_points)
start_points[fix_points] <- end_points[fix_points]

# total re_turns aggregated over non-overlapping windows
agg_rets <- apply(cum_pnls, 2, function(x) (x[end_points]-x[start_points]))

# switch to best manager with biggest total re_turns
be_st <- apply(agg_rets, 1, which.max)
be_st <- rutils::lag_it(be_st)
be_st[1] <- 1
# be_st <- c(rep(1, NROW(end_points)-NROW(be_st)), be_st)
pnl_s <- agg_rets[cbind(1:NROW(agg_rets), be_st)]
plot.zoo(cumsum(pnl_s))


## cum_pnl for multi-manager strategy (simpler version)
cum_pnl <- function(look_back, re_turns) {
  n_row <- NROW(re_turns)
  # define end_points with beginning stub
  num_agg <- n_row %/% look_back
  end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  len_gth <- NROW(end_points)
  # start_points are single-period lag of end_points
  start_points <- end_points[c(1, 1:(len_gth-1))] + 1
  fix_points <- (start_points > end_points)
  start_points[fix_points] <- end_points[fix_points]
  # total re_turns aggregated over non-overlapping windows
  re_turns <- apply(re_turns, 2, function(x) (x[end_points]-x[start_points]))
  # switch to best manager with biggest total re_turns
  be_st <- apply(re_turns, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnl_s <- re_turns[cbind(1:NROW(re_turns), be_st)]
  sum(re_turns[cbind(1:NROW(re_turns), be_st)])
}  # end cum_pnl

cum_pnl(look_back=100, re_turns=cum_pnls)


## cum_pnl for trend-following multi-manager strategy (without end_points)
cum_pnl <- function(look_back, re_turns, cum_pnls) {
  # n_row <- NROW(re_turns)
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(cum_pnls, 2, rutils::diff_it, lag=look_back)
  # switch to best manager with biggest total re_turns
  be_st <- apply(agg_rets, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnl_s <- re_turns[cbind(1:NROW(re_turns), be_st)]
  sum(re_turns[cbind(1:NROW(re_turns), be_st)])
}  # end cum_pnl

# calculate cumulative returns
cum_pnls <- apply(re_turns, 2, cumsum)
cum_pnl(look_back=100, re_turns=re_turns, cum_pnls=cum_pnls)


## perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
pnl_s <- sapply(look_backs, cum_pnl, se_lect=1, re_turns=re_turns, cum_pnls=cum_pnls)
pnl_s <- cbind(look_backs, pnl_s)
plot(pnl_s, t="l")
# plot(cumsum(pnl_s), t="l")


## pre-calculate row order indices for a vector of look_backs
# perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
order_stats <- lapply(look_backs, function(look_back) {
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(cum_pnls, 2, rutils::diff_it, lag=look_back)
  or_der <- t(apply(agg_rets, 1, order))
  or_der <- rutils::lag_it(or_der)
  or_der[1, ] <- 1
  or_der
})  # end lapply
names(order_stats) <- look_backs


## cum_pnl for long-short multi-manager strategy (without end_points)
cum_pnl <- function(select_best=NULL, select_worst=NULL, re_turns, or_der) {
  n_row <- NROW(re_turns)
  if(!is.null(select_best)) {
    n_col <- NCOL(re_turns)
    be_st <- or_der[, (n_col-select_best+1):n_col]
    be_st <- cbind(1:n_row, be_st)
  } else {
    be_st <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    wor_st <- or_der[, 1:select_worst]
    wor_st <- cbind(1:n_row, wor_st)
  } else {
    wor_st <- NULL
  }  # end if
  # return total expected pnl
  # pnl_s <- re_turns[be_st]-re_turns[wor_st]
  sum(re_turns[be_st])/select_best-sum(re_turns[wor_st])/(if(is.null(select_worst)) 1)
}  # end cum_pnl

# calculate pnl for long-short multi-manager strategy
cum_pnl(select_best=1, select_worst=1, re_turns=re_turns, or_der=order_stats[[5]])


## perform loop over lookback windows
pnl_s <- sapply(order_stats, cum_pnl, select_best=1, select_worst=1, re_turns=re_turns)
pnl_s <- cbind(look_backs, pnl_s)
plot(pnl_s, t="l")
# plot(cumsum(pnl_s), t="l")


num_managers <- 5
dri_ft <- sapply(1:num_managers, function(x)
  mea_n*sin(ra_te*(1:n_row)/n_row + 2*pi*x/num_managers))
set.seed(1121)  # reset random number generator
re_turns <- matrix(vol_at*rnorm(num_managers*n_row) - vol_at^2/2, nc=num_managers) + dri_ft
# calculate cumulative returns
cum_pnls <- apply(re_turns, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
look_backs <- 20*(1:50)
order_stats <- lapply(look_backs, function(look_back) {
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(cum_pnls, 2, rutils::diff_it, lag=look_back)
  or_der <- t(apply(agg_rets, 1, order))
  or_der <- rutils::lag_it(or_der)
  or_der[1, ] <- 1
  or_der
})  # end lapply
names(order_stats) <- look_backs

## cum_pnl for long-short multi-manager strategy (without end_points)
cum_pnl <- function(select_best=NULL, select_worst=NULL, re_turns, or_der) {
  n_row <- NROW(re_turns)
  if(!is.null(select_best)) {
    n_col <- NCOL(re_turns)
    be_st <- or_der[, (n_col-select_best+1):n_col]
    be_st <- cbind(1:n_row, be_st)
  } else {
    be_st <- NULL
  }  # end if
  if(!is.null(select_worst)) {
    wor_st <- or_der[, 1:select_worst]
    wor_st <- cbind(1:n_row, wor_st)
  } else {
    wor_st <- NULL
  }  # end if
  # return total expected pnl
  # pnl_s <- re_turns[be_st]-re_turns[wor_st]
  sum(re_turns[be_st])-sum(re_turns[wor_st])
}  # end cum_pnl

# calculate pnl for long-short multi-manager strategy
# cum_pnl(select_best=1, select_worst=1, re_turns=re_turns, or_der=order_stats[[5]])

# perform loop over lookback windows
pnl_s <- sapply(order_stats, cum_pnl, select_best=1, select_worst=NULL, re_turns=re_turns)
pnl_s <- cbind(look_backs, pnl_s)
# par(mar=c(1, 1, 1, 1), oma=c(1, 1, 1, 1))
# plot(pnl_s, t="l", main="Trend-following PnL, as function of lookback window")


## double the dri_ft
set.seed(1121)  # reset random number generator
re_turns <- matrix(vol_at*rnorm(num_managers*n_row) - vol_at^2/2, nc=num_managers) + 2*dri_ft
# calculate cumulative returns
cum_pnls <- apply(re_turns, 2, cumsum)

## pre-calculate row order indices for a vector of look_backs
order_stats_2x <- lapply(look_backs, function(look_back) {
  # total re_turns aggregated over overlapping windows
  agg_rets <- apply(cum_pnls, 2, rutils::diff_it, lag=look_back)
  or_der <- t(apply(agg_rets, 1, order))
  or_der <- rutils::lag_it(or_der)
  or_der[1, ] <- 1
  or_der
})  # end lapply
names(order_stats_2x) <- look_backs

plot.zoo(cbind(pnl_s[, 2], pnls_2x), main="Long-short Ensemble PnL, as function of lookback window",
         lwd=2, xaxt="n", xlab="lookback windows", ylab="PnL", plot.type="single", col=c("black", "red"))
# add x-axis
axis(1, seq_along(look_backs), look_backs)
# add legend
legend(x="top", legend=paste0("SR=", c(0.4, 0.8)),
       inset=0.0, cex=0.8, bg="white",
       lwd=6, lty=c(1, 1), col=c("black", "red"))




## parallel version with loops - much slower and more complicated
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(num_cores-1)

foo <- sapply(look_backs, function(look_back) {
  # define end_points with beginning stub
  num_agg <- n_row %/% look_back
  end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
  len_gth <- NROW(end_points)
  # start_points are single-period lag of end_points
  start_points <- end_points[c(1, 1:(len_gth-1))] + 1
  # redefine end_points
  end_points <- cbind(start_points, end_points)

  # perform parallel loop over re_turns
  clusterExport(clus_ter, varlist=c("len_gth", "end_points"))
  sharpe_ratios <- parApply(clus_ter, MARGIN=2, re_turns, function(re_turns) {
    sapply(2:len_gth, function(in_dex) {
      x_ts <- re_turns[end_points[in_dex, 1]:end_points[in_dex, 2]]
      # calculate annualized Sharpe ratio of returns
      sum(x_ts)/sd(x_ts)
    })  # end sapply
  })  # end parApply

  # sharpe_ratios <- rutils::do_call(cbind, sharpe_ratios)
  sharpe_ratios[which(is.na(sharpe_ratios), arr.ind=TRUE)] <- 1

  # calculate dispersion of SRs
  # c(by_strategy=mean(apply(sharpe_ratios, 1, sd)),
  #   by_period=mean(apply(sharpe_ratios, 2, sd)))
  # diff_sr <- apply(sharpe_ratios, 2, rutils::diff_it) / sharpe_ratios
  # mean(abs(diff_sr))
  # c(by_strategy=mean(apply(sharpe_ratios, 1, sd)),
  #   by_period=mean(apply(diff_sr, 1, sd)))
  cum_pnl(sharpe_ratios, re_turns, end_points)
})  # end sapply

foo <- t(foo)
dim(foo)
foo
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(foo[, 1]/foo[, 2], t="l")

## end perform loop over lookback windows




###############
### portfolio optimization using quadratic solver

load("C:/Develop/data/etf_data.RData")
ls(env_etf)
dim(env_etf$re_turns)
colnames(env_etf$re_turns)



## perform standard calibration over oh_lc interval
op_tim <- optim(par=rep(0.5, 2*NCOL(de_sign)),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=rep(2, 2*NCOL(de_sign)),
                lower=rep(-2, 2*NCOL(de_sign)),
                de_sign=de_sign[in_dex],
                re_turns=returns_running[in_dex],
                lamb_da=lamb_da)

beta_s <- op_tim$par
names(beta_s) <- c(paste0(colnames(de_sign), "_long"), paste0(colnames(de_sign), "_short"))


## cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(beta_s, la_g=15, de_sign=de_sign, re_turns=returns_running, lamb_da=0) {
  n_col <- NCOL(de_sign)
  position_s <- rep.int(NA, NROW(de_sign))
  position_s[1] <- 0
  # buy signal
  bu_y <- (de_sign %*% beta_s[1:n_col] < -1)
  position_s[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lag_it(bu_y, lag=la_g))
  # sell signal
  position_s[se_ll] <- -1.0
  position_s[bu_y] <- 1.0
  position_s <- zoo::na.locf(position_s)
  position_s <- c(0, position_s[-NROW(position_s)])
  # pnl_s <- position_s*re_turns
  # be_ta <- (sum(pnl_s * re_turns) - sum(pnl_s) * sum(re_turns)) / (sum(pnl_s * pnl_s) - sum(pnl_s)^2 )
  # -(exp(sum(pnl_s) - be_ta * sum(re_turns)) - 1)
  # -(exp(sum(position_s*re_turns))-1) # / (sum(abs(rutils::diff_it(position_s))) / 2/ 1e5) / abs(sum(position_s>0) - sum(position_s<0))
  -((exp(sum(position_s*re_turns))-1) - lamb_da*sum(abs(beta_s)))
}  # end cum_pnl

cum_pnl(beta_s=beta_s, de_sign=de_sign[date_s], re_turns=returns_running[date_s])

# perform calibration over oh_lc interval
op_tim <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, NCOL(de_sign)),
                           lower=rep(-2, NCOL(de_sign)),
                           de_sign=de_sign[in_dex],
                           re_turns=returns_running[in_dex],
                           lamb_da=lamb_da,
                           control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))


beta_s <- op_tim$optim$bestmem
names(beta_s) <- colnames(de_sign)
# names(beta_s) <- colnames(de_sign)
op_tim$optim$bestval
cum_pnl(beta_s, de_sign=de_sign[in_dex])


bu_y <- (de_sign %*% beta_s[1:n_col] < -1)

cum_pnl <- function(inter_val) {
  position_s <- rep.int(NA, NROW(de_sign))
  position_s[1] <- 0
  position_s[bu_y] <- 1.0
  se_ll <- as.logical(rutils::lag_it(bu_y, lag=inter_val))
  position_s[se_ll] <- -1.0
  position_s[bu_y] <- 1.0
  position_s <- zoo::na.locf(position_s)
  position_s <- c(0, position_s[-NROW(position_s)])
  exp(sum((position_s * returns_running)))-1
}  # end cum_pnl

cum_pnl(200)
sapply(20*(1:30), cum_pnl)



###############
### convert correlation matrix into distance object
dis_tance <- xts::.index(vol_spikes)
dis_tance <- abs(outer(X=dis_tance, Y=dis_tance, FUN="-"))
# dis_tance <- rutils::diff_it(xts::.index(vol_spikes))
dis_tance <- as.dist(dis_tance)
# Perform hierarchical clustering analysis
clus_ter <- hclust(dis_tance)
plot(clus_ter, ann=FALSE, xlab="", ylab="")
title("clustering of vol_spikes", line=0.0)
foo <- cutree(clus_ter, h=2000)
# foo <- cutree(clus_ter, k=100)
NROW(vol_spikes)
NROW(foo)
NROW(unique(foo))
tail(foo)
tail(vol_spikes)
bar <- match(index(vol_spikes), index(vari_ance))
tail(bar)



hc <- hclust(dist(USArrests))
plot(hc)
cutree(hc, k=5)
cutree(hc, h=50)

returns_future <- rutils::roll_sum(returns_running, look_back=5)
returns_future <- rutils::lag_xts(returns_running, lag=-5)
colnames(returns_future) <- "returns_future"
foo <- lm(returns_future["2008"] ~ de_sign["2008"] - 1)
summary(foo)


##

16*sd(rutils::env_etf$re_turns[, "VTI"])
sqrt(250)
250/5

# Summary: Create a functional which aggregates
# asset returns over lookback and look-forward
# intervals.


# define functional

# 1. (20pts) Create a functional called roll_agg(),

# should perform only a single


###############
###
# 4. (20pts) Create a scatterplot of returns and forward returns
# Create a scatterplot of alphas for "2008" and "2009",
# and add labels with ETF names,
# use columns of "alphas_capm" and functions plot() and text(),

dim(fwd_rets)
dim(cum_pnls)

foo <- na.omit(merge(fwd_rets[, 5], cum_pnls[, 5]))
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
# add regression line
abline(mo_del, lwd=2, col="red")


# select weight_s proportional to cum_pnls
dim(cum_pnls)
weight_s <- coredata(cum_pnls[index(fwd_rets)])
weight_s <- weight_s/sqrt(rowSums(weight_s^2))

# bar <- matrixStats::rowMaxs(weight_s)
bar <- coredata(fwd_rets)
dim(bar)

# select best and worst models in each period
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, function(x) {which.min(x)})
bes_t <- apply(weight_s, 1, which.max)
wors_t <- apply(weight_s, 1, which.min)

back_test <- rowSums(weight_s * bar)
x11()
plot(cumsum(back_test), t="l")

# back_test <- t(weight_s) %*% bar
back_test <- rowSums(weight_s * bar)
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
#  x_ts - an xts series containing one or more columns of data,
#  end_points - integer vector of end points,
#  look_back - number of intervals in the lookback window,
#  FUN - name of of an aggregation function,
#  "..." - optional dots arguments to FUN.

# The functional roll_agg() should perform an lapply()
# loop over end_points, subset the x_ts series, and pass
# it to FUN, together with the dots "..." argument.
# roll_agg() should return an xts series, with each
# row equal to the vector returned by FUN.
# hint: You can adapt code from the slide:
# Performing Aggregations Over Overlapping Intervals.

roll_agg <- function(x_ts, end_points, look_back, FUN, ...) {
  len_gth <- NROW(end_points)
  # start_points are multi-period lag of end_points
  start_points <-  end_points[c(rep_len(1, look_back-1), 1:(len_gth-look_back+1))]
  # perform lapply() loop over length of end_points
  agg_s <- lapply(2:len_gth,
                  function(in_dex) {
                    FUN(x_ts[start_points[in_dex]:end_points[in_dex]], ...)
                  })  # end lapply
  # rbind list into single xts or matrix
  agg_s <- rutils::do_call_rbind(agg_s)
  if (!is.xts(agg_s))
    agg_s <- xts(agg_s, order.by=index(x_ts[end_points]))
  agg_s
}  # end roll_agg


# 2. (20pts) Create an aggregation function called
# agg_regate(), which calls the function simu_ewma()
# and calculates the Sharpe ratios of the EWMA strategy,
# for a given vector of lambdas.
# agg_regate() should accept three arguments:
#  oh_lc - an OHLC series containing four columns of data,
#  lamb_das - integer vector of lambda parameters,
#  "..." - additional dots arguments to be passed to simu_ewma().
# hint: You can adapt code from the slide:
# Simulating Multiple EWMA Strategies

agg_regate <- function(oh_lc, lamb_das, ...) {
  sapply(lamb_das, function(lamb_da) {
    # Simulate EWMA strategy and calculate Sharpe ratio
    re_turns <- simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, ...)[, "re_turns"]
    sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
  })  # end sapply
}  # end agg_regate

# Source the function simu_ewma() from the file
# ewma_model.R, using function source().
# Download the latest version from NYU Classes.

source("C:/Develop/R/scripts/ewma_model.R")

# Define oh_lc series, the EWMA look_back, and lamb_das.

library(HighFreq)
oh_lc <- rutils::env_etf$VTI["/2011"]
look_back <- 51
lamb_das <- seq(0.001, 0.01, 0.001)

# Call agg_regate() as follows:
agg_regate(oh_lc, lamb_das, look_back=look_back)

# You should get the following output:
#  [1] 0.1220623 0.1620571 0.1887122 0.2399056 0.2308350 0.1594881 0.1702486 0.1539695 0.1136539
# [10] 0.1180002


# 3. (20pts) Apply the functional roll_agg() to roll
# the function simu_ewma() over overlapping 12-month
# intervals in the past.

# Define end points at the end of each month.
# Use function endpoints() from package xts.

end_points <- xts::endpoints(oh_lc, on="months")
len_gth <- NROW(end_points)

# Define number of monthly intervals per lookback interval:
look_back <- 12

# Note that there are two different windows in this simulation.
# The first window is the EWMA window, called look_back and equal
# to 51 by default.
# The second window is the lookback interval, called look_back.
# To avoid an error, the end_points should be greater than
# the EWMA look_back, except for the first end_points, which
# should be equal to zero.
# Adjust the end_points so that they are greater than the
# EWMA look_back.

end_points[(end_points > 0) & (end_points <= look_back)] <- look_back+1

# Run roll_agg() as follows:

sharpe_ratios <- roll_agg(x_ts=oh_lc,
                          end_points=end_points,
                          look_back=look_back,
                          FUN=agg_regate,
                          lamb_das=lamb_das,
                          look_back=look_back)

# You should get the following output:
# > sharpe_ratios[1:6, 1:5]
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

var_1 <- sum(pc_1*pc_1)
# make re_turns orthogonal to pc1
in_dex <- index(re_turns)
re_turns <- apply(re_turns, MARGIN=2,
                  function(x) {x - sum(pc_1*x)*pc_1/var_1})
# apply(re_turns, MARGIN=2, function(x) sum(pc_1*x)) # verify orthogonality

###

x11()
foo <- seq(0, 2*pi, length.out=24)
plot(x=cos(foo), y=sin(foo), asp=1)
abline(a=0, b=-0.1, col="red")
abline(a=0, b=10, col="blue")


###

heatmap(sharpe_ratios, col=colorRampPalette(c("blue", "red"))(22))

summary(microbenchmark(
  tee=-t(portf_rets) %*% portf_rets,
  s_um=-sum(portf_rets*portf_rets),
  times=10))[, c(1, 4, 5)]


###

w_1 <- sqrt(0.5); w_2 <- w_1
foo <- matrix(c(w_1, w_2, -w_2, w_1), nc=2)
t(foo) %*% foo
# bar <- re_turns %*% t(solve(foo))
(t(bar) %*% bar) / NROW(bar)

cov_mat <- function(re_turns, an_gle=0) {
  w_1 <- cos(an_gle)
  w_2 <- sin(an_gle)
  mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
  compo_nents <- re_turns %*% t(mat_rix)
  (t(compo_nents) %*% compo_nents) / NROW(compo_nents)
}  # end cov_mat

bar <- cov_mat(re_turns, an_gle=pi/4)
(t(re_turns) %*% re_turns) / NROW(re_turns)
(t(bar) %*% bar) / NROW(bar)

angle_s <- seq(0, pi/2, by=pi/24)
cov_mat <- sapply(angle_s, function(an_gle)
  cov_mat(re_turns, an_gle=an_gle)[1, 1])
plot(x=angle_s, y=cov_mat, t="l")

op_tim <- optimize(
  f=function(an_gle)
    -cov_mat(re_turns, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- op_tim$minimum
bar <- cov_mat(re_turns, an_gle=an_gle)
tan(an_gle)

w_1 <- cos(an_gle)
w_2 <- sin(an_gle)
mat_rix <- matrix(c(w_1, -w_2, w_2, w_1), nc=2)
compo_nents <- re_turns %*% t(mat_rix)
(t(compo_nents) %*% compo_nents) / NROW(compo_nents)

plot(x=compo_nents[, 1], y=compo_nents[, 2],
     xlim=c(-10, 10), ylim=c(-10, 10))

mo_del <- lm(reg_formula, data=re_turns)
# get regression coefficients
coef(summary(mo_del))

foo <- cbind(rnorm(1000, sd=0.2), rnorm(1000)) %*% t(mat_rix)
(t(foo) %*% foo) / NROW(foo)
plot(x=foo[, 1], y=foo[, 2])
summary(lm(foo[, 1] ~ foo[, 2]))

op_tim <- optimize(
  f=function(an_gle)
    -cov_mat(foo, an_gle=an_gle)[1, 1],
  interval=range(angle_s))
an_gle <- op_tim$minimum
tan(an_gle)

###

library(plotly)

df <- data.frame(Date=seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by="days"),
                 Value=sample(100:200, size=244, replace=T))

plot_ly(data=df, x=df$Date, y=df$Value, type="scatter", mode="lines") %>%
  add_trace(x=~df$Date, y=~df$Value, name="20yr Treasury rate") %>%
  layout(xaxis=list(range=c( as.numeric(max(df$Date)-30) *86400000,
                                 as.numeric(max(df$Date)) * 86400000   ),
                      rangeslider=list(type="date")  ))

###



