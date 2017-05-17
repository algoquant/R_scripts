### Simulate several managers, with only one manager with skill.
# The remaining managers underperform slightly, so that average performance is zero.
# In each period select the best performing manager.
# Does this strategy always outperform selecting a manager at random?
# Demonstrate that out-of-sample performance pnl increases with the length of the lookback window.
# Demonstrate that out-of-sample performance decreases with greater number of managers.
# Calculate the p-values in each period, and demonstrate that they are meaningless.

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
set.seed(1121)  # reset random number generator
vol_at <- 0.01
re_turns <- sapply(mean_s, rnorm, n=n_row, sd=vol_at)
re_turns <- apply(re_turns, 2, cumsum)
# apply(re_turns, 2, mean)
# plot.zoo(re_turns, plot.type="single")

# length of look-back window
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


### cum_pnl for multi-manager strategy
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
  }
  else {
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



### cum_pnl for multi-manager strategy (simpler version)
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



### cum_pnl for multi-manager strategy (simplest version)
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

# perform loop over look-back windows
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



### simulation of asset returns, with a time-dependent drift plus a random noise.

# define daily volatility and growth rate
vol_at <- 0.01
n_row <- 5000
path_s <- 3
ra_te <- 4*pi
# dri_ft <- 0.01*sin(ra_te*(1:n_row)/n_row)
# re_turns <- xts(vol_at*rnorm(n_row) + dri_ft - vol_at^2/2, 
#                 order.by=seq.Date(Sys.Date()-n_row+1, Sys.Date(), by=1))
# chart_Series(x=re_turns, name="Multiple paths of geometric Brownian motion")

# simulate multiple paths of geometric Brownian motion
dri_ft <- sapply(1:path_s, function(x) 
  vol_at*sin(ra_te*(1:n_row)/n_row + 2*pi*x/path_s))
plot.zoo(dri_ft, plot.type="single")

# dri_ft <- rutils::do_call(c, lapply(1:path_s, function(x) (dri_ft + 2*pi*x/path_s)))
re_turns <- matrix(vol_at*rnorm(path_s*n_row) - vol_at^2/2, nc=path_s) + dri_ft
# re_turns <- exp(matrixStats::colCumsums(re_turns))
# create zoo time series
re_turns <- xts(re_turns, order.by=seq.Date(Sys.Date()-NROW(re_turns)+1, Sys.Date(), by=1))
# plot zoo time series
col_ors <- colorRampPalette(c("red", "blue"))(NCOL(re_turns))
# col_ors <- col_ors[order(order(re_turns[NROW(re_turns), ]))]
# par(mar=c(3, 3, 2, 2), oma=c(0, 0, 0, 0))
# plot.zoo(re_turns, main="Multiple paths of geometric Brownian motion", xlab=NA, ylab=NA, plot.type="single", col=col_ors)

plot_theme <- chart_theme()
plot_theme$col$line.col <- col_ors
chart_Series(re_turns, theme=plot_theme, name="Multiple paths of geometric Brownian motion")


### calculate pnl over look-back window
# calculate cumulative returns
cum_rets <- apply(re_turns, 2, cumsum)
# length of look-back window
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
agg_rets <- apply(cum_rets, 2, function(x) (x[end_points]-x[start_points]))

# switch to best manager with biggest total re_turns
be_st <- apply(agg_rets, 1, which.max)
be_st <- rutils::lag_it(be_st)
be_st[1] <- 1
# be_st <- c(rep(1, NROW(end_points)-NROW(be_st)), be_st)
pnl_s <- agg_rets[cbind(1:NROW(agg_rets), be_st)]
plot.zoo(cumsum(pnl_s))


### cum_pnl from cumulative returns for multi-manager strategy (simpler version)
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

cum_pnl(look_back=100, re_turns=cum_rets)


### cum_pnl from cumulative returns for multi-manager strategy (without end_points)
cum_pnl <- function(look_back, re_turns, cum_rets) {
  # n_row <- NROW(re_turns)
  # total re_turns aggregated over non-overlapping windows
  agg_rets <- apply(cum_rets, 2, rutils::diff_it, lag=look_back)
  # switch to best manager with biggest total re_turns
  be_st <- apply(agg_rets, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  # return total expected pnl
  # pnl_s <- re_turns[cbind(1:NROW(re_turns), be_st)]
  sum(re_turns[cbind(1:NROW(re_turns), be_st)])
}  # end cum_pnl

cum_pnl(look_back=100, re_turns=re_turns, cum_rets=cum_rets)


### perform loop over look-back windows
# length of look-back window
look_backs <- 10*(1:100)
foo <- sapply(look_backs, cum_pnl, re_turns=re_turns, cum_rets=cum_rets)
foo <- cbind(look_backs, foo)
plot(foo, t="l")
plot(cumsum(pnl_s), t="l")



### parallel version with loops - much slower and more complicated
# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)

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

### end perform loop over look-back windows


### simulation of trading strategy

### cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(sharpe_ratios, re_turns, end_points) {
  be_st <- apply(sharpe_ratios, 1, which.max)
  be_st <- rutils::lag_it(be_st)
  be_st[1] <- 1
  re_turns <- lapply(seq_along(be_st), function(in_dex) {
    re_turns[end_points[in_dex+1, 1]:end_points[in_dex+1, 2], be_st[in_dex]]
  })  # end lapply
  sum(rutils::do_call(rbind, re_turns))
}  # end cum_pnl

cum_pnl(sharpe_ratios, re_turns, end_points)

# switch to best asset with biggest SR
be_st <- apply(sharpe_ratios, 1, which.max)
be_st <- rutils::lag_it(be_st)
be_st[1] <- 1
bar <- lapply(seq_along(be_st), function(in_dex) {
  re_turns[end_points[in_dex+1, 1]:end_points[in_dex+1, 2], be_st[in_dex]]
})  # end lapply
bar <- rutils::do_call(rbind, bar)

chart_Series(x=cumsum(bar), name="Back-test of SR strategies")

### simulation for determining the optimal length of the lookback interval

library(HighFreq)
options(max.print=40)
oh_lc <- HighFreq::SPY["/2008-03"]
in_dex <- index(oh_lc)
n_row <- NROW(oh_lc)

# calculate close to close percentage returns
cl_ose <- Cl(oh_lc)
re_turns <- 60*HighFreq::run_returns(x_ts=HighFreq::SPY)

# define aggregation window and decay parameter
win_dow <- 51
lamb_da <- 0.05
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:win_dow)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
ew_ma <- xts(ew_ma, order.by=index(oh_lc))
colnames(ew_ma) <- "VTI EWMA"

# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
po_sitions <- rep(NA_integer_, NROW(cl_ose))
po_sitions[1] <- 0
po_sitions[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
po_sitions <- na.locf(po_sitions)
po_sitions <- xts(po_sitions, order.by=index(oh_lc))

prices_lag <- rutils::lag_xts(cl_ose)
position_lagged <- rutils::lag_xts(po_sitions)
# calculate daily profits and losses
re_turns <- position_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  po_sitions[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]), pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")


# define function for simulating EWMA crossover strategy
simu_ewma <- function(oh_lc, lamb_da=0.05, win_dow=51) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:win_dow)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(win_dow-1)] <- ew_ma[win_dow]
  # determine dates right after EWMA has crossed prices
  in_dic <- xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_xts(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  po_sitions <- rep(NA_integer_, NROW(cl_ose))
  po_sitions[1] <- 0
  po_sitions[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
  po_sitions <- xts(na.locf(po_sitions), order.by=index(oh_lc))
  op_en <- Op(oh_lc)
  prices_lag <- rutils::lag_xts(cl_ose)
  position_lagged <- rutils::lag_xts(po_sitions)
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <-
    position_lagged[trade_dates] *
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    po_sitions[trade_dates] *
    (cl_ose[trade_dates] - op_en[trade_dates])
  out_put <- cbind(po_sitions, re_turns)
  colnames(out_put) <- c("po_sitions", "re_turns")
  out_put
}  # end simu_ewma


# perform parallel loop over lamb_das
lamb_das <- seq(0.001, 0.03, 0.001)
lamb_das <- seq(0.01, 1.0, 0.1)

# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, varlist=c("oh_lc", "win_dow", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
re_turns <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, win_dow=win_dow)[, "re_turns"]
})  # end parLapply


# set up loop over look-back windows
# length of look-back window
# look_back <- 11
# define end_points with beginning stub
end_points <- endpoints(oh_lc, on="days")
# num_agg <- n_row %/% look_back
# end_points <- c(0, n_row-look_back*num_agg+look_back*(0:num_agg))
len_gth <- NROW(end_points)
# start_points are single-period lag of end_points
start_points <- end_points[c(1, 1:(len_gth-1))] + 1
# redefine end_points
end_points <- cbind(start_points, end_points)

# perform parallel loop over re_turns
clusterExport(clus_ter, varlist=c("len_gth", "end_points"))
sharpe_ratios <- parLapply(clus_ter, re_turns, function(re_turns) {
  sapply(2:len_gth, function(in_dex) {
    x_ts <- re_turns[end_points[in_dex, 1]:end_points[in_dex, 2]]
    # calculate annualized Sharpe ratio of returns
    sqrt(260)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
  })  # end sapply
})  # end parLapply

sharpe_ratios <- rutils::do_call(cbind, sharpe_ratios)

# calculate dispersion of SRs of individual strategies over time periods
apply(sharpe_ratios, 2, sd)
# calculate dispersion of SRs of strategies in each time period
foo <- apply(sharpe_ratios, 1, sd)
mean(foo)

# calculate differences of SRs over periods
foo <- apply(sharpe_ratios, 2, rutils::diff_it)
dim(foo)
dim(sharpe_ratios)
tail(foo)
tail(sharpe_ratios)

# are the sharpe_ratios autocorrelated?
# yes, about -50%
bar <- foo[-NROW(foo), ]
bar <- rbind(rep(0, NCOL(foo)), bar)
bar <- bar*foo
colSums(bar) / apply(foo, 2, sd) / NROW(bar)

# switch to best strategy
bar <- apply(sharpe_ratios, 1, which.max)

# switch to strategy with biggest differences of SRs over periods
bar <- apply(foo, 1, which.max)
bar <- rutils::lag_it(bar)
bar[1] <- 1
bar <- lapply(2:len_gth, function(in_dex) {
  re_turns[[bar[in_dex-1]]][end_points[in_dex, 1]:end_points[in_dex, 2]]
})  # end lapply
bar <- rutils::do_call(rbind, bar)

# average over all strategies
bar <- rutils::do_call(cbind, re_turns)
bar <- xts(rowSums(bar), order.by=index(re_turns[[1]]))

chart_Series(x=-cumsum(bar), name="Back-test of EWMA strategies")


### perform loop over look-back windows
# length of look-back window
look_backs <- 50*(5:30)

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
  sharpe_ratios <- parLapply(clus_ter, re_turns, function(re_turns) {
    sapply(2:len_gth, function(in_dex) {
      x_ts <- re_turns[end_points[in_dex, 1]:end_points[in_dex, 2]]
      # calculate annualized Sharpe ratio of returns
      sqrt(260)*sum(x_ts)/sd(x_ts)/NROW(x_ts)
    })  # end sapply
  })  # end parLapply
  
  sharpe_ratios <- rutils::do_call(cbind, sharpe_ratios)
  sharpe_ratios[which(is.na(sharpe_ratios), arr.ind=TRUE)] <- 1
  
  # calculate dispersion of SRs
  c(by_strategy=mean(apply(sharpe_ratios, 2, sd)), 
    by_period=mean(apply(sharpe_ratios, 1, sd)))
})  # end sapply

foo <- t(foo)
dim(foo)
foo
plot(foo[, 1]/foo[, 2], t="l")

### end perform loop over look-back windows


# stop R processes over cluster under Windows
stopCluster(clus_ter)


### portfolio optimization using quadratic solver

load("C:/Develop/data/etf_data.RData")
ls(env_etf)
dim(env_etf$re_turns)
colnames(env_etf$re_turns)



### perform standard calibration over oh_lc interval
op_tim <- optim(par=rep(0.5, 2*NCOL(SPY_design)),
                fn=cum_pnl,
                method="L-BFGS-B",
                upper=rep(2, 2*NCOL(SPY_design)),
                lower=rep(-2, 2*NCOL(SPY_design)), 
                de_sign=SPY_design[in_dex], 
                re_turns=returns_running[in_dex],
                lamb_da=lamb_da)

beta_s <- op_tim$par
names(beta_s) <- c(paste0(colnames(SPY_design), "_long"), paste0(colnames(SPY_design), "_short"))


### cum_pnl vectorized function for contrarian strategy with threshold
cum_pnl <- function(beta_s, la_g=15, de_sign=SPY_design, re_turns=returns_running, lamb_da=0) {
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

cum_pnl(beta_s=beta_s, de_sign=SPY_design[ran_ge], re_turns=returns_running[ran_ge])

# perform calibration over oh_lc interval
op_tim <- DEoptim::DEoptim(fn=cum_pnl,
                           upper=rep(2, NCOL(SPY_design)),
                           lower=rep(-2, NCOL(SPY_design)), 
                           de_sign=SPY_design[in_dex], 
                           re_turns=returns_running[in_dex],
                           lamb_da=lamb_da,
                           control=list(trace=FALSE, itermax=500, parallelType=1, packages="rutils"))


beta_s <- op_tim$optim$bestmem
names(beta_s) <- colnames(SPY_design)
# names(beta_s) <- colnames(SPY_design)
op_tim$optim$bestval
cum_pnl(beta_s, de_sign=SPY_design[in_dex])


bu_y <- (SPY_design %*% beta_s[1:n_col] < -1)

cum_pnl <- function(inter_val) {
  position_s <- rep.int(NA, NROW(SPY_design))
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


###

# convert correlation matrix into distance object
dis_tance <- as.numeric(xts::.index(vol_spikes))
dis_tance <- abs(outer(X=dis_tance, Y=dis_tance, FUN="-"))
# dis_tance <- rutils::diff_it(as.numeric(xts::.index(vol_spikes)))
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
bar <- match(index(vol_spikes), index(var_running))
tail(bar)



hc <- hclust(dist(USArrests))
plot(hc)
cutree(hc, k=5)
cutree(hc, h=50)

returns_future <- rutils::roll_sum(returns_running, win_dow=5)
returns_future <- rutils::lag_xts(returns_running, lag=-5)
colnames(returns_future) <- "returns_future"
foo <- lm(returns_future["2008"] ~ SPY_design["2008"] - 1)
summary(foo)


###

16*sd(rutils::env_etf$re_turns[, "VTI"])
sqrt(250)
250/5

# Summary: Create a functional which aggregates 
# asset returns over look-back and look-forward 
# intervals.


# define functional

# 1. (20pts) Create a functional called roll_agg(), 

# should perform only a single 


#########

# 4. (20pts) Create a scatterplot of returns and forward returns 
# Create a scatterplot of alphas for "2008" and "2009",
# and add labels with ETF names,
# use columns of "alphas_capm" and functions plot() and text(),

dim(fwd_rets)
dim(cum_rets)

foo <- na.omit(merge(fwd_rets[, 5], cum_rets[, 5]))
colnames(foo) <- c("forward_returns", "past_returns")
foo <- as.data.frame(foo)
head(foo)
dim(foo)

x11()
# perform regression
reg_formula <- paste(colnames(foo), collapse=" ~ ")
reg_model <- lm(reg_formula, data=foo)
summary(reg_model)
# plot scatterplot using formula
plot(foo[, 2], foo[, 1], xlab="past returns", ylab="forward returns")
# plot(foo)
title(main="Simple Regression", line=-1)
# add regression line
abline(reg_model, lwd=2, col="red")


# select weight_s proportional to cum_rets
dim(cum_rets)
weight_s <- coredata(cum_rets[index(fwd_rets)])
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

# define look-back windows


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

# Define oh_lc series, the EWMA win_dow, and lamb_das.

library(HighFreq)
oh_lc <- rutils::env_etf$VTI["/2011"]
win_dow <- 51
lamb_das <- seq(0.001, 0.01, 0.001)

# Call agg_regate() as follows:
agg_regate(oh_lc, lamb_das, win_dow=win_dow)

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

# Define number of monthly intervals per look-back interval:
look_back <- 12

# Note that there are two different windows in this simulation.
# The first window is the EWMA window, called win_dow and equal 
# to 51 by default.
# The second window is the look-back interval, called look_back.
# To avoid an error, the end_points should be greater than 
# the EWMA win_dow, except for the first end_points, which 
# should be equal to zero.
# Adjust the end_points so that they are greater than the 
# EWMA win_dow.

end_points[(end_points > 0) & (end_points <= win_dow)] <- win_dow+1

# Run roll_agg() as follows:

sharpe_ratios <- roll_agg(x_ts=oh_lc, 
                          end_points=end_points, 
                          look_back=look_back, 
                          FUN=agg_regate, 
                          lamb_das=lamb_das,
                          win_dow=win_dow)

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
co_var <- sapply(angle_s, function(an_gle) 
  cov_mat(re_turns, an_gle=an_gle)[1, 1])
plot(x=angle_s, y=co_var, t="l")

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

reg_model <- lm(reg_formula, data=re_turns)
# get regression coefficients
coef(summary(reg_model))

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

df <- data.frame(Date = seq(as.Date("2016-01-01"), as.Date("2016-08-31"), by="days"),
                 Value = sample(100:200, size = 244, replace = T))

plot_ly(data = df, x = df$Date, y = df$Value, type = "scatter", mode="lines") %>%
  add_trace(x=~df$Date, y=~df$Value, name="20yr Treasury rate") %>% 
  layout(xaxis = list(range = c( as.numeric(max(df$Date)-30) *86400000,
                                 as.numeric(max(df$Date)) * 86400000   ),
                      rangeslider = list(type = "date")  ))

###



