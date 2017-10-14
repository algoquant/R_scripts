##############################
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



##############################
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




##############################
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


##############################
### exponentiation operator is a function:

'^'(3, 2)
"^"(3, 2)


##############################
### Code to check for duplicate dates

# create xts with duplicate dates
x <- xts(rnorm(5), order.by=c(Sys.Date() + 1:4, Sys.Date() + 2))
diff(index(x))

rutils::diff_it(as.numeric(index(x)))

as.POSIXct(make.index.unique(.index(x)), origin="1970-01-01")


##############################
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



##############################
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



##############################
### Forecast and trade minutely stock returns, using static betas over design matrix

re_turns <- 6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, sca_le=FALSE)
look_back <- 5
rets_lag <- 6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, lag=look_back, sca_le=FALSE)
colnames(rets_lag) <- "rets_lag"
rets_lag2 <- 6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, lag=2*look_back, sca_le=FALSE)
colnames(rets_lag2) <- "rets_lag2"
rets_adv <- rutils::lag_xts(rets_lag, lag=-look_back)
colnames(rets_adv) <- "rets_adv"
rets_adv2 <- rutils::lag_xts(rets_lag2, lag=-2*look_back)
colnames(rets_adv2) <- "rets_adv2"
vari_ance <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY, sca_le=FALSE)
vari_ance <- HighFreq::roll_vwap(oh_lc=HighFreq::SPY, x_ts=vari_ance, look_back=look_back)
colnames(vari_ance) <- "variance"
# var_lag2 <- HighFreq::roll_vwap(oh_lc=HighFreq::SPY, x_ts=vari_ance, look_back=2*look_back)
# colnames(var_lag2) <- "var_lag2"

# sk_ew <- 6.5*60^4*HighFreq::run_skew(oh_lc=HighFreq::SPY)
# sk_ew <- ifelse(vari_ance==0, 0, sk_ew/(vari_ance)^(1.5))
# sk_ew[1, ] <- 0
# sk_ew <- roll_vwap(oh_lc=HighFreq::SPY, x_ts=sk_ew, look_back=2*look_back)
# colnames(sk_ew) <- "skew"
# set plot panels
# par(mfrow=c(2,1))
# chart_Series(HighFreq::SPY["2013-11-15"], name="SPY")
# chart_Series(SPY_design["2013-11-15"], name="position_s")
# plot.zoo(position_s[match(index(HighFreq::SPY["2013-11-15"]), index(HighFreq::SPY))], main="position_s")
# bars with zero skew
# bar_s <- HighFreq::SPY["2013-11-15"][(sk_ew["2013-11-15"]==0)]

# sharp_e <- HighFreq::run_sharpe(oh_lc=HighFreq::SPY)
# sharpe_rolling <- roll_vwap(oh_lc=HighFreq::SPY, x_ts=sharp_e, look_back=look_back)
# sharpe_rolling <- as.numeric(stats::filter(sharp_e, filter=weight_s, sides=2))
# colnames(sharpe_rolling) <- "sharpe"

hu_rst <- roll_hurst(oh_lc=HighFreq::SPY, look_back=look_back)
colnames(hu_rst) <- "hurst"


# rets_lag <- lapply(1:(3*look_back), function(lag) {
#   6.5*60*HighFreq::run_returns(x_ts=HighFreq::SPY, lag=lag, sca_le=FALSE)
# })  # end lapply
rets_lag <- lapply(1:(3*look_back), HighFreq::run_returns,
                          x_ts=HighFreq::SPY, col_umn=4, sca_le=FALSE)
rets_lag <- 6.5*60*rutils::do_call(cbind, rets_lag)
colnames(rets_lag) <- paste0("rets_lag_", 1:(3*look_back))



SPY_design <- cbind(rets_lag2, sharpe_rolling)
# SPY_design <- cbind(rets_lag2, z_scores[[3]], hu_rst, sharpe_rolling)
# colnames(SPY_design) <- c("returns", "variance", "skew", "hurst")
end_days <- xts::endpoints(SPY_design, "days")

# apply rolling centering and scaling to the design matrix
# library(roll)
SPY_design <- roll::roll_scale(data=SPY_design, width=100*look_back, min_obs=1)
# remove NAs
SPY_design[is.na(SPY_design)] <- 0
sum(is.na(SPY_design))


mo_del <- lm(rets_adv2 ~ SPY_design)
summary(mo_del)
coef(summary(mo_del))
beta_s <- -coef(summary(mo_del))[-1, 1]


### calculate indicator from static betas and applly its rolling z-scores

in_dic <- matrix(rowSums(SPY_design %*% beta_s), ncol=1)
# in_dic <- roll::roll_scale(data=in_dic, width=6, min_obs=1)
# in_dic[is.na(in_dic)] <- 0
# regress future returns against z-scores
mo_del <- lm(rets_adv2 ~ in_dic)
summary(mo_del)
# calculate rolling range of z-scores
look_back <- 21
ran_ge <- cbind(min=-runMax(-in_dic, n=look_back),
                max=runMax(in_dic, n=look_back))
ran_ge[1:(look_back-1), ] <- ran_ge[look_back, ]
ran_ge <- rutils::lag_xts(ran_ge)
# calculate position_s and pnls from z-scores and ran_ge
position_s <- ifelse(in_dic > 0.96*ran_ge[, "max"], -1,
                     ifelse(in_dic < 0.96*ran_ge[, "min"], 1, NA))
position_s[1] <- 0
position_s <- na.locf(position_s)
# position_s <- rutils::lag_xts(position_s)
position_s <- lapply(1:3, rutils::lag_xts, x_ts=position_s)
position_s <- rutils::do_call(cbind, position_s)
position_s <- rowSums(position_s)/NCOL(position_s)
cum_pnls <- cumsum(position_s*re_turns)
x11()
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


### calculate the strategy success rate as the pnl divided by asset return volatility (to normalize the asset returns)
# result: the plot of the strategy success rate doesn't show any time variation or dependence on volatility
vari_ance <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY, sca_le=TRUE)
vari_ance <- sqrt(vari_ance)
vari_ance <- HighFreq::roll_vwap(oh_lc=HighFreq::SPY, x_ts=vari_ance, look_back=look_back)
bar <- rutils::diff_xts(cum_pnls, lag=look_back) / vari_ance
bar[1] <- 0
plot.zoo(bar[end_days], main="bar", xlab=NA, ylab=NA)
# the strategy average daily success rate isn't more successful when the volatility is higher
foo <- apply.daily(abs(bar), FUN=sum)
plot.zoo(foo, main="foo", xlab=NA, ylab=NA)


# calculate correlation between strategy pnl_s and vari_ance: there is no correlation
vari_ance <- 6.5*60^3*HighFreq::run_variance(oh_lc=HighFreq::SPY, sca_le=TRUE)
vari_ance <- sqrt(vari_ance)
range(vari_ance)
range(vari_ance[vari_ance > 1e-06])
pnl_s <- position_s*re_turns
mo_del <- lm(pnl_s[vari_ance > 1e-03] ~ vari_ance[vari_ance > 1e-03])
summary(mo_del)
plot(x=as.numeric(vari_ance[vari_ance > 1e-03]), y=as.numeric(pnl_s[vari_ance > 1e-03]))


### calculate the strategy success rate as the product of the forecast position_s times the actual position (return direction)
# result: : there is no significant correlation between the daily average success rate and the level of vari_ance
bar <- apply.daily(position_s*sign(re_turns), FUN=sum)
foo <- apply.daily(vari_ance, FUN=sum)
mo_del <- lm(bar ~ foo)
summary(mo_del)
plot(x=as.numeric(foo), y=as.numeric(bar))
plot.zoo(cbind(foo, cumsum(bar)))



### calculate z-scores and apply them to regression of future returns

# function for calculating z-scores
z_score <- function(width) {
  z_score <- roll::roll_scale(data=HighFreq::SPY[, 4], width=width, min_obs=1)
  z_score[is.na(z_score)] <- 0
  colnames(z_scores) <- paste0("z_width_", width)
  z_score
}  # end z_score

# calculate z-scores for different widths (lookbacks)
width_s <- 4:20
z_scores <- lapply(width_s, z_score)
names(z_scores) <- paste0("z_width_", width_s)
# z_scores <- lapply(names(z_scores), function(x) {
#   colnames(z_scores[[x]]) <- x
#   z_scores[[x]]
# })  # end lapply


# regress future returns against z-scores
t_val <- function(z_scores) {
  mo_del <- lm(rets_adv2 ~ z_scores)
  coef(summary(mo_del))[2, 3]
}  # end t_val

t_vals <- sapply(z_scores, t_val)
# t_vals <- cbind(width_s, t_val)
t(sapply(z_scores, range))


# calculate rolling range of z-scores

range(z_scores[[3]])
ran_ge <- cbind(min=-runMax(-z_scores[[3]], n=look_back),
                max=runMax(z_scores[[3]], n=look_back))
ran_ge[1:(look_back-1), ] <- ran_ge[look_back, ]
ran_ge <- rutils::lag_xts(ran_ge)
# range(ran_ge[, 1])
# plot.zoo(ran_ge[end_days, 1], main="rolling min of z-scores", xlab=NA, ylab=NA)


# calculate position_s and pnls from z-scores and ran_ge

position_s <- ifelse(z_scores[[3]] > 0.96*ran_ge[, "max"], -1,
                  ifelse(z_scores[[3]] < 0.96*ran_ge[, "min"], 1, NA))
position_s[1] <- 0
position_s <- na.locf(position_s)
# position_s <- rutils::lag_xts(position_s)
position_s <- lapply(1:3, rutils::lag_xts, x_ts=position_s)
position_s <- rutils::do_call(cbind, position_s)
position_s <- -rowSums(position_s)/NCOL(position_s)
cum_pnls <- cumsum(position_s*re_turns)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)

# average number of trades per day
sum(abs(rutils::diff_it(position_s))) / mean(abs(position_s)) / 2 / NROW(end_days)
# average holding period (minutes)
2*NROW(position_s) / sum(abs(rutils::diff_it(position_s))) * mean(abs(position_s))


# calculate total pnls from z-scores (dynamic thresh_old)
cum_pnl <- function(z_scores, thresh_old=1.0, look_back=21, lag=3) {
  ran_ge <- cbind(min=-runMax(-z_scores, n=look_back),
                  max=runMax(z_scores, n=look_back))
  ran_ge[1:(look_back-1), ] <- ran_ge[look_back, ]
  ran_ge <- rutils::lag_xts(ran_ge)
  position_s <- ifelse(z_scores > thresh_old*ran_ge[, "max"], -1,
                       ifelse(z_scores < thresh_old*ran_ge[, "min"], 1, NA))
  position_s[1] <- 0
  position_s <- na.locf(position_s)
  position_s <- lapply(1:lag, rutils::lag_xts, x_ts=position_s)
  position_s <- rutils::do_call(cbind, position_s)
  position_s <- rowSums(position_s)/NCOL(position_s)
  cumsum(position_s*re_turns)
}  # end cum_pnl

bar <- cum_pnl(z_scores=z_scores[[3]], thresh_old=0.96, look_back=21, lag=3)
plot.zoo(bar[end_days], main="cum_pnls", xlab=NA, ylab=NA)

# calculate total pnls for different thresh_olds
thresh_olds <- seq(from=0.9, to=1.1, by=0.01)
bar <- lapply(thresh_olds, cum_pnl,
              z_scores=z_scores[[3]],
              look_back=21,
              lag=3)  # end lapply
names(bar) <- paste0("threshold_", thresh_olds)
unlist(lapply(bar, last))

# calculate total pnls for different look_backs
look_backs <- seq(from=11, to=31, by=2)
bar <- lapply(look_backs, cum_pnl,
              z_scores=z_scores[[3]],
              thresh_old=0.96,
              lag=3)  # end lapply
names(bar) <- paste0("look_back_", look_backs)
unlist(lapply(bar, last))



# function for calculating position_s from z-scores (static thresh_old)
z_pos <- function(z_scores, thresh_old=2.0) {
  position_s <- ifelse(abs(z_scores) > thresh_old, sign(z_scores), NA)
  position_s[1] <- 0
  na.locf(position_s)
}  # end z_pos

# calculate time series of pnls from z-scores
position_s <- z_pos(z_scores[[3]], thresh_old=1.4)
position_s <- lapply(1:3, rutils::lag_xts, x_ts=position_s)
position_s <- rutils::do_call(cbind, position_s)
position_s <- rowSums(position_s)/NCOL(position_s)
cum_pnls <- -cumsum(position_s*re_turns)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


# calculate total pnls from z-scores
cum_pnl <- function(z_scores, thresh_old=2.0, lag=3) {
  position_s <- z_pos(z_scores, thresh_old=thresh_old)
  position_s <- lapply(1:lag, rutils::lag_xts, x_ts=position_s)
  position_s <- rutils::do_call(cbind, position_s)
  position_s <- rowSums(position_s)/NCOL(position_s)
  -sum(position_s*re_turns)
}  # end cum_pnl

cum_pnl(z_scores[[3]], thresh_old=1.4, lag=3)

# calculate total pnls for different thresh_olds
thresh_olds <- seq(from=1.0, to=3.0, by=0.1)
bar <- sapply(thresh_olds, cum_pnl,
              z_scores=z_scores[[3]],
              lag=3)
bar <- cbind(thresh_olds, bar)


position_s <- lapply(z_scores, z_pos, thresh_old=1.0)
position_s <- rutils::do_call(cbind, position_s)
position_s <- rutils::lag_xts(position_s, lag=1)


z_rets <- lapply(z_scores, function(z_scores) {
  position_s <- ifelse(abs(z_scores) > 2.0, sign(z_scores), NA)
  position_s[1] <- 0
  position_s <- na.locf(position_s)
  position_s <- rutils::lag_xts(position_s)
  -position_s*re_turns
})  # end lapply

cum_pnls <- lapply(z_rets, cumsum)
cum_pnls <- rutils::do_call(cbind, cum_pnls)

cum_pnls <- rowSums(cum_pnls)
plot.zoo(cum_pnls[end_days], main="cum_pnls", xlab=NA, ylab=NA)


## simulate weighting the best performing strategies

weight_s <- rutils::diff_xts(cum_pnls, lag=1000)
row_sums <- rowSums(abs(weight_s))
row_sums[row_sums==0] <- 1.0
weight_s <- weight_s/row_sums
weight_s <- rutils::lag_xts(weight_s)

barr <- cumsum(rowSums(weight_s*rutils::do_call(cbind, z_rets)))
plot.zoo(barr[end_days], main="cum_pnls")



## perform rolling beta regressions in parallel
lm_roll <- roll::roll_lm(x=SPY_design,
                        y=rets_adv2,
                        width=3000*look_back)
beta_s <- lm_roll$coefficients[, -1]
beta_s[!complete.cases(beta_s), ] <- 0
# sum(is.na(beta_s))
beta_s <- rutils::lag_xts(beta_s, lag=2*look_back)
# beta_s <- na.omit(beta_s[, 2])
chart_Series(x=beta_s[end_days, "rets_lag2"], name="rolling betas")


## perform rolling daily beta regressions
# calculate daily endpoints
end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
# length of lookback window
look_back <- 3000*look_back

# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, varlist=c("look_back", "rets_adv2", "SPY_design"))

# perform parallel loop over daily endpoints - strange results independent of look_back
lm_roll <- parLapply(clus_ter, end_days, function(end_day) {
  in_dex <- max(1, end_day-look_back):end_day
  summary(lm(rets_adv2[in_dex, ] ~ SPY_design[in_dex, ]))
})  # end parLapply

# stop R processes over cluster under Windows
stopCluster(clus_ter)

# perform loop over daily endpoints
lm_roll <- lapply(end_days, function(end_day) {
  in_dex <- max(1, end_day-look_back):end_day
  summary(lm(rets_adv2[in_dex, ] ~ SPY_design[in_dex, ]))
})  # end lapply


t_vals <- sapply(lm_roll, function(x) x$coefficients[-1, 3])
t_vals <- t(t_vals)
t_vals <- xts(t_vals, order.by=index(SPY_design[end_days, ]))
colnames(t_vals) <- colnames(SPY_design)
plot.zoo(cbind(t_vals[, 2], HighFreq::SPY[end_days, 4])["2010", ])

co_ef <- sapply(lm_roll, function(x) x$coefficients[-1, 1])
co_ef <- t(co_ef)
colnames(co_ef) <- colnames(SPY_design)
co_ef <- rutils::lag_it(co_ef)
beta_s <- NA*SPY_design
beta_s[1, ] <- 0
beta_s[end_days, ] <- co_ef
beta_s <- na.locf(beta_s)

# calculate position_s and pnl_s
position_s <- rowSums(SPY_design * beta_s)
# static beta_s work better than rolling regression
# beta_s <- c(rep(-1.0, 5), 0.00)
position_s <- rowSums(SPY_design %*% beta_s)
# position_s <- ifelse(abs(position_s)>0.01, sign(position_s), NA)
# position_s[1] <- 0
# position_s <- na.locf(position_s)
position_s <- rutils::lag_it(position_s)
position_s <- rutils::roll_sum(position_s, look_back=5) / 5
# histo_gram <- hist(position_s, breaks=200, xlim=c(-0.05, 0.05))
# average number of trades per day
sum(abs(rutils::diff_it(position_s))) / mean(abs(position_s)) / 2 / NROW(end_days)
# average holding period (minutes)
2*NROW(position_s) / sum(abs(rutils::diff_it(position_s))) * mean(abs(position_s))
# colnames(position_s) <- "position_s"
# plot.zoo(cbind(position_s[end_days], HighFreq::SPY[end_days, 4])["2010", ])
pnl_s <- cumsum(position_s*re_turns)
colnames(pnl_s) <- "SPY contrarian"
chart_Series(x=pnl_s["2008-01-29/2008-01-31"], name="pnl_s")
chart_Series(x=pnl_s[end_days, ], name="pnl_s")

# apply moving average crossover strategy to resulting pnl_s
# define aggregation window, decay parameter, and calculate VWAP
lamb_da <- 0.01
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:(10*look_back+1))
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(pnl_s, filter=weight_s, sides=1)
ew_ma <- as.numeric(ew_ma)
ew_ma[1:(10*look_back)] <- ew_ma[10*look_back+1]
# calculate VWAP indicator
in_dic <- sign(pnl_s - ew_ma)
# determine dates right after VWAP has crossed prices
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1

# calculate positions, either: -1, 0, or 1
pos_vwap <- rep(NA_integer_, NROW(pnl_s))
pos_vwap[1] <- 0
pos_vwap[trade_dates] <- in_dic[trade_dates]
pos_vwap <- na.locf(pos_vwap)
pos_vwap <- xts(pos_vwap, order.by=index(pnl_s))

# calculate daily profits and losses
pnl_vwap <- cumsum(pos_vwap*rutils::diff_xts(pnl_s))
colnames(pnl_vwap) <- "SPY contrarian plus vwap"

# plot
date_s <- "2010-05-05/2010-05-07"
back_test <- cbind(HighFreq::SPY[, 4], cum_pnls)[date_s, ]
# back_test <- cbind(HighFreq::SPY[, 4], sharpe_rolling)[date_s, ]
back_test[, 1] <- back_test[, 1] - as.numeric(back_test[1, 1])
back_test[, 2] <- back_test[, 2] - as.numeric(back_test[1, 2])
back_test[, 2] <- 3*back_test[, 2] / max(back_test[, 2])

plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
chart_Series(back_test, theme=plot_theme,
             name="SPY contrarian strategy plus vwap")
add_TA(cbind(HighFreq::SPY[, 4], position_s)[date_s, 2] > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(cbind(HighFreq::SPY[, 4], position_s)[date_s, 2] < 0, on=-1,
       col="lightgrey", border="lightgrey")
legend("topleft", legend=c("pnl_s", "pnl_vwap"),
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6),
       col=plot_theme$col$line.col, bty="n")



### Simulating minutely EWMA strategies


# define function for simulating minutely EWMA crossover strategy
simu_ewma <- function(x_ts, lamb_da=0.05, look_back=51) {
  # calculate EWMA prices
  # weight_s <- exp(-lamb_da*(1:look_back))
  # weight_s <- weight_s/sum(weight_s)
  # ew_ma <- as.numeric(stats::filter(x_ts, filter=weight_s, sides=1))
  # ew_ma[1:(look_back-1)] <- ew_ma[look_back]
  ew_ma <- HighFreq::roll_vwap(x_ts, look_back=look_back)
  # determine dates right after EWMA has crossed prices
  in_dic <- sign(as.numeric(x_ts[, 4] - ew_ma))
  trade_dates <- (rutils::diff_it(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(x_ts)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(x_ts))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_it(in_dic)[trade_dates]
  na.locf(position_s)
}  # end simu_ewma

end_days <- xts::endpoints(HighFreq::SPY, "days")[-1]
end_hours <- xts::endpoints(HighFreq::SPY, "hours")[-1]
positions_hours <- simu_ewma(x_ts=HighFreq::SPY[end_hours], lamb_da=0.01, look_back=1001)
position_s <- rep(NA_integer_, NROW(HighFreq::SPY))
position_s[1] <- 0
position_s[end_hours] <- positions_hours
position_s <- na.locf(position_s)
chart_Series(-cumsum(position_s*re_turns)[end_days], name="SPY minutely vwap strategy")
position_s <- xts(position_s, order.by=index(re_turns))
add_TA(position_s > 0, on=-1,
       col="lightgreen", border="lightgreen")
add_TA(position_s < 0, on=-1,
       col="lightgrey", border="lightgrey")



# perform parallel loop over lamb_das
lamb_das <- seq(0.001, 0.03, 0.001)
window_s <- seq(500, 1500, 100)

# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, varlist=c("oh_lc", "look_back", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
re_turns <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, look_back=look_back)[, "re_turns"]
})  # end parLapply


### Simple trend-following strategy

bar <- rutils::env_etf$re_turns[, "VTI"]
bar <- cumsum(bar*sign(rutils::lag_xts(bar)))
chart_Series(bar, name="Simple trend-following strategy")


bar <- -cumsum(re_turns*sign(rutils::lag_xts(sk_ew, lag=2)))
bar <- rutils::roll_sum(rutils::lag_xts(sk_ew), look_back=3) / 3
bar <- -cumsum(re_turns*sign(bar))


position_s <- ifelse(abs(SPY_design)>0.052, sign(SPY_design), NA)
position_s[1] <- 0
position_s <- na.locf(position_s)
position_s <- rutils::lag_xts(position_s)
pnl_s <- -cumsum(position_s*re_turns)
colnames(pnl_s) <- "SPY skew contrarian"
chart_Series(x=pnl_s[end_days, ], name="SPY skew contrarian")

cum_pnl <- function(position_s=sk_ew, thresh_old=0.05, re_turns=re_turns) {
  position_s <- ifelse(abs(position_s)>thresh_old, sign(position_s), NA)
  position_s[1] <- 0
  position_s <- na.locf(position_s)
  position_s <- rutils::lag_xts(position_s)
  -sum(position_s*re_turns)
}  # end cum_pnl

cum_pnl(thresh_old=0.045, re_turns=re_turns)

thresh_olds <- seq(from=0.04, to=0.065, by=0.001)
bar <- sapply(thresh_olds, cum_pnl,
              position_s=as.numeric(rutils::lag_xts(SPY_design)),
              re_turns=re_turns)
bar <- cbind(thresh_olds, bar)



### Simulating several managers, with only one manager with skill.

library(HighFreq)
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


### calculate pnl over lookback window
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


### cum_pnl for multi-manager strategy (simpler version)
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


### cum_pnl for trend-following multi-manager strategy (without end_points)
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


### perform loop over lookback windows
# lengths of lookbacks windows
look_backs <- 20*(1:50)
pnl_s <- sapply(look_backs, cum_pnl, se_lect=1, re_turns=re_turns, cum_pnls=cum_pnls)
pnl_s <- cbind(look_backs, pnl_s)
plot(pnl_s, t="l")
# plot(cumsum(pnl_s), t="l")


### pre-calculate row order indices for a vector of look_backs
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


### cum_pnl for long-short multi-manager strategy (without end_points)
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


### perform loop over lookback windows
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

### pre-calculate row order indices for a vector of look_backs
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

### cum_pnl for long-short multi-manager strategy (without end_points)
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


### double the dri_ft
set.seed(1121)  # reset random number generator
re_turns <- matrix(vol_at*rnorm(num_managers*n_row) - vol_at^2/2, nc=num_managers) + 2*dri_ft
# calculate cumulative returns
cum_pnls <- apply(re_turns, 2, cumsum)

### pre-calculate row order indices for a vector of look_backs
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

### end perform loop over lookback windows


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
look_back <- 51
lamb_da <- 0.05
# calculate EWMA prices
weight_s <- exp(-lamb_da*1:look_back)
weight_s <- weight_s/sum(weight_s)
ew_ma <- stats::filter(cl_ose, filter=weight_s, sides=1)
ew_ma[1:(look_back-1)] <- ew_ma[look_back]
ew_ma <- xts(ew_ma, order.by=index(oh_lc))
colnames(ew_ma) <- "VTI EWMA"

# determine dates right after EWMA has crossed prices
in_dic <- sign(cl_ose - ew_ma[, 2])
trade_dates <- (rutils::diff_xts(in_dic) != 0)
trade_dates <- which(trade_dates) + 1
# calculate positions, either: -1, 0, or 1
position_s <- rep(NA_integer_, NROW(cl_ose))
position_s[1] <- 0
position_s[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
position_s <- na.locf(position_s)
position_s <- xts(position_s, order.by=index(oh_lc))

prices_lag <- rutils::lag_xts(cl_ose)
position_lagged <- rutils::lag_xts(position_s)
# calculate daily profits and losses
re_turns <- position_lagged*(cl_ose - prices_lag)
re_turns[trade_dates] <-
  position_lagged[trade_dates] *
  (op_en[trade_dates] - prices_lag[trade_dates]) +
  position_s[trade_dates] *
  (cl_ose[trade_dates] - op_en[trade_dates])
# calculate annualized Sharpe ratio of strategy returns
sqrt(260)*sum(re_turns)/sd(re_turns)/NROW(re_turns)
pnl_s <- cumsum(re_turns)
pnl_s <- cbind(cl_ose-as.numeric(cl_ose[1, ]), pnl_s)
colnames(pnl_s) <- c("VTI", "EWMA PnL")


# define function for simulating daily EWMA crossover strategy
simu_ewma <- function(oh_lc, lamb_da=0.05, look_back=51) {
  # calculate EWMA prices
  weight_s <- exp(-lamb_da*1:look_back)
  weight_s <- weight_s/sum(weight_s)
  cl_ose <- Cl(oh_lc)
  ew_ma <- stats::filter(as.numeric(cl_ose), filter=weight_s, sides=1)
  ew_ma[1:(look_back-1)] <- ew_ma[look_back]
  # determine dates right after EWMA has crossed prices
  in_dic <- xts(sign(as.numeric(cl_ose) - ew_ma), order.by=index(oh_lc))
  trade_dates <- (rutils::diff_xts(in_dic) != 0)
  trade_dates <- which(trade_dates) + 1
  trade_dates <- trade_dates[trade_dates<NROW(oh_lc)]
  # calculate positions, either: -1, 0, or 1
  position_s <- rep(NA_integer_, NROW(cl_ose))
  position_s[1] <- 0
  position_s[trade_dates] <- rutils::lag_xts(in_dic)[trade_dates]
  position_s <- xts(na.locf(position_s), order.by=index(oh_lc))
  op_en <- Op(oh_lc)
  prices_lag <- rutils::lag_xts(cl_ose)
  position_lagged <- rutils::lag_xts(position_s)
  # calculate daily profits and losses
  re_turns <- position_lagged*(cl_ose - prices_lag)
  re_turns[trade_dates] <-
    position_lagged[trade_dates] *
    (op_en[trade_dates] - prices_lag[trade_dates]) +
    position_s[trade_dates] *
    (cl_ose[trade_dates] - op_en[trade_dates])
  out_put <- cbind(position_s, re_turns)
  colnames(out_put) <- c("position_s", "re_turns")
  out_put
}  # end simu_ewma


# perform parallel loop over lamb_das
lamb_das <- seq(0.001, 0.03, 0.001)
lamb_das <- seq(0.01, 1.0, 0.1)

# initialize compute cluster under Windows
library(parallel)
clus_ter <- makeCluster(detectCores()-1)
clusterExport(clus_ter, varlist=c("oh_lc", "look_back", "simu_ewma"))
# perform parallel loop over lamb_das under Windows
re_turns <- parLapply(clus_ter, lamb_das, function(lamb_da) {
  library(quantmod)
  # simulate EWMA strategy and calculate re_turns
  simu_ewma(oh_lc=oh_lc, lamb_da=lamb_da, look_back=look_back)[, "re_turns"]
})  # end parLapply


# set up loop over lookback windows
# length of lookback window
# look_back <- 11
# define end_points at end of every day
end_points <- xts::endpoints(oh_lc, on="days")
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


### perform loop over lookback windows
# lengths of lookbacks windows
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

### end perform loop over lookback windows


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

cum_pnl(beta_s=beta_s, de_sign=SPY_design[date_s], re_turns=returns_running[date_s])

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
bar <- match(index(vol_spikes), index(vari_ance))
tail(bar)



hc <- hclust(dist(USArrests))
plot(hc)
cutree(hc, k=5)
cutree(hc, h=50)

returns_future <- rutils::roll_sum(returns_running, look_back=5)
returns_future <- rutils::lag_xts(returns_running, lag=-5)
colnames(returns_future) <- "returns_future"
foo <- lm(returns_future["2008"] ~ SPY_design["2008"] - 1)
summary(foo)


###

16*sd(rutils::env_etf$re_turns[, "VTI"])
sqrt(250)
250/5

# Summary: Create a functional which aggregates
# asset returns over lookback and look-forward
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



