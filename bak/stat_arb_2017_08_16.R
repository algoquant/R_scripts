### stat-arb questions:
# what is logic behind SBMode and SSMode?


### stat-arb script
library(quantmod)

# read the data
XLP <- zoo::read.zoo(file="C:/Develop/data/XLP Prices 26052017.csv", 
                     header=FALSE, sep=",", FUN=as.Date,
                     format="%m/%d/%Y")
XLP <- xts::as.xts(XLP)
XLU <- zoo::read.zoo(file="C:/Develop/data/XLU Prices 26052017.csv", 
                     header=FALSE, sep=",", FUN=as.Date,
                     format="%m/%d/%Y")
XLU <- xts::as.xts(XLU)
price_s <- cbind(XLU, XLP)
colnames(price_s) <- c("XLU", "XLP")
head(price_s)
sum(is.na(price_s))

# plot interactive dygraphs plot
##library(dygraphs)
# plot dygraph with date range selector
##dygraph(price_s, main="XLP and XLU prices") %>%
##  dyOptions(colors=c("orange", "blue")) %>%
##  dyRangeSelector()


# or static plot with custom line colors
##plot_theme$col$line.col <- c("orange", "blue")
##x11(width=8, height=6)
##chart_Series(price_s, theme=plot_theme, 
##             name="XLP and XLU prices")
##legend("top", legend=colnames(price_s), cex=0.8,
##       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
##       col=plot_theme$col$line.col, bty="n")


## training prices
start_train <- "2015-05-29"
end_train <- "2016-05-26"
train_prices <- price_s[paste(start_train, end_train, sep="/")]
n_row <- NROW(train_prices)
##dygraph(train_prices, main="XLP and XLU prices") %>%
##  dyOptions(colors=c("orange", "blue")) %>%
##  dyRangeSelector()

## regression of training prices
reg_model <- lm(XLP ~ XLU, data=train_prices)
##summary(reg_model)
##coef(reg_model)[2]

# scatterplot
##plot(as.data.frame(train_prices))
##abline(reg_model, lwd=2, col="blue")


## regression of training prices - zero intercept
reg_model <- lm(XLP ~ XLU - 1, data=train_prices)
##summary(reg_model)
##coef(reg_model)[2]

## regression of training prices versus date index
train_prices <- cbind(train_prices, 1:n_row)
##coef(lm(train_prices[, "XLP"] ~ train_prices[, 3]))[2] / coef(lm(train_prices[, "XLU"] ~ train_prices[, 3]))[2]



## regression residuals and z_scores
reg_model <- lm(XLP ~ XLU, data=train_prices)
##summary(reg_model)
hedge_ratio <- coef(reg_model)[2]
# resi_duals <- train_prices[, "XLP"] - coef(reg_model)[2]*train_prices[, "XLU"]
resi_duals <- residuals(reg_model) + coef(reg_model)[1]
aver_age <- mean(resi_duals)
std_dev <- sd(resi_duals)
z_scores <- (resi_duals-aver_age)/std_dev

# plot z_scores
###x11(width=8, height=6)
###da_ta <- cbind(as.data.frame(train_prices), z_scores)[, c(3, 4)]
###plot(da_ta)
###abline(lm(da_ta[, 2] ~ da_ta[, 1]), lwd=2, col="blue")

#for(enter_long in seq(-2.0,0,by = 0.1)) {
#  print(enter_long)

## model parameters
#enter_long <- -1.5
enter_long <- -0.2
enter_short <- 1.5
profit_long <- 0
profit_short <- 0
stop_long <- -2.5
stop_short <- 2.5


## simulate model positions

# simulate model long positions
sto_p <- rep(NA_integer_, n_row)
sto_p[1] <- (z_scores[1] < stop_long)
sto_p[z_scores < stop_long] <- TRUE
sto_p[z_scores > enter_long] <- FALSE
sto_p <- zoo::na.locf(sto_p)
en_ter <- (rutils::diff_it(z_scores < enter_long) > 0) | 
  (rutils::diff_it(sto_p) < 0)
pro_fit <- (rutils::diff_it(z_scores > profit_long) > 0)
pos_long <- rep(NA_integer_, n_row)
pos_long[1] <- ((z_scores[1] < enter_long) & !sto_p[1])
pos_long[en_ter & !sto_p] <- 1
pos_long[pro_fit] <- 0
pos_long[as.logical(sto_p)] <- 0
pos_long <- zoo::na.locf(pos_long)

#cbind(round(z_scores,2),z_scores < enter_long, rutils::diff_it(z_scores < enter_long) > 0, sto_p, rutils::diff_it(sto_p) < 0, en_ter, (z_scores > profit_long)>0, pro_fit, pos_long)

# simulate model short positions
sto_p <- rep(NA_integer_, n_row)
sto_p[1] <- (z_scores[1] > stop_short)
sto_p[z_scores > stop_short] <- TRUE
sto_p[z_scores < enter_short] <- FALSE
sto_p <- zoo::na.locf(sto_p)
en_ter <- (rutils::diff_it(z_scores > enter_short) > 0) | 
  (rutils::diff_it(sto_p) < 0)
pro_fit <- (rutils::diff_it(z_scores<profit_short) > 0)
pos_short <- rep(NA_integer_, n_row)
pos_short[1] <- ((z_scores[1] > enter_short) & !sto_p[1])
pos_short[en_ter & !sto_p] <- (-1)
pos_short[pro_fit] <- 0
pos_short[as.logical(sto_p)] <- 0
pos_short <- zoo::na.locf(pos_short)


# total positions
po_sitions <- pos_long + pos_short


### calculate Pnls

p_nl <- rutils::lag_it(po_sitions)*(rutils::diff_xts(train_prices[, "XLP"]) - 
                                      hedge_ratio*rutils::diff_xts(train_prices[, "XLU"]))

### JP: shorten and lighten notation using these variables:
in_dex <- index(p_nl)
na_xts <- xts(rep(NA_real_, n_row), order.by=in_dex)
colnames(na_xts) <- "na_xts"

##head(p_nl, 22)
##tail(p_nl, 22)

### Jack
### calculating the rest of statistics

# portfolio_dollar_values <- na_xts
portfolio_dollar_values <- p_nl
colnames(portfolio_dollar_values) <- "portfolio_dollar_values"

# portfolio_dollar_values[1] <- as.double(train_prices[1, "XLP"]) +
#                               as.double(train_prices[1, "XLU"]) *
#                               abs(hedge_ratio)

### JP: this works just fine
portfolio_dollar_values[1] <- train_prices[1, "XLP"] + train_prices[1, "XLU"] * abs(hedge_ratio)

portfolio_dollar_values <- cumsum(portfolio_dollar_values)
# all.equal(portfolio_dollar_values,portfolio_dollar_values_foo)
# tail(portfolio_dollar_values)

### JP: no need to initialize portfolio_percentage_returns
# portfolio_percentage_returns <- na_xts
# colnames(portfolio_percentage_returns) <- "portfolio_percentage_returns"
portfolio_percentage_returns <- p_nl / rutils::lag_xts(portfolio_dollar_values)
# portfolio_percentage_returns <- unname(portfolio_percentage_returns)

# for (i in 2:n_row) {
#  portfolio_percentage_returns[i] <- as.double(p_nl[i]) / 
#                                     as.double(portfolio_dollar_value[i-1])
# }

# round(head(portfolio_percentage_returns), digits = 6)

portfolio_cumulative_returns <- cumprod(1 + portfolio_percentage_returns) - 1
colnames(portfolio_cumulative_returns) <- "portfolio_cumulative_returns"

# round(head(portfolio_cumulative_returns, 252), digits = 6)
# round(tail(portfolio_cumulative_returns), digits = 6)


# plot interactive dygraphs plot
##dygraph(portfolio_cumulative_returns, main="Cumulative PnL") %>%
##  dyOptions(colors=c("orange", "blue")) %>%
##  dyRangeSelector()


sharpe_ratio <- sqrt(n_row - 1) * mean(portfolio_percentage_returns[-1]) / sd(portfolio_percentage_returns[-1])
#round(sharpe_ratio, digits = 2)

sharpe_ratio_dollar <- sqrt(n_row - 1) * mean(p_nl[-1]) / sd(p_nl[-1])
#round(sharpe_ratio_dollar, digits = 2)

portfolio_return <- as.double(portfolio_cumulative_returns[n_row])
portfolio_volatility <- sqrt(n_row - 1) * sd(portfolio_percentage_returns[-1])

portfolio_high_watermark <- na_xts
portfolio_high_watermark[1] <- 0
for (i in 2:n_row) {
  portfolio_high_watermark[i] <- max(portfolio_high_watermark[i-1], portfolio_cumulative_returns[i])
}
high_watermark_loop <- portfolio_high_watermark

### JP: this works without loop
portfolio_high_watermark <- cummax(portfolio_cumulative_returns)

all.equal(as.numeric(high_watermark_loop), 
          as.numeric(portfolio_high_watermark))

# head(portfolio_high_watermark, n_row)

portfolio_drawdown <- (1 + portfolio_cumulative_returns) / (1 + portfolio_high_watermark) - 1
# head(portfolio_drawdown, n_row)
# min(portfolio_drawdown)
# zoo::plot.zoo(portfolio_drawdown)

portfolio_drawdown_duration <- xts(rep(NA_integer_, n_row), order.by=in_dex)
portfolio_drawdown_duration[1] <- 0
for (i in 2:n_row) {
  if(portfolio_drawdown[i] == 0) {
    portfolio_drawdown_duration[i] <- 0
  } else {
    portfolio_drawdown_duration[i] <- portfolio_drawdown_duration[i-1] + 1
  }
}  # end for


### JP: this is a vectorized version of above loop, but with more R gymnastics than I would prefer

portfolio_drawdown_duration <- -sign(portfolio_drawdown)
cum_sum <- cumsum(portfolio_drawdown_duration)
ze_ro <- rutils::diff_xts(portfolio_drawdown_duration)
# sum(ze_ro==(-1))
whi_ch <- which(ze_ro==(-1))
which_cumsum <- cum_sum[whi_ch]
lag_which_cumsum <- rutils::lag_xts(which_cumsum)
lag_which_cumsum[1] <- 0
portfolio_drawdown_duration[whi_ch] <- (lag_which_cumsum - which_cumsum)
portfolio_drawdown_duration <- cumsum(portfolio_drawdown_duration)

# plot both in two panels
# zoo::plot.zoo(cbind(portfolio_drawdown, portfolio_drawdown_duration))


# head(portfolio_drawdown_duration, 20)
# max(portfolio_drawdown_duration)

paste("return = ", round(as.double(portfolio_cumulative_returns[n_row]) * 100, digits = 2), "%", sep = "")
paste("volatility = ", round(portfolio_volatility * 100, digits = 2), "%", sep = "")
paste("Sharpe Ratio = ", round(sharpe_ratio, digits = 2), sep = "")
paste("Max Drawdown = ", round(min(portfolio_drawdown) * 100, digits = 2), "%", sep = "")
paste("Max DrawDown Duration = ", max(portfolio_drawdown_duration), sep = "")
paste("Number of trades = ", length(po_sitions[abs(po_sitions - rutils::lag_it(po_sitions))]), sep = "")

print(sharpe_ratio)



### another simpler, alternative model,

# simulate model long positions
en_ter <- (rutils::diff_it(z_scores < enter_long) > 0) | 
  ((rutils::diff_it(z_scores < stop_long) < 0) & (z_scores < enter_long))
pro_fit <- (rutils::diff_it(z_scores > profit_long) > 0)
sto_p <- (rutils::diff_it(z_scores < stop_long) > 0)
pos_long <- rep(NA_integer_, n_row)
pos_long[1] <- ((z_scores[1] < enter_long) & (z_scores[1] > stop_long))
pos_long[en_ter & !sto_p] <- 1
pos_long[pro_fit] <- 0
pos_long[sto_p] <- 0
pos_long <- zoo::na.locf(pos_long)


# simulate model short positions
en_ter <- (rutils::diff_it(z_scores > enter_short) > 0) | 
  ((rutils::diff_it(z_scores > stop_short) < 0) & (z_scores > enter_short))
pro_fit <- (rutils::diff_it(z_scores < profit_short) > 0)
sto_p <- (rutils::diff_it(z_scores > stop_short) > 0)
pos_short <- rep(NA_integer_, n_row)
pos_short[1] <- ((z_scores[1] > enter_short) & (z_scores[1] < stop_short))
pos_short[en_ter & !sto_p] <- -1
pos_short[pro_fit] <- 0
pos_short[sto_p] <- 0
pos_short <- zoo::na.locf(pos_short)

# total positions
po_sitions <- pos_long + pos_short



## ignore below
# in-sample training set
ss_mode <- rep(NA, n_row)
ss_mode[1] <- (z_scores[1] < stop_long)
ss_mode[z_scores < stop_long] <- TRUE
ss_mode[z_scores > enter_long] <- FALSE
ss_mode <- zoo::na.locf(ss_mode)

stop_sell <- rep(NA_real_, n_row)
in_dic <- (z_scores < stop_long)
stop_sell[in_dic] <- z_scores[in_dic]
# stop_sell <- zoo::na.locf(stop_sell)

sb_mode <- rep(NA, n_row)
sb_mode[1] <- (z_scores[1] < stop_short)
sb_mode[z_scores > stop_short] <- TRUE
sb_mode[z_scores < enter_short] <- FALSE
sb_mode <- zoo::na.locf(sb_mode)

z_short <- numeric(n_row)
in_dic <- ((z_scores > enter_short) & !sb_mode) | 
  ((!sb_mode) & rutils::lag_it(sb_mode))
z_short[in_dic] <- z_scores[in_dic]


z_buy <- numeric(n_row)
in_dic <- ((z_scores < enter_long) & !ss_mode) | 
  ((!ss_mode) & rutils::lag_it(ss_mode))
z_buy[in_dic] <- z_scores[in_dic]

