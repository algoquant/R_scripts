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
library(dygraphs)
# plot dygraph with date range selector
dygraph(price_s, main="XLP and XLU prices") %>%
  dyOptions(colors=c("orange", "blue")) %>%
  dyRangeSelector()


# or static plot with custom line colors
plot_theme <- chart_theme()
plot_theme$col$line.col <- c("orange", "blue")
x11(width=8, height=6)
chart_Series(price_s, theme=plot_theme, 
             name="XLP and XLU prices")
legend("top", legend=colnames(price_s), cex=0.8,
       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
       col=plot_theme$col$line.col, bty="n")


## training prices
start_train <- "2015-05-29"
end_train <- "2016-05-26"
train_prices <- price_s[paste(start_train, end_train, sep="/")]
n_row <- NROW(train_prices)
dygraph(train_prices, main="XLP and XLU prices") %>%
  dyOptions(colors=c("orange", "blue")) %>%
  dyRangeSelector()

## regression of training prices
reg_model <- lm(XLP ~ XLU, data=train_prices)
summary(reg_model)
coef(reg_model)[2]

# scatterplot
plot(as.data.frame(train_prices))
abline(reg_model, lwd=2, col="blue")


## regression of training prices - zero intercept
reg_model <- lm(XLP ~ XLU - 1, data=train_prices)
summary(reg_model)
coef(reg_model)[2]

## regression of training prices versus date index
train_prices <- cbind(train_prices, 1:n_row)
coef(lm(train_prices[, "XLP"] ~ train_prices[, 3]))[2] / coef(lm(train_prices[, "XLU"] ~ train_prices[, 3]))[2]



## regression residuals and z_scores
reg_model <- lm(XLP ~ XLU, data=train_prices)
summary(reg_model)
hedge_ratio <- coef(reg_model)[2]
# resi_duals <- train_prices[, "XLP"] - coef(reg_model)[2]*train_prices[, "XLU"]
resi_duals <- residuals(reg_model) + coef(reg_model)[1]
aver_age <- mean(resi_duals)
std_dev <- sd(resi_duals)
z_scores <- (resi_duals-aver_age)/std_dev

# plot z_scores
x11(width=8, height=6)
da_ta <- cbind(as.data.frame(train_prices), z_scores)[, c(3, 4)]
plot(da_ta)
abline(lm(da_ta[, 2] ~ da_ta[, 1]), lwd=2, col="blue")


## model parameters
enter_long <- -1
enter_short <- 1
profit_long <- 0
profit_short <- 0
stop_long <- -1.5
stop_short <- 1.5


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
head(p_nl, 22)
tail(p_nl, 22)

# plot interactive dygraphs plot
dygraph(cumsum(p_nl), main="Cumulative PnL") %>%
  dyOptions(colors=c("orange", "blue")) %>%
  dyRangeSelector()


### another simpler, alternative model

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

