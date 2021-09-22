### stat-arb script
library(quantmod)
library(plotly)

### Load the functions
source("C:/Develop/R/scripts/stat_arb_func.R")

### read time series data from csv files
prices_ts_all <- upload_data(c("XLU Prices 26052017.csv", "XLP Prices 26052017.csv"))
#head(prices_ts_all)
#NROW(prices_ts_all)
sum(is.na(prices_ts_all))

### subset historical window of prices
start_date <- "2015-05-29"
end_date <- "2016-05-26"
prices_ts <- prices_ts_all[paste(start_date, end_date, sep="/")]
prices_ts <- cbind(1:NROW(prices_ts), prices_ts)
colnames(prices_ts)[1] <- "index"
# head(prices_ts)
# NROW(prices_ts)


### initial parameters
name_x <- "XLU"
name_y <- "XLP"
flag_hr_type <- 1 # 0 ols crossing the origin, 1(default) ols, 2 tls, 3 no-drift index, 4 no-drift dates
flag_sticky_stops <- TRUE  # use sticky stops, FALSE use regular stops 
flag_print <- TRUE
flag_plot <- TRUE

results_hr <- jk_hedge_ratio(prices_ts_x = prices_ts[,name_x], 
                             prices_ts_y = prices_ts[,name_y],
                             flag_hr_type = 0, 
                             flag_print = flag_print,
                             flag_plot = flag_plot)

trading_levels <- list(z_long_enter = -1.5, 
                       z_long_profit = 0, 
                       z_long_stop = -2.5, 
                       z_short_enter = 1.5, 
                       z_short_profit = 0, 
                       z_short_stop = 2.5)

trade_stats <- trade_pair(prices_ts = prices_ts, 
                      name_x = name_x, 
                      name_y = name_y, 
                      trading_levels = trading_levels, 
                      flag_hr_type = flag_hr_type, 
                      flag_sticky_stops = flag_sticky_stops, 
                      flag_print = flag_print, 
                      flag_plot = flag_plot)

cat(paste0("sharpe ratio: \t", round(trade_stats$results_val$port_sharpe_ratio, digits = 6)))



####################################
### ignore below

# plot interactive dygraphs plot
##dygraph(port_cumret, main="Cumulative PnL") %>%
##  dyOptions(colors=c("orange", "blue")) %>%
##  dyRangeSelector()

#z_step <- 0.1
#n_step <- -level_long_stop / z_step
#sharpe_ratio_matrix <- matrix(0, nrow = n_step, ncol = n_step)
#level_enter_long_vector <- level_long_stop + z_step * 1:n_step
#profit_long_vector <- level_long_stop + z_step * 1:n_step

#for(index_level_enter_long in 1:n_step) {
#  level_long_enter <- level_enter_long_vector[index_level_enter_long]
#  print(index_level_enter_long)

#for(level_long_profit in seq(level_long_enter + z_step, 0, by = z_step)) {
#for(index_profit_long in (index_level_enter_long +1):n_step) {
#  if (index_profit_long == index_level_enter_long) next
#  level_long_profit <- profit_long_vector[index_profit_long]
#  print(index_profit_long)



#results_reg <- calc_reg(prices_ts = prices_ts, name#print(c("z_long_enter = ", z_long_enter, "z_long_profit = ", z_long_profit, "sharpe_ratio = ", round(sharpe_ratio, 4)))
#sharpe_ratio_matrix[index_level_enter_long, index_profit_long] <- sharpe_ratio

#}

#}

#plot_ly(x = profit_long_vector, y = level_enter_long_vector, z = sharpe_ratio_matrix, type = "contour")
#plot_ly(x = ~profit_long_vector, y = ~level_enter_long_vector, z = ~sharpe_ratio_matrix, type = "contour")
#cat(sprintf("%s %.2f %s %.2f %s %.6f \n", "z_long_enter=", z_long_enter, 
#            "z_long_profit=", z_long_profit, 
#            "sharpe_ratio=", sharpe_ratio))

#results_reg <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_hr_type = -1)
#results_reg <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_hr_type = 2)
#results_reg <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_hr_type = 1)

#hr_ols <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_hr_type = FALSE)
#port_ols <- prices_ts[, name_y] - hr_ols * prices_ts[, name_x]
#colnames(port_ols) <- "port_ols"
#prices_ts <- cbind(prices_ts, port_ols)
#head(prices_ts)

#hr_ndr <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_hr_type = 2)
#port_ndr <- prices_ts[, name_y] - hr_ndr * prices_ts[, name_x]
#colnames(port_ndr) <- "port_ndr"
#prices_ts <- cbind(prices_ts, port_ndr)
#head(prices_ts)

#hr_tls <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_hr_type = 1)
#port_tls <- prices_ts[, name_y] - hr_tls * prices_ts[, name_x]
#colnames(port_tls) <- "port_tls"
#prices_ts <- cbind(prices_ts, port_tls)
#head(prices_ts)

#calc_reg(prices_ts = prices_ts, name_x = "index", name_y = "port_ols", flag_hr_type = FALSE, flag_print = 1)
#calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = "port_ols", flag_hr_type = FALSE, flag_print = 1)
#calc_reg(prices_ts = prices_ts, name_x = "index", name_y = "port_ndr", flag_hr_type = FALSE, flag_print = 1)
#calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = "port_ndr", flag_hr_type = 2, flag_print = 1)

### plot interactive dygraphs plot with data range sector
#library(dygraphs)
#dygraph(prices_ts_all, main="XLP and XLU prices") %>%
#dyOptions(colors=c("orange", "blue")) %>%
#dyRangeSelector()

### or static plot with custom line colors
#plot_theme$col$line.col <- c("orange", "blue")
#x11(width=8, height=6)
#chart_Series(prices_ts_all, theme=plot_theme, 
#             name="XLP and XLU prices")
#legend("top", legend=colnames(prices_ts_all), cex=0.8, 
#       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
#       col=plot_theme$col$line.col, bty="n")

#dygraph(prices_ts[, c(2, 3)], main="XLP and XLU prices") %>%
#  dyOptions(colors=c("orange", "blue")) %>%
#  dyRangeSelector()

## regression of training prices
#reg_model <- lm(XLP ~ XLU, data = prices_ts)
#summary(reg_model)
#coef(reg_model)[2]
#summary(reg_model)$r.squared

## scatterplot
#plot(as.data.frame(prices_ts[, c(2, 3)]))
#abline(reg_model, lwd=2, col="blue")


## regression of training prices - zero intercept
#reg_model <- lm(XLP ~ XLU - 1, data = prices_ts)
#summary(reg_model)
#coef(reg_model)[1]

## regression of training prices versus date index
#prices_ts <- cbind(prices_ts, 1:n_row)
##coef(lm(prices_ts[, "XLP"] ~ prices_ts[, 3]))[2] / coef(lm(prices_ts[, "XLU"] ~ prices_ts[, 3]))[2]

## regression residuals and z_val
#reg_model <- lm(XLP ~ XLU, data=prices_ts)
##summary(reg_model)
#hedge_ratio <- coef(reg_model)[2]
## resi_duals <- prices_ts[, "XLP"] - coef(reg_model)[2]*prices_ts[, "XLU"]
#resi_duals <- residuals(reg_model) + coef(reg_model)[2]
#aver_age <- mean(resi_duals)
#std_dev <- sd(resi_duals)
#z_val <- (resi_duals-aver_age)/std_dev

# plot z_val
###x11(width=8, height=6)
###da_ta <- cbind(as.data.frame(prices_ts), z_val)[, c(3, 4)]
###plot(da_ta)
###abline(lm(da_ta[, 2] ~ da_ta[, 1]), lwd=2, col="blue")


## model parameters
#z_long_enter <- -1.5
#z_long_profit <- 0
#z_long_stop <- -2.5

#z_short_enter <- 1.5
#z_short_profit <- 0
#z_short_stop <- 2.5

# in-sample training set
#stop_sell_mode <- rep(NA, n_row)
#stop_sell_mode[1] <- (z_val[1] < z_long_stop)
#stop_sell_mode[z_val < z_long_stop] <- TRUE
#stop_sell_mode[z_val > z_long_enter] <- FALSE
#stop_sell_mode <- zoo::na.locf(stop_sell_mode)

#stop_sell <- rep(NA_real_, n_row)
#indic_sell <- (z_val < z_long_stop)
#stop_sell[indic_sell] <- z_val[indic_sell]
## stop_sell <- zoo::na.locf(stop_sell)

#stop_buy_mode <- rep(NA, n_row)
#stop_buy_mode[1] <- (z_val[1] < z_short_stop)
#stop_buy_mode[z_val > z_short_stop] <- TRUE
#stop_buy_mode[z_val < z_short_enter] <- FALSE
#stop_buy_mode <- zoo::na.locf(stop_buy_mode)

#z_short <- numeric(n_row)
#indic_short <- ((z_val > z_short_enter) & !stop_buy_mode) | 
#  ((!stop_buy_mode) & rutils::lag_it(stop_buy_mode))
#z_short[indic_short] <- z_val[indic_short]

#z_buy <- numeric(n_row)
#indic_buy <- ((z_val < z_long_enter) & !stop_sell_mode) | 
#  ((!stop_sell_mode) & rutils::lag_it(stop_sell_mode))
#z_buy[indic_buy] <- z_val[indic_buy]
