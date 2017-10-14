####################################
### Scripts for performing backtest simulations of trading strategies
# These are legacy scripts to replicate the analysis for "XLU" and "XLP"


### Change log
## 2017-09-12: major rewrite and renaming of functions:
# run_backtest() -> run_backtest_endpoints()
# run_backtest2() -> run_backtest_subset()

### stat-arb script


### Install packages rutils and HighFreq from github
devtools::install_github("algoquant/rutils")
devtools::install_github("algoquant/HighFreq")
library(HighFreq)
# library(plotly)

### Load the custom functions
source("C:/Develop/R/scripts/stat_arb_func_2017_09_29.R")

### Read time series data from csv files
# prices_ts_all <- load_data(c("UST10.csv", "USDJPY.csv"))
prices_ts_all <- xts::as.xts(zoo::read.zoo(
  file=file.path("C:/Develop/data",
                 "data prices close 2017-08-31.csv"),
  header=TRUE, sep=",", FUN=as.Date, format="%m/%d/%Y"))
# head(prices_ts_all)
# NROW(prices_ts_all)
# remove NAs
# prices_ts_all <- na.omit(prices_ts_all)
# remove XLR column because it has mostly NAs
prices_ts_all <- prices_ts_all[, -which("XLR"==colnames(prices_ts_all))]
sum(is.na(prices_ts_all))

### Subset the time series (historical window of prices)
# start_date <- "2015-05-29"
# end_date <- "2016-05-26"
start_date <- start(prices_ts_all)
start_date <- "2015-05-29"
end_date <- end(prices_ts_all)
end_date <- "2016-05-26"
prices_ts <- prices_ts_all[paste(start_date, end_date, sep="/")]
prices_ts <- cbind(1:NROW(prices_ts), prices_ts)
colnames(prices_ts)[1] <- "index"
# head(prices_ts)
# NROW(prices_ts)


### Define model parameters
name_x <- "XLU"
name_y <- "XLP"
symbol_s <- c("XLP", "XLU")
model_type <- 1 # 0: OLS crossing the origin, 1: (default) ols, 2: TLS, 3: no-drift index, 4: no-drift dates
flag_sticky_stops <- TRUE  # use sticky stops, FALSE use regular stops
flag_print <- FALSE
flag_plot <- FALSE

trading_levels <- list(z_long_enter = -1.5,
                       z_long_profit = 0,
                       z_long_stop = -2.5,
                       z_short_enter = 1.5,
                       z_short_profit = 0,
                       z_short_stop = 2.5)

model_params <- list(
  symbol_s=symbol_s,
  model_type=model_type,
  flag_print=flag_print,
  flag_plot=flag_plot
)  # end list


trading_params <- list(
  symbol_s=symbol_s,
  trading_levels=trading_levels,
  flag_sticky_stops=flag_sticky_stops,
  flag_print=flag_print,
  flag_plot=flag_plot
)  # end list



### calibrate and run (test) models using the whole time series

# run model
res_hr <- hedge_ratio(prices_ts_x = prices_ts[, name_x],
                      prices_ts_y = prices_ts[, name_y],
                      model_type = model_type,
                      flag_print = TRUE,
                      flag_plot = TRUE)

# or
calibrated_model <- train_model(time_series = prices_ts,
                                model_params = model_params)
all.equal(res_hr, calibrated_model)


# run model
res_tp <- trade_pair(prices_ts_x = prices_ts[, name_x],
                     prices_ts_y = prices_ts[, name_y],
                     res_hr = res_hr,
                     trading_levels = trading_levels,
                     flag_sticky_stops = flag_sticky_stops,
                     flag_print = TRUE,
                     flag_plot = TRUE)

# or
trade_output <- trade_model(time_series = prices_ts,
                            calibrated_model = calibrated_model,
                            trading_params = trading_params)
all.equal(res_tp, trade_output)


### Perform backtest simulations of model over a sliding lookback window

## Define backtest parameters
# start_date <- "2007-12-31"
start_date <- start(prices_ts_all)
# end_date <- "2010-05-27"
end_date <- end(prices_ts_all)
prices_ts <- prices_ts_all[paste(start_date, end_date, sep="/")]
start_point <- which(start_date==index(prices_ts_all))
end_point <- which(end_date==index(prices_ts_all))



### Perform backtest loop over equally spaced business days (rows of data),
# with an extended lookback interval, and with a stub interval at the beginning.
# I think this is the proper way to do it.

# number of rows of available data
n_row <- (end_point - start_point) + 1
look_forward <- 2*22  # about twice monthly
look_back <- 252  # about one year

# number of intervals in the backtest
num_intervals <- n_row %/% look_forward
# calculate end points and convert them to dates
end_points <- n_row - num_intervals*look_forward + (0:num_intervals)*look_forward + start_point - 1
# or use package rutils
end_points <- rutils::calc_endpoints(prices_ts, look_forward)
end_points <- index(prices_ts[end_points, ])



# perform the backtest using roll_backtest()
back_test <- t(roll_backtest(end_points=end_points[-NROW(end_points)],
                            look_forward=look_forward,
                            look_back=look_back,
                            train_model = train_model,
                            model_params = model_params,
                            trade_model = trade_model,
                            trading_params = trading_params,
                            price_s=prices_ts))
back_test <- xts::xts(back_test, order.by=end_points[-NROW(end_points)])


back_test <- HighFreq::roll_backtest(end_points=rutils::calc_endpoints(prices_ts, look_forward),
                           look_forward=look_forward,
                           look_back=look_back,
                           train_func = train_model,
                           trade_func = trade_model,
                           model_params = model_params,
                           trading_params = trading_params,
                           x_ts=prices_ts)

all.equal(back_test_old, back_test)






####################################
### legacy code below

### Perform loop using while() loop

start_date_train <- start_date
end_date_train <- start_date_train + look_back
start_date_test <- end_date_train + 1
end_date_test <- start_date_test + look_back

back_test <- NULL
start_dates <- NULL

while (end_date_test <= end_date) {

  prices_ts_train <- prices_ts_all[paste(start_date_train, end_date_train, sep="/")]
  prices_ts_train <- cbind(1:NROW(prices_ts_train), prices_ts_train)
  colnames(prices_ts_train)[1] <- "index"

  prices_ts_test <- prices_ts_all[paste(start_date_test, end_date_test, sep="/")]
  prices_ts_test <- cbind(1:NROW(prices_ts_test), prices_ts_test)
  colnames(prices_ts_test)[1] <- "index"

  res_hr <- hedge_ratio(prices_ts_x = prices_ts_train[, name_x],
                        prices_ts_y = prices_ts_train[, name_y],
                        model_type = 1,
                        flag_print = FALSE,
                        flag_plot = 0)

  res_tp <- trade_pair(prices_ts_x = prices_ts_test[, name_x],
                       prices_ts_y = prices_ts_test[, name_y],
                       res_hr = res_hr,
                       trading_levels = trading_levels,
                       flag_sticky_stops = flag_sticky_stops,
                       flag_print = FALSE,
                       flag_plot = 0)

  cat(paste0(start_date_train, "\t",
             end_date_train, "\t",
             round(res_hr$reg_summary$coefficients[1],4), "\t",
             round(res_hr$reg_summary$coefficients[2],4), "\t",
             start_date_test, "\t",
             end_date_test, "\t",
             round(res_tp$res_val$port_sharpe_ratio, digits = 4), "\t",
             round(res_tp$res_val$port_num_trades, digits = 4), "\n"))
  # cat(paste0(start_date_train, "\t",
  #            end_date_train, "\t",
  #            start_date_test, "\t",
  #            end_date_test, "\n"))

  back_test <- rbind(back_test,
                   c(round(res_hr$reg_summary$coefficients[1],4),
                     round(res_hr$reg_summary$coefficients[2],4),
                     round(res_tp$res_val$port_sharpe_ratio, digits = 4),
                     round(res_tp$res_val$port_num_trades, digits = 4)))

  start_dates <- rbind(start_dates, start_date_train)

  start_date_train <- end_date_train + 1
  end_date_train <- end_date_train + look_back

  start_date_test <- end_date_train + 1
  end_date_test <- end_date_test + look_back
}  # end while

back_test <- xts::xts(back_test, order.by=as.Date(start_dates))
colnames(back_test) <- c("intercept", "slope", "sharpe_ratio", "num_trades")

# copy for later
backtest_while <- back_test



### Perform the same calculation as the above while() loop,
# but using run_backtest_endpoints() with sapply() loop.
# Perform a loop over lookback intervals with a fixed number of calendar days.

# define the start and end points at equally spaced calendar days
num_intervals <- (as.numeric(end_date) - as.numeric(start_date)) %/% look_back
end_points <- cbind(c(0, (1:(num_intervals-1))*look_back + 1) + start_date,
                    (1:num_intervals)*look_back + start_date)
start_dates <- as.Date(end_points[, 1])
end_points <- lapply(1:NROW(end_points), function(end_point) {
  paste(as.Date(end_points[end_point, ]), collapse = "/")
})  # end lapply

# Calculate the number of days in the lookback intervals.
# The first lookback interval has one more day than the other intervals - not right.
sapply(end_points, function(end_point) {
  diff(as.Date(strsplit(end_point, split = "/")[[1]]))
})  # end sapply

# Perform the same calculation as the above while() loop,
# but using run_backtest_endpoints() with sapply() loop.
back_test <- run_backtest_endpoints(end_points=end_points, price_s=prices_ts_all)
back_test <- xts::xts(back_test, order.by=start_dates[1:(NROW(start_dates)-1)])

# check if both outputs are identical
# one of the sharpe_ratio values is off - need to find out why
sapply(colnames(back_test), function(coln_ame)
  identical(back_test[, coln_ame], backtest_while[, coln_ame]))



### Perform a similar loop as above, but over equal length lookback intervals
# with a fixed number of calendar days, and with a stub interval at the end.
# I think this is a better way to do it.

num_intervals <- (as.numeric(end_date) - as.numeric(start_date)) %/% look_back
end_points <- c((1:num_intervals)*look_back + start_date-1, end_date)
end_points <- cbind(c(start_date, end_points[1:(NROW(end_points)-1)] + 1),
                    end_points)
start_dates <- as.Date(end_points[, 1])
end_points <- lapply(1:NROW(end_points), function(end_point) {
  paste(as.Date(end_points[end_point, ]), collapse = "/")
})  # end lapply

# Calculate the number of days in the lookback intervals.
sapply(end_points, function(end_point) {
  diff(as.Date(strsplit(end_point, split = "/")[[1]]))
})  # end sapply

# perform the backtest using run_backtest_endpoints()
back_test <- run_backtest_endpoints(end_points=end_points, price_s=prices_ts_all)
back_test <- xts::xts(back_test, order.by=start_dates[1:(NROW(start_dates)-1)])




### Perform a backtest over equal length lookback intervals with a
# fixed number of calendar days, with a stub interval at the beginning.

n_row <- (end_date - start_date) + 1
end_points <- n_row - num_intervals*look_back + (0:num_intervals)*look_back + start_date - 1
end_points <- cbind(c(start_date, end_points[1:(NROW(end_points)-1)] + 1),
                    end_points)
start_dates <- as.Date(end_points[, 1])
end_points <- lapply(1:NROW(end_points), function(end_point) {
  paste(as.Date(end_points[end_point, ]), collapse = "/")
})  # end lapply

# Calculate the number of days in the lookback intervals.
sapply(end_points, function(end_point) {
  diff(as.Date(strsplit(end_point, split = "/")[[1]]))
})  # end sapply

# perform the backtest using run_backtest_endpoints()
back_test <- run_backtest_endpoints(end_points=end_points, price_s=prices_ts_all)
back_test <- xts::xts(back_test, order.by=start_dates[1:(NROW(start_dates)-1)])



### Perform loop over equally spaced business days (rows of data),
# and with a stub interval at the end.

# Define end points that are equally spaced business days (data rows),
# with a stub interval at the end.
start_point <- which(start_date==index(prices_ts_all))
end_point <- which(end_date==index(prices_ts_all))
n_row <- (end_point - start_point) + 1
look_back <- 252
num_intervals <- n_row %/% look_back

end_points <- c((1:num_intervals)*look_back + start_point-1, end_point)
end_points <- cbind(c(start_point, end_points[1:(NROW(end_points)-1)] + 1),
                    end_points)
start_dates <- index(prices_ts_all[end_points[, 2], ])
end_points <- lapply(1:NROW(end_points), function(end_point) {
  end_points[end_point, 1]:end_points[end_point, 2]
})  # end lapply

# Calculate the number of days in the lookback intervals.
sapply(end_points, NROW)

back_test <- run_backtest_endpoints(end_points=end_points, price_s=prices_ts_all)
back_test <- xts::xts(back_test, order.by=start_dates[1:(NROW(start_dates)-1)])



### Perform loop over equally spaced business days (rows of data),
# and with a stub interval at the beginning.

# Define end points that are equally spaced business days (data rows),
# with a stub interval at the beginning.
start_point <- which(start_date==index(prices_ts_all))
end_point <- which(end_date==index(prices_ts_all))
n_row <- (end_point - start_point) + 1
look_back <- 252
num_intervals <- n_row %/% look_back

end_points <- n_row - num_intervals*look_back + (0:num_intervals)*look_back + start_point - 1
end_points <- cbind(c(start_point-1, end_points[1:(NROW(end_points)-1)]) + 1,
                    end_points)
start_dates <- index(prices_ts_all[end_points[, 2], ])
end_points <- lapply(1:NROW(end_points), function(end_point) {
  end_points[end_point, 1]:end_points[end_point, 2]
})  # end lapply

# Calculate the number of days in the lookback intervals.
sapply(end_points, NROW)

back_test <- run_backtest_endpoints(end_points=end_points, price_s=prices_ts_all)
back_test <- xts::xts(back_test, order.by=start_dates[1:(NROW(start_dates)-1)])



### Perform a similar loop as above, but use sub_set() instead of run_backtest_endpoints()
# Perform loop over equally spaced business days (rows of data),
# and with a stub interval at the beginning.
# I think this is the proper way to do it.

start_point <- which(start_date==index(prices_ts_all))
end_point <- which(end_date==index(prices_ts_all))
n_row <- (end_point - start_point) + 1
look_back <- 252
look_forward <- look_back
num_intervals <- n_row %/% look_back
# Calculate end_points and convert them to dates
end_points <- n_row - num_intervals*look_back + (0:num_intervals)*look_back + start_point - 1
end_points <- index(prices_ts_all[end_points, ])
start_dates <- end_points
# Calculate the number of days in the lookback and look_forward intervals.
# look_back <- rutils::diff_it(c(0, end_points))[-1]
# look_forward <- rutils::lag_it(look_back, -1)


# perform the backtest using run_backtest_subset()
back_test <- t(run_backtest_subset(end_points=end_points[-NROW(end_points)],
                             look_forward=look_forward,
                             look_back=look_back,
                             price_s=prices_ts_all))
back_test <- xts::xts(back_test, order.by=end_points[-NROW(end_points)])




### Perform loop over equally spaced business days (rows of data),
# with an extended lookback interval, and with a stub interval at the beginning.

start_point <- which(start_date==index(prices_ts_all))
end_point <- which(end_date==index(prices_ts_all))
n_row <- (end_point - start_point) + 1
look_forward <- 252
look_back <- 2*look_forward
num_intervals <- n_row %/% look_forward
# Calculate end_points and convert them to dates
end_points <- n_row - num_intervals*look_forward + (0:num_intervals)*look_forward + start_point - 1
end_points <- index(prices_ts_all[end_points, ])
start_dates <- end_points


# perform the backtest using run_backtest_subset()
back_test <- t(run_backtest_subset(end_points=end_points[-NROW(end_points)],
                             look_forward=look_forward,
                             look_back=look_back,
                             price_s=prices_ts_all))
back_test <- xts::xts(back_test, order.by=end_points[-NROW(end_points)])




####################################
### ignore below

# plot interactive dygraphs plot
##dygraph(port_cumret, main="Cumulative PnL") %>%
##  dyOptions(colors=c("orange", "blue")) %>%
##  dyRangeSelector()



