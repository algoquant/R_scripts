####################################
### Scripts for performing backtest simulations of trading strategies

### Change log

## 2017-09-19: major rewrite of scripts
# added rolling volume-weighted prices
# added plotting

###########

## Install packages rutils and HighFreq from github
# install.packages("dygraphs")
# devtools::install_github("algoquant/rutils")
# devtools::install_github("algoquant/HighFreq")
library(HighFreq)
# library(plotly)

## Load the custom functions
source("C:/Develop/R/scripts/quant_models_func_2017_09_30.R")



###########
### data loading scripts


## Load time series data from csv files, and cbind them 
# together into one xts time series.

## Read time series data from csv files
# sym_bols <- c("XLU", "XLP")
sym_bols <- c("SPX", "VIX")
price_s <- load_data(paste0(sym_bols, ".csv"))
# price_s <- xts::as.xts(zoo::read.zoo(
#   file=file.path("C:/Develop/data",
#                  "data prices close 2017-08-31.csv"),
#   header=TRUE, sep=",", FUN=as.Date, format="%m/%d/%Y"))
# head(price_s)
# NROW(price_s)
# remove NAs
price_s <- price_s[, -match("VIX.Volume", colnames(price_s))]
price_s <- na.omit(price_s)
sum(is.na(price_s))

## Subset the time series (historical window of prices)
# start_date <- "2015-05-29"
# end_date <- "2016-05-26"
# start_date <- start(price_s)
# end_date <- end(price_s)
# price_s <- price_s[paste(start_date, end_date, sep="/")]

# subset close prices
price_s <- price_s[, paste0(sym_bols, ".Close")]
col_names <- unname(sapply(price_s, rutils::na_me))
colnames(price_s) <- col_names



## Load time series data from csv files, and 
# save the separate xts time series into an environment.

# create new environment for data
data_env <- new.env()
load_env(file_names=paste0(sym_bols, ".csv"),
         data_env=data_env)


## Extract the closing prices into a single xts time series
price_s <- lapply(as.list(data_env)[sym_bols], quantmod::Cl)
# flatten (cbind) prices into single xts series
price_s <- rutils::do_call(cbind, price_s)
# remove NA values
price_s <- rutils::na_locf(price_s)
price_s <- rutils::na_locf(price_s, from_last=TRUE)
sum(is.na(price_s))
# rename columns
colnames(price_s) <- rutils::get_name(colnames(price_s), separator="[.]")



###########
### plotting scripts

# dygraphs plot with two y-axes
# library(dygraphs)
rutils::chart_dygraph2y(price_s)

# zoo plot with two y-axes
rutils::chart_xts2y(price_s)


# flexdashboard





###########
### rolling variance and correlation scripts







###########
### model scripts


### Define model parameters
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
  sym_bols=sym_bols,
  model_type=model_type,
  flag_print=flag_print,
  flag_plot=flag_plot
)  # end list


trading_params <- list(
  sym_bols=sym_bols,
  trading_levels=trading_levels,
  flag_sticky_stops=flag_sticky_stops,
  flag_print=flag_print,
  flag_plot=flag_plot
)  # end list



### calibrate and run (test) models using the whole time series

# calibrate model
calibrated_model <- train_model(time_series = price_s,
                                model_params = model_params)
all.equal(res_hr, calibrated_model)


# run model
trade_output <- trade_model(time_series = price_s,
                            calibrated_model = calibrated_model,
                            trading_params = trading_params)
all.equal(res_tp, trade_output)




###########
### Backtesting scripts

## Perform backtest simulations of model over a sliding lookback window

## Define backtest parameters
# start_date <- "2007-12-31"
start_date <- start(price_s)
# end_date <- "2010-05-27"
end_date <- end(price_s)
price_s <- price_s[paste(start_date, end_date, sep="/")]
start_point <- which(start_date==index(price_s))
end_point <- which(end_date==index(price_s))



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
end_points <- rutils::calc_endpoints(price_s, look_forward)
end_points <- index(price_s[end_points, ])



# perform the backtest using roll_backtest()
back_test <- t(roll_backtest(end_points=end_points[-NROW(end_points)],
                            look_forward=look_forward,
                            look_back=look_back,
                            train_model = train_model,
                            model_params = model_params,
                            trade_model = trade_model,
                            trading_params = trading_params,
                            price_s=price_s))
back_test <- xts::xts(back_test, order.by=end_points[-NROW(end_points)])


back_test <- HighFreq::roll_backtest(end_points=rutils::calc_endpoints(price_s, look_forward),
                           look_forward=look_forward,
                           look_back=look_back,
                           train_func = train_model,
                           trade_func = trade_model,
                           model_params = model_params,
                           trading_params = trading_params,
                           x_ts=price_s)

all.equal(back_test_old, back_test)






####################################
### legacy code below

