####################################
### Scripts for performing backtest simulations of trading strategies
# These are legacy scripts to replicate the analysis for "XLU" and "XLP"
### stat-arb-sharpe-ratio script

### Install packages rutils and HighFreq from github
devtools::install_github("algoquant/rutils")
devtools::install_github("algoquant/HighFreq")
library(HighFreq)
library(plotly)

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

### Define model parameters
name_x <- "XLU"
name_y <- "XLP"
symbol_s <- c("XLP", "XLU")
model_type <- 1 # 0: OLS crossing the origin, 1: (default) ols, 2: TLS, 3: no-drift index, 4: no-drift dates
flag_sticky_stops <- TRUE  # use sticky stops, FALSE use regular stops
flag_print <- FALSE
flag_plot <- FALSE

z_step <- 0.01
z_long_stop <- -2.5
n_step <- -z_long_stop / z_step
sharpe_ratio <- matrix(0, nrow = n_step, ncol = n_step)
long_enter <- z_long_stop + z_step * 1:n_step
long_profit <- z_long_stop + z_step * 1:n_step

for(index_z_long_enter in 1:n_step) {
  z_long_enter <- z_long_stop + z_step * index_z_long_enter 
  #print(z_long_enter)
  
  #for(z_long_profit in seq(z_long_enter + z_step, 0, by = z_step)) {
  for(index_z_long_profit in index_z_long_enter:n_step){
    z_long_profit <- z_long_stop + z_step * index_z_long_profit 
    #print(z_long_profit)
    if(z_long_profit == z_long_enter) next

    trading_levels <- list(z_long_enter = z_long_enter,
                           z_long_profit = z_long_profit,
                           z_long_stop = z_long_stop,
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


    # calibrate model
    calibrated_model <- invisible(train_model(time_series = prices_ts,
                                    model_params = model_params))
    # run model
    trade_output <- invisible(trade_model(time_series = prices_ts,
                                calibrated_model = calibrated_model,
                                trading_params = trading_params))
  
    cat("z_long_enter=", trading_levels$z_long_enter, "\tz_long_profit=", trading_levels$z_long_profit, "\tSharpe Ratio=", trade_output["port_sharpe_ratio"], "\n")
#    cat(sprintf("%s %.2f %s %.2f %s %.6f \n", "z_long_enter=", z_long_enter,
#                "z_long_profit=", z_long_profit, 
#                "sharpe_ratio=", trade_output["port_sharpe_ratio"]))
    sharpe_ratio[index_z_long_enter, index_z_long_profit] <- trade_output["port_sharpe_ratio"]
  }
}
plot_ly(x = ~long_profit, y = ~long_enter, z = ~sharpe_ratio, type = "contour")
