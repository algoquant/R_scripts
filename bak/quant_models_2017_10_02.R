####################################
### Scripts for performing backtest simulations of trading strategies

### Change log

## 2017-10-01: major rewrite of scripts
# Added plotting rolling volume-weighted prices.
# Data loading function rutils::get_data() imported from package rutils.

## 2017-09-19: major rewrite of scripts
# Added plotting.

####################################


###########
## Install packages rutils and HighFreq from github
# install.packages("dygraphs")
# devtools::install_github("algoquant/rutils")
# devtools::install_github("algoquant/HighFreq")
# devtools::install_github("jjf234/roll")
# devtools::install_github("kevinushey/RcppRoll")
library(HighFreq)
# library(plotly)


###########
### Load data from csv files

## Load time series data from csv files into an environment.

# create new environment for data
data_env <- new.env()
dir_data <- "C:/Develop/data_bbg_records"
# sym_bols <- c("SPX", "VIX")
# file_names <- paste0(sym_bols, ".csv")
file_names <- dir(dir_data)
sym_bols <- rutils::get_name(file_names)

# subset sym_bols by removing currency symbols
sub_symbols <- sym_bols[-grep("USD", sym_bols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("EUR", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("UST", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("JGB", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("GDB", sub_symbols, ignore.case=TRUE)]


# load data from csv files into the environment
out <- rutils::get_data(sym_bols=sub_symbols,
                        data_dir=dir_data,
                        data_env=data_env,
                        e_cho=FALSE)


## Extract the closing prices and cbind them together
# into into a single xts time series

# price_s <- lapply(as.list(data_env)[sym_bols], quantmod::Cl)
# flatten (cbind) prices into single xts series
# price_s <- rutils::do_call(cbind, price_s)

price_s <- rutils::get_col(oh_lc=ls(data_env),
                           data_env=data_env)
# overwrite NA values
price_s <- rutils::na_locf(price_s)
price_s <- rutils::na_locf(price_s, from_last=TRUE)
# sum(is.na(price_s))
# save column names
col_names <- rutils::get_name(colnames(price_s))



###########
### Plotting

library(dygraphs)
library(htmltools)

## plot candlesticks without shading.
rutils::chart_xts(data_env$EEM["2017"])

## plot candlesticks with vertical background shading.
# select EEM
oh_lc <- data_env$EEM
# calculate volume-weighted average price
v_wap <- HighFreq::roll_vwap(oh_lc, look_back=22)
# plot candlesticks with vertical background shading and trading volume.
rutils::chart_xts(data_env$EEM["2017"], 
                  name="EEM plus VWAP",
                  TA="add_Vo(); add_TA(v_wap['2017'], col='red', lwd=2, on=1)",
                  in_dic=(data_env$EEM["2017", 4] > v_wap["2017"]))


## plot two instruments with two y-axis

# plot line plot in x11 window two instruments with two y-axis
rutils::chart_xts2y(price_s[, c("EEM.Close", "SPY.Close")])

# plot dygraphs for two instruments with two y-axis
rutils::chart_dygraph2y(price_s[, c("EEM.Close", "SPY.Close")])


## plot line plots in x11 windows for several instruments in a loop

for (i in (NCOL(price_s)-5):NCOL(price_s)) {
  # cat("Plot of: ", sym_bols[i], "\n")
  # plot(zoo::coredata(price_s[, i]), main=paste("Plot of:", sym_bols[i]), t="l")
  # plot(quantmod::chart_Series(price_s[, i], name=paste("Plot of:", sym_bols[i])))
  # the fig.width and fig.height chunck options are ignored by dygraphs, so need to use the width and height parameters directly
  rutils::chart_xts(price_s[, i], name=paste("Plot of:", col_names[i]))
  NULL
}  # end for


## plot dygraphs for all instruments in a loop
# !!! this takes some time !!!
# create a list of dygraphs objects in loop and then render them
lapply(1:NCOL(price_s), function(i) {
  dygraphs::dygraph(price_s[, i],
                    main=paste("Plot of:", col_names[i]),
                    width=600, height=400)
}) %>% # end lapply
  # render the dygraphs objects
  htmltools::tagList() %>% htmltools::browsable()




###########
### Rolling variance


# calculate the Yang-Zhang rolling daily variance using roll_variance()
look_back <- 22
var_rolling <- eapply(data_env, roll_variance, look_back=look_back, sca_le=FALSE)
# sapply(var_rolling, function(x) sum(is.na(x)))

# plot rolling variance and volume
rutils::chart_xts(var_rolling$INDU, 
                  TA="add_TA(rutils::get_col(data_env$INDU, 'Vo'), name='INDU volume')",
                  name="INDU rolling monthly variance")



###########
### Correlation and cointegration heatmaps

re_turns <- lapply(price_s, rutils::diff_xts)
in_dex <- rutils::diff_it(as.numeric(xts::.index(price_s)))
in_dex[in_dex==0] <- 1
re_turns <- rutils::do_call(cbind, re_turns)/in_dex
# overwrite NA values
sum(is.na(re_turns))
# re_turns <- rutils::na_locf(re_turns)
# re_turns <- rutils::na_locf(re_turns, from_last=TRUE)

# covariance matrix and variance vector of returns
re_turns <- scale(re_turns)
cov_mat <- (t(re_turns) %*% re_turns) / (NROW(re_turns)-1)
vari_ance <- diag(cov_mat)
cor_mat <- cov_mat / sqrt(vari_ance)
cor_mat <- t(t(cor_mat) / sqrt(vari_ance))
# cor_mat <- cor(re_turns)
# reorder correlation matrix based on clusters
library(corrplot)
or_der <- corrMatOrder(cor_mat, 
                       order="hclust", 
                       hclust.method="complete")
cor_mat <- cor_mat[or_der, or_der]
# plot the correlation matrix
col_ors <- colorRampPalette(c("red", "white", "blue"))
x11()
corrplot(cor_mat, title="Correlation Matrix", 
         tl.col="black", tl.cex=0.8, mar=c(0,0,1,0), 
         method="square", col=col_ors(8), 
         cl.offset=0.75, cl.cex=0.7, 
         cl.align.text="l", cl.ratio=0.25)
# draw rectangles on the correlation matrix plot
corrRect.hclust(cor_mat, k=NROW(cor_mat) %/% 2, 
                method="complete", col="red")





####################################
### ignore below


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

