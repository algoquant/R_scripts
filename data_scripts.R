####################################
### Scripts for downloading data from external sources,
### and for loading data from files.

library(rutils)



###########
### Download multiple symbols from Bloomberg

# install.packages("Rblpapi")
library(Rblpapi)
# connect R to Bloomberg
bbg_connect <- blpConnect()

## download daily historical OHLC prices and volume for SPX Index and XLU ETF
# bbg_symbols <- "SPX Index"
bbg_symbols <- c("SPX Index", "XLU US Equity")
bbg_fields <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST", "VOLUME")
start_date <- as.Date("2017-08-01")
data_dir <- "C:/Develop/data/bbg_data"
file_names <- file.path(data_dir,
  paste0(gsub(bbg_symbols, pattern=" ", replacement="_"), ".csv"))

# bbg_data <- bdh(securities = bbg_symbols,
#                 fields = bbg_fields,
#                 start.date = start_date)

# download data from Bloomberg in loop
lapply(seq_along(bbg_symbols), function(in_dex) {
  sym_bol <- bbg_symbols[in_dex]
  bbg_data <- xts::as.xts(Rblpapi::bdh(securities = sym_bol,
                                       fields = bbg_fields,
                                       start.date = start_date))
  file_name <- file.path(data_dir,
                         paste0(gsub(sym_bol, pattern=" ", replacement="_"), ".csv"))
  zoo::write.zoo(bbg_data, file = file_name, sep=",")
  sym_bol
})  # end lapply



###########
### Load data from csv files

## Load time series data from a single csv file

prices_ts <- xts::as.xts(zoo::read.zoo(
  file=file.path("C:/Develop/data",
                 "data prices close 2017-08-31.csv"),
  header=TRUE, sep=",", FUN=as.Date, format="%m/%d/%Y"))
# overwrite NA values
prices_ts <- rutils::na_locf(prices_ts)
prices_ts <- rutils::na_locf(prices_ts, from_last=TRUE)
symbol_s <- c("XLP", "XLU")
prices_ts <- prices_ts[, symbol_s]



## Load time series data from csv files into an environment.

# create new environment for data
data_env <- new.env()
dir_data <- "C:/Develop/data/bbg_records"
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


## Extract the closing prices into a single xts time series

# price_s <- lapply(as.list(data_env)[sym_bols], quantmod::Cl)
# flatten (cbind) prices into single xts series
# price_s <- rutils::do_call(cbind, price_s)

price_s <- rutils::get_col(oh_lc=ls(data_env),
                           data_env=data_env)
# overwrite NA values
price_s <- rutils::na_locf(price_s)
price_s <- rutils::na_locf(price_s, from_last=TRUE)
# save column names
col_names <- rutils::get_name(colnames(price_s))




####################################
# ignore below


# coerce bbg_data from data frame to xts
# bbg_data <- xts::as.xts(bbg_data)
# need to verify date format
bbg_data <- xts::xts(bbg_data[, bbg_fields],
                     order.by = as.Date(bbg_data[, "date"], format="%Y-%m-%d"))

# coerce bbg_data from data frame to xts
# bbg_data <- xts::as.xts(bbg_data)
# bbg_data <- xts::xts(bbg_data, order.by = as.Date(bbg_data[, "date"]))
# bbg_data
# write bbg_data to CSV file
zoo::write.zoo(bbg_data, file = file_name, sep=",")


# write bbg_data to CSV files
zoo::write.zoo(bbg_data, file="bbg_data.csv", sep=",")


## download daily historical close prices and daily volume for SPX Index and XLU ETF



### Bloomberg script for a list of symbols





