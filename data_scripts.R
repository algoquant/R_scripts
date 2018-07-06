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




###############
### Load and save OHLC bar data

library(HighFreq)

## Load ES1 futures data from binary file
load(file="C:/Develop/data/ES1.RData")
# or
# load ES1 futures data from CSV file
oh_lc <- read.zoo(file="C:/Develop/data/bar_data/ES1.csv", 
                  header=TRUE, sep=",",
                  drop=FALSE, format="%Y-%m-%d %H:%M",
                  FUN=as.POSIXct, tz="America/New_York")
# coerce to xts series
oh_lc <- as.xts(oh_lc)
# subset to trading hours
oh_lc <- oh_lc["T09:00:00/T16:30:00"]
# save the bar data to binary file
save(oh_lc, file="C:/Develop/data/ES1.RData")


## Load futures data from CSV files

# read file names
file_names <- scan(file="C:/Develop/data/bar_data/etf_file_names.txt", what=character(), sep=",")

# remember the cwd
c_wd <- getwd()
# set the cwd to the file directory
file_dir <- strsplit(file_names[1], split="/")[[1]]
file_dir <- file_dir[-NROW(file_dir)]
file_dir <- paste(file_dir, collapse="/")
# or
# file_dir <- do.call(file.path, as.list(file_dir))
setwd(dir=file_dir)

# loop over the file_names, load data from CSV files,
# and save the bar data to binary files
for (file_name in file_names) {
  file_name <- strsplit(file_name, split="/")[[1]]
  file_name <- file_name[NROW(file_name)]
  # load time series data from CSV file
  oh_lc <- read.zoo(file=file_name, 
                    header=TRUE, sep=",",
                    drop=FALSE, format="%Y-%m-%d %H:%M",
                    FUN=as.POSIXct, tz="America/New_York")
  # coerce to xts series
  oh_lc <- as.xts(oh_lc)
  sym_bol <- strsplit(file_name, split="[.]")[[1]][1]
  # rename column names
  colnames(oh_lc) <- paste(sym_bol, colnames(oh_lc), sep=".")
  # subset to trading hours
  # oh_lc <- oh_lc["T09:00:00/T16:30:00"]
  # save the bar data to binary file
  save(oh_lc, file=paste0(sym_bol, ".RData"))
}  # end for

# restore the cwd
setwd(dir=c_wd)


## Load futures data from RData files

# read the symbols
sym_bols <- scan(file="C:/Develop/data/bar_data/etf_symbols.txt", what=character(), sep=",")
# specify the file directory
file_dir <- "C:/Develop/data/bar_data/"
# specify new environment for data
env_etf <- new.env()
# specify the file names
# file_names <- paste0(file_dir, sym_bols, ".RData")

# load data in a loop and copy into env_etf
for (sym_bol in sym_bols) {
  # specify the file name
  file_name <- paste0(file_dir, sym_bol, ".RData")
  load_ed <- load(file=file_name)
  assign(x=sym_bol, value=get(load_ed), envir=env_etf)
}  # end for


## Combine the ETF series of prices into a single xts series and save it into env_etf

# extract only first 4 OHLC price columns from each ETF series
assign(x="oh_lc", 
       value=rutils::do_call(cbind, eapply(env_etf, function(x_ts) x_ts[, 1:4])), 
       envir=env_etf)
# oh_lc <- rutils::do_call(cbind, eapply(env_etf, function(x_ts) x_ts[, 1:4]))
env_etf$oh_lc <- na.omit(env_etf$oh_lc)
# subset to trading hours
env_etf$oh_lc <- env_etf$oh_lc["T09:00:00/T16:30:00"]
# save the bar data to binary file
save(env_etf, file=paste0(file_dir, "etf_series.RData"))



## Load futures data from binary files and combine into a single xts series
# first load ES1 data and extract only first 4 OHLC price columns
load(file="C:/Develop/data/ES1.RData")
com_bo <- oh_lc[, 1:4]
colnames(com_bo) <- paste0("ES1.", colnames(com_bo))
# next load TU1 data and cbind it to ES1 data
load(file="C:/Develop/data/TU1UST2yr.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("TU1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
# next load TY1 data and cbind it to ES1 data
load(file="C:/Develop/data/TY1UST10yr.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("TY1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
# next load UX1 data and cbind it to ES1 data
load(file="C:/Develop/data/UX1_VIX.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("UX1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
load(file="C:/Develop/data/UX2_VIX.RData")
oh_lc <- oh_lc[, 1:4]
# next load UX1 data and cbind it to ES1 data
colnames(oh_lc) <- paste0("UX2.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)

# combine into a single xts series
oh_lc <- na.omit(com_bo)
# save the bar data to binary file
save(com_bo, file="C:/Develop/data/combined.RData")
# load(file="C:/Develop/data/combined.RData")

# plot dygraph
label_s <- c("TY1.Close", "TU1.Close")
# dygraphs::dygraph(cbind(clo_se, da_ta())["2018-02-09"], main="OHLC Technicals Strategy") %>%
dygraphs::dygraph(oh_lc[endpoints(oh_lc, on="hours"), label_s], main="OHLC Data") %>%
  dyAxis("y", label=label_s[1], independentTicks=TRUE) %>%
  dyAxis("y2", label=label_s[2], independentTicks=TRUE) %>%
  dySeries(label_s[2], axis="y2", col=c("blue", "red"))



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





