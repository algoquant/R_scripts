####################################
# Scripts for downloading data from external sources,
# and for loading data from files.

library(rutils)


###############
# Load S&P500 constituent stock prices and
# calculate the percentage daily returns scaled 
# by their intraday range.

library(HighFreq)


load("C:/Develop/lecture_slides/data/sp500.RData")

## Calculate the percentage returns
price_s <- eapply(env_sp500, quantmod::Cl)
price_s <- rutils::do_call(cbind, price_s)
# carry forward and backward non-NA prices
price_s <- zoo::na.locf(price_s, na.rm=FALSE)
price_s <- zoo::na.locf(price_s, fromLast=TRUE)
col_names <- unname(sapply(colnames(price_s),
  function(col_name) strsplit(col_name, split="[.]")[[1]][1]))
colnames(price_s) <- col_names
# Calculate percentage returns of the S&P500 constituent stocks
# Calculate percentage returns of the S&P500 constituent stocks
# re_turns <- rutils::diff_it(log(price_s))
# Or
# re_turns <- lapply(price_s, function(x)
#   rutils::diff_it(x)/rutils::lag_it(x))
# re_turns <- rutils::do_call(cbind, re_turns)
re_turns <- rutils::diff_it(price_s)/rutils::lag_it(price_s)
set.seed(1121)
sam_ple <- sample(NCOL(re_turns), s=100, replace=FALSE)
returns_100 <- re_turns[, sam_ple]

## Calculate scaled returns using price range
returns_scaled <- eapply(env_sp500, function(oh_lc) {
  oh_lc <- log(oh_lc)
  # op_en <- Op(oh_lc)
  hi_gh <- Hi(oh_lc)
  lo_w <- Lo(oh_lc)
  clo_se <- Cl(oh_lc)
  # Scale returns using price range
  re_turns <- rutils::diff_it(clo_se)
  rang_e <- as.numeric(hi_gh - lo_w)
  rang_e <- ifelse(rang_e == 0, 1, rang_e)
  # re_turns <- ifelse(rang_e>0, re_turns/rang_e, 0)
  re_turns <- re_turns/rang_e
  # re_turns[is.na(re_turns)] <- 0
  zoo::na.locf(re_turns, na.rm=FALSE)
})  # end eapply

returns_scaled <- rutils::do_call(cbind, returns_scaled)
returns_scaled[is.na(returns_scaled)] <- 0
sum(is.na(returns_scaled))
sum(!is.finite(returns_scaled))
# returns_scaled <- zoo::na.locf(returns_scaled, na.rm=FALSE)
# returns_scaled <- zoo::na.locf(returns_scaled, fromLast=TRUE)
colnames(returns_scaled) <- col_names

returns_100_scaled <- returns_scaled[, sam_ple]

## Save the data
save(price_s, re_turns, returns_scaled, returns_100, returns_100_scaled,
     file="C:/Develop/lecture_slides/data/sp500_prices.RData")

## Save the returns
save(price_s, re_turns, returns_scaled, 
     file="C:/Develop/lecture_slides/data/sp500_returns.RData")



###############
# Read daily futures prices from CSV files and save 
# them into an environment.
# The OHLC futures prices were collected from IB on 
# consecutive days and were saved into CSV files.

# Set parameters for directory with CSV files
sym_bol <- "ES"
data_dir <- "C:/Develop/data/ib_data"
setwd(dir=data_dir)

# Get all CSV file names in the data_dir directory
file_names <- Sys.glob(paste(data_dir, "*.csv", sep="/"))
# Subset to only sym_bol file names
file_names <- file_names[grep(sym_bol, file_names)]
# Subset to only files createed after "2018-10-01"
cut_off <- as.POSIXct("2018-10-01", tz="America/New_York", origin="1970-01-01")
file_names <- file_names[file.info(file_names)$mtime > cut_off]
# Subset to only files createed before "2019-06-30"
cut_off <- as.POSIXct("2019-06-30", tz="America/New_York", origin="1970-01-01")
file_names <- file_names[file.info(file_names)$mtime < cut_off]
# Exclude "ESTSY"
file_names <- file_names[-1]

# Create new environment for data
data_env <- new.env()

# Loop over the file_names, load data from CSV files,
# and save the bar data to binary files
count_er <- 1
for (file_name in file_names) {
  cat("count_er: ", count_er, "\n")
  # Load time series data from CSV file
  oh_lc <- data.table::fread(file_name)
  data.table::setDF(oh_lc)
  if (!is.numeric(oh_lc[1, 1])) {
    # Remove rows with non-numeric datestamps
    oh_lc <- oh_lc[!is.na(as.numeric(oh_lc[, 1])), ]
    oh_lc <- sapply(oh_lc, as.numeric)
  }  # end if
  oh_lc <- xts::xts(oh_lc[, -1], order.by=as.POSIXct(oh_lc[, 1], tz="America/New_York", origin="1970-01-01"))
  # nam_e <- paste0("ES", count_er)
  assign(x=paste0(sym_bol, count_er), value=oh_lc, envir=data_env)
  count_er <- count_er + 1
}  # end for

# Inspect the data
ls(data_env)
do.call(rbind, eapply(data_env, dim))
save(data_env, file="data_env.RData")



###############
# Chain together futures prices and save them into 
# an .RData file.

# Define function for chaining futures prices from 
# consecutive days.
# It adjusts prices by adding the difference in Close 
# prices, instead of multiplying them by a ratio.
# No need for this: Fix chain_ohlc() by using ratio, as in slide Chaining Together Futures Prices in markets_trading.Rnw
# https://www.interactivebrokers.com/en/software/tws/usersguidebook/technicalanalytics/continuous.htm

chain_ohlc <- function(ohlc_1, ohlc_2) {
  if (end(ohlc_1) < start(ohlc_2)) {
    di_ff <- as.numeric(ohlc_2[start(ohlc_2), 4]) - as.numeric(ohlc_1[end(ohlc_1), 4])
    ohlc_1[, c(1:4, 6)] <- ohlc_1[, c(1:4, 6)] + di_ff
    return(rbind(ohlc_1, ohlc_2))
  } else if (end(ohlc_2) < start(ohlc_1)) {
    di_ff <- as.numeric(ohlc_1[start(ohlc_1), 4]) - as.numeric(ohlc_2[end(ohlc_2), 4])
    ohlc_2[, c(1:4, 6)] <- ohlc_2[, c(1:4, 6)] + di_ff
    return(rbind(ohlc_2, ohlc_1))
  } else {
    warning("Overlapping data")
    return(NULL)
  } # end if
}  # end chain_ohlc

chain_ed <- chain_ohlc(data_env$ES1, data_env$ES2)

# Chain the data
oh_lc <- rutils::do_call(chain_ohlc, as.list(data_env)[order(sapply(data_env, start))])
dim(oh_lc)
colnames(oh_lc) <- paste(sym_bol, c("Open", "High", "Low", "Close", "Volume", "WAP", "Count"), sep=".")

# Create new time index
in_deks <- seq.int(from=5*((as.numeric(Sys.time())-NROW(oh_lc)) %/% 5), by=5, length.out=NROW(oh_lc))
in_deks <- as.POSIXct(in_deks, tz="America/New_York", origin="1970-01-01")
oh_lc <- xts::xts(coredata(oh_lc), in_deks)
sum(is.na(oh_lc))

library(dygraphs)
dygraphs::dygraph(xts::to.minutes(oh_lc)[, 1:4]) %>% dyCandlestick()

data_env$oh_lc <- oh_lc
save(data_env, file=paste0(paste(sym_bol, "data_env", sep="_"), ".RData"))
save(oh_lc, file=paste0(sym_bol, "_ohlc.RData"))



###############
# Load futures OHLC prices from RData file.

# Set parameters for directory with CSV files
sym_bol <- "ES"
data_dir <- "C:/Develop/data/ib_data"
setwd(dir=data_dir)

load(file=paste0("C:/Develop/data/ib_data/", sym_bol, "_ohlc.RData"))
dim(oh_lc)


###############
# Chain together VIX futures prices in a loop.
# Perform a for() loop, and one-by-one add to chain_ed 
# the VIX futures prices given by the remaining sym_bols.
# Hint: Adapt code from the slide: Chaining Together Futures Prices.

## This code is for chaining by starting at the last sym_bols.
# It gives a slightly different answer from the homework.

chain_ed <- get(last(sym_bols), envir=vix_env_new)

for (sym_bol in rev(sym_bols)[-1]) {
  cat("Chaining the symbol: ", sym_bol, "\n")
  # Get data for sym_bol
  oh_lc <- get(sym_bol, envir=vix_env_new)
  # Calculate end date of oh_lc
  en_d <- end(oh_lc)
  star_t <- (en_d-30)
  # Calculate start date of chain_ed
  # star_t <- start(chain_ed)
  # cbind overlapping volume data of oh_lc and chain_ed, between star_t and en_d
  over_lap <- paste0(star_t, "/", en_d)
  # vol_ume <- cbind(Vo(chain_ed), Vo(oh_lc))[paste0(star_t, "/", en_d)]
  # vol_ume <- na.omit(vol_ume)
  # Find date when volume of oh_lc first exceeds chain_ed
  exceed_s <- (Vo(oh_lc[over_lap]) < Vo(chain_ed[over_lap]))
  if (sum(exceed_s) > 0) {
    in_dex <- match(TRUE, exceed_s)
    in_dex <- zoo::index(exceed_s[in_dex])
    # Scale the prices
    fac_tor <- as.numeric(Cl(chain_ed[in_dex])/Cl(oh_lc[in_dex]))
  } else {
    in_dex <- NROW(exceed_s)
    in_dex <- zoo::index(exceed_s[in_dex])
    # Scale the prices
    fac_tor <- 1
  }  # end if
  oh_lc[, 1:4] <- fac_tor*oh_lc[, 1:4]
  # Chain oh_lc to chain_ed
  chain_ed <- rbind(oh_lc[index(oh_lc) <= in_dex],
                    chain_ed[index(chain_ed) > in_dex])
}  # end for

# Rename the column names
colnames(chain_ed) <- c("Open", "High", "Low", "Close", "Volume")



###########
# Download multiple symbols from Bloomberg

# install.packages("Rblpapi")
library(Rblpapi)
# Connect R to Bloomberg
bbg_connect <- blpConnect()

## Download daily historical OHLC prices and volume for SPX Index and XLU ETF
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

# Download data from Bloomberg in loop
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
# Load data from CSV files

## Load time series data from a single CSV file

prices_ts <- xts::as.xts(zoo::read.zoo(
  file=file.path("C:/Develop/data",
                 "data prices close 2017-08-31.csv"),
  header=TRUE, sep=",", FUN=as.Date, format="%m/%d/%Y"))
# overwrite NA values
prices_ts <- rutils::na_locf(prices_ts)
prices_ts <- rutils::na_locf(prices_ts, from_last=TRUE)
symbol_s <- c("XLP", "XLU")
prices_ts <- prices_ts[, symbol_s]



## Load time series data from CSV files into an environment.

# Create new environment for data
data_env <- new.env()
data_dir <- "C:/Develop/data/bbg_records"
# sym_bols <- c("SPX", "VIX")
# file_names <- paste0(sym_bols, ".csv")
file_names <- dir(data_dir)
sym_bols <- rutils::get_name(file_names)

# Subset sym_bols by removing currency symbols
sub_symbols <- sym_bols[-grep("USD", sym_bols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("EUR", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("UST", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("JGB", sub_symbols, ignore.case=TRUE)]
sub_symbols <- sub_symbols[-grep("GDB", sub_symbols, ignore.case=TRUE)]


# Load data from CSV files into the environment
out <- rutils::get_data(sym_bols=sub_symbols,
                        data_dir=data_dir,
                        data_env=data_env,
                        e_cho=FALSE)


## Extract the closing prices into a single xts time series

# price_s <- lapply(as.list(data_env)[sym_bols], quantmod::Cl)
# Flatten (cbind) prices into single xts series
# price_s <- rutils::do_call(cbind, price_s)

price_s <- rutils::get_col(oh_lc=ls(data_env),
                           data_env=data_env)
# overwrite NA values
price_s <- rutils::na_locf(price_s)
price_s <- rutils::na_locf(price_s, from_last=TRUE)
# Save column names
col_names <- rutils::get_name(colnames(price_s))




###############
# Load and save OHLC bar data

library(HighFreq)

## Load ES1 futures data from binary file
load(file="C:/Develop/data/ES1.RData")
# or
# Load ES1 futures data from CSV file
oh_lc <- read.zoo(file="C:/Develop/data/bar_data/ES1.csv",
                  header=TRUE, sep=",",
                  drop=FALSE, format="%Y-%m-%d %H:%M",
                  FUN=as.POSIXct, tz="America/New_York")
# Coerce to xts series
oh_lc <- as.xts(oh_lc)
# Subset to trading hours
oh_lc <- oh_lc["T09:00:00/T16:30:00"]
# Save the bar data to binary file
save(oh_lc, file="C:/Develop/data/ES1.RData")



## Read IB futures data from CSV files

# Read file names
file_names <- scan(file="C:/Develop/data/bar_data/etf_file_names.txt", what=character(), sep=",")

# Remember the cwd
c_wd <- getwd()
# Set the cwd to the file directory
file_dir <- strsplit(file_names[1], split="/")[[1]]
file_dir <- file_dir[-NROW(file_dir)]
file_dir <- paste(file_dir, collapse="/")
# or
# file_dir <- do.call(file.path, as.list(file_dir))
setwd(dir=file_dir)

# Loop over the file_names, load data from CSV files,
# and save the bar data to binary files
for (file_name in file_names) {
  file_name <- strsplit(file_name, split="/")[[1]]
  file_name <- file_name[NROW(file_name)]
  # Load time series data from CSV file
  oh_lc <- read.zoo(file=file_name,
                    header=TRUE, sep=",",
                    drop=FALSE, format="%Y-%m-%d %H:%M",
                    FUN=as.POSIXct, tz="America/New_York")
  # Coerce to xts series
  oh_lc <- as.xts(oh_lc)
  sym_bol <- strsplit(file_name, split="[.]")[[1]][1]
  # Rename column names
  colnames(oh_lc) <- paste(sym_bol, colnames(oh_lc), sep=".")
  # Subset to trading hours
  # oh_lc <- oh_lc["T09:00:00/T16:30:00"]
  # Save the bar data to binary file
  save(oh_lc, file=paste0(sym_bol, ".RData"))
}  # end for

# Restore the cwd
setwd(dir=c_wd)



## Download VIX CSV files from CBOE

# Old stuff - ignore?
# Download text data from URL - doesn't download data - just html code
foo <- RCurl::getURL("https://markets.cboe.com/us/futures/market_statistics/historical_data/")
foo <- readLines("https://markets.cboe.com/us/futures/market_statistics/historical_data/")
foo <- read.csv("https://markets.cboe.com/us/futures/market_statistics/historical_data/")
class(foo)
NROW(foo)
bar <- grep(glob2rx("*.csv"), foo)
# end old stuff

# Read CBOE futures expiration dates
date_s <- read.csv(file="C:/Develop/R/data/futures_expiration_dates_codes.csv",
                   stringsAsFactors=FALSE, row.names=1)
data_dir <- "C:/Users/Jerzy/Downloads/vix_data"
# dir.create(data_dir)
sym_bols <- rownames(date_s)
# Select only some sym_bols
se_lect <- 19:NROW(sym_bols)
sym_bols <- sym_bols[se_lect]
file_names <- file.path(data_dir, paste0(sym_bols, ".csv"))
log_file <- file.path(data_dir, "log_file.txt")
cboe_url <- "https://markets.cboe.com/us/futures/market_statistics/historical_data/products/csv/VX/"
url_s <- paste0(cboe_url, date_s[se_lect, 1])
# Download files in loop
for (it in seq_along(url_s)) {
  tryCatch(  # Warning and error handler
    download.file(url_s[it],
                  destfile=file_names[it], quiet=TRUE),
    # Warning handler captures warning condition
    warning=function(warning_cond) {
      cat(paste("warning handler: ", warning_cond, "\n"), file=log_file, append=TRUE)
    },  # end warning handler
    # Error handler captures error condition
    error=function(error_cond) {
      cat(paste("error handler: ", error_cond, "\n"), append=TRUE)
    },  # end error handler
    finally=cat(paste("Processing file name =", file_names[it], "\n"), append=TRUE)
  )  # end tryCatch
}  # end for



## Read CBOE futures data from CSV files

# Define utility function
# last <- function(x) x[NROW(x)]

# Load VIX futures data from binary file
load(file="C:/Develop/data/vix_data/vix_cboe.RData")
col_names <- colnames(vix_env$VXF8)

# Get all CSV file names in the data_dir directory
# data_dir <- "C:/Users/Jerzy/Downloads/vix_data"
data_dir <- "C:/Develop/data/vix_data/"
file_names <- Sys.glob(paste(data_dir, "VX*.csv", sep="/"))

vix_env_new <- new.env()

# Loop over the file_names, load data from CSV files,
# and copy the bar data to vix_env_new
for (file_name in file_names) {
  # Load time series data from CSV file
  oh_lc <- read.zoo(file=file_name,
                    header=TRUE, sep=",")
  colnames(oh_lc)[8] <- col_names[5]
  oh_lc <- oh_lc[, col_names]
  # Coerce data to numeric and make xts
  oh_lc <- xts(apply(oh_lc, 2, as.numeric), index(oh_lc))
  # Create sym bol from file name
  sym_bol <- strsplit(file_name, split="/")[[1]]
  sym_bol <- strsplit(last(sym_bol), split="[.]")[[1]][1]
  assign(x=sym_bol, value=oh_lc, envir=vix_env_new)
}  # end for

# Copy from vix_env_new to vix_env
rm(VXN8, envir=vix_env_new)
sym_bols <- ls(vix_env_new)
for (sym_bol in sym_bols) {
  rm(sym_bol, envir=vix_env)
  assign(x=sym_bol, value=get(sym_bol, envir=vix_env_new), envir=vix_env)
}  # end for

save(vix_env, file="C:/Develop/data/vix_data/vix_cboe.RData")



## Load futures data from RData files

# Read the symbols
sym_bols <- scan(file="C:/Develop/data/bar_data/etf_symbols.txt", what=character(), sep=",")
# Specify the file directory
file_dir <- "C:/Develop/data/bar_data/"
# Specify new environment for data
etf_env <- new.env()
# Specify the file names
# file_names <- paste0(file_dir, sym_bols, ".RData")

# Load data in a loop and copy into etf_env
for (sym_bol in sym_bols) {
  # Specify the file name
  file_name <- paste0(file_dir, sym_bol, ".RData")
  load_ed <- load(file=file_name)
  assign(x=sym_bol, value=get(load_ed), envir=etf_env)
}  # end for


## Combine the ETF series of prices into a single xts series and save it into etf_env

# Extract only first 4 OHLC price columns from each ETF series
assign(x="oh_lc",
       value=rutils::do_call(cbind, eapply(etf_env, function(x_ts) x_ts[, 1:4])),
       envir=etf_env)
# oh_lc <- rutils::do_call(cbind, eapply(etf_env, function(x_ts) x_ts[, 1:4]))
etf_env$oh_lc <- na.omit(etf_env$oh_lc)
# Subset to trading hours
etf_env$oh_lc <- etf_env$oh_lc["T09:00:00/T16:30:00"]
# Save the bar data to binary file
save(etf_env, file=paste0(file_dir, "etf_series.RData"))



## Load futures data from binary files and combine into a single xts series
# First load ES1 data and extract only first 4 OHLC price columns
load(file="C:/Develop/data/ES1.RData")
com_bo <- oh_lc[, 1:4]
colnames(com_bo) <- paste0("ES1.", colnames(com_bo))
# Next load TU1 data and cbind it to ES1 data
load(file="C:/Develop/data/TU1UST2yr.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("TU1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
# Next load TY1 data and cbind it to ES1 data
load(file="C:/Develop/data/TY1UST10yr.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("TY1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
# Next load UX1 data and cbind it to ES1 data
load(file="C:/Develop/data/UX1_VIX.RData")
oh_lc <- oh_lc[, 1:4]
colnames(oh_lc) <- paste0("UX1.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)
load(file="C:/Develop/data/UX2_VIX.RData")
oh_lc <- oh_lc[, 1:4]
# Next load UX1 data and cbind it to ES1 data
colnames(oh_lc) <- paste0("UX2.", colnames(oh_lc))
com_bo <- cbind(com_bo, oh_lc)

# Combine into a single xts series
oh_lc <- na.omit(com_bo)
# Save the bar data to binary file
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


# Coerce bbg_data from data frame to xts
# bbg_data <- xts::as.xts(bbg_data)
# need to verify date format
bbg_data <- xts::xts(bbg_data[, bbg_fields],
                     order.by = as.Date(bbg_data[, "date"], format="%Y-%m-%d"))

# Coerce bbg_data from data frame to xts
# bbg_data <- xts::as.xts(bbg_data)
# bbg_data <- xts::xts(bbg_data, order.by = as.Date(bbg_data[, "date"]))
# bbg_data
# write bbg_data to CSV file
zoo::write.zoo(bbg_data, file = file_name, sep=",")


# write bbg_data to CSV files
zoo::write.zoo(bbg_data, file="bbg_data.csv", sep=",")


## Download daily historical close prices and daily volume for SPX Index and XLU ETF



# Bloomberg script for a list of symbols





