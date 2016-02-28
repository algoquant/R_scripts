#################################
###  initial code for developing package HighFreq

###########
# scrubbing functions

### identify suspect bid_offer values in univariate xts time series
extreme_values <- function(bid_offer, vol_window=51, thresh_old=2) {
  
  stopifnot(
    (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      ("package:caTools" %in% search()) || require("caTools", quietly=TRUE)
  )
  
  # calculate vo_lat as running quantile
  vo_lat <- runquantile(x=abs(as.vector(bid_offer)), k=vol_window, probs=0.9, endrule="constant", align="center")
  vo_lat <- xts(vo_lat, order.by=index(bid_offer))
  colnames(vo_lat) <- "volat"
  # carry forward non-zero vo_lat values
  vo_lat[vo_lat==0] <- NA
  vo_lat[1] <- 1
  vo_lat <- na.locf(vo_lat)
  #  vo_lat <- na.locf(vo_lat, fromLast=TRUE)
  
  # find suspect values
  # suspect if bid_offer greater than vo_lat
  sus_pect <- (abs(bid_offer) > 2*thresh_old*vo_lat)
  sus_pect[1] <- FALSE
  
  cat("date:", format(as.Date(index(first(bid_offer)))), "\tscrubbed", sum(sus_pect), "suspect bid-offer values\n")
  sus_pect
}  # end extreme_values


### identify suspect jump values in univariate xts price time series
jump_values <- function(price_data, vol_window=51, thresh_old=2) {
  
  stopifnot(
    (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      ("package:caTools" %in% search()) || require("caTools", quietly=TRUE)
  )
  
  # calculate simple returns
  diff_prices <- diff(price_data)
  diff_prices[1, ] <- 0
  colnames(diff_prices) <- "diffs"
  diff_prices_fut <- lag(diff_prices, -1)
  diff_prices_fut[nrow(diff_prices_fut)] <- 0
  colnames(diff_prices_fut) <- "diff_prices_fut"
  
  # calculate vo_lat as running quantile
  vo_lat <- runquantile(x=abs(as.vector(diff_prices)), k=vol_window, probs=0.9, endrule="constant", align="center")
  vo_lat <- xts(vo_lat, order.by=index(diff_prices))
  colnames(vo_lat) <- "volat"
  # carry forward non-zero vo_lat values
  vo_lat[vo_lat==0] <- NA
  vo_lat[1] <- 1
  vo_lat <- na.locf(vo_lat)
  #  vo_lat <- na.locf(vo_lat, fromLast=TRUE)
  
  # find suspect values
  # suspect if abs diffs greater than vo_lat, and if abs sum of diffs less than vo_lat
  sus_pect <- (
    (abs(diff_prices) > thresh_old*vo_lat) & 
      (abs(diff_prices_fut) > thresh_old*vo_lat) & 
      (abs(diff_prices+diff_prices_fut) < 2*thresh_old*vo_lat)
  )
  sus_pect[1] <- FALSE
  colnames(sus_pect) <- "suspect"
  # cat("Parsing", deparse(substitute(taq_data)), "\n")
  # cat("Parsing", strsplit(deparse(substitute(taq_data)), split="[.]")[[1]][4], "on date:", format(to_day), "\tscrubbed", sum(sus_pect), "suspect values\n")
  cat("date:", format(as.Date(index(first(price_data)))), "\tscrubbed", sum(sus_pect), "suspect jump values\n")
  sus_pect
}  # end jump_values



### scrub and aggregate a single day of TAQ data in xts format
# return mid price and volume
scrub_agg <- function(taq_data, vol_window=51, thresh_old=2) {
  
  stopifnot(
    (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      (("package:lubridate" %in% search()) || require("lubridate", quietly=TRUE))
  )
  
  # convert time index to New_York
  index(taq_data) <- with_tz(index(taq_data), "America/New_York")
  # subset data to NYSE trading hours
  taq_data <- taq_data['T09:30:00/T16:00:00', ]
  # return NULL if no data
  if (nrow(taq_data)==0)  return(NULL)
  to_day <- as.Date(index(first(taq_data)))
  
  # remove duplicate time stamps using duplicated
  taq_data <- taq_data[!duplicated(index(taq_data)), ]
  
  # scrub quotes with suspect bid-offer spreads
  bid_offer <- taq_data[, 'Ask.Price'] - taq_data[, 'Bid.Price']
  #  bid_offer <- na.omit(bid_offer)
  sus_pect <- extreme_values(bid_offer)
  # remove suspect values
  taq_data <- taq_data[!sus_pect]
  # replace suspect values
  # taq_data[sus_pect, "Bid.Price"] <- taq_data[sus_pect, "Trade.Price"]
  # taq_data[sus_pect, "Ask.Price"] <- taq_data[sus_pect, "Trade.Price"]
  
  # scrub quotes with suspect price jumps
  # calculate mid prices
  mid_prices <- 0.5 * (taq_data[, "Bid.Price"] + taq_data[, "Ask.Price"])
  #  mid_prices <- na.omit(mid_prices)
  colnames(mid_prices) <- "Mid.Price"
  # replace suspect values with NA
  mid_prices[jump_values(mid_prices)] <- NA
  mid_prices <- na.locf(mid_prices)
  #  mid_prices <- na.locf(mid_prices, fromLast=TRUE)
  mid_prices <- cbind(mid_prices, taq_data[index(mid_prices), "Volume"])
  mid_prices[is.na(mid_prices[, "Volume"]), "Volume"] <- 0
  
  # aggregate to OHLC minutes data and cumulative volume
  mid_prices <- to.period(x=mid_prices, period="minutes")
  # round up times to next minute
  index(mid_prices) <- align.time(x=index(mid_prices), 60)
  mid_prices
}  # end scrub_agg



### scrub and return a single day of TAQ data
scrub_TAQ <- function(taq_data, vol_window=51, thresh_old=2) {
  
  stopifnot(
    (("package:xts" %in% search()) || require("xts", quietly=TRUE))
    &&
      (("package:lubridate" %in% search()) || require("lubridate", quietly=TRUE))
  )
  
  # convert time index to New_York
  index(taq_data) <- with_tz(index(taq_data), "America/New_York")
  # subset data to NYSE trading hours
  taq_data <- taq_data['T09:30:00/T16:00:00', ]
  # return NULL if no data
  if (nrow(taq_data)==0)  return(NULL)
  
  # remove duplicate time stamps using duplicated
  taq_data <- taq_data[!duplicated(index(taq_data)), ]
  
  # scrub quotes with suspect bid-offer spreads
  bid_offer <- taq_data[, 'Ask.Price'] - taq_data[, 'Bid.Price']
  #  bid_offer <- na.omit(bid_offer)
  sus_pect <- extreme_values(bid_offer)
  # remove suspect values
  taq_data <- taq_data[!sus_pect]
  # replace suspect values
  # taq_data[sus_pect, "Bid.Price"] <- taq_data[sus_pect, "Trade.Price"]
  # taq_data[sus_pect, "Ask.Price"] <- taq_data[sus_pect, "Trade.Price"]
  
  # scrub quotes with suspect price jumps
  # calculate mid prices
  mid_prices <- 0.5 * (taq_data[, "Bid.Price"] + taq_data[, "Ask.Price"])
  #  mid_prices <- na.omit(mid_prices)
  colnames(mid_prices) <- "Mid.Price"
  # replace suspect values with NA
  mid_prices[jump_values(mid_prices)] <- NA
  mid_prices <- na.locf(mid_prices)
  #  mid_prices <- na.locf(mid_prices, fromLast=TRUE)
  mid_prices <- cbind(mid_prices, taq_data[index(mid_prices), "Volume"])
  mid_prices[is.na(mid_prices[, "Volume"]), "Volume"] <- 0
  
  # aggregate to OHLC minutes data and cumulative volume
  mid_prices <- to.period(x=mid_prices, period="minutes")
  # round up times to next minute
  index(mid_prices) <- align.time(x=index(mid_prices), 60)
  mid_prices
}  # end scrub_TAQ


daily_scrub <- scrub_agg(taq_data=daily_xts[[3]])
chartSeries(daily_scrub, name=sym_bol, theme=chartTheme("white"))

# daily_xts <- NULL
# sapply(head(file_names), function(file_name) {
#   cat("loading", file_name, "\n")
#   data_name <- load(file_name)
#   daily_xts <<- rbind(daily_xts, get(data_name))
#   data_name
# })



### scrub data from single jump outliers, if two consecutive returns exceed threshold

# calculate the (symmetric) running average absolute deviation
vol_window <- 51
# abs_returns <- abs(as.vector(daily_returns))  # vector dispatches faster code
# vo_lat <- runMean(abs_returns, n=vol_window)
# vo_lat[1:(vol_window/2), ] <- vo_lat[(vol_window/2+1), ]
# system.time(vo_lat <- filter(abs_returns, filter=rep(1/vol_window,vol_window), sides=2))
# daily_mean <- runmean(x=as.vector(daily_returns), k=vol_window, alg="fast", endrule="constant", align="center")
# vo_lat <- runmean(x=abs(as.vector(daily_returns)-daily_mean), k=vol_window, alg="fast", endrule="constant", align="center")
# vo_lat <- runmean(x=abs(as.vector(diff_prices)), k=vol_window, alg="fast", endrule="constant", align="center")
# calculate vo_lat as running quantile
# vo_lat <- runmad(x=as.vector(diff_prices), k=vol_window, endrule="constant", align="center")
vo_lat <- runquantile(x=abs(as.vector(diff_prices)), k=vol_window, probs=0.9, endrule="constant", align="center")
vo_lat <- xts(vo_lat, order.by=index(diff_prices))
colnames(vo_lat) <- "volat"
# carry forward non-zero vo_lat values
vo_lat[vo_lat==0] <- NA
vo_lat <- na.locf(vo_lat)
plot(vo_lat, xlab="", ylab="", type='l')


# scrub the data
# lag_daily_returns <- c(daily_returns[-1], tail(daily_returns, 1))
# lag_daily_returns <- lag(daily_returns)
# lag_daily_returns[1,] <- 0.0

# find suspect values
thresh_old <- 1
# suspect if sum of abs diffs greater than abs sum of diffs - doesn't work
# sus_pect <- (abs(diff_prices) + abs(diff_prices_fut)) > thresh_old*abs(diff_prices+diff_prices_fut)
# suspect if abs diffs greater than vo_lat, and if abs sum of diffs less than vo_lat
sus_pect <- (
  (abs(diff_prices) > thresh_old*vo_lat) & 
    (abs(diff_prices_fut) > thresh_old*vo_lat) & 
    (abs(diff_prices+diff_prices_fut) < 2*thresh_old*vo_lat)
)
colnames(sus_pect) <- "suspect"
sum(sus_pect)
plot(sus_pect, xlab="", ylab="", type='l')


# replace suspect values with NA
# prices_scrub <- mid_prices
prices_scrub <- mid_prices
prices_scrub[sus_pect] <- NA
prices_scrub <- na.locf(prices_scrub)

# plot
plot(prices_scrub, xlab="", ylab="", type='l')
lines(prices_scrub, col='red', lwd=1)

# calculate scrubbed returns
returns_scrub <- diff(prices_scrub)/c(1, diff(.index(prices_scrub)))
returns_scrub[1,] <- 0.0

# mid_prices <- xts(raw.data[,2], order.by=as.POSIXlt(raw.data[,1]))

# End scrub

### plot scrub data

# inspect scrubbing
da_ta <- cbind(mid_prices, diff_prices, diff_prices_fut, vo_lat, sus_pect, prices_scrub)
colnames(da_ta) <- c("mid_prices", "diff_prices", "diff_prices_fut", "vo_lat", "sus_pect", "prices_scrub")
da_ta[995:1005]
plot.zoo(da_ta[7990:8010])


###########
# load and scrub multiple days of data for a single symbol
save_OHLC <- function(sym_bol) {
  
  # create path to directory with *.RData files
  file_dir <- file.path(data_dir, sym_bol)
  # get list of *.RData files
  file_list <- list.files(file_dir)
  # create paths to *.RData files
  file_names <- file.path(file_dir, file_list)
  
  # load data into list
  data <- sapply(file_names, function(file_name) {
    cat("loading", sym_bol, "frome file: ", file_name, "\n")
    data_name <- load(file_name)
    get(data_name)
  })
  
  # scrub and aggregate the data
  data <- sapply(data, scrub_agg)
  
  # recursively "rbind" the list into a single xts
  data <- do_call_rbind(data)
  
  colnames(data) <- sapply(strsplit(colnames(data), split="[.]"), 
                           function(strng) paste(sym_bol, strng[-1], sep="."))
  
  assign(sym_bol, data)
  
  save(list=eval(sym_bol), file=paste0(sym_bol, ".RData"))
  
}  # end save_OHLC



### functions new

vol_ohlc <- function(log_ohlc, calc="rogers.satchell") {
  switch(calc,
         "close"={(log_ohlc[, 4]-log_ohlc[, 1])^2},
         "garman.klass"={0.5*(log_ohlc[, 2]-log_ohlc[, 3])^2 - 
             (2*log(2)-1)*(log_ohlc[, 4]-log_ohlc[, 1])^2},
         "rogers.satchell"={(log_ohlc[, 2]-log_ohlc[, 4])*(log_ohlc[, 2]-log_ohlc[, 1]) + 
             (log_ohlc[, 3]-log_ohlc[, 4])*(log_ohlc[, 3]-log_ohlc[, 1])},
  )  # end switch
}  # end vol_ohlc


skew_ohlc <- function(log_ohlc, calc="rogers.satchell") {
  (log_ohlc[, 2]-log_ohlc[, 4])*(log_ohlc[, 2]-log_ohlc[, 1])*(log_ohlc[, 2]-0.5*(log_ohlc[, 4] + log_ohlc[, 1])) + 
    (log_ohlc[, 3]-log_ohlc[, 4])*(log_ohlc[, 3]-log_ohlc[, 1])*(log_ohlc[, 3]-0.5*(log_ohlc[, 4] + log_ohlc[, 1]))
}  # end skew_ohlc


moment_ohlc <- function(ohlc, mom_fun="vol_ohlc", calc="rogers.satchell", vo_lu=TRUE, ...) {
  log_ohlc <- log(ohlc[, 1:4])
  # match "mom_fun" with moment function
  mom_fun <- match.fun(mom_fun)
  mo_ment <- mom_fun(log_ohlc=log_ohlc, calc=calc)
  # weight by volume
  if (vo_lu) {
    mo_ment <- ohlc[, 5]*mo_ment
    mo_ment <- sum(mo_ment)/sum(ohlc[, 5])
  } else
    mo_ment <- sum(mo_ment)
  mo_ment
}  # end moment_ohlc


run_moment_ohlc <- function(ohlc, mom_fun="vol_ohlc", calc="rogers.satchell", n=20, N=260, vo_lu=TRUE, ...) {
  log_ohlc <- log(ohlc[, 1:4])
  # match "mom_fun" with moment function
  mom_fun <- match.fun(mom_fun)
  mo_ment <- mom_fun(log_ohlc=log_ohlc, calc=calc)
  # weight by volume
  if (vo_lu) {
    mo_ment <- ohlc[, 5]*mo_ment
    run_volume <- runSum(ohlc[, 5], n=n)
    mo_ment <- N*runSum(mo_ment, n=n)/(n*run_volume)
  } else
    mo_ment <- N*runSum(mo_ment, n=n)/n
  mo_ment[1:(n-1)] <- 0
  colnames(mo_ment) <- paste(
    strsplit(colnames(ohlc)[1], split="[.]")[[1]][1], 
    "Vol", sep=".")
  mo_ment
}  # end run_moment_ohlc




