################################################
### Read and Write Data Functions  #############
################################################

################################################
### Load OneTick Data Functions  ###############
################################################
### Load CDS price and bid-offer ticks for a single symbol
# The defualt is to load two fields: VALID_MID and VALID_BID_OFFER
loadCDS <- function(SYMBOL='GLENCR', otq.file=NULL, START_DATE=20120101000000, END_DATE=20121015000000, START_TIME='083000000', END_TIME='163000000', SDATE=20120731, TZ='America/New_York', SYMBOLOGY='CMATICKER', FIELD='VALID_MID,VALID_BID_OFFER', CONTEXT='412', DB='CDS_CMA_INTRADAY_E', BUCKET_INTERVAL=NULL, BUCKET_INTERVAL_UNITS=NULL, BAND_WIDTH=NULL, OFFER_SCRUB='CLEAN_UF_500_OFFER', BID_SCRUB='CLEAN_UF_500_BID', DELTA=NULL, TERM=5, ...) {

  if (!is.null(BAND_WIDTH))
# Load ticks from ENRICHED database via Tick_Scrubbing.otq
    {
      otq.file <- paste(otq.dir, 'Tick_Scrubbing.otq::Get_scrub_ticks', sep="")
      ts.price <- tryCatch(oneTickQueryOTQ(otq.file,
                                           start=START_DATE, end=END_DATE, 
                                           START_TIME=START_TIME, END_TIME=END_TIME,
                                           SDATE=SDATE,
                                           TZ=TZ,
                                           SYMBOL=SYMBOL,
                                           FIELD=FIELD,
                                           TERM=TERM,
                                           OFFER_SCRUB=OFFER_SCRUB,
                                           BID_SCRUB=BID_SCRUB,
                                           SYMBOLOGY=SYMBOLOGY,
                                           DB=DB,
                                           apply_times_daily=FALSE,
                                           running_query=FALSE,
                                           context=CONTEXT, ...),
                           error=function(e) writeMessage(e))
    }
# Load ticks from ENRICHED database via Get_scrub_ticks.otq
  else if (is.null(BUCKET_INTERVAL))
# Load individual ticks
    {
      otq.file <- paste(otq.dir, 'Get_scrub_ticks.otq::Get_scrub_ticks', sep="")
      ts.price <- tryCatch(oneTickQueryOTQ(otq.file,
                                           start=START_DATE, end=END_DATE, 
                                           START_TIME=START_TIME, END_TIME=END_TIME,
                                           SDATE=SDATE,
                                           TZ=TZ,
                                           SYMBOL=SYMBOL,
                                           FIELD=FIELD,
                                           TERM=TERM,
                                           OFFER_SCRUB=OFFER_SCRUB,
                                           BID_SCRUB=BID_SCRUB,
                                           SYMBOLOGY=SYMBOLOGY,
                                           DB=DB,
                                           apply_times_daily=FALSE,
                                           running_query=FALSE,
                                           context=CONTEXT, ...),
                           error=function(e) writeMessage(e))
    }
  else
# Load aggregated ticks
    {
      otq.file <- paste(otq.dir, 'Get_scrub_ticks.otq::Get_Median_Ticks', sep="")
      ts.price <- tryCatch(oneTickQueryOTQ(otq.file, SYMBOL=symbol, BUCKET_INTERVAL=bucket.interval, BUCKET_INTERVAL_UNITS=bucket.interval.units, FIELD=field, CONTEXT=CONTEXT, TERM=TERM, ...), error = function(e) e)
    }

# Remove NAs and duplicate time stamps, and assign colnames
#  ts.price <- na.omit(ts.price[!duplicated(.index(ts.price))])
#  if(inherits(ts.price, "xts"))
#    {
#      ts.price <- ts.price[!duplicated(index(ts.price))]
#      ts.price <- make.index.unique(na.locf(ts.price))
#      ts.price <- make.index.unique(na.locf(ts.price), eps=1)
#      colnames(ts.price) <- makeColNames(symbol, field)
      ts.price
#    }
#  else
#    stop(paste("loadCDS: failed loading the symbol:", symbol, "\nThis is what the query returned:\n", ts.price))
}
### End loadCDS



### Load chained Index price and bid-offer ticks
# The defualt is to load two fields: VALID_MID and VALID_BID_OFFER
loadIndex <- function(symbol='CDXHY', roll.current=18, otq.file=NULL, field='VALID_MID,VALID_BID_OFFER', CONTEXT='412', bucket.interval=NULL, bucket.interval.units=NULL, ...) {

# Load the on-the-run index first
# roll.current is the number of the on-the-run roll
  symb <- paste(symbol, roll.current, sep="")
  ts.price <- loadCDS(symb, otq.file, field=field, CONTEXT=CONTEXT, bucket.interval=bucket.interval, bucket.interval.units=bucket.interval.units, ...)

# Loop over off-run indices
  rolls <- (roll.current-1):(roll.current-2)
  for (roll in rolls)
    {
      symb <- paste(symbol, roll, sep="")
      ts.price.roll <- loadCDS(symb, otq.file, field=field, CONTEXT=CONTEXT, bucket.interval=bucket.interval, bucket.interval.units=bucket.interval.units, ...)
# Roll the index three days after roll date
      date.roll <- index(ts.price[1,]) + 3*86400
# Chop off ends
      ts.price <- ts.price[paste(date.roll, "/", sep=""),]
      ts.price.roll <- ts.price.roll[paste("/", date.roll, sep=""),]
# Adjust prices on roll date
      adj <- as.vector(first(ts.price[,1])) - as.vector(last(ts.price.roll[,1]))
      ts.price.roll[,1] <- ts.price.roll[,1] + adj
# Bind off-run index to current on-run
      ts.price <- rbind.xts(ts.price.roll, ts.price)
    }

# Output
  if(inherits(ts.price, "xts"))
    {
      ts.price <- make.index.unique(na.locf(ts.price))
      colnames(ts.price) <- makeColNames(symbol, field)
      ts.price
    }
  else
    stop(paste('loadIndex: failed loading the symbol:', symbol, "\nThis is what the query returned:\n", ts.price))
}
### End loadIndex



### Load CDS price and bid-offer ticks joined with a single Index (aggregation and join are performed in OT)
# The defualt is to load two fields for CDS and then two for Index: VALID_MID and VALID_BID_OFFER
# The Index price is the third column of the xts
loadCDSIndex <- function(SYMBOL_CDS='GLENCR', SYMBOL_INDEX='HY', otq.file=NULL, START_DATE=20120101000000, END_DATE=20121015000000, START_TIME='083000000', END_TIME='163000000', SDATE=20120731, TZ='America/New_York', SYMBOLOGY_CDS='CMATICKER', FIELD='VALID_MID,VALID_BID_OFFER', CONTEXT='412', DB_CDS='CDS_CMA_INTRADAY_E', BUCKET_INTERVAL=NULL, BUCKET_INTERVAL_UNITS=NULL, BAND_WIDTH=NULL, OFFER_SCRUB='CLEAN_UF_500_OFFER', BID_SCRUB='CLEAN_UF_500_BID', DELTA=NULL, TERM=5, ...) {

if (is.null(BUCKET_INTERVAL))
# Load regular ticks
    {
      otq.file <- paste(otq.dir, 'Get_scrub_ticks.otq::Get_Joined_Ticks', sep="")
      ts.price <- tryCatch(oneTickQueryOTQ(otq.file, SYMBOLCDS=symbol, SYMBOLINDEX=symbol.index, FIELD=field, CONTEXT=CONTEXT, ...), error = function(e) e)
    }
  else
# Load aggregated ticks
    {
      otq.file <- paste(otq.dir, 'Get_scrub_ticks.otq::Join_CDS_Index', sep="")
      ts.price <- tryCatch(oneTickQueryOTQ(otq.file,
                                           start=START_DATE, end=END_DATE, 
                                           START_TIME=START_TIME, END_TIME=END_TIME,
                                           SDATE=SDATE,
                                           SYMBOL_CDS=SYMBOL_CDS,
                                           SYMBOL_INDEX=SYMBOL_INDEX,
                                           TERM=TERM,
                                           FIELD=FIELD,
                                           BUCKET_INTERVAL_UNITS=BUCKET_INTERVAL_UNITS,
                                           BUCKET_INTERVAL=BUCKET_INTERVAL, 
                                           DB_CDS=DB_CDS,
                                           TZ=TZ,
                                           apply_times_daily=FALSE,
                                           running_query=FALSE,
                                           context=CONTEXT, ...),
                           error=function(e) writeMessage(e))
    }

# Remove duplicate time stamps
#  ts.price <- na.omit(ts.price[!duplicated(.index(ts.price))])
#  if(inherits(ts.price, "xts"))
#    {
#      nDataMin <- 30
#      if (length(ts.price)<nDataMin)
#        stop(paste("loadCDSIndex: Not enough data points for the symbol:", symbol))
#      ts.price <- make.index.unique(na.locf(ts.price))
#      colnames(ts.price) <- c(makeColNames(symbol, field), makeColNames(symbol.index, field))
#  colnames(ts.price) <- c(symbol, symbol.index)
      ts.price
#    }
#  else
#    stop(paste("loadCDSIndex: failed loading the symbol:", symbol, symbol.index, "\nThis is what the query returned:\n", ts.price))

}
### End loadCDSIndex



### Load CDS price and bid-offer ticks joined with chained Index ticks
# The defualt is to load two fields for CDS and then two for Index: VALID_MID and VALID_BID_OFFER
# The Index price is the third column of the xts
loadCDSIndexChained <- function(symbol='GLENCR', symbol.index='CDXHY', roll.current=18, otq.file=NULL, field='VALID_MID,VALID_BID_OFFER', CONTEXT='412', ...) {

# Load the on-the-run index first
  symbol.index.roll <- paste(symbol.index, roll.current, sep="")
  ts.price <- loadCDSIndex(symbol, symbol.index.roll, field=field, CONTEXT=CONTEXT, ...)

# Loop over off-run indices
  rolls <- (roll.current-1):(roll.current-2)
  for (roll in rolls)
    {
      symbol.index.roll <- paste(symbol.index, roll, sep="")
      ts.cds.index.roll <- loadCDSIndex(symbol, symbol.index.roll, otq.file, field=field, CONTEXT=CONTEXT, ...)
# Roll the index three days after roll date
      date.roll <- index(ts.price[1,]) + 3*86400
# Chop off ends
      ts.price <- ts.price[paste(date.roll, "/", sep=""),]
      ts.cds.index.roll <- ts.cds.index.roll[paste("/", date.roll, sep=""),]
# Adjust Index prices on roll date
      adj <- as.vector(first(ts.price[,3])) - as.vector(last(ts.cds.index.roll[,3]))
      ts.cds.index.roll[,3] <- ts.cds.index.roll[,3] + adj
# Bind off-run index to current on-run
      ts.price <- rbind.xts(ts.cds.index.roll, ts.price)
    }

# Output
  if(inherits(ts.price, "xts"))
    {
      nDataMin <- 30
      if (length(ts.price)<nDataMin)
        stop(paste("loadCDSIndex: Not enough data points for the symbol:", symbol))
      ts.price <- make.index.unique(na.locf(ts.price))
      colnames(ts.price) <- c(makeColNames(symbol, field), makeColNames(symbol.index, field))
      ts.price
    }
  else
    stop(paste("loadCDSIndexChained: failed loading the symbol:", symbol, symbol.index, "\nThis is what the query returned:\n", ts.price))

}
### End loadCDSIndexChained


### Load tick data from source BBG_IDX
loadBbgIdx <- function(symbol='CDX18', start=20120401, end=20120901, otq.file=NULL, field='VALID_MID,VALID_BID_OFFER', running_query=FALSE, CONTEXT='412', bucket.interval=NULL, bucket.interval.units=NULL, BAND_WIDTH=NULL, TERM=5, APPLY_TIMES_DAILY=FALSE, ...) {

  if (substr(symbol,1,3)=="ITX")
    otq.file <- paste(otq.dir, "VOB.otq::best_vob_updates_EUR", sep="")
  else
    otq.file <- paste(otq.dir, "VOB.otq::best_vob_updates_US", sep="")

  symbol <- paste("CMATICKER::BBG_IDX::", symbol, sep="")
# Load ticks from BBG_IDX database
  if (is.null(bucket.interval))
    {
      ts.price <- tryCatch(oneTickQueryOTQ(otq.file,
                                           start=start, end=end,
                                           security=symbol,
                                           running_query=running_query,
                                           APPLY_TIMES_DAILY=APPLY_TIMES_DAILY,
                                           CONTEXT=CONTEXT,
                                           sdate=20120731,
                                           TO=60,
#                                           db='BBG_IDX',
                                           tz='America/New_York',
                                           ...),
                           error=function(e) writeMessage(e), warning=function(w) writeMessage(w))
    }
  else
# Load aggregated ticks
    {
      otq.file <- paste(otq.dir, "VOB.otq::qte_vs_best_qte_3", sep="")
      ts.price <- tryCatch(oneTickQueryOTQ(otq.file, security=symbol, running_query=running_query, CONTEXT=CONTEXT, ...), error = function(e) e)
    }

# Remove NAs and duplicate time stamps, and assign colnames
#  ts.price <- na.omit(ts.price[!duplicated(.index(ts.price))])
  if(inherits(ts.price, "xts"))
    {
#      ts.price <- ts.price[!duplicated(index(ts.price))]
      ts.price[,1] <- 0.0
      ts.price <- na.locf(ts.price)
#      na.rows <- is.na(ts.price[,1])
#      ts.price[na.rows,1] <- ts.price[na.rows,5]
#      ts.price[na.rows,2] <- ts.price[na.rows,6]
#      ts.price[na.rows,3] <- (ts.price[na.rows,1] + ts.price[na.rows,2])/2

      ts.price <- make.index.unique(ts.price)
#      ts.price <- make.index.unique(na.locf(ts.price), eps=1)
#      colnames(ts.price) <- makeColNames(symbol, field)
      ts.price
    }
  else
    stop(paste("loadBbgIdx: failed loading the symbol:", symbol, "\nThis is what the query returned:\n", ts.price))
}
### End loadBbgIdx



### Load time series for a list of tickers stored in a CSV file
loadTickers <- function(file.symbols=NULL, ...) {

# First column of table.symbols are tickers
# Second column of table.symbols are betas
  table.symbols <- read.table(file.symbols, header=TRUE, sep=",", as.is=TRUE)
# Load first symbol
  tsOut <- loadCDS(table.symbols[1,1], ...)/table.symbols[1,2]
#  tsOut <- table.symbols[1,1]
  for (symbol in (2:length(table.symbols[,1])))
    {
      ts.price <- loadCDS(table.symbols[symbol,1], ...)/table.symbols[symbol,2]
      tsOut <- cbind.xts(tsOut, ts.price)
#      tsOut <- c(tsOut, symbol)
    }

# Output
#  tsOut <- make.index.unique(na.locf(tsOut))
#  colnames(tsOut) <- symbol
#  tsOut <- tsOut[,-1]
  tsOut <- na.locf(tsOut)
  na.omit(tsOut)
}
### End loadTickers



#############
# Data Join in R
#############
### Join CDS price ticks with Index price ticks (join in R)
joinCDSIndex <- function(ts.price, ts.price.index) {

# Perform the join in two steps
# First perform outer join and apply locf
  ts.price.join <- merge.xts(ts.price, ts.price.index, join='outer', fill=na.locf)
# Remove first column, and return only columns containing expanded index
  ts.price.join <- ts.price.join[,-1]
# Perform left join of expanded index onto CDS column
  ts.price.join <- na.locf(merge.xts(ts.price, ts.price.join, join='left'))
  ts.price.join
}
### End joinCDSIndex



### Load CDS price ticks and then join them with Index (join in R)
loadCDSIndexJoin <- function(symbol='GLENCR', ts.price.index, otq.file=NULL, field='VALID_MID') {

  ts.price <- loadCDS(symbol, otq.file, field=field)
  nDataMin <- 30
  if (length(ts.price)<nDataMin)
    stop(paste("loadCDSIndex: Not enough data points for the symbol:", symbol))
  ts.price.join <- joinCDSIndex(ts.price, ts.price.index)
  ts.price.join
}
### End loadCDSIndexJoin



#############
# Data Aggregation in R
#############
### xts aggregation function
aggTimeSeries <- function(ts.price, agg.window) {

  indeks <- index(ts.price)
  ts.price.mean <- na.locf(.Call("runSum", ts.price, n=agg.window))/agg.window
  indic <- seq(from=length(ts.price.mean), to=1, by=-agg.window)
  ts.price.mean <- ts.price.mean[indic,]
  xtsData <- as.vector(ts.price.mean)
  xtsNew <- xts(xtsData, order.by=tail(indeks,length(xtsData)))
  xtsNew
}
### End aggTimeSeries


##################################
### alphaModel Data Functions  ###
##################################

### Write the last values of the internal model time series to file
#' @export
write.alphaModel <- function(model) {

  stopifnot(inherits(model, "alphaModel"))

  try(write(matrix(
                   c(last(model$prices),
                     last(model$signals),
                     last(model$positions)),
                   nrow=1),
            ncolumns=3,
            sep=',',
            file = targetFile,
            append=TRUE))

}
# End write.alphaModel



