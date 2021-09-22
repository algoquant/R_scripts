suppressMessages(library(ROneTick))
library(dlm)
oneTickLib()


# Define query parameters
OTQDir <- "../trunk/OneTick/"
init.query <- paste(OTQDir, "Scrub_Ticks.otq::Get_Sane_Ticks_Priming",sep='')
main.query <- paste(OTQDir, "Join_Agg_Ticks.otq::Single_Median",sep='')

marRoll = as.Date(paste(format(Sys.Date(), '%Y'),'03-20',sep='-'))
junRoll = as.Date(paste(format(Sys.Date(), '%Y'),'06-20',sep='-'))
sepRoll = as.Date(paste(format(Sys.Date(), '%Y'),'09-20',sep='-'))
decRoll = as.Date(paste(format(Sys.Date(), '%Y'),'12-20',sep='-'))


### A common function to retrieve 

loadData <- function(bucket, symbol.list, start.time, end.time, start.date, end.date)
{ 
  init.mid <- 0
  init.bo <- 0
  symbol.date <- '20130101'
  term.query <- 5
  context.query <- '412'
  db.query <- 'CDS_CMA_INTRADAY_E'
  symbol <- symbol.list$CMATicker[bucket]
  time.zone <- symbol.list$TimeZone[bucket]
  calendar.query <- symbol.list$Calendar[bucket]
  symbology.query <- symbol.list$Symbology[bucket]
  deal.type <- symbol.list$DealType[bucket]  
  
  write(paste('querying data for symbol', symbol, sep=' '), stdout())
  
  init.ticks <- oneTickQueryOTQ(init.query,
                                start=start.date, 
                                end=end.date, 
                                DELAY_TIMESTAMP=30000,
                                SYMBOL=symbol,
                                PRIME_TICKS=10,
                                DAYS=10,
                                START_TIME='073000000',
                                END_TIME='163000000',
                                BID_SCRUB='PAR_SPREAD_BID',
                                OFFER_SCRUB='PAR_SPREAD_OFFER',
                                SYMBOLOGY=symbology.query,
                                DEALTYPE=deal.type,
                                CALENDAR=calendar.query,
                                SDATE=symbol.date,
                                TERM=term.query,
                                MAX=10000,
                                MIN=-10000,
                                DB=db.query,
                                TZ=time.zone,
                                running_query=FALSE,
                                apply_times_daily=FALSE,
                                context=context.query)
  
  # Extract init.ticks
  if(is.null(init.ticks)){
    write('Query to retrieive init values failed. Using default.', stdout())
  }else{
    init.mid <- as.numeric(first(init.ticks$MID))
    init.bo <- as.numeric(first(init.ticks$BO))
    write(paste('Priming MID value', init.mid, sep=' '), stdout())
    write(paste('Priming BO value', init.bo, sep=' '), stdout())
  }
  
  # Load median ticks
  ts.ticks <- oneTickQueryOTQ(main.query,
                              start=start.date, 
                              end=end.date, 
                              PARTIAL_BUCKET_HANDLING = 'AS_SEPARATE_BUCKET',
                              BUCKET_INTERVAL_UNITS='DAYS',
                              BUCKET_INTERVAL=1,
                              DELAY_TIMESTAMP=30000,
                              BUCKET_TIME='BUCKET_START',
                              OFFER_SCRUB='PAR_SPREAD_OFFER',
                              BID_SCRUB='PAR_SPREAD_BID',
                              START_TIME=start.time,
                              END_TIME=end.time,                                                                                                                     
                              SYMBOLOGY=symbology.query,
                              SYMBOL_LEAD=symbol,
                              CALENDAR=calendar.query,
                              DEALTYPE=deal.type,
                              DB=db.query,
                              INIT_MID=init.mid,
                              INIT_BO=init.bo,                                       
                              SDATE=symbol.date,
                              TERM=term.query,
                              TZ=time.zone,
                              MAX=10000,
                              MIN=-10000,
                              context=context.query)
  if(length(ts.ticks) > 0){
    ts.ticks <- ts.ticks[,'MEDIAN']    
    colnames(ts.ticks) <- symbol
    return (ts.ticks)
  }
  else{
    return (NULL)
  }
  
  
}



########### login 10:30 bucket ##################
load1030Bucket <- function(symbols, start.date, end.date)
{
  today  <- format(Sys.time(),'%Y-%m-%d')  
  
  start.date.string <- paste(format(start.date,'%Y%m%d'),'000000',sep='')
  end.date.string <- paste(format(end.date,'%Y%m%d'),'230000',sep='')
  
  ### only load from data base if the current system time is already past the bucket time  
  ########### load 10:30 bucket #################
  list.prices <- lapply(1:nrow(symbols), loadData, 
                        symbol.list = symbols, start.time ='073000000', end.time ='103000000', 
                        start.date = start.date.string, end.date = end.date.string)
  
  ts.prices.df  <- data.frame(list.prices, stringsAsFactors=FALSE)
  ts.prices.dm  <- data.matrix(ts.prices.df)
  ts.prices.xts <- as.xts(ts.prices.dm)
  ## adjust time to 10:30 because the query only uses data for the 10:30 bucket
  .index(ts.prices.xts) <- .index(ts.prices.xts) + (60*60*10+60*30)
  
  return (ts.prices.xts)  
  
}

load1330Bucket <- function(symbols, start.date, end.date)
{
  today  <- format(Sys.time(),'%Y-%m-%d')  
  
  start.date.string <- paste(format(start.date,'%Y%m%d'),'000000',sep='')
  end.date.string <- paste(format(end.date,'%Y%m%d'),'230000',sep='')
  
  ########### load 13:30 bucket #################
  list.prices <- lapply(1:nrow(symbols), loadData, 
                        symbol.list = symbols, start.time ='103000000', end.time ='133000000', 
                        start.date = start.date.string, end.date = end.date.string)
  
  ts.prices.df  <- data.frame(list.prices, stringsAsFactors=FALSE)
  ts.prices.dm  <- data.matrix(ts.prices.df)
  ts.prices.xts <- as.xts(ts.prices.dm)
  ## adjust time to 10:30 because the query only uses data for the 10:30 bucket
  .index(ts.prices.xts) <- .index(ts.prices.xts) + (60*60*13+60*30)  
  return (ts.prices.xts)  
  
}

load1630Bucket <- function(symbols, start.date, end.date) {
  
  today  <- format(Sys.time(),'%Y-%m-%d')  
  
  start.date.string <- paste(format(start.date,'%Y%m%d'),'000000',sep='')
  end.date.string <- paste(format(end.date,'%Y%m%d'),'230000',sep='')
  ########### load 16:30 bucket #################
  list.prices <- lapply(1:nrow(symbols), loadData, 
                        symbol.list = symbols, start.time ='133000000', end.time ='163000000', 
                        start.date = start.date.string, end.date = end.date.string)
  
  ts.prices.df  <- data.frame(list.prices, stringsAsFactors=FALSE)
  ts.prices.dm  <- data.matrix(ts.prices.df)
  ts.prices.xts <- as.xts(ts.prices.dm)
  ## adjust time to 10:30 because the query only uses data for the 10:30 bucket
  .index(ts.prices.xts) <- .index(ts.prices.xts) + (60*60*16+60*30)  
  return (ts.prices.xts) 
  
  
}

