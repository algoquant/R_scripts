# Load generic libraries
library(xts)
library(xtsExtra)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)
library(MASS)
library(quantstrat)
library(dynlm)
library(fArma)
library(caTools)
library(RTisean)

# Set the time-zone to GMT (UTC)
Sys.setenv(TZ="UTC")
# Show time-zone
Sys.timezone()

# Load ROneTick
library(ROneTick)
oneTickLib()
example(oneTickQueryOTQ)

setwd("c:/Devel/Models/trunk/R/alphaLib")
source("C:/Devel/Models/trunk/R/alphaLib/defaults.R")
# RobFilter
source("C:/Devel/Models/trunk/R/BetaFilters/Filt.Rob.Signal.R")


# Get object info
class(ts.data)
sapply(ts.data, class)
attributes(ts.data)
attributes(ts.data)$names
attributes(ts.data)$class
str(ts.data)


# Show all the methods associated with function 'mean'
methods("mean")


# Get sessionInfo
sessionInfo()
# Display installed packages
.packages(all=TRUE)
# Display currently loaded objects
objects()
# Display packages that have been loaded by library() or require()
search()
# Get info on all packages (produces a lot of stuff)
installed.packages()
# Another way to display installed packages
packinfo <- installed.packages(fields=c("Package", "Version"))
packinfo[,c("Package", "Version")]
packinfo["xts",c("Package", "Version")]
# Get package description
packageDescription('xts')
# Get timezone info
Sys.timezone()
Sys.setenv(TZ="GMT")
Sys.getenv("TZ")


#########################
### Load Lists of Symbols #
#########################
# Load using OTQ query
otq.get.symbols <- "S:/Software/Develop/OneTick/Get_symbols.otq"
field <- "SYMBOL_NAME"
symbols.top <- oneTickQueryOTQ(otq.get.symbols, FIELD=field, context='REMOTE')
# Load from .CSV file
file.symbols <- "S:/Data/R_Data/HY_Basket.csv"
file.symbols <- "S:/Data/R_Data/TOPTRND_Symbols_Short.csv"
file.symbols <- "S:/Data/R_Data/TOPTRND_Symbols.csv"
# Most liquid symbols less tranches
file.symbols <- "S:/Data/R_Data/TOPLTLQD_Symbols.csv"
# Most liquid less indices less tranches
file.symbols <- "S:/Data/R_Data/TOPLTLILQD_Symbols.csv"
file.symbols <- "S:/Data/R_Data/TRNCH_Symbols.csv"
# Read symbols as list
symbols.top <- read.csv(file.symbols, stringsAsFactors=FALSE)[,1]
# Read symbols as table
symbols.top <- read.table(file.symbols, header=TRUE, sep=",", as.is=TRUE)$CMATicker
names(symbols.top) <- symbols.top
symbols.some <- head(symbols.top)
file.symbols <- "S:/Data/R_Data/TOPNA_Symbols.csv"
symbols.na <- read.csv("S:/Data/R_Data/TOPNA_Symbols.csv", stringsAsFactors=FALSE)
# symbols.na <- read.table("S:/Data/R_Data/TOPNA_Symbols.csv", header=TRUE, sep=",", as.is=TRUE)
# symbols.top <- unlist(read.csv(file.symbols), use.names=FALSE)
# symbols.top <- as.vector(read.csv(file.symbols))
# Load symbols and aggnumbers from .CSV file
file.symbols <- "S:/Data/R_Data/TOPNAIG_Symbols.csv"
symbols.data <- read.csv(file.symbols, stringsAsFactors=FALSE)
symbols.top <- symbols.data$TICKER
agg.windows <- symbols.data$AGGWIN


##############################
### Load Single CDS Data ###
##############################
# OT query for loading regular ticks
otq.get.ticks <- "S:/Software/Develop/OneTick/Get_scrub_ticks.otq::Get_scrub_ticks"
symbol.cds <- "BZH"
# SPREAD_MID, CLEAN_UF_MID, DIRTY_UF_MID
field <- "CLEAN_UF_MID" # For TOPLT symbols
field <- "VALID_MID" # For TRNCH symbols
# Load prices from SCRUB database
ts.prices <- oneTickQueryOTQ(otq.get.ticks, SYMBOL=symbol.cds, FIELD=field, context='REMOTE')
colnames(ts.prices) <- symbol.cds
# Remove duplicate time stamps
ts.prices <- ts.prices[!duplicated(.index(ts.prices))]

#######################################
### Load Time Series Data From CSV  ###
#######################################
raw.data <- read.table("C:/TEMP/XO17.csv", header=TRUE, sep=",", as.is=TRUE)
ts.prices <- xts(raw.data[,2], order.by=as.POSIXlt(raw.data[,1]))

# Load prices from CSV and remove NA's
ts.prices <- read.csv('C:/jerzy/Develop/Data/Stock ETFs.csv', stringsAsFactors=FALSE)
ts.prices <- xts(sapply(ts.prices[,-1], as.numeric), order.by=as.POSIXlt(ts.prices[,1]) )
ts.prices <- na.locf(ts.prices)
ts.prices <- na.locf(ts.prices,fromLast=TRUE)
dev.off()
plot.zoo(ts.prices, main=paste(c('Stock ETFs ', format(Sys.time(),'%m-%d-%y', tz="GMT"))))

### Load CDS price ticks using function call
# Load prices from ENRICHED database
ts.prices <- loadCDS(symbol.cds)
field <- "SPREAD_BID,SPREAD_OFFER"
ts.prices <- loadCDS(symbol.cds, field=field)
# Load prices within date-time period
ts.prices <- loadCDS(symbol.cds, start=20111001000000, end=20120901000000)
# Load aggregated prices within date-time period
ts.prices <- loadCDS(symbol.cds, bucket.interval="1", bucket.interval.units="DAYS", start=20110520093000, end=20120527163000)
# Load prices from ENRICHED database - change scrubbing parameter
ts.prices <- loadCDS(symbol.cds, bandWidth=0.7)
plot.xts(ts.prices, main=paste("Price ", symbol.cds), major.format="%b %y")
# Plotting using chart_Series() from package:quantmod
theme <- chart_theme()
theme$col$up.col <- 'lightgreen'
theme$col$up.border <- 'lightgreen'
theme$col$dn.col <- 'pink'
theme$col$dn.border <- 'pink'
# chart_Series(ts.prices, theme=theme, name=colnames(ts.prices))
chartPrice(ts.prices, theme)
# This doesn't work
plot(add_SMA(n=10, col=4, lwd=2))


# Load data and fix NAs
ts.prices.ig <- loadBbgIdx("CDX18", start=20120322110000, end=20120810210000) # Time is GMT
na.rows <- is.na(ts.prices.ig[,3])
ts.prices.ig[na.rows,3] <- (ts.prices.ig[na.rows,5] + ts.prices.ig[na.rows,6])/2


# Calculate overlapping rolling mean prices
agg.window <- 10
# ts.prices.mean <- na.omit(.Call("runSum", ts.prices, n=agg.window))/agg.window
ts.prices.mean <- runMean(ts.prices, n=agg.window)
ts.prices.mean[1:agg.window,] <- ts.prices.mean[agg.window,]
# Calculate non-overlapping rolling mean price (by=agg.window)
ts.prices.mean <- na.omit(apply.rolling(ts.prices, width=agg.window, by=agg.window, FUN="mean"))
# Set print area to two rows and one column
par(mfrow=c(2,1))
plot.xts(ts.prices, main=paste("Price ", symbol.cds), major.format="%b %y")
plot.xts(ts.prices.mean, main=paste("Price ", symbol.cds, " rolling mean"), major.format="%b %y")
# Calculate tick returns
# ts.rets <- na.omit(log(ts.prices/lag(ts.prices)))
ts.rets <- diff(ts.prices)
ts.rets[1,] <- ts.rets[2,]
plot.xts(cumsum(ts.rets), main=paste("Price ", symbol.cds), major.format="%b %y")


###################################
### Load Single Aggregated CDS Data #
###################################
# OT query for loading aggregated ticks
otq.get.ticks <- "S:/Software/Develop/OneTick/Get_scrub_ticks.otq::Agg_Median_Ticks"
# Load prices directly from OT server
ts.prices <- oneTickQueryOTQ(otq.get.ticks, SYMBOL=symbol.cds, BUCKET_INTERVAL="3600", BUCKET_INTERVAL_UNITS="SECONDS", FIELD=field, context='REMOTE')
# Load aggregated prices using function call
ts.prices <- loadCDS(symbol.cds, bucket.interval="3600", bucket.interval.units="SECONDS")


####################################
### Load and Join CDS and Index Data #
####################################
### Load the Index data
symbol.index <- "CDXHY17"
ts.prices.index <- oneTickQueryOTQ(otq.get.ticks, SYMBOL=symbol.index, FIELD=field, context='REMOTE')
colnames(ts.prices.index) <- symbol.index
plot.xts(ts.prices.index, main=paste("Price ",symbol.index), major.format="%b %y")
### Perform the join in two steps
# First perform outer join, apply locf, and return only second index column
ts.prices.join <- merge.xts(ts.prices, ts.prices.index, join='outer', retside=c(FALSE,TRUE), fill=na.locf)
# Second perform left join of expanded index onto CDS column
ts.prices.join <- na.omit(merge.xts(ts.prices, ts.prices.join, join='left'))
plot.zoo(ts.prices.join, main=paste("Price ", symbol.cds, " joined with ", symbol.index), major.format="%b %y")
grid(nx=30)
# Perform the join another way - almost the same
ts.prices.join <- na.locf(merge.xts(ts.prices, ts.prices.index, join='outer'))
ts.prices.join <- na.omit(ts.prices.join[index(ts.prices),])
ts.rets.join <- diff(ts.prices.join)
ts.rets.join[1,] <- na.omit(ts.rets.join)[1,]

### Load aggregated index price data from ENRICHED database (aggregation in OTQ)
ts.prices <- loadCDS(symbol="ITXXO17", bucket.interval="60", bucket.interval.units="SECONDS", start=20120401070000, end=20120901160000, field="SPREAD_MID", apply_times_daily=TRUE, context="412", tz='GMT')

### Load chained HY index price data
ts.prices.index <- loadIndex(symbol="CDXHY")
# Load aggregated Index prices
ts.prices.index <- loadIndex(symbol="CDXHY", bucket.interval="1", bucket.interval.units="DAYS")
ts.prices.index <- loadIndex(symbol="ITXXO", roll.current=17, bucket.interval="1", bucket.interval.units="DAYS")
plot.xts(ts.prices.index, main=paste(colnames(ts.prices.index), " chained"), major.format="%b %y")
### Load chained IG index price data
ts.prices.ig.index <- loadIndex(symbol="CDX")
ts.rets.index <- diff(ts.prices.index)
ts.rets.index[1,] <- na.omit(ts.rets.index)[1,]

### Outer join HY with IG index price data
ts.index <- na.omit(merge.xts(ts.prices.index, ts.prices.ig.index, join='outer', fill=na.locf))
plot.zoo(ts.index)

### Load CDS price ticks and join them with Index price ticks (join is performed in R)
ts.prices.join <- loadCDSIndexJoin(symbol.cds, ts.index, otq.get.ticks=otq.get.ticks, field=field)
# Join CDS price data with index
ts.prices.join <- joinCDSIndex(ts.prices, ts.index)

### Load multiple CDS data joined in OT
otq.get.join.ticks <- "S:/Software/Develop/OneTick/Get_scrub_ticks.otq::Get_Joined_Median_Ticks"
ts.prices.index <- oneTickQueryOTQ(otq.get.join.ticks, SYMBOLCDS=symbol.cds, SYMBOLINDEX=symbol.index, FIELD=field, context='REMOTE')

### Load aggregated CDS price ticks joined with Index (join is performed in OT)
# No aggregation
ts.prices.join <- loadCDSIndex(symbol.cds, symbol.index)
ts.prices.join <- loadCDSIndex(symbol=symbols.cds, symbol.index="CDXHY18", field="VALID_MID,VALID_BID_OFFER")
# With aggregation
ts.prices.join <- loadCDSIndex(symbol.cds, symbol.index, bucket.interval="3600", bucket.interval.units="SECONDS")
# Chained index
ts.prices.join <- loadCDSIndexChained(symbol.cds, symbol.index="CDXHY")
ts.prices.join <- loadCDSIndexChained(symbol.cds, symbol.index="CDXHY", bucket.interval="3600", bucket.interval.units="SECONDS")
ts.rets.join <- diff(ts.prices.join)
ts.rets.join[1,] <- na.omit(ts.rets.join)[1,]


###############################################################
### Load Aggregated Tranche Prices and Join with REF Index  ###
###############################################################
symbol.cds <- "CDXHY10-15-25"
symbol.cds <- paste('CDX9-', c('0-3','3-7','7-10','10-15','15-30','30-100'), sep='')
ts.tranche <- loadCDS(symbol.cds, TERM=7, bucket.interval="1", bucket.interval.units="DAYS")
symbol.cds <- "CDX9"
ts.spread <- loadCDS(symbol.cds, field="SPREAD_MID", TERM=7, bucket.interval="1", bucket.interval.units="DAYS")
ts.join <- na.locf(cbind(ts.tranche, 2.0*ts.spread/100))
ts.join <- ts.join[,1] - 3.4*ts.join[,3]
plot(ts.join["2012-02-01/"])
# Insert term.string into colnamev
colnames(ts.tranche.prices)) <- apply(matrix(colnames(ts.tranche.prices)), 1, function(col.name, term.string) {
  col.string <- strsplit(col.name, '.', fixed=TRUE)[[1]]
  paste(c(col.string[1], term.string, col.string[-1]), collapse=".")
}, term.string='7yr')


#####################################################################
### Load HY index replicating basket and regress against HY index ###
#####################################################################
file.symbols <- "S:/Data/R_Data/HY_Basket_Feb2012.csv"
ts.prices <- loadTickers(file.symbols, bucket.interval="1", bucket.interval.units="DAYS", field="VALID_MID", start=20111001000000, end=20120901000000)
plot.zoo(ts.prices)
grid(nx=30)
ts.prices <- xts(rowMeans(ts.prices),order.by=index(ts.prices))
plot.xts(ts.prices, main=paste("Price basket.hy"), major.format="%b %y")
ts.rets <- diff(ts.prices)
ts.rets[1,] <- na.omit(ts.rets)[1,]
pacf(ts.rets, lag=30, xlab="Lag ticks", main=paste("PACF basket.hy"))
Box.test(ts.rets, type="Ljung")
ts.prices.index <- loadIndex(symbol="CDXHY", bucket.interval="1", bucket.interval.units="DAYS")
ts.rets.index <- diff(ts.prices.index)
ts.rets.index[1,] <- na.omit(ts.rets.index)[1,]
ts.prices <- cbind.xts(ts.prices,ts.prices.index)
ts.prices <- na.omit(ts.prices)
colnames(ts.prices) <- c("basket.hy","CDXHY")
plot.zoo(ts.prices)
ts.rets <- diff(ts.prices)
ts.rets[1,] <- na.omit(ts.rets)[1,]
lmBetas <- lm(ts.rets[,1] ~ ts.rets[,2])
plot.xts(cumsum(lmBetas$residuals), main=paste("Price basket.hy"), major.format="%b %y")
pacf(lmBetas$residuals, lag=30, xlab="Lag ticks", main=paste("PACF basket.hy"))


####################
### Data Loading   #
####################

sink("S:/Data/R_Data/Beta_analysis.csv")

# Define query parameters
OTQDir <- "../../OneTick/"
init.query <- paste(OTQDir, "Scrub_Ticks.otq::Get_Sane_Ticks_Priming",sep='')
main.query <- paste(OTQDir, "Join_Agg_Ticks.otq::Single_Median",sep='')
start.date <- '20121110000000'
# end.date <- '20130304000000'
end.date <- paste(format(Sys.time(),'%Y%m%d', tz="GMT"),'000000',sep='')
start.time <- '073000000'
end.time <- '163000000'
end.morning.time <- '103000000'
symbol.date <- '20130101'
term.query <- 5
bid.query <- 'PAR_SPREAD_BID'
offer.query <- 'PAR_SPREAD_OFFER'
# bid.query <- 'UPF_BID'
# offer.query <- 'UPF_OFFER'
# bid.query <- 'SPREAD_BID'
# offer.query <- 'SPREAD_OFFER'
context.query <- '412'
symbology.query <- 'CMATICKER'
# symbology.query <- 'CMAID'
db.query <- 'CDS_CMA_INTRADAY_E'
time.zone <- 'America/New_York'
# time.zone <- 'Europe/London'
calendar.query <- 'US_FED'
# calendar.query <- 'GB_CAL'
deal.type <- 'Senior'
# deal.type <- 'Subordinated' # for ITXSUBF

symbols.table <- symbols.data
### Load Median CDS quotes for a list of symbols
list.prices <- lapply(1:nrow(symbols.table), function(n.row)
                      {
# Load median ticks for initializing scrubber
                        symbol <- symbols.table$CMATicker[n.row]
                        time.zone <- symbols.table$TimeZone[n.row]
                        calendar.query <- symbols.table$Calendar[n.row]
                        symbology.query <- symbols.table$Symbology[n.row]
                        deal.type <- symbols.table$DealType[n.row]
                        write(symbol, stdout())
                        init.ticks <- tryCatch(oneTickQueryOTQ(init.query,
                                                       start=start.date, end=end.date, 
                                                       DELAY_TIMESTAMP=30000,
                                                       SYMBOL=symbol,
                                                       PRIME_TICKS=10,
                                                       DAYS=100,
                                                       START_TIME=start.time,
                                                       END_TIME=end.time,
                                                       BID_SCRUB=bid.query,
                                                       OFFER_SCRUB=offer.query,
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
                                                       context=context.query),
                                               error=function(e) writeMessage(paste(symbol, e)), warning=function(w) writeMessage(w))

# Extract init.ticks
                        if(is.null(init.ticks)){
                          write('Query to retrieive init values failed. Using default.', stdout())
                        }else{
                          init.mid <<- as.numeric(first(init.ticks$MID))
                          init.bo <<- as.numeric(first(init.ticks$BO))
                          write(paste('Priming MID value', init.mid, sep=' '), stdout())
                          write(paste('Priming BO value', init.bo, sep=' '), stdout())
                        }

# Load median ticks
                        ts.ticks <- tryCatch(oneTickQueryOTQ(main.query,
                                                     start=start.date, end=end.date, 
                                                     START_TIME=start.time,
                                                     END_TIME=end.morning.time,
                                                     BUCKET_INTERVAL_UNITS='DAYS',
                                                     BUCKET_INTERVAL=1,
                                                     SYMBOL=symbol,
                                                     OFFER_SCRUB=offer.query,
                                                     BID_SCRUB=bid.query,
                                                     SYMBOLOGY=symbology.query,
                                                     DEALTYPE=deal.type,
                                                     DB=db.query,
                                                     INIT_MID=init.mid,
                                                     INIT_BO=init.bo,
                                                     CALENDAR=calendar.query,
                                                     SDATE=symbol.date,
                                                     TERM=term.query,
                                                     TZ=time.zone,
                                                     context=context.query),
                                               error=function(e) writeMessage(paste(symbol, e)), warning=function(w) writeMessage(w))

                        ts.ticks <- ts.ticks[,'MEDIAN']
                        colnames(ts.ticks) <- symbol
                        ts.ticks
                      }
                      )
# End lapply

### Load CMA DATAVISION CDS quotes
query.datavision <- paste(OTQDir, "Join_Agg_Ticks.otq::EOD_History",sep='')
list.prices <- apply(matrix(1:nrow(symbols.data)), 1, function(n.row)
                     {
                       ts.ticks <- oneTickQueryOTQ(query.datavision,
                                                    start=start.date, end=end.date, 
                                                    SYMBOLOGY='CMAID',
                                                    DB='CDS_CMA_HISTORY',
                                                    SYMBOL=symbols.data[n.row,'CMAID'],
                                                    TENOR=term.query,
                                                    context=context.query)
                       ts.ticks <- ts.ticks[,c(-2,-3)]
                       colnames(ts.ticks) <- symbols.data[n.row,'CMATicker']
                       ts.ticks <- xts(coredata(ts.ticks), order.by=as.Date(trunc(index(ts.ticks), units='days')))
                       ts.ticks
                     }
                     )
# End lapply

# cbind into a single xts
ts.prices <- do.call('cbind', list.prices)
# Remove duplicate dates (produced by cbind)
# indeks <- c(1,diff(.index(ts.prices)))
# indeks <- which(indeks>0)
# ts.prices <- ts.prices[indeks,]
# Scrub the data
ts.prices <- xts(apply(ts.prices, 2, function(ts.tmp)
                       {
# Get rid of zero prices
                         ts.tmp[which(ts.tmp==0)] <- NA
# Get rid of NAs created by cbind
                         ts.tmp[1] <- na.omit(ts.tmp)[1]
                         ts.tmp <- na.locf(ts.tmp)
# Get rid of price spikes
                         diff.tmp.1 <- abs(c(ts.tmp[1],diff(ts.tmp)))
                         diff.tmp.2 <- abs(c(ts.tmp[1:2],diff(ts.tmp,2)))
                         diff.tmp.2 <- c(head(diff.tmp.2,1),head(diff.tmp.2,-1))
                         l.spikes <- which(diff.tmp.1>5*diff.tmp.2)
                         ts.tmp[l.spikes] <- (c(head(ts.tmp,1),head(ts.tmp,-1))[l.spikes]+c(tail(ts.tmp,-1),tail(ts.tmp,1))[l.spikes])/2
                         ts.tmp
                       }
                       ), order.by=index(ts.prices))

# Daily aggregation will cause timestamp to be shifted one day ahead - shift back one day, and truncate dates to midnight
index(ts.prices) <- as.Date(index(ts.prices)-1)
index(ts.prices) <- as.Date(trunc(index(ts.prices), units='days'))

# Check for duplicate dates
which(diff(.index(ts.prices))==0)
# Check for NAs
sum(is.na((ts.prices)))
# Explore and manipulate
# Plot
plot.zoo(ts.prices[,symbols.some$CMATicker], main='CDS')
chart_Series(ts.prices[,'MS'], name="MS")
# str.headers <- paste(symbols.some,'MEDIAN',sep=".")

deal.type <- 'Senior'
deal.type <- 'Subordinated' # for ITXSUBF
time.zone <- 'Europe/London'
calendar.query <- 'GB_CAL'
### Load Index quotes
main.query <- paste(OTQDir, "Join_Agg_Ticks.otq::Single_Median",sep='')
ts.prices.ig <- oneTickQueryOTQ(main.query,
                                start=start.date, end=end.date, 
                                START_TIME=start.time,
                                END_TIME=end.morning.time,
                                BUCKET_INTERVAL_UNITS='DAYS',
                                BUCKET_INTERVAL=1,
                                SYMBOL='IG',
                                BID_SCRUB=bid.query,
                                OFFER_SCRUB=offer.query,
                                DEALTYPE=deal.type,
                                SYMBOLOGY='CMAID',
                                DB=db.query,
                                CALENDAR=calendar.query,
                                SDATE=symbol.date,
                                TERM=term.query,
                                TZ=time.zone,
                                context=context.query)
# Daily aggregation will cause timestamp to be shifted one day ahead - shift back one day
index(ts.prices.ig) <- as.Date(index(ts.prices.ig)-86400) # 86400 is seconds in a single day
# Alternative way to do the same
index(ts.prices.ig) <- as.Date(index(ts.prices.ig)-1)
index(ts.prices.ig) <- as.Date(trunc(index(ts.prices.ig), units='days'))


# Rename columns and combine IG and CDS data
# colnames(ts.prices.ig) <- paste("IG",colnames(ts.prices.ig),sep=".")
index(ts.prices.ig) <- index(ts.prices.ig)
index(ts.prices) <- index(ts.prices)
ts.prices <- cbind(ts.prices.ig[,'MEDIAN'],ts.prices)
colnames(ts.prices)[1] <- 'IG'
# Check for NAs
sum(is.na(ts.prices))
which(is.na(ts.prices))
ts.prices[,1] <- na.locf(ts.prices[,1])
save(ts.prices, file="S:/Data/R_Data/data_topglob.RData")
symbols.comb <- c("IG",symbols.top)
str.headers <- paste(symbols.comb,'MEDIAN',sep=".")
ts.prices.med <- ts.prices[,str.headers]
colnames(ts.prices.med) <- sub('.MEDIAN','',colnames(ts.prices.med))
chart_Series(ts.prices.med[,'JCP'], name="JCP")
ts.rets.med <- diff(ts.prices.med)
ts.rets.med[1,] <- ts.rets.med[2,]
# Calculate percentage returns
ts.lrets <- diff(log(ts.prices))
ts.lrets[1,] <- ts.lrets[2,]
index(ts.lrets) <- index(ts.lrets)
# Calculate rolling mean returns
agg.param <- 2
ts.rets.mean <- apply(ts.lrets[,symbols.some], 2, runMean, n=agg.param)
ts.rets.mean <- xts(ts.rets.mean, order.by=index(ts.lrets))
ts.rets.mean[1:agg.param,] <- ts.rets.mean[agg.param,]
index(ts.rets.mean) <- index(ts.rets.mean)

# Combine IG and Top NA CDS data
ts.rets.top <- cbind(ts.rets.ig[,'IG'],ts.rets[,symbols.na$CMATicker[1:165]])
colnames(ts.rets.top)[1] <- 'IG'
# Remove NAs from IG data
ts.rets.top[which(is.na(ts.rets.top[,1])),] <- 0.0
# Reformat the xts of ts.rets.top
names.top <- colnames(ts.rets.top)
ts.rets.top <- xts(matrix(as.vector(ts.rets.top), ncol=ncol(ts.rets.top)), order.by=index(ts.rets.top))
colnames(ts.rets.top) <- names.top
# A simpler way to do the same
index(ts.rets.top) <- index(ts.rets.top)


########################
### Generate Random xts  #
########################
ts.random <- cumsum(sign(runif(nrow(ts.prices))-0.5))
ts.random <- cumsum(rnorm(nrow(ts.prices)))
ts.random <- xts(ts.random, order.by=index(ts.prices))
colnames(ts.random) <- "ts.random"
# Univariate random walk time series
ts.random <- cumsum(xts(rnorm(nrow(ts.prices)), order.by=index(ts.prices)))
colnames(ts.random) <- 'rand'
# Bivariate independent random walk time series
ts.random <- cumsum(xts(cbind(rnorm(nrow(ts.prices)),rnorm(nrow(ts.prices))), order.by=index(ts.prices)))
# Bivariate cointegrated random walk with random residual
ts.random <- cumsum(rnorm(nrow(ts.prices)))
ts.random <- xts(cbind(ts.random,ts.random+rnorm(nrow(ts.prices))), order.by=index(ts.prices))
# Bivariate cointegrated random walk with ARIMA residual
ts.random <- cumsum(rnorm(nrow(ts.prices)))
ts.random <- xts(cbind(ts.random,ts.random+arima.sim(n=nrow(ts.prices), model=list(ar=0.05*(1:4)))), order.by=index(ts.prices))
colnames(ts.random) <- c('rand1','rand2')

plot.xts(ts.random, main=paste("Price", colnames(ts.random)), major.format="%b %y")
ts.random.ret <- diff(ts.random)
ts.random.ret[1,] <- na.omit(ts.random.ret)[1,]


#########################
### Data Exploration  ###
#########################
ts.rets <- diff(ts.ighyes.10min[,'IG'])
ts.rets[1,] <- ts.rets[2,]
hist(ts.rets, prob=T, ylim=c(0,2.3), xlim=c(-3,3), col="red")
lines(density(ts.rets),lwd=2)
plot(density(ts.rets),lwd=2,xlim=c(-2,2))
qqnorm(ts.rets,main=paste("Q-Q plot of ",colnames(ts.rets)))
ks.test(ts.rets,"pnorm",mean(ts.rets),sd(ts.rets))
shapiro.test(coredata(last(ts.rets,4999)))


### Plot in loop over a list of symbols, and prompt before plotting
# Get symbols with biggest differences
f.sums <- colSums(ts.prices-ts.prices.old, na.rm=TRUE)
f.sums <- f.sums[order(abs(f.sums), decreasing=TRUE)]
symbols.top <- names(head(f.sums,22))
par(ask=TRUE)
apply(matrix(head(symbols.top,22)), 1, function(n.symbol) 
      {
        ## f.diff <- sum(ts.prices[,n.symbol]-ts.prices.old[,n.symbol])
        ## plot.zoo(cbind(ts.prices[,n.symbol], ts.prices.old[,n.symbol]), main=paste(n.symbol, '=', f.diff))
# Filter the returns first
        ts.data <- ts.lrets['2013-02-03/',n.symbol]
        filter.kalman <- dlmFilter(y=ts.data, mod=dlm.poly)
        ts.filter.lrets <- xts(filter.kalman$m[-1,1], order.by=index(ts.data))
        colnames(ts.filter.lrets) <- paste(n.symbol, 'filtered.rets')
# Filter the prices
        ts.data <- ts.prices['2013-02-03/',n.symbol]
        filter.kalman <- dlmFilter(y=ts.data, mod=dlm.poly)
        ts.filter.prices <- xts(filter.kalman$m[-1,1], order.by=index(ts.data))
        colnames(ts.filter.prices) <- paste(n.symbol, 'filtered.prices')
# Plot
        chart.TimeSeries(cbind(ts.data,ts.filter.prices), main=paste(n.symbol, 'Prices and Kalman filtered prices'), colorset=c(1,2), lty=c(1,1), ylab="", xlab="", legend.loc='topright')
        plot.zoo(cbind(ts.data,ts.filter.prices,ts.filter.lrets), main=paste(n.symbol, 'Prices and Kalman filtered prices'), colorset=c(1,2,3), lty=c(1,1,1), ylab="", xlab="", legend.loc='topright')
#        chart.TimeSeries(ts.prices['2012-11-03/',n.symbol], main=n.symbol, colorset=1, lty=1, ylab="", xlab="")
        n.symbol
      }
      )
# End lapply
par(ask=FALSE)


######################
### Statistical Tests  #
######################
# Shapiro-Wilk Normality test
shapiro.test(as.vector(head(ts.rets,4999)))

qqnorm(ts.rets,main=paste("Q-Q plot of ",symbol.cds))

# Scatterplots of lagged returns
lag.plot(ts.rets,4,main=paste("Lag plots of ",symbol.cds))

### Autocorrelation tests
### Ljung-Box to test for autocorrelations different from zero
# 'lag' is the number of autocorrelation coefficients
LjBox <- Box.test(ts.rets, lag=10, type="Ljung")
LjBox$statistic
LjBox$p.value


### Durbin-Watson test for first order autocorrelations of regression residuals
lmBetas <- lm(ts.rets[,1] ~ ts.rets[,2] + ts.rets[,3])
# From package lmtest
library(lmtest)
dwtest(lmBetas)
# From package car
library(car)
durbinWatsonTest(lmBetas)


### Augmented Dickey-Fuller test (null is that ts has a unit root)
# 'k' is the number of autocorrelation terms
library(tseries)
adf.test(rnorm(1000),k=5)
adf.test(cumsum(rnorm(1000)))
adf.test(as.vector(ts.random))


# Calculate spectral density using AR fit
spec.ar(ts.retsPair,log='no')
spec.pgram(ts.retsPair,log='no')

# Calculate multi-variate lagged cross-correlations
ccf(as.vector(ts.rets[,1]),as.vector(ts.rets[,2]), lag.max=10, type="correlation", plot=TRUE)


####################
### Data Analysis  #
####################

# Plot IG prices time series and filtered data points
ig.filtered <- xts(filter(as.vector(ts.prices[,'IG']), rep(0.05,20)), order.by=index(ts.prices))
ig.filtered[1] <- na.omit(ig.filtered)[1]
ig.filtered <- na.locf(ig.filtered)
# ig.filtered[which(is.na(ig.filtered))] <- 0.0
colnames(ig.filtered) <- 'IG.FILTERED'
plot.xts(ts.prices[,'IG'], xlab='', ylab='', main='IG', type='l')
par(new=TRUE)
lines(ig.filtered, col='red', lwd=3)


### Calculate variance ratios for a list of time scales
time.scales <- matrix(10:200)
var.ratios <- cbind(time.scales, matrix(apply(time.scales, 1, var.ratio, ts.rets=ts.rets, agg.min=2)))
colnames(var.ratios) <- c('time.scales','var.ratios')

### Calculate variance ratios for a list of symbols
var.ratios <- as.matrix(apply(ts.rets, 2, var.ratio, agg.min=1, agg.max=5) )
# Sort the symbols by highest variance ratios
var.ratios <- as.matrix(var.ratios[order(var.ratios, decreasing=TRUE),1])
colnames(var.ratios) <- 'Var.Ratios'
write.csv(var.ratios, "S:/Data/R_Data/data.var.ratios.csv")
barplot(as.vector(var.ratios[1:22,]), names.arg=rownames(var.ratios)[1:22], las=3, ylab="Var.ratios", xlab="TS", main="Var.ratio scores")
# Select the top symbols
length.ratios <- length(var.ratios)
var.ratios.top <- var.ratios[trunc(0.75*length.ratios):length.ratios]
# Select prices for the top symbols with highest var.ratios
ts.prices.top <- ts.prices[,names(var.ratios.top)]
# Remove RESCAP from top symbols
ts.prices.top <- ts.prices.top[,colnames(ts.prices.top)!='RESCAP']


### Calculate spectrum scores for a list of symbols
data.spec <- as.matrix(apply(ts.rets, 2, function(ts.ret) tryCatch(spectral.frac(ts.ret), error=function(e) writeMessage(e)) ))
# Sort
data.spec <- as.matrix(data.spec[order(data.spec, decreasing=TRUE),1])
colnames(data.spec) <- 'Spectrum.Scores'
#colnames(data.spec) <- c('CMATicker','Spectrum.Energyfrac')
barplot(as.vector(data.spec[1:22,]), names.arg=rownames(data.spec)[1:22], las=3, ylab="Spec.ratios", xlab="TS", main="Spec.ratio scores")
write.csv(data.spec, "S:/Data/R_Data/data.spec.csv")
# Plot scatterplot
plot(x=var.ratios, y=data.spec, xlab=colnames(var.ratios), ylab=colnames(data.spec), main='Scores')
# Add reg line
data.lm <- lm(data.spec ~ var.ratios)
abline(data.lm)
summary(data.lm)
# Add labels
text(x=var.ratios, y=data.spec, labels=rownames(var.ratios), cex= 0.7, pos=2)
# Add labels by clicking
identify(var.ratios, data.spec, labels=rownames(var.ratios), cex = 0.7)
# Plot scatterplot with wordcloud labels to prevent label overlaps
library(wordcloud)
library(tm)
wordcloud::textplot(x=var.ratios, y=data.spec, words=rownames(var.ratios), xlab=colnames(var.ratios), ylab=colnames(data.spec), main='Scores', cex= 0.7, xlim=c(min(var.ratios),max(var.ratios)), ylim=c(min(data.spec),max(data.spec)))


# Calculate spectrum scores for a list of ARIMA processes (autocorrelation parameters)
length.ar <- nrow(ts.rets)
coeffs.ar <- as.matrix(0.1*(1:9))
data.ar <- apply(coeffs.ar, 1, function(coeff.ar) spectral.frac(arima.sim(n=length.ar, model=list(ar=coeff.ar))) )
data.ar <- cbind(coeffs.ar,data.ar)
colnames(data.ar) <- c('AR coeffs','Spectrum.Scores')


### Calculate Omega scores for some symbols
# Omega score of zero means not forecastable (white noise); 100 score means perfectly forecastable (a sinusoid).
library(ForeCA)
Omega(ts.rets[,'NOKIA'])
data.omega <- t(as.matrix(Omega(ts.rets, spectrum_method="wosa")))
colnames(data.omega) <- 'Omega.Scores'

# Calculate omega scores for a list of symbols
data.omega <- apply(ts.rets, 2, function(ts.ret,...) tryCatch(Omega(ts.ret,...), error=function(e) writeMessage(e)), spectrum_method="multitaper")
data.omega <- as.matrix(data.omega)
data.omega <- data.omega[order(data.omega, decreasing=TRUE),1]
data.omega <- as.matrix(data.omega)
colnames(data.omega) <- 'Omega.Scores'
write.csv(data.omega, "C:/Data/data.omega.csv")
barplot(as.vector(data.omega[1:22,]), names.arg=rownames(data.omega)[1:22], las=3, ylab="Omega", xlab="TS", main="Omega scores")
plot.zoo(ts.prices[,rownames(head(data.omega,11))])

# Scatterplot omega scores for a list of ts
plot(omega.wosa, omega.multitaper, ylab="multitaper", xlab="wosa", main='Omega scores')

# Calculate omega scores for sine wave plus random ts
data.omega <- as.matrix(apply(matrix(0.1*(1:10)), 1, function(rand.param,...) tryCatch(Omega(rand.param*sin(20*(1:nrow(ts.prices))/nrow(ts.prices)) + rnorm(nrow(ts.prices)),...), error=function(e) writeMessage(e)), spectrum_method="wosa"))
colnames(data.omega) <- 'Omega.Scores'


# Calculate rolling variance scores for multiple ts
agg.min <- 2
agg.max <- 10
ts.var.ratios <- as.matrix(apply(ts.rets, 2, function(ts.ret) {
  ts.ratios <- apply.rolling(ts.ret, width=look.back, FUN="var.ratio", agg.min=agg.min, agg.max=agg.max)
  ts.ratios[1:(look.back-1),] <- ts.ratios[look.back,]
  ts.ratios
  } ))
ts.var.ratios <- xts(ts.var.ratios, order.by=index(ts.rets))
colnames(ts.var.ratios) <- colnames(ts.rets)
plot.zoo(ts.var.ratios, main=paste(c('var.ratios ',format(Sys.time(),'%m-%d-%y',tz="UTC"))), xlab="")


# Calculate efficiently rolling variance scores for multiple ts
# This actually gives slightly different result - as it should
ts.var.ratios <- as.matrix(apply(ts.rets, 2, function(ts.ret) {
  var.agg.min <- runMean(ts.ret,n=agg.min)
  var.agg.min[1:(agg.min-1)] <- var.agg.min[agg.min]
  var.agg.min <- runSD(var.agg.min,n=look.back)
  var.agg.max <- runMean(ts.ret,n=agg.max)
  var.agg.max[1:(agg.max-1)] <- var.agg.max[agg.max]
  var.agg.max <- runSD(var.agg.max,n=look.back)
  ts.ratios <- (var.agg.max/var.agg.min)*(agg.min/agg.max)/2
  ts.ratios[1:(look.back-1)] <- ts.ratios[look.back]
  ts.ratios
} ))
ts.var.ratios <- xts(ts.var.ratios, order.by=index(ts.rets))
colnames(ts.var.ratios) <- colnames(ts.rets)
plot.zoo(ts.var.ratios, main=paste(c('var.ratios ',format(Sys.time(),'%m-%d-%y',tz="UTC"))), xlab="")


### Calculate regression credit betas for a list of symbols
# Prepare cumulated returns data
cum.rets <- apply(ts.rets, 2, function(rets)
                  {
                    rets <- .Call("runSum", rets, 2)
                    rets[1] <- na.omit(rets)[1]
                    rets <- na.locf(rets)
                  }
                  )
cum.rets <- xts(cum.rets, order.by=index(ts.rets))

profile.beta <- sapply(colnames(ts.rets)[-1],
                       function(colname)
                       {
#                         formula.lm <- as.formula(paste(colname,"~",'IG',sep=" "))
                         formula.lm <- as.formula(paste('IG',"~",colname,sep=" "))
                         lm.ig <- lm(formula.lm, data=cum.rets)
                         c(lm.beta=summary(lm.ig)$coefficients[2,1],lm.error=summary(lm.ig)$coefficients[2,2],lm.t.value=summary(lm.ig)$coefficients[2,3],lm.r.squared=summary(lm.ig)$r.squared)
                       }
                       )
# End sapply
profile.beta <- t(profile.beta)
# Sort the list according to highest regression r.squared
profile.beta <- profile.beta[order(profile.beta[,'lm.r.squared'], decreasing=TRUE),]
# Sort the list according to highest regression t.value
profile.beta <- profile.beta[order(profile.beta[,'lm.t.value'], decreasing=TRUE),]
# Create scatterplot of t.value versus r.squared
plot(lm.t.value~lm.r.squared, data=profile.beta)


### Perform pair-wise correlation analysis
# Calculate correlation matrix
corr.matrix <- cor(ts.rets)
colnames(corr.matrix) <- colnames(ts.rets)
rownames(corr.matrix) <- colnames(ts.rets)
# Reorder the correlation matrix based on clusters
# Calculate permutation vector
library(corrplot)
corr.order <- corrMatOrder(corr.matrix, order="hclust", hclust.method="complete")
# Apply permutation vector
corr.matrix.ordered <- corr.matrix[corr.order,corr.order]
# Plot the correlation matrix
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(corr.matrix.ordered, tl.col="black", tl.cex=0.8, method="square", col=col3(8), cl.offset=0.75, cl.cex=0.7, cl.align.text="l", cl.ratio=0.25)
# Draw rectangles on the correlation matrix plot
corrRect.hclust(corr.matrix.ordered, k=13, method="complete", col="red")


# Perform hierarchical clustering analysis
data.dist <- as.dist(1-corr.matrix.ordered)
data.cluster <- hclust(data.dist)
plot(data.cluster, main="Dissimilarity = 1-Correlation", xlab="")


### Perform principal component analysis PCA
data.pca <- prcomp(ts.rets, center=TRUE, scale=TRUE)
data.pca <- prcomp(ts.rets[,symbols.data$CMATicker], center=TRUE, scale=TRUE)
plot(data.pca)
barplot(data.pca$sdev[1:10], names.arg=colnames(data.pca$rotation)[1:10], las=3, ylab="STDEV", xlab="PCVec", main="PCA Explain VAR")
var.pca <- sum(data.pca$sdev^2)
barplot(data.pca$sdev^2/var.pca, names.arg=colnames(data.pca$rotation), las=3, ylab="VAR", xlab="PCVec", main="PCA Explain VAR")
# Show first three principal component loadings
data.pca$rotation[,1:3]
# Permute second principal component loadings by size
loadings.pca2 <- as.matrix(data.pca$rotation[order(data.pca$rotation[,2], decreasing=TRUE),2])
colnames(loadings.pca2) <- "PCA2"
loadings.pca2
# The option las=3 rotates the names.arg labels
barplot(as.vector(loadings.pca2), names.arg=rownames(loadings.pca2), las=3, ylab="Loadings", xlab="Symbol", main="Loadings PCA2")


# Convert time series of returns of principal components to xts
rets.pca <- xts(data.pca$x, order.by=index(ts.rets))
index(rets.pca) <- index(rets.pca)
plot.zoo(cumsum(rets.pca[,1:10]))
chart_Series(cumsum(rets.pca[,1]), name=colnames(rets.pca[,1]))
pacf(as.vector(rets.pca[,1]), lag=10, xlab="Lag ticks", main=paste("PACF of PC1"))
# Calculate time series of prices of principal components - not same as cumsum(rets.pca) !!!
ts.pca <- xts(ts.prices[,symbols.data$CMATicker] %*% data.pca$rotation, index(ts.prices))


# cbind IG index with PC's, and calculate statistics
rets.pca <- cbind(ts.rets[,'IG'],rets.pca[,1:10])
# colnames(rets.pca)[1] <- 'IG'
index(rets.pca) <- index(rets.pca)
var.ratios <- as.matrix(apply(rets.pca, 2, var.ratio, agg.min=1, agg.max=5))
colnames(var.ratios) <- 'Var.Ratios'
# Calculate cumsum(rets.pca)
ts.pca <- cumsum(rets.pca)


# Combine all the top CDS, and perform cluster analysis,
names.pca <- apply(as.matrix(1:10), 1, function(n.pc)
{
  loadings.pca <- as.matrix(data.pca$rotation[order(abs(data.pca$rotation[,n.pc]), decreasing=TRUE),n.pc])
  rownames(head(loadings.pca,20))
}
)


### Calculate returns for sector portfolios
symbols.top <- read.csv('S:/Data/R_Data/TOPNAIG_Symbols_ordered.csv', stringsAsFactors=FALSE)
# rets.portfolios <- NULL
rets.portfolios <- apply(as.matrix(unique(symbols.top$Portfolio)), 1, function(n.portfolio)
{
  write(n.portfolio, stdout())
  n.rows <- which(symbols.top$Portfolio==n.portfolio)
  n.notionals <- symbols.top$Notional[n.rows]
  ts.portfolio <-  (ts.rets[,symbols.top$CMAEntityId[n.rows]] %*% n.notionals)/sum(n.notionals)
#  colnames(ts.portfolio) <- n.portfolio
#  rets.portfolios <<- cbind(rets.portfolios,ts.portfolio)
  ts.portfolio
}
)
rets.portfolios <- xts(rets.portfolios, index(ts.rets))
colnames(rets.portfolios) <- unique(symbols.top$Portfolio)
plot.zoo(cumsum(rets.portfolios['2013-01-03/',]), main=paste('Sector portfolios', '/', format(Sys.time(),'%y-%m-%d', tz="GMT")))
n.portfolio <- 'Mtg Ins'
plot.zoo(ts.prices['2013-01-03/',symbols.top$CMAEntityId[which(symbols.top$Portfolio==n.portfolio)]], main=paste(n.portfolio, '/', format(Sys.time(),'%y-%m-%d', tz="GMT")))
as.matrix(apply(rets.portfolios, 2, var.ratio, agg.min=1, agg.max=5))
# Plot portfolios in loop
par(ask=TRUE)
apply(as.matrix(unique(symbols.top$Portfolio)), 1, function(n.portfolio)
{
  plot.zoo(ts.prices['2013-01-03/',symbols.top$CMAEntityId[which(symbols.top$Portfolio==n.portfolio)]], main=paste(n.portfolio, '/', format(Sys.time(),'%y-%m-%d', tz="GMT")))
  n.portfolio
}
)
par(ask=FALSE)

# Get last prices
last.prices <- t(coredata(tail(ts.prices,1)))
last.prices <- as.matrix(last.prices[order(last.prices, decreasing=TRUE),1])
colnames(last.prices) <- 'last.prices'
head(last.prices)
write.csv(last.prices, "S:/Data/R_Data/data.last.prices.2013.csv")
# Write last prices for only the top symbols
rows.top <- match(symbols.top$CMAEntityId, rownames(last.prices))
write.csv(cbind(rownames(last.prices)[rows.top], last.prices[rows.top]), "S:/Data/R_Data/data.last.prices.2013.csv")
# write.csv(last.prices[match(symbols.top$CMAEntityId, rownames(last.prices))], "S:/Data/R_Data/data.last.prices.2013.csv")
# write.csv(last.prices[which(rownames(last.prices) %in% symbols.top$CMAEntityId)], "S:/Data/R_Data/data.last.prices.2013.csv")
# apply(matrix(symbols.top$CMAEntityId), 1, match, table=colnames(ts.prices))
# apply(matrix(symbols.top$CMAEntityId), 1, function(n.name) which(n.name==colnames(ts.prices)))


### Calculate rate of change for a list of symbols
look.back <- 2
ts.lrets <- diff(log(ts.prices),lag=look.back)
ts.lrets[1:look.back,] <- ts.lrets[look.back+1,]
# Calculate variance for a list of symbols
profile.var <- sapply(ts.lrets, function(ts.lret) sqrt(var(ts.lret)))

# Sort symbol positions by lowest to highest rate of change
# symbols.top <- t(sapply(1:length(ts.lrets[,1]), function(row.num) order(coredata(ts.lrets[row.num,]))))
# Sort symbols by highest to lowest absolute rate of change
row.num <- nrow(ts.lrets)
sd.lrets <- sqrt(apply(na.omit(diff(log(ts.prices))), 2, var))
norm.lrets <- as.vector(ts.lrets[row.num,])/sd.lrets
symbols.top <- colnames(ts.lrets)[order(abs(norm.lrets), decreasing=TRUE)]
plot.zoo(ts.prices['2012-01-03/', symbols.top[11:22]], main=paste('Top CDS/', format(Sys.time(),'%y-%m-%d', tz="GMT")))
# For each date, sort symbols by highest to lowest rate of change
symbols.top <- t(sapply(1:nrow(ts.lrets), function(row.num) colnames(ts.lrets)[order(coredata(ts.lrets[row.num,]), decreasing=TRUE)]))
symbols.top <- xts(symbols.top,order.by=index(ts.lrets))
# Show symbols with highest to lowest rate of change
cut.off <- 10
tail(symbols.top[,1:cut.off],22)
tail(symbols.top[,(length(symbols.top[1,])-cut.off+1):length(symbols.top[1,])],22)


# Perform regression of IG versus PC's
formula.lm <- IG ~ PC1
data.lm <- lm(formula.lm, data=rets.pca)
summary(data.lm)
# Plot regression scatterplot
plot(IG ~ PC1, data=rets.pca, ylab="IG", xlab="PC1", main='Regression of IG versus PC1')
abline(data.lm)
# Plot IG and PC1 in out plot
plot.xts(cumsum(rets.pca[,'IG']), xlab='', ylab='', main='IG', type='l')
par(new=TRUE)
lines(cumsum(data.lm$coefficients[2]*rets.pca[,'PC1']), col='red', lwd=3)
# Calculate residuals
data.lm$residuals <- xts(data.lm$residuals, order.by=index(rets.pca))
colnames(data.lm$residuals) <- 'resid.lm'
plot.zoo(cumsum(cbind(rets.pca[,'IG'],-rets.pca[,'PC1'],rets.pca[,'PC2'],data.lm$residuals)))
pacf(data.lm$residuals, lag=10, xlab="Lag ticks", main=paste("PACF of lm"))
# Perform prediction from lm model (this isn't forecasting!)
predict.ig <- as.xts(predict(data.lm))
colnames(predict.ig) <- 'predict.ig'
# Calculate fitted IG values from regression (same as predicted)
fitted.ig <- as.xts(fitted(data.lm))
colnames(fitted.ig) <- 'fitted.ig'
plot.zoo(cumsum(cbind(rets.pca[,'IG'],predict.ig,fitted.ig)))
plot.xts(cumsum(rets.pca[,'IG']), xlab='', ylab='', main='IG', type='l')
par(new=TRUE)
lines(cumsum(fitted.ig), col='red', lwd=3)


# Fit ARIMA model to PC1 spreads (not returns)
library(forecast)
arima.pc1 <- auto.arima(x=as.vector(ts.pca[,'PC1']))
summary(arima.pc1)
# Plot fitted PC1 values
fitted.pc1 <- xts(fitted(arima.pc1), order.by=index(ts.pca))
plot.xts(ts.pca[,'PC1'], xlab='', ylab='', main='PC1 fit in RED', type='l')
par(new=TRUE)
lines(fitted.pc1, col='red', lwd=1)
# Forecast next few PC1 ticks
forecast.pc1 <- forecast(arima.pc1)
plot(forecast.pc1)


# Fit ARIMA model to IG spreads versus PC1 spreads (not returns)
# Fit ARIMA(2,0,0) model to IG versus PC1
arima.ig <- Arima(x=as.vector(ts.pca[,'IG']), xreg=as.vector(ts.pca[,'PC1']), order=c(2,0,0))
# Choose best ARIMA model for IG index using auto.arima and PC1 as external regressor
arima.ig <- auto.arima(x=as.vector(ts.pca[,'IG']), xreg=as.vector(ts.pca[,'PC1']), max.P=5, max.Q=5, max.order=10)
summary(arima.ig)
# Plot fitted PC1 values
fitted.ig <- xts(fitted(arima.ig), order.by=index(ts.pca))
plot.xts(ts.pca[,'IG'], xlab='', ylab='', main='IG fit in RED', type='l')
par(new=TRUE)
lines(fitted.ig, col='red', lwd=1)
# Forecast next few IG ticks
forecast.ig <- forecast.Arima(arima.ig)
plot(forecast.ig)


### Run simple trading strategy based on signal from medians
# Prepare data
bid.offer <- 0.125
ts.data <- ts.1min.ig['2013-01-03/','MEDIAN']
ts.rets <- diff(ts.data)
ts.rets[1,] <- ts.rets[2,]
agg.params <- matrix(c(3,5,9,13,17))
ts.aggs <- apply(agg.params, 1, function(agg.param)
                 {
                   ts.agg <- apply(ts.data, 2, runMedian, agg.param)
                   ts.agg[1:agg.param] <- ts.agg[agg.param]
                   ts.agg
                 }
                 )
# End apply
# ts.aggs <- cbind(ts.data,ts.aggs)
ts.aggs <- xts(ts.aggs, order.by=index(ts.data))
colnames(ts.aggs) <- paste('IG.med', agg.params, sep='.')
# Run trading strategy
agg.pnl <- apply(ts.aggs, 2, function(ts.agg)
                 {
                   ts.forecast <- ts.data-ts.agg
                   ts.forecast <- lag(ts.forecast)
                   ts.forecast[1] <- 0.0
#                         filter.dlm.poly <- dlmFilter(y=ts.data, mod=dlm.poly)
#                         ts.filter.poly <- xts(filter.dlm.poly$m[-1,1], order.by=index(ts.data))
                   sum(ts.rets*ts.forecast)/sum(bid.offer*abs(na.omit(diff(ts.forecast))))
                 }
                 )
# End apply
agg.pnl <- cbind(agg.params,agg.pnl)
colnames(agg.pnl) <- c('aggs','pnls')

### Calculate Variance Beta Profile for a single symbol
betas <- 0.5*(0:10)
profile.beta <- sapply(betas, var.Hurst, ts.ret=cbind(ts.rets.med[,'GS.MEDIAN'],ts.rets.med[,'IG']), agg.min=agg.min, agg.max=agg.max)
profile.beta <- cbind(betas, profile.beta)
colnames(profile.beta) <- c("beta","Var.ratio")
profile.beta[which.max(profile.beta[,2]),c(1,2)]

### Calculate Variance Profile credit betas for a list of symbols
profile.beta <- sapply(colnames(ts.rets.med)[-1],
                       function(colname)
                       {
                         betas.var <- beta.profile(var.Hurst, ts.rets=ts.rets.med, colname1=colname, colname2='IG', betas=betas, agg.min=agg.min, agg.max=agg.max)
                         betas.var
                       }
                       )
# End sapply
profile.beta <- t(profile.beta)
# Sort the list according to highest Var.ratio
profile.beta <- profile.beta[order(profile.beta[,'Var.ratio'], decreasing=TRUE),]


### Calculate Variance Profile pair-wise credit betas for a list of symbols
# Get all the symmbols except the first (index)
str.headers <- colnames(ts.rets.med)[-1]
# Create symmbol combinations
combs <- expand.grid(colname1=str.headers,colname2=str.headers)
profile.beta <- apply(combs, 1,
                      function(comb)
                      {
                        betas.var <- beta.profile(var.Hurst, ts.rets=ts.rets.med, colname1=comb[['colname1']], colname2=comb[['colname2']], betas=betas, agg.min=agg.min, agg.max=agg.max)
                        c(base=comb[['colname1']], hedge=comb[['colname2']], betas.var)
                      }
                      )
# End sapply
profile.beta <- t(profile.beta)
# Sort the list according to highest Var.ratio
profile.beta <- profile.beta[order(profile.beta[,'Var.ratio'], decreasing=TRUE),]


### Calculate number of quotes for a list of symbols
profile.beta <- sapply(symbols.top, function(symbol)
                       tryCatch(nrow(loadCDS(symbol, start=20120101000000, end=20120718000000)), error=function(e) writeMessage(e))
                       )
# End sapply


### Calculate recent median spreads and UF for a list of symbols
profile.beta <- sapply(symbols.top,
                       function(symbol) {
                         ts.prices <- loadCDS(symbol, FIELD='SPREAD_MID,SPREAD_BO,UF_MID,UF_BO', START_DATE=20121002000000, END_DATE=20121015000000)
                         if (is.null(dim(ts.prices)))
                           stat.summary <- c(NULL, NULL, NULL, NULL)
                         else
                           stat.summary <- c(median(ts.prices[,1]), median(ts.prices[,2]), median(ts.prices[,3]), median(ts.prices[,4]))
                         stat.summary
                       }
                       )
# End sapply

write.csv(aperm(profile.beta, c(2,1)), "S:/Data/R_Data/CDS_analysis.csv")


### Calculate regression credit betas and PACF statistics for a list of symbols
profile.beta <- sapply(symbols.top,
                       function(symbol) {
                         ts.prices.join <- loadCDSIndexChained(symbol, symbol.index, bucket.interval="1", bucket.interval.units="DAYS", start=20120201000000, end=20120625000000)
                         tryCatch(betaCDS(ts.prices.join), error=function(e) writeMessage(e), finally=print(paste("Symbol= ",symbol)))
                       }
                       )
# End sapply


### Calculate stats for a list of symbols
profile.beta <- sapply(tail(symbols.top,3),
                       function(symbol, betas) {
                         ts.prices <- tryCatch(loadCDSIndex(
                                                            SYMBOL_CDS=symbol, SYMBOL_INDEX='HY',
                                                            START_DATE=20120401000000, END_DATE=20121015000000,
                                                            START_TIME='093000000', END_TIME='113000000',
                                                            FIELD='VALID_MID,VALID_BID_OFFER',
                                                            BUCKET_INTERVAL_UNITS='DAYS', BUCKET_INTERVAL=1
                                                            ),
                                               error=function(e) writeMessage(e), warning=function(w) writeMessage(w))
                         ts.rets <- diff(ts.prices)
                         ts.rets[1,] <- ts.rets[2,]
#                         ts.rets[1,] <- na.omit(ts.rets)[1,]
#                         stat.summary <- tryCatch(statSummary(ts.prices), error=function(e) writeMessage(e))
                         stat.summary <- tryCatch(betaProfile(optimObjective, cbind(ts.rets[,2], ts.rets[,4]), betas, 2, 10), error=function(e) writeMessage(e))
                         print(c(symbol,stat.summary))
                         stat.summary
                       }, betas=betas
                       )
# End sapply

# Transpose the data
profile.beta <- aperm(profile.beta, c(2,1))



# Calculate a single range beta profile - first column is CDS, second is Index
betas <- seq(from=0, to=2.0, by=0.1)
beta.profile <- sapply(betas, function(beta) statRange(beta*ts.prices[,1] - ts.prices[,2]))
# End sapply
beta.profile <- cbind(betas, beta.profile)
rangeMin <- min(beta.profile[,2])
betaMin <- beta.profile[which.min(beta.profile[,2]),1]
# Call function betaProfile() that does the above
betaProfile(statRange, ts.prices, betas)

sink()


########################
### Aggregation Analysis #
########################
# Redirect output to file
sink("S:/Data/R_Data/Stat_scores.csv")
# PACF analysis
pACF <- pacf(tail(ts.rets, 400), lag=10)
pACF <- pacf(ts.rets, lag=30, xlab="Lag ticks", main=paste("PACF ", colnames(ts.rets)))
plot(pACF[10:50])

agg.window <- 2:50
# Calculate statistical scores as a function of aggregation window
aggPacf <- sapply(agg.window, function(aggW, ts.prices)
                  {
                    ts.retMean <- na.omit(diff(aggTimeSeries(ts.prices, aggW)))
                    statSummary(ts.retMean)
                  },
                  ts.prices=ts.prices
                  )

# Calculate the aggregation window producing the highest statistical score
agg.window <- which.max(tail(aggPacf,length(aggPacf)-3))
ts.retMean <- na.omit(diff(aggTimeSeries(ts.prices, agg.window)))
pacf(ts.retMean, lag=10, main=paste("PACF ", symbol.cds))

# Calculate the statistical scores for a list of symbols
vPacfScores <- sapply(symbols.top, function(symb)
                      {
#                        ts.rets <- tryCatch(loadCDS(symb), error=function(e) e)
                        ts.rets <- na.omit(diff(na.omit(tryCatch(loadCDS(symb, bucket.interval="1", bucket.interval.units="DAYS"), error=function(e) e))))
#                        tryCatch(aggStat(tryCatch(loadCDS(symb), error=function(e) e)), error=function(e) e)
#                        tryCatch(statSummary(ts.rets), error=function(e) e)
                        tryCatch(statSummary(ts.rets), error=function(e) e)
                      }
                      )

sink()



####################
### Variance Profile #
####################
# Set aggregation ranges
agg.min <- 1
agg.max <- 1000
# Calculate the Variance Profile of the time series
profile.var <- varProfile(ts.rets, agg.min=agg.min, agg.max=agg.max)

# Set par() to ask for prompt before plotting
par(ask=TRUE)
# Plot the Variance Profile with axes flipped
plot.xts(cbind(profile.var$model[2], profile.var$model[1]), main=paste("varProfile ",symbol.cds," profile.var slope=",round(profile.var$coefficients[[2]],2)), major.format="%b %y")
plot.xts(varRet ~ nPeriods, data=profile.var$model, main="varProfile", major.format="%b %y")
# This is not working - probably needs type conversion
var1 <- as.character(colnames(profile.var$model[1]))
var2 <- as.character(colnames(profile.var$model[2]))
plot(var1 ~ var2, data=profile.var$model, main="varProfile")

# Rescale the Variance Profile and calculate its max
cutoff <- 50
profile.var <- tail(cbind(profile.var$model[2],cutoff*profile.var$model[1]/profile.var$model[2]/profile.var$model[cutoff,1]),length(profile.var$model[,1])-cutoff)
var.max <- max(profile.var[,2])
tick.max <- which.max(profile.var[,2])
# Plot the Variance Profile
plot.xts(profile.var, main=paste("varProfile ",symbol.cds," var.max=",round(var.max,2)," at ",tick.max," ticks"), major.format="%b %y")
# Add title caption
title(main="varProfile")
# Set par() to NOT ask for prompt before plotting
par(ask=FALSE)

# Print the regression coefficients
profile.var$coefficients
profile.var$coefficients[[2]]


### Calculate and plot Variance Profiles of several time series
# ts.ighyes.1min are one-minute bar data for IG, HY, and ES
ts.risk.proxy <- 100+4*(500-5*ts.ighyes.1min[,'IG'])/100
ts.risk.proxy <- 1.2*ts.risk.proxy[,1]-0.2*ts.ighyes.1min[,'HY.MEDIAN']
colnames(ts.risk.proxy) <- "RISK.PROXY"
# Calculate returns
ts.rets <- diff(cbind(ts.risk.proxy, ts.ighyes.1min[,c('IG','ES.EQMEDIAN')]))
ts.rets[1,] <- 0.0
# Calculate Variance Profiles for risk.proxy, IG, and ES
profile.var <- varProfile(ts.rets[,1], agg.min=10, agg.max=30000)
profile.risk <- cbind(profile.var$model[,2],(profile.var$model[,1]/profile.var$model[1,1])*(profile.var$model[1,2]/profile.var$model[,2])/2)
profile.var <- varProfile(ts.rets[,2], agg.min=10, agg.max=30000)
profile.ig <- cbind(profile.var$model[,2],(profile.var$model[,1]/profile.var$model[1,1])*(profile.var$model[1,2]/profile.var$model[,2])/2)
profile.var <- varProfile(ts.rets[,3], agg.min=10, agg.max=30000)
profile.es <- cbind(profile.var$model[,2],(profile.var$model[,1]/profile.var$model[1,1])*(profile.var$model[1,2]/profile.var$model[,2])/2)
# Plot all three profiles in one chart
plot(x=profile.es[,1], y=profile.risk[,2], col="red", type='l', main='Variance Profile', xlab='Agg Scale [min]', ylab='Variance Ratio')
points(x=profile.es[,1], y=profile.ig[,2], lty=2, col="blue", type='l')
points(x=profile.es[,1], y=profile.es[,2], lty=3, col="green", type='l')
legend("topleft",col=c("red","blue","green"), lty=c(1,2,3), legend=c("HYvsIG","IG","SPX"))



######################
### Variance Beta Profile #
######################
# Calculate the variance beta profile for a single pair of CDS and Index
betas <- (0:10)/10
profile.var <- sapply(betas, varProfPortf, ts.prices=ts.prices.join, agg.min=agg.min, agg.max=agg.max)
# Transpose for easy viewing
profile.var <- aperm(profile.var, c(2,1))
tick.max <- which.max(profile.var[,2])
profile.var[tick.max,]


########################
### Compile VAR profiles #
########################
# Plot the time series one by one
par(ask=TRUE)
sapply(symbols.some, function(symb) plot.xts(oneTickQueryOTQ(otq.get.ticks, SYMBOL=as.character(symb), FIELD=field, context='REMOTE'), main=as.character(symb), major.format="%b %y") )

# Turn PDF writer on
file.cds.plots <- "S:/Data/R_Data/CDS_plots.pdf"
pdf(file.cds.plots, width=7, height=11)
# pdf(file.cds.plots)
par(mfrow=c(2,1))

# Redirect output to file
sink("S:/Data/R_Data/VarProfSlopesTOPLT.csv")
sink("S:/Data/R_Data/VarProfSlopesTRNCH.csv")

# Compute the Rescaled Variance Profile for a single symbol
profile.var <- varProfileRescaled(symbol=symbol.cds, otq.get.ticks=otq.get.ticks, field=field, agg.min=agg.min, agg.max=agg.max)

# Plot the Variance Profile for a list of symbols, one by one
profile.var <- sapply(symbols.top, function(symb) tryCatch(varProfileRescaled(symbol=as.character(symb), otq.get.ticks=otq.get.ticks, field=field, agg.min=agg.min, agg.max=agg.max), error=function(e) e, finally=print(paste("Symbol= ",symb))) )

# Turn redirect off
sink()
# Turn PDF writer off
dev.off()

filePrice <- "S:/Data/R_Data/ts.prices.csv"
write.zoo(ts.prices, filePrice, sep=",")
Write_VarProfSlopes <- "S:/Data/R_Data/VarProfSlopes.csv"
write.csv(aperm(profile.var),Write_VarProfSlopes)

par(ask=FALSE)



########################
### Persistence Analysis #
########################
agg.window <- 0.01
draws <- persistenceIntervals(ts.prices, agg.window)
nDraws <- 20
maxDraws <- sum(head(draws[,1]))
barplot(head(draws[,1], nDraws), names.arg=1:nDraws, ylab="Draw percentage", xlab="Ordinal", main=paste("Largest Price Draws sum= ", round(maxDraws,2)))

agg.window <- 2:500/length(ts.prices)
aggProfile <- sapply(agg.window, function(aggW) {
                     draws <- persistenceIntervals(ts.prices=ts.prices, agg.window=aggW)[,1]
                     sDraws <- sum(head(draws, nDraws))
                     sDraws
                   }
                     )

plot(aggProfile, ylab="Sum Draws", xlab="Aggregation ticks", main="Sum of largest Price Draws")

# Turn PDF writer on
file.cds.plots <- "S:/Data/R_Data/CDS_plots.pdf"
pdf(file.cds.plots, width=7, height=11)
# pdf(file.cds.plots)
par(mfrow=c(2,1))

# Redirect output to file
sink("S:/Data/R_Data/Persistence_data.csv")

# Calculate the persistence profile for a symbol
fProfile <- persistenceProfile(symbol="BZH", otq.get.ticks=otq.get.ticks, field=field, agg.window=agg.window)

list.symbols <- 1:length(symbols.top)
# Calculate the persistence profiles for a list of symbols
v.persistence <- sapply(list.symbols, function(indic.symbol) {
                         symbol <- symbols.top[indic.symbol]
                         agg.window <- agg.windows[indic.symbol]
                         tryCatch(persistenceIntervalsWrapper(symbol=as.character(symbol), otq.get.ticks=otq.get.ticks, field=field, agg.window=agg.window), error=function(e) e, finally=print(paste("Symbol= ",symbol))) }
                       )

# Calculate the persistence profiles for a list of symbols
v.persistence <- sapply(symbols.top, function(symb) tryCatch(persistenceProfile(symbol=as.character(symb), otq.get.ticks=otq.get.ticks, field=field, agg.window=agg.window), error=function(e) e, finally=print(paste("Symbol= ",symb))) )

# Calculate the time series of persistence statistics for a symbol, and calculate its histogram
time.window <- 100
stat.persist <- statPersist(ts.prices[,1], time.window)
hist.persist <- hist(coredata(stat.persist))

# Calculate histograms of persistence statistics for random time series
v.persistence <- sapply(1:10, function(n.rep) {
  ts.random <- xts(cumsum(rnorm(length(ts.prices))), order.by=index(ts.prices))
  hist.persist <- hist(coredata(statPersist(ts.random, time.window)), plot=FALSE)
  hist.persist$counts
}
                       )
v.persistence <- rowSums(v.persistence)/10

breaks <- 10
# Calculate persistence scores for a list of symbols
scores.persistence <- sapply(symbols.top,
                             function(symbol, time.window) {
                               ts.prices <- loadCDS(symbol, bucket.interval="1", bucket.interval.units="DAYS", start=20111001000000, end=20120901000000)
                               score.persistence <- tryCatch(persistenceHistogram(ts.prices[,1], time.window, breaks), error=function(e) e)
                               score.persistence
                             },
                             time.window=time.window,
                             breaks=breaks
                             )
# End sapply
as.matrix(scores.persistence)


betas <- seq(from=0, to=2.0, by=0.1)
# Calculate persistence scores for a list of symbols
scores.persistence <- sapply(symbols.top,
                             function(symbol, time.window, breaks) {
                               ts.prices <- loadCDSIndexChained(symbol, symbol.index="CDXHY", bucket.interval="1", bucket.interval.units="DAYS", start=20111001000000, end=20120901000000)
                               score.persistence <- tryCatch(betaProfile(persistenceHistogram, ts.prices, betas, time.window, breaks), error=function(e) e)
                               score.persistence
                             },
                             time.window=time.window,
                             breaks=breaks
                             )
# End sapply
aperm(as.matrix(scores.persistence), c(2,1))



# Turn redirect off
sink()
# Turn PDF writer off
dev.off()


#####################
### Spectral Analysis #
#####################
# Perform smoothing and then recalculate periodogram
# Calculate overlapping rolling mean prices
agg.window <- 10
ts.prices.mean <- na.omit(.Call("runSum", ts.prices, n=agg.window))/agg.window
# Subset non-overlapping intervals
indic <- seq(from=length(ts.prices), to=1, by=-agg.window)
ts.prices.mean <- ts.prices[indic,]
# Calculate non-overlapping rolling mean price (by=agg.window)
ts.prices.mean <- na.omit(apply.rolling(ts.prices, width=agg.window, by=agg.window, FUN="mean"))
# Subset non-overlapping intervals
ts.prices.mean <- ts.prices.mean[indic,]
ts.rets <- na.omit(diff(ts.prices.mean))
ts.rets[1,] <- na.omit(ts.rets)[1,]
periodogram <- spec.pgram(ts.rets, spans=c(11,11), main=paste(symbol.cds," periodogram"))
periodogram <- spec.pgram(ts.random.ret, spans=c(11,11))
# Plot
magn <- length(periodogram$spec)/5
plot(periodogram$freq[1:magn], periodogram$spec[1:magn], ylab="Spectrum", xlab="Freq", main=paste(symbol.cds," periodogram"))


# Calculate the percentage of the signal's energy spectrum in low frequencies
cutoff <- 0.25
energy.frac <- spectral.frac(ts.rets, cutoff=cutoff)

# Redirect output to file
sink("S:/Data/R_Data/Spectral_data.csv")

# Calculate energy spectrum fractions for a list of symbols
energy.frac <- sapply(symbols.some, function(symb, cutoff)
                      {
                        ts.rets <- loadRets(symb)
                        spectral.frac(ts.rets, cutoff)
                      },
                      cutoff=cutoff
                      )

# Turn redirect off
sink()


########################
### SavGol Filtering  ##
########################
# Plot a price time series and its SavGol estimates of first and second derivative
func.signal <- list(filter.func="filtSavGol", filter.params=c(2, 30, 0, 0))
chart.SavGol(ts.prices[,3], func.signal)
chart.SavGol(ts.prices=ts.risk.proxy, func.signal=func.signal, time.range="2012-07-10/2012-07-20", n.flips.max=500)
# widths.SavGol is an array of SavGol width parameters
widths.SavGol <- c(5,10,20,30,40,50,60)
# Calculate SavGol filter estimates, and cbind them with original time series
ts.SavGol <- filters.SavGol(ts.prices=ts.ighyes.10min[,'IG'], func.signal=func.signal, widths.SavGol=widths.SavGol)
ts.SavGol <- filters.SavGol(ts.prices=ts.ighyes.10min[,'IG'], func.signal=func.signal, widths.SavGol=widths.SavGol, time.range="2012-05-14/2012-05-16")
plot.zoo(ts.SavGol)
plot.zoo(ts.SavGol["2012-05-14/2012-05-16"])
# Regress n-ahead returns against SavGol filter estimates
func.signal <- list(filter.func="filtSavGol", filter.params=c(2, 30, 0, 1))
ts.SavGol <- filters.SavGol(ts.prices=ts.ighyes.10min[,'IG'], func.signal=func.signal, lag=2, widths.SavGol=widths.SavGol)
formula.lm <- as.formula(paste(colnames(ts.SavGol)[1], "~", paste(colnames(ts.SavGol)[-1], collapse=" + "), sep=""))
lmBetas <- lm(formula.lm, data=ts.SavGol)
summary(lmBetas)
fitted.values <- cbind(ts.ighyes.10min[,'IG'], cumsum(lmBetas$fitted.values))
colnames(fitted.values)[2] <- "Fitted.values"
plot.zoo(fitted.values)
chart.xts(fitted.values)
# Plot the regression
layout(matrix(1:4,2,2))
plot(lmBetas)
# Plot a price time series and its SavGol estimates using plot.xts
plot.SavGol.filters(ts.prices=ts.ighyes.10min[,'IG'], func.signal=func.signal, widths.SavGol=widths.SavGol, time.range="2012-05-14/2012-05-16")
# This is similar, but uses plotSeries
plot.ts(cbind.SavGol(ts.prices[,3], func.signal))


### Testing RTisean SavGol Filter
# ts.savgol.tisean <- sav_gol(as.vector(ts.prices), n=c("5,5"))
# ts.savgol.tisean <- xts(ts.pricesSavGol, order.by=index(ts.prices))
win.savgol <- c("5,5")
ts.savgol.tisean <- xts(sav_gol(as.vector(ts.prices), n=win.savgol, p=2, D=0), order.by=index(ts.prices))
plot.xts(ts.savgol.tisean, main=paste("Price ", colnames(ts.prices), "SavGol Tisean, win.savgol=", win.savgol), major.format="%b %y", type='l')

### Testing JR SavGol Filter
source("../JR/sgfilter.R")
filter.coef <- sgcoef(M=2, nl=5, nr=5, ld=0)
#ts.savgol <- filtSavGol(ts.prices, filter.coef, M=2, nl=5, nr=0, ld=2)
ts.savgol <- filtSavGol(ts.prices, M=2, nl=5, nr=0, ld=2)
ts.savgol <- xts(ts.savgol, order.by=index(ts.prices))
plot.xts(ts.savgol, main=paste("SavGol ", symbol.cds), major.format="%b %y")
# Define SavGol filter function
filterSG <- function(ts.prices, M, nl, nr, ld)
  {
    ts.savgol <- filtSavGol(ts.prices, M, nl, nr, ld)
    ts.savgol <- xts(ts.savgol,order.by=index(ts.prices))
#    ts.rets <- na.omit(diff(ts.savgol))
  }

# Old version
ts.savgol.jr <- sgfilt(ts.prices, M=2, nl=5, nr=5, ld=0)
ts.savgol.jr <- xts(ts.savgol.jr, order.by=index(ts.prices))
# Use JR data
ex2.orig <- read.table("S:\\Software\\Develop\\R\\SavGol\\example-2.0.dat")[,2]  # values
plot(cumsum(as.numeric(ex2.orig)), type='l')
ex2.p210_0 <- read.table("S:\\Software\\Develop\\R\\SavGol\\ex2.0sav_gol.p2.nl10.nr0.dat")[,1] # TISEAN call results
ex2.p410_0 <- read.table("S:\\Software\\Develop\\R\\SavGol\\ex2.0sav_gol.p4.nl10.nr0.dat")[,1] # TISEAN call results
ex2sgfilt.p210_0 <- sgfilt(ex2.orig, M=2, nl=10, nr=0, ld=0)
ex2sgfilt.p410_0 <- sgfilt(ex2.orig, M=4, nl=10, nr=0, ld=0)

print(tail(cbind(ex2.p210_0,as.numeric(ex2sgfilt.p210_0))))
print(tail(cbind(ex2.p410_0,as.numeric(ex2sgfilt.p410_0))))

# coef table as on p651 NR in C v2(?)
mapply(sgcoef,M=c(2,2,2,2,4,4),nl=c(2,3,4,5,4,5),nr=c(2,1,0,5,4,5))


####################
###  Simulate single-name CDS momentum strategy for a list of symbols
####################
# Every day sell losers and buy winners from previous day
top.losers <- 1:cut.off
loser.symbols <- order.rets[,top.losers]
loser.symbols <- lag(loser.symbols)
loser.symbols[1,] <- loser.symbols[2,]
top.winners <- (length(order.rets[1,])-cut.off+1):length(order.rets[1,])
winner.symbols <- order.rets[,top.winners]
winner.symbols <- lag(winner.symbols)
winner.symbols[1,] <- winner.symbols[2,]
# Calculate pnls
winner.pnls <- t(sapply(1:length(ts.lrets[,1]), function(row.num) ts.rets.med[row.num,winner.symbols[row.num,]]))
winner.pnls <- xts(winner.pnls,order.by=index(ts.rets.med))
loser.pnls <- t(sapply(1:length(ts.lrets[,1]), function(row.num) ts.rets.med[row.num,loser.symbols[row.num,]]))
loser.pnls <- xts(loser.pnls,order.by=index(ts.rets.med))
winner.pnl <- xts(rowMeans(winner.pnls),order.by=index(winner.pnls))
loser.pnl <- xts(rowMeans(loser.pnls),order.by=index(loser.pnls))
# chart_Series(cumsum(winner.pnl-loser.pnl), name="Winners - Losers")
# Calculate transaction costs
# Calculate number of winner trades per period
n.win.trades <- cut.off-c(0,sapply(2:length(ts.rets.med[,1]), function(row.num) length(intersect(winner.symbols[row.num-1,],winner.symbols[row.num,])) ))
n.win.trades <- xts(n.win.trades,order.by=index(ts.rets.med))
win.costs <- 10*n.win.trades/cut.off # Assuming cut.off winning symbols and 10bps BO per trade
# plot(n.win.trades, type='l')
n.lose.trades <- cut.off-c(0,sapply(2:length(ts.rets.med[,1]), function(row.num) length(intersect(loser.symbols[row.num-1,],loser.symbols[row.num,])) ))
n.lose.trades <- xts(n.lose.trades,order.by=index(ts.rets.med))
lose.costs <- 10*n.lose.trades/cut.off # Assuming cut.off losing symbols and 10bps BO per trade
total.pnl <- cumsum(winner.pnl-loser.pnl-win.costs-lose.costs)
chart_Series(total.pnl, name="Total PnL")

# Run CDS momentum strategy using defined function
func.signal <- list(filter.func="filtSavGol", filter.params=c(2, 3, 0, 1))
portfolio.trades <- adaptivePortfolioTrader(ts.prices=ts.prices.top["2012-04-01/",], func.signal=func.signal, cut.off=5)
portfolio.trades$sums
chart_Series(cumsum(portfolio.trades$time.series[,'Total.pnl']), name="Total PnL")


####################
###  alphaModel
####################
### Load functions
### Prepare model data
# A signal threshold is defined by three numbers: a trigger level, crossing direction, and position
# Set up multiple signal threshold trading levels
vec.threshold1 <- c(0.01,1.0,1.0)
vec.threshold2 <- c(0.02,1.0,2.0)
vec.threshold3 <- c(0.0,-1.0,0.0)
vec.threshold4 <- c(0.0,1.0,0.0)
vec.threshold5 <- c(-0.01,-1.0,-1.0)
vec.threshold6 <- c(-0.02,-1.0,-2.0)
matx.thresholds <- rbind(vec.threshold1,vec.threshold2,vec.threshold3,vec.threshold4,vec.threshold5)
# These are two simple thresholds to trigger when first derivative is close to crossing zero
vec.threshold1 <- c(0.1,-1.0,-1.0) # Go short (-1.0) if signal crosses the value 0.1 from above (-1.0)
vec.threshold2 <- c(-0.1,1.0,1.0) # Go long (1.0) if signal crosses the value -0.1 from below (1.0)
matx.thresholds <- rbind(vec.threshold1,vec.threshold2)
# Specify trading rules function
matx.thresholds <- rbind(c(0.1,-1.0,-1.0),c(-0.1,1.0,1.0))
matx.thresholds <- rbind(c(0.5,-1.0,-1.0),c(0.2,-1.0,-1.0),c(-0.2,1.0,1.0),c(-0.5,1.0,1.0))
trading.rules <- list(rules.func="fillOrders.alphaModel", rules.params=matx.thresholds)
trading.rules <- list(rules.func="fillOrders.alphaModel", rules.params=rbind(c(0.5,1.0,1.0),c(-0.5,-1.0,-1.0)))
# Create ladder of trigger levels
triggers <- do.call("rbind", lapply(-30:30, function(x) rbind(c(0.1*x,-1,-1),c(-0.1*x,1,1))))
trading.rules <- list(rules.func="fillOrders.alphaModel", rules.params=triggers)
# Set up input time series - not needed anymore
# ts.bidoffers <- xts(rep(0, length(ts.prices[,1])), order.by=index(ts.prices))
# ts.betas <- xts(rep(1.0, length(ts.prices[,1])), order.by=index(ts.prices))
# Set up signal function
# func.test <- c("runSum", 5)
# func.signal <- c("filtSavGol", 2, 10, 0, 2)
# Shift prices in parallel so that last price is zero
ts.constant <- cbind(ts.prices,1)
ts.constant[,2] <- ts.prices[length(ts.prices[,1])]
ts.constant <- ts.constant[,2]
ts.prices <- ts.prices-ts.constant

# Specify signal function
func.signal <- list(signal.func="signals.alphaModel", filter.func="filtSavGol", filter.params=c(3, 1200, 0, 2), normalize.returns=TRUE, normalize.returns.window=1200, normalize.signal=TRUE, normalize.signal.window=1200)
# Delta-hedge prices
ts.prices[,1] <- ts.prices[,1] - 0.3*ts.prices[,3]

# Construct the model and assign it a model name
model.test <- alphaModel(paste("Trending", symbol.cds, "Model"))
# Update the model
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=ts.prices[,1])
# model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=ts.prices[,1], ts.bidoffers=ts.bidoffers, ts.betas=ts.betas)
# model.test <- update.alphaModel(model=model.test, func.signal=func.signal)
# model.test <- update.alphaModel(model=model.test, trading.rules=trading.rules)
# Calculate positions and pnls
model.test <- recalc.alphaModel(model.test)


# VOB Model
# Load data and fix NAs
ts.prices <- loadBbgIdx("CDX18", start=20120401110000, end=20120910210000) # Time is GMT
ts.prices <- loadBbgIdx("ITXXO17", start=20120401110000, end=20120910210000)
na.rows <- is.na(ts.prices[,3])
ts.prices[na.rows,3] <- (ts.prices[na.rows,5] + ts.prices[na.rows,6])/2
# Load VWAP data
otq.file <- paste(otq.dir, "CDS_VOB.otq::best_vob_updates_w_vwap", sep="")
ts.prices <- oneTickQueryOTQ(otq.file,
	start=20120401000000,end=20120910000000,
	band_width_bo=3,
	security='ITXXO17',
	apply_times_daily=FALSE,
	sdate=symbol.date,
	TO2=7200,
	vwap_interval=1,
	bid_stack=4,
	ask_stack=4,
	db='BBG_IDX_RT',
	running_query=FALSE,
	tz='Europe/London',
	context='408'
	)

# Construct the VWAP model with ancillary data
# Load chained VWAP data
ts.prices.hy <- oneTickQueryOTQ(otq.file,
    start=20120402000000,end=20120930000000, 
    band_width_bo=3,
    security='HY',
    symbology='CMAID',
    apply_times_daily=FALSE,
    sdate=symbol.date,
    TO2=600,
    Interval=600,
    vwap_interval=1,
    bid_stack=4,
    ask_stack=4,
    db='BBG_IDX_RT',
    running_query=FALSE,
    tz=time.zone,
    context='408'
    )
ts.ancillary <- cbind(ts.prices[,12],ts.prices[,13])
func.signal <- list(signal.func="signals.ancillary.alphaModel", filter.func="filtSavGol", filter.params=c(3, 110, 0, 1), normalize.returns=FALSE, normalize.signal=FALSE)
trading.rules <- list(rules.func="fillOrders4.alphaModel", rules.params=0.0)
model.test <- alphaModel("XO 1sec VOB VWAP Model")
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=ts.prices[,3], ts.ancillary=cbind(ts.prices[,12],ts.prices[,13]))

# IG ancillary model
otq.file <- "../../OneTick/Get_scrub_ticks.otq::Agg_Median_Prices"
ts.prices.ig <- oneTickQueryOTQ(otq.file,
    start=20120402000000,end=20120930000000, 
    band_width_bo=3,
    SYMBOL='IG',
    SYMBOLOGY='CMAID',
    apply_times_daily=FALSE,
    sdate=symbol.date,
    BUCKET_INTERVAL_UNITS='SECONDS',
    BUCKET_INTERVAL=60,
    innovation=0.33,
    db='BBG_IDX_RT',
    running_query=FALSE,
    tz=time.zone,
    context='408'
    )

otq.file <- paste(otq.dir, "Get_scrub_ticks.otq::Join_Prices_Median", sep="")
ts.prices.hy <- oneTickQueryOTQ(otq.file,
    start=20120402000000,end=20121010000000, 
    SYMBOL_LEAD='HY',
    SYMBOL_ANCIL='IG',
    SYMBOL_INDEX='ES',
    SYMBOLOGY='CMAID',
    DB='BBG_IDX_RT',
    DB_INDEX='REUTERS_A',
    SDATE=symbol.date,
    BUCKET_INTERVAL_UNITS='SECONDS',
    BUCKET_INTERVAL=3600,
    INNOVATION=0.01,
    apply_times_daily=FALSE,
    running_query=FALSE,
    TZ=time.zone,
    context=context.query
    )

# Build the model
trading.rules <- list(rules.func="fillOrders4.alphaModel", rules.params=c(3.1,0.01))
func.signal <- list(signal.func="signals.ancillary.alphaModel", filter.func="filtSavGol", filter.params=c(2,37,16,0), normalize.returns=FALSE, normalize.signal=FALSE)
model.test <- alphaModel("HY 60min Bars w/ IG SavGol Signal Model")
hy.proxy <- -ts.prices.hy[,4]
ts.bidoffers <- xts(rep(0.1, length(hy.proxy[,1])), order.by=index(hy.proxy))
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=ts.prices.hy[,1], ts.bidoffers=ts.bidoffers, ts.ancillary=cbind(hy.proxy[,1],ts.bidoffers[,1]))
model.test <- recalc.alphaModel(model.test)
summary(model.test)
plot.alphaModel(model.test, n.flips.max=500)

# Calculate drawdowns - works only for percentage changes
table.Drawdowns(model.test$pnls)
# Calculate drawdowns on rescaled pnl
table.Drawdowns(model.test$pnls/100)
# Calculate drawdowns on daily pnl
ep <-  endpoints(model.test$pnls, on='days')
cpnl <- cumsum(model.test$pnls)
daily.cpnl.diff <- diff(cpnl[ep])
daily.cpnl.diff[1,] <- daily.cpnl.diff[2,]
daily.cpnl.diff.bp <- daily.cpnl.diff.bp/1000
table.Drawdowns(daily.cpnl.diff.bp[,1,drop=FALSE])


# Build the model
trading.rules <- list(rules.func="fillOrders4.alphaModel", rules.params=c(0.62,0.88))
func.signal <- list(signal.func="signals.ancillary.alphaModel", filter.func="filtSavGol", filter.params=c(2,51,8,0), normalize.returns=FALSE, normalize.signal=FALSE)
model.test <- alphaModel("IG 60sec Bars SavGol Model")
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=ts.prices.ig[,1], ts.ancillary=cbind(ts.prices.ig[,1],ts.prices.ig[,2]))
model.test <- recalc.alphaModel(model.test)
summary(model.test)
plot.alphaModel(model.test, n.flips.max=500)

# Calculate proxy ancillary time series
ts.risk.proxy <- 100+4*(500-5*ts.ighyes.30min[,'IG'])/100
ts.risk.proxy <- 1.2*ts.risk.proxy[,1]-0.2*ts.ighyes.30min[,'HY.MEDIAN']
colnames(ts.risk.proxy) <- "RISK.PROXY"
ts.bidoffers <- xts(rep(0.1, length(ts.risk.proxy[,1])), order.by=index(ts.risk.proxy))
names(ts.bidoffers) <- "BID.OFFERS"
ts.ancillary <- cbind(ts.risk.proxy, ts.bidoffers)

ts.ig.proxy <- 100+4*(100-ts.risk.proxy[,1])
colnames(ts.ig.proxy) <- "IG.PROXY"

ts.risk.proxy <- cbind(ts.risk.proxy, ts.prices.hy[,1])
diff <- ts.risk.proxy[,1]-ts.risk.proxy[,2]
diff <- na.locf(diff)
na.missing <- is.na(ts.risk.proxy[,1])
ts.risk.proxy[na.missing,1] <- ts.risk.proxy[na.missing,2]+diff[na.missing]
na.missing <- is.na(ts.risk.proxy[,2])
ts.risk.proxy[na.missing,2] <- ts.risk.proxy[na.missing,1]-diff[na.missing]
chart_Series(ts.risk.proxy)
# IG
ts.ig.proxy <- (ts.risk.proxy[,1]+ts.risk.proxy[,2])/2
ts.ig.proxy <- -5*(ts.ig.proxy-120)
ts.ig.proxy <- cbind(ts.ig.proxy,ts.prices.ig[,1])
chart_Series(ts.ig.proxy)


### Run alphaModel
func.signal <- list(signal.func="signals.alphaModel", filter.func="filtSavGol", filter.params=c(3, 1700, 0, 2), normalize.returns=TRUE, normalize.returns.window=1700, normalize.signal=TRUE, normalize.signal.window=1700)
threshold <- 0.53
matx.thresholds <- rbind(c(-threshold-0.1,1.0,1.0),c(-threshold-0.05,1.0,1.0),c(-threshold,1.0,1.0),c(threshold,-1.0,-1.0),c(threshold+0.05,-1.0,-1.0),c(threshold+0.1,-1.0,-1.0))
matx.thresholds <- rbind(c(0.2,-1.0,-1.0),c(0.1,-1.0,-1.0),c(-0.1,1.0,1.0),c(-0.2,1.0,1.0))
trading.rules <- list(rules.func="fillOrders.alphaModel", rules.params=matx.thresholds)
# Run model
model.test <- alphaModel(paste("Tick IG Model"))
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=ts.prices[,3])
model.test <- recalc.alphaModel(model.test)
plot.alphaModel(model.test)
plot.alphaModel(model.test, n.flips.max=500)
plot.alphaModel(model.test, time.range="2012-08-02/2012-08-02", n.flips.max=500)
# Short time scale model
func.signal <- list(signal.func="signals.alphaModel", filter.func="filtSavGol", filter.params=c(3, 100, 0, 2), normalize.returns=TRUE, normalize.returns.window=100, normalize.signal=TRUE, normalize.signal.window=100)
matx.thresholds <- rbind(c(0.2,-1.0,1.0),c(0.1,-1.0,1.0),c(-0.1,1.0,-1.0),c(-0.2,1.0,-1.0))
trading.rules <- list(rules.func="fillOrders.alphaModel", rules.params=matx.thresholds)
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules)
model.test <- recalc.alphaModel(model.test)
plot.alphaModel(model.test)
model.test <- update.alphaModel(model=model.test, develop.mode=FALSE)
# Save model summary to file
summary(model.test, file="S:/Data/R_Data/model_xo.txt", append=TRUE)
# Save model to file
save(model.test, file="S:/Data/R_Data/model_xo360.RData")
# Load model from file
load(file="S:/Data/R_Data/model_xo360.RData")
# Run real-time mode with set initialize position
init.position <- model.test$positions[length(model.test$positions)-3,]
model.test <- update.alphaModel(model=model.test, develop.mode=FALSE, init.position=init.position)


### Run alphaModel for VAR model
library(vars)
# Load data
mex.prices <- read.csv(paste(data.dir, "MEX.csv", sep=""), stringsAsFactors=FALSE)
mex.prices <- xts(mex.prices[,-1], order.by=as.POSIXlt(mex.prices[,1]) )
colnames(mex.prices) <- c('mex.cds','EWW','IG','OIL','MXN','MXN.vol')
# plot.zoo(mex.prices, main=paste(c('MEXICO ', format(Sys.time(),'%m-%d-%y', tz="GMT"))))
mex.rets <- diff(log(mex.prices))
mex.rets[1,] <- mex.rets[2,]

# Run alphaModel
source(paste(alpha.dir, "alphaModel.R", sep=""))
source(paste(alpha.dir, "utilLib.R", sep=""))
source(paste(rmodels.dir, "chartLib.R", sep=""))
source(paste(rmodels.dir, "alphaModelVAR.R", sep=""))
ts.bidoffers <- xts(rep(0.017, nrow(mex.prices)), order.by=index(mex.prices))
func.signal <- list(signal.func="signals.ancillary1.1.alphaModel", filter.func="VAR", filter.params=c(200,0.01), normalize.returns=FALSE, normalize.signal=FALSE)
trading.rules <- list(rules.func="fillOrders4.2.alphaModel", rules.params=0.0)
model.test <- alphaModel("MEXICO rets VAR Model")
model.test <- update.alphaModel(model=model.test, func.signal=func.signal, trading.rules=trading.rules, ts.prices=mex.rets, ts.bidoffers=ts.bidoffers)
model.test <- recalc.alphaModel(model.test)
chart.data <- cbind(model.test)
chart_Series(chart.data[,'PnLs'], name="mex.prices")
# Make sure that in function calcProfitLoss.alphaModel  the line is: asset.returns <- (model$prices[,1])
plot.alphaModel(model.test, time.range="2012-10-01/", n.flips.max=500)


# Introduce stop-losses
source("C:/Devel/Models/Rmodels/defaults.R")
load(file="S:/Data/R_Data/model_xo100.RData") # load model.test
summary(model.test) # no stop-loss to begin
trading.rules <- list(stop.loss=list(apply=TRUE, limit=-5))
model.test <- update.alphaModel(model=model.test, trading.rules=trading.rules)
model.test <- recalc.alphaModel(model.test)
plot.alphaModel(model.test)
summary(model.test)


### Plot
plot.alphaModel(model.test, "2012-01-01/2012-07-30")
t.range <- "2012-06-14/2012-06-20"
plotPositions.alphaModel(model.test, t.range)

par(mfrow=c(2,1))
plot(model.test$positions["2012-04-01/2012-07-20"])
plot(model.test$prices["2012-04-01/2012-07-20"])
plot(model.test$prices, type='l')
plot(cumsum(model.test$pnls), type='l')
plot(model.test$signals, type='l')
plot(model.test$positions, type='l')

### Calculate SavGol width profile
widths <- 100*(5:15)
# Create list of func.signals and pass them through function objective.alphaModel()
func.signals <- lapply(widths, function(width) list(signal.func="signals.alphaModel", filter.func="filtSavGol", filter.params=c(2, width, 0, 2), normalize.returns=TRUE, normalize.returns.window=width, normalize.signal=TRUE, normalize.signal.window=width))
profile.alpha <- sapply(func.signals, objective.alphaModel, model=model.test)
profile.alpha <- cbind(widths, profile.alpha)
c(which.max(profile.alpha[,2]), profile.alpha[which.max(profile.alpha[,2]),])
plot(profile.alpha, type='l')

### Calculate SavGol width and window profiles
# Heatmap objective function for outer analysis
v.objective.alphaModel <- function(width.filter, window.filter, model=model.test) {
  model$signal.list$filter.params[2] <- width.filter
  model$signal.list$filter.params[3] <- window.filter
  model$recalc$signal <- TRUE
  model <- recalc.alphaModel(model)
  model$performance$CumPnL
}
# Heatmap objective function for SavGol filter analysis
v.objective.alphaModel <- function(window.filter, threshold, model=model.test) {
  model$signal.list$filter.params[3] <- window.filter
  model$rules.list$rules.params[1] <- threshold
  model$recalc$signal <- TRUE
  model <- recalc.alphaModel(model)
  model$performance$CumPnL
}
# Heatmap objective function for metamodel analysis
v.objective.alphaModel <- function(width.filter, window.filter, model=model.test, metamodel=metamodel.test) {
  model$signal.list$filter.params[2] <- width.filter
  model$recalc$signal <- TRUE
  model <- recalc.alphaModel(model)
  ts.cumsum <- cumsum(model$pnls)
  metamodel$signal.list$filter.params[2] <- window.filter
  metamodel <- update.alphaModel(model=metamodel, ts.prices=ts.cumsum)
  metamodel$recalc$signal <- TRUE
  metamodel <- recalc.alphaModel(metamodel)
  metamodel$performance$CumPnL
}
# Another version of objective function
v.objective.alphaModel <- function(width.filter, threshold, model=model.test) {
  func.signal <- list(signal.func="signals.alphaModel", filter.func="filtSavGol", filter.params=c(2,width.filter,0,2), normalize.returns=FALSE, normalize.signal=FALSE)
  trading.rules <- list(rules.func="fillOrders.alphaModel", rules.params=rbind(c(threshold,1.0,1.0),c(-threshold,-1.0,-1.0)))
  model <- update.alphaModel(model=model, func.signal=func.signal, trading.rules=trading.rules)
  model <- recalc.alphaModel(model)
  model$performance$CumPnL
}

# Vectorize the objective function
vec.objective.alphaModel <- Vectorize(v.objective.alphaModel)
widths <- 65:75
windows <- 15:25
thresholds <- 0.1*(13:45)
profile.alpha <- outer(widths, windows, vec.objective.alphaModel)
profile.alpha <- outer(windows, thresholds, vec.objective.alphaModel)
max(profile.alpha)
rownames(profile.alpha) <- widths
colnames(profile.alpha) <- windows
rownames(profile.alpha) <- windows
colnames(profile.alpha) <- thresholds
coordinates.matrix(profile.alpha, min)
coordinates.matrix(profile.alpha, max)


# Optimization on width, window, and threshold parameters
optim(par=c(30,10,0.5), fn=objective.alphaModel, method="L-BFGS-B", control=list(trace=2, pgtol=1e-2, factr=1e7), lower=c(30,10,0.5), upper=c(70,40,1.0), model=model.test)

# Grid optimization on width, window, and threshold parameters
# Some obscure package - doesn't work
# optgrid(f=objective.alphaModel, par=c(30,10,0.5), incr=c(1,1,0.01), lower=c(20,10,0.1), upper=c(80,50,1.0), verbose=1, model=model.test)


# Create a dataframe with all possible combinations of parameter values
combs <- expand.grid(threshold=seq(1.1, 10.1, by=1),
                     vol=seq(0, 1000, by=100),
                     width=25:75,
                     window=25:75)

combs <- expand.grid( width=25:75, window=25:75, threshold=0.01*(0:10) )
# Apply combinations to scenario function
combs <- expand.grid(width=5*(1:50), window=10:20)
profile.alpha <- apply(combs, 1, model.values, model.input=model.test)
profile.alpha <- matrix(profile.alpha[1,], nrow=50)
colnames(profile.alpha) <- 10:20
rownames(profile.alpha) <- 5*(1:50)


# Find min/max pnls and their parameters
pnls <- unlist(lapply(profile.alpha, function(x) x$pnl))
min.pnl <- min(pnls)
max.pnl <- max(pnls)
min.location <- which(pnls %in% min.pnl)[1]
max.location <- which(pnls %in% max.pnl)[1]
min.profile <- profile.alpha[[min.location]]
max.profile <- profile.alpha[[max.location]]
max.params <- list(pnl=max.pnl, width=max.profile$width, window=max.profile$window, threshold=max.profile$threshold)
unlist(max.params)

# Calculate pnls over all combinations of parameters, over a single time window
combs <- expand.grid(width=40:45, window=20:22, threshold=0.01*(60:65))
time.window <- "2012-10-12/2012-11-09"
profiles.params <- model.comb(combs=combs, time.window=time.window, model.input=model.test)
# Calculate pnls over all combinations of parameters, without changing time window
profiles.params <- model.comb(combs=combs, model.input=model.test)
# Assume profiles.params is a list of listv, each list containing an xts of pnls
# Extract pnls into xts
profiles.widths <- matrix(sapply(profiles.params, function(profile) profile[['cumpnl']]), ncol=1)
width.names <- sapply(profiles.params, function(profile) profile[['width']])
rownames(profiles.widths) <- width.names
profiles.pnls <- sapply(profiles.params, function(profile) coredata(profile[['pnl']]))
profiles.pnls <- xts(profiles.pnls, order.by=index(profiles.params[[1]][['pnl']]))
colnames(profiles.pnls) <- width.names
plot.ts(cumsum(profiles.pnls[,c(3,14)]))
# Calculate correlations of pnls
n.params <- ncol(profiles.pnls)
array.corr <- table.Correlation(profiles.pnls,profiles.pnls)
matrix.corr <- matrix(array.corr[,1],nrow=n.params,ncol=n.params,byrow=TRUE)
colnames(matrix.corr) <- colnames(profiles.pnls)
rownames(matrix.corr) <- colnames(profiles.pnls)
heatmap(matrix.corr, Rowv=NA, Colv=NA)

# Create overlaping time windows
end.points <- index(ts.ancillary[endpoints(ts.bidoffers, on="weeks"),])
end.points <- c(index(ts.bidoffers[1,]), end.points)
window.size <- 4 #weeks
# Create overlaping time windows for optimization
optim.windows <- lapply(window.size:(length(end.points)-1),  function(end.point) paste(end.points[end.point-window.size],"/",end.points[end.point], sep=""))
# Create overlaping time windows for out-of-sample pnl
apply.windows <- lapply(window.size:(length(end.points)-1),  function(end.point) paste(end.points[end.point],"/",end.points[end.point+1], sep=""))

# Calculate list of pnls for all parameter combinations over overlaping time windows
combs <- expand.grid(width=20:60, window=20:30, threshold=0.1*(10:20))
profiles.params <- lapply(optim.windows, model.comb, combs=combs, model.input=model.test)
# profiles.params <- aperm(profiles.params, c(2,1))
# profiles.params <- t(profiles.params)
# profiles.params <- profiles.params[,c('width','window','threshold')]
# cbind(unlist(optim.windows), aperm(profiles.params, c(2,1)))


# Convert data for single time window to matrix (for heatmap)
# profiles.params <- do.call("rbind", lapply(profiles.params, function(x) c(x$width, x$window, x$pnl)))
profiles.widths <- matrix(profiles.params['pnl',], nrow=length(unique(combs[,1])), byrow=FALSE)
rownames(profiles.widths) <- unique(combs[,1])
colnames(profiles.widths) <- unique(combs[,2])
# For single time window and one-dim model
profiles.widths <- matrix(profiles.widths[,2], nrow=dim(combs)[1], ncol=1, byrow=TRUE)
rownames(profiles.widths) <- combs[,1]

# Convert data for multiple time windows and one-dim model (for heatmap)
# profiles.widths <- sapply(profiles.params, function(profile) sapply(profile, function(param) param$pnl))
# rownames(profiles.widths) <- sapply(profiles.params[[1]], function(param) param$width)
profiles.widths <- sapply(profiles.params, function(profile) profile['pnl',])
rownames(profiles.widths) <- profiles.params[[1]]['width',]
heatmap(profiles.widths, Rowv=NA, Colv=NA)


# Create heatmap of width parameters (old version)
width.params <- lapply(optim.windows, model.comb, combs=combs, model.input=model.test)
rownames(width.params) <- combs[,1]
colnames(width.params) <- 1:length(optim.windows)
heatmap(width.params, Rowv=NA, Colv=NA)


# Convert data for multiple time windows to list of optimal parameters (for out-of-sample pnls)
profiles.widths <- matrix(sapply(profiles.params, optim.params), ncol=1)
profiles.widths <- matrix(profiles.widths, ncol=1)
colnames(profiles.widths) <- 'width'
# rownames(profiles.widths) <- sapply(profiles.params[[1]], function(param) param$width)
# rownames(profiles.widths) <- profiles.params[[1]]['width',]

# Calculate list of optimal parameters for a single time window
optim.params <- function(profile) {
#  profile.matrix <- t(sapply(profile, function(param) c(param$pnl, param$width, param$window)))
#  max.location <- which(profile.matrix==max(profile.matrix[,1]), arr.ind=T)[1]
  profile.matrix <- profile['pnl',]
  max.location <- which.max(profile.matrix)
  profile[['width',max.location]]
#  min.pnl <- min(pnls)
#  max.pnl <- max(pnls)
#  min.location <- which(pnls %in% min.pnl)[1]
#  max.location <- which(pnls %in% max.pnl)[1]
#  min.profile <- profiles.params[[min.location]]
#  max.profile <- profiles.params[[max.location]]
#  max.params <- list(pnl=max.pnl, width=max.profile$width, window=max.profile$window, threshold=max.profile$threshold)
#  unlist(max.params)
#  max.profile$width
}


# Create a core cluster with local sockets on local PC
library(snow)
core.cluster <- makeCluster(4, type = "SOCK")
msg.txt <- clusterEvalQ(core.cluster, source("../alphaLib/defaults.R"))
msg.txt <- clusterEvalQ(core.cluster, source("../../../Rmodels/alphaModelsTemp.R"))
msg.txt <- clusterEvalQ(core.cluster, library(xts))
msg.txt <- clusterEvalQ(core.cluster, library(quantmod))
msg.txt <- clusterEvalQ(core.cluster, library(PerformanceAnalytics))
# Run apply on core cluster
profile.alpha <- parApply(core.cluster, combs, 1, model.values, model.input=model.test)
# Stop the core cluster
stopCluster(core.cluster)

# Calculate out-of-sample pnls using optimal parameter combinations
pnls.sample <- sapply(1:length(profiles.widths[,1]), model.apply, optim.params=profiles.widths, apply.windows=apply.windows, model.input=model.test)
pnls.sample <- aperm(pnls.sample, c(2,1))
sum(unlist(pnl.sample[,4]))

### Calculate beta.profile for mixing ancillary signal time-series
betas <- 0.1*(0:10)
beta.profile <- sapply(betas,
                      function(beta, model, hy.proxy, ts.prices.hy) {
# IG risk proxy is WA IG and HY time-series
                        risk.proxy <- beta*hy.proxy[,1] + (1-beta)*ts.prices.hy[,3]
                        model <- update.alphaModel(model=model, ts.ancillary=cbind(-risk.proxy, -risk.proxy))
                        model$recalc$signal <- TRUE
                        model <- recalc.alphaModel(model)
                        model$performance$CumPnL
                      },
                      model=model.test, hy.proxy=hy.proxy, ts.prices.hy=ts.prices.hy
                      )

# Calculate beta.profile of Hurst exponents
agg.min <- 2
agg.max <- 10
beta.profile <- sapply(betas, var.Hurst, ts.ret=ts.hy.ret, agg.min=agg.min, agg.max=agg.max)

beta.profile <- cbind(betas, beta.profile)
beta.profile


### Calculate Quantiles profile
profile.quantiles <- sapply(widths, function(width)
                            {
                              ts.quantiles <- rollingQuantiles(model.test$ancillary$supply.demand, "hours", width, n.quantiles=c(0.02,0.99))
                              model.test$recalc$rules <- TRUE
                              model.test <- recalc.alphaModel(model.test)
                              PnL.flip <- model.test$performance$PnL.flip
                              PnL.flip
                            }
                            )

### Calculate Rules profile
thresholds <- 0.1*(1:6)
trading.rules.list <- lapply(thresholds, function(threshold) list(rules.func="fillOrders.alphaModel", rules.params=rbind(c(threshold,1.0,1.0),c(-threshold,-1.0,-1.0))) )
trading.rules.list <- lapply(thresholds, function(threshold) list(rules.func="fillOrders.alphaModel", rules.params=rbind(c(-threshold-0.2,1.0,1.0),c(-threshold-0.1,1.0,1.0),c(-threshold,1.0,1.0),c(threshold,-1.0,-1.0),c(threshold+0.1,-1.0,-1.0),c(threshold+0.2,-1.0,-1.0))) )
trading.rules.list <- lapply(thresholds, function(threshold) list(rules.func="fillOrders4.alphaModel", rules.params=c(threshold,0.21)))
profile.alpha <- sapply(trading.rules.list, objective.alphaModel, model=model.test)
profile.alpha <- cbind(thresholds, profile.alpha)
which(profile.alpha==max(profile.alpha), arr.ind=T)
plot(profile.alpha, type='l')


### Run metaTrader
# Create list of func.signals
widths <- 100*(1:4)
func.signals1 <- lapply(widths, function(width) list(signal.func="signals.alphaModel", filter.func="filtSavGol", filter.params=c(3, width, 0, 2), normalize.returns=TRUE, normalize.returns.window=width, normalize.signal=TRUE, normalize.signal.window=width))
widths <- 100*(5:10)
func.signals2 <- lapply(widths, function(width) list(signal.func="signals.alphaModel", filter.func="filtSavGol", filter.params=c(3, width, 0, 2), normalize.returns=TRUE, normalize.returns.window=width, normalize.signal=TRUE, normalize.signal.window=width))
func.signals <- c(func.signals1, func.signals2)
# Run metaTrader on a list of func.signals for the data in model.test
time.window <- 100
meta.data <- metaPortfolioTrader.alphaModel(model=model.test, func.signals=func.signals, time.window=time.window)


# Run metaTrader over a list of time windows, to see what is the optimal time window
time.windows <- 20*(1:5)
ret.windows <- sapply(time.windows, function(time.window) coredata(metaPortfolioTrader.alphaModel(model=model.test, func.signals=func.signals, time.window=time.window, file.data=file.data)))
ret.windows <- xts(ret.windows, order.by=index(model.test$prices))

# Run quasi-CEP mode
cep.ticks <- 0:100 # number of ticks cut off from tail
n.buffer <- 500 # buffer size of ticks fed into model
model.cep <- model.test
ts.prices <- model.test$prices
cep.signals <- sapply(cep.ticks, function(cep.tick)
                      {
                        cep.prices <- tail(last(ts.prices,-cep.tick), n.buffer)
                        model.cep <- update.alphaModel(model=model.cep, ts.prices=cep.prices)
                        model.cep <- recalc.alphaModel(model.cep)
                        as.vector(last(model.cep$signals))
                      }
                      )
write.csv(cep.signals, "S:/Data/R_Data/signals.cep.csv")
write.csv(model.test$signals, "S:/Data/R_Data/signals.csv")


###############################
### Portfolio Optimization  ###
###############################
library(DEoptim)

### Load data
stock.sectors.prices <- read.csv(paste(alpha.dir, "Stock_Sectors.csv", sep=""), stringsAsFactors=FALSE)
stock.sectors.prices <- xts(stock.sectors.prices[,-1], order.by=as.POSIXlt(stock.sectors.prices[,1]))
ts.rets <- diff(stock.sectors.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]


### Screen time series by variance ratios
var.ratios <- as.matrix(apply(ts.rets, 2, var.ratio, agg.min=1, agg.max=5))
var.ratios <- as.matrix(var.ratios[order(var.ratios, decreasing=TRUE),1])
colnames(var.ratios) <- 'Var.Ratios'
hurst.exponents <- as.matrix(apply(ts.rets['2012/'], 2, Hurst))

### Simple Portfolio Optimization
# Objective function
func.objective <- function(v.weights, ts.rets) {
#  v.weights <- v.weights/sqrt(sum((v.weights)^2))
#  -as.vector(Hurst(ts.rets=ts.rets %*% v.weights))
#  -spectral.frac(ts.rets %*% c(1,v.weights))
  -as.vector(var.ratio(ts.rets=ts.rets %*% c(1,v.weights), agg.min=2, agg.max=10))
#  -as.vector(Hurst(ts.rets=ts.rets %*% v.weights)) + 10*abs(1-sum((v.weights)^2))
#  -as.vector(var.ratio(ts.rets=ts.rets %*% v.weights, agg.min=1, agg.max=5)) + 10*abs(1-sum((v.weights)^2))
#  single.rets <- xts(ts.rets %*% v.weights, order.by=index(ts.rets))
#  sd.rets <- as.vector(sqrt(var(single.rets)))
#  -as.vector(sum(single.rets)/sd.rets) + 10*abs(1-sum((v.weights)^2))
}
# End func.objective

### Simple Optimization with optim()
n.col <- ncol(ts.rets)
init.weights <- rep(0,n.col-1)
max.weights <- rep(10,n.col-1)
optim.obj <- optim(par=init.weights, fn=func.objective, ts.rets=ts.rets, method="L-BFGS-B", lower=-max.weights, upper=max.weights, control=list(trace=1, pgtol=1e-2, factr=1e7))

v.weights <- as.matrix(c(1,optim.obj$par))
rownames(v.weights) <- colnames(ts.rets)
colnames(v.weights) <- 'weights'

### Optimization with DEoptim()
set.seed(1234)
optim.obj <- DEoptim(fn=func.objective, lower=-max.weights, upper=max.weights, control=list(storepopfrom=1), ts.rets=ts.rets)
optim.obj$optim$bestval
v.weights <- as.matrix(c(1,optim.obj$optim$bestmem))
# v.weights <- v.weights/sqrt(sum((v.weights)^2))
rownames(v.weights) <- colnames(ts.rets)
colnames(v.weights) <- 'weights'
# Plot convergence
plot(optim.obj, plot.type="storepop")

# Plot optim.rets
optim.rets <- xts(ts.rets %*% v.weights, order.by=index(ts.rets))
colnames(optim.rets) <- 'optim.rets'
chart_Series(cumsum(optim.rets), name="Optimal Portfolio", xlab="", ylab="", legend.loc='topright')

# Sort v.weights
sorted.weights <- as.matrix(v.weights[order(v.weights, decreasing=TRUE),1])
colnames(sorted.weights) <- 'Spectrum.Scores'
barplot(as.vector(sorted.weights), names.arg=rownames(sorted.weights), las=3, xlab="", ylab="", main="Sorted.weights")


### Calculate PnLs for a simple trending strategy
ts.temp <- optim.rets
look.back <- 5
# ts.signals <- lag(ts.temp)
ts.signals <- lag(sign(ts.temp)*sqrt(abs(ts.temp)))
ts.signals[1] <- ts.signals[2]
# ts.signals <- lag(sign(runSum(ts.temp,look.back)))
# ts.signals[1:(look.back+1)] <- 0
colnames(ts.signals) <- 'signals'
ts.pnls <- ts.signals*ts.temp
colnames(ts.pnls) <- 'PnLs'
plot.zoo(cumsum(cbind(ts.temp,ts.pnls)), xlab="", ylab="", main="Simple Trending Strategy")
chart_Series(cumsum(ts.pnls), name="Simple Trending Strategy", xlab="", ylab="", legend.loc='topright')


### Create overlaping time windows
step.points <- 20 # periods
look.back <- 4 # step.points
n.end.points <- trunc(nrow(ts.rets)/step.points)
end.points <- nrow(ts.rets)-step.points*n.end.points+step.points*(1:n.end.points)
end.points <- c(1,end.points)
# end.points <- endpoints(ts.rets, on="months")
# end.points <- index(ts.rets[endpoints(ts.rets, on="months"),])
# end.points <- c(index(ts.rets[1,]), end.points)
# Optimization windows
# optim.windows <- apply(matrix((look.back+1):(length(end.points)-1)), 1, function(end.point) end.points[end.point-look.back]:end.points[end.point])
# Apply windows
# apply.windows <- apply(matrix((look.back+2):length(end.points)), 1, function(end.point) end.points[end.point-1]:end.points[end.point])

### Perform out-of-sample optimization over sliding window
# range.date <- matrix(look.back:(nrow(ts.data)-1))
ts.rets <- xts(diff(ts.prices[,c('EEM','IVV')]), order.by=index(ts.prices))
ts.rets[1,] <- ts.rets[2,]
optim.weights <- NA*ts.rets[1:end.points[look.back]]
# optim.weights <- xts(NA*(1:end.points[look.back]), order.by=index(ts.rets[1:end.points[look.back]]))
# optim.weights <- 0.0*ts.rets[optim.windows[[1]],1]
colnames(optim.weights) <- paste('weights',colnames(optim.weights),sep='-')
# Add another column for optim.value
optim.weights <- cbind(NA*ts.rets[1:end.points[look.back],1], optim.weights)
colnames(optim.weights) <- c('optim.value',colnames(optim.weights)[-1])


### Find optim.weights
max.weights <- rep(10,ncol(ts.rets)-1)
init.weights <- rep(0,ncol(ts.rets)-1)
# Perform a rolling optimization over a sliding window to find the weights of the most forecastable portfolios
# Apply the optimal weights out of sample to obtain rolling forecastable portfolios
v.weights <- apply(matrix(look.back:(length(end.points)-1)), 1, function(end.point)
  {
    in.sample.rets <- ts.rets[end.points[end.point-look.back+1]:end.points[end.point],]
    optim.obj <- optim(par=init.weights, fn=func.objective, ts.rets=in.sample.rets, method="L-BFGS-B", lower=-max.weights, upper=max.weights, control=list(trace=1, pgtol=1e-2, factr=1e7))
    init.weights <<- as.matrix(optim.obj$par)
# Scale weights and make XLP weight positive
#    init.weights <- sign(init.weights[2])*init.weights/sqrt(sum((init.weights)^2))
    out.weights <- c(1,init.weights)/sqrt(1+sum((init.weights)^2))
    out.weights <- c(optim.obj$value,out.weights)
#    rownames(init.weights) <- colnames(ts.rets)
#    colnames(init.weights) <- 'weights'
    out.rets <- ts.rets[(end.points[end.point]+1):end.points[end.point+1],]
#    local.optim.weights <- xts(out.rets %*% init.weights, order.by=index(out.rets))
#    local.optim.weights <- xts(rep(init.weights,nrow(in.sample.rets)), order.by=index(in.sample.rets))
    local.optim.weights <- xts(matrix(rep(out.weights,nrow(out.rets)),ncol=length(out.weights),byrow=TRUE), order.by=index(out.rets))
# Scale rets to enforce constant SD budget
#    blah <- -func.objective(v.weights=as.matrix(optim.obj$par), ts.rets=in.sample.rets)
#    local.optim.weights <- blah*local.optim.weights/sd.rets
#    sd.rets <- as.vector(sqrt(var(local.optim.weights)))
#    local.optim.weights <- local.optim.weights/sd.rets
    optim.weights <<- rbind.xts(optim.weights, local.optim.weights)
    optim.obj$value
#    optim.weights <<- c(optim.weights, rep(init.weights,nrow(in.sample.rets)))
#    init.weights
  }
)
# End apply
optim.weights <- na.locf(optim.weights)
optim.weights <- na.locf(optim.weights,fromLast=TRUE)
optim.rets <- xts(rowSums(optim.weights[,-1]*ts.rets), order.by=index(ts.rets))
colnames(optim.rets) <- 'optim.rets'
plot.zoo(cbind(ts.prices[,c('EEM','IVV')], cumsum(optim.rets),optim.weights), main=paste(c('Weights ',format(Sys.time(),'%m-%d-%y',tz="UTC"))), xlab="")


### Find optim portfolios
# Perform a rolling optimization over a sliding window to find portfolios with the highest Sharpe ratio
# Carry these optimal portfolios out of sample
optim.portfolio <- 0.0*ts.rets[1:end.points[look.back],1]
colnames(optim.portfolio) <- 'optim.portfolio'
v.weights <- apply(matrix(look.back:(length(end.points)-1)), 1, function(end.point)
  {
    in.sample.rets <- ts.rets[end.points[end.point-look.back+1]:end.points[end.point],]
    optim.obj <- optim(par=init.weights, fn=func.objective, ts.rets=in.sample.rets, method="L-BFGS-B", lower=-max.weights, upper=max.weights, control=list(trace=1, pgtol=1e-2, factr=1e7))
    v.local.weights <- as.matrix(optim.obj$par)
# Scale weights
    v.local.weights <- v.local.weights/sqrt(sum((v.local.weights)^2))
    rownames(v.local.weights) <- colnames(ts.rets)
    colnames(v.local.weights) <- 'weights'
    in.sample.rets <- xts(in.sample.rets %*% v.local.weights, order.by=index(in.sample.rets))
    sd.rets <- as.vector(sqrt(var(in.sample.rets)))
    v.local.weights <- v.local.weights/sd.rets
    out.rets <- ts.rets[(end.points[end.point]+1):end.points[end.point+1],]
    out.rets <- xts(out.rets %*% v.local.weights, order.by=index(out.rets))
    optim.portfolio <<- rbind.xts(optim.portfolio, out.rets)
  }
)
# End apply


### Find optim portfolios
# Perform a rolling optimization over a sliding window to find portfolios with the highest Sharpe ratio
# Carry these optimal portfolios out of sample
optim.portfolio <- 0.0*ts.rets[1:look.back,1]
colnames(optim.portfolio) <- 'optim.portfolio'
v.weights <- apply(matrix(look.back:(nrow(ts.rets)-1)), 1, function(end.point)
  {
    in.sample.rets <- ts.rets[(end.point-look.back+1):end.point,]
    optim.obj <- optim(par=init.weights, fn=func.objective, ts.rets=in.sample.rets, method="L-BFGS-B", lower=-max.weights, upper=max.weights, control=list(trace=0, pgtol=1e-2, factr=1e7))
    v.local.weights <- as.matrix(optim.obj$par)
# Scale weights
    v.local.weights <- v.local.weights/sqrt(sum((v.local.weights)^2))
    rownames(v.local.weights) <- colnames(ts.rets)
    colnames(v.local.weights) <- 'weights'
    lag.rets <- as.vector(ts.rets[end.point,] %*% v.local.weights)
#    lag.rets <- sum(as.vector(ts.rets[(end.point-1):end.point,] %*% v.local.weights))
    out.rets <- ts.rets[end.point+1,]
    out.rets <- lag.rets*xts(out.rets %*% v.local.weights, order.by=index(out.rets))
    optim.portfolio <<- rbind.xts(optim.portfolio, out.rets)
  }
)
# End apply


### Find weights
v.weights <- lapply(optim.windows, function(n.window)
  {
    input.data <- ts.rets[n.window,]
    optim.obj <- optim(par=init.weights, fn=func.objective, ts.rets=input.data, method="L-BFGS-B", lower=-max.weights, upper=max.weights, control=list(trace=1, pgtol=1e-2, factr=1e7))
    v.local.weights <- as.matrix(optim.obj$par)
    rownames(v.local.weights) <- colnames(ts.rets)
    colnames(v.local.weights) <- 'weights'
    v.local.weights
  }
)
# End apply



### alphaModel Portfolio Optimization
# Given a portfolio with multiple time series of returns, find the optimal portfolio weights, over a period of time
optim.obj <- optimPortf.alphaModel(ts.prices=tail(ts.prices, 2*time.window+1), func.objective="optimObjective.alphaModel", time.window=time.window, lambda=lambda)
save(optim.obj, file="S:/Data/R_Data/optim_portf.RData")
load(file="S:/Data/R_Data/optim_portf.RData")

######################
### Optimization calls
optim.results <- optim(portf.weights[-1],
                       function(optim.weights, first.weight, time.weights, ts.rets) optimObjective.alphaModel(ts.rets=ts.rets, portf.weights=c(first.weight, optim.weights), time.weights=time.weights),
                       first.weight=portf.weights[1],
                       time.weights=time.weights,
                       ts.rets=head(ts.rets, ts.length-time.loop),
                       method=method,
                       lower=-bounds,
                       upper=bounds,
                       control=control
                       )

optim.results <- optim(portf.weights[-1],
                       function(optim.weights, first.weight, time.weights, ts.prices) optimObjective.alphaModel(ts.prices=ts.prices, portf.weights=c(first.weight, optim.weights), time.weights=time.weights),
                       first.weight=portf.weights[1],
                       time.weights=time.weights,
                       ts.prices=ts.prices,
                       method=method,
                       lower=-bounds,
                       upper=bounds,
                       control=control
                       )



####################
### Test Code    ###
####################

# fun with sapply() and rep()
assets <- c("asset1","asset2","asset3")
rep.assets <- as.vector(sapply(assets, function(x) rep(x,2)))
list.assets <- paste(rep(c("price","bo"), length(assets)), rep.assets, sep=".")


