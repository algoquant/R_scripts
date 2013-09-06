# Load generic libraries
library(xts)
library(xtsExtra)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)
# Load ROneTick
library(ROneTick)
oneTickLib()

### Kalman Filter models Using package dlm
library(dlm)

Sys.setenv(TZ="UTC")

setwd("c:/Devel/Models/trunk/R/alphaLib")
source("C:/Devel/Models/trunk/R/alphaLib/defaults.R")


#########################
### Load CDS Data     ###
#########################
load(file="S:/Data/R_Data/data_topglob.RData")

symbols.data <- read.csv('S:/Data/R_Data/TOPNAIG_Symbols.csv', stringsAsFactors=FALSE)
symbols.itx <- read.csv('S:/Data/R_Data/ITX_SOV_Symbols.csv', stringsAsFactors=FALSE)
symbols.cdx <- read.csv('S:/Data/R_Data/CDX_SOV_Symbols.csv', stringsAsFactors=FALSE)

top.symbols <- symbols.data

# Define query parameters
OTQDir <- "../../OneTick/"
init.query <- paste(OTQDir, "Scrub_Ticks.otq::Get_Sane_Ticks_Priming",sep='')
main.query <- paste(OTQDir, "Join_Agg_Ticks_03-19-13.otq::Single_Median",sep='')
start.date <- '20121110000000'
# end.date <- '20130304000000'
end.date <- paste(format(Sys.time(),'%Y%m%d', tz="GMT"),'000000',sep='')
start.time <- '073000000'
end.time <- '163000000'
end.morning.time <- '163000000'
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

### Load Median CDS quotes for a list of symbols
list.prices <- lapply(1:nrow(top.symbols), function(n.row)
                      {
# Load median ticks for initializing scrubber
                        symbol <- top.symbols$CMATicker[n.row]
                        time.zone <- top.symbols$TimeZone[n.row]
                        calendar.query <- top.symbols$Calendar[n.row]
                        symbology.query <- top.symbols$Symbology[n.row]
                        deal.type <- top.symbols$DealType[n.row]
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

# cbind into a single xts
ts.prices <- do.call('cbind', list.prices)

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
# Fix the time index
index(ts.prices) <- index(ts.prices)


#########################
### Load Index Data   ###
#########################
symbol <- 'IG'
symbology.query <- 'CMAID'
ts.prices.ig <- oneTickQueryOTQ(main.query,
                                start=start.date, end=end.date, 
                                START_TIME=start.time,
                                END_TIME=end.morning.time,
                                BUCKET_INTERVAL_UNITS='DAYS',
                                BUCKET_INTERVAL=1,
                                SYMBOL=symbol,
                                BID_SCRUB=bid.query,
                                OFFER_SCRUB=offer.query,
                                DEALTYPE=deal.type,
                                SYMBOLOGY=symbology.query,
                                DB=db.query,
                                CALENDAR=calendar.query,
                                SDATE=symbol.date,
                                TERM=term.query,
                                TZ=time.zone,
                                context=context.query)
# Daily aggregation will cause timestamp to be shifted one day ahead - shift back one day
index(ts.prices.ig) <- as.Date(index(ts.prices.ig)-1)
index(ts.prices.ig) <- as.Date(trunc(index(ts.prices.ig), units='days'))

# Rename columns and combine IG and CDS data
index(ts.prices.ig) <- index(ts.prices.ig)
ts.prices <- cbind(ts.prices.ig[,'MEDIAN'],ts.prices)
colnames(ts.prices)[1] <- 'IG'
# Check for NAs
sum(is.na(ts.prices))
which(is.na(ts.prices))
# ts.prices[,1] <- na.locf(ts.prices[,1])
save(ts.prices, file="S:/Data/R_Data/data_topglob.RData")


# Calculate percentage returns
look.back <- 2
ts.rets <- diff(ts.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]
ts.lrets <- diff(log(ts.prices),lag=look.back)
ts.lrets[1:look.back,] <- ts.lrets[look.back+1,]


######################################################
### Load number of quotes for a list of symbols    ###
######################################################
main.query <- paste(OTQDir, "Scrub_Ticks.otq::Get_Scrub_Ticks",sep='')
start.date <- last(index(ts.prices),5)[1]
start.date <- paste(unlist(strsplit(x=as.character(start.date), split="-")), collapse="")
start.date <- paste(start.date,'000000',sep='')
# end.date <- '20130304000000'
end.date <- paste(format(Sys.time(),'%Y%m%d', tz="GMT"),'000000',sep='')
start.time <- '073000000'
end.time <- '163000000'

### Load number of quotes for a list of symbols
list.prices <- lapply(1:nrow(top.symbols), function(n.row)
                      {
# Initialize parameters
                        symbol <- top.symbols$CMATicker[n.row]
                        time.zone <- top.symbols$TimeZone[n.row]
                        calendar.query <- top.symbols$Calendar[n.row]
                        symbology.query <- top.symbols$Symbology[n.row]
                        deal.type <- top.symbols$DealType[n.row]
                        write(symbol, stdout())
# Load scrubbed ticks
                        ts.ticks <- tryCatch(oneTickQueryOTQ(main.query,
                                                     start=start.date, end=end.date, 
                                                     START_TIME=start.time,
                                                     END_TIME=end.time,
                                                     SYMBOL=symbol,
                                                     OFFER_SCRUB=offer.query,
                                                     BID_SCRUB=bid.query,
                                                     SYMBOLOGY=symbology.query,
                                                     DEALTYPE=deal.type,
                                                     DB=db.query,
                                                     CALENDAR=calendar.query,
                                                     SDATE=symbol.date,
                                                     TERM=term.query,
                                                     TZ=time.zone,
                                                     context=context.query),
                                               error=function(e) writeMessage(paste(symbol, e)), warning=function(w) writeMessage(w))

                        n.ticks <- nrow(ts.ticks)
                        names(n.ticks) <- symbol
                        n.ticks
                      }
                      )
# End lapply

# cbind into a single xts
n.ticks <- unlist(list.prices)
# n.ticks <- n.ticks[order(n.ticks, decreasing=TRUE)]

# Screen out tight and illiquid credits
last.prices <- as.vector(last(ts.prices))
rows.top <- match(colnames(ts.prices[,(last.prices>quantile(last.prices)[2]) & (n.ticks>quantile(n.ticks,0.15))]), symbols.data[,1])
ts.prices <- ts.prices[,rows.top]


############################################
### Calculate regression credit betas    ###
############################################
profile.beta <- as.matrix(apply(matrix(2:ncol(ts.prices)), 1,
                       function(n.col)
                       {
#                         browser()
                         formula.lm <- as.formula(paste(colnames(ts.prices[,n.col]),"~",'IG',sep=" "))
                         lm.ig <- lm(formula.lm, data=ts.prices['2012-10-19/',])
                         c(lm.beta=summary(lm.ig)$coefficients[2,1],lm.error=summary(lm.ig)$coefficients[2,2],lm.t.value=summary(lm.ig)$coefficients[2,3],lm.r.squared=summary(lm.ig)$r.squared)
                       }
                       ))
# End apply
colnames(profile.beta) <- colnames(ts.prices[,-1])
profile.beta <- t(profile.beta)
# profile.beta <- cbind(colnames(ts.prices[,-1]),t(profile.beta))
# Sort the list according to highest regression r.squared
profile.beta <- profile.beta[order(profile.beta[,'lm.r.squared'], decreasing=TRUE),]
# Sort the list according to highest regression t.value
profile.beta <- profile.beta[order(profile.beta[,'lm.t.value'], decreasing=TRUE),]
# Create scatterplot of t.value versus r.squared
plot(lm.r.squared~lm.t.value, data=profile.beta, xlab='lm.t.value', ylab='lm.r.squared', main='lm.r.squared vs lm.t.value')

### Calculate hedge ratios for hedging CDS with index
betas <- matrix(0.1*(0:40))
hedge.ratios <- as.matrix(apply(matrix(2:ncol(ts.prices)), 1,
                       function(n.col)
                       {
# name.symbol <- 'CTL'
                         name.symbol <- colnames(ts.prices)[n.col]
                         ts.hedged <- as.matrix(apply(betas, 1, function(beta) as.vector(ts.prices[,name.symbol] - beta*ts.prices[,'IG']) ))
                         ts.hedged <- xts(ts.hedged, index(ts.prices))
                         colnames(ts.hedged) <- betas
                         trend.scores <- as.matrix(apply(na.omit(diff(ts.hedged)), 2, var.ratio, agg.min=1, agg.max=5))
#                         chart.TimeSeries(ts.hedged, main=paste('Hedge profiles for', name.symbol), legend.loc='topleft')
#                         chart.TimeSeries(ts.hedged[,c(1,which.max(trend.scores))], main=paste('Hedge profiles for', name.symbol), legend.loc='topleft')
                         max.index <- which.max(trend.scores)
                         c(hedge=as.numeric(rownames(trend.scores)[max.index]), score=trend.scores[1], score.hedged=trend.scores[max.index])
                       }
                                ))
# End apply

hedge.ratios <- t(hedge.ratios)
rownames(hedge.ratios) <- colnames(ts.prices[,-1])
hedge.ratios <- hedge.ratios[order(hedge.ratios[,'score.hedged'], decreasing=TRUE),]
write.csv(hedge.ratios, "S:/Data/R_Data/hedge.ratios.csv")

### Plot CDS with hedges
# symbols.data <- read.csv('S:/Data/R_Data/HEDGED_Symbols.csv', stringsAsFactors=FALSE)
ts.hedged <- as.matrix(apply(matrix(rownames(hedge.ratios)), 1,
                       function(name.symbol)
                       {
#                         name.symbol <- symbols.data[name.symbol,'CMATicker']
                         beta <- hedge.ratios[name.symbol,'hedge']
                         return(ts.prices[,name.symbol] - beta*ts.prices[,'IG'])
                       }
                                ))
# End apply
ts.hedged <- xts(ts.hedged, index(ts.prices))
# colnames(ts.hedged) <- symbols.data[,'CMATicker']
colnames(ts.hedged) <- rownames(hedge.ratios)
plot.zoo(ts.hedged[,1:11], main=paste('Hedged CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))


#########################
### Screen Data       ###
#########################

### Define Kalman filter model
dlm.Kalman <- dlmModPoly(order=2, dV=100.1)

### Calculate filtered returns
start.date <- '2013-01-03/'
ts.filter.lrets <- apply(matrix(colnames(ts.lrets)), 1, function(n.symbol) 
                         {
                           filter.kalman <- dlmFilter(y=ts.lrets[,n.symbol], mod=dlm.Kalman)
                           filter.kalman$m[-1,1]
                         }
                         )
# End lapply
ts.filter.lrets <- xts(ts.filter.lrets, order.by=index(ts.prices))
colnames(ts.filter.lrets) <- colnames(ts.lrets)

### Screen time series by hurst exponents
hurst.exponents <- as.matrix(apply(ts.rets, 2, Hurst))
# Sort the symbols by highest hurst exponents
hurst.exponents <- as.matrix(hurst.exponents[order(hurst.exponents, decreasing=TRUE),1])
colnames(hurst.exponents) <- 'Hurst.Exponents'
top.hurst.symbols <- rownames(hurst.exponents)
plot.zoo(ts.prices['2013-02-03/', top.hurst.symbols[1:11]], main=paste('Top CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))


### Screen time series by variance ratios
var.ratios <- as.matrix(apply(ts.rets, 2, var.ratio, agg.min=1, agg.max=5))
# Sort the symbols by highest variance ratios
var.ratios <- as.matrix(var.ratios[order(var.ratios, decreasing=TRUE),1])
colnames(var.ratios) <- 'Var.Ratios'
top.var.symbols <- rownames(var.ratios)
plot.zoo(ts.prices['2013-02-03/', top.var.symbols[1:11]], main=paste('Top CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))


### Screen time series for recent trends
row.num <- nrow(ts.lrets)
sd.lrets <- sqrt(apply(ts.lrets, 2, var))
norm.lrets <- as.vector(ts.filter.lrets[row.num,])/sd.lrets
top.momentum.symbols <- colnames(ts.lrets)[order(abs(norm.lrets), decreasing=TRUE)]
# Plot top recent trends
plot.zoo(ts.prices['2013-02-03/', top.momentum.symbols[1:11]], main=paste('Top CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))


### Combine variance ratio scores with momentum scores
cds.scores <- cbind(match(colnames(ts.prices), top.momentum.symbols), match(colnames(ts.prices), top.var.symbols))
cds.scores <- cbind(cds.scores, rowSums(cds.scores))
colnames(cds.scores) <- c('momentum.score','var.score', 'combined')
rownames(cds.scores) <- colnames(ts.prices)

top.symbols <- rownames(cds.scores)[order(cds.scores[,'combined'], decreasing=FALSE)]
cds.scores[top.symbols,]
# top.symbols <- colnames(ts.lrets)[order(abs(norm.lrets), decreasing=TRUE)]
plot.zoo(ts.prices['2013-03-03/', top.symbols[1:11]], main=paste('Top CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))


### Plot in loop over a list of symbols, and prompt before plotting
start.date <- '2013-02-03/'
par(ask=TRUE)
apply(matrix(head(top.symbols,11)), 1, function(n.symbol) 
      {
# Filter the returns first
        ts.data <- ts.lrets[start.date,n.symbol]
        filter.kalman <- dlmFilter(y=ts.data, mod=dlm.Kalman)
        ts.filter.lrets <- xts(filter.kalman$m[-1,1], order.by=index(ts.data))
        colnames(ts.filter.lrets) <- paste(n.symbol, 'filtered.rets')
# Find dates when returns flipped
        ts.flips <- diff(sign(ts.filter.lrets))[which(abs(diff(sign(ts.filter.lrets)))>0),]
        ts.flips <- rbind(ts.flips,last(ts.filter.lrets))
        dates.flips <- do.call('cbind', apply(matrix(1:(nrow(ts.flips)-1)), 1,
                                              function(n.row) if(ts.flips[n.row,]>0) paste(index(ts.flips[n.row,]),'/',index(ts.flips[n.row+1,]),sep='')
                                              ))
# Filter the prices/spreads
        ts.data <- ts.prices[start.date,n.symbol]
        filter.kalman <- dlmFilter(y=ts.data, mod=dlm.Kalman)
        ts.filter.prices <- xts(filter.kalman$m[-1,1], order.by=index(ts.data))
        colnames(ts.filter.prices) <- paste(n.symbol, 'filtered.prices')
# Plot
        chart.TimeSeries(ts.data, period.areas=c(dates.flips), period.color='gray', main=paste(n.symbol, 'Prices and Kalman filtered signals'), colorset=c(1), lty=c(1), ylab="", xlab="", legend.loc='topright')
        n.symbol
      }
      )
# End lapply
par(ask=FALSE)


#########################
### Monitor Data      ###
#########################

### Calculate returns for sector portfolios
top.symbols <- read.csv('S:/Data/R_Data/TOPNAIG_Symbols_ordered.csv', stringsAsFactors=FALSE)
# rets.portfolios <- NULL

# Calculate and plot in loop
par(ask=TRUE)
rets.portfolios <- apply(as.matrix(unique(top.symbols$Portfolio)), 1, function(n.portfolio)
{
  write(n.portfolio, stdout())
# Calculate portfolio returns
  n.rows <- which(top.symbols$Portfolio==n.portfolio)
  n.notionals <- top.symbols$Notional[n.rows]
  ts.portfolio <-  (ts.rets[,top.symbols$CMATicker[n.rows]] %*% n.notionals)/sum(n.notionals)
  ts.portfolio <- xts(ts.portfolio, index(ts.rets))
  colnames(ts.portfolio) <- n.portfolio
# Filter the returns first
  filter.kalman <- dlmFilter(y=ts.portfolio, mod=dlm.Kalman)
  ts.filter.rets <- xts(filter.kalman$m[-1,1], order.by=index(ts.portfolio))
  colnames(ts.filter.rets) <- paste(n.portfolio, 'filtered.rets')
# Find dates when returns flipped
  ts.flips <- diff(sign(ts.filter.rets))[which(abs(diff(sign(ts.filter.rets)))>0),]
  ts.flips <- rbind(ts.flips,last(ts.filter.rets))
  dates.flips <- do.call('cbind', apply(matrix(1:(nrow(ts.flips)-1)), 1,
                                        function(n.row) if(ts.flips[n.row,]>0) paste(index(ts.flips[n.row,]),'/',index(ts.flips[n.row+1,]),sep='')
                                        ))
# Plot
  chart.TimeSeries(cumsum(ts.portfolio), period.areas=c(dates.flips), period.color='gray', main=paste(n.portfolio, 'Prices and Kalman filtered signals'), colorset=c(1), lty=c(1), ylab="", xlab="", legend.loc='topright')
#  rets.portfolios <<- cbind(rets.portfolios,ts.portfolio)
  ts.portfolio
}
)
par(ask=FALSE)

# Combine portfolio returns
rets.portfolios <- xts(rets.portfolios, index(ts.rets))
colnames(rets.portfolios) <- unique(top.symbols$Portfolio)
plot.zoo(cumsum(rets.portfolios['2013-01-03/',]), main=paste('Sector portfolios', '/', format(Sys.time(),'%m-%d-%y', tz="GMT")))
n.portfolio <- 'Idiosyncratic'
plot.zoo(ts.prices['2013-01-03/',top.symbols$CMAEntityId[which(top.symbols$Portfolio==n.portfolio)]], main=paste(n.portfolio, '/', format(Sys.time(),'%m-%d-%y', tz="GMT")))
as.matrix(apply(rets.portfolios, 2, var.ratio, agg.min=1, agg.max=5))
# Plot portfolios in loop
par(ask=TRUE)
apply(as.matrix(unique(top.symbols$Portfolio)), 1, function(n.portfolio)
{
  plot.zoo(ts.prices['2013-01-03/',top.symbols$CMAEntityId[which(top.symbols$Portfolio==n.portfolio)]], main=paste(n.portfolio, '/', format(Sys.time(),'%m-%d-%y', tz="GMT")))
  n.portfolio
}
)
par(ask=FALSE)


### Load and screen data
ts.tmp <- read.csv('S:/Data/R_Data/tmp.csv', stringsAsFactors=FALSE)
ts.tmp <- xts(ts.tmp[,-1], order.by=as.POSIXlt(ts.tmp[,1]))
colnames(ts.tmp) <- 'prices'
chart.TimeSeries(ts.tmp, main=paste('Prices / ', format(Sys.time(),'%m-%d-%y', tz="GMT")), legend.loc='topleft')
rets.tmp <- diff(log(ts.tmp),lag=1)
# rets.tmp <- diff(ts.tmp,lag=1)
rets.tmp[1,] <- 0
chart.TimeSeries(cumsum(rets.tmp), main=paste('Indices / ', format(Sys.time(),'%m-%d-%y', tz="GMT")), legend.loc='topleft')
# plot.zoo(ts.tmp, main=paste('Indices / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))
as.matrix(apply(rets.tmp, 2, var.ratio, agg.min=1, agg.max=5))
as.matrix(apply(rets.tmp['2012/'], 2, Hurst))

betas <- matrix(0.1*(0:10))
ts.hedged <- as.matrix(apply(betas, 1, function(beta) var.ratio(rets.tmp[,2] - beta*rets.tmp[,1], agg.min=1, agg.max=5) ))

ts.hedged <- as.matrix(apply(betas, 1, function(beta) as.vector(rets.tmp[,2] - beta*rets.tmp[,1]) ))
ts.hedged <- xts(ts.hedged, index(rets.tmp))
colnames(ts.hedged) <- betas
chart.TimeSeries(cumsum(ts.hedged), main=paste('CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")), legend.loc='topleft')


### Calculate hedge ratios for hedging CDS with index
betas <- matrix(0.1*(0:40))
tickers <- matrix(colnames(ts.rets)[-1])
hedge.ratios <- as.matrix(apply(tickers, 1, function(ticker)
                         {
                           ts.hedged <- as.matrix(apply(betas, 1, function(beta) as.vector(ts.rets[,ticker] - beta*ts.rets[,'IG']) ))
                           ts.hedged <- xts(ts.hedged, index(ts.rets))
                           colnames(ts.hedged) <- betas
                           trend.scores <- as.matrix(apply(ts.hedged, 2, var.ratio, agg.min=1, agg.max=5))
#                           trend.scores <- as.matrix(apply(ts.hedged, 2, Hurst))
                           max.index <- which.max(trend.scores)
                           c(hedge=as.numeric(rownames(trend.scores)[max.index]), score=trend.scores[1], score.hedged=trend.scores[max.index])
                         }
                                ))
# End apply
hedge.ratios <- t(hedge.ratios)
rownames(hedge.ratios) <- colnames(ts.rets[,-1])
hedge.ratios <- hedge.ratios[order(hedge.ratios[,'score.hedged'], decreasing=TRUE),]
write.csv(hedge.ratios, "S:/Data/R_Data/hedge.ratios.hurst.csv")


hedge.ratios <- read.csv('S:/Data/R_Data/hedge.ratios.csv', stringsAsFactors=FALSE)
tickers <- matrix(hedge.ratios$CMATicker)
# tickers <- matrix(head(hedge.ratios$CMATicker))
ts.hedged <- as.matrix(apply(tickers, 1, function(ticker)
                             {
                               beta <- hedge.ratios[which(hedge.ratios$CMATicker==ticker),'hedge.ratio']
                               as.vector(ts.rets[,ticker] - beta*ts.rets[,1])
                             }
                             ))
colnames(ts.hedged) <- paste(hedge.ratios$CMATicker, '/', hedge.ratios$hedge.ratio)
ts.hedged <- xts(ts.hedged, index(ts.rets))
plot.zoo(cumsum(ts.hedged['2013-03-03/',1:11]), main=paste('CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))

### Screen time series by hurst exponents
hurst.exponents <- as.matrix(apply(ts.hedged, 2, Hurst))
hurst.exponents <- as.matrix(hurst.exponents[order(hurst.exponents, decreasing=TRUE),1])
colnames(hurst.exponents) <- 'Hurst.Exponents'
top.hurst.symbols <- rownames(hurst.exponents)
plot.zoo(cumsum(ts.hedged['2013-03-03/', top.hurst.symbols[1:11]]), main=paste('Top CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))

### Screen time series by variance ratios
var.ratios <- as.matrix(apply(ts.hedged, 2, var.ratio, agg.min=1, agg.max=5))
# Sort the symbols by highest variance ratios
var.ratios <- as.matrix(var.ratios[order(var.ratios, decreasing=TRUE),1])
colnames(var.ratios) <- 'Var.Ratios'
top.var.symbols <- rownames(var.ratios)
plot.zoo(cumsum(ts.hedged['2013-03-03/', top.var.symbols[1:11]]), main=paste('Top CDS / ', format(Sys.time(),'%m-%d-%y', tz="GMT")))
