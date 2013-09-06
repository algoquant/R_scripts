library(xts)
library(PerformanceAnalytics)
library(ROneTick)
library(foreach)
library(quantmod)
library(car)
source("C:/Devel/Models/Rmodels/tsdPlotCoint.R")
options(scipen=15,digits.secs=6)


#get meta data for the index
sn.meta  <- read.csv('//ShareDir2/sysCredit/Data/Reference/singlenames/singlenameUniverse.csv',stringsAsFactors=FALSE)
          
OTQDir<-"../trunk/OneTick/"
queryName<- paste(OTQDir, "Join_Agg_Ticks.otq::EOD_EQUITY_CDS",sep='')
beginTime <- '20090101000000'
endTime <-  paste(format(Sys.time(),'%Y%m%d', tz="GMT"),'000000',sep='')
otContext='412'

#do u want to load the data from file or from OT
reload.ot  <- FALSE
reload.file <- FALSE
#get a list of spreads in the index


if(reload.ot){
  sn.data <- lapply(1:length(sn.meta$RIC),
                       function(x, dataset) {
                         print(paste(dataset$CMATicker[x],dataset$RIC[x],dataset$BloombergEquityTicker[x]))
                         try(data.history <- oneTickQueryOTQ(queryName,
                                                         start=beginTime, end=endTime, 
                                                         SDATE='20130101',
                                                         TENOR=5,
                                                         CDS=dataset$CMATicker[x],
                                                         EQ1=dataset$RIC[x],
                                                         EQ2=paste(dataset$BloombergEquityTicker[x],'Equity',sep=' '),
                                                         context=otContext))
                       }
                       , dataset=sn.meta)
  
  ig.queryName<- paste(OTQDir, "Join_Agg_Ticks.otq::EOD_History",sep='')
  ig.spreads <- oneTickQueryOTQ(ig.queryName,
                                start=beginTime, end=endTime, 
                                SYMBOLOGY='CMAID',
                                DB='CDS_CMA_HISTORY',
                                SYMBOL='IG',
                                TENOR=5,
                                context='412')
  
}



if(reload.file){
  load('//ShareDir2/SysCredit/Data/R_Data/ig_spreads.R')
}


#flatten the list into one .xts
sn.xts.data <- do.call('cbind',sn.data)

#grep out the CDS values
pattern.cds  <- grepl(pattern='.CDS.',x=colnames(sn.xts.data))
cds.xts <- sn.xts.data[,pattern.cds]

#grep out the EQ values
pattern.eq  <- grepl(pattern='.EQ.',x=colnames(sn.xts.data))
eq.xts <- sn.xts.data[,pattern.eq]

#grep out the VOL values
pattern.vol  <- grepl(pattern='.VOL.',x=colnames(sn.xts.data))
vol.xts <- sn.xts.data[,pattern.vol]

#get IG mids
ig.mid.xts <- ig.spreads$IG.MID

#first remove the na.names
na.names  <- names(last(cds.xts[,is.na(last(cds.xts))]))
na.cols  <- which(colnames(cds.xts)%in%na.names)
cds.xts  <- cds.xts[,-na.cols]

#get data from 2010-07/ onward... when the data went SNAC CMA had naming convention issues b/t 2010-01/2010-06
date.range <- '2010-07/'

#get unique tickers
utickers <- unique(unlist(lapply(colnames(cds.xts), function (x) strsplit(x,split='.CDS.')[[1]][1])))

#get cds mid data
cds.mid.xts  <- cds.xts[date.range,paste(utickers,'.CDS.MID',sep = '')]

#get overlapping equity closing data
eq.close.xts  <- eq.xts[date.range,paste(utickers,'.EQ.CLOSE',sep='')]

#get overlapping 30D 90% moneyness vol
vol.30d90.xts <- vol.xts[date.range,paste(utickers,'.VOL.THIRTY_DAY_IMPVOL_90',sep='')]
vol.3m90.xts <- vol.xts[date.range,paste(utickers,'.VOL.THREE_MNTH_IMPVOL_90',sep='')]


#### simple graph vs equity #####
g.data  <- cbind(cds.mid.xts$WYN.CDS.MID,eq.close.xts$WYN.EQ.CLOSE)
q.endpoints <- endpoints(g.data,on='quarters')
q.data <- lapply(2:length(q.endpoints), function(x) g.data[q.endpoints[x-1]:q.endpoints[x]] )
y.min <- min(na.omit(g.data[,1]))
y.max <- max(na.omit(g.data[,1]))
x.min <- min(na.omit(g.data[,2]))
x.max <- max(na.omit(g.data[,2]))
plot(y=coredata(q.data[[1]][,1]),x=coredata(q.data[[1]][,2]),ylab='GS.CDS',xlab='GS.EQUITY',col=1,ylim=c(y.min,y.max),xlim=c(x.min,x.max),pch=1)
for(x in 2:length(q.data)) {points(y=coredata(q.data[[x]][,1]),x=coredata(q.data[[x]][,2]),col=x,pch=x)}
points(y=coredata(last(q.data[[length(q.data)]][,1])),x=coredata(last(q.data[[length(q.data)]][,2])), pch=17, col = 'red', cex=2)

date.ranges <- unlist(lapply(2:length(q.endpoints), function(x) paste(format(index(g.data[q.endpoints[x-1]+1,]),'%Y%m%d'),format(index(g.data[q.endpoints[x],]),'%Y%m%d'),sep='-')))

legend('topright',date.ranges, col =1:length(date.ranges) ,
       text.col = "green4", lty = c(2, -1, 1), pch = 1:length(date.ranges),
       merge = TRUE, bg = 'gray90')

y <- coredata(g.data[,1])
x1 <- coredata(g.data[,2])
x2  <- x1^2
mdl <- lm(y~x1+x2)
cf  <- mdl$coefficients
sm <- summary(mdl)

x1_seq <- seq(x.min,x.max,by=.01) 
x2_seq <- x1_seq^2 
x1_var  <- x1_seq * cf[2]
x2_var  <- x2_seq * cf[3]
y_seq  <-  (x1_var + x2_var) +  rep(cf[1] ,times=length(x2_seq))
lines(x=x1_seq,y=y_seq,type='l',lwd=2,col='black')
lines(x=x1_seq,y=y_seq+sm$sigma,type='l',lwd=1,col='green')
lines(x=x1_seq,y=y_seq-sm$sigma,type='l',lwd=1,col='red')

####### End of simple Graph vs equity#########

####### simple graph vs vol ###############
g.data  <- cbind(cds.mid.xts$AA.CDS.MID,vol.30d90.xts$AA)
q.endpoints <- endpoints(g.data,on='quarters')
q.data <- lapply(2:length(q.endpoints), function(x) g.data[q.endpoints[x-1]:q.endpoints[x]] )
y.min <- min(na.omit(g.data[,1]))
y.max <- max(na.omit(g.data[,1]))
x.min <- min(na.omit(g.data[,2]))
x.max <- max(na.omit(g.data[,2]))
plot(y=coredata(q.data[[1]][,1]),x=coredata(q.data[[1]][,2]),ylab='DHI.CDS',xlab='DHI.30D_90PIVOL',col=1,ylim=c(y.min,y.max),xlim=c(x.min,x.max),pch=1)
for(x in 2:length(q.data)) {points(y=coredata(q.data[[x]][,1]),x=coredata(q.data[[x]][,2]),col=x,pch=x)}
points(y=coredata(last(q.data[[length(q.data)]][,1])),x=coredata(last(q.data[[length(q.data)]][,2])), pch=17, col = 'red', cex=2)

date.ranges <- unlist(lapply(2:length(q.endpoints), function(x) paste(format(index(g.data[q.endpoints[x-1]+1,]),'%Y%m%d'),format(index(g.data[q.endpoints[x],]),'%Y%m%d'),sep='-')))

legend('bottomright',date.ranges, col =1:length(date.ranges) ,
       text.col = "green4", lty = c(2, -1, 1), pch = 1:length(date.ranges),
       merge = TRUE, bg = 'gray90')

y <- coredata(g.data[,1])
x1 <- coredata(g.data[,2])
x2  <- x1^2
mdl <- lm(y~x1+x2)
cf  <- mdl$coefficients
sm <- summary(mdl)

x1_seq <- seq(x.min,x.max,by=.01) 
x2_seq <- x1_seq^2 
x1_var  <- x1_seq * cf[2]
x2_var  <- x2_seq * cf[3]
y_seq  <-  (x1_var + x2_var) +  rep(cf[1] ,times=length(x2_seq))
lines(x=x1_seq,y=y_seq,type='l',lwd=2,col='black')
lines(x=x1_seq,y=y_seq+sm$sigma,type='l',lwd=1,col='green')
lines(x=x1_seq,y=y_seq-sm$sigma,type='l',lwd=1,col='red')

####### End of simple graph vs vol ################


##### simple weekly changes in spread vs weekly changes in equity ##########

g.data  <- cbind(cds.mid.xts$AA.CDS.MID,eq.close.xts$AA.EQ.CLOSE)
w.endpoints <- endpoints(g.data,on='weeks')
g.data.weeks  <- g.data[w.endpoints,]
g.data.weeks.diff <- na.omit(diff(g.data.weeks))
mdl.w.diff  <- lm(g.data.weeks[,1]~lag(g.data.weeks[,2]))

y.min <- min(na.omit(g.data.weeks.diff[,1]))
y.max <- max(na.omit(g.data.weeks.diff[,1]))
x.min <- min(na.omit(g.data.weeks.diff[,2]))
x.max <- max(na.omit(g.data.weeks.diff[,2]))

##### End of simple weekly changes in spread vs weekly changes in equity ##########



tsdPlotCoint(eq.close.xts$AA.EQ.CLOSE['2012-06/',],cds.mid.xts$AA.CDS.MID)

pairs(~eq.close.xts$GS+
lines()

equity.ret  <- log(equity.xts/lag(equity.xts))
equity.ret[1,]  <- 0
equity.ret <- na.locf(equity.ret)
colnames(equity.ret) <- sub(pattern='.Close',replacement='',x=colnames(equity.ret))
colnames(equity.ret) <- sub(pattern='_',replacement='/',x=colnames(equity.ret))

#get equity tickers and replace with cds tickers
equity.tickers  <- sn.meta$BloombergEquityTicker
equity.tickers  <- sub(pattern=' US',replacement='',x=equity.tickers)  
equity.tickers <- sub(pattern=' CN',replacement='',x=equity.tickers)
equity.sel  <- which(colnames(equity.ret) %in% equity.tickers )

cds.equity.ticker  <- sn.meta$CMATicker[equity.sel]
#rename equity returns
colnames(equity.ret) <- cds.equity.ticker




#get vol tickers and replace with cds tickers (only using the 30day Impvol for now)
ptrn <- grepl(pattern=three.month.110.iv,x=colnames(vol.xts))
vol.xts <- vol.xts[,ptrn]
colnames(vol.xts) <- unlist(strsplit(colnames(vol.xts),split=paste('.', three.month.110.iv,sep='')))
colnames(vol.xts) <- sub(pattern='_',replacement='/',x=colnames(vol.xts))
vol.sel  <- which(colnames(vol.xts) %in% equity.tickers )
cds.vol.ticker <- sn.meta$CMATicker[vol.sel]
colnames(vol.xts) <- cds.vol.ticker


# get the intersect of available names accross the three datasets
int.names  <- intersect(intersect(colnames(sn.ret),colnames(equity.ret)),colnames(vol.xts))
#take a look at some cross sectional regressions of equityequity/vol as a predictor for cds
#vol momentum factors using 3Month IMPVOL
vol.diff.xts  <- diff(vol.xts[,int.names])
vol.diff.xts[1,]  <- 0
vol.diff.xts <- na.locf(vol.diff.xts)
vte  <- lag(vol.diff.xts)  #daily
vte.retw  <- apply(vol.diff.xts,2,runSum,5) 
vte.retM<-apply(vol.diff.xts,2,runSum,20)
vte.retWm <- vte.retw - vte



#get the equity names that overlap with vol
equity.ret <- equity.ret[,int.names]
#get the cds returns that we have valid equity returns for
sn.overlap <-sn.ret[,int.names] 


#this is the y variable, the cds returns in risk terms (i.e. buy the top 10 names sell(buy))
rtp <- sn.overlap

# equity momentum factors
rte <- lag(equity.ret) #daily
rte.retw<-apply(equity.ret,2,runSum,5) 
rte.retM<-apply(equity.ret,2,runSum,20)
rte.retwm<-xts(rte.retw-coredata(rte),index(rte)) #weekly
rte.retMm<-xts(rte.retM-rte.retw,index(rte)) #monthly


dt <- '2013-01-01'
wts <- TTR::EMA(1:40,ratio=.8)/40
#wts  <- rep(wts,times=ncol(rtp))
smpl <- 923:1023
l.rtp <- as.numeric(rtp[smpl,])
l.rt <- as.numeric(rte[smpl])
l.retWm  <- as.numeric(rte.retwm[smpl])
l.retMm <- as.numeric(rte.retMm[smpl])
l.vte <- as.numeric(vte)
l.vetWm <- as.numeric()
#summary(lm(as.numeric(last(rtp))~as.numeric(last(rt[dt]))))
mdl  <- lm(l.rtp~l.rt+l.retWm+l.retMm)#,weights=wts)










############################Charts ################################
+############# create some meaningful groupings
  
  airlines  <- 'Airlines'
textiles  <- c('Apparel','Textiles','Housewares','Home Furnishings','Toys/Games/Hobbies')
leisure  <- c('Leisure Time','Lodging','Entertainment')
autos  <- c('Auto Parts&Equipment','Auto Manufacturers')
homebuilders  <- 'Home Builders'
chemicals  <- 'Chemicals'
mining  <- 'Mining'
metals  <- 'Iron/Steel'
paper  <-  'Forest Products&Paper'
banks  <- 'Banks'
defense  <- 'Aerospace/Defense'
computers  <- 'Computers'

i.sectors  <- list(airlines=airlines,
                   textiles=textiles,
                   leisure=leisure,
                   autos=autos,
                   homebuilders=homebuilders,
                   chemicals=chemicals,
                   mining = mining,
                   metals = metals,
                   paper=paper,
                   banks=banks,
                   defense=defense,
                   computers=computers)
#get the names for each sector then retireve the constinuent spreads
sector.names <- lapply(i.sectors,function(x) sn.meta$CMATicker[which(sn.meta$INDUSTRY_GROUP%in%x)])
sector.spreads <- lapply(sector.names, function(x) sn.ret[,x])
sector.ave  <- xts(do.call('cbind',lapply(sector.spreads,function(x) rowSums(x)/ncol(x))),index(sn.ret))

### take a look at the sectors over the last year or so
par(cex.lab=.8) 
op <- par(no.readonly = TRUE)
layout(matrix(c(1, 2)), height = c(2, 1.3), width = 1)
par(mar = c(1, 4, 1, 2)) #c(bottom, left, top, right)
chart.CumReturns(sector.ave['2012/'], main = "", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", colorset= rainbow12equal, ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)
par(mar = c(4, 4, 0, 2))
chart.Drawdown(sector.ave['2012/'], main = "", ylab = "Drawdown", colorset = rainbow12equal, cex.axis=.6, cex.lab=.7)
dev.off()

#take a look at the bar charts. Weekly so data is easily viewed
temp.cum  <- sector.ave['2012/']
ep  <- endpoints(temp.cum, on='weeks')
sector.weekly <- diff(temp.cum[ep,])

charts.BarVaR(sector.weekly, p=(1-1/12), gap=1, main="", show.greenredbars=TRUE, 
              methods='none', show.endvalue=TRUE, #c("ModifiedES", "ModifiedVaR")
              colorset=rep("Black",7), ylim=c(-.02,.02)) #this sets the range so +/- 2%


#take a look at the correlations 
require("corrplot")
col3 <- colorRampPalette(c("darkgreen", "white", "darkred"))
M <- cor(sector.ave['2012/'])
colnames(M) = rownames(M) = colnames(sector.ave)
order.hc2 <- corrMatOrder(M, order="hclust", hclust.method="complete")
M.hc2 <- M[order.hc2,order.hc2]

corrplot(M.hc2, tl.col="black", tl.cex=0.8, method="square", col=col3(8), cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25)
corrRect.hclust(M.hc2, k=3, method="complete", col="blue") #looks fairly reasonable
dev.off()
####################end Charts ################################