


#get meta data for the index
sn.meta  <- read.csv('//ShareDir2/sysCredit/Data/Reference/singlenames/singlenameUniverse.csv',stringsAsFactors=FALSE)

OTQDir<-"../trunk/OneTick/"
queryName<- paste(OTQDir, "Join_Agg_Ticks.otq::EOD_EQUITY_CDS",sep='')
beginTime <- '20100701000000'
endTime <-  paste(format(Sys.time(),'%Y%m%d', tz="GMT"),'000000',sep='')
otContext='412'

#do u want to load the data from file or from OT
reload.ot  <- TRUE
reload.file <- FALSE
#get a list of spreads in the index
#load(file='sn.data')
#load(file='ig.spreads')

if(reload.ot){
  sn.data <- lapply(1:length(sn.meta$RIC),      
                    #sn.data <- lapply(1:length(sn.meta$RIC),
                    #sn.data <- lapply(1:10,
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
if(length(na.cols)!=0){
  cds.xts  <<- cds.xts[,-na.cols]
}

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

## hash out of the unique names within this list
instruments <- names(last(cds.xts[,grep(pattern='CDS.MID',names(cds.xts))]))

instruments <-unique( lapply(instruments, function(x){ unlist(strsplit(x,'\\.'))[1] }) )

resultCor <- lapply(instruments,sortByCor)
resultCor <- do.call('rbind',resultCor)
resultCor <- as.data.frame(resultCor,stringsAsFactors=FALSE)
resultCor <- data.frame(instrument=resultCor$instrument,test=as.numeric(resultCor$test),stringsAsFactors=FALSE)
resultCor <- resultCor[order(resultCor[,2],decreasing=TRUE),]
resultCor.1.8 <- resultCor[resultCor[,2]>=2.8,]



