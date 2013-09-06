

simpleEquityGraph  <- function(instrument){
  #### simple graph vs equity #####
  cdsColName <- paste(instrument,'.CDS.MID',sep='')
  equityColName <- paste(instrument,'.EQ.CLOSE',sep='')
  
  g.data  <- cbind(cds.mid.xts[,cdsColName],eq.close.xts[,equityColName])
  q.endpoints <- endpoints(g.data,on='quarters')
  q.data <- lapply(2:length(q.endpoints), function(x) g.data[q.endpoints[x-1]:q.endpoints[x]] )
  y.min <- min(na.omit(g.data[,1]))
  y.max <- max(na.omit(g.data[,1]))
  x.min <- min(na.omit(g.data[,2]))
  x.max <- max(na.omit(g.data[,2]))
  plot(y=coredata(q.data[[1]][,1]),x=coredata(q.data[[1]][,2]),ylab=paste(instrument,'.CDS',sep=''),xlab=paste(instrument,'.EQ',sep=''),col=1,ylim=c(y.min,y.max),xlim=c(x.min,x.max),pch=1)
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
}

simpleVolGraph <- function(instrument){
  ####### simple graph vs vol ###############
  
  cdsColName <- paste(instrument,'.CDS.MID',sep='')  
  volColName <- paste(instrument,'.VOL.THIRTY_DAY_IMPVOL_90',sep='')
  
  g.data  <- cbind(cds.mid.xts[,cdsColName],vol.30d90.xts[,volColName])
  q.endpoints <- endpoints(g.data,on='quarters')
  q.data <- lapply(2:length(q.endpoints), function(x) g.data[q.endpoints[x-1]:q.endpoints[x]] )
  y.min <- min(na.omit(g.data[,1]))
  y.max <- max(na.omit(g.data[,1]))
  x.min <- min(na.omit(g.data[,2]))
  x.max <- max(na.omit(g.data[,2]))
  plot(y=coredata(q.data[[1]][,1]),x=coredata(q.data[[1]][,2]),ylab=paste(instrument,'.CDS',sep=''),xlab=paste(instrument,'.30D_90PIVOL',sep=''),col=1,ylim=c(y.min,y.max),xlim=c(x.min,x.max),pch=1)
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
}


combineGraph  <- function(instrument){
  
  #print(paste('plotting ', instrument, sep=''))  
  
  simpleEquityGraph(instrument)
  simpleVolGraph(instrument)
  cdsColName <- paste(instrument,'.CDS.MID',sep='')
  equityColName <- paste(instrument,'.EQ.CLOSE',sep='')
  tsdPlotCoint(eq.close.xts[,equityColName],cds.mid.xts[,cdsColName])
}



sortByCor  <- function(instrument){
  #simpleEquityGraph(instrument)
  #simpleVolGraph(instrument)
  cdsColName <- paste(instrument,'.CDS.MID',sep='')
  equityColName <- paste(instrument,'.EQ.CLOSE',sep='')
  stats <- tsdPlotCoint(eq.close.xts[,equityColName],cds.mid.xts[,cdsColName], plotGraph=FALSE)
  test=as.numeric(abs(stats$adfTeststat))
  testResult <- cbind(instrument=instrument, test)  
  return(  testResult )
  
}

