
library(xts)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)
library(ROneTick)
library(robfilter)
library(snow)
library(parallel)
oneTickLib()

### automatically configured the cluster to use 75% of available cores.
clusterNodes <- floor(detectCores() * .75)

#setwd('C:/Devel/Models/trunk/R/BTST/')
#source("../alphaLib/defaults.R")
source('C:/Devel/Models/trunk/R/alphaLib/alphaModel.R')
source('C:/Devel/Models/trunk/R/alphaLib/utilLib.R')
source('C:/Devel/Models/trunk/R/JR/sgfilter.R')
#load the test data for three years
load('//ShareDir2/SysCredit/Data/R_Data/testData.R')
load('//ShareDir2/SysCredit/Data/R_Data/in.sample.xts.R')
setwd('C:/Devel/Models/trunk/R/BTST/')


library(xts)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)
library(ROneTick)
library(robfilter)
library(snow)
oneTickLib()

#setwd('C:/Devel/Models/trunk/R/BTST/')
#source("../alphaLib/defaults.R")
source('C:/Devel/Models/trunk/R/alphaLib/alphaModel.R')
source('C:/Devel/Models/trunk/R/alphaLib/utilLib.R')
source('C:/Devel/Models/trunk/R/JR/sgfilter.R')
#load the test data for three years
load('//ShareDir2/SysCredit/Data/R_Data/testData.R')
load('//ShareDir2/SysCredit/Data/R_Data/in.sample.xts.R')
#create data and proxies for the model
ts.eq.proxy <- 100+4*(500-5*ts.prices.ighyes[,'IG.MEDIAN'])/100
ts.eq.proxy <- 1.2*ts.eq.proxy[,1]-0.2*ts.prices.ighyes[,'HY.MEDIAN']

ts.diff <- diff(ts.eq.proxy[,1])
ts.diff[1,] <- 0.0
ts.prices <- cumsum(ts.diff)
colnames(ts.prices) <- 'ES.PROXY'
ts.bidoffers <- xts(rep(0.25, nrow(ts.prices)), order.by=index(ts.prices))
names(ts.bidoffers) <- 'BID.OFFERS'
ts.ancillary <- cbind(ts.prices, ts.bidoffers)

ts.diff <- diff(ts.prices.ighyes[,'ES.EQMID'])
ts.diff[1,] <- 0.0
ts.priceLevels <- cumsum(ts.diff)


#calculate price samples
width <- 60
period <- 'hours'


ep <- endpoints(ts.priceLevels,on=period)
iTo <- length(ep) - 1
iFrom <- width + 1


#this is an xts of our outputs
in.sample.xts <- NULL           
#find the max pnl for each three month sample

#create a cluster, this is invoking 4 local sockets. 
cl <- makeCluster(12, type = "SOCK")
#load environment on each cluster
clusterEvalQ(cl, source('C:/Devel/Models/trunk/R/alphaLib/alphaModel.R'))
clusterEvalQ(cl, source('C:/Devel/Models/trunk/R/alphaLib/utilLib.R'))
clusterEvalQ(cl, source('C:/Devel/Models/trunk/R/JR/sgfilter.R'))
clusterEvalQ(cl, library(xts))
clusterEvalQ(cl, library(quantmod))
clusterEvalQ(cl, library(PerformanceAnalytics))

l.profiles <- vector('list',length(ep))
rolling.signal  <- ts.priceLevels * NA
##### beginning of massive for loop ########
for(i in iFrom:iTo){
  
  #i=723
  #get the index of the insample data
  indicesFit <- (ep[i-width]+1):ep[i]
  is.priceLevels <- ts.priceLevels[indicesFit,]
  is.ancillary <- ts.ancillary[indicesFit,]
  is.bidoffers <- ts.bidoffers[indicesFit,]
  #create an IS model
  model.test <- alphaModel("In Sample Tester")
  func.signal <- list(signal.func="signals.ancillary1.1.alphaModel",
                      filter.func="filtSavGol",
                      filter.params=c(2,0,0,0),  #set these to zero for sanity check, should bomb if it isn't updated
                      normalize.returns=FALSE,
                      normalize.signal=FALSE)
  trading.rules <- list(rules.func="fillOrders4.1.alphaModel", rules.params=c(0,0))
  model.test <- update.alphaModel(model=model.test,
                                  func.signal=func.signal,
                                  trading.rules=trading.rules,
                                  ts.prices=is.priceLevels,
                                  ts.bidoffers=is.bidoffers,
                                  ts.ancillary=is.ancillary)
  
  
  #create data.frame of possible parameters
  combs <- expand.grid(threshold=seq(.3, 2.0, by= .01),                     
                       width=20,
                       window=10)
  
  
  #itterate over the parameter space to find best combination of parameters
  print(paste('Running for three months ending:',index(last(is.priceLevels)),"Current Time:",Sys.time()))
  system.time(profiles <- parApply(cl, combs, 1, function(comb,model,price.history,ep,i) {
    
    trading.rules <- list(rules.params=c(comb[['threshold']], model$rules.list$rules.params[2]))
    func.signal <- list(filter.params=c(model$signal.list$filter.params[1],comb[['width']],
                                        comb[['window']],
                                        comb[['window']]))
    model <- update.alphaModel(model=model,
                               func.signal=func.signal,
                               trading.rules=trading.rules)
    model <- recalc.alphaModel(model)
    last.1M <- index(price.history[(ep[i-1]+1):ep[i]])
    list(threshold=comb[['threshold']],
         width=comb[['width']],
         window=comb[['window']],
         pnl=sum(model$pnls[last.1M]))
  },model.test,ts.priceLevels,ep,i))
  
  #get the min and max profiles and add them to model listv here
  #browser()
  pnls <- unlist(lapply(profiles, function(x) x$pnl))
  #pnls <- 
  thresholds <- unlist(lapply(profiles, function(x) x$threshold))
  widths <- unlist(lapply(profiles, function(x) x$width))
  p.matrix  <- matrix(c(pnls,thresholds,widths),ncol=3,nrow=length(pnls),byrow=FALSE)
  
  t.quantile <- quantile(pnls,.8)
  unique.pnls  <- unique(pnls[pnls>=t.quantile])
  probs <- sapply(unique(pnls[pnls>=t.quantile]),function(x) length(which(pnls%in%x))/length(pnls))
  mode.pnl <- unique.pnls[which(probs%in%max(probs))]
  
  max.pnl <- max(pnls)
  mode.location <- which(pnls %in% mode.pnl)
  max.location <- which(pnls %in% max.pnl)
  
  mode.profile <- profiles[[mode.location[1]]]
  max.profile <- profiles[[max.location[1]]]
  
  #check if your trials have been initialized. 
  if(is.null(in.sample.xts)){
    #add two columns for EP and PNL
    in.sample.xts <- xts(matrix(rep(NA,(length(names(combs))+2)*(length(ep)-1)),
                                ncol=(length(combs)*2)+3,
                                nrow=length(ep)-1),
                         order.by=index(ts.priceLevels[tail(ep,-1)]))
    names(in.sample.xts) <- c('EP','PNL.max',paste(names(combs),'.max',sep=''),'PNL.mode',paste(names(combs),'.mode',sep=''))
  }
  
  in.sample.xts[i-1,] <- c(i,max.profile$pnl,unlist(max.profile[names(combs)]),mode.profile$pnl,unlist(mode.profile[names(combs)]))
  l.profiles[[i-1]] <- p.matrix
}

##### end  of massive for loop ########                                                    
#### stop the cluster #####
stopCluster(cl)


##### Get the top quintile of profiles each week ########
t.q.profiles <- sapply(l.profiles, 
                       function(x){
                         if(is.null(x)){
                           NULL
                         }else{
                           t.quintile <- quantile(x[,1],.75)
                           t.quintile.vec  <- x[x[,1] >= t.quintile]
                           matrix(t.quintile.vec,nrow=length(t.quintile.vec)/3,ncol=3)
                         }
                       } )  


#get the overlap of parameters in the current itteration vs the prior
o.l.prior <- sapply(1:(length(t.q.profiles)),
                    function(x){
                      if(x!=1){
                        prior <- t.q.profiles[[x-1]]
                        current <- t.q.profiles[[x]]
                        if(is.null(prior)||is.null(current)){
                          NULL
                        }else{
                          current[which(current[,2]%in%prior[,2]),]
                        }
                      }else{
                        NULL
                      }
                    })


#get a best guess for initial approximation, this is the overlap of the modes with the prior in the top quartile of the distribution
b.guess <- sapply(1:length(o.l.prior), 
               function(x){
                 if(!is.null(o.l.prior[[x]]))
                 { 
                   if(class((o.l.prior[[x]]))=='numeric') {
                     o.l.prior[[x]][2]
                   }else{
                     m.pnl <- max(o.l.prior[[x]][,1])
                     m.loc <- which(o.l.prior[[x]][,1]%in%m.pnl)
                     m.thres <- median(o.l.prior[[x]][,2])
                     #print(class(m.thres))
                     if(is.na(m.thres)) { m.thres<- m.t.q.thresh[x]}
                     m.thres
                   }
                 }
                 else{
                   if(!is.null(t.q.profiles[[x]])){
                      m.pnl <- max(t.q.profiles[[x]][,1])
                      m.loc <- which(t.q.profiles[[x]][,1]%in%m.pnl)
                      m.thres <- median(t.q.profiles[[x]][,2])
                      m.thres
                   }else{
                     NA
                   }
                 }
               })
b.guess <- unlist(b.guess)




summary.t.q <- sapply(t.q.profiles,function(x) summary(x[,2]))
median.t.q <- unlist(sapply(summary.t.q,
                            function(x){
                              if(is.na(x['Median'])){
                                NA
                              }else{
                                x['Median']
                              }
                            }))








new.test <- in.sample.xts
#create an initial (unforcasted) attempt to fit the prior
new.test$threshold.mode <- sim.threshold[1:nrow(new.test)]
###calculate the out of sample one month period####
out.sample.xts <- in.sample.xts[,1:5]*NA
#### beginning of apply function ######
#### this assumes assumes a 1 month lag before we recalibrate #####
blah <- apply(na.omit(new.test),1,function(x) {
  
  e.p <- x[['EP']] #+ 1 #get one period forward and calculate 
  b.p <- e.p - width
  
  indicesFit <- ep[b.p]:ep[e.p]
  
  os.priceLevels <- ts.priceLevels[indicesFit,]
  os.ancillary <- ts.ancillary[indicesFit,]
  os.bidoffers <- ts.bidoffers[indicesFit,]
  
  width <- x[['width.mode']]
  window <- x[['window.mode']]
  threshold <- x[['threshold.mode']]
  
  model <- alphaModel("Out of  Sample Tester")
  func.signal <- list(signal.func="signals.ancillary1.1.alphaModel",
                      filter.func="filtSavGol",
                      filter.params=c(2,width,window,0),
                      normalize.returns=FALSE,
                      normalize.signal=FALSE)
  trading.rules <- list(rules.func="fillOrders4.1.alphaModel", rules.params=c(threshold,0))
  model <- update.alphaModel(model=model.test,
                             func.signal=func.signal,
                             trading.rules=trading.rules,
                             ts.prices=os.priceLevels,
                             ts.bidoffers=os.bidoffers,
                             ts.ancillary=os.ancillary)
  model <- recalc.alphaModel(model) 
  
 
  
  val.date <- index(ts.priceLevels[ep[e.p],])
  last.1M <- index(ts.priceLevels[(ep[e.p-1]+1):ep[e.p]])
  rolling.signal[last.1M,]  <<- model$signals[last.1M,]
  out.sample.xts[val.date,] <<- c(e.p,sum(model$pnls[last.1M,]),threshold,width,window)
  
})

#### end of apply function ######
plot(cumsum(na.omit(out.sample.xts$PNL.max))*50)
sum(na.omit(out.sample.xts$PNL.max)*50) ###not very encouraging


###### helper function for lm ###########

L <- function(x, k = 1) {
  if(length(k) > 1) {
    rval <- do.call("cbind", lapply(k, lag.xts, x = x))
    colnames(rval) <- k
  } else {
    rval <- lag(x, k)
  }
  rval
}

#########################################


##### begin linear approximation of the out of sample threshold #####
threshold.diff <- diff(in.sample.xts$threshold.mode)
threshold.diff <- cbind(threshold.diff,threshold.diff*NA,threshold.diff*NA)
#### attempt at forecasting the next n-period thresholds with lags of the prior #####
frm.lag <- formula('lag.xts(threshold.mode,-1)~L(threshold.mode,0:8)')


#looks like the thresholds in new.test are antipersistant with 8 significant lags
#regression width
sample.period  <- 100
nDays <- 1:nrow(threshold.diff)
daysTo <- nrow(threshold.diff) - 1
daysFrom <- sample.period + 1
oput  <- threshold.diff *NA
ts.daily <- diff(ts.ancillary[ep,1])
names(ts.daily) <- 'ES.PROXY'

###This creates a normal distribution of changes, teh changes between itterations aren't normal examine (hist(threshold.mode))
for(i in daysFrom:daysTo){
  #i=723
  indicesFit <- (nDays[i-sample.period]+1):nDays[i]
  indicesFitPlusOne <- (nDays[i - sample.period] + 1) : (nDays[i] + 1)
  
  in.sample.threshold <- cbind(threshold.diff[indicesFit,],ts.daily[indicesFit,])
  in.sample.threshold.plus  <- cbind(threshold.diff[indicesFitPlusOne,],ts.daily[indicesFitPlusOne,])
  
  mdl.reg <- lm(frm.lag,data=in.sample.threshold)
  mdl.pred <- last(predict(mdl.reg,in.sample.threshold.plus))
  threshold.diff[last(indicesFitPlusOne),2] <- mdl.pred
  threshold.diff[last(indicesFitPlusOne),3] <- summary(mdl.reg)$adj.r.squared
  print(paste('Running Regression for ',index(threshold.diff[last(indicesFitPlusOne),3])))
}
threshold.diff[last(indicesFitPlusOne)]


threshold.pred <- threshold.diff[,2]+lag(new.test$threshold.mode,1)
threshold.resid.2 <- threshold.pred - new.test$threshold.mode
names(threshold.resid) <- 'resid'
###### end of linear prediction ... this doenst' work that well #####




sample.period.sim  <- 10
thresholds.sim  <- do.call('cbind',rep(list(in.sample.xts$threshold.mode*NA),3))
thresholds.sim[,1] <- in.sample.xts$threshold.mode
##### simulate a distribution of the prior and choose a threshold that way, assuming distribution is roughly the same #####
for(i in daysTo:daysFrom){
  #i=755
  indicesFit <- (nDays[i-sample.period.sim]+1):nDays[i]
  indicesFitPlusOne <- (nDays[i - sample.period.sim] + 1) : (nDays[i] + 1)
  
  draws <- sample(in.sample.xts$threshold.mode[indicesFit],1000,replace=TRUE)

  try(thresholds.sim[last(indicesFitPlusOne),2] <- mfv(draws))
  thresholds.sim[last(indicesFitPlusOne),3] <- thresholds.sim[last(indicesFitPlusOne),1]-thresholds.sim[last(indicesFitPlusOne),2]
  print(paste('Running mfv for ',index(threshold.diff[last(indicesFitPlusOne),3])))
}
#### end of sampling based on sampling the changes ... not very encouraging ######
?
### break the thresholds out by probablity assuming the correct distribution of the prior will carry forward ####
t.q.rank <- lapply(t.q.profiles,
                   function(x){
                     if(!is.null(x)){
                       try(rank(x[,1]))
                     } 
                     else{
                       NULL
                     }
                   })
t.q.prob <- lapply(t.q.rank,function(x) x/sum(x))
t.q.threshold <- lapply(t.q.profiles,function(x) x[,2])

sim.threshold   <- rep(NA, length(t.q.threshold))
for( i in 4:length(t.q.threshold)){
  #take the highest ranked theshold from each of the two priors 
  if(is.null(t.q.threshold[[i-2]])||is.null(t.q.threshold[[i-3]])) {
    sim.threshold[i]  <- NA
  }else{
    #take a sample of the two priors and weight them .75/.25
    print(paste('Running: ',i,'of :',length(t.q.threshold) ))
    tst <- sample(c(t.q.threshold[[i-3]],t.q.threshold[[i-2]])
                  ,10000
                  ,replace=TRUE,
                  prob=c(t.q.prob[[i-3]]*.25,
                         t.q.prob[[i-2]]*.75))
    sim.threshold[i] <- mfv(tst)[1]  
    
  }
}


#### check the hit rate to see if including longer history matters, a success is if the simulated threshold is in the 
#### posterior distribution of thresholds.


in.dist <- unlist(lapply(1:length(sim.threshold),
                         function(x){
                           tst <- which(t.q.threshold[[x]]%in%sim.threshold[x])
                           ifelse(length(tst)!=0,tst,NA)
                         } ))

sum(!is.na(in.dist))/length(in.dist)
#create data and proxies for the model
ts.eq.proxy <- 100+4*(500-5*ts.prices.ighyes[,'IG.MEDIAN'])/100
ts.eq.proxy <- 1.2*ts.eq.proxy[,1]-0.2*ts.prices.ighyes[,'HY.MEDIAN']

ts.diff <- diff(ts.eq.proxy[,1])
ts.diff[1,] <- 0.0
ts.prices <- cumsum(ts.diff)
colnames(ts.prices) <- 'ES.PROXY'
ts.bidoffers <- xts(rep(0.25, nrow(ts.prices)), order.by=index(ts.prices))
names(ts.bidoffers) <- 'BID.OFFERS'
ts.ancillary <- cbind(ts.prices, ts.bidoffers)

ts.diff <- diff(ts.prices.ighyes[,'ES.EQMID'])
ts.diff[1,] <- 0.0
ts.priceLevels <- cumsum(ts.diff)


#calculate price samples
width <- 3
period <- 'weeks'


ep <- endpoints(ts.priceLevels,on=period)
iTo <- length(ep) - 1
iFrom <- width + 1


#this is an xts of our outputs
in.sample.xts <- NULL           
#find the max pnl for each three month sample

#create a cluster, this is invoking 4 local sockets. 
cl <- makeCluster(clusterNodes, type = "SOCK")
#load environment on each cluster
clusterEvalQ(cl, source('C:/Devel/Models/trunk/R/alphaLib/alphaModel.R'))
clusterEvalQ(cl, source('C:/Devel/Models/trunk/R/alphaLib/utilLib.R'))
clusterEvalQ(cl, source('C:/Devel/Models/trunk/R/JR/sgfilter.R'))
clusterEvalQ(cl, library(xts))
clusterEvalQ(cl, library(quantmod))
clusterEvalQ(cl, library(PerformanceAnalytics))


##### beginning of massive for loop ########
for(i in iFrom:iTo){
  #get the index of the insample data
  indicesFit <- (ep[i-width]+1):ep[i]
  is.priceLevels <- ts.priceLevels[indicesFit,]
  is.ancillary <- ts.ancillary[indicesFit,]
  is.bidoffers <- ts.bidoffers[indicesFit,]
  #create an IS model
  model.test <- alphaModel("In Sample Tester")
  func.signal <- list(signal.func="signals.ancillary1.1.alphaModel",
                      filter.func="filtSavGol",
                      filter.params=c(2,0,0,0),  #set these to zero for sanity check, should bomb if it isn't updated
                      normalize.returns=FALSE,
                      normalize.signal=FALSE)
  trading.rules <- list(rules.func="fillOrders4.1.alphaModel", rules.params=c(0,0))
  model.test <- update.alphaModel(model=model.test,
                                  func.signal=func.signal,
                                  trading.rules=trading.rules,
                                  ts.prices=is.priceLevels,
                                  ts.bidoffers=is.bidoffers,
                                  ts.ancillary=is.ancillary)
  
  
  #create data.frame of possible parameters
  combs <- expand.grid(threshold=seq(.4, 1.6, by= .02),                     
                       width=10:25,
                       window=10:15)
  
  
  #itterate over the parameter space to find best combination of parameters
  print(paste('Running for three months ending:',index(last(is.priceLevels)),"Current Time:",Sys.time()))
  system.time(profiles <- parApply(cl, combs, 1, function(comb,model,price.history,ep,i) {
    
    trading.rules <- list(rules.params=c(comb[['threshold']], model$rules.list$rules.params[2]))
    func.signal <- list(filter.params=c(model$signal.list$filter.params[1],comb[['width']],
                                        comb[['window']],
                                        comb[['window']]))
    model <- update.alphaModel(model=model,
                               func.signal=func.signal,
                               trading.rules=trading.rules)
    model <- recalc.alphaModel(model)
    last.1M <- index(price.history[(ep[i-1]):ep[i]])
    list(threshold=comb[['threshold']],
         width=comb[['width']],
         window=comb[['window']],
         pnl=sum(model$pnls[last.1M]))
  },model.test,ts.priceLevels,ep,i))
  
  #get the min and max profiles and add them to model listv here
  #browser()
  pnls <- unlist(lapply(profiles, function(x) x$pnl))
  t.quantile <- quantile(pnls,.8)
  med.pnl <- median(pnls[pnls>=t.quantile])
  max.pnl <- max(pnls)
  med.location <- which(pnls %in% med.pnl)
  max.location <- which(pnls %in% max.pnl)
  
  med.profile <- profiles[[med.location[1]]]
  max.profile <- profiles[[max.location[1]]]
  
  #check if your trials have been initialized. 
  if(is.null(in.sample.xts)){
    #add two columns for EP and PNL
    in.sample.xts <- xts(matrix(rep(NA,(length(names(combs))+2)*(length(ep)-1)),
                                ncol=(length(combs)*2)+3,
                                nrow=length(ep)-1),
                         order.by=index(ts.priceLevels[tail(ep,-1)]))
    names(in.sample.xts) <- c('EP','PNL.max',paste(names(combs),'.max',sep=''),'PNL.med',paste(names(combs),'.med',sep=''))
  }
  
  in.sample.xts[i-1,] <- c(i,max.profile$pnl,unlist(max.profile[names(combs)]),med.profile$pnl,unlist(med.profile[names(combs)]))
}

##### end  of massive for loop ########                                                    


#### stop the cluster #####
stopCluster(cl)



###calculate the out of sample one month period####
out.sample.xts <- in.sample.xts*NA
#### beginning of apply function ######
#### this assumes assumes a 1 month lag before we recalibrate #####
blah <- apply(na.omit(lag(in.sample.xts)),1,function(x) {
  e.p <- x[['EP']] + 1 #get one period forward and calculate 
  b.p <- e.p - 3
  indicesFit <- ep[b.p]:ep[e.p]
  
  os.priceLevels <- ts.priceLevels[indicesFit,]
  os.ancillary <- ts.ancillary[indicesFit,]
  os.bidoffers <- ts.bidoffers[indicesFit,]
  
  width <- x[['width']]
  window <- x[['window']]
  threshold <- x[['threshold']]
  
  model <- alphaModel("Out of  Sample Tester")
  func.signal <- list(signal.func="signals.ancillary1.1.alphaModel",
                      filter.func="filtSavGol",
                      filter.params=c(2,width,window,window),
                      normalize.returns=FALSE,
                      normalize.signal=FALSE)
  trading.rules <- list(rules.func="fillOrders4.1.alphaModel", rules.params=c(threshold,0))
  model <- update.alphaModel(model=model.test,
                             func.signal=func.signal,
                             trading.rules=trading.rules,
                             ts.prices=os.priceLevels,
                             ts.bidoffers=os.bidoffers,
                             ts.ancillary=os.ancillary)
  model <- recalc.alphaModel(model) 
  val.date <- index(ts.priceLevels[ep[e.p],])
  last.1M <- index(ts.priceLevels[(ep[e.p-1]+1):ep[e.p]])
  out.sample.xts[val.date,] <<- c(e.p,sum(model$pnls[last.1M,]),threshold,width,window)
  
})

#### end of apply function ######
