library(PortfolioAnalytics) #install from S:\Software\Source\R\R Packages\PortfolioAnalytics_0.8.2.zip
require(xts)
require(DEoptim) #required for PortfolioAnalytics
source('C:/Devel/src/syscredit/Models/trunk/R/alphaLib/defaults.R')
require(TTR)
require(Rglpk)
require(tseries)
setwd("C:/Devel/Models/trunk/R/GL")
#for establishing cohorts
agg.min  <- 1
agg.max <- 5


# Load data
load(file="S:/Data/R_Data/data_ig_top_daily.RData")
# Load symbols
file.symbols <- "S:/Data/R_Data/TOPNALTLILLBQD_Symbols.csv"
symbols.liquid <- read.table(file.symbols, header=TRUE, sep=",", as.is=TRUE)$Ticker

# Load symbols
str.headers <- paste(symbols.liquid,'MEDIAN',sep=".")
ts.prices <- ts.prices[,str.headers]
colnames(ts.prices) <- sub('.MEDIAN','',colnames(ts.prices))
ts.rets <- diff(ts.prices)
ts.rets[1,] <- 0.0

# Calculate variance ratios for a list of symbols
var.ratios <- sapply(colnames(ts.rets), function(colname) var.ratio(ts.ret=ts.rets["/2012-04-01",colname], agg.min, agg.max) )
# Sort the symbols by highest variance ratios
var.ratios <- var.ratios[order(var.ratios)]
# Select the top symbols with highest var.ratios
length.ratios <- length(var.ratios)
var.ratios.top <- var.ratios[trunc(0.75*length.ratios):length.ratios]
# Select prices for the top symbols
ts.prices.top <- ts.prices[,names(var.ratios.top)]
# Remove RESCAP from top symbols
ts.prices.top <- ts.prices.top[,colnames(ts.prices.top)!='RESCAP']

# Run CDS momentum strategy
func.signal <- list(filter.func="filtSavGol", filter.params=c(2, 3, 0, 1))
portfolio.trades <- adaptivePortfolioTrader(ts.prices=ts.prices.top["2012-04-01/",], func.signal=func.signal, cut.off=5)
portfolio.trades$sums
chart_Series(cumsum(portfolio.trades$time.series[,'Total.pnl']), name="Total PnL")

################ first attempt at Butler paper ############################

head(ts.prices.top)
### convert ts.prices.top to all upfront par prices #### 
ts.prices.temp <- lapply(ts.prices.top, function(x) 1e7 - (x * (1-exp(-x*5e-4) )/(x*1e-7)))
ts.prices.alluf <- do.call('cbind',ts.prices.temp)

### convert to daily continuous return space in SELL protection terms #######################
ts.ret.top <- log(ts.prices.alluf/lag(ts.prices.alluf))
ts.ret.top[1,] <- 0.0
charts.RollingPerformance(ts.ret.top[,-2], width=30, main="", colorset=rainbow8equal, legend.loc="topleft")
chart.CumReturns(ts.ret.top[,-2], main = "", xaxis = FALSE, legend.loc = "topleft", ylab = "Cumulative Return", ylog=TRUE, wealth.index=TRUE, cex.legend=.7, cex.axis=.6, cex.lab=.7)


####anaylyze some simple corr graphs
require("corrplot")
col3 <- colorRampPalette(c("darkgreen", "white", "darkred"))
M <- cor(ts.ret.top)
colnames(M) = rownames(M) = colnames(ts.ret.top)
order.hc2 <- corrMatOrder(M, order="hclust", hclust.method="complete")
M.hc2 <- M[order.hc2,order.hc2]
corrplot(M.hc2, tl.col="black", tl.cex=0.8, method="square", col=col3(8), cl.offset=.75, cl.cex=.7, cl.align.text="l", cl.ratio=.25)
corrRect.hclust(M.hc2, k=3, method="complete", col="blue")
dev.off()

### analyze the rolling 30 day volatility,ret,correlation #####
look.back  <- 30 #days
ts.vol.lb.top <-  do.call('cbind',lapply(ts.ret.top, runSD ,n=look.back))
ts.ret.lb.top <-  do.call('cbind',lapply(ts.ret.top, runSum ,n=look.back))

#calculate rolling 30 day correlations for optim, should parallelize this (will do in future, takes 10mins for 1yr)
ts.corr.lb.top  <- list()
ts.covar.lb.top  <- list()
system.time(for(i in 1:nrow(ts.ret.top)){
  if(i<look.back+1){
    ts.corr.lb.top[i] <- NULL
  }else{
    start <- i - look.back
    end  <- i
    cr <- table.Correlation(ts.ret.top[start:end,],ts.ret.top[start:end,])
    cr  <- matrix(cr$Correlation,nrow=ncol(ts.ret.top),ncol=ncol(ts.ret.top))
    sdev <- sd(ts.ret.top[start:end,])
    covar  <- MBESS::cor2cov(cr,sdev)
    ts.corr.lb.top[i] <- list(cr)
    ts.covar.lb.top[i] <- list(covar)
  }
})

#get number of assets, number of obs
num.assets  <- ncol(ts.ret.top)
num.obs  <- nrow(ts.ret.top)


#Exhibit 1 Equal Weight
weights = xts(matrix(rep(1/num.assets,num.obs*num.assets), ncol=num.assets), order.by=index(ts.ret.top))
colnames(weights) <- colnames(ts.ret.top)
EqWgt = Return.rebalancing(ts.ret.top,weights)
plot(cumsum(EqWgt),type='l')
dd.ew <- table.Drawdowns(EqWgt)
sharpe.ew  <- SharpeRatio(EqWgt,Rf=0) * (250^.5)
sortino.ew  <- SortinoRatio(EqWgt,MAR=0)
#ES equals Expected Shortfall. See help ?ES
charts.PerformanceSummary(EqWgt,Rf=0, methods=c("ModifiedVaR", "ModifiedES"),colorset=bluefocus)

#Exhbit 2 volatilty rebalancing, rebalance daily (not realistic but in here for sake of illustration)
#each asset contributes 1/n*.006/[observed vol over look.back],< 1/n (average period vol ~ 60bps)
#60 bp vol over the lookback period
vol.obj <- .006
weights.vol.rebal <- xts(t(apply(ts.vol.lb.top,1,
                                 function(x) {                                   
                                   if(is.na(x)){
                                     x
                                   }else{                                     
                                     ob <- (vol.obj/x)*(1/num.assets)
                                     ob[ob>(1/num.assets)] <- 1/num.assets
                                     ob
                                   }
                                 })),order.by=index(ts.vol.lb.top))
weights.vol.rebal[1,] <- 0
weights.vol.rebal <- na.locf(weights.vol.rebal)
VolWgt  <- Return.rebalancing(ts.ret.top,weights.vol.rebal) #calculate the returns after the formation period
dd.vw <- table.Drawdowns(VolWgt)
sharpe.ew <- SharpeRatio(VolWgt) * (250^.5) #actually looks worse than equal weighting, sharpe roughly halves
charts.PerformanceSummary(VolWgt,Rf=0, colorset=bluefocus)


####Exhibit 3 
####Hold the top five momentum assets based on the rolling 30day return, rebalance daily
weights.momo <- xts(t(apply(ts.ret.lb.top,1,
                                 function(x) {                                   
                                   if(is.na(x)){
                                     x
                                   }else{                                     
                                     ob <- rank(-coredata(x) , ties.method = 'first')
                                     ob[ob<=5]  <- .2                                     
                                     ob[ob!=.2]  <- 0
                                     ob
                                   }
                                 })),order.by=index(ts.vol.lb.top))
weights.momo[,1] <- 0
weights.momo  <- na.locf(weights.momo)
colnames(weights.momo) <- colnames(ts.ret.lb.top)
MoMoWgt <- Return.rebalancing(ts.ret.top,weights.momo) #calculate the returns after the formation period
dd.momo <- table.Drawdowns(MoMoWgt)
sharpe.momo <- SharpeRatio(MoMoWgt) * (250^.5)
charts.PerformanceSummary(MoMoWgt,Rf=0, colorset=bluefocus)

###Exhibit 4
### volatility weight the MoMo Portfolio
weights.momo.vw <- weights.momo
for(i in look.back:nrow(weights.momo)){
  prd.weights  <- weights.momo[i,]
  if(!is.na(prd.weights)){
    prd.date  <- index(prd.weights)
    prd.hld  <- names(prd.weights[,prd.weights!=0])
    prd.vol  <- ts.vol.lb.top[prd.date,prd.hld]
    #assume same 60bp vol tollerance 
    vol.weight  <- vol.obj/prd.vol*(1/length(prd.hld))
    vol.weight[vol.weight>(1/length(prd.hld))] <- 1/length(prd.hld) 
    weights.momo.vw[prd.date,prd.hld] <- vol.weight
    
  }
}


weights.momo.vw[1,] <- 0
weights.momo.vw  <- na.locf(weights.momo.vw)
colnames(weights.momo.vw) <- colnames(ts.ret.lb.top)
MoVwWgt <- Return.rebalancing(ts.ret.top,weights.momo.vw) #calculate the returns after the formation period
dd.momo.vw <- table.Drawdowns(MoVwWgt)
sharpe.momo.vw <- SharpeRatio(MoVwWgt)*(250^.5)
charts.PerformanceSummary(MoVwWgt,Rf=0, colorset=rainbow12equal) #slightly better results

###Exhibit 5
###CVaR optimization, assumes look.back == 30 and no friction, assumes a .01 shortfall
###I chose CvarOpt rather than mean/variance, as teh mean variance optimizaiton on this particular portfolio blew up on corner solutions about 1/3rd of the time
cvarOpt <- function(rmat, alpha=0.01, rmin=0, wmin=-1, wmax=1, weight.sum=1)
{
  require(Rglpk)
  n = ncol(rmat) # number of assets
  s = nrow(rmat) # number of scenarios i.e. periods
  averet = colMeans(rmat)
  # creat objective vector, constraint matrix, constraint rhs
  Amat = rbind(cbind(rbind(1,averet),matrix(data=0,nrow=2,ncol=s+1)),
               cbind(rmat,diag(s),1))
  objL = c(rep(0,n), rep(-1/(alpha*s), s), -1)
  bvec = c(weight.sum,rmin,rep(0,s))
  # direction vector
  dir.vec = c("==",">=",rep(">=",s))
  # bounds on weights
  bounds = list(lower = list(ind = 1:n, val = rep(wmin,n)),
                upper = list(ind = 1:n, val = rep(wmax,n)))
  res = Rglpk_solve_LP(obj=objL, mat=Amat, dir=dir.vec, rhs=bvec,
                       types=rep("C",length(objL)), max=T, bounds=bounds)
  w = as.numeric(res$solution[1:n])
  return(list(w=w,status=res$status))
}


cvar.weights <- weights.momo.vw *0

for(i in look.back:nrow(cvar.weights)){
  #i=50
  date <- index(ts.ret.top[i,])
  start=i-look.back+1
  rts <- ts.ret.top[start:i,]
  #shortfall == 1% for every thirty day optimization
  #portfolio has 0 net notional position, max and min weights are 1 and -1 respectively
  ob <- cvarOpt(coredata(rts),alpha=.03,wmin=-1,wmax=1,weight.sum=0)
  cvar.weights[date,] <- ob$w
}

CvarWgt <- Return.rebalancing(ts.ret.top,cvar.weights) #calculate the returns after the formation period
dd.momo.cv <- table.Drawdowns(CvarWgt)
sharpe.momo.cv <- SharpeRatio(CvarWgt) * (250^.5)
charts.PerformanceSummary(CvarWgt,Rf=0, colorset=rainbow12equal) #very good results, rebalancing not totally feasible


