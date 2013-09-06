suppressMessages(library(knitr))
suppressMessages(library(markdown))
suppressMessages(library(getopt))
suppressMessages(library(xts))
suppressMessages(library(PerformanceAnalytics))
suppressMessages(library(ROneTick))
suppressMessages(library(foreach))
suppressMessages(library(quantmod))
suppressMessages(library(car))
suppressMessages(library(urca))
suppressMessages(library(compiler))
enableJIT(3)
source('PortfolioGraphBase.R')

#symbols.naig <- read.csv('//sharedir2/syscredit/Data/R_Data/TOPNAIG_Symbols.csv', stringsAsFactors=FALSE)
#symbols.itx <- read.csv('//sharedir2/syscredit/Data/R_Data/ITX_SOV_Symbols.csv', stringsAsFactors=FALSE)
#symbols.cdx <- read.csv('//sharedir2/syscredit/Data/R_Data/CDX_SOV_Symbols.csv', stringsAsFactors=FALSE)
symbols.port <- read.csv('//sharedir2/syscredit/Data/R_Data/TOPNAIG_Symbols_ordered.csv', stringsAsFactors=FALSE)

end.date <- Sys.Date()
start.date <- end.date-60

part1 <- load1030Bucket(symbols.port, start.date, end.date)
part2 <- load1330Bucket(symbols.port, start.date, end.date)  
part3 <- load1630Bucket(symbols.port, start.date, end.date)

final.xts <- rbind(part1, part2, part3)
final.xts  <- na.locf(final.xts)

# Scrub the data
ts.prices <- xts(apply(final.xts, 2, function(ts.tmp)
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
), order.by=index(final.xts))

# Calculate percentage returns
look.back <- 2
ts.rets <- diff(ts.prices,lag=1)
ts.rets[1,] <- ts.rets[2,]
ts.lrets <- diff(log(ts.prices),lag=look.back)
ts.lrets[1:look.back,] <- ts.lrets[look.back+1,]
sd.lrets <- sqrt(apply(ts.lrets, 2, var))

### Screen time series for recent trends
row.num <- nrow(ts.lrets)
norm.lrets <- as.vector(ts.lrets[row.num,])/sd.lrets
symbols.top <- colnames(ts.lrets)[order(abs(norm.lrets), decreasing=TRUE)]

sectors <- unique(symbols.port$Portfolio)


spec = matrix(c(
  'output' , 'o', 1, "character"
), byrow=TRUE, ncol=4);
opt = getopt(spec);


outputDir <- '.'
if(!is.null(opt$output)){
  outputDir <<- opt$output
}

#################### Graphic takes place here
html <- paste(outputDir,'/portfolio.html',sep='')
knit2html(input="PortfolioGraph.Rmd",output=html)
