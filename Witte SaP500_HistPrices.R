# Jan Hendrik Witte
# me@jhwitte.com
#
# file source:
# http://www.bellmanoptimality.com/programming/
#
# 15 January 2015
#
# Orinigally based on: http://grollchristian.wordpress.com/2013/08/07/sp500-stock-price-data/
# 
# Reads historic prices for all constituents of SP500.
# Performs various ranking and plotting functions.
#

library(zoo)
library(tseries)                        

## read in list of constituents, with company name in first column and
## ticker symbol in second column
## From here: https://www.dropbox.com/s/vei2cbxy0kfulqv/sp500_constituents.csv
# spComp <- read.csv("sp500_constituents.csv" )

## download the data
# snp <- donload_SnP(spComp)
  
## save the data
# write.zoo(snp, file = "price_data.csv", index.name = "time")

# load the data from disk
snp = read.zoo(file = "price_data.csv", header=TRUE, format="%Y-%m-%d") 

# we should now have two files, spComp with company names and ticker symbols,
# and snp with historic data for different tickers

# first, while Christian's original script uses zoo, lets move to xts here
# and generate daily returns
library(xts)
snp <- returns(as.xts(snp))

# removing tickers and going back to orignal names
names(snp) <- c("SaP500",levels(spComp[,1])[which(names(snp) %in% spComp[,2])])

# generating cumulative returns for each stocks, all starting at zero
snp_cumreturns <- cumreturns(snp)

library(ggplot2)
# now basic plotting of S&P500 constituents works, and the plots look not reasonable.
# examples: uncomment following line for basic plot of "Anadarko Petroleum Corp"

# plot(snp_cumreturns[,35]) 

# now we want to plot the best performing stocks in a given time period
# and save the plots as *.jpg files so we can use them later

# top 10 in the last 10 years
top10 <- winners(snp,"2005/",10)
plot(top10,"Top 10 in the Last 10 Years")
ggsave("top10.png")

# top 20 in the last 20 years
top20 <- winners(snp,"1995/",20)
plot(top20,"Top 20 in the Last 20 Years")
ggsave("top20.png")

# top 30 in the last 30 years
top30 <- winners(snp,"1985/",30)
plot(top30,"Top 30 in the Last 30 Years")
ggsave("top30.png")

# top 10 in the last 50 years
top1050 <- winners(snp,"1965/",10)
plot(top1050,"Top 10 in the Last 50 Years")
ggsave("top1050.png")


#----------------------------------------------------------------------
# only subfunctions after this point
#----------------------------------------------------------------------

# find the top n stocks in a given period, input returns only
winners <- function(x, period, n) {
  z <- cumreturns(x[period,])
  return(z[,1:dim(z)[2] %in% order(z[dim(z)[1],],decreasing=TRUE)[1:n] ])
}

# generating cumulative returns for xts object, starting at zero on day one
cumreturns <- function(x) {
  x[1,] <- 0
  z     <- na.fill(x,0)
  z     <- do.call(cbind,lapply(1:dim(z)[2],function(k) cumprod(1+z[,k])))-1
  return(z)
}

# generating arithmetic returns for xts object
returns <- function(x) {
  z <- (x-lag(x,1))/lag(x,1)
  return(z)
}

# quick function for plotting
plot <- function(x,title="S&P500 Components") {
  x <- fortify(x, melt=TRUE)
  p <- ggplot(data=x, aes(x=Index, y=Value, colour=Series)) +
       geom_line() + ggtitle(title)
  print(p)
}

# download S&P500 data from the web
donload_SnP <-function(spComp) {
   
  # extract symbols and number of iterations
  symbols = spComp$Symbol  
  nAss    = length(symbols)
  
  # specify time period
  dateStart = "1950-01-03"               
  dateEnd = "2015-01-15"
  
  # get the SP500
  sp.zoo = get.hist.quote(instrument = "^gspc", 
                          start = dateStart,
                          end = dateEnd, 
                          quote = "AdjClose",
                          retclass = "zoo", 
                          quiet = FALSE)
  
  # use ticker symbol as the column names 
  dimnames(sp.zoo)[[2]] = as.character("SNP")
  
  snp.assets.adjclose.zoo = sp.zoo                      
  
  # download all the assets 
  for (i in 1:nAss) {
    # display progress by showing the current iteration step
    cat("Downloading ", i, " out of ", nAss , "\n")
    
    result = try({x = get.hist.quote(instrument = symbols[i],
                                     start = dateStart,
                                     end = dateEnd, 
                                     quote = "AdjClose",
                                     retclass = "zoo")})
    
    if(class(result) == "try-error") {
      next
    } else {
      dimnames(x)[[2]] = as.character(symbols[i])
      # merge with already downloaded data to get assets on same dates 
      snp.assets.adjclose.zoo = merge(snp.assets.adjclose.zoo, x)                      
    }
  }
  return(snp.assets.adjclose.zoo)
}
