# Jan Hendrik Witte
# me@jhwitte.com
# file source:
# http://www.bellmanoptimality.com/programming/
#
# 25 January 2015
# 
# Reads historic prices for all constituents of FTSE100, DAX, and DJIA.
# Ranks by performance over 1st specified time period.
# Plots over 2nd specified time period.
#

library(tseries)
library(xts)
library(ggplot2)

#----------------------------------------------------------------------
# only subfunctions
#----------------------------------------------------------------------

# find the top n stocks in a given period, input returns only
winners <- function(x, period="/", n=1) {
      z <- cumreturns(x[period,])
  return(1:dim(z)[2] %in% order(z[dim(z)[1],],decreasing=TRUE)[1:n])
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

# quick function to prepare plotting
plot <- function(x,title="Index Components") {
  x <- fortify(x, melt=TRUE)
  p <- ggplot(data=x, aes(x=Index, y=Value, colour=Series)) +
       geom_line() + ggtitle(title)
  return(p)
}

# download data from the web
get.stock <- function(tick,name, begin.date = "1950-01-01") {
  z <- xts(get.hist.quote(instrument = tick,
                          start = begin.date,
                          end = Sys.Date(), 
                          quote = "AdjClose"))
  names(z) <- name
  return(z)
}

# multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#----------------------------------------------------------------------
# main code from here
#----------------------------------------------------------------------

# starting date from which to get data
FROM = "2015-01-01"

# load DAX
DAX <- rbind(read.csv(file = "DAX.csv", header=TRUE, stringsAsFactors = FALSE),c("^GDAXI","DAX Index")) 
dax.raw <- returns( do.call("cbind",lapply(1:dim(DAX)[1],function(k) get.stock(DAX[k,1],DAX[k,2], begin.date
                                                                               = FROM))) )
dax.raw[dax.raw>0.5]    <- NA # crude cleansing of the data
dax.raw[dax.raw<(-0.5)] <- NA # crude cleansing of the data

# load FTSE
FTSE <- rbind(read.csv(file = "FTSE.csv", header=TRUE, stringsAsFactors = FALSE),c("^FTSE","FTSE Index"))
ftse.raw <-returns( do.call("cbind",lapply(1:dim(FTSE)[1],function(k) get.stock(FTSE[k,1],FTSE[k,2], begin.date
                                                                                = FROM))) )
ftse.raw[ftse.raw>0.5]    <- NA # crude cleansing of the data
ftse.raw[ftse.raw<(-0.5)] <- NA # crude cleansing of the data

# load DJIA
DJIA     <- rbind(read.csv(file = "DJIA.csv", header=TRUE, stringsAsFactors = FALSE),c("^GSPC","DJIA Index"))
djia.raw <-returns( do.call("cbind",lapply(1:dim(DJIA)[1],function(k) get.stock(DJIA[k,1],DJIA[k,2], begin.date
                                                                                = FROM))) )
djia.raw[djia.raw>0.5]    <- NA # crude cleansing of the data
djia.raw[djia.raw<(-0.5)] <- NA # crude cleansing of the data

# subperiod over which to capture the biggest movers
MOVE = "2015-01-27/28"

# identify strongest perfomers over MOVE period
dax.top.idx  <- winners(dax.raw,period=MOVE,n=5)
ftse.top.idx <- winners(ftse.raw,period=MOVE,n=5)
djia.top.idx  <- winners(djia.raw,period=MOVE,n=5)

# make sure full indices are plotted as well
dax.top.idx[dim(dax.raw)[2]]   <- TRUE
ftse.top.idx[dim(ftse.raw)[2]] <- TRUE
djia.top.idx[dim(djia.raw)[2]] <- TRUE

# plot the n best performers from FTSE
p1 <- plot(cbind(cumreturns(ftse.raw[,ftse.top.idx])), title=paste(Sys.Date(),"-- FTSE: Today's Top 5 & Index"))
# plot the n best performers from DAX
p2 <- plot(cumreturns(dax.raw[,dax.top.idx]), title=paste(Sys.Date(),"-- DAX: Today's Top 5 & Index"))
# plot the n best performers from DJIA
p3 <- plot(cumreturns(djia.raw[,djia.top.idx]), title=paste(Sys.Date(),"-- DJIA: Today's Top 5 & Index"))

multiplot(p1,p2,p3,cols=1)
ggsave("IdxTop.png")