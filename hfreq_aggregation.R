################################################
###  file contains scripts for high frequency data tasks:
###
###  1. estimating variance, skewness, and kurtosis,
###
###  2. running simple trading strategies,
###
###  3. plotting using chart_Series, 
###
################################################


# plotting
chart_Series(get(symbol)[rangev], name=symbol)
chart_Series(volat, name=paste(symbol, "vol"))
# plotting skew with custom y-axis range
# extract and modify plot object to reduce y-axis range
chobj <- chart_Series(x=skew, plot=FALSE)
# extract and modify ylim using accessor and setter functions
ylim <- chobj$get_ylim()
ylim[[2]] <- structure(c(-1, 1), fixed=TRUE)
chobj$set_ylim(ylim)
# render the plot
plot(chobj)

chart_xts(skew)
chart_xts(skew, ylim=c(-1, 1))


### simple trading strategy for daily aggs

returns <- get(symbol)[index(skew), 4]
returns <- diff(log(returns))
colnames(returns) <- paste(symbol, "Ret", sep=".")

# correlation
blah <- na.omit(merge(skew, returns))
colnames(blah) <- c(colnames(blah)[1], "SPY.Ret")
# scatterplot of skew and returns
plot(coredata(blah["2008-09/2009-05"]))
cor(coredata(blah["2008-09/2009-05"]))
cor.test(coredata(blah["2008-09/2009-05"])[, 1], coredata(blah["2008-09/2009-05"])[, 2])


# run strategy out-of-sample
blah <- -sign(lag(skew))
colnames(blah) <- paste(symbol, "Posit", sep=".")
blah <- na.omit(merge(blah, returns))
# scatterplot of skew and returns
plot(coredata(blah["2008-09/2009-05"]))
cor(coredata(blah["2008-09/2009-05"]))
cor.test(coredata(blah["2008-09/2009-05"])[, 1], coredata(blah["2008-09/2009-05"])[, 2])
blah <- cumsum(blah[, 1]*blah[, 2])
plot(blah)



### simple trading strategy for rolling minute aggs

# signal threshold trading level
threshold <- 0.5
# score <- sign(skew-threshold)
score <- NA*numeric(nrow(skew))
score <- ifelse(skew>threshold, -1, score)
score <- ifelse(skew<(-threshold), 1, score)
score <- ifelse((skew*lag(skew))<0, 0, score)
# score <- xts(x=score, order.by=index(skew))
# lag the signal by one
# score <- lag(score)
score <- c(0, score)
score <- score[-length(score)]
score <- na.locf(score)
score <- merge(skew, score)
colnames(score)[2] <- "positions"

# number of bars in long and short positions
sum(score[, 2]>0)
sum(score[, 2]<0)

chart_Series(score["2013-10-12/2013-11-13"],
             name=paste(symbol, "skew"))

# chart_xts(score["2013-10-12/2013-11-13"])

# plotting
rangev <- "2013-10-12/2013-11-13"
# extract and modify plot object to reduce y-axis range
chobj <- chart_Series(x=score[rangev, 1], 
                      name=paste(colnames(score[rangev, 1]), "/", date()), 
                      plot=FALSE)
# extract and modify ylim using accessor and setter functions
ylim <- chobj$get_ylim()
ylim[[2]] <- structure(c(-1, 1), fixed=TRUE)
chobj$set_ylim(ylim)
# render the plot
plot(chobj)
add_TA(score[rangev, 2]>0, on=-1, col="lightgreen", border=NA)
add_TA(score[rangev, 2]<0, on=-1, col="lightpink", border=NA)
add_TA(score[rangev, 2]==0, on=-1, col="lightgrey", border=NA)

# cumulative PnL
blah <- cumsum(score[, 2]*returns[, 1])
plot(blah, format.labels="%Y-%m")

# number of position flips
blahh <- diff(score[, 2])
blahh[1,] <- 0
head(blahh)
tail(blahh)
sum(abs(blahh))


plotSeries <- function(datav, name.plot, indeks=NULL) {
  stopifnot(inherits(datav, "xts"))
  theme <- chart_theme()
#  if(name=='Signals') theme$col$line.col = 'red' 
  invisible(chart_Series(datav, name=name.plot))
  if (!is.null(indeks))
  {
    invisible(add_TA(indeks$positions>0, on=-1, col="lightgreen", border=NA))
    invisible(add_TA(indeks$positions<0, on=-1, col="lightgrey", border=NA))
    invisible(add_TA(indeks$stop.loss>0, on=-1, col="red", border=NA))
  }
# Plot the charts and suppress warnings
  suppressWarnings(.chob)
}  # end plotSeries


