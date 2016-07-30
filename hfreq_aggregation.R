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
chart_Series(get(sym_bol)[ran_ge], name=sym_bol)
chart_Series(vol_at, name=paste(sym_bol, "vol"))
# plotting sk_ew with custom y-axis range
# extract and modify plot object to reduce y-axis range
ch_ob <- chart_Series(x=sk_ew, plot=FALSE)
# extract and modify ylim using accessor and setter functions
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(c(-1, 1), fixed=TRUE)
ch_ob$set_ylim(y_lim)
# render the plot
plot(ch_ob)

chart_xts(sk_ew)
chart_xts(sk_ew, ylim=c(-1, 1))


### simple trading strategy for daily aggs

re_turns <- get(sym_bol)[index(sk_ew), 4]
re_turns <- diff(log(re_turns))
colnames(re_turns) <- paste(sym_bol, "Ret", sep=".")

# correlation
blah <- na.omit(merge(sk_ew, re_turns))
colnames(blah) <- c(colnames(blah)[1], "SPY.Ret")
# scatterplot of sk_ew and re_turns
plot(coredata(blah["2008-09/2009-05"]))
cor(coredata(blah["2008-09/2009-05"]))
cor.test(coredata(blah["2008-09/2009-05"])[, 1], coredata(blah["2008-09/2009-05"])[, 2])


# run strategy out-of-sample
blah <- -sign(lag(sk_ew))
colnames(blah) <- paste(sym_bol, "Posit", sep=".")
blah <- na.omit(merge(blah, re_turns))
# scatterplot of sk_ew and re_turns
plot(coredata(blah["2008-09/2009-05"]))
cor(coredata(blah["2008-09/2009-05"]))
cor.test(coredata(blah["2008-09/2009-05"])[, 1], coredata(blah["2008-09/2009-05"])[, 2])
blah <- cumsum(blah[, 1]*blah[, 2])
plot(blah)



### simple trading strategy for rolling minute aggs

# signal threshold trading level
thresh_old <- 0.5
# sig_nal <- sign(sk_ew-thresh_old)
sig_nal <- NA*numeric(nrow(sk_ew))
sig_nal <- ifelse(sk_ew>thresh_old, -1, sig_nal)
sig_nal <- ifelse(sk_ew<(-thresh_old), 1, sig_nal)
sig_nal <- ifelse((sk_ew*lag(sk_ew))<0, 0, sig_nal)
# sig_nal <- xts(x=sig_nal, order.by=index(sk_ew))
# lag the signal by one
# sig_nal <- lag(sig_nal)
sig_nal <- c(0, sig_nal)
sig_nal <- sig_nal[-length(sig_nal)]
sig_nal <- na.locf(sig_nal)
sig_nal <- merge(sk_ew, sig_nal)
colnames(sig_nal)[2] <- "positions"

# number of bars in long and short positions
sum(sig_nal[, 2]>0)
sum(sig_nal[, 2]<0)

chart_Series(sig_nal["2013-10-12/2013-11-13"],
             name=paste(sym_bol, "skew"))

# chart_xts(sig_nal["2013-10-12/2013-11-13"])

# plotting
ran_ge <- "2013-10-12/2013-11-13"
# extract and modify plot object to reduce y-axis range
ch_ob <- chart_Series(x=sig_nal[ran_ge, 1], 
                      name=paste(colnames(sig_nal[ran_ge, 1]), "/", date()), 
                      plot=FALSE)
# extract and modify ylim using accessor and setter functions
y_lim <- ch_ob$get_ylim()
y_lim[[2]] <- structure(c(-1, 1), fixed=TRUE)
ch_ob$set_ylim(y_lim)
# render the plot
plot(ch_ob)
add_TA(sig_nal[ran_ge, 2]>0, on=-1, col="lightgreen", border=NA)
add_TA(sig_nal[ran_ge, 2]<0, on=-1, col="lightpink", border=NA)
add_TA(sig_nal[ran_ge, 2]==0, on=-1, col="lightgrey", border=NA)

# cumulative PnL
blah <- cumsum(sig_nal[, 2]*re_turns[, 1])
plot(blah, format.labels="%Y-%m")

# number of position flips
blahh <- diff(sig_nal[, 2])
blahh[1,] <- 0
head(blahh)
tail(blahh)
sum(abs(blahh))


plotSeries <- function(da_ta, name.plot, in_dex=NULL) {
  stopifnot(inherits(da_ta, "xts"))
  theme <- chart_theme()
#  if(name=='Signals') theme$col$line.col = 'red' 
  invisible(chart_Series(da_ta, name=name.plot))
  if (!is.null(in_dex))
  {
    invisible(add_TA(in_dex$positions>0, on=-1, col="lightgreen", border=NA))
    invisible(add_TA(in_dex$positions<0, on=-1, col="lightgrey", border=NA))
    invisible(add_TA(in_dex$stop.loss>0, on=-1, col="red", border=NA))
  }
# Plot the charts and suppress warnings
  suppressWarnings(.chob)
}  # end plotSeries


