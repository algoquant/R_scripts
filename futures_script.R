library(quantmod)
library(TTR)

# script for loading from FRE6871_Lecture_5.pdf slide #13
da_ta <- read.zoo(file="data.csv", 
                  header=TRUE, sep=",", FUN=as.POSIXct, 
                  tz="America/New_York",
                  format="%m/%d/%Y")
# coerce to xts
da_ta <- as.xts(da_ta)
tail(da_ta)

# calculate returns - produces a list of xts
re_turns <- lapply(da_ta, dailyReturn)
length(re_turns)
names(re_turns)
tail(re_turns$"Jan.16")
# calculate volatility - produces a named numeric vector
sapply(re_turns, sd)

running_vol <- function(da_ta=da_ta, ra_nge=NULL, se_ries=colnames(da_ta[, 1]), win_dow=10) {
  stopifnot("package:quantmod" %in% search() || require("quantmod", quietly=TRUE))
  stopifnot("package:TTR" %in% search() || require("TTR", quietly=TRUE))
  if (is.null(ra_nge))
    ra_nge <- index(da_ta)
# calculate returns
  re_turns <- dailyReturn(na.omit(da_ta[ra_nge, se_ries]))
# calculate running volatility
  na.omit(runSD(x=re_turns, n=win_dow))
}  # end running_vol

foo_bar <- running_vol(da_ta, ra_nge="2015-03-01/2015-11-25", se_ries="Apr.16", win_dow=20)
chart_Series(foo_bar)


