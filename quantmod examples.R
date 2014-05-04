# Volatility of Dow Jones Index
library(quantmod)

# Dow Jones Index from FRED, Yahoo doesn't provide data anymore
getSymbols("DJIA", src="FRED", from="1800-01-01")

dji = na.exclude(DJIA["/2013"])

djiVol = aggregate(
  dji,
  as.numeric(format(index(dji), "%Y")),
  function(ss) coredata(tail(TTR:::volatility(
    ss,
    n=NROW(ss),
    calc="close"), 1)))
ecdf(as.vector(djiVol))(as.numeric(tail(djiVol,1)))
# The result is 0.1864407, the 18nd quantile

# Compute the absolute returns
absRets = na.exclude(abs(ROC(dji["/2013"], type="discrete")))

# Summarize annually
yy = as.numeric(format(index(absRets), "%Y"))
zz = aggregate(absRets, yy, function(ss) tail(cumprod(1+ss),1))

print(as.vector(tail(zz,1)))
# The result is 3.45
