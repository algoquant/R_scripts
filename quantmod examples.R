###########
# first steps
library(quantmod)
library(xtable)


### ETF symbols - tickers for Tactical Asset Allocation System by Mebane Faber
symbolv <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")

# read etf database into data frame
etf_list <- read.csv(file='etf_list.csv', stringsAsFactors=FALSE)
# symbolv %in% etf_list$Symbol
# subset etf_list to include only those ETF's in symbolv
etf_list <- etf_list[etf_list$Symbol %in% symbolv, ]

etf_names <- sapply(etf_list$Name, 
               function(name) {
                 namesvplit <- strsplit(name, split=" ")[[1]]
                 namesvplit <- namesvplit[c(-1, -length(namesvplit))]
                 paste(namesvplit, collapse=" ")
       })
etf_list$Name <- etf_names
names(etf_list)
rownames(etf_list) <- NULL
etf_table <- xtable(etf_list)
print(xtable(etf_list))




###

symbolv <- c('QQQ','SPY')

ls()
suppressWarnings(getSymbols(symbolv))
ls()
class(QQQ)
head(QQQ)
plot(QQQ[, "QQQ.Close"])

data_env <- new.env()
getSymbols(symbolv, src='yahoo', from='1896-01-01', env=data_env, auto.assign=T)
# bt.prep(data_env, align='keep.all', dates='1896::2011')

# compare dailyReturn() with diff(log()) - why are they slightly different - see below?
# http://quant.stackexchange.com/questions/1079/quantmod-whats-the-difference-between-rocclspy-and-clclspy
# http://stackoverflow.com/questions/13522528/calculate-returns-of-xts-object-with-multiple-columns
QQQ_rets <- dailyReturn(QQQ)
# ROC returns all columns
blah <- ROC(QQQ)
# dailyReturn exact replication
blah <- lag(QQQ[, "QQQ.Adjusted"])
blah <- (QQQ[, "QQQ.Adjusted"] - blah)/blah
# dailyReturn only approximate replication
blah <- diff(log(QQQ[, "QQQ.Adjusted"]))
tail(blah)
tail(QQQ_rets)

# but colnamev is now "daily.returns"


# myRet <- sapply(list(STOXX50E,GSPC,N225), dailyReturn)
# cbind(myRet[[1]], myRet[[2]], myRet[[3]])


# showSymbols
# removeSymbols


###########
# very simple getSymbols script

# download tick data using quantmod getSymbols function
s1 <- "AAPL"
getSymbols(s1)
barChart(get(s1))

# or assign by hand

s2 <- getSymbols(s1, auto.assign=FALSE)
barChart(s2)



###########
# specify new environment in which to store data

library(quantmod)
s <- c("MSFT","C","MMM")
e <- new.env() #environment in which to store data
getSymbols(s, src="yahoo", env=e)
do.call(merge, eapply(e, Cl)[s])

# Or, using try like the OP
L <- lapply(symbols, function(sym) try(getSymbols(sym, auto.assign=FALSE)))
do.call(merge, lapply(L, Cl))

# http://stackoverflow.com/questions/20850143/quantmod-getsymbols-error-trying-to-replicate-answer



###########
# volatility of Dow Jones Index

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



