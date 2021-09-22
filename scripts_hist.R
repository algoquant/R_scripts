rm(list=ls())  # remove all
options(digits.secs=6)
options(stringsAsFactors=FALSE)
require(quantstrat)
require(utils)
loadInstruments(file_name='E:/mktdata/instruments.rda')
ls()
search()
ls(FinancialInstrument)
ls(package:FinancialInstrument)
ls_instruments()
tail(ls(.instrument))
search()
tail(ls(FinancialInstrument:::.instrument))

data_source <- "E:/mktdata/sec/"
setDefaults(getSymbols.FI,
            extension="RData",
            dir=data_source,
            days_to_omit="Saturday",
            use_identifier="X.RIC")
getSymbols("ES")
getSymbols("ESM9")
getDefaults(getSymbols.FI)
data_source
getSymbols("ESM5")
getSymbols("ESU9")
ls()
loadInstruments(file_name='E:/mktdata/instruments.rda')


?array
blah$acf <- array(data=blah$acf[-1,1,1], dim=c(21,1,1))
blah$acf
dim(blah$acf)[1]
my.acf <- function (x, ...)
{
acf.data <- acf(x, plot=FALSE, ...)
acf.data$acf <- array(data=acf.data$acf[-1,1,1], dim=c(dim(acf.data$acf)[1]-1,1,1))
acf.data$lag <- array(data=acf.data$lag[-1,1,1], dim=c(dim(acf.data$lag)[1]-1,1,1))
plot(acf.data, ...)
return(invisible(acf.data))
}  # end my.acf
acf.unemprate <-
my.acf(diff.macro[, "unemprate"], lag.max=10,
xlab="", ylab="",
main="average quarterly unemployment rate")
acf.3mTbill <-
my.acf(diff.macro[, "3mTbill"], lag=10,
xlab="", ylab="",
main="3 month T-bill EOQ")
library(astsa)
install.packages("astsa")
library(astsa)
acf2
my.acf <- function (ts.data, ...)
{
acf.data <- acf(ts.data, plot=FALSE, ...)
acf.data$acf <-
array(data=acf.data$acf[-1,1,1],
dim=c(dim(acf.data$acf)[1]-1,1,1))
acf.data$lag <-
array(data=acf.data$lag[-1,1,1],
dim=c(dim(acf.data$lag)[1]-1,1,1))
plot(acf.data)
return(invisible(acf.data))
}  # end my.acf
acf.3mTbill <-
my.acf(diff.macro[, "3mTbill"], lag=10,
xlab="", ylab="",
main="3 month T-bill EOQ")
my.acf <- function (ts.data, xlab=xlab, ylab=ylab, main=main, ...)
{
acf.data <- acf(ts.data, plot=FALSE, ...)
acf.data$acf <-
array(data=acf.data$acf[-1,1,1],
dim=c(dim(acf.data$acf)[1]-1,1,1))
acf.data$lag <-
array(data=acf.data$lag[-1,1,1],
dim=c(dim(acf.data$lag)[1]-1,1,1))
plot(acf.data, xlab=xlab, ylab=ylab, main=main)
return(invisible(acf.data))
}  # end my.acf
acf.unemprate <-
my.acf(diff.macro[, "unemprate"], lag.max=10,
xlab="", ylab="",
main="average quarterly unemployment rate")
acf.3mTbill <-
my.acf(diff.macro[, "3mTbill"], lag=10,
xlab="", ylab="",
main="3 month T-bill EOQ")
str(acf.unemprate)
my.acf(diff.macro[, "unemprate"], lag.max=10,
xlab="", ylab="",
main="average quarterly unemployment rate")
acf.unemprate <-
my.acf(diff.macro[, "unemprate"], lag.max=10,
xlab="", ylab="",
main="average quarterly unemployment rate")
?str
acf.dax <- acf(ts.dax, lag=5, xlab=NA)
ts.dax <- diff(log(EuStockMarkets[, 1]))
acf.dax <- acf(ts.dax, lag=5, xlab=NA)
dim(acf.dax)
dim(ts.dax)
head(ts.dax)
length(ts.dax)
acf.dax <- acf(ts.dax, lag=5, xlab=NA)
str(acf.dax)  # get the structure of the "acf" object
acf.dax <- acf(ts.dax, xlab=NA)
str(acf.dax)  # get the structure of the "acf" object
dim(acf.dax$acf)
dim(acf.dax$lag)
dim(acf.dax$lag)[1]
head(acf.dax$acf)
head(acf.dax$acf[-1,1,1])
head(acf.dax$acf[-1])
LjBox <- Box.test(ts.dax, lag=10, type="Ljung")
LjBox$statistic
LjBox$p.value
LjBox
Box.test(diff.macro[, "unemprate"], lag=10, type="Ljung")
Box.test(diff.macro[, "3mTbill"], lag=10, type="Ljung")
?Box.test
library(lmtest)
dwtest(rnorm(length(ts.dax)))
?arima.sim
?zoo
daily.index <- Sys.Date() + 0:999  # daily series over one year
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.05*(1:4)))
order.by=daily.index
)  # zoo.ar
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.05*(1:4))),
order.by=daily.index
)  # zoo.ar
colnames(zoo.ar) <- "AR time series"
name(zoo.ar) <- "AR time series"
colnames(zoo.ar)
colnames(zoo.ar) <- "aa"
autoplot(  # generic ggplot2 for "zoo"
object=zoo.ar, main="AR time series",
facets=Series ~ .
) + xlab("") + # end autoplot
theme(  # modify plot theme
legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)  # end theme
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.2*(1:4))),
order.by=daily.index
)  # zoo.ar
autoplot(  # generic ggplot2 for "zoo"
object=zoo.ar, main="AR time series",
facets=Series ~ .
) + xlab("") + # end autoplot
theme(  # modify plot theme
legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)  # end theme
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.2*(1:2))),
order.by=daily.index
)  # zoo.ar
autoplot(  # generic ggplot2 for "zoo"
object=zoo.ar, main="AR time series",
facets=Series ~ .
) + xlab("") + # end autoplot
theme(  # modify plot theme
legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)  # end theme
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.5)),
#  x=arima.sim(n=1000, model=list(ar=0.2*(1:2))),
order.by=daily.index
)  # zoo.ar
autoplot(  # generic ggplot2 for "zoo"
object=zoo.ar, main="AR time series",
facets=Series ~ .
) + xlab("") + # end autoplot
theme(  # modify plot theme
legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)  # end theme
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.9)),
#  x=arima.sim(n=1000, model=list(ar=0.2*(1:2))),
order.by=daily.index
)  # zoo.ar
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.9)),
#  x=arima.sim(n=1000, model=list(ar=0.2*(1:2))),
order.by=daily.index
)  # zoo.ar
autoplot(  # generic ggplot2 for "zoo"
object=zoo.ar, main="AR time series",
facets=Series ~ .
) + xlab("") + ylab("") + # end autoplot
theme(  # modify plot theme
legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)  # end theme
zoo.ar <- zoo(  # AR time series
x=arima.sim(n=1000, model=list(ar=0.98)),
#  x=arima.sim(n=1000, model=list(ar=0.2*(1:2))),
order.by=daily.index
)  # zoo.ar
autoplot(  # generic ggplot2 for "zoo"
object=zoo.ar, main="AR time series",
facets=Series ~ .
) + xlab("") + ylab("") + # end autoplot
theme(  # modify plot theme
v.phis <- c(0.01, 0.1, 0.2)
blah <- sapply(  # create three AR time series
v.phis, function(phi)
arima.sim(n=1000,
model=list(ar=phi)))
zoo.ar <- zoo(x=blah,
order.by=daily.index)
zoo.ar <- as.zoo(cumsum(zoo.ar))
colnames(zoo.ar) <- paste("autocorr", v.phis)
autoplot(zoo.ar, main="AR prices",
facets=Series ~ .) + xlab("") +
theme(legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)
v.phis <- c(0.01, 0.2, 0.4)
blah <- sapply(  # create three AR time series
v.phis, function(phi)
arima.sim(n=1000,
model=list(ar=phi)))
zoo.ar <- zoo(x=blah,
order.by=daily.index)
zoo.ar <- as.zoo(cumsum(zoo.ar))
colnames(zoo.ar) <- paste("autocorr", v.phis)
autoplot(zoo.ar, main="AR prices",
facets=Series ~ .) + xlab("") +
theme(legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)
v.phis <- c(0.01, 0.2, 0.4)
blah <- sapply(  # create three AR time series
v.phis, function(phi)
arima.sim(n=1000,
model=list(ar=phi)))
zoo.ar <- zoo(x=blah,
order.by=daily.index)
zoo.ar <- cumsum(zoo.ar)
colnames(zoo.ar) <- paste("autocorr", v.phis)
autoplot(zoo.ar, main="AR prices",
facets=Series ~ .) + xlab("") +
theme(legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)
v.phis <- c(0.01, 0.4, 0.8)
blah <- sapply(  # create three AR time series
v.phis, function(phi)
arima.sim(n=1000,
model=list(ar=phi)))
zoo.ar <- zoo(x=blah,
order.by=daily.index)
zoo.ar <- cumsum(zoo.ar)
colnames(zoo.ar) <- paste("autocorr", v.phis)
autoplot(zoo.ar, main="AR prices",
facets=Series ~ .) + xlab("") +
theme(legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank()
)
daily.index <- Sys.Date() + 0:999  # daily series over one year
zoo.ar <- zoo(  # AR time series of returns
x=arima.sim(n=1000, model=list(ar=0.2)),
order.by=daily.index)  # zoo.ar
blah <- cbind(zoo.ar, cumsum(zoo.ar))
colnames(blah) <- c("AR returns", "AR prices")
?range
range(blah)
range(blah[,1])
range(blah[,2])
?coeff
r1 <- range(blah[,1])
r2 <- range(blah[,2])
m.factor <- abs(r2[1]-r2[2])/abs(r1[1]-r1[2])
m.factor
blah <- m.factor*blah
autoplot(  # plot AR returns
object=blah, main="Autoregressive model (phi=0.2)",
facets=Series ~ .) + xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank())
blah <- cbind(zoo.ar, cumsum(zoo.ar))
colnames(blah) <- c("AR returns", "AR prices")
r1 <- range(blah[,1])
r2 <- range(blah[,2])
m.factor <- abs(r2[1]-r2[2])/abs(r1[1]-r1[2])
blah[,1] <- m.factor*blah[,1]
autoplot(  # plot AR returns
object=blah, main="Autoregressive model (phi=0.2)",
facets=Series ~ .) + xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank())
blah <- cbind(zoo.ar, cumsum(zoo.ar))
colnames(blah) <- c("AR returns", "AR prices")
r1 <- range(blah[,1])
r2 <- range(blah[,2])
m.factor <- abs(r2[1]-r2[2])/abs(r1[1]-r1[2])
blah[,1] <- 2*m.factor*blah[,1]
autoplot(  # plot AR returns
object=blah, main="Autoregressive model (phi=0.2)",
facets=Series ~ .) + xlab("") + ylab("") +
theme(legend.position=c(0.1, 0.5),
plot.title=element_text(vjust=-2.0),
plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"),
plot.background=element_blank(),
axis.text.y=element_blank())
?arima
my.acf(na.omit(diff(zoo.ar[,1])), lag.max=10,
xlab="", ylab="",
main="AR model")
my.acf(na.omit(diff(zoo.ar[,3])), lag.max=10,
xlab="", ylab="",
main="AR model")
blah <- na.omit(diff(zoo.ar[,3]))
tail(blah)
my.acf(coredata(na.omit(diff(zoo.ar[,3]))), lag.max=10,
xlab="", ylab="",
main="AR model")
my.acf(coredata(na.omit(diff(zoo.ar[,3]))), lag.max=5,
xlab="", ylab="",
main="AR model")
v.phis <- c(0.01, 0.4, 0.8)
zoo.ar <- sapply(  # create three AR time series
v.phis, function(phi)
arima.sim(n=1000, model=list(ar=phi)))
zoo.ar <- zoo(x=zoo.ar, order.by=daily.index)
zoo.ar <- cumsum(zoo.ar)  # returns to prices
colnames(zoo.ar) <- paste("autocorr", v.phis)
r1 <- range(zoo.ar[,1])
r2 <- range(zoo.ar[,2])
r3 <- range(zoo.ar[,3])
m.factor <- abs(r3[1]-r3[2])/abs(r1[1]-r1[2])
zoo.ar[,1] <- m.factor*zoo.ar[,1]
m.factor <- abs(r3[1]-r3[2])/abs(r2[1]-r2[2])
zoo.ar[,2] <- m.factor*zoo.ar[,2]
my.acf(coredata(na.omit(diff(zoo.ar[,3]))), lag.max=5,
xlab="", ylab="",
main="AR model")
my.acf(coredata(na.omit(diff(zoo.ar[,3]))), lag.max=10,
xlab="", ylab="",
main="AR model")
pacf(coredata(na.omit(diff(zoo.ar[,3]))), lag.max=10,
xlab="", ylab="",
main="AR model")
pacf(coredata(na.omit(diff(zoo.ar[,3]))), lag.max=5,
xlab="", ylab="",
main="AR model")
zoo.ar3 <- zoo(  # AR(3) time series of returns
x=arima.sim(n=1000, model=list(ar=c(0.2, 0.2, 0.2)), sd=0),
order.by=daily.index)  # zoo.ar
pacf(zoo.ar3, lag.max=10,
xlab="", ylab="", main="PACF of AR(3) process")
blah <- arima.sim(n=1000, model=list(ar=c(0.2, 0.2, 0.2)), sd=0.01)
tail(blah)
head(blah)
pacf(blah, lag.max=10, xlab="", ylab="", main="PACF of AR(3) process")
blah <- arima.sim(n=10000, model=list(ar=c(0.2, 0.2, 0.2)), sd=0.01)
pacf(blah, lag.max=10, xlab="", ylab="", main="PACF of AR(3) process")
blah <- arima.sim(n=10000, model=list(ar=c(0.2, 0.2, 0.2)), sd=0.1)
pacf(blah, lag.max=10, xlab="", ylab="", main="PACF of AR(3) process")
?rnorm
blah <- arima.sim(n=10000, model=list(ar=c(0.2, 0.2, 0.2)), rand.gen=function() rnorm(sd=0.001))
blah <- arima.sim(n=10000, model=list(ar=c(0.2, 0.2, 0.2)), rand.gen=function(n, ...) rnorm(sd=0.001))
blah <- arima.sim(n=10000, model=list(ar=c(0.2, 0.2, 0.2)), sd=0.001)
pacf(blah, lag.max=10, xlab="", ylab="", main="PACF of AR(3) process")
blah <- arima.sim(n=100000, model=list(ar=c(0.2, 0.2, 0.2)), sd=0.001)
pacf(blah, lag.max=10, xlab="", ylab="", main="PACF of AR(3) process")
blah <- arima.sim(n=100000, model=list(ar=c(0.2, 0.2, 0.2)), sd=0.1)
pacf(blah, lag.max=10, xlab="", ylab="", main="PACF of AR(3) process")
blah <- arima.sim(n=100000, model=list(ar=c(0.1, 0.3, 0.1)), sd=0.001)
pacf(blah, lag.max=10, xlab="", ylab="", main="PACF of AR(3) process")
?Arima
?arima
blahh <- arima(blah)
str(blahh)
summary(blahh)
blahh <- arima(blah, order = c(3,0,0))
summary(blahh)
?auto.arima
blahh <- auto.arima(blah)
summary(blahh)
blahh <- auto.arima(diff.macro[, "unemprate"])
dim(diff.macro)
summary(blahh)
blahh <- auto.arima(diff.macro[, "3mTbill"])
summary(blahh)
length(ts.dax)
ts.dax <- diff(log(EuStockMarkets[, 1]))
blahh <- auto.arima(ts.dax)
summary(blahh)
ts.dax <- diff(log(EuStockMarkets[, 1]))
blah <- arima(ts.dax, order = c(3,0,0))
summary(blah)
blah
blah <- arima(ts.dax, order = c(5,0,5))
blah <- arima(ts.dax, order = c(3,0,3))
blah <- arima(ts.dax, order = c(5,0,1))
blah
library(Ecdat)  # load Ecdat
zoo.macro <- as.zoo(  # coerce to "zoo"
Macrodat[, c("lhur", "fygm3")])
library(zoo)  # load zoo
library(ggplot2)  # load ggplot2
library(gridExtra)  # load gridExtra
zoo.macro <- as.zoo(  # coerce to "zoo"
Macrodat[, c("lhur", "fygm3")])
colnames(zoo.macro) <- c("unemprate", "3mTbill")
diff.macro <- na.omit(diff(zoo.macro))
blah <- arima(zoo.macro[, "unemprate"], order = c(5,0,1))
blah
blah <- auto.arima(diff.macro[, "unemprate"])
(f
)
library(forecast)  # load forecast
blah <- auto.arima(diff.macro[, "unemprate"])
blah
zoo.ar <- arima.sim(n=10000,
model=list(ar=c(0.1, 0.3, 0.1)))
arima(zoo.ar, order = c(5,0,0))
arima(zoo.ar3, order = c(5,0,0))
zoo.ar3 <- zoo(  # AR(3) time series of returns
x=arima.sim(n=1000, model=list(ar=c(0.1, 0.3, 0.1))),
order.by=daily.index)  # zoo.ar
arima(zoo.ar3, order = c(5,0,0))
?arima
auto.arima(zoo.ar3)
length(daily.index)
zoo.ar3 <- arima.sim(n=10000, model=list(ar=c(0.1, 0.3, 0.1)))
auto.arima(zoo.ar3)
arima(zoo.ar3, order = c(5,0,0))
f.obj <- y ~ x
class(f.obj)
f.obj
?formula
paste0("x", 1:5)
paste(paste0("x", 1:5), collapse="+")
?lm
set.seed(1121)  # initialize random number generator
v.xvar <- 0.1*1:30  # independent variable
v.yvar <- 3 + 2*v.xvar + rnorm(30)  # dependent variable plus noise
lm.simp <- lm(v.yvar ~ v.xvar)  # perform regression
summary(lm.simp)  # regression summary
plot(lm.simp)
plot(lm.simp)
form.simple <- v.yvar ~ v.xvar  # specify model
lm.simple <- lm(form.simple)  # perform regression
summary(lm.simple)  # regression summary
str(lm.simple)
lm.simple$coefficients
lm.simple$residuals
str(summary(lm.simple))
summary(lm.simple)$call
summary(lm.simple)$coefficients
summary(lm.simple)$r.squared
str(lm.simple)
library(lmtest)
dwtest(lm.simple)
ls()
summary(lm.simple)
summary(lm.simple)$formula
str(summary(lm.simple))
summary(lm.simple)$call
str(summary(lm.simple)$call)
deparse(summary(lm.simple)$call)
lm.simple$call
lm.simple$model
my.acf <- function (ts.data, xlab,
ylab, main, ...)
# wrapper for base acf()
{
acf.data <- acf(x=ts.data, plot=FALSE, ...)
acf.data$acf <-  # remove first element
array(data=acf.data$acf[-1],
dim=c(dim(acf.data$acf)[1]-1,1,1))
acf.data$lag <-  # remove first element
array(data=acf.data$lag[-1],
dim=c(dim(acf.data$lag)[1]-1,1,1))
plot(acf.data, xlab=xlab, ylab=ylab,
main=main)
return(invisible(acf.data))
}  # end my.acf
my.acf(diff.macro[, "unemprate"], lag.max=10,
xlab="", ylab="",
main="average quarterly unemployment rate")
my.acf(diff.macro[, "3mTbill"], lag.max=10,
xlab="", ylab="",
main="3 month T-bill EOQ")
set.seed(1121)  # initialize random number generator
v.xvar <- 0.1*1:30  # independent variable
v.yvar <- 3 + 2*v.xvar + rnorm(30)  # dependent variable plus noise
form.simple <- v.yvar ~ v.xvar  # specify model
lm.simple <- lm(form.simple)  # perform regression
summary(lm.simple)  # regression summary
par(mfrow=c(2, 2))
plot(lm.simple)
library(devtools)
install.packages("devtools")
library(devtools)
library(devtools)
savehistory("C:/Develop/R/scripts/scripts_hist.R")

if (flip.indic[date.bar]) {
  cat('\n')
  # store trade in blotter
  addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
} # end if
# Calculate P&L and resulting equity (blotter)
updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
for (date.bar in ma.window:nrow(GSPC)) {
  # browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
  # get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
  # Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
    # store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if
  # Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=time(GSPC)[ma.window], TxnPrice=1, TxnQty=1 , TxnFees=0, verbose=TRUE)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
for (date.bar in ma.window:nrow(GSPC)) {
  # browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
  # get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
  # Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
    # store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if
  # Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
posn.indic[11]*UnitSize
posn.indic[11]
unclass(posn.indic[11])
as.numeric(posn.indic[11])
as.vector(posn.indic[11])
posn.indic <- sign(as.numeric(Ad(GSPC) - GSPC[, 'indicator']))
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
for (date.bar in ma.window:nrow(GSPC)) {
  # browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
  # get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
  # Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
    # store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if
  # Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
tail(posn.indic)
posn.indic[2]
head(posn.indic)
tail(flip.indic)
head(flip.indic)
?lag
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
for (date.bar in ma.window:nrow(GSPC)) {
  # browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
  # get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
  # Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
    # store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if
  # Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=time(GSPC)[ma.window], TxnPrice=1, TxnQty=UnitSize, TxnFees=0, verbose=TRUE)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=time(GSPC)[ma.window], TxnPrice=1, TxnQty=posn.indic[1]*UnitSize, TxnFees=0, verbose=TRUE)
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=index.GSPC[ma.window], TxnPrice=1, TxnQty=posn.indic[ma.window]*UnitSize, TxnFees=0, verbose=TRUE)
for (date.bar in ma.window:nrow(GSPC)) {
  # browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
  # get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
  # Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
    # store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if
  # Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
head(posn.indic)
tail(posn.indic)
posn.indic[ma.window]
posn.indic[ma.window]*UnitSize
head(cbind(posn.indic, flip.indic), 11)
cbind(posn.indic, flip.indic)[1:22, ]
cbind(posn.indic, flip.indic)[ma.window]
posn.indic[ma.window]*UnitSize
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=index.GSPC[ma.window], TxnPrice=as.numeric(Ad(GSPC[ma.window, ])), TxnQty=posn.indic[ma.window]*UnitSize, TxnFees=0, verbose=TRUE)
for (date.bar in ma.window:nrow(GSPC)) {
  # browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
  # get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
  # Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
    # store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if
  # Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
try(rm("account.longtrend", "portfolio.longtrend", pos=.blotter), silent=TRUE)
try(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "date.bar", "initDate", "initEq", "posn.indic", "flip.indic", "UnitSize"), silent=TRUE)
initDate <- '1997-12-31'
initEq <- 10000
currency("USD")
stock("GSPC", currency="USD", multiplier=1)
GSPC <- to.monthly(GSPC.daily, indexAt='endof', drop.time=FALSE)
ma.window <- 8
GSPC$indicator <- SMA(Ad(GSPC), ma.window)
GSPC[1:(ma.window-1), 'indicator'] <- GSPC[ma.window, 'indicator']
ltportfolio <- 'longtrend'
ltaccount <- 'longtrend'
initPortf(ltportfolio, 'GSPC', initDate=initDate)
initAcct(ltaccount, portfolios='longtrend', initDate=initDate, initEq=initEq)
UnitSize <- 1
index.GSPC <- index(GSPC)
posn.indic <- sign(Ad(GSPC) - GSPC[, 'indicator'])
flip.indic <- lag(posn.indic)*posn.indic < 0
flip.indic[1] <- FALSE
posn.indic <- as.numeric(posn.indic)
addTxn(ltportfolio, Symbol='GSPC', TxnDate=index.GSPC[1], TxnPrice=as.numeric(Ad(GSPC[1, ])), TxnQty=posn.indic[1]*UnitSize, TxnFees=0, verbose=TRUE)
for (date.bar in 2:nrow(GSPC)) {
  # browser()
  cat('.')
  CurrentDate <- index.GSPC[date.bar]
  # get value of 'Adjusted' price column
  ClosePrice <- as.numeric(Ad(GSPC[date.bar, ]))
  # Flip position if indicator is opposite to current position
  if (flip.indic[date.bar]) {
    cat('\n')
    # store trade in blotter
    addTxn(ltportfolio, Symbol='GSPC', TxnDate=CurrentDate, TxnPrice=ClosePrice, TxnQty=2*posn.indic[date.bar]*UnitSize, TxnFees=0, verbose=TRUE)
  } # end if
  # Calculate P&L and resulting equity (blotter)
  updatePortf(ltportfolio, Dates=CurrentDate)
} # End dates loop
chart.Posn(ltportfolio, Symbol='GSPC', Dates='1998::')
plot(add_SMA(n=ma.window, col='darkgreen', on=1))
tradeStats(Portfolio=ltportfolio)
trade.stats <- tradeStats(Portfolio=ltportfolio)
tab.trades <- cbind(
  c("Trades","Win Percent","Loss Percent","W/L Ratio"),
  c(trade.stats[,"Num.Trades"],trade.stats[,c("Percent.Positive","Percent.Negative")],
    trade.stats[,"Percent.Positive"]/trade.stats[,"Percent.Negative"])
)
tab.trades
tab.profit <- cbind(
  c("Net Profit","Gross Profits","Gross Losses","Profit Factor"),
  c(trade.stats[,c("Net.Trading.PL","Gross.Profits","Gross.Losses",
   "Profit.Factor")])
)
# averages
tab.wins <- cbind(
  c("Avg Trade","Avg Win","Avg Loss","Avg W/L Ratio"),
  c(trade.stats[,c("Avg.Trade.PL","Avg.Win.Trade","Avg.Losing.Trade",
   "Avg.WinLoss.Ratio")])
)
# combine
trade.stats.tab <- data.frame(tab.trades,tab.profit,tab.wins)
trade.stats.tab
objects()
search()
ts.rets <- PortfReturns(Account=ltportfolio)
rownames(ts.rets)
head(ts.rets)
class(ts.rets)
colnames(ts.rets)
rownames(ts.rets) <- NULL
rownames(ts.rets)
tail(ts.rets)
head(ts.rets)
charts.PerformanceSummary(ts.rets, colorset=bluefocus)
table.Arbitrary(ts.rets)
?table.Arbitrary
tab.perf <- table.Arbitrary(ts.rets,
            metrics=c(
              "Return.cumulative",
              "Return.annualized",
              "SharpeRatio.annualized",
              "CalmarRatio"),
            metricsNames=c(
              "Cumulative Return",
              "Annualized Return",
              "Annualized Sharpe Ratio",
              "Calmar Ratio")
)
tab.perf
tab.risk <- table.Arbitrary(rets,
            metrics=c(
              "StdDev.annualized",
              "maxDrawdown",
              "VaR",
              "ES"),
            metricsNames=c(
              "Annualized StdDev",
              "Max DrawDown",
              "Value-at-Risk",
              "Conditional VaR"))
tab.risk <- table.Arbitrary(ts.rets,
            metrics=c(
              "StdDev.annualized",
              "maxDrawdown",
              "VaR",
              "ES"),
            metricsNames=c(
              "Annualized StdDev",
              "Max DrawDown",
              "Value-at-Risk",
              "Conditional VaR"))
tab.risk
performance.stats.tab <- data.frame(
  rownames(tab.perf),tab.perf[,1],
  rownames(tab.risk),tab.risk[,1])
performance.stats.tab
R.Version()
################################################
read.clip <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}  # end read.clip
data.xls <- read.clip()
data.xls
data.xls <- read.clip()
data.xls
?read.table
read.clip <- function(file="clipboard", sep="\t", header=TRUE, ...) {
  read.table(file=file, sep=sep, header=header, ...)
}  # end read.clip
data.xls <- read.clip()
data.xls <- read.clip()
data.xls
?write.table
write.clip <- function(data, row.names=FALSE, col.names=TRUE, ...) {
  write.table(x=data, file="clipboard", sep="\t", row.names=row.names, col.names=col.names, ...)
}  # end write.clip
write.clip(data=data.xls)
edit(data.xls)
data.xls
edit(data.xls)
data.xls
rm(data.xls)
data.xls <- read.clip()
data.xls
data.xls[,1]
class(data.xls[,1])
edit(data.xls)
data.xls
blah <- edit(data.xls)
blah
library(ggplot2)
.libPaths()
UBS_Op18Nov <-(data.frame(read.table("C:/Develop/R/FRE6811/option_data.csv",header=TRUE, sep=",")))
summary(UBS_Op18Nov)
UBS_Op18Nov
rm(list=ls())  # remove all
options(digits.secs=6)
options(stringsAsFactors=FALSE)
require(quantstrat)
require(utils)
loadInstruments(file_name='E:/mktdata/instruments.rda')
ls()
search()
ls(FinancialInstrument)
ls(package:FinancialInstrument)
ls_instruments()
tail(ls(.instrument))
search()
tail(ls(FinancialInstrument:::.instrument))
getInstrument(ZW_Z9)
getInstrument(ZW)
getInstrument("IBM")
getInstrument("TSLA")
getInstrument("FSLR")
find.instrument("bond")
find.instrument("computer)
""
"
find.instrument("computer")
?setDefaults
?getSymbols
data_source <- "E:/mktdata/"
setDefaults(getSymbols.FI,
            extension="RData",
            dir=data_source,
            days_to_omit="Saturday",
            use_identifier="X.RIC")
setDefaults(getSymbols, verbose=FALSE, dir=data_source, src="rda")
getSymbols.rda
?getSymbolLookup
getSymbols("MSFT", verbose=FALSE, dir=data_source, src="rda")
getSymbols("ESH0", verbose=FALSE, dir=data_source, src="rda")
getInstrument("MSFT")
sink("C:/Develop/R/research/instruments.csv")
ls_instruments()  # vary large list!!!
sink()
?sink
ls()
load("E:/mktdata/ZQF3.rda")
load("E:/mktdata/ZQF3/2013.01.31.ZQF3.rda")
load("E:/mktdata/ZQF3/2013.01.31.ZQF3.rdata")
ls()
class(ZQF3)
tail(ZQF3)
head(ZQF3)
dim(ZQF3)
blah <- ZQF3[, 4:6]
dim(blah)
blah <- ZQF3[, 3:6]
dim(blah)
head(blah)
sum(is.na((blah)))
blah <- cbind((blah[, 1] + blah[, 3])/2, blah)
dim(blah)
head(blah)
colnames(blah) <- c("Bid.Price", "Bid.Price.1", "Bid.Size", "Ask.Price", "Ask.Size")
head(blah)
colnames(blah) <- c("Mid.Price", "Bid.Price", "Bid.Size", "Ask.Price", "Ask.Size")
head(blah)
plot(blah[, 1])
tail(blah)
setDefaults(getSymbols, verbose=FALSE, src="FI")
getSymbols("ESH3")
getSymbols.FI
getSymbols("ZQZ9")
loadInstruments(file_name='E:/mktdata/instruments.rda')
getSymbols("ZQZ9")
getSymbols("QZ9")
getSymbols("ESH3")
getSymbols("ZSK7")
getSymbols("ZSK5")
ls()
dim(ZSK5)
tail(ZSK5)
plot(ZSK5[, "Bid.Price"])
plot(tail(ZSK5[, "Bid.Price"], 1000))
plot(tail(ZSK5[, "Bid.Price"], 10000))
dim(ZSK5)[1]
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-100)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-1000):(dim(ZSK5)[1]-100)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-5000):(dim(ZSK5)[1]-100)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-1000)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-10000)), "Bid.Price"])
plot(ZSK5[((dim(ZSK5)[1]-10000):(dim(ZSK5)[1]-5000)), "Bid.Price"])
?setDefaults
getDefaults(getSymbols)
getDefaults(getSymbols.FI)
getSymbols("MSFT", verbose=FALSE, dir=data_source, src="rda")
blah <- ls_instruments()
tail(blah)
length(blah)
write.csv(blah, file="C:/Develop/R/research/instruments.csv")
getSymbols("ZSK6")
getSymbols("ZSK5")
getSymbols("ZSK5.N5")
dim(ZSK5)
dim(ZSK5.N5)
tail(ZSK5.N5, 22)
dim(ZSK5)
tail(ZSK5.N5)
plot(ZSK5.N5[, "Bid.Price"])
plot(ZSK5.N5[(100:(dim(ZSK5.N5)[1]-100)), "Bid.Price"])
plot(ZSK5.N5[(100:(dim(ZSK5.N5)[1]-500)), "Bid.Price"])
plot(ZSK5.N5[(500:(dim(ZSK5.N5)[1]-500)), "Bid.Price"])
rm(list=ls())  # remove all
data_source <- "E:/mktdata/sec/"
setDefaults(getSymbols.FI,
            extension="RData",
            dir=data_source,
            days_to_omit="Saturday",
            use_identifier="X.RIC")
getSymbols("ES")
getSymbols("ESM9")
getDefaults(getSymbols.FI)
data_source
getSymbols("ESM5")
getSymbols("ESU9")
ls()
loadInstruments(file_name='E:/mktdata/instruments.rda')
getSymbols("ESU9")
getSymbols("ESM5")
ls()
load("E:/mktdata/sec/ESM9/2009.04.02.ESM9.rdata")
ls()
dim(ESM9)
tail(ESM9)
head(ESM9)
plot(ESM9[(1000:(dim(ESM9)[1]-1000)), "Bid.Price"])
plot(ESM9[(10000:(dim(ESM9)[1]-10000)), "Bid.Price"])
plot(ESM9[((dim(ESM9)[1]-20000):(dim(ESM9)[1]-10000)), "Bid.Price"])
plot(ESM9[((dim(ESM9)[1]-11000):(dim(ESM9)[1]-10000)), "Bid.Price"])
dim(ESM9)[1]
len_es <- dim(ESM9)[1]
ESM9[(len_es-1010):(len_es-10000), ]
ESM9[(len_es-10010):(len_es-10000), ]
plot(ESM9[((len_es-20000):(len_es-10000)), "Bid.Price"])
sink("C:/Develop/R/research/temp.txt")
getSymbols.FI
sink()

