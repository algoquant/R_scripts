# misc scripts for testing

rm(list=ls())
options(max.print=80)
options(digits=3)

library(zoo)
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))


###########
# data scrubbing

rm(SPY)
sym_bols <- load("SPY.RData")

sym_bols <- load("E:/mktdata/sec/SPY/2012.02.16.SPY.RData")
sym_bols <- load("E:/mktdata/sec/SPY/2013.04.16.SPY.RData")

sym_bols <- scrub_agg(SPY)

indexTZ(SPY)
index(SPY) <- with_tz(index(SPY), "America/New_York")
SPY <- SPY['T09:15:00/T16:15:00', ]
SPY[11:44, "Bid.Price"]
SPY[11:44, "Ask.Price"]


chartSeries(SPY["2013-07-08", ], name="SPY", theme=chartTheme("white"))
chartSeries(SPY["2013-02-08", ], name="SPY", theme=chartTheme("white"))


chartSeries(SPY["2013-04", ], name="SPY", theme=chartTheme("white"))
chartSeries(SPY["2013-04-16", ], name="SPY", theme=chartTheme("white"))

chartSeries(SPY["2012", ], name="SPY", theme=chartTheme("white"))
chartSeries(SPY["2012-02", ], name="SPY", theme=chartTheme("white"))
chartSeries(SPY["2012-02-16", ], name="SPY", theme=chartTheme("white"))
chartSeries(SPY["2012-02", ], name="SPY", theme=chartTheme("white"))
chartSeries(SPY["2012-02-15/2012-02-16", ], name="SPY", theme=chartTheme("white"))

head(SPY[, "Bid.Price"], 44)
head(SPY[, "Ask.Price"], 44)
SPY <- scrub_agg(SPY)
chartSeries(SPY["2012-05-08", "SPY.Open"], name="SPY", theme=chartTheme("white"))
chartSeries(SPY["2012-05-08", 1], name="SPY", theme=chartTheme("white"))
head(SPY[, 1], 44)
head(SPY[, c(1,2)], 44)
head(SPY[, c('Bid.Price', 'Ask.Price')], 44)
head(SPY[, "Trade.Price"], 44)

re_turns <- diff(SPY[, 4])/c(1, diff(.index(SPY)))
re_turns[1, ] <- 0
re_turns <- na.locf(re_turns)


hist(re_turns, breaks=400, main="", xlab="", ylab="", xlim=c(-0.006, 0.006), freq=FALSE)
lines(density(re_turns), col='red', lwd=1)

library(PerformanceAnalytics)
chart.CumReturns(re_turns, lwd=2,
                 ylab="", legend.loc="topleft", main="")
chart.Histogram(re_turns, main="",
                xlim=c(-0.003, 0.003),
                methods = c("add.density", "add.normal"))
chart.Histogram(re_turns, main="",
                xlim=c(-0.003, 0.003), breaks=300,
                methods = c("add.normal"))



