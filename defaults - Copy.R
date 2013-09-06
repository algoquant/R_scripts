library(xts)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)

library(ROneTick)
oneTickLib()

root.dir <- "C:/Devel/Models/"
otq.dir <- "../../OneTick/"
alpha.dir <- "../alphaLib/"
jr.dir <- "../JR/"
rmodels.dir <- "C:/Devel/Models/Rmodels/"

source(paste(alpha.dir, "alphaModel.R", sep=""))
source(paste(alpha.dir, "utilLib.R", sep=""))
source(paste(jr.dir, "sgfilter.R", sep=""))
source(paste(rmodels.dir, "dataLib.R", sep=""))
source(paste(rmodels.dir, "chartLib.R", sep=""))
source(paste(rmodels.dir, "riskLib.R", sep=""))
source(paste(rmodels.dir, "optimLib.R", sep=""))
