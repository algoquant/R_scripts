
# http://www.programmingr.com/content/installing-quantstrat-r-forge-and-source/
# install packages
install.packages("blotter", repos="http://R-Forge.R-project.org")
install.packages("quantstrat", repos="http://R-Forge.R-project.org")

library(quantstrat)

# xts objects are stored in the global environment
# portfolio and account objects are stored in .blotter environment
# currency and trading instrument objects are stored in the .instrument environment

# assign portfolio and account names
# create portfolio and account objects (blotter)
b.strategy <- "my.strategy"
initPortf(b.strategy, 'SPY', initDate='1997-12-31')

# initDate is before the start of data
initAcct(b.strategy, portfolios=b.strategy, initDate='1997-12-31', initEq=1e6)

first(SPY)



### explore objects and environments
# get all objects in the global environment
ls(all=T)
# get all objects in the .blotter environment
ls(envir=.blotter)
# get all objects in the .instrument environment
ls(envir=FinancialInstrument:::.instrument)
# show "SPY" object
get("SPY,envir=FinancialInstrument:::.instrument)


# Remove objects associated with a strategy
rm.strat(b.strategy)
