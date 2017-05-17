# source: https://gist.github.com/rshowcase/cc30fac759195e0d014c

# In my portfolio, I show how the popular Fama-MacBeth (1973) procedure is constructed in R.
# The procedure is used to estimate risk premia and determine the validity of asset pricing models.
# Google shows that the original paper has currently over 9000 citations (Mar 2015), making the methodology one of the most  
# influential papers in asset pricing studies. It's used by thousands of finance students each year, but I'm unable to find a 
# complete description of it from the web. 
#
# While the methodology is not statistically too complex (although the different standard errors can get complex),
# it can pose some serious data management challenges to students and researchers.
#
# The goal of the methodology is to estimate risk premia in the financial markets. While newer, more sophisticated methods for
# estimating risk premia exist, FM has remained popular due to its intuition. The methodology can be summarized as follows:
#
#    1. Construct risk factor return series
#        - A risk factor return series is constructed from a zero-investment portfolio, where high-risk assets are held and
#	   financed by short-selling low-risk assets: it is up to the student or researcher to explain the criterion behind a risk factor
#        - The return series is thus a differential of two series: the returns of the long portfolio minus the returns of the short portfolio
#        - The portfolios don’t need to be equal-weighted, although they usually are in classic asset pricing studies.
#	   But hedge-fund originated strategies can use more sophisticated weighting, such as zero-beta: recent example.
#    2. Estimate factor loadings (FM 1st stage)
#        - Betas (=factor loadings) are estimated for each asset in a linear time series regression
#        - Thus, we need to specify what we consider a “correct” beta: remember, betas vary over time and they are always 
# 	   estimated with error
#        - I demonstrate the ex-ante and ex-post testing approaches with individual assets, as explained in more detail in Ang, Liu & Schwartz (2010).
#    3. Estimate risk premia (FM 2nd stage)
#        - Be careful not to confuse this stage with Fama-French (1993).
#        - The main idea is that beta estimates should explain individual asset returns
#        - This is tested by estimating multiple cross-sectional regression across asset returns
#        - Finally, average estimates are reported
#        - This step is pre-programmed in 3rd-party packages
#



#
# Start with some useful functions to help import data
#

# Point to dataset
dataset <- read.csv(file.choose())

# Replace commas with dots (R recognizes only dots as decimal separators)
dots <- sapply(commas, function(x) {as.numeric(gsub(",", ".", as.character(x)))})



#
# Now start with the data import
#

# Load libraries
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(repmis)
library(zoo)
library(sandwich)
library(lmtest)

# Read MSCI Equity index prices from my Dropbox
# Notice that the dataset is converted from an xlsx into csv, using ";" as separator
data <- source_DropboxData(file = "data.csv", key = "ocbkfvedc3aola8", sep = ";", header = TRUE)

# Delete first column with non-recognized date format
prices <- data[, -1]

# The numbers contain spaces as thousand separators and R doesn't like this
prices <- sapply(prices, function(x) {as.numeric(gsub("\\s","", as.character(x)))})

# Basic descriptive functions
str(prices)
head(prices)
summary(prices)
dim(prices)

# Transform prices into returns, omit the first row
# Declare first the prices to be a time series object
prices <- ts(data=prices, frequency=12, start=c(1969, 12))
returns <- Return.calculate(prices)
returns <- na.omit(returns)

# Keep track of the MSCI World returns
world <- grep("world", colnames(returns))

# Keep track of rows (time)
returns.t <- nrow(returns)

# Risk-free rate: read straight from FRED database and transform into monthly returns for our time period
getSymbols("TB3MS", src="FRED")
rf <- TB3MS[paste("1970-02-01", "2014-12-01", sep="/")]
rf <- (1+(rf/100))^(1/12)-1
rfts <- ts(data=rf, frequency=12, start=c(1970, 1))

# Finally calculate the market return factor
rmrf <- returns[,world]-rfts



#
# Example of constructing a risk factor
#

# There’s an infinite number of ways to build risk factor returns and it’s up to the researcher to motivate her decision.
# I will focus here on a t,t (here 6,6) momentum strategy approximation (reforming the portfolio is done every six months and
# the assets are held for six months. However, the portfolio is rebalanced monthly and the factor is thus an approximation –
# compound returns in the momentum period are not taken into account) that is common in the asset pricing literature.


# t,t month momentum strategy implementation

# 6,6 momentum, equal-weighted portfolios, rebalancing done every six months
mo <- 6

# Keep track of time
momentum.t <- returns.t-mo

# Create a matrix of 6-month simple moving average returns
smamat <- apply(returns, 2, SMA, n=mo+1)

# Copy the returns of every mo until the reforming of the portfolio
for (i in seq(from=1, to=nrow(smamat), by=mo)) {
  for (j in 1:mo-1) {
    smamat[i+j,] <- smamat[i,]
  }
}

# Then omit the first six NA's
smamat <- na.omit(smamat)

# Apply row-wise rank - higher return, higher rank
ranks <- t(apply(smamat, 1, rank))

# Define functions that assign assets into the highest and lowest quartiles
hiq <- function(hi) {
	return(hi>quantile(ranks, c(.75)))
}

loq <- function(lo) {
return(lo<quantile(ranks, c(.25)))
}

# Assign the assets into quartiles
highret <- t(apply(ranks, 1, hiq))
lowret <- t(apply(ranks, 1, loq))

# Calculate returns for the high (winner) and low (loser) portfolios
ret <- returns[-c(1:mo),]
ret <- ts(data=ret, frequency=12, start=c(1970, 7))

highp <- ret*highret
lowp <- ret*lowret
highstrat <- rowSums(highp)/rowSums(highp != 0)
lowstrat <- rowSums(lowp)/rowSums(lowp != 0)

# Finally we get the factor WML return series (Winners-minus-Losers)
wml <- highstrat-lowstrat

# Combine the needed information into a matrix
rmrf <- rmrf[-c(1:mo),]
row <- c(seq(momentum.t))
ret <- ret[,-1]

rets <- cbind(row, wml, rmrf, ret)



#
# First stage
#

# Specify the parameters
int <- 12 # Estimation period interval ("stationarity period")
est <- 60 # Beta estimation period length
fact <- 2 # Number of factors in the model

# Keep track of time
fstage.t <- momentum.t-est

# Create an empty list for estimates
estimates <- list()
for(s in colnames(ret)) {
estimates[[s]] <- matrix(, nrow=fstage.t+mo, ncol=fact+1)
colnames(estimates[[s]]) <- c("alphas", "mktbetas", "factorbetas")
}

# Ex-ante portfolios
# Fill every int:th row with estimates
for(t in seq(from=0, to=fstage.t, by=int)) {
m t & row < t+est) # For a 3-factor model, add the factor into the equation
for(i in 1:(ncol(ret))) {
estimates[[i]][t+1, fact-1] <- coef(m)[fact-1, i]
estimates[[i]][t+1, fact] <- coef(m)[fact, i]
estimates[[i]][t+1, fact+1] <- coef(m)[fact+1, i]
# For a 3-factor model, add row: estimates[[i]][t+1, fact+2] <- coef(m)[fact+2, i]
}
}

# Fill in rest of the estimates
for(k in 1:ncol(ret)) {
estimates[[k]] <- na.locf(estimates[[k]])
}

# Align the return matrix (ex-ante)
assets <- ret[-c(1:(est-mo)),]

# Return matrix into vector
assets <- c(assets)

# Ready the data frame for 2nd stage
sstage <- do.call(rbind.data.frame, estimates)
sstage$time <- rep(seq(fstage.t+mo), times=ncol(ret))
sstage$id <- rep(colnames(ret), each=fstage.t+mo)
sstage$returns <- assets



#
# Second stage
#

# This section is pretty much identical to the example code available through Mitchell Petersen’s website.
# First, we can check that we’re doing the right estimation by using Petersen’s test data and results.


# Use custom clustering functions by Stockholm University's Mahmood Arai
source("http://people.su.se/~ma/clmcl.R")

# Read test data from Petersen's website
test <- read.table("http://www.kellogg.northwestern.edu/faculty/petersen/htm/papers/se/test_data.txt", col.names = c("firmid", "year", "x", "y"))
head(test)

# Fit the model
fm <- lm(y ~ x, data=test)

# Compare the different standard errors
coeftest(fm) # OLS
coeftest(fm, vcov=vcovHC(fm, type="HC0")) # White
cl(test,fm, firmid) # Clustered by firm
cl(test,fm, year) # Clustered by year
mcl(test,fm, firmid, year) # Clustered by firm and year


# Next we do the same for our two-factor model.

#Fit the model
twof <- lm(returns ~ mktbetas + factorbetas, data=sstage)

# Compare the standard errors
coeftest(twof) # OLS
coeftest(twof, vcov=vcovHC(fm, type="HC0")) # White
cl(sstage,twof, firmid) # Clustered by firm
cl(sstage,twof, time) # Clustered by year
mcl(sstage,twof, firmid, time) # Clustered by firm and year

# And now we have estimated a two-factor model for market and momentum risk premia with N assets and T months.
