rm(list=ls())
options(max.print=20)

library(zoo)
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))


###

# ideas for HW and tests

### create function that throws error if argument is negative
test_func <- function(arg_var) {
  if (!is.numeric(arg_var)) {
    warning(paste("argument", arg_var, "isn't numeric"))
    return(NULL)
  }
  2*arg_var
} # end test_func


### create a function called "my_sqrt" that calculates the square root of its single argument,
# "my_sqrt" should check if the input is numeric and positive,
# if the input is numeric and positive, then "my_sqrt" should return the square root,
# if the input is numeric and negative, then "my_sqrt" should broadcast a warning using "cat", and return the square root of the absolute value,
# if the input is not numeric, then "my_sqrt" should broadcast a different warning using "cat", and return NA,
my_sqrt <- function(arg_var) {
  if (is.numeric(arg_var) && arg_var>=0) {
    sqrt(arg_var)
  } else if (is.numeric(arg_var)) {
    cat("negative input!\t")
    sqrt(abs(arg_var))
  } else {
    cat("not numeric input!\t")
    NULL
  }
}  # my_sqrt
my_sqrt(4)
my_sqrt(-4)
my_sqrt("a")


### create function called "my_sqrt" that throws error if argument is negative
my_sqrt <- function(arg_var) {
  if (arg_var > 0) {
    sqrt(arg_var)
  } else {
    stop('bad input!')  # throw error
  }
}  # my_sqrt


### create function called "read_numeric" that reads numbers input by the user, and returns them in a vector,
# "read_numeric" should ask the user to input a number, and should read the input using the function "readline",
# "read_numeric" should read numbers from the console in a "while" loop,
# "read_numeric" should validate the inputs, and produce errors and Warnings,
# if the user input is numeric, then "read_numeric" should append the input to the numeric output vector,
# if the input is not numeric, then "read_numeric" should produce a Warning "input is not numeric!",
# if the input is empty, then "read_numeric" should terminate, and return the numeric output vector,
# hint: "read_numeric" should use "readline", and can also use "is.na", "nchar", "as.numeric", "length", "identical", etc.
# the function reads numeric lines from input, and returns them in a vector,
# "read_numeric" should create a numeric vector consisting of the input numbers, 
# ignore it

read_numeric <- function() {
  out_put <- numeric(0)
  nu_meric <- readline("Enter a number: ")
  while(nchar(nu_meric) > 0) {
    nu_meric <- as.numeric(nu_meric)
    if (!is.na(nu_meric)) {
      out_put <- c(out_put, as.numeric(nu_meric))
    } else {
      warning("input is not numeric!")
    }  # end if
    nu_meric <- readline("Enter a number: ")
  }  # end while
  out_put
}  # end read_numeric
read_numeric()

# old version
read_numeric <- function() {
  out_put <- numeric(0)
  nu_meric <- readline("Enter a number: ")
  while(!identical(nu_meric, "")) {
    nu_meric <- as.numeric(nu_meric)
    if (!is.na(nu_meric)) {
      out_put <- c(out_put, nu_meric)
    }  # end if
    nu_meric <- readline("Enter a number: ")
  }  # end while
  out_put
}  # end read_numeric
read_numeric()


# ideas for HW and tests

### using mtcars data, plot a boxplot of mpg of cars with six cylinders
boxplot(mtcars[mtcars$cyl==6, ]$mpg)
with(mtcars[mtcars$cyl==6, ], boxplot(mpg))

# 2. (10pts) plot a histogram of "mpg" for all cars in the mtcars data frame, 
#    use function "truehist", and set the "prob" argument so that the plot displays the number of cars in each bin,
#    "truehist" counts the first two bins differently from "hist"
truehist(mtcars$mpg, nbins="FD", prob=FALSE, col="blue", xlab="mpg", ylab="number of cars", main="mpg true histogram")
hist(mtcars$mpg, breaks="FD", prob=FALSE, col="blue", xlab="mpg", ylab="number of cars", main="mpg histogram")

### using mtcars data, create a data frame called "cars_18", containing cars that have mpg greater than 18,
cars_18 <- mtcars[mtcars$mpg>18, ]
# using the function "table", calculate the number of cars in "cars_18", that have four, six, and eight cylinders
table(cars_18$cyl)
# or
sum(cars_18$cyl==4)
sapply(cars_18$cyl, sum)

with(mtcars[mtcars$mpg>20, ], barplot(mpg))

barplot(mtcars$mpg)
barplot(mtcars[mtcars$cyl==6, ]$mpg)
with(mtcars[mtcars$cyl==6, ], barplot(mpg))


###

# from field Garch$date create vector of strings in format "yyyy-mm-dd", using nested paste() and substring()
# 2. the column Garch$date contains dates as strings in the format "yymmdd",
library("Ecdat")  # load econometric data sets
head(Garch)  # explore the data
ymd(paste0(19, Garch[1, 1]))
# my_date is a numeric date that represents "1997-18-05"
# use several functions to convert it to a as.POSIXct date
# create a my_date is a numeric date that represents "1997-05-18",
# you can use functions from package lubridate, or other functions paste and substr,
# you will not receive any credit for creating a date "by hand": as.POSIXct("1997-05-18"),
my_date <- 970518
library(lubridate)
ymd(paste0(19, my_date), tz="America/New_York")
# as.POSIXct("1997-05-18")
as.POSIXct(
paste(paste0(19, substr(my_date, 1, 2)), 
      substr(my_date, 3, 4), 
      substr(my_date, 5, 6), sep="-")
)

###

# calculate stats of ts and return as vector of variable length
my_stats <- function(ts_var) {
  c(max(ts_var), min(ts_var), mean(ts_var), if (rnorm(1)>0) 1 else NULL)
}

# sapply returns list because of vectors of variable length
out_sapply <- sapply(EuStockMarkets, my_stats)


###

# create function which calculates summary statistics of zoo
# first split
# then call lapply or call sapply
# does sapply return list? - yes - if vectors are not same length
# convert to matrix using
do.call(rbind, list.data)


###
# returns list of data frames (zoo)
split_eu <- split(as.zoo(EuStockMarkets), colnames(EuStockMarkets))

out_lapply <- lapply(EuStockMarkets, mean)  # returns list of means
do.call(rbind, out_lapply)  # returns single column matrix
do.call(cbind, out_lapply)  # returns single row matrix
as.vector(out_lapply)
as.vector(do.call(cbind, out_lapply))
unlist(out_lapply)  # returns
class(unlist(out_lapply))
is.vector(unlist(out_lapply))
is.vector(do.call(cbind, out_lapply))
is.matrix(do.call(cbind, out_lapply))


###

# create function that Calculates row and column of the extreme value of a matrix
coordinates.matrix <- function(var.matrix, func.matrix) {
  func.name <- match.fun(func.matrix)
  tmp <- which(var.matrix==func.name(var.matrix), arr.ind=T)
  coordinates <- as.numeric(c(rownames(var.matrix)[tmp[1,1]], colnames(var.matrix)[tmp[1,2]]))
  coordinates
}



### seed_random() returns the pseudo-random generating function random_generator
# this version is with for loop instead of recursion
# the formal argument 'seed' persists in the evaluation environment of seed_random
seed_random <- function(seed) {  # seed must be an integer
  random_number <- as.numeric(paste0('0.', seed))  # initialize
# anon function returns a vector of pseudo-random numbers of length length_rand
  function(length_rand=1) {
    rand_vector <- numeric(length_rand)
    for (inter in 1:length_rand) {
# logistic map
      random_number <<- 4*random_number*(1 - random_number)
      rand_vector[inter] <- random_number
    }  # end for
  rand_vector
  }  # end anon
}  # end seed_random

# create a random number generating function and set seed
make_random <- seed_random(88)
make_random(10)  #  calculate vector of 10 pseudo-random numbers
ls(environment(make_random))  # list objects in scope of make_random

###



#################################
### numerical methods ####
#################################


########################
### optimization examples

### single variable optimization
# ?optimize
### generic optimization
# ?optim


### optimization example: fit normal variables

# target vector is histogram of normal distribution
histo_gram <- hist(rnorm(100, mean=4, sd=2), 
  main="histogram of normal variables")  # end hist
target_vector <- histo_gram$density
target_vector <- rnorm(100, mean=4, sd=2)
# objective function is log-likelihood
object_ive <- function(parm, target) {
#  cat(c(parm[1], parm[2]), "\n")
#  -sum(log(max(dnorm(target, mean=parm[1], sd=parm[2]), 0.01)))
  sum(2*log(parm[2]) + ((target - parm[1])/parm[2])^2)
}  # end object_ive

blah <- (-20:60)/10
plot(x=blah, 
     y=sapply(blah, function(parm)
       object_ive(c(parm, 0.1), 
                  target_vector)),
     type="l")
blah <- (10:40)/10
plot(x=blah, 
     y=sapply(blah, function(parm)
       object_ive(c(4.0, parm), 
                  target_vector)),
     type="l")

# initial parameters
par_init <- c(m=0, s=2)

# perform optimization
optim_run <- optim(par=par_init, 
                   fn=object_ive, 
                   target=target_vector,
                   method="L-BFGS-B",
                   upper=c(10,10),
                   lower=c(-10,1))
str(optim_run)
optim_run$par
optim_run$convergence


### optimization example: fit mixture of normal variables

# target vector is histogram of mixture of normal distributions
target_vector <- c(rnorm(100, sd=1.0), rnorm(100, mean=4, sd=1.0))
histo_gram <- hist(target_vector, 
  main="mixture of normal distributions")  # end hist
target_vector <- histo_gram$density
# objective function is log-likelihood of mixture
object_ive <- function(parm, target) {
#     parm[1]*sum(2*log(parm[3]) + ((target - parm[2])/parm[3])^2) +
#     (1-parm[1])*sum(2*log(parm[5]) + ((target - parm[4])/parm[5])^2)
#   likelihood <- parm[1]/parm[3] * dnorm(target, mean=parm[2], sd=parm[3]) +
#     (1-parm[1])/parm[5] * dnorm(target, mean=parm[4], sd=parm[5])
  likelihood <- parm[1]/parm[3] * dnorm((target - parm[2])/parm[3]) +
    (1-parm[1])/parm[5] * dnorm((target - parm[4])/parm[5])
  if(any(likelihood <= 0))
    Inf
  else
    -sum(log(likelihood))
}  # end object_ive

blah <- (1:9)/10
plot(x=blah, 
     y=sapply(blah, function(parm)
       object_ive(c(parm, 0, 1.0, 4, 1.0), 
                  target_vector)),
     type="l")
blah <- (-20:20)/10
blah <- (5:40)/10
plot(x=blah, 
     y=sapply(blah, function(parm)
              object_ive(c(0.5, 1, parm, 4, 1), 
                  target_vector)),
     type="l")

# initial parameters
par_init <- c(weight=0.5, m1=0, s1=1, m2=4, s2=1)

# perform optimization
optim_run <- optim(par=par_init, 
            fn=object_ive, 
            target=target_vector,
            method="L-BFGS-B",
            upper=c(1,2,2,10,2),
            lower=c(0,-2,0.2,-1,0.2))
str(optim_run)
optim_run$par
optim_run$convergence


fit_func <- function(x, parm) {
  parm["weight"] * dnorm(x, mean=parm["m1"], sd=parm["s1"]) + 
    (1 - parm["weight"]) * dnorm(x, mean=parm["m2"], sd=parm["s2"])
}  # end fit_func

curve(expr=fit_func(x, parm=optim_run$par), add=TRUE,
      xlim=c(-5, 7), type="l", lwd=2, col="red")



########################
### portfolio optimization

### PortfolioAnalytics

library(PerformanceAnalytics)
library(TTR)
require(xts)
library(PortfolioAnalytics)
library(DEoptim)

# load ETF returns
load(file="C:/Develop/data/etf_analysis.RData")


### portfolio setup
portf_names <- c("VTI", "IEF", "DBC", "XLF", "VNQ", "XLP", "XLV", "XLU", "XLB", "XLE")
# portfolio equal weights
portf_init <- rep(0.1, length(portf_names))
names(portf_init) <- portf_names  # named vector
portf_init <- portfolio.spec(assets=portf_init)
# args(portfolio.spec)
str(portf_init)
portf_init$assets


########################
### visualize portfolio

plot_portf <- function(portfolio, rets_data=etf_rets) {
  portf_weights <- portfolio$weights
  portf_names <- names(portf_weights)
  # calculate xts of portfolio
  portf_max <- xts(rets_data[, portf_names] %*% portf_weights, 
                   order.by=index(rets_data))
  colnames(portf_max) <- "best portf"
  graph_params <- par(oma=c(1, 0, 1, 0), mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), cex.lab=0.8, cex.axis=1.0, cex.main=0.8, cex.sub=0.5)
  layout(matrix(c(1,2), 2), widths=c(1,1), heights=c(1,3))
  barplot(portf_weights, 
          names.arg=portf_names, 
          las=3, ylab="", 
          xlab="Symbol", main="")
  title(main="Loadings best portf", line=-1)
  chart.CumReturns(cbind(portf_max, rets_data[, c("IEF", "VTI")]), 
                   lwd=2, ylab="", legend.loc="topleft", 
                   main="")
  title(main="best portf, IEF, VTI", line=-1)
  par(graph_params)  # restore original parameters
  invisible(portf_max)
}  # end plot_portf



########################
### rbr presentation

### add constraints

# add constraint such that the portfolio weights sum to 0*
portf_init <- add.constraint(portf_init, type="weight_sum",
                           min_sum=-0.01, max_sum=0.01)
# add box constraint such that no asset can have a weight of greater than
# 20% or less than -20%
portf_init <- add.constraint(portf_init, type="box", min=-0.2, max=0.2)
# add constraint such that we have at most 20 positions
portf_init <- add.constraint(portf_init, type="position_limit", max_pos=20)
# add constraint such that the portfolio beta is between -0.25 and 0.25
betas <- t(CAPM.beta(etf_rets, market, Rf))
portf_init <- add.constraint(portf_init, type="factor_exposure", B=betas,
                           lower=-0.25, upper=0.25)

### add objectives

# add objective to maximize portfolio return with a target of 0.0015
portf_objective <- add.objective(portf_init, type="return", name="mean",
                                 target=0.0015)
# add objective to minimize portfolio StdDev with a target of 0.02
portf_objective <- add.objective(portf_objective, type="risk", name="StdDev",
                                 target=0.001)


########################
### optimize.portfolio using random portfolios

# generate random portfolios
portf_rand <- random_portfolios(portf_init, 10000, "sample")

# perform optimization using random portfolios
portf_optim <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_objective,
                             optimize_method="random", rp=portf_rand,
                             trace=TRUE)

plot(portf_optim, main="Dollar Neutral Portfolio", risk.col="StdDev", neighbors=10)

chart.EfficientFrontier(portf_optim, match.col="StdDev", n.portfolios=25, type="l")



########################
### optimize.portfolio maxSR

# Maximizing Sharpe Ratio can be formulated as a quadratic programming 
# problem and solved very quickly using optimize_method="ROI". Although "StdDev"
# was specified as an objective, the quadratic programming problem uses the 
# variance-covariance matrix in the objective function.

# The default action if "mean" and "StdDev" are specified as objectives with
# optimize_method="ROI" is to maximize quadratic utility. If we want to maximize
# Sharpe Ratio, we need to pass in maxSR=TRUE to optimize.portfolio.

# add constraints
portf_maxSR <- add.constraint(portfolio=portf_init, type="full_investment")
portf_maxSR <- add.constraint(portfolio=portf_maxSR, type="long_only")

# add objectives
portf_maxSR <- add.objective(portfolio=portf_maxSR, type="return", name="mean")
portf_maxSR <- add.objective(portfolio=portf_maxSR, type="risk", name="StdDev")


maxSR_ROI <- optimize.portfolio(
                R=etf_rets[, portf_names], 
                portfolio=portf_maxSR, 
                optimize_method="ROI", 
                maxSR=TRUE, 
                trace=TRUE)


# visualize
plot_portf(portfolio=maxSR_ROI)
chart.RiskReward(maxSR_ROI, risk.col="StdDev", return.col="mean")



########################
### optimize.portfolio maxSTARR

# add constraints
portf_maxSTARR <- add.constraint(portfolio=portf_init, type="full_investment")
portf_maxSTARR <- add.constraint(portfolio=portf_maxSTARR, type="long_only")

# add objectives
portf_maxSTARR <- add.objective(portfolio=portf_maxSTARR, type="return", name="mean")
portf_maxSTARR <- add.objective(portfolio=portf_maxSTARR, type="risk", name="ES",
                            arguments=list(p=0.925))

# perform optimization using ROI
maxSTARR_ROI <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_maxSTARR, 
                                      optimize_method="ROI",
                                      trace=TRUE)
maxSTARR_ROI

# visualize
plot_portf(portfolio=maxSTARR_ROI)
chart.RiskReward(maxSTARR_ROI, risk.col="ES", return.col="mean")


# for random portfolios and DEoptim, the leverage constraints should be relaxed slightly
portf_maxSTARR$constraints[[1]]$min_sum=0.9
portf_maxSTARR$constraints[[1]]$max_sum=1.1

# random portfolios optimization
maxSTARR_RP <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_maxSTARR, 
                                     optimize_method="random",
                                     search_size=2000,
                                     trace=TRUE)
maxSTARR_RP
# visualize
plot_portf(portfolio=maxSTARR_RP)
chart.RiskReward(maxSTARR_RP, risk.col="ES", return.col="mean")

# Use DEoptim to run the optimization.
maxSTARR_DEOpt <- optimize.portfolio(R=etf_rets[, portf_names], portfolio=portf_maxSTARR, 
                                     optimize_method="DEoptim",
                                     search_size=2000,
                                     trace=TRUE)
maxSTARR_DEOpt
chart.RiskReward(maxSTARR_DEOpt, risk.col="ES", return.col="mean")



########################
### efficient frontier (without optimize.portfolio)

# add constraints
portf_eff <- add.constraint(portfolio=portf_init, type="weight_sum", min_sum=0.9, max_sum=1.1)
portf_eff <- add.constraint(portfolio=portf_eff, type="box", min=-0.3, max=0.3)

# add objectives for mean-variance portfolio
portf_eff <- add.objective(portfolio=portf_eff, type="risk", name="var", risk_aversion=10)
portf_eff <- add.objective(portfolio=portf_eff, type="return", name="mean")

# Compute the mean-variance efficient frontier
efficient_front <- create.EfficientFrontier(R=etf_rets[, portf_names], portfolio=portf_eff, type="mean-StdDev")
chart.EfficientFrontier(efficient_front, match.col="StdDev", n.portfolios=5, type="l")

efficient_front
summary(efficient_front, digits=2)
# efficient_front$frontier

# calculate best portfolio on efficient frontier
mean_var <- efficient_front$frontier[, 1:2]
mean_var <- cbind(mean_var, mean_var[, 1]/mean_var[, 2])
colnames(mean_var) <- c(colnames(mean_var)[1:2], "SR")
portf_best <- which.max(mean_var[, "SR"])

# calculate xts of best portfolio
portf_weights <- efficient_front$frontier[portf_best, -(1:3)]
portf_max <- etf_rets[, portf_names] %*% portf_weights
portf_max <- xts(portf_max, order.by=index(etf_rets))
colnames(portf_max) <- "best portf"

barplot(portf_weights, 
        names.arg=portf_names, 
        las=3, ylab="Loadings", 
        xlab="Symbol", main="Loadings efficient frontier")

chart.CumReturns(cbind(portf_max, etf_rets[, c("IEF", "VTI")]), lwd=2, ylab="", legend.loc="topleft", main="best portf, IEF, VTI")


### charts of efficient frontier

# The RAR.text argument can be used for the risk-adjusted-return name on the 
# legend, by default it is 'Modified Sharpe Ratio'.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", 
                        RAR.text="Sharpe Ratio", pch=4)

# The tangency portfolio and line are plotted by default, these can be 
# ommitted by setting rf=NULL.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="b", rf=NULL)

# The tangency line can be omitted with tangent.line=FALSE. The tangent 
# portfolio, risk-free rate and Sharpe Ratio are still included in the plot.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", tangent.line=FALSE)

# The assets can be omitted with chart.assets=FALSE.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", 
                        tangent.line=FALSE, chart.assets=FALSE)

# Just the names of the assets can be omitted with labels.assets=FALSE and the 
# plotting character can be changed with pch.assets.
chart.EfficientFrontier(efficient_front, match.col="StdDev", type="l", 
                        tangent.line=FALSE, labels.assets=FALSE, pch.assets=1)

# Chart the asset weights along the efficient frontier.
chart.Weights.EF(efficient_front, colorset=bluemono, match.col="StdDev")

# Chart the group weights along the efficient frontier.
chart.Weights.EF(efficient_front, colorset=bluemono, by.groups=TRUE, match.col="StdDev")

# The labels for Mean, Weight, and StdDev can be increased or decreased with
# the cex.lab argument. The default is cex.lab=0.8.
chart.Weights.EF(efficient_front, colorset=bluemono, match.col="StdDev", main="", cex.lab=1)




### older

# Set the MAR parameter
MAR =.005 #~6%/year

# Example 1 maximize Sortino Ratio
SortinoConstr <- constraint(assets = colnames(indexes[,1:4]), min = 0.05, max = 1, min_sum=.99, max_sum=1.01, weight_seq = generatesequence(by=.001))
SortinoConstr <- add.objective(constraints=SortinoConstr, type="return", name="SortinoRatio",  enabled=TRUE, arguments = list(MAR=MAR))
SortinoConstr <- add.objective(constraints=SortinoConstr, type="return", name="mean",  enabled=TRUE, multiplier=0) # multiplier 0 makes it availble for plotting, but not affect optimization

# Use random portfolio engine
SortinoResult <- optimize.portfolio(R=indexes[,1:4], constraints=SortinoConstr, optimize_method='random', search_size=2000, trace=TRUE, verbose=TRUE)
plot(SortinoResult, risk.col='SortinoRatio')

# Alternately, Use DEoptim engine
#SortinoResultDE <- optimize.portfolio(R=indexes[,1:4], constraints=SortinoConstr, optimize_method='DEoptim', search_size=2000, trace=TRUE, verbose=FALSE,strategy=6, parallel=TRUE) #itermax=55, CR=0.99, F=0.5,
#plot(SortinoResultDE, risk.col='SortinoRatio')

# Now rebalance quarterly
SortinoRebalance <- optimize.portfolio.rebalancing(R=indexes[,1:4], constraints=SortinoConstr, optimize_method="random", trace=TRUE, rebalance_on='quarters', trailing_periods=NULL, training_period=36, search_size=2000)






#################################
### ggplot2 ####
#################################

library(car)
# qqPlot with t-quantiles
qqPlot(dax_rets, distribution="t", df=5, ylab="DAX Returns", envelope=FALSE)
# Box Plots




#################################
### ggplot2 ####
#################################


autoplot(object=ar_zoo,  # plot AR returns
         main="Autoregressive process (phi=0.2)", 
         facets=Series ~ .) + facet_grid(Series ~ ., scales="free_y") +
  xlab("") + ylab("") + 
  theme(legend.position=c(0.1, 0.5), 
        plot.title=element_text(vjust=-2.0), 
        plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
        plot.background=element_blank(),
        axis.text.y=element_blank())

autoplot(object=ar_zoo, 
         facets="Series ~ .", 
         main="Autoregressive process (phi=0.2)") + 
  facet_grid("Series ~ .", scales="free_y") +
  xlab("") + ylab("") + 
  theme(
    legend.position="none", 
    #  plot.title=element_text(vjust=-1.0), 
    #  plot.margin=unit(c(-0.5, 0.0, -0.5, 0.0), "cm"), 
    plot.background=element_blank(),
    axis.text.y=element_blank())


ar_data_frame <- as.data.frame(ar_zoo)
ggplot(data=ar_zoo, mapping=aes(x=index(ar_zoo), y=ar_zoo[, 2])) + geom_line()
ggplot(data=ar_data_frame, mapping=aes(x=rownames(ar_data_frame), y=ar_data_frame[, 2])) + geom_line()



###

# autoplot.zoo with some examples
# http://www.inside-r.org/packages/cran/zoo/docs/autoplot.zoo

x.Date <- as.Date(paste(2003, 02, c(1, 3, 7, 9, 14), sep="-"))
x <- zoo(rnorm(5), x.Date)
xlow <- x - runif(5)
xhigh <- x + runif(5)
z <- cbind(x, xlow, xhigh)

# univariate plot
autoplot(x)

# multivariate plotting in multiple or single panels
# multiple panels + no legend
autoplot(z)
# by rows + legend + series-dependent color/linetype
autoplot(z, facets="Series ~ .")
# by rows + no legend
autoplot(z, facets="Series ~ .") + theme(legend.position="none")
# by columns + no legend
autoplot(z, facets=". ~ Series") + theme(legend.position="none")
# single panel + no legend
autoplot(z, facets=NULL) + theme(legend.position="none")

autoplot(as.zoo(EuStockMarkets))

# point plot
autoplot(z, geom="point")
# plot with connected points
autoplot(z, facets=NULL) + geom_point()
# b&w plot
autoplot(z, facets=NULL) + scale_colour_grey() + theme_bw()

autoplot(z) +
aes(colour = NULL, linetype = NULL) + 
facet_grid("Series ~ .", scales = "free_y")



#################################
### download data ####
#################################

### scrape ETF ticker table using XML, qmao packages - from 2012, doesn't work now
# http://stackoverflow.com/questions/5246843/how-to-get-a-complete-list-of-ticker-symbols-from-yahoo-finance

# first find out how many ETFs there are, then construct a URL
etf_list <- readLines("http://finance.yahoo.com/etf/browser/mkt")
# Sorry for the ugly regex
etf_num <- gsub("^(\\w+)\\s?(.*)$", "\\1", 
                gsub("(.*)(Showing 1 - 20 of )(.*)", "\\3",
                     etf_list[grep("Showing 1 - 20", etf_list)]))
etf_url <- paste0("http://finance.yahoo.com/etf/browser/mkt?c=0&k=5&f=0&o=d&cs=1&ce=", etf_num)

library(XML)
etf_tbl <- readHTMLTable(etf_url, stringsAsFactors=FALSE)
etf_dat <- etf_tbl[[tail(grep("Ticker", etf_tbl), 1)]][-1, ]
colnames(etf_dat) <- etf_dat[1, ]
etf_dat <- etf_dat[-1, ]
etfs <- etf_dat$Ticker # All ETF tickers from yahoo
length(etfs)
head(etfs)

### end scrape ETF ticker table


### download database of stock tickers using stockSymbols() from package TTR - no ETFs
library(TTR)
# stock_table <- stockSymbols("NYSE")
stock_table <- stockSymbols()
write.csv(stock_table, file='stock_table.csv')


### ETF symbols - tickers for Tactical Asset Allocation System by Mebane Faber
sym_bols <- c("VTI", "VEU", "IEF", "VNQ", "DBC", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", "IWB", "IWD", "IWF", "IWM", "IWN", "IWO", "IWP", "IWR", "IWS", "IWV", "IUSV", "IUSG")

# read etf database into data frame
etf_list <- read.csv(file='etf_list.csv')
sym_bols %in% etf_list$Symbol
# subset etf_list to include only those ETF's in sym_bols
etf_list <- etf_list[etf_list$Symbol %in% sym_bols, ]


### download time series of "AdjClose" and "Volume" for single symbol
library(tseries)
sym_bol <- "MSFT"
field_names <- c("AdjClose", "Volume")
zoo_series <- suppressWarnings(  # load MSFT data
get.hist.quote(instrument=sym_bol, 
               quote=field_names,
               start=Sys.Date()-365, 
               end=Sys.Date(), 
               origin="1970-01-01")
)  # end suppressWarnings


# download data for list of symbols
sym_bols <- c("YHOO", "MSFT")
zoo_series <- suppressWarnings(
  lapply(sym_bols, # loop for loading data
         get.hist.quote,
         quote=field_names,
         start=Sys.Date()-365, 
         end=Sys.Date(), 
         origin="1970-01-01")
)  # end suppressWarnings
names(zoo_series) <- sym_bols

# flatten into a single zoo
zoo_series <- do.call(merge, zoo_series)
names(zoo_series) <- as.vector(sapply(sym_bols, paste, c("Close", "Volume"), sep="."))
head(zoo_series)

# write zoo to CSV file
write.zoo(zoo_series, file='zoo_series.csv', sep=",")
# save to binary file
save(zoo_series, file='zoo_series.Rdata')

### plot

etf_gg <- autoplot(zoo_series[, "VTI.Close"], 
                   main="Vanguard Total Stock Market ETF") + 
  xlab("") + ylab("") + 
  theme(
#  legend.position="none", 
  legend.position=c(0.1, 0.5), 
  plot.title=element_text(vjust=-2.0), 
#  plot.margin=unit(c(-0.0,0.0,-0.5,0.0),"cm"), 
#  axis.text.y=element_blank(),
  plot.background=element_blank()
)  # end theme
# render ggplot
etf_gg


###

tick_data <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=BAC&a=fM&b=fD&c=fY&d=M&e=D&f=Y", 
                 header=FALSE)

# version with anon function
zoo_series <- suppressWarnings(  # load MSFT data
  sapply(sym_bols, function(sym_bol, ...) {
    coredata(get.hist.quote(sym_bol, ...))
  },
         quote=field_names,
         start=Sys.Date()-365, 
         end=Sys.Date(), 
         origin="1970-01-01")
)  # end suppressWarnings



#################################
### analyzing some functions ####
#################################

### read numeric lines from input, and return them
fun_input <- function() {
  x <- readline("Enter the value of x: ")
  y <- readline("Enter the value of y: ")

  x <- as.numeric(unlist(strsplit(x, ",")))
  y <- as.numeric(unlist(strsplit(y, ",")))

  return(c(x, y))
}  # end fun_input


### read lines one by one, and return them
f_con <- file("stdin")
open(f_con)
while (length(line <- readLines(f_con, n=1)) > 0) {
  # process line
  write(line, stderr())
}  # end while
close(f_con)


### test deparse code
dep_fun <- function(arg_var) {
  my_var <- 2
  names(my_var) <- deparse(substitute(arg_var))
  my_var
}  # end dep_fun
dep_fun(hey)


### test sapply code
temp_fun <- function(data, stuff=1, some_stuff=2) {
  c(data, stuff, some_stuff)
}

in_data <- 5:9

# sapply binds "in_data" to "data", and binds remaining arguments either by name or position
sapply(in_data, temp_fun, stuff=2)
sapply(in_data, temp_fun, stuff=2, some_stuff=3)
sapply(in_data, temp_fun, 2, 3)


