##########################################
### Information Functions  ###############
##########################################

R.Version()
update.packages(ask=FALSE, checkBuilt=TRUE)



################################################
### Miscelaneous Data Loading Functions  #######
################################################


### Load CDS price and bid-offer ticks for a single symbol
file.data <- "S:/Data/R_Data/Time_Series.RData"


# write.table()

# read.table()

# save(blah, file=file.data)
# load(file=file.data)

# Save a single R object to a file
# saveRDS(blah, file=file.data)

# Load data from a .RData file into test environment
# test.env <- new.env()
# load(file=file.data, envir=test.env)
# ls(test.env)


### script for reading panel data from *.csv file and creating xts object
# panel data consists of prices by ticker symbol and dates

# read into data frame
df.data <- read.csv(file='sampleJ.csv', stringsAsFactors=FALSE)

# convert dates to POSIX
df.data[,"date"] <- as.POSIXct(df.data[,"date"], format="%m/%d/%Y")

# sort by date
# df.data <- df.data[order(df.data[,"date"]), ]
# sort by ticker then by date
df.data <- df.data[order(df.data[,"ticker"], df.data[,"date"]), ]

# calculate returns
# the returns for the first date should be disregarded
df.data[,"return"] <- c(0, diff(log(df.data[, "price"])))

# more code to create list of xts
# get all ticker names
ticker.names <- unique(df.data[, "ticker"])

# extract matrices by ticker name - creates list of matrices
list.data <- apply(as.matrix(ticker.names), 1, 
                   function(ticker.name) 
                     df.data[df.data[, "ticker"] == ticker.name, c("date", "price")]
)  # end apply
names(list.data) <- ticker.names

# extract matrices by ticker name - creates list of smaller matrices
list.data <- apply(as.matrix(ticker.names), 1, 
                   function(ticker.name) 
                     df.data[df.data[, "ticker"] == ticker.name, c("date", "price")]
)  # end apply
names(list.data) <- ticker.names

# extract matrices by ticker name - creates list of xts
list.data <- apply(as.matrix(ticker.names), 1, 
                   function(ticker.name) {
                     xts.data <- df.data[df.data[, "ticker"] == ticker.name, c("date", "price")]
                     xts.data <- xts(x=xts.data[, "price"], order.by=xts.data[, "date"])
                     colnames(xts.data) <- ticker.name
                     xts.data
                   }
)  # end apply
names(list.data) <- ticker.names


# convert list of matrices to one big matrix
matrix.data <- do.call(rbind, list.data)
rownames(matrix.data) <- NULL


####

cbind.SavGol <- function(ts.price, func.signal) {
# Extract function name
  func.name <- match.fun(func.signal$filter.func)
# Calculate mean
  func.signal$filter.params[4] <- 0
  ts.zero <- xts(do.call(func.name, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
  ts.zero[1,] <- na.omit(ts.zero)[1,]
  ts.zero <- na.locf(ts.zero)
# Calculate first derivative
  func.signal$filter.params[4] <- 1
  ts.first <- xts(do.call(func.name, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
  ts.first[1,] <- na.omit(ts.first)[1,]
  ts.first <- na.locf(ts.first)
# Calculate second derivative
  func.signal$filter.params[4] <- 2
# Apply function and calculate signal
  ts.second <- xts(do.call(func.name, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
  ts.second[1,] <- na.omit(ts.second)[1,]
  ts.second <- na.locf(ts.second)

  out.put <- cbind(ts.price, ts.zero, ts.first, ts.second)
  colnames(out.put) <- c(colnames(ts.price), "Mean", "First derivative", "Second derivative")
  out.put

}
# End cbind.SavGol


# Plot multiple time series in separate panels using chart_Series
plot.ts <- function(chart.data) {

  par(mfrow=c(dim(chart.data)[2],1))

  sapply(1:(dim(chart.data)[2]), function(n.col) plotSeries(chart.data[,n.col], name.plot=paste(colnames(chart.data[,n.col]), "/", date())))

}
# End plot.ts


# Plot xts object, similar way to chart_Series
#' @export
chart.Series <- function(ts.data, name.plot) {

  stopifnot(inherits(ts.data, "xts"))

# Set up chart
  plot(range(index(ts.data)), range(ts.data), type="n", xlab="Date")

  sapply(1:(dim(ts.data)[2]), function(n.col) lines(ts.data[,n.col]))

}
# End chart.Series


#############
### Toy Functions
#############

# Toy function returning list
funcTest <- function(input)
  {
    a <- input+1
    b <- input+2
    output <- list()
#  return(output)
#  output <- c(a,b)
    output$a <- a
    output$b <- b
#  names(output) <- c("a","b")
#    output
    lapply(output, function(outp) cat(outp, ","))
  }

# Toy function accepting matrix
funcTest <- function(input)
  {
    sapply(1:(dim(input)[1]), function(row.ind) cat(input[row.ind,], sep="\t", "\n"))
  }


# Testing sapply
funcSapplyTest <- function(input, ...) {
#    output <- 1
#    rolls <- 17:18
  table.symbols <- read.table(input, sep=",", header=TRUE)

  output <- sapply(table.symbols[,1], 
                   function(symbol) {
                     symbol
#                     ts.price <- loadCDS(symbol, ...)
#                     input+roll
                   }
                   )
  output
}


# Test: passing a function as argument, and passing its arguments as "..."
funcTestFunc <- function(func.name, ...) {
  inputFunc <- match.fun(func.name)
  inputFunc(...)
}


funcTestFunc <- function(inputFunc, ...) {
  inputFunc <- match.fun(func.name)
  inputFunc(...)
}


# Test: Passing a function as a list, with the first element equal to the function name, and the remaing to function arguments
funcTestFunc <- function(input.list) {
  func.name <- match.fun(input.list[1])
  do.call(func.name, list(as.numeric(input.list[-1])))
}


# Test: Returning a function
# from http://www.markmfredrickson.com/thoughts/2011-02-06-peeking-inside-r-functions.html
funcAdder <- function(n) {
  function(i) {
    n+i
  }
}

# Examples:
# f1 <- funcAdder(7)
# f2 <- funcAdder(3)
# f1(10)
# f2(10)


# Test: Return args - second function argument is optional
funcEcho <- function(input1, input2=NULL) {
  if (is.null(input2))
    input1
  else
    input1+input2
}

# Test scoping of variable assignments
funcScope <- function(model=NULL) {
  mdl$ts$positions <<- c(10,11)
}

# Some misc code
# out.put <- sapply(someSymbols, function(symb) { funEcho(symb); readline(prompt = "Pause. Press <Enter> to continue...")  })

funcCropout <- function(var.input) { stopifnot(is.xts(var.input)); length(var.input) }


funcSwitch <- function(var.input, var.type="one") {
  var.out <- ifelse(var.type=="one", {var.out <- var.input; var.out <- paste("first", var.out)},
                    ifelse(var.type=="two", paste("second", var.input), "something else"))
  var.out
}

# Fix the first NA in a xts (it's snooping data)
na.init <- function(ts.data) {
  ts.data[1,] <- na.omit(ts.data)[1,]
  ts.data
}

# Calculate row and column of the extreme value of a matrix
coordinates.matrix <- function(var.matrix, func.matrix) {
  func.name <- match.fun(func.matrix)
  tmp <- which(var.matrix==func.name(var.matrix), arr.ind=T)
  coordinates <- as.numeric(c(rownames(var.matrix)[tmp[1,1]], colnames(var.matrix)[tmp[1,2]]))
  coordinates
}


### Recursive functions

# Recursive Fibonacci sequence
FibRec <- function(n.num) {
  if (n.num > 2) {
    fib.seq <- FibRec(n.num-1)  # recursion
    c(fib.seq, sum(tail(fib.seq, 2)))
  } else {
    c(1, 1)  # initialize
  }
}  # end FibRec
FibRec(10)
tail(FibRec(10), 2)

# Recursive sum using dots
SumDots <- function(n.var, ...) {
  if (length(list(...)) == 0) {
    return(n.var)
  } else {
    n.var + SumDots(...)
  }
}  # end SumDots


# Function that returns another function as its value
FuncPower <- function(n.exp) {
  function(n.arg) {
    n.arg^n.exp
  }
}
FuncSquare <- FuncPower(2)
FuncCube <- FuncPower(3)
FuncSquare(4)
FuncCube(2)


# pseudo-random function
MyRandom <- function(seed) {  # seed must be an integer
# Returns pseudo-random generating function based on logistic map
# the formal argument 'seed' exists in the evaluation environment of MyRandom
  pseudo.random <- as.numeric(paste('0.', seed, sep=''))  # initialize
#  pseudo.random <- seed/4  # initialize
#  cat("ls()= ", ls(), c(seed=seed, pseudo.random=pseudo.random))
  NewRandom <- function(n.rand=1) {  # assign function name for recursion
    pseudo.random <<- 4*pseudo.random*(1 - pseudo.random)
    if(n.rand == 1) {
      return(pseudo.random)
    } else {
      return(c(pseudo.random, NewRandom(n.rand - 1)))
    }
  }
}  # end MyRandom

# now run MyRandom:
# seed the pseudo-random function
PseudoRandom <- MyRandom(88)
# plot histogram of pseudo-random numbers
hist(PseudoRandom(500), breaks=30, main="Poor quality pseudo-random numbers", xlim=c(0.0, 1.0), 
     xlab="", ylab="", freq = FALSE)
lines(density(ts.rets[, 1]), col='red', lwd=2)  # draw density
# title(main=ch.title, line=-1)  # add title


# bank account example (from Venables) demonstrates mutable states
# the formal argument 'balance' exists in the OpenAccount evaluation environment
# this allows 'balance' to be persistent between function calls
# the super-assignment operator '<<-' adjusts the balance
OpenAccount <- function(balance) {
# returns a list of functions that perform account operations
  list(

    deposit = function(amount) {
# make account deposit
      if(amount > 0) {
        balance <<- balance + amount  # '<<-' super-assignment operator
        cat(amount, "deposited. Your balance is now:", balance, "\n")
      } else {
        cat("Deposits must be positive!\n")
      }
    },  # end deposit

    withdraw = function(amount) {
# make account withdrawal
      if(amount <= balance) {
        balance <<- balance - amount  # '<<-' super-assignment operator
        cat(amount, "withdrawn. Your balance is now:", balance, "\n")
      } else {
        cat("You don't have that much money!\n")
      }
    },  # end withdraw

    get.balance = function() {
# get account balance
      cat("Your current balance is:", balance, "\n")
    }  # end get.balance

  )  # end list

}  # end OpenAccount

# perform account operations
my.account <- OpenAccount(100)  # open an account with 100 deposit
attach(my.account)  # add my.account to search path
withdraw(30)  # withdraw from account to buy groceries
deposit(100)  # deposit paycheck to account
withdraw(200)  # withdraw from account to buy Gucci bag
get.balance()  # get account balance

ls(environment(get.balance))  # list objects in scope of get.balance

detach(my.account)  # remove my.account from search path


#############
### Exception handling
#############

MySqrt <- function(n.inp) {  # function that throws error
  if (n.inp > 0) {
    sqrt(n.inp)
  } else {
    stop('bad input!')  # throw error
  }
}

# set option to turn warnings into errors
options(warn=2)

stop('my error')  # stop and produce error condition

args(tryCatch)  # get arguments of tryCatch()

tryCatch(  # without error handler
  {  # evaluate expressions
    n.val <- 101  # assign
    stop('my error')  # throw error
  }, 
  finally=print(paste("n.val=", n.val))
)  # end tryCatch

tryCatch(  # with error handler
  {  # evaluate expressions
    n.val <- 101  # assign
    stop('my error')  # throw error
  }, 
  error=function(e.cond)  # handler captures error condition
    print(paste("error handler: ", e.cond)),
  finally=print(paste("n.val=", n.val))
)  # end tryCatch

# apply loop without tryCatch
apply(as.matrix(1:5), 1, function(n.val) {  # anonymous function
  stopifnot(n.val != 3)  # check for error
  cat("(cat) n.val=", n.val)  # broadcast
  paste("(return) n.val=", n.val)  # return value
}  # end anonymous function
)  # end apply

# apply loop with tryCatch
apply(as.matrix(1:5), 1, function(n.val) {  # anonymous function
  tryCatch(  # with error handler
{  # body
  stopifnot(n.val != 3)  # check for error
  cat("(cat) n.val=", n.val)  # broadcast
  paste("(return) n.val=", n.val)  # return value
},
error=function(e.cond)  # handler captures error condition
  paste("handler: ", e.cond),
finally=print(paste("(finally) n.val=", n.val))
  )  # end tryCatch
}  # end anonymous function
)  # end apply

# this loop throws error with little information
for (my.index in 1:10) {  # loop
  stopifnot(my.index < 6)
}  # end for

for (my.index in 1:10) {  # loop with tryCatch
  tryCatch(stopifnot(my.index < 6), 
           error=function(e) print(paste("my.index=", my.index, "error: ", e)),
           finally=print(paste("my.index=", my.index)))
}  # end for

for (my.index in 1:10) {  # loop with tryCatch
  tryCatch(stopifnot(my.index < 6), 
           error=function(e) print(paste("my.index=", my.index, "error: ", e)))
}  # end for


ErrorHandler <- function(try.cond) {
# wrapper for error handler (returns a function)
  function(try.cond) {  # error handler
    message("error for: ", n.inp, "\nerror message:", try.cond)
    return(NA)  # return NA on error
  }  # end error handler
}  # end error wrapper

WarningHandler <- function(try.cond) {
# wrapper for warning handler (returns a function)
  function(try.cond) {  # warning handler
    message("warning for: ", n.inp, "\nwarning message: ", try.cond)
    return(NULL)  # return NULL on warning
  }  # end warning handler
}  # end warning wrapper


# define wrapper function for tryCatch (wrapper needed for apply)
TrySqrt <- function(n.inp) {  # wrapper for tryCatch
  tryCatch(
    {  # expressions to be evaluated
      message("start processing: ", n.inp)
      sqrt(n.inp)
    },  # end expressions
    error=ErrorHandler(try.cond),  # error handler
    warning=WarningHandler(try.cond),  # warning handler
    finally=message("finished processing: ", n.inp)  # end finally
  )  # end tryCatch
}  # end wrapper


v.inp <- c(2:6, -1, 7:10)
# run regular sqrt - loop stops on first error
for (n.inp in v.inp) {
  print(paste("sqrt of", n.inp, "=", sqrt(n.inp)))
}
# run TrySqrt - loop continues after error
for (n.inp in v.inp) {
  print(paste("sqrt of", n.inp, "=", TrySqrt(n.inp)))
}

# run tryCatch without wrapper function
for (n.inp in v.inp) {
  try.sqrt <- tryCatch(
    expr={  # expressions to be evaluated
      message("start processing: ", n.inp)
      #      MySqrt(n.inp)
      sqrt(n.inp)
    },  # end expressions
    error=ErrorHandler(try.cond),  # error handler
    warning=WarningHandler(try.cond),  # warning handler
    finally=message("finished processing: ", n.inp)  # end finally
  )  # end tryCatch
  print(paste("sqrt of", n.inp, "=", try.sqrt))
}

# tryCatch with handlers using anonymous functions (without wrapper)
for (n.inp in v.inp) {
  try.sqrt <- tryCatch(
    expr={  # expressions to be evaluated
      message("start processing: ", n.inp)
      #      MySqrt(n.inp)
      sqrt(n.inp)
    },  # end expressions
    error=  function(try.cond) {  # anonymous error handler
      message("error for: ", n.inp, "\nerror message:", try.cond)
      return(NA)  # return NA on error
    },  # end error handler
    warning=function(try.cond) {  # anonymous warning handler
      message("warning for: ", n.inp, "\nwarning message: ", try.cond)
      return(NULL)  # return NULL on warning
    },  # end warning handler
    finally=message("finished processing: ", n.inp)  # end finally
  )  # end tryCatch
  print(paste("sqrt of", n.inp, "=", try.sqrt))
}


#############
### Rolling Functions
#############

### Lag function for xts data when lag > 1
L <- function(ts.data, n.lag=1) {
  if(length(n.lag) > 1)
    {
      rval <- do.call("cbind", lapply(n.lag, lag.xts, x=ts.data))
      colnames(rval) <- n.lag
    }
  else
    {
      rval <- lag(ts.data, n.lag)
    }
  rval
}

###' @export
rollingLmSignals <- function(ts.returns, end.period, look.back, lags, expand.window=FALSE) {

# Type checking
  stopifnot(inherits(ts.returns, "xts") && !any(is.na(ts.returns)))

# Calculate endpoints
  end.points <- if(inherits(end.period, "numeric"))
    seq(1, nrow(ts.returns), end.period)
  else
    tail(endpoints(ts.returns, end.period), -1)

  stopifnot(look.back < length(end.points)-1)

# Initialize Data
  cum.returns <- cumsum(ts.returns)
  colnames(cum.returns) <- paste("cum", toupper(substr(colnames(ts.returns), 1, 1)), substr(colnames(ts.returns), 2, 1000), sep="")
  signals <- NA * ts.returns[,1]
  coef <- NULL
  reg.lvl <- NULL
  reg.ect <- NULL
  nameY <- colnames(cum.returns)[1]
  namesX <- colnames(cum.returns)[-1]
  form.lvl <- paste(nameY, paste(namesX, collapse=" + "), sep=" ~ ")
  print(paste("levels regression:", form.lvl))
# Define regression formula
  form.ect <- paste(paste("lag.xts(ect, -1)", sep=""),
                    paste("L(", c(namesX, "ect"),", 0:", lags-1, ")", sep="", collapse=" + "),
                    sep=" ~ ")
  print(paste("ect regression:", form.ect))

# Main loop over endpoints
  for (end.point in (look.back+1):(length(end.points)-1)) {
    print(paste("endpoint", end.point, "datetime", index(cum.returns)[now.point]))

# Find calc window
    if (expand.window)
      {
        calc.window <- 1:now.point
        calc.window.plus <- 1:(now.point+1)
      }
    else
      {
        calc.window <- (end.points[end.point-look.back]+1):now.point
        calc.window.plus <- (end.points[end.point-look.back]+1):(now.point+1)
      }

    window.data <- cum.returns[calc.window,]
    window.data.plus <- cum.returns[calc.window.plus,]
    if (is.null(reg.lvl) )
      {
# Perform initial regressions if start of loop
        reg.lvl <- lm(form.lvl, data=window.data)
        ectFit <- xts(data.frame(ect=residuals(reg.lvl)), index(window.data))
        reg.ect <- lm(form.ect, data=merge.xts(window.data.plus, ectFit))
      }
    else
      {
# Update regressions with extra period of data
        reg.lvl <- update(reg.lvl, data=window.data)
        ectFit <- xts(data.frame(ect=residuals(reg.lvl)), index(window.data))
        reg.ect <- update(reg.ect, data=merge(window.data.plus, ectFit))
      }

# Extented window
    if (expand.window)
      indicesExt <- 1:end.points[end.point+1]
    else
## learn window+predict window
      indicesExt <- (end.points[end.point-look.back]+1):end.points[end.point+1]

# Extrapolate (predict) over next period using regression coefficients
    dataExt <- cum.returns[indicesExt,]

# YectExt <- -xts(data.frame(ect=predict(reg.lvl, dataExt)), index(dataExt))+dataExt[,1]
# Calculate the signal as the difference between the extrapolated (predicted) value minus the actual value

#signal= -Res(t)
    signalsExt <- predict(reg.ect, merge(dataExt, ectExt))-ectExt

# Predict window
# Append predicted signals and regression coefficients
    indicesPred <- (now.point+1):end.points[end.point+1]
    signals[indicesPred] <- tail(signalsExt, length(indicesPred))
    coefPred <- c(coef(reg.lvl), coef(reg.ect))
    if (is.null(coef)) {
      coef <- xts(do.call("cbind", rep(list(coredata(NA * ts.returns[,1])), length(coefPred))), index(ts.returns))
      colnames(coef) <- names(coefPred)
    }
    coef[now.point,] <- coefPred

  }
# End loop over endpoints

# Prepare and return data
  coef <- na.locf(coef)
  betas <- -coef[, 1:ncol(ts.returns)]
  betas[,1] <- 1
  colnames(betas) <- colnames(ts.returns) 
  list(signals=signals, coef=coef, betas=betas, reg.lvl=reg.lvl, reg.ect=reg.ect)

}
### End rollingLmSignals


################################################
### HF data aggregation and moment estimation  #
################################################

# compute beta coefficients from robust regressions
my.lmr.beta <- function (object, classic = FALSE) {
  if(class(object) != "lmRob")
    stop("Object must be of class 'lmRob'")
  model <- object$model
  num <- sapply(model, is.numeric)  # numeric vars only
  b <- object$coefficients[num][-1]  # final coefficients w/o intercept
  ## compute robust covariance
  covr <- NULL
  try(covr <- diag(covRob(model[num])$cov), silent = TRUE)
  if(is.null(covr) & classic == FALSE)
    warning("covRob() coud not be computed, instead covClassic() was applied.")
  ## compute classic covariance if robust failed
  if(is.null(covr) | classic == TRUE)
    covr <- diag(covClassic(model[num])$cov)
  sx <- sqrt(covr[-1])  # standard deviation of x's
  sy <- sqrt(covr[1])  # standard deviation of y
  beta <- b * sx/sy
  return(beta)
}  # End my.lmr.beta
