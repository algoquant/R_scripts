##########################################
### Information Functions  ###############
##########################################

R.Version()
update.packages(ask=FALSE, checkBuilt=TRUE)

rm(list=ls())
options(max.print=40)

# required package loading inside a function
stopifnot(
  (("package:xts" %in% search()) || require("xts", quietly=TRUE))
  &&
    ("package:caTools" %in% search()) || require("caTools", quietly=TRUE)
)



################################################
### operator overloading  #######
################################################

# overloading "+" operator
# first define variables with explicit "character" class
# char1 <- "a"
# class(char1) <- "character"
# char2 <- "b"
# class(char2) <- "character"
# or
char1 <- structure("a", class="character")
char2 <- structure("b", class="character")

# define "+" method for "character" class
"+.character" <- function (a, b, ...) {
  in_dex <- (which(letters==substr(a, 1, 1)) + which(letters==substr(b, 1, 1))) %% length(letters)
  letters[in_dex]
}  # end +.character

# modified above to define "+" as "paste"
"+.character" <- function (a, b, ...) {
  paste(a, "plus", b)
}  # end +.character

# add two "character" objects
char1 + char2



################################################
### operator overwriting  #######
################################################

###  overwrite "+" operator
# http://stackoverflow.com/questions/4730551/making-a-string-concatenation-operator-in-r
"+" = function(a, b) {
  if(is.character(a) && is.character(b)) {
    paste(a, "plus", b)
  } else {
    .Primitive("+") (a, b)
  }
}



################################################
### Functions under development  #######
################################################


### check if variable exists in globalenv
is_exist <- function(stringy) {
  stringy %in% ls(globalenv())
}
is_exist("etf_rets")


# inspect function environment
peek_in <- function(in_var, name=NULL) {
  envy <- environment()
  if (is.null(name)) {
    return(ls(envir=envy))
  }
  if (name %in% ls(envir=envy)) {
    return(get(name, envy))
  }
  return(NULL)
}  # end peek_in
my_var <- 1
peek_in(my_var)
peek_in(my_var, name="n")


# copy named dots arguments into function environment
peek_in <- function (in_var, ...) {
  cat("top: ", ls(environment()), "\n")
  envy <- environment()
  for (var in names(list(...))) {
    assign(var, list(...)[[var]], envy)
  }
  cat("bottom: ", ls(environment()), "\n")
}  # end peek_in
peek_in(my_var)
peek_in(my_var, one=1, two=2, three=3)


### plot a few risk_ret_points in portfolio scatterplot
risk_ret_points <- function(rets=etf_rets,
        risk=c("sd", "ETL"), sym_bols=c("VTI", "IEF")) {
  risk <- match.arg(risk)  # match to arg list
  if (risk=="ETL") {
    stopifnot("package:PerformanceAnalytics" %in% search() ||
              require("PerformanceAnalytics", quietly=TRUE))
  }  # end if
  risk <- match.fun(risk)  # match to function
  risk_ret <- t(sapply(rets[, sym_bols],
     function(x_ts) c(ret=mean(x_ts), risk=abs(risk(x_ts)))))
  points(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
         col="red", lwd=3, pch=21)
  text(x=risk_ret[, "risk"], y=risk_ret[, "ret"],
       labels=rownames(risk_ret), col="red",
       lwd=2, pos=4)
}  # end risk_ret_points


match_arg <- function(risk=c("sd", "ETL")) {
  cat(match.arg(risk))
}



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
  func_tion <- match.fun(func.signal$filter.func)
# Calculate mean
  func.signal$filter.params[4] <- 0
  ts.zero <- xts(do.call(func_tion, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
  ts.zero[1,] <- na.omit(ts.zero)[1,]
  ts.zero <- na.locf(ts.zero)
# Calculate first derivative
  func.signal$filter.params[4] <- 1
  ts.first <- xts(do.call(func_tion, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
  ts.first[1,] <- na.omit(ts.first)[1,]
  ts.first <- na.locf(ts.first)
# Calculate second derivative
  func.signal$filter.params[4] <- 2
# Apply function and calculate signal
  ts.second <- xts(do.call(func_tion, append(list(coredata(ts.price)), func.signal$filter.params)), order.by=index(ts.price))
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
# higher-order functions


# Test: passing a function as argument, and passing its arguments as "..."
funcTestFunc <- function(func_tion, ...) {
  inputFunc <- match.fun(func_tion)
  inputFunc(...)
}


funcTestFunc <- function(inputFunc, ...) {
  inputFunc <- match.fun(func_tion)
  inputFunc(...)
}


# Test: Passing a function as a list, with the first element equal to the function name, and the remaing to function arguments
funcTestFunc <- function(input.list) {
  func_tion <- match.fun(input.list[1])
  do.call(func_tion, list(as.numeric(input.list[-1])))
}



### functions that return functions as their value

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


# function factory for power functions
factory_power <- function(n_exp) {  # wrapper function
  function(n_base) {  # anonymous closure
#    cat(ls(environment()), "\n")
    n_base^n_exp
  }
}  # end factory_power
f_square <- factory_power(2)
f_square(2)
f_cube <- factory_power(3)
f_cube(2)



#############
# test functions


### test deparse function
dep_fun <- function(arg_var) {
  my_var <- 2
  names(my_var) <- deparse(substitute(arg_var))
  my_var
}  # end dep_fun
dep_fun(hey)


### test function returning list
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


### test function accepting matrix
funcTest <- function(input)
  {
    sapply(1:(dim(input)[1]), function(row.ind) cat(input[row.ind,], sep="\t", "\n"))
  }


### test sapply
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


### test sapply
temp_fun <- function(data, stuff=1, some_stuff=2) {
  c(data, stuff, some_stuff)
}

in_data <- 5:9

# sapply binds "in_data" to "data", and binds remaining arguments either by name or position
sapply(in_data, temp_fun, stuff=2)
sapply(in_data, temp_fun, stuff=2, some_stuff=3)
sapply(in_data, temp_fun, 2, 3)


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


# test: return args - second function argument is optional
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
# out.put <- sapply(someSymbols, function(symb) { funEcho(symb); readline(prompt="Pause. Press <Enter> to continue...")  })

funcCropout <- function(in_put) { stopifnot(is.xts(in_put)); length(in_put) }

# switch using ifelse
switch_test <- function(in_put, op_tion="one") {
  var.out <- ifelse(op_tion=="one", {var.out <- in_put; var.out <- paste("first", var.out)},
                    ifelse(op_tion=="two", paste("second", in_put), "something else"))
  var.out
}  # end switch_test

# switch using switch
switch_test <- function(in_put, op_tion="one") {
  switch(op_tion,
         one=paste("first option:", in_put),
         two=paste("second option:", in_put),
         "1 min"=paste("1 min option:", in_put),
         paste("something else:", in_put)
  )  # end switch
}  # end switch_test
switch_test("hello")
switch_test(in_put="hello", op_tion="two")
switch_test(in_put="hello", op_tion="1 min")
switch_test(in_put="hello", op_tion="blah")

# Fix the first NA in a xts (it's snooping data)
na.init <- function(ts.data) {
  ts.data[1,] <- na.omit(ts.data)[1,]
  ts.data
}


# Calculate row and column of the extreme value of a matrix
# which(mat_rix==func_tion(mat_rix), arr.ind=TRUE) does the same
coordinates.matrix <- function(mat_rix, func_tion) {
  func_tion <- match.fun(func_tion)
  tmp <- which(mat_rix==func_tion(mat_rix), arr.ind=T)
  coordinates <- as.numeric(c(rownames(mat_rix)[tmp[1,1]], colnames(mat_rix)[tmp[1,2]]))
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
     xlab="", ylab="", freq=FALSE)
lines(density(ts.rets[, 1]), col='red', lwd=2)  # draw density
# title(main=ch.title, line=-1)  # add title



#############################################
# bank account example (from Venables) demonstrates mutable states
#############################################
# the formal argument 'balance' exists in the OpenAccount evaluation environment
# this allows 'balance' to be persistent between function calls
# the super-assignment operator '<<-' adjusts the balance
OpenAccount <- function(balance) {
# returns a list of functions that perform account operations
  list(

    deposit=function(amount) {
# make account deposit
      if(amount > 0) {
        balance <<- balance + amount  # '<<-' super-assignment operator
        cat(amount, "deposited. Your balance is now:", balance, "\n")
      } else {
        cat("Deposits must be positive!\n")
      }
    },  # end deposit

    withdraw=function(amount) {
# make account withdrawal
      if(amount <= balance) {
        balance <<- balance - amount  # '<<-' super-assignment operator
        cat(amount, "withdrawn. Your balance is now:", balance, "\n")
      } else {
        cat("You don't have that much money!\n")
      }
    },  # end withdraw

    get.balance=function() {
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

MySqrt <- function(arg_var) {  # function that throws error
  if (arg_var > 0) {
    sqrt(arg_var)
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
    message("error for: ", arg_var, "\nerror message: ", try.cond)
    return(NA)  # return NA on error
  }  # end error handler
}  # end error wrapper

WarningHandler <- function(try.cond) {
# wrapper for warning handler (returns a function)
  function(try.cond) {  # warning handler
    message("warning for: ", arg_var, "\nwarning message: ", try.cond)
    return(NULL)  # return NULL on warning
  }  # end warning handler
}  # end warning wrapper


# define wrapper function for tryCatch (wrapper needed for apply)
TrySqrt <- function(arg_var) {  # wrapper for tryCatch
  tryCatch(
    {  # expressions to be evaluated
      message("start processing: ", arg_var)
      sqrt(arg_var)
    },  # end expressions
    error=ErrorHandler(try.cond),  # error handler
    warning=WarningHandler(try.cond),  # warning handler
    finally=message("finished processing: ", arg_var)  # end finally
  )  # end tryCatch
}  # end wrapper


arg_vec <- c(2:6, -1, 7:10)
# run regular sqrt - loop stops on first error
for (arg_var in arg_vec) {
  print(paste("sqrt of", arg_var, "=", sqrt(arg_var)))
}
# run TrySqrt - loop continues after error
for (arg_var in arg_vec) {
  print(paste("sqrt of", arg_var, "=", TrySqrt(arg_var)))
}

# run tryCatch without wrapper function
for (arg_var in arg_vec) {
  try.sqrt <- tryCatch(
    expr={  # expressions to be evaluated
      message("start processing: ", arg_var)
      #      MySqrt(arg_var)
      sqrt(arg_var)
    },  # end expressions
    error=ErrorHandler(try.cond),  # error handler
    warning=WarningHandler(try.cond),  # warning handler
    finally=message("finished processing: ", arg_var)  # end finally
  )  # end tryCatch
  print(paste("sqrt of", arg_var, "=", try.sqrt))
}

# tryCatch with handlers using anonymous functions (without wrapper)
for (arg_var in arg_vec) {
  try.sqrt <- tryCatch(
    expr={  # expressions to be evaluated
      message("start processing: ", arg_var)
      #      MySqrt(arg_var)
      sqrt(arg_var)
    },  # end expressions
    error=  function(try.cond) {  # anonymous error handler
      message("error for: ", arg_var, "\nerror message: ", try.cond)
      return(NA)  # return NA on error
    },  # end error handler
    warning=function(try.cond) {  # anonymous warning handler
      message("warning for: ", arg_var, "\nwarning message: ", try.cond)
      return(NULL)  # return NULL on warning
    },  # end warning handler
    finally=message("finished processing: ", arg_var)  # end finally
  )  # end tryCatch
  print(paste("sqrt of", arg_var, "=", try.sqrt))
}



#############
### Rolling Functions

### Lag function performs a vector of lags on xts data when lag > 1
L <- function(ts.data, n.lag=1) {
  if(length(n.lag) > 1) {
    rval <- do.call("cbind", lapply(n.lag, lag.xts, x=ts.data))
    colnames(rval) <- n.lag
    }
  else
    rval <- lag(ts.data, n.lag)
  rval
}  # end L

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

# roll_sum() using RcppRoll::roll_sum()
# it's actua;;y slower than using cumsum() because reclass() back to xts is slow
library(RcppRoll)
roll_sum <- function(x_ts, win_dow) {
  roll_sum <- RcppRoll::roll_sum(c(rep(0,win_dow-1), coredata(x_ts)), n=win_dow, align="right")
  roll_sum <- xts(x=roll_sum, order.by=index(x_ts))
  colnames(roll_sum) <- colnames(x_ts)
  roll_sum
}  # end roll_sum


#############
### functions for package HighFreq

### modification of function to_period() from package HighFreq
#' Aggregates an \code{OHLC} time series to a lower periodicity, but uses
#' a sliding window (lookback period) instead of end points.
#'
#' Given an \code{OHLC} time series at high periodicity (say seconds),
#' calculates the \code{OHLC} prices at lower periodicity (say minutes).
#'
#' @export
#' @param x_ts an \code{xts} time series containing one or more columns of data.
#' @param period aggregation interval ("seconds", "minutes", "hours", "days",
#'   "weeks", "months", "quarters", and "years").
#' @param k number of periods to aggregate over (for example if period="minutes"
#'   and k=2, then aggregate over two minute intervals.)
#' @param end_points an integer vector of end points.
#' @return \code{xts} \code{OHLC} time series of lower periodicity defined by
#'   end_points.
#' @details #' Performs a similar aggregation as function \code{to.period()}
#'   from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}, but has
#'   the flexibility to aggregate to a user-specified vector of end points. The
#'   function \code{to_period_rolling()} simply calls the compiled function
#'   \code{toPeriod()} (from package
#'   \href{https://cran.r-project.org/web/packages/xts/index.html}{xts}), to
#'   perform the actual aggregations.  If \code{end_points} are passed in
#'   explicitly, then the \code{period} argument is ignored.
#' @examples
#' # define end points at 10-minute intervals (SPY is minutely bars)
#' end_points <- rutils::end_points(SPY["2009"], inter_val=10)
#' # aggregate over 10-minute end_points:
#' to_period_rolling(x_ts=SPY["2009"], end_points=end_points)
#' # aggregate over days:
#' to_period_rolling(x_ts=SPY["2009"], period="days")
#' # equivalent to:
#' to.period(x=SPY["2009"], period="days", name=rutils::na_me(SPY))

to_period_rolling <- function(x_ts, win_dow=10) {
  roll_open <- rutils::lag_xts(Op(x_ts), k=(win_dow-1))
  roll_hi <- TTR::runMax(Hi(x_ts), n=win_dow)
  roll_lo <- -TTR::runMax(-Lo(x_ts), n=win_dow)
  roll_close <- Cl(x_ts)
  roll_volume <- rutils::roll_sum(x_ts=Vo(x_ts), win_dow=win_dow)
  out_put <- cbind(roll_open, roll_hi, roll_lo, roll_close, roll_volume)
  out_put[1:(win_dow-1), ] <- 1
  colnames(out_put) <- colnames(x_ts)
  out_put
}  # end to_period_rolling



#' Adjusts an \code{OHLC} time series to make open prices equal to the close
#' prices from the previous period.
#'
#' @export
#' @param oh_lc an \code{OHLC} time series of prices in \code{xts} format.
#' @return \code{OHLC} time series of prices in \code{xts} format, with open
#'   prices equal to the close prices from the previous period.
#' @details Adds or subtracts a price adjustment to make all open prices equal
#'   to the close prices from the previous period.  The adjustment preserves the
#'   price differences within each bar of \code{OHLC} prices, and so preserves
#'   open to close returns, variance estimates, etc.
#' @examples
#' # define end points at 10-minute intervals (SPY is minutely bars)
#' end_points <- rutils::end_points(SPY["2009"], inter_val=10)
#' # aggregate over 10-minute end_points:
#' open_close(x_ts=SPY["2009"], end_points=end_points)
#' # aggregate over days:
#' open_close(oh_lc=SPY["2009"], period="days")

open_close <- function(oh_lc) {
  op_en <- Op(oh_lc)
  clo_se <- lag.xts(Cl(oh_lc), k=-1)
  which(!(op_en==clo_se))
}  # end open_close



################################################
### HF data aggregation and moment estimation  #
################################################

# compute beta coefficients from robust regressions
my.lmr.beta <- function (object, classic=FALSE) {
  if(class(object) != "lmRob")
    stop("Object must be of class 'lmRob'")
  model <- object$model
  num <- sapply(model, is.numeric)  # numeric vars only
  b <- object$coefficients[num][-1]  # final coefficients w/o intercept
  ## compute robust covariance
  covr <- NULL
  try(covr <- diag(covRob(model[num])$cov), silent=TRUE)
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

