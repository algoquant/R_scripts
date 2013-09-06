################################################
### Miscelaneous Data Functions  ###############
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


