#' Plot time series with possible one factor cointegration relationship
#'
#' tsdY = tsdX * coeff[-1] + coeff[1] + tsdDiff (matrix multiplication)
#' coeff are the coefficients from lm(tsdY ~ tsdX)
#'
#' @param tsdX xts time series with one or multiple columns
#' @param tsdY xts time series with one column
#' @param cum logical indicating whether to make the time series cumulative before doing any analysis or graphing
#' @param coeff numeric coefficients for tsdX in cointegration relationship
#' @param ... optional parameters passed on to plot.zoo
#' @note If tsdY is not provided then tsdY is assumed to tbe the first column of tsdX (and this column is removed from tsdX).
#' @note coeff will be calculated if missing or NULL
#' @return invisible list with calculation results
#' @export
tsdPlotCoint <- function(tsdXs, tsdY=NULL, cum=FALSE, plotGraph=TRUE, coeff, name, ...) {
  
  stopifnot(inherits(tsdXs, "xts") &&
              (is.null(tsdY) || (inherits(tsdY, "xts") && ncol(tsdY) == 1)))
  
  tsd <- na.trim(merge.xts(tsdY, tsdXs))
  
  if (cum){
    tsd <- cumsum(na.omit(tsd))
  }
  tsdY <- tsd[, 1]
  tsdXs <- tsd[, -1]
  
  if (missing(coeff) || is.null(coeff)) {
    coeff <- lm(tsdY ~ tsdXs)$coeff
    names(coeff) <- c(names(coeff)[1], colnames(tsdXs))
  }
  tsdX <- xts(tsdXs %*% coeff[-1], index(tsdXs)) + coeff[1]
  colnames(tsdX) <- "x"
  tsdErr <- tsdY - tsdX
  #symbolField(colnames(tsdErr)) <- "err"
  adfTeststat <- urca::ur.df(na.omit(tsdErr))@teststat
  
  ## only perform this function if producing graph
  if(plotGraph){
    formstr <- paste(paste(colnames(tsdY),
                           paste(round(coeff[-1], 2),
                                 colnames(tsdXs),
                                 sep=" * ",
                                 collapse=" + "),
                           sep=" = "),
                     round(coeff[1], 2),
                     "err",
                     sep=" + ")
    adfstr <- paste('adf teststat :', round(adfTeststat, 1))
    if (missing(name) || is.null(name))
      name <- paste(formstr, adfstr, sep="\n")
    else
      name <- paste(name, formstr, adfstr, sep="\n")
    
    theme <- chart_theme()
    theme$col$line.col  <-  c("red", "blue")
    chart_Series(merge(tsdY, tsdX), name=name, theme = theme)
    add_TA(tsdErr)
    quantmod:::add_Last()  
    plot(.chob)
  }
  
  invisible(list(tsdXs=tsdXs, tsdY=tsdY, tsdX=tsdX, tsdErr=tsdErr, coeff=coeff, adfTeststat=adfTeststat))
  
}

