
##########################################################################
#' Calculate the Sharpe, Sortino, or Calmar ratios for a time series of returns.
#'
#' @param \code{timeser} An \emph{time series} of returns with
#'   one or more columns.
#'
#' @param \code{method} A \emph{string} specifying the performance ratio to
#'   calculate: \code{"sharpe"}, \code{"sortino"}, or \code{"calmar"}
#'   (default is \code{"sharpe"}).
#'
#' @param \code{riskf} Annualized risk-free rate (default is \code{0}).
#'
#' @param \code{nperiods} Number of return periods per year used to scale
#'   \code{riskf} and annualize Calmar (default is \code{252}).
#'
#' @return A numeric vector containing the selected ratio for each column of
#'   \code{timeser}.
#'
#' @details
#'   \code{calc_sharpe()} calculates one of the following ratios:
#'   \itemize{
#'     \item \strong{Sharpe}: mean return divided by standard deviation.
#'     \item \strong{Sortino}: mean return divided by downside deviation,
#'       where downside returns are \eqn{\min(r_t, 0)}.
#'     \item \strong{Calmar}: mean return divided by the absolute maximum
#'       drawdown computed from cumulative wealth \eqn{\prod_t (1 + r_t)}.
#'   }
#'
#'   Missing values are removed column-by-column before calculation.
#'   If a denominator is zero (or data are empty), the function returns
#'   \code{NA_real_} for that column.
#'
#' @examples
#' \dontrun{
#' returns <- na.omit(rutils::etfenv$returns[, c("VTI", "XLF")])
#' HighFreq::calc_sharpe(returns, method="sharpe")
#' HighFreq::calc_sharpe(returns, method="sortino")
#' HighFreq::calc_sharpe(returns, method="calmar")
#' HighFreq::calc_sharpe(returns, "calmar")
#' }
#'
#' @export

calc_sharpe <- function(timeser, riskf = 0, nperiods = 252, method = "sharpe") {

  # Backward-compatible dispatch:
  # calc_sharpe(timeser, "calmar") should treat 2nd arg as method.
  if (is.character(riskf) && (length(riskf) == 1)) {
    method <- riskf
    riskf <- 0
  }

  method <- tolower(method)

  if (!(method %in% c("sharpe", "sortino", "calmar"))) {
    stop("method must be one of: 'sharpe', 'sortino', 'calmar'")
  }

  if ((!is.numeric(riskf)) || (length(riskf) != 1) || is.na(riskf)) {
    stop("riskf must be a single numeric value")
  }
  if ((!is.numeric(nperiods)) || (length(nperiods) != 1) || is.na(nperiods) || (nperiods <= 0)) {
    stop("nperiods must be a single positive numeric value")
  }

  riskf_per_period <- riskf / nperiods

  riskv <- switch(
    method,
    "sharpe" = {
      sapply(timeser, function(x) {
        x <- stats::na.omit(as.numeric(x))
        if (length(x) == 0) return(NA_real_)
        x <- x - riskf_per_period
        volatility <- stats::sd(x)
        if ((is.na(volatility)) || (volatility == 0)) return(NA_real_)
        mean(x) / volatility
      })
    },
    "sortino" = {
      sapply(timeser, function(x) {
        x <- stats::na.omit(as.numeric(x))
        if (length(x) == 0) return(NA_real_)
        x <- x - riskf_per_period
        downside_dev <- sqrt(mean(pmin(x, 0)^2))
        if ((is.na(downside_dev)) || (downside_dev == 0)) return(NA_real_)
        mean(x) / downside_dev
      })
    },
    "calmar" = {
      sapply(timeser, function(x) {
        x <- stats::na.omit(as.numeric(x))
        if (length(x) == 0) return(NA_real_)
        x <- x - riskf_per_period
        wealth <- cumprod(1 + x)
        running_peak <- cummax(wealth)
        drawdowns <- wealth / running_peak - 1
        max_drawdown <- abs(min(drawdowns, na.rm = TRUE))
        if ((is.na(max_drawdown)) || (max_drawdown == 0)) return(NA_real_)
        nperiods * mean(x) / max_drawdown
      })
    }
  )

  return(riskv)

}  # end calc_sharpe


