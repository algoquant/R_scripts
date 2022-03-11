
######################################################################
#' Download an \emph{OHLC} time series of prices from Polygon.
#'
#' @export
#' @param \code{symbol} The stock symbol (ticker).
#' 
#' @param \code{startd} The start date (default is "1997-01-01").
#'   
#' @param \code{endd} The end date (default is \code{Sys.Date()}).
#'   
#' @param \code{tspan} The data frequency, i.e. the time span for data
#'   aggregations (default is "day" for daily data).
#'   
#' @param \code{apikey} The API key issued by Polygon.
#'   
#' @return An \emph{OHLC} time series of class \emph{xts}.
#'
#' @details The function \code{getpoly()} downloads historical prices from
#'   \href{https://polygon.io}{Polygon}, and returns an \emph{OHLC} time series
#'   of class \emph{xts}.
#'
#'   \href{https://polygon.io}{Polygon} is a provider of live and historical
#'   prices for stocks, options, foreign currencies, and cryptocurrencies.
#'   
#'   The function \code{getpoly()} sends a request for data to the
#'   \href{https://polygon.io}{Polygon} rest API, using the function
#'   \code{read_json()} from package
#'   \href{https://cran.r-project.org/web/packages/jsonlite/index.html}{jsonlite}.
#'   The query requires an API key issued by \href{https://polygon.io}{Polygon}.
#'   The API key must be passed to the argument \code{apikey}.
#'   
#'   \href{https://polygon.io}{Polygon} returns data in \emph{JSON} format,
#'   which is then formatted into an \emph{OHLC} time series of class
#'   \emph{xts}.
#'   
#'   The argument \code{tspan} determines the data frequency, i.e. it's the time
#'   span for data aggregations.  The default is "day" for daily data.  Other
#'   possible values of \code{tspan} are "minute", "hour", "week", and "month".
#'
#' @examples
#' \dontrun{
#' # Polygon API key - user must obtain their own key
#' apikey <- "0Q2f7j8CwAbdY5M8VYt_8pwdP0V4TunxbvRVC_"
#' # Download SPY prices from Polygon
#' ohlc <- rutils::getpoly(symbol="SPY", apikey=apikey)
#' # Plot candlesticks of SPY OHLC prices
#' library(highcharter)
#' highcharter::highchart(type="stock") %>% hc_add_series(ohlc, type="candlestick")
#' }

getpoly <- function(symbol="SPY", startd=as.Date("1997-01-01"), endd=Sys.Date(), tspan="day", apikey) {
  
  # Create url for download
  urll <- "https://api.polygon.io/v2/aggs/ticker/"
  urll <- paste0(urll, symbol, "/range/1/", tspan, "/", startd, "/", endd, 
                 "?adjusted=true&sort=asc&limit=50000&apiKey=", apikey)
  # Download JSON format data with OHLC bars
  ohlc <- jsonlite::read_json(urll)
  # Extract list of prices from JSON object
  ohlc <- ohlc$results
  # Coerce from list to matrix
  ohlc <- lapply(ohlc, function(x) unlist(x)[c("t","o","h","l","c","v","vw")])
  ohlc <- do.call(rbind, ohlc)
  # Coerce time from milliseconds to date-time
  dates <- ohlc[, "t"]/1e3
  dates <- as.POSIXct(dates, origin="1970-01-01")
  # Coerce from matrix to xts
  ohlc <- ohlc[, -1]
  colnames(ohlc) <- c("Open", "High", "Low", "Close", "Volume", "VWAP")
  # Return xts
  xts::xts(ohlc, order.by=dates)
  
}  # end getpoly


