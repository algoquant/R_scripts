#!/usr/bin/env Rscript

# Download account-level minute PnL snapshots for 2026-09-23 through 2026-10-02.
# These are Alpaca's combined realized/unrealized account PnLs, not per-stock
# or per-strategy PnLs. The API resets the PnL baseline to the previous trading
# day's closing equity each day. profit_loss_pct is a fraction, not percent.
# pnl_change_1min is the difference between consecutive same-day minute PnLs;
# it is NA at the first snapshot of each day and after gaps or missing values.
# Timestamps are left-labeled by Alpaca and exported in America/New_York.
# The range is downloaded in five-day chunks because Alpaca rejects 1Min
# resolution for requests spanning more than seven days.
#
# Install dependencies once:
#   install.packages("httr2")
#
# Set ALPACA_TRADE_KEY and ALPACA_TRADE_SECRET in the environment or an env file.
# Never put the actual credentials in this script.
# Run:
#   Rscript /Users/jerzy/Develop/R/alpaca_pnls.R
#   Rscript /Users/jerzy/Develop/R/alpaca_pnls.R /path/to/.env
#
# Optional environment settings:
#   ALPACA_BASE_URL=https://paper-api.alpaca.markets (default, paper account)
#   ALPACA_BASE_URL=https://api.alpaca.markets       (live account)
#   ALPACA_INTRADAY_REPORTING=extended_hours        (default)
#     Alternatives: market_hours or continuous.
#   ALPACA_PNLS_OUTPUT=/path/to/output.csv
# Default output: alpaca_pnls_20260923_20261002.csv beside this script.
# Existing output files are not overwritten. Choose another output path to rerun.
# API documentation:
#   https://docs.alpaca.markets/reference/getaccountportfoliohistory-1

history_to_frame <- function(history, start_date, end_date) {
  if (!identical(history$timeframe, "1Min")) {
    stop("Alpaca did not return the requested 1Min resolution.")
  }
  timestamp <- as.numeric(unlist(history$timestamp))
  if (!length(timestamp)) stop("Alpaca returned no portfolio history.")
  if (any(!is.finite(timestamp))) stop("Invalid portfolio timestamps.")
  numeric_column <- function(name) {
    values <- history[[name]]
    if (length(values) != length(timestamp)) {
      stop("Unexpected length for portfolio history field: ", name)
    }
    vapply(values, function(value) {
      if (is.null(value)) NA_real_ else as.numeric(value)
    }, numeric(1))
  }
  time_ny <- as.POSIXct(timestamp, origin = "1970-01-01", tz = "America/New_York")
  date_ny <- as.Date(format(time_ny, "%Y-%m-%d", tz = "America/New_York"))
  data <- data.frame(
    timestamp = format(time_ny, "%Y-%m-%d %H:%M:%S%z", tz = "America/New_York"),
    equity = numeric_column("equity"),
    profit_loss = numeric_column("profit_loss"),
    profit_loss_pct = numeric_column("profit_loss_pct"),
    stringsAsFactors = FALSE
  )
  selected <- which(date_ny >= start_date & date_ny <= end_date)
  selected <- selected[order(timestamp[selected])]
  if (!length(selected)) stop("No portfolio history falls within the requested dates.")
  if (anyDuplicated(timestamp[selected])) stop("Duplicate portfolio timestamps returned.")
  data <- data[selected, , drop = FALSE]
  contiguous <- c(FALSE, diff(timestamp[selected]) == 60 &
    diff(as.numeric(date_ny[selected])) == 0)
  data$pnl_change_1min <- c(NA_real_, diff(data$profit_loss))
  data$pnl_change_1min[!contiguous] <- NA_real_
  rownames(data) <- NULL
  data
}

main <- function() {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Install the required package: install.packages('httr2')")
  }
  arguments <- commandArgs(trailingOnly = TRUE)
  if (length(arguments) > 1L) stop("Usage: Rscript alpaca_pnls.R [env-file]")
  if (length(arguments)) {
    if (!file.exists(arguments[[1]])) stop("Environment file does not exist.")
    if (!readRenviron(arguments[[1]])) stop("Could not load environment file.")
  }
  trade_key <- Sys.getenv("ALPACA_TRADE_KEY")
  trade_secret <- Sys.getenv("ALPACA_TRADE_SECRET")
  if (!nzchar(trade_key) || !nzchar(trade_secret)) {
    stop("Set ALPACA_TRADE_KEY and ALPACA_TRADE_SECRET, or pass an env file.")
  }
  base_url <- sub("/+$", "", Sys.getenv("ALPACA_BASE_URL",
    "https://paper-api.alpaca.markets"))
  if (!base_url %in% c("https://paper-api.alpaca.markets", "https://api.alpaca.markets")) {
    stop("ALPACA_BASE_URL must be the official Alpaca paper or live endpoint.")
  }
  reporting <- Sys.getenv("ALPACA_INTRADAY_REPORTING", "extended_hours")
  if (!reporting %in% c("market_hours", "extended_hours", "continuous")) {
    stop("Invalid ALPACA_INTRADAY_REPORTING.")
  }
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_dir <- if (length(script_arg)) {
    dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
  } else getwd()
  output_file <- Sys.getenv("ALPACA_PNLS_OUTPUT",
    file.path(script_dir, "alpaca_pnls_20260923_20261002.csv"))
  if (file.exists(output_file)) stop("Output already exists: ", output_file)
  if (!dir.exists(dirname(output_file))) stop("Output directory does not exist.")
  start_date <- as.Date("2026-09-23")
  end_date <- as.Date("2026-10-02")
  request <- httr2::request(paste0(base_url, "/v2/account/portfolio/history"))
  request <- httr2::req_headers(request,
    `APCA-API-KEY-ID` = trade_key, `APCA-API-SECRET-KEY` = trade_secret)
  request <- httr2::req_url_query(request,
    timeframe = "1Min", intraday_reporting = reporting, pnl_reset = "per_day")
  request <- httr2::req_timeout(request, 60)
  request <- httr2::req_retry(request, max_tries = 3)
  request <- httr2::req_error(request, body = function(response) {
    httr2::resp_body_string(response)
  })
  message("Downloading minute account PnLs from ", base_url, " (", reporting, ")...")
  chunk_starts <- seq(start_date, end_date, by = "5 days")
  frames <- lapply(seq_along(chunk_starts), function(chunk_index) {
    chunk_start <- chunk_starts[chunk_index]
    chunk_end <- min(chunk_start + 4, end_date)
    message("Downloading ", chunk_start, " through ", chunk_end, "...")
    chunk_request <- httr2::req_url_query(request,
      start = paste0(chunk_start, "T00:00:00-04:00"),
      end = paste0(chunk_end, "T23:59:59-04:00"))
    response <- httr2::req_perform(chunk_request)
    history <- httr2::resp_body_json(response, simplifyVector = FALSE)
    history_to_frame(history, chunk_start, chunk_end)
  })
  data <- do.call(rbind, frames)
  write.csv(data, output_file, row.names = FALSE, na = "NA")
  message("Saved ", nrow(data), " snapshots to ", output_file)
  message("Range (New York): ", data$timestamp[[1]], " through ", tail(data$timestamp, 1))
  invisible(data)
}

if (sys.nframe() == 0L) main()