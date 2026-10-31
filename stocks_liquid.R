#!/usr/bin/env Rscript

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install the jsonlite package before running this script.")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
} else {
  getwd()
}

api_key <- Sys.getenv("TIINGO_API_KEY")
if (identical(api_key, "")) {
  stop("Set TIINGO_API_KEY before running this script.")
}

years_back <- 20L
top_n <- 500L
output_file <- Sys.getenv("LIQUID_STOCKS_OUTPUT") %||% file.path(script_dir, "liquid_stocks.csv")
base_url <- "https://api.tiingo.com"
search_queries <- c(LETTERS, as.character(0:9))
otc_markers <- c("OTC", "OTCQX", "OTCQB", "OTCBB", "OTCM", "OTCMKTS", "PINK", "GREY")
profile_cache <- new.env(parent = emptyenv())

tiingo_get <- function(path, query = list()) {
  query$token <- api_key
  query_string <- paste0(
    names(query),
    "=",
    vapply(query, URLencode, character(1), reserved = TRUE),
    collapse = "&"
  )
  url <- paste0(base_url, path, "?", query_string)
  temp_file <- tempfile(fileext = ".json")
  utils::download.file(url, temp_file, quiet = TRUE, mode = "wb")
  on.exit(unlink(temp_file), add = TRUE)
  jsonlite::fromJSON(temp_file, simplifyDataFrame = TRUE)
}

normalize_ticker <- function(value) {
  toupper(trimws(as.character(value %||% "")))
}

is_otc_exchange <- function(exchange_code) {
  exchange <- normalize_ticker(exchange_code)
  if (identical(exchange, "")) {
    return(FALSE)
  }
  if (exchange %in% otc_markers) {
    return(TRUE)
  }

  any(vapply(otc_markers, function(marker) grepl(marker, exchange, fixed = TRUE), logical(1)))
}

discover_stock_universe <- function() {
  search_url <- "/tiingo/utilities/search"
  symbols <- character(0)

  for (query in search_queries) {
    result <- tryCatch(
      tiingo_get(search_url, query = list(query = query, limit = 100)),
      error = function(e) NULL
    )
    if (is.null(result) || !is.data.frame(result) && !is.list(result)) {
      next
    }

    rows <- as.data.frame(result, stringsAsFactors = FALSE)
    if (!all(c("ticker", "assetType", "countryCode") %in% names(rows))) {
      next
    }

    rows$ticker <- normalize_ticker(rows$ticker)
    rows$assetType <- toupper(trimws(as.character(rows$assetType)))
    rows$countryCode <- toupper(trimws(as.character(rows$countryCode)))
    rows$isActive <- if ("isActive" %in% names(rows)) {
      as.logical(rows$isActive)
    } else {
      TRUE
    }

    rows <- rows[
      rows$ticker != "" &
        grepl("^[A-Z][A-Z0-9.\\-]*$", rows$ticker) &
        rows$assetType == "STOCK" &
        rows$countryCode == "US" &
        rows$isActive,
      ,
      drop = FALSE
    ]

    symbols <- unique(c(symbols, rows$ticker))
  }

  sort(unique(symbols))
}

get_ticker_profile <- function(ticker) {
  if (exists(ticker, envir = profile_cache, inherits = FALSE)) {
    return(get(ticker, envir = profile_cache, inherits = FALSE))
  }

  search_url <- "/tiingo/utilities/search"
  daily_url <- sprintf("/tiingo/daily/%s", ticker)

  search_data <- tryCatch(
    tiingo_get(search_url, query = list(query = ticker, limit = 10)),
    error = function(e) NULL
  )

  asset_type <- ""
  country_code <- ""
  is_active <- TRUE
  if (!is.null(search_data)) {
    search_rows <- as.data.frame(search_data, stringsAsFactors = FALSE)
    if (nrow(search_rows) > 0) {
      search_rows$ticker <- normalize_ticker(search_rows$ticker)
      exact <- search_rows[search_rows$ticker == ticker, , drop = FALSE]
      if (nrow(exact) == 0) {
        exact <- search_rows[1, , drop = FALSE]
      }
      asset_type <- toupper(trimws(as.character(exact$assetType[[1]] %||% "")))
      country_code <- toupper(trimws(as.character(exact$countryCode[[1]] %||% "")))
      if ("isActive" %in% names(exact)) {
        is_active <- isTRUE(exact$isActive[[1]])
      }
    }
  }

  daily_meta <- tryCatch(tiingo_get(daily_url), error = function(e) NULL)
  exchange_code <- ""
  if (!is.null(daily_meta) && is.data.frame(daily_meta) && nrow(daily_meta) > 0 && "exchangeCode" %in% names(daily_meta)) {
    exchange_code <- toupper(trimws(as.character(daily_meta$exchangeCode[[1]] %||% "")))
  } else if (!is.null(daily_meta) && is.list(daily_meta) && !is.data.frame(daily_meta) && "exchangeCode" %in% names(daily_meta)) {
    exchange_code <- toupper(trimws(as.character(daily_meta$exchangeCode %||% "")))
  }

  profile <- list(
    asset_type = asset_type,
    country_code = country_code,
    is_active = is_active,
    exchange_code = exchange_code,
    is_otc = is_otc_exchange(exchange_code)
  )

  assign(ticker, profile, envir = profile_cache)
  profile
}

fetch_year_bars <- function(ticker, year) {
  start_date <- as.Date(sprintf("%d-01-01", year))
  end_date <- as.Date(sprintf("%d-12-31", year))
  path <- sprintf("/tiingo/daily/%s/prices", ticker)
  result <- tryCatch(
    tiingo_get(path, query = list(startDate = format(start_date), endDate = format(end_date), resampleFreq = "daily")),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(data.frame())
  }

  bars <- as.data.frame(result, stringsAsFactors = FALSE)
  required_cols <- c("date", "close", "volume")
  if (!all(required_cols %in% names(bars)) || nrow(bars) == 0) {
    return(data.frame())
  }

  bars$date <- as.Date(bars$date)
  if ("adjClose" %in% names(bars)) {
    bars$close <- as.numeric(bars$adjClose)
  } else {
    bars$close <- as.numeric(bars$close)
  }
  if ("adjVolume" %in% names(bars)) {
    bars$volume <- as.numeric(bars$adjVolume)
  } else {
    bars$volume <- as.numeric(bars$volume)
  }

  bars$ticker <- ticker
  bars$dollar_volume <- bars$close * bars$volume
  bars[, c("ticker", "date", "close", "volume", "dollar_volume")]
}

rank_year <- function(year) {
  message(sprintf("Processing %d (%d candidate stocks)", year, length(candidate_universe)))

  rows <- lapply(candidate_universe, function(ticker) {
    profile <- get_ticker_profile(ticker)
    if (is.null(profile) || !isTRUE(profile$is_active) || isTRUE(profile$is_otc)) {
      return(data.frame())
    }

    bars <- fetch_year_bars(ticker, year)
    if (nrow(bars) == 0) {
      return(data.frame())
    }

    data.frame(
      year = year,
      ticker = ticker,
      avg_dollar_volume = mean(bars$dollar_volume, na.rm = TRUE),
      avg_volume = mean(bars$volume, na.rm = TRUE),
      trade_days = length(unique(bars$date)),
      stringsAsFactors = FALSE
    )
  })

  ranked <- do.call(rbind, rows)
  if (is.null(ranked) || nrow(ranked) == 0) {
    return(data.frame())
  }

  ranked <- ranked[order(-ranked$avg_dollar_volume, ranked$ticker), ]
  ranked$rank <- seq_len(nrow(ranked))
  ranked <- ranked[ranked$rank <= top_n, c("year", "rank", "ticker", "avg_dollar_volume", "avg_volume", "trade_days")]
  rownames(ranked) <- NULL
  ranked
}

end_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
start_year <- end_year - (years_back - 1L)

candidate_universe <- discover_stock_universe()
message(sprintf("Discovered %d Tiingo stock candidates", length(candidate_universe)))

results <- do.call(rbind, lapply(start_year:end_year, rank_year))
if (is.null(results) || nrow(results) == 0) {
  stop("No liquidity results were produced.")
}

dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
write.csv(results, output_file, row.names = FALSE)
message(sprintf("Saved %d rows to %s", nrow(results), output_file))