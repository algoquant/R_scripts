#!/usr/bin/env Rscript

# Rank US ETFs by assets under management and liquidity.
#
# Purpose
# -------
# Discover active US-listed ETFs through Tiingo, collect daily price and volume
# history for each completed calendar year, enrich each ticker with a current
# Yahoo Finance snapshot, and write the top-ranked rows to a CSV file.
#
# Data sources
# ------------
# * Tiingo search API: discovers the candidate ETF universe.
# * Tiingo daily metadata API: identifies the listing exchange and excludes OTC
#   securities.
# * Tiingo daily prices API: supplies adjusted close and adjusted volume when
#   available, otherwise unadjusted close and volume.
# * Yahoo Finance quoteSummary API: supplies the ETF name, category, current
#   assets under management, and current market capitalization.
#
# Ranking methodology
# -------------------
# For each year, eligible ETFs are ordered by:
#
# 1. Current AUM, descending.
# 2. Current market capitalization, descending.
# 3. Average historical daily dollar volume for that year, descending.
# 4. Ticker, ascending, as a deterministic final tie-breaker.
#
# The output is limited to `top_n` rows per year. Average dollar volume is
# calculated as adjusted close multiplied by adjusted volume for every available
# trading day in the requested year.
#
# Important limitations
# ---------------------
# AUM and market capitalization are point-in-time values fetched when the script
# runs. They are not historical year-end observations. Consequently, the script
# does not reconstruct the largest ETFs as they were known at each year-end.
# The current active ETF universe also introduces survivorship bias: delisted or
# currently inactive ETFs are not represented in earlier years. The annual rank
# should therefore be interpreted as a current-size ranking of today's eligible
# ETFs that had price history in each year, with that year's liquidity used as a
# secondary ranking measure.
#
# Configuration
# -------------
# * `TIINGO_API_KEY`: Tiingo authentication token.
# * `LARGEST_ETFS_OUTPUT`: optional output path. Defaults to
#   `largest_etfs.csv` beside this script.
# * `years_back`: number of completed calendar years to process.
# * `top_n`: maximum number of ETFs retained per year.
#
# Output columns
# --------------
# * `year`: completed calendar year represented by the price history.
# * `rank`: one-based rank within the year.
# * `ticker`: normalized uppercase ETF ticker.
# * `aum`: current Yahoo Finance total-assets snapshot.
# * `market_cap`: current Yahoo Finance market-cap snapshot.
# * `avg_dollar_volume`: mean daily close times volume for the year.
# * `trade_days`: count of distinct dates returned by Tiingo for the year.
#
# Usage
# -----
# Run from a shell after configuring the Tiingo credential:
#
#   TIINGO_API_KEY="..." Rscript etf_largest.R
#
# Override the output location when needed:
#
#   LARGEST_ETFS_OUTPUT="/tmp/largest_etfs.csv" Rscript etf_largest.R
#
# Network/API failures for an individual query, profile, snapshot, or ticker-year
# are treated as missing data so processing can continue. The script stops only
# when a required package or API credential is unavailable, or when no ranking
# rows can be produced.

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Install the jsonlite package before running this script.")
} # end if

`%||%` <- function(x, y) {
  # Return y only when x is NULL, empty, or the scalar empty string.
  if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

# Resolve relative output paths against the script location. In interactive
# sessions, where R does not provide --file, use the current working directory.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1]])))
} else {
  getwd()
}

tiingo_api_key <- Sys.getenv("TIINGO_API_KEY")
if (identical(tiingo_api_key, "")) {
  stop("Set TIINGO_API_KEY before running this script.")
} # end if

years_back <- 10L
top_n <- 500L
output_file <- Sys.getenv("LARGEST_ETFS_OUTPUT") %||% file.path(script_dir, "largest_etfs.csv")
tiingo_base_url <- "https://api.tiingo.com"
yahoo_base_url <- "https://query1.finance.yahoo.com"
search_queries <- c(LETTERS, as.character(0:9))
otc_markers <- c("OTC", "OTCQX", "OTCQB", "OTCBB", "OTCM", "OTCMKTS", "PINK", "GREY")
profile_cache <- new.env(parent = emptyenv())
snapshot_cache <- new.env(parent = emptyenv())

# Perform an authenticated Tiingo GET request and parse its JSON response.
#
# Args:
#   path: API path beginning with "/", relative to `tiingo_base_url`.
#   query: Named list of query-string parameters. The configured API token is
#     appended automatically.
#
# Returns:
#   The value produced by `jsonlite::fromJSON(..., simplifyDataFrame = TRUE)`.
#
# Errors:
#   Download and JSON parse errors propagate to the caller. Higher-level fetch
#   functions selectively convert those failures to missing data with tryCatch.
tiingo_get <- function(path, query = list()) {
  query$token <- tiingo_api_key
  query_string <- paste0(
    names(query),
    "=",
    vapply(query, URLencode, character(1), reserved = TRUE),
    collapse = "&"
  )
  url <- paste0(tiingo_base_url, path, "?", query_string)
  temp_file <- tempfile(fileext = ".json")
  utils::download.file(url, temp_file, quiet = TRUE, mode = "wb")
  on.exit(unlink(temp_file), add = TRUE)
  jsonlite::fromJSON(temp_file, simplifyDataFrame = TRUE)
} # tiingo_get

# Perform an unauthenticated Yahoo Finance GET request and parse JSON.
#
# Args:
#   path: API path, including any query string, relative to `yahoo_base_url`.
#
# Returns:
#   A simplified R representation of the JSON response.
yahoo_get <- function(path) {
  url <- paste0(yahoo_base_url, path)
  temp_file <- tempfile(fileext = ".json")
  utils::download.file(url, temp_file, quiet = TRUE, mode = "wb")
  on.exit(unlink(temp_file), add = TRUE)
  jsonlite::fromJSON(temp_file, simplifyDataFrame = TRUE)
}

# Convert a ticker or exchange-like value to trimmed uppercase text. NULL and
# zero-length values become the empty string through `%||%`.
normalize_ticker <- function(value) {
  toupper(trimws(as.character(value %||% "")))
}

# Return TRUE when an exchange code is an OTC venue or contains a recognized OTC
# marker. Empty exchange codes are not classified as OTC.
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

# Extract the first usable numeric value from a nested API object.
#
# Args:
#   x: Named list-like object returned from parsed JSON.
#   paths: Character vector of dot-delimited candidate paths, in priority order.
#
# Returns:
#   The first value coercible to a non-missing number, including Yahoo's common
#   `{raw = ...}` representation, or `NA_real_` when no path yields a value.
extract_first_numeric <- function(x, paths) {
  for (path in paths) {
    value <- x
    valid <- TRUE
    for (component in strsplit(path, "\\.")[[1]]) {
      if (is.null(value) || !(component %in% names(value))) {
        valid <- FALSE
        break
      }
      value <- value[[component]]
    }
    if (valid && !is.null(value)) {
      if (is.list(value) && "raw" %in% names(value)) {
        value <- value$raw
      }
      numeric_value <- suppressWarnings(as.numeric(value))
      if (length(numeric_value) > 0 && !is.na(numeric_value[[1]])) {
        return(numeric_value[[1]])
      }
    }
  }
  NA_real_
}

# Extract the first non-empty character value from a nested API object.
# Candidate paths are evaluated in order and `{raw = ...}` wrappers are unboxed.
# Returns `NA_character_` when every path is absent, empty, or missing.
extract_first_character <- function(x, paths) {
  for (path in paths) {
    value <- x
    valid <- TRUE
    for (component in strsplit(path, "\\.")[[1]]) {
      if (is.null(value) || !(component %in% names(value))) {
        valid <- FALSE
        break
      }
      value <- value[[component]]
    }
    if (valid && !is.null(value)) {
      if (is.list(value) && "raw" %in% names(value)) {
        value <- value$raw
      }
      text_value <- trimws(as.character(value))
      if (length(text_value) > 0 && !identical(text_value[[1]], "") && !is.na(text_value[[1]])) {
        return(text_value[[1]])
      }
    }
  }
  NA_character_
}

# Discover the current active US ETF universe from Tiingo.
#
# Tiingo's search endpoint does not provide a bulk ETF-universe response here,
# so the function searches every letter and digit, filters each response to
# active US ETFs with conventional ticker syntax, then de-duplicates and sorts
# the combined symbols.
#
# Returns:
#   A sorted character vector of uppercase ticker symbols. Failed or malformed
#   search responses are skipped.
discover_etf_universe <- function() {
  search_url <- "/tiingo/utilities/search"
  symbols <- character(0)

  for (query in search_queries) {
    result <- tryCatch(
      tiingo_get(search_url, query = list(query = query, limit = 100)),
      error = function(e) NULL
    )
    if (is.null(result) || (!is.data.frame(result) && !is.list(result))) {
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
        rows$assetType == "ETF" &
        rows$countryCode == "US" &
        rows$isActive,
      ,
      drop = FALSE
    ]

    symbols <- unique(c(symbols, rows$ticker))
  }

  sort(unique(symbols))
}

# Fetch and cache eligibility metadata for one ETF.
#
# Search metadata supplies asset type, country, and active status. Daily-symbol
# metadata supplies the exchange code used for OTC exclusion. Cache entries last
# for the current R process and avoid repeating these requests for every year.
#
# Args:
#   ticker: Normalized ticker symbol.
#
# Returns:
#   A list containing `asset_type`, `country_code`, `is_active`,
#   `exchange_code`, and `is_otc`.
get_etf_profile <- function(ticker) {
  if (exists(ticker, envir = profile_cache, inherits = FALSE)) {
    return(get(ticker, envir = profile_cache, inherits = FALSE))
  }

  search_data <- tryCatch(
    tiingo_get("/tiingo/utilities/search", query = list(query = ticker, limit = 10)),
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

  daily_meta <- tryCatch(tiingo_get(sprintf("/tiingo/daily/%s", ticker)), error = function(e) NULL)
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

# Fetch one ticker's daily bars for a complete calendar year.
#
# Adjusted close and adjusted volume are preferred when Tiingo returns them.
# `dollar_volume` is calculated row-wise as close multiplied by volume.
#
# Args:
#   ticker: Normalized ETF ticker.
#   year: Four-digit calendar year.
#
# Returns:
#   A data frame with ticker, date, close, volume, and dollar_volume. An empty
#   data frame indicates a failed request, malformed response, or no bars.
fetch_year_bars <- function(ticker, year) {
  start_date <- as.Date(sprintf("%d-01-01", year))
  end_date <- as.Date(sprintf("%d-12-31", year))
  result <- tryCatch(
    tiingo_get(
      sprintf("/tiingo/daily/%s/prices", ticker),
      query = list(startDate = format(start_date), endDate = format(end_date), resampleFreq = "daily")
    ),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(data.frame())
  }

  bars <- as.data.frame(result, stringsAsFactors = FALSE)
  required_cols <- c("date", "close")
  if (!all(required_cols %in% names(bars)) || nrow(bars) == 0) {
    return(data.frame())
  }

  bars$date <- as.Date(bars$date)
  bars$close <- suppressWarnings(as.numeric(if ("adjClose" %in% names(bars)) bars$adjClose else bars$close))
  bars$volume <- suppressWarnings(as.numeric(if ("adjVolume" %in% names(bars)) bars$adjVolume else bars$volume))
  bars$ticker <- ticker
  bars$dollar_volume <- bars$close * bars$volume
  bars[, c("ticker", "date", "close", "volume", "dollar_volume")]
}

# Fetch and cache the current Yahoo Finance snapshot for one ETF.
#
# Multiple candidate JSON paths are checked because Yahoo's response shape can
# vary across funds. Missing fields remain typed NA values. `etf_name` and
# `category` are retained in the cache for possible downstream use, although the
# current ranking output does not write them.
#
# Args:
#   ticker: Normalized ETF ticker.
#
# Returns:
#   A list with ticker, etf_name, category, aum, and market_cap.
get_snapshot_metrics <- function(ticker) {
  if (exists(ticker, envir = snapshot_cache, inherits = FALSE)) {
    return(get(ticker, envir = snapshot_cache, inherits = FALSE))
  }

  path <- sprintf(
    "/v10/finance/quoteSummary/%s?modules=price,summaryDetail,defaultKeyStatistics,fundProfile",
    ticker
  )
  datav <- tryCatch(yahoo_get(path), error = function(e) NULL)

  result <- list(
    ticker = ticker,
    etf_name = NA_character_,
    category = NA_character_,
    aum = NA_real_,
    market_cap = NA_real_
  )

  if (!is.null(datav) && !is.null(datav$quoteSummary$result) && length(datav$quoteSummary$result) > 0) {
    quote <- datav$quoteSummary$result[[1]]
    result$etf_name <- extract_first_character(quote, c("price.longName", "price.shortName"))
    if (!is.null(quote$fundProfile$category)) {
      result$category <- as.character(quote$fundProfile$category)
    } else if (!is.null(quote$summaryDetail$category)) {
      result$category <- as.character(quote$summaryDetail$category)
    }
    result$aum <- extract_first_numeric(
      quote,
      c("summaryDetail.totalAssets", "defaultKeyStatistics.totalAssets", "fundProfile.totalAssets")
    )
    result$market_cap <- extract_first_numeric(
      quote,
      c("price.marketCap", "summaryDetail.marketCap", "defaultKeyStatistics.marketCap")
    )
  }

  assign(ticker, result, envir = snapshot_cache)
  result
}

# Build the ranked ETF table for one calendar year.
#
# Each candidate must be active, non-OTC, and have usable daily history for the
# year. Snapshot AUM and market cap may be NA; R's default ordering places those
# values after available values under the descending sort expressions.
#
# Args:
#   year: Completed four-digit calendar year.
#
# Returns:
#   Up to `top_n` rows with the documented output columns, or an empty data frame
#   if no candidate produces a usable row.
rank_year <- function(year) {
  message(sprintf("Processing %d (%d candidate ETFs)", year, length(candidate_universe)))

  rows <- lapply(candidate_universe, function(ticker) {
    profile <- get_etf_profile(ticker)
    if (is.null(profile) || !isTRUE(profile$is_active) || isTRUE(profile$is_otc)) {
      return(data.frame())
    }

    bars <- fetch_year_bars(ticker, year)
    if (nrow(bars) == 0) {
      return(data.frame())
    }

    metrics <- get_snapshot_metrics(ticker)
    data.frame(
      year = year,
      ticker = ticker,
      aum = metrics$aum,
      market_cap = metrics$market_cap,
      avg_dollar_volume = mean(bars$dollar_volume, na.rm = TRUE),
      trade_days = length(unique(bars$date)),
      stringsAsFactors = FALSE
    )
  })

  ranked <- do.call(rbind, rows)
  if (is.null(ranked) || nrow(ranked) == 0) {
    return(data.frame())
  }

  ranked <- ranked[order(-ranked$aum, -ranked$market_cap, -ranked$avg_dollar_volume, ranked$ticker), ]
  ranked$rank <- seq_len(nrow(ranked))
  ranked <- ranked[ranked$rank <= top_n, c("year", "rank", "ticker", "aum", "market_cap", "avg_dollar_volume", "trade_days")]
  rownames(ranked) <- NULL
  ranked
}

# Process only completed years. For example, a run during 2026 covers 2016-2025
# when `years_back` is 10.
end_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
start_year <- end_year - (years_back - 1L)

candidate_universe <- discover_etf_universe()
message(sprintf("Discovered %d Tiingo ETF candidates", length(candidate_universe)))

results <- do.call(rbind, lapply(start_year:end_year, rank_year))
if (is.null(results) || nrow(results) == 0) {
  stop("No ETF ranking results were produced.")
}

dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
write.csv(results, output_file, row.names = FALSE)
message(sprintf("Saved %d rows to %s", nrow(results), output_file))