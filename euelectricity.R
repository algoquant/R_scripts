#!/usr/bin/env Rscript

library(HighFreq)

# Download the full daily electricity price history for Germany from euenergy.live.

base_url <- "https://euenergy.live/api/v1/summary"
auth_header <- "Authorization: Bearer fb1baef596a3112740fd4342000a82de"
zone <- "DE-LU"
start_date <- as.Date("2022-01-01")
end_date <- Sys.Date()

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required. Install it with install.packages('jsonlite').")
}

if (!requireNamespace("parallel", quietly = TRUE)) {
  stop("Package 'parallel' is required.")
}

fetch_daily_summary <- function(date_value) {
  value_or_na <- function(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
      NA
    } else {
      x[[1]]
    }
  }

  response_text <- system2(
    "curl",
    args = c(
      "--fail",
      "--silent",
      "--show-error",
      shQuote(sprintf("%s?date=%s&zone=%s", base_url, format(date_value, "%Y-%m-%d"), zone)),
      "-H",
      shQuote(auth_header)
    ),
    stdout = TRUE,
    stderr = FALSE
  )

  status_code <- attr(response_text, "status")
  if (!is.null(status_code) && status_code != 0) {
    stop(sprintf("curl failed for %s with status %s", format(date_value, "%Y-%m-%d"), status_code))
  }

  response_text <- paste(response_text, collapse = "\n")
  if (!jsonlite::validate(response_text)) {
    stop(sprintf("API response for %s was not valid JSON", format(date_value, "%Y-%m-%d")))
  }

  parsed <- jsonlite::fromJSON(response_text, simplifyDataFrame = TRUE)
  data.frame(
    date = as.Date(date_value),
    zone = value_or_na(parsed$zone),
    avgPriceEur = value_or_na(parsed$avgPriceEur),
    avgLoadMw = value_or_na(parsed$avgLoadMw),
    priceChangePct = value_or_na(parsed$priceChangePct),
    loadChangePct = value_or_na(parsed$loadChangePct),
    avgPriceY2d = value_or_na(parsed$avgPriceY2d),
    avgPriceM2d = value_or_na(parsed$avgPriceM2d),
    stringsAsFactors = FALSE
  )
}

date_sequence <- seq.Date(start_date, end_date, by = "day")
mc_cores <- max(1L, min(4L, parallel::detectCores(logical = FALSE) - 1L))
cat(sprintf("Downloading %d daily rows using %d workers...\n", length(date_sequence), mc_cores))

data_eu <- do.call(
  rbind,
  parallel::mclapply(date_sequence, fetch_daily_summary, mc.cores = mc_cores)
)

cat("Germany daily electricity prices:\n")
print(utils::head(data_eu, 3))
cat(sprintf("\nDownloaded %d daily rows from %s to %s\n", nrow(data_eu), format(start_date, "%Y-%m-%d"), format(end_date, "%Y-%m-%d")))

write.csv(data_eu, "germany_electricity_prices.csv", row.names = FALSE)
cat("Saved to germany_electricity_prices.csv\n")

eprices_germany <- xts::xts(data_eu$avgPriceEur, order.by = data_eu$date)
dygraph(eprices_germany, main = "Germany Daily Electricity Prices (EUR/MWh)") %>%
  dyAxis("y", label = "Price (EUR/MWh)") %>%
  dyOptions(colors = "blue")


