
# Install packages if needed
# install.packages(c("DBI", "RPostgres", "xts", "dplyr"))
# install.packages("wrds", repos = "https://cloud.r-project.org")

library(DBI)
library(RPostgres)
library(dplyr)
library(xts)

#--------------------------------------------------
# Connect to WRDS
#--------------------------------------------------
wrds <- dbConnect(
  Postgres(),
  host     = "wrds-pgdata.wharton.upenn.edu",
  port     = 9737,
  dbname   = "wrds",
  sslmode  = "require",
  user     = "jp3900",
  password = rstudioapi::askForPassword("WRDS Password"),
)

#--------------------------------------------------
# Get PERMNOs for the ticker symbols
#--------------------------------------------------
symbols <- c("AAPL", "NVDA", "MSFT")

ticker_sql <- paste0(
  "'", paste(symbols, collapse = "','"), "'"
)

permno_query <- "
SELECT DISTINCT permno, ticker
FROM crsp.stocknames
WHERE ticker IN ('AAPL','NVDA','MSFT')
"

permnos <- dbGetQuery(wrds, permno_query)

print(permnos)


#--------------------------------------------------
# Download daily OHLC data from CRSP
#--------------------------------------------------
permno_list <- paste(permnos$permno, collapse = ",")

ohlc_query <- sprintf("
    SELECT
        a.date,
        b.ticker,
        a.openprc / NULLIF(a.cfacpr, 0)  AS Open,
        a.askhi   / NULLIF(a.cfacpr, 0)  AS High,
        a.bidlo   / NULLIF(a.cfacpr, 0)  AS Low,
        ABS(a.prc) / NULLIF(a.cfacpr, 0) AS Close,
        a.vol      AS Volume,
        a.ret      AS Return
    FROM crsp.dsf a
    JOIN crsp.stocknames b
         ON a.permno = b.permno
    WHERE a.permno IN (%s)
    ORDER BY b.ticker, a.date
", permno_list)

ohlc <- dbGetQuery(wrds, ohlc_query)

#--------------------------------------------------
# Convert each symbol to xts
#--------------------------------------------------
library(xts)

xts_list <- lapply(split(ohlc, ohlc$ticker), function(df) {
  df <- df[order(df$date), ]
  daily_growth <- 1 + ifelse(is.na(df$return), 0, df$return)
  total_return_index <- df$close[1] * cumprod(c(1, daily_growth[-1]))

  series <- df[, c("open", "high", "low", "close", "volume", "return")]
  series$total_return_index <- total_return_index

  xts(series, order.by = as.Date(df$date))
})

names(xts_list) <- unique(ohlc$ticker)

#--------------------------------------------------
# Examples
#--------------------------------------------------
AAPL <- xts_list[["AAPL"]]
NVDA <- xts_list[["NVDA"]]
MSFT <- xts_list[["MSFT"]]

head(AAPL)
head(NVDA)
head(MSFT)

#--------------------------------------------------
# Disconnect
#--------------------------------------------------
dbDisconnect(wrds)
