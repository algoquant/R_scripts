extract_total_fills <- function(blotter) {
  required <- c("timestamp", "symbol", "price", "qty", "type",
    "broker_filled_qty", "order_id")
  if (!all(required %in% names(blotter))) stop("Missing required blotter columns.")
  fills <- blotter[!is.na(blotter$broker_filled_qty) & blotter$broker_filled_qty > 0,
    required, drop = FALSE]
  if (anyNA(fills$order_id) || any(!nzchar(fills$order_id))) {
    stop("Fill confirmations must have order IDs.")
  }
    fills$price[!fills$type %in% c("fill", "partial fill")] <- NA_real_
    if (anyNA(fills$qty) ||
      any(!is.na(fills$price) & (!is.finite(fills$price) | fills$price <= 0)) ||
      any(!is.finite(fills$qty)) || any(fills$qty == 0)) {
    stop("Invalid fill prices or trade directions.")
  }
  fills <- fills[order(fills$order_id, -fills$broker_filled_qty, is.na(fills$price),
    fills$timestamp, decreasing = FALSE), , drop = FALSE]
  for (order_id in unique(fills$order_id)) {
    order_rows <- fills[fills$order_id == order_id, , drop = FALSE]
    if (length(unique(order_rows$symbol)) != 1L ||
        length(unique(sign(order_rows$qty))) != 1L) {
      stop("Inconsistent symbol or side for order: ", order_id)
    }
    final_rows <- order_rows[order_rows$broker_filled_qty ==
      max(order_rows$broker_filled_qty), , drop = FALSE]
    if (length(unique(na.omit(final_rows$price))) > 1L) {
      stop("Conflicting final average fill prices for order: ", order_id)
    }
  }
  fills <- fills[!duplicated(fills$order_id), , drop = FALSE]
  result <- data.frame(timestamp = fills$timestamp, symbol = fills$symbol,
    num_shares = fills$broker_filled_qty,
    fill_price = -sign(fills$qty) * fills$price,
    stringsAsFactors = FALSE)
  result <- result[order(result$timestamp, result$symbol), , drop = FALSE]
  rownames(result) <- NULL
  result
}

main <- function() {
  directory <- "/Users/jerzy/Develop/trading/logs"
  dates <- c("20260923", "20260924", "20260925", "20260928",
    "20260929", "20260930", "20261001", "20261002", "20261005")
  required <- c("timestamp", "symbol", "price", "qty", "type",
    "broker_filled_qty", "order_id")
  blotters <- lapply(dates, function(date) {
    path <- file.path(directory, paste0("Ladder_blotter_", date, ".csv"))
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)[, required]
  })
  result <- extract_total_fills(do.call(rbind, blotters))
  output <- file.path(directory, "Ladder_fills.csv")
  if (file.exists(output)) {
    backup <- paste0(output, ".bak_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    if (!file.copy(output, backup, overwrite = FALSE)) stop("Could not back up output.")
    message("Previous output backed up to ", backup)
  }
  write.csv(result, output, row.names = FALSE)
  message("Saved ", nrow(result), " order fills (", sum(result$num_shares),
    " shares) to ", output)
  message("Orders with unavailable average fill price: ", sum(is.na(result$fill_price)))
  print(table(substr(result$timestamp, 1, 10)))
  invisible(result)
}

if (sys.nframe() == 0L) main()