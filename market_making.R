# make_market_vec() Function for market making strategy - vectorized version
make_market <- function(oh_lc, ohlc_lag=rutils::lag_it(oh_lc),
                        # std_dev,
                        buy_spread, sell_spread) {
  # Run the trading model (strategy):
  # sprea_d <- 0.25*trunc(std_dev)
  buy_limit <- (ohlc_lag[, 3] - buy_spread)
  sell_limit <- (ohlc_lag[, 2] + sell_spread)
  # lag_1 <- rutils::lag_it(oh_lc)
  # buy_limit <- (pmin(ohlc_lag[, 3], lag_1[, 3]) - buy_spread)
  # sell_limit <- (pmax(ohlc_lag[, 2], lag_1[, 2]) + sell_spread)

  # buy_limit should be no greater than open price oh_lc[, 1]
  buy_limit <- pmin(oh_lc[, 1], buy_limit)
  # sell_limit should be no less than open price oh_lc[, 1]
  sell_limit <- pmax(oh_lc[, 1], sell_limit)

  buy_ind <- (oh_lc[, 3] < buy_limit)
  sell_ind <- (oh_lc[, 2] > sell_limit)

  n_buy <- cumsum(buy_ind)
  n_sell <- cumsum(sell_ind)

  n_rows <- NROW(oh_lc)
  buy_s <- numeric(n_rows)
  buy_s[buy_ind] <- buy_limit[buy_ind]
  buy_s <- cumsum(buy_s)
  sell_s <- numeric(n_rows)
  sell_s[sell_ind] <- sell_limit[sell_ind]
  sell_s <- cumsum(sell_s)

  pnl_s <- ((sell_s-buy_s) - oh_lc[, 4]*(n_sell-n_buy))
  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, n_buy-n_sell)
  colnames(pnl_s)[5:6] <- c("Strategy", "Inventory")
  pnl_s
}  # end make_market_vec


# make_market_loop() Function for market making strategy - loop version
make_market_loop <- function(oh_lc, ohlc_lag=rutils::lag_it(oh_lc), buy_spread, sell_spread) {
  n_rows <- NROW(oh_lc)
  # buy_limit <- numeric(n_rows)
  # sell_limit <- numeric(n_rows)
  n_buy <- numeric(n_rows)
  n_sell <- numeric(n_rows)
  buy_s <- numeric(n_rows)
  sell_s <- numeric(n_rows)
  pnl_s <- numeric(n_rows)

  for (it in 2:n_rows) {
    b_spread <- buy_spread
    s_spread <- sell_spread
    # Spread widening rules to limit inventory risk
    # doesn't work well
    # if (n_sell[it-1] > n_buy[it-1])
    #   s_spread <- sell_spread + ((n_sell[it-1] - n_buy[it-1]) %/% 25) / 4
    # else
    #   b_spread <- buy_spread + ((n_buy[it-1] - n_sell[it-1]) %/% 25) / 4
    # Or
    # if (n_sell[it-1] > n_buy[it-1])
    #   b_spread <- buy_spread - ((n_sell[it-1] - n_buy[it-1]) %/% 25) / 4
    # else
    #   s_spread <- sell_spread - ((n_buy[it-1] - n_sell[it-1]) %/% 25) / 4
    # Or
    # if ((n_sell[it-1]-n_buy[it-1]) > 20)
    #   s_spread <- sell_spread + 5
    # if ((n_buy[it-1]-n_sell[it-1]) > 20)
    #   b_spread <- buy_spread + 5

    buy_limit <- (ohlc_lag[it, 3] - b_spread)
    # buy_limit should be no greater than open price
    buy_limit <- min(oh_lc[it, 1], buy_limit)
    sell_limit <- (ohlc_lag[it, 2] + s_spread)
    # sell_limit should be no less than open price
    sell_limit <- max(oh_lc[it, 1], sell_limit)

    buy_ind <- (oh_lc[it, 3] < buy_limit)
    sell_ind <- (oh_lc[it, 2] > sell_limit)
    n_buy[it] <- n_buy[it-1] + buy_ind
    n_sell[it] <- n_sell[it-1] + sell_ind
    buy_s[it] <- buy_s[it-1] + buy_ind*buy_limit
    sell_s[it] <- sell_s[it-1] + sell_ind*sell_limit
    pnl_s[it] <- ((sell_s[it] - buy_s[it]) - oh_lc[it, 4]*(n_sell[it] - n_buy[it]))
  }  # end for

  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, n_buy-n_sell)
  colnames(pnl_s)[5:6] <- c("Strategy", "Inventory")
  pnl_s
}  # end make_market

