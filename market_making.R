# make_market_vec() Function for market making strategy - vectorized version
make_market <- function(oh_lc, ohlc_lag=rutils::lag_it(oh_lc),
                        # std_dev,
                        buy_spread, sell_spread) {
  op_en <- oh_lc[, 1]
  hi_gh <- oh_lc[, 2]
  lo_w <- oh_lc[, 3]
  clo_se <- oh_lc[, 4]

  # Run the trading model (strategy):
  # sprea_d <- 0.25*trunc(std_dev)
  # limit prices are the low and high prices of the lagged bar, plus the spreads
  buy_limit <- (ohlc_lag[, 3] - buy_spread)
  sell_limit <- (ohlc_lag[, 2] + sell_spread)
  # lag_1 <- rutils::lag_it(oh_lc)
  # buy_limit <- (pmin(ohlc_lag[, 3], lag_1[, 3]) - buy_spread)
  # sell_limit <- (pmax(ohlc_lag[, 2], lag_1[, 2]) + sell_spread)

  # Buy_limit should be no greater than open price op_en
  buy_limit <- pmin(op_en, buy_limit)
  # sell_limit should be no less than open price op_en
  sell_limit <- pmax(op_en, sell_limit)

  # Indicators of whether the orders were filled
  buy_ind <- (lo_w < buy_limit)
  sell_ind <- (hi_gh > sell_limit)

  # Cumulative numbers of filled orders
  n_buys <- cumsum(buy_ind)
  n_sells <- cumsum(sell_ind)

  # Prices of the filled orders
  n_rows <- NROW(oh_lc)
  buy_s <- numeric(n_rows)
  buy_s[buy_ind] <- buy_limit[buy_ind]
  buy_s <- cumsum(buy_s)
  sell_s <- numeric(n_rows)
  sell_s[sell_ind] <- sell_limit[sell_ind]
  sell_s <- cumsum(sell_s)

  # Realized and unrealized pnls
  mark_to_market <- clo_se*(n_sells - n_buys)
  past_buys <- buy_s[match(n_sells, n_buys)]
  past_sells <- sell_s[match(n_buys, n_sells)]
  re_al <- ifelse(n_buys > n_sells,
                  sell_s - past_buys,
                  past_sells - buy_s)
  un_real <- ifelse(n_buys > n_sells,
                    past_buys - buy_s - mark_to_market,
                    sell_s - past_sells - mark_to_market)
  # Pnls equal to difference between filled sell minus buy prices
  pnl_s <- ((sell_s-buy_s) - mark_to_market)
  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, n_buys-n_sells, re_al, un_real)
  colnames(pnl_s)[5:8] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL")

  pnl_s
}  # end make_market_vec


# make_market_loop() Function for market making strategy - loop version
make_market_loop <- function(oh_lc, ohlc_lag=rutils::lag_it(oh_lc), buy_spread, sell_spread) {
  n_rows <- NROW(oh_lc)
  # buy_limit <- numeric(n_rows)
  # sell_limit <- numeric(n_rows)
  n_buys <- numeric(n_rows)
  n_sells <- numeric(n_rows)
  buy_s <- numeric(n_rows)
  sell_s <- numeric(n_rows)
  re_al <- numeric(n_rows)
  un_real <- numeric(n_rows)
  pnl_s <- numeric(n_rows)

  for (it in 2:n_rows) {
    # cat("iteration:", it, "\n")
    b_spread <- buy_spread
    s_spread <- sell_spread
    # Spread widening rules to limit inventory risk
    # doesn't work well
    # if (n_sells[it-1] > n_buys[it-1])
    #   s_spread <- sell_spread + ((n_sells[it-1] - n_buys[it-1]) %/% 25) / 4
    # else
    #   b_spread <- buy_spread + ((n_buys[it-1] - n_sells[it-1]) %/% 25) / 4
    # Or
    # if (n_sells[it-1] > n_buys[it-1])
    #   b_spread <- buy_spread - ((n_sells[it-1] - n_buys[it-1]) %/% 25) / 4
    # else
    #   s_spread <- sell_spread - ((n_buys[it-1] - n_sells[it-1]) %/% 25) / 4
    # Or
    # if ((n_sells[it-1]-n_buys[it-1]) > 20)
    #   s_spread <- sell_spread + 5
    # if ((n_buys[it-1]-n_sells[it-1]) > 20)
    #   b_spread <- buy_spread + 5

    buy_limit <- (ohlc_lag[it, 3] - b_spread)
    # buy_limit should be no greater than open price
    buy_limit <- min(oh_lc[it, 1], buy_limit)
    sell_limit <- (ohlc_lag[it, 2] + s_spread)
    # sell_limit should be no less than open price
    sell_limit <- max(oh_lc[it, 1], sell_limit)

    buy_ind <- (oh_lc[it, 3] < buy_limit)
    sell_ind <- (oh_lc[it, 2] > sell_limit)
    n_buys[it] <- n_buys[it-1] + buy_ind
    n_sells[it] <- n_sells[it-1] + sell_ind
    buy_s[it] <- buy_s[it-1] + buy_ind*buy_limit
    sell_s[it] <- sell_s[it-1] + sell_ind*sell_limit

    # Realized and unrealized pnls
    mark_to_market <- oh_lc[it, 4]*(n_sells[it] - n_buys[it])
    past_buys <- buy_s[match(n_sells[it], n_buys)]
    past_sells <- sell_s[match(n_buys[it], n_sells)]

    re_al[it] <- ifelse(n_buys[it] > n_sells[it],
                        sell_s[it] - past_buys,
                        past_sells - buy_s[it])
    un_real[it] <- ifelse(n_buys[it] > n_sells[it],
                          past_buys - buy_s[it] - mark_to_market,
                          sell_s[it] - past_sells - mark_to_market)


    pnl_s[it] <- ((sell_s[it] - buy_s[it]) - mark_to_market)
  }  # end for

  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, n_buys-n_sells, re_al, un_real)
  colnames(pnl_s)[5:8] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL")
  pnl_s
}  # end make_market

