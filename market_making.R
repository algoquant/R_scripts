# make_market_loop() Function for market making strategy - loop version
make_market <- function(oh_lc, ohlc_lag=rutils::lag_it(oh_lc),
                        buy_spread, sell_spread, lamb_da, invent_limit, warm_up) {
  n_rows <- NROW(oh_lc)
  op_en <- oh_lc[, 1]
  hi_gh <- oh_lc[, 2]
  lo_w <- oh_lc[, 3]
  clos_e <- oh_lc[, 4]
  # look_back <- 111
  # weight_s <- exp(-lamb_da*1:look_back)
  # weight_s <- weight_s/sum(weight_s)
  ew_ma <- numeric(n_rows)
  ew_ma[1] <- oh_lc[1, 6]
  # ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)
  bia_s <- numeric(n_rows)
  # buy_limit <- numeric(n_rows)
  # sell_limit <- numeric(n_rows)
  n_buys <- numeric(n_rows)
  n_sells <- numeric(n_rows)
  # inventory
  inv_ent <- numeric(n_rows)
  buy_s <- numeric(n_rows)
  sell_s <- numeric(n_rows)
  re_al <- numeric(n_rows)
  un_real <- numeric(n_rows)
  pnl_s <- numeric(n_rows)

  for (it in 1:(n_rows-1)) {
    # cat("iteration:", it, "\n")
    # b_spread <- buy_spread
    # s_spread <- sell_spread
    # Spread widening rules to limit inventory risk
    # limit prices are the low and high prices of the lagged bar, plus the spreads
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

    ew_ma[it] <- lamb_da*oh_lc[it, 6] + (1-lamb_da)*ew_ma[max(it-1, 1)]

    if (it > warm_up) {
      # bia_s[it] <- (if (ohlc_lag[it, 4] > ew_ma[it]) 0.25 else -0.25)

      buy_limit <- (ohlc_lag[it, 3] - buy_spread + bia_s[it])
      # buy_limit should be no greater than previous close price
      buy_limit <- min(clos_e[it], buy_limit)

      sell_limit <- (ohlc_lag[it, 2] + sell_spread + bia_s[it])
      # sell_limit should be no less than previous close price
      sell_limit <- max(clos_e[it], sell_limit)

      # Trade in the next period - but don't trade if inventory exceeds limit
      buy_ind <- (lo_w[it+1] < buy_limit) & (inv_ent[it] < invent_limit)
      sell_ind <- (hi_gh[it+1] > sell_limit) & (inv_ent[it] > -invent_limit)
      n_buys[it+1] <- n_buys[it] + buy_ind
      n_sells[it+1] <- n_sells[it] + sell_ind
      inv_ent[it+1] <- (n_buys[it+1] - n_sells[it+1])
      buy_s[it+1] <- buy_s[it] + buy_ind*buy_limit
      sell_s[it+1] <- sell_s[it] + sell_ind*sell_limit

      # Realized and unrealized pnls
      mark_to_market <- (-clos_e[it+1]*inv_ent[it+1])
      # This part takes long to run, so commented out
      # past_buys <- buy_s[match(n_sells[it], n_buys)]
      # past_sells <- sell_s[match(n_buys[it], n_sells)]
      #
      # re_al[it] <- ifelse(n_buys[it] > n_sells[it],
      #                     sell_s[it] - past_buys,
      #                     past_sells - buy_s[it])
      # un_real[it] <- ifelse(n_buys[it] > n_sells[it],
      #                       past_buys - buy_s[it] - mark_to_market,
      #                       sell_s[it] - past_sells - mark_to_market)


      # Pnls equal to difference between filled sell minus buy prices, minus mark_to_market
      pnl_s[it+1] <- ((sell_s[it+1] - buy_s[it+1]) - mark_to_market)
    }  # end for
  }  # end if

  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, inv_ent, re_al, un_real, ew_ma)
  colnames(pnl_s)[5:9] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")
  pnl_s
}  # end make_market



# make_market_vec() Function for market making strategy - vectorized version
make_market_vec <- function(oh_lc, ohlc_lag=rutils::lag_it(oh_lc),
                            # std_dev,
                            buy_spread, sell_spread, lamb_da, invent_limit) {
  op_en <- oh_lc[, 1]
  hi_gh <- oh_lc[, 2]
  lo_w <- oh_lc[, 3]
  clos_e <- oh_lc[, 4]
  look_back <- 111
  weight_s <- exp(-lamb_da*1:look_back)
  weight_s <- weight_s/sum(weight_s)
  ew_ma <- HighFreq::roll_wsum(vec_tor=rutils::lag_it(oh_lc)[, 4], weight_s=rev(weight_s))
  ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)

  # Run the trading model (strategy):
  # bia_s <- numeric(n_rows)
  bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)

  # limit prices are the low and high prices of the lagged bar, plus the spreads
  buy_limit <- (ohlc_lag[, 3] - buy_spread + bia_s)
  sell_limit <- (ohlc_lag[, 2] + sell_spread + bia_s)
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
  mark_to_market <- clos_e*(n_sells - n_buys)
  past_buys <- buy_s[match(n_sells, n_buys)]
  past_sells <- sell_s[match(n_buys, n_sells)]
  re_al <- ifelse(n_buys > n_sells,
                  sell_s - past_buys,
                  past_sells - buy_s)
  un_real <- ifelse(n_buys > n_sells,
                    past_buys - buy_s - mark_to_market,
                    sell_s - past_sells - mark_to_market)

  # Pnls equal to difference between filled sell minus buy prices and mark_to_market
  pnl_s <- ((sell_s-buy_s) - mark_to_market)
  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, n_buys-n_sells, re_al, un_real, ew_ma)
  colnames(pnl_s)[5:9] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")

  pnl_s
}  # end make_market_vec



###


# trade_median() Function for market making strategy - vectorized version
trade_median <- function(re_turns, oh_lc, ohlc_lag=rutils::lag_it(oh_lc),
                         look_back, thresh_old, buy_spread, sell_spread, lamb_da, invent_limit) {
  
  n_rows <- NROW(oh_lc)
  op_en <- oh_lc[, 1]
  hi_gh <- oh_lc[, 2]
  lo_w <- oh_lc[, 3]
  clos_e <- oh_lc[, 4]
  # weight_s <- exp(-lamb_da*1:look_back)
  # weight_s <- weight_s/sum(weight_s)
  # ew_ma <- HighFreq::roll_wsum(vec_tor=rutils::lag_it(oh_lc)[, 4], weight_s=rev(weight_s))
  # ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)

  mean_roll <- TTR::runMean(x=re_turns, n=look_back)
  median_roll <- TTR::runMedian(x=re_turns, n=look_back)
  mad_roll <- TTR::runMAD(x=re_turns, n=look_back)
  in_dic <- rutils::lag_it((mean_roll - median_roll)/mad_roll)
  in_dic <- zoo::na.locf(in_dic, na.rm=FALSE)
  in_dic <- zoo::na.locf(in_dic, na.rm=FALSE, fromLast=TRUE)
  # in_dic <- (in_dic > thresh_old)
  
  # Run the trading model (strategy):
  # bia_s <- numeric(n_rows)
  # limit prices are the low and high prices of the lagged bar, plus the spreads
  buy_limit <- rep(NA_integer_, n_rows)
  # buy_limit <- ifelse((abs(rutils::diff_it(in_dic > thresh_old)) > 0), ohlc_lag[, 3] - buy_spread, 0)
  buy_limit <- ifelse((abs(rutils::diff_it(in_dic < (-thresh_old))) > 0), ohlc_lag[, 4] - buy_spread, 0)
  buy_limit <- zoo::na.locf(buy_limit, na.rm=FALSE)
  buy_limit <- zoo::na.locf(buy_limit, na.rm=FALSE, fromLast=TRUE)
  
  sell_limit <- rep(NA_integer_, n_rows)
  # sell_limit <- ifelse((abs(rutils::diff_it(in_dic < (-thresh_old))) > 0), ohlc_lag[, 2] + sell_spread, ohlc_lag[, 2] + 1e5)
  sell_limit <- ifelse((abs(rutils::diff_it(in_dic > thresh_old)) > 0), ohlc_lag[, 4] + sell_spread, ohlc_lag[, 2] + 1e5)
  sell_limit <- zoo::na.locf(sell_limit, na.rm=FALSE)
  sell_limit <- zoo::na.locf(sell_limit, na.rm=FALSE, fromLast=TRUE)
  
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
  buy_s <- numeric(n_rows)
  buy_s[buy_ind] <- buy_limit[buy_ind]
  buy_s <- cumsum(buy_s)
  sell_s <- numeric(n_rows)
  sell_s[sell_ind] <- sell_limit[sell_ind]
  sell_s <- cumsum(sell_s)
  
  # Realized and unrealized pnls
  mark_to_market <- clos_e*(n_sells - n_buys)
  past_buys <- buy_s[match(n_sells, n_buys)]
  past_sells <- sell_s[match(n_buys, n_sells)]
  re_al <- ifelse(n_buys > n_sells,
                  sell_s - past_buys,
                  past_sells - buy_s)
  un_real <- ifelse(n_buys > n_sells,
                    past_buys - buy_s - mark_to_market,
                    sell_s - past_sells - mark_to_market)
  
  # Pnls equal to difference between filled sell minus buy prices and mark_to_market
  pnl_s <- ((sell_s-buy_s) - mark_to_market)
  # pnl_s <- cumsum(sign(in_dic)*re_turns)
  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, n_buys-n_sells, re_al, un_real)
  colnames(pnl_s)[5:8] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL")
  
  pnl_s
}  # end trade_median



###


# make_market_ewma() Function for EWMA crossover strategy
make_market_ewma <- function(oh_lc, ohlc_lag=rutils::lag_it(oh_lc), lagg,
                             # std_dev,
                             buy_spread, sell_spread, lamb_da, invent_limit,
                             look_back=100, warm_up=100) {
  # look_back <- 111
  # weight_s <- exp(-lamb_da*1:look_back)
  # weight_s <- weight_s/sum(weight_s)
  # ew_ma <- HighFreq::roll_wsum(vec_tor=oh_lc[, 4], weight_s=rev(weight_s))
  # ew_ma <- drop(ew_ma)

  # Run the trading model (strategy):

  ## Vector code

  # bia_s <- numeric(n_rows)
  # bia_s <- rutils::lag_it(ifelse(oh_lc[, 4] > ew_ma, -1, 1), lagg=lagg)

  # Pnls equal to sum
  # pnl_s <- cumsum(bia_s*rutils::diff_it(oh_lc[, 4]))
  # pnl_s <- cbind(oh_lc[, 1:4], pnl_s, ew_ma)
  # colnames(pnl_s)[5:6] <- c("Strategy PnL", "EWMA")

  ## Loop code

  n_rows <- NROW(oh_lc)
  op_en <- oh_lc[, 1]
  hi_gh <- oh_lc[, 2]
  lo_w <- oh_lc[, 3]
  clos_e <- oh_lc[, 4]
  # look_back <- 111
  # weight_s <- exp(-lamb_da*1:look_back)
  # weight_s <- weight_s/sum(weight_s)
  ew_ma <- numeric(n_rows)
  ew_ma[1] <- oh_lc[1, 6]
  z_score <- numeric(n_rows)
  # ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)
  # buy_limit <- numeric(n_rows)
  # sell_limit <- numeric(n_rows)
  n_buys <- numeric(n_rows)
  n_sells <- numeric(n_rows)
  # inventory
  inv_ent <- numeric(n_rows)
  buy_s <- numeric(n_rows)
  sell_s <- numeric(n_rows)
  re_al <- numeric(n_rows)
  un_real <- numeric(n_rows)
  pnl_s <- numeric(n_rows)

  for (it in 1:(n_rows-1)) {
    # cat("iteration:", it, "\n")
    # b_spread <- buy_spread
    # s_spread <- sell_spread
    # Spread widening rules to limit inventory risk
    # limit prices are the low and high prices of the lagged bar, plus the spreads
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

    ew_ma[it] <- lamb_da*oh_lc[it, 6] + (1-lamb_da)*ew_ma[max(it-1, 1)]
    # z_score[it] <- calc_zscore(val_ue=clos_e[it],
    #                        se_ries=clos_e[max(it-warm_up, 1):max(it-1, 1)],
    #                        de_sign, design_inv, design_2, oo_s, oos_t, deg_free)


    if (it > warm_up) {

      sell_limit <- 0
      buy_limit <- 0
      # if (clos_e[it-lagg] > ew_ma[it-lagg]) {
      if (clos_e[it-lagg] > ew_ma[it-lagg]) {
        # for limit order
        sell_limit <- (hi_gh[it] + sell_spread)
        sell_ind <- (hi_gh[it+1] > sell_limit) & (inv_ent[it] > -invent_limit)
        # for market order
        # sell_limit <- op_en[it+1]
        # sell_ind <- 1
        n_sold <- sell_ind*max(1+inv_ent[it], 0)
        n_bot <- 0
      } else {
        # for limit order
        buy_limit <- (lo_w[it] - buy_spread)
        buy_ind <- (lo_w[it+1] < buy_limit) & (inv_ent[it] < invent_limit)
        # for market order
        # buy_limit <- op_en[it+1]
        # buy_ind <- 1
        n_bot <- buy_ind*max(1-inv_ent[it], 0)
        n_sold <- 0
      }  # end if

      n_sells[it+1] <- n_sells[it] + n_sold
      sell_s[it+1] <- sell_s[it] + sell_limit*n_sold
      n_buys[it+1] <- n_buys[it] + n_bot
      buy_s[it+1] <- buy_s[it] + buy_limit*n_bot

      # Trade in the next period - but don't trade if inventory exceeds limit
      inv_ent[it+1] <- (n_buys[it+1] - n_sells[it+1])

      # Realized and unrealized pnls
      mark_to_market <- (-clos_e[it+1]*inv_ent[it+1])
      # This part takes long to run, so commented out
      # past_buys <- buy_s[match(n_sells[it], n_buys)]
      # past_sells <- sell_s[match(n_buys[it], n_sells)]
      #
      # re_al[it] <- ifelse(n_buys[it] > n_sells[it],
      #                     sell_s[it] - past_buys,
      #                     past_sells - buy_s[it])
      # un_real[it] <- ifelse(n_buys[it] > n_sells[it],
      #                       past_buys - buy_s[it] - mark_to_market,
      #                       sell_s[it] - past_sells - mark_to_market)


      # Pnls equal to difference between filled sell minus buy prices, minus mark_to_market
      pnl_s[it+1] <- ((sell_s[it+1] - buy_s[it+1]) - mark_to_market)
    }  # end for
  }  # end if
  ew_ma[n_rows] <- lamb_da*oh_lc[n_rows, 6] + (1-lamb_da)*ew_ma[n_rows-1]

  pnl_s <- cbind(oh_lc[, 1:4], pnl_s, inv_ent, re_al, un_real, ew_ma)
  colnames(pnl_s)[5:9] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")

  pnl_s
}  # end make_market_ewma


###
# calc_zscore() Function calculates z-scores of out-of-sample values relative to their predictions
calc_zscore <- function(val_ue, se_ries, de_sign, design_inv, design_2, oo_s, oos_t, deg_free) {

  if (NROW(se_ries) < deg_free) return(0)
  beta_s <- design_inv %*% se_ries
  fit_ted <- drop(de_sign %*% beta_s)
  resid_uals <- (se_ries - fit_ted)
  r_ss <- sqrt(sum(resid_uals^2)/deg_free)
  predic_tions <- cbind(predicted=drop(oo_s %*% beta_s),
                        stddev=diag(r_ss*sqrt(oo_s %*% design_2 %*% oos_t)))
  (val_ue-predic_tions[1])/predic_tions[2]
}  # end calc_zscore

