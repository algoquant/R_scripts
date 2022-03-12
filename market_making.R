# make_market_loop() Function for market making strategy - loop version
make_market <- function(ohlc, ohlc_lag=rutils::lagit(ohlc),
                        buy_spread, sell_spread, lambda, invent_limit, warm_up) {
  nrows <- NROW(ohlc)
  openp <- ohlc[, 1]
  highp <- ohlc[, 2]
  lowp <- ohlc[, 3]
  closep <- ohlc[, 4]
  # look_back <- 111
  # weightv <- exp(-lambda*1:look_back)
  # weightv <- weightv/sum(weightv)
  ew_ma <- numeric(nrows)
  ew_ma[1] <- ohlc[1, 6]
  # ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)
  bia_s <- numeric(nrows)
  # buylimit <- numeric(nrows)
  # sell_limit <- numeric(nrows)
  n_buys <- numeric(nrows)
  n_sells <- numeric(nrows)
  # inventory
  inv_ent <- numeric(nrows)
  buy_s <- numeric(nrows)
  sell_s <- numeric(nrows)
  re_al <- numeric(nrows)
  un_real <- numeric(nrows)
  pnls <- numeric(nrows)

  for (it in 1:(nrows-1)) {
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

    ew_ma[it] <- lambda*ohlc[it, 6] + (1-lambda)*ew_ma[max(it-1, 1)]

    if (it > warm_up) {
      # bia_s[it] <- (if (ohlc_lag[it, 4] > ew_ma[it]) 0.25 else -0.25)

      buylimit <- (ohlc_lag[it, 3] - buy_spread + bia_s[it])
      # buylimit should be no greater than previous close price
      buylimit <- min(closep[it], buylimit)

      sell_limit <- (ohlc_lag[it, 2] + sell_spread + bia_s[it])
      # sell_limit should be no less than previous close price
      sell_limit <- max(closep[it], sell_limit)

      # Trade in the next period - but don't trade if inventory exceeds limit
      buy_ind <- (lowp[it+1] < buylimit) & (inv_ent[it] < invent_limit)
      sell_ind <- (highp[it+1] > sell_limit) & (inv_ent[it] > -invent_limit)
      n_buys[it+1] <- n_buys[it] + buy_ind
      n_sells[it+1] <- n_sells[it] + sell_ind
      inv_ent[it+1] <- (n_buys[it+1] - n_sells[it+1])
      buy_s[it+1] <- buy_s[it] + buy_ind*buylimit
      sell_s[it+1] <- sell_s[it] + sell_ind*sell_limit

      # Realized and unrealized pnls
      mark_to_market <- (-closep[it+1]*inv_ent[it+1])
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
      pnls[it+1] <- ((sell_s[it+1] - buy_s[it+1]) - mark_to_market)
    }  # end for
  }  # end if

  pnls <- cbind(ohlc[, 1:4], pnls, inv_ent, re_al, un_real, ew_ma)
  colnames(pnls)[5:9] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")
  pnls
}  # end make_market



# make_market_vec() Function for market making strategy - vectorized version
make_market_vec <- function(ohlc, ohlc_lag=rutils::lagit(ohlc),
                            # stdev,
                            buy_spread, sell_spread, lambda, invent_limit) {
  openp <- ohlc[, 1]
  highp <- ohlc[, 2]
  lowp <- ohlc[, 3]
  closep <- ohlc[, 4]
  look_back <- 111
  weightv <- exp(-lambda*1:look_back)
  weightv <- weightv/sum(weightv)
  ew_ma <- HighFreq::roll_wsum(vectorv=rutils::lagit(ohlc)[, 4], weightv=rev(weightv))
  ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)

  # Run the trading model (strategy):
  # bia_s <- numeric(nrows)
  bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)

  # limit prices are the low and high prices of the lagged bar, plus the spreads
  buylimit <- (ohlc_lag[, 3] - buy_spread + bia_s)
  sell_limit <- (ohlc_lag[, 2] + sell_spread + bia_s)
  # lag1 <- rutils::lagit(ohlc)
  # buylimit <- (pmin(ohlc_lag[, 3], lag1[, 3]) - buy_spread)
  # sell_limit <- (pmax(ohlc_lag[, 2], lag1[, 2]) + sell_spread)

  # Buylimit should be no greater than open price openp
  buylimit <- pmin(openp, buylimit)
  # sell_limit should be no less than open price openp
  sell_limit <- pmax(openp, sell_limit)

  # Indicators of whether the orders were filled
  buy_ind <- (lowp < buylimit)
  sell_ind <- (highp > sell_limit)

  # Cumulative numbers of filled orders
  n_buys <- cumsum(buy_ind)
  n_sells <- cumsum(sell_ind)

  # Prices of the filled orders
  nrows <- NROW(ohlc)
  buy_s <- numeric(nrows)
  buy_s[buy_ind] <- buylimit[buy_ind]
  buy_s <- cumsum(buy_s)
  sell_s <- numeric(nrows)
  sell_s[sell_ind] <- sell_limit[sell_ind]
  sell_s <- cumsum(sell_s)

  # Realized and unrealized pnls
  mark_to_market <- closep*(n_sells - n_buys)
  past_buys <- buy_s[match(n_sells, n_buys)]
  past_sells <- sell_s[match(n_buys, n_sells)]
  re_al <- ifelse(n_buys > n_sells,
                  sell_s - past_buys,
                  past_sells - buy_s)
  un_real <- ifelse(n_buys > n_sells,
                    past_buys - buy_s - mark_to_market,
                    sell_s - past_sells - mark_to_market)

  # Pnls equal to difference between filled sell minus buy prices and mark_to_market
  pnls <- ((sell_s-buy_s) - mark_to_market)
  pnls <- cbind(ohlc[, 1:4], pnls, n_buys-n_sells, re_al, un_real, ew_ma)
  colnames(pnls)[5:9] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")

  pnls
}  # end make_market_vec



###


# trade_median() Function for market making strategy - vectorized version
trade_median <- function(returns, ohlc, ohlc_lag=rutils::lagit(ohlc),
                         look_back, threshold, buy_spread, sell_spread, lambda, invent_limit) {
  
  nrows <- NROW(ohlc)
  openp <- ohlc[, 1]
  highp <- ohlc[, 2]
  lowp <- ohlc[, 3]
  closep <- ohlc[, 4]
  # weightv <- exp(-lambda*1:look_back)
  # weightv <- weightv/sum(weightv)
  # ew_ma <- HighFreq::roll_wsum(vectorv=rutils::lagit(ohlc)[, 4], weightv=rev(weightv))
  # ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)

  mean_roll <- TTR::runMean(x=returns, n=look_back)
  median_roll <- TTR::runMedian(x=returns, n=look_back)
  mad_roll <- TTR::runMAD(x=returns, n=look_back)
  indic <- rutils::lagit((mean_roll - median_roll)/mad_roll)
  indic <- zoo::na.locf(indic, na.rm=FALSE)
  indic <- zoo::na.locf(indic, na.rm=FALSE, fromLast=TRUE)
  # indic <- (indic > threshold)
  
  # Run the trading model (strategy):
  # bia_s <- numeric(nrows)
  # limit prices are the low and high prices of the lagged bar, plus the spreads
  buylimit <- rep(NA_integer_, nrows)
  # buylimit <- ifelse((abs(rutils::diffit(indic > threshold)) > 0), ohlc_lag[, 3] - buy_spread, 0)
  buylimit <- ifelse((abs(rutils::diffit(indic < (-threshold))) > 0), ohlc_lag[, 4] - buy_spread, 0)
  buylimit <- zoo::na.locf(buylimit, na.rm=FALSE)
  buylimit <- zoo::na.locf(buylimit, na.rm=FALSE, fromLast=TRUE)
  
  sell_limit <- rep(NA_integer_, nrows)
  # sell_limit <- ifelse((abs(rutils::diffit(indic < (-threshold))) > 0), ohlc_lag[, 2] + sell_spread, ohlc_lag[, 2] + 1e5)
  sell_limit <- ifelse((abs(rutils::diffit(indic > threshold)) > 0), ohlc_lag[, 4] + sell_spread, ohlc_lag[, 2] + 1e5)
  sell_limit <- zoo::na.locf(sell_limit, na.rm=FALSE)
  sell_limit <- zoo::na.locf(sell_limit, na.rm=FALSE, fromLast=TRUE)
  
  # lag1 <- rutils::lagit(ohlc)
  # buylimit <- (pmin(ohlc_lag[, 3], lag1[, 3]) - buy_spread)
  # sell_limit <- (pmax(ohlc_lag[, 2], lag1[, 2]) + sell_spread)
  
  # Buylimit should be no greater than open price openp
  buylimit <- pmin(openp, buylimit)
  # sell_limit should be no less than open price openp
  sell_limit <- pmax(openp, sell_limit)
  
  # Indicators of whether the orders were filled
  buy_ind <- (lowp < buylimit)
  sell_ind <- (highp > sell_limit)
  
  # Cumulative numbers of filled orders
  n_buys <- cumsum(buy_ind)
  n_sells <- cumsum(sell_ind)
  
  # Prices of the filled orders
  buy_s <- numeric(nrows)
  buy_s[buy_ind] <- buylimit[buy_ind]
  buy_s <- cumsum(buy_s)
  sell_s <- numeric(nrows)
  sell_s[sell_ind] <- sell_limit[sell_ind]
  sell_s <- cumsum(sell_s)
  
  # Realized and unrealized pnls
  mark_to_market <- closep*(n_sells - n_buys)
  past_buys <- buy_s[match(n_sells, n_buys)]
  past_sells <- sell_s[match(n_buys, n_sells)]
  re_al <- ifelse(n_buys > n_sells,
                  sell_s - past_buys,
                  past_sells - buy_s)
  un_real <- ifelse(n_buys > n_sells,
                    past_buys - buy_s - mark_to_market,
                    sell_s - past_sells - mark_to_market)
  
  # Pnls equal to difference between filled sell minus buy prices and mark_to_market
  pnls <- ((sell_s-buy_s) - mark_to_market)
  # pnls <- cumsum(sign(indic)*returns)
  pnls <- cbind(ohlc[, 1:4], pnls, n_buys-n_sells, re_al, un_real)
  colnames(pnls)[5:8] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL")
  
  pnls
}  # end trade_median



###


# make_market_ewma() Function for EWMA crossover strategy
make_market_ewma <- function(ohlc, ohlc_lag=rutils::lagit(ohlc), lagg,
                             # stdev,
                             buy_spread, sell_spread, lambda, invent_limit,
                             look_back=100, warm_up=100) {
  # look_back <- 111
  # weightv <- exp(-lambda*1:look_back)
  # weightv <- weightv/sum(weightv)
  # ew_ma <- HighFreq::roll_wsum(vectorv=ohlc[, 4], weightv=rev(weightv))
  # ew_ma <- drop(ew_ma)

  # Run the trading model (strategy):

  ## Vector code

  # bia_s <- numeric(nrows)
  # bia_s <- rutils::lagit(ifelse(ohlc[, 4] > ew_ma, -1, 1), lagg=lagg)

  # Pnls equal to sum
  # pnls <- cumsum(bia_s*rutils::diffit(ohlc[, 4]))
  # pnls <- cbind(ohlc[, 1:4], pnls, ew_ma)
  # colnames(pnls)[5:6] <- c("Strategy PnL", "EWMA")

  ## Loop code

  nrows <- NROW(ohlc)
  openp <- ohlc[, 1]
  highp <- ohlc[, 2]
  lowp <- ohlc[, 3]
  closep <- ohlc[, 4]
  # look_back <- 111
  # weightv <- exp(-lambda*1:look_back)
  # weightv <- weightv/sum(weightv)
  ew_ma <- numeric(nrows)
  ew_ma[1] <- ohlc[1, 6]
  z_score <- numeric(nrows)
  # ew_ma <- drop(ew_ma)
  # bia_s is the spread for biasing the limit price, depending on the ew_ma
  # bia_s <- ifelse(ohlc_lag[, 4] > ew_ma, 0.25, -0.25)
  # buylimit <- numeric(nrows)
  # sell_limit <- numeric(nrows)
  n_buys <- numeric(nrows)
  n_sells <- numeric(nrows)
  # inventory
  inv_ent <- numeric(nrows)
  buy_s <- numeric(nrows)
  sell_s <- numeric(nrows)
  re_al <- numeric(nrows)
  un_real <- numeric(nrows)
  pnls <- numeric(nrows)

  for (it in 1:(nrows-1)) {
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

    ew_ma[it] <- lambda*ohlc[it, 6] + (1-lambda)*ew_ma[max(it-1, 1)]
    # z_score[it] <- calc_zscore(val_ue=closep[it],
    #                        tseries=closep[max(it-warm_up, 1):max(it-1, 1)],
    #                        design, design_inv, design2, oo_s, oos_t, deg_free)


    if (it > warm_up) {

      sell_limit <- 0
      buylimit <- 0
      # if (closep[it-lagg] > ew_ma[it-lagg]) {
      if (closep[it-lagg] > ew_ma[it-lagg]) {
        # for limit order
        sell_limit <- (highp[it] + sell_spread)
        sell_ind <- (highp[it+1] > sell_limit) & (inv_ent[it] > -invent_limit)
        # for market order
        # sell_limit <- openp[it+1]
        # sell_ind <- 1
        n_sold <- sell_ind*max(1+inv_ent[it], 0)
        n_bot <- 0
      } else {
        # for limit order
        buylimit <- (lowp[it] - buy_spread)
        buy_ind <- (lowp[it+1] < buylimit) & (inv_ent[it] < invent_limit)
        # for market order
        # buylimit <- openp[it+1]
        # buy_ind <- 1
        n_bot <- buy_ind*max(1-inv_ent[it], 0)
        n_sold <- 0
      }  # end if

      n_sells[it+1] <- n_sells[it] + n_sold
      sell_s[it+1] <- sell_s[it] + sell_limit*n_sold
      n_buys[it+1] <- n_buys[it] + n_bot
      buy_s[it+1] <- buy_s[it] + buylimit*n_bot

      # Trade in the next period - but don't trade if inventory exceeds limit
      inv_ent[it+1] <- (n_buys[it+1] - n_sells[it+1])

      # Realized and unrealized pnls
      mark_to_market <- (-closep[it+1]*inv_ent[it+1])
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
      pnls[it+1] <- ((sell_s[it+1] - buy_s[it+1]) - mark_to_market)
    }  # end for
  }  # end if
  ew_ma[nrows] <- lambda*ohlc[nrows, 6] + (1-lambda)*ew_ma[nrows-1]

  pnls <- cbind(ohlc[, 1:4], pnls, inv_ent, re_al, un_real, ew_ma)
  colnames(pnls)[5:9] <- c("Strategy PnL", "Inventory", "Realized PnL", "Unrealized PnL", "EWMA")

  pnls
}  # end make_market_ewma


###
# calc_zscore() Function calculates z-scores of out-of-sample values relative to their predictions
calc_zscore <- function(val_ue, tseries, design, design_inv, design2, oo_s, oos_t, deg_free) {

  if (NROW(tseries) < deg_free) return(0)
  betas <- design_inv %*% tseries
  fit_ted <- drop(design %*% betas)
  residuals <- (tseries - fit_ted)
  r_ss <- sqrt(sum(residuals^2)/deg_free)
  predic_tions <- cbind(predicted=drop(oo_s %*% betas),
                        stddev=diag(r_ss*sqrt(oo_s %*% design2 %*% oos_t)))
  (val_ue-predic_tions[1])/predic_tions[2]
}  # end calc_zscore

