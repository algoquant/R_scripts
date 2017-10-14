####################################
### file stat_arb_func.R
### Functions for performing regressions and simulating pairs trading

### function upload_data() reads time series data from csv files
upload_data <- function(file_names=c("XLU.csv", "XLP.csv"), 
                        data_dir="C:/Develop/data", 
                        date_fun= match.fun("as.Date"), 
                        for_mat="%m/%d/%Y") {
  da_ta <- lapply(file_names, function(file_name) {
    xts::as.xts(zoo::read.zoo(file=file.path(data_dir, file_name), 
                              header=FALSE, sep=",", 
                              FUN=date_fun, 
                              format=for_mat))
  })  # end lapply
  da_ta <- rutils::do_call(cbind, da_ta)
  colnames(da_ta) <- sapply(file_names, 
                            function(file_name) strsplit(file_name, split=" ")[[1]][1])
  da_ta
}  # end upload_data


### function calc_reg() performs time series regressions and 
### calculates hedge ratios
calc_reg <- function(prices_ts, 
                     name_x, name_y, 
                     flag_hr_type = 0, flag_print = FALSE) {
  
  # JP to-do: use switch() instead of if()
  # JP to-do: add regression with fixed intercept:
  # https://stackoverflow.com/questions/7333203/linear-regression-with-a-known-fixed-intercept-in-r
  
  if (flag_hr_type == 0) {
    # regress y vs x with free intercept
    
    reg_formula <- as.formula(paste0(name_y, " ~ ", name_x))
    reg_model <- lm(formula = reg_formula, data = prices_ts)
    x11(width=14,height=7)
    plot(reg_formula, data=prices_ts)
    abline(reg_model, lwd=2, col="red")
    hedge_ratio <- unname(coef(reg_model)[2])
    if (flag_print) { 
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n", 
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n", 
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n", 
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }
    return(list(reg_summary = summary(reg_model), 
                reg_formula = reg_formula, 
                hedge_ratio = hedge_ratio, 
                r_squared = summary(reg_model)$r.squared))
    
  } else if (flag_hr_type == (-1)) { 
    # regress y vs x forcing the line to cross the origin
    
    reg_formula <- as.formula(paste0(name_y, " ~ ", name_x, " - 1"))
    reg_model <- lm(formula = reg_formula, data = prices_ts)
    x11(width=14,height=7)
    plot(reg_formula, data=prices_ts)
    abline(reg_model, lwd=2, col="red")
    hedge_ratio <- unname(coef(reg_model)[1])
    if (flag_print) { 
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n", 
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n", 
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n", 
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }
    return(list(reg_summary = summary(reg_model), 
                reg_formula = reg_formula, 
                hedge_ratio = hedge_ratio, 
                r_squared = summary(reg_model)$r.squared))
    
  } else if (flag_hr_type == 1) {
    # perpendicular regression or total least square fit
    # https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca
    # https://robotwealth.com/exploring-mean-reversion-and-cointegration-part-2/
    
    reg_model <- prcomp(cbind(prices_ts[, name_x], prices_ts[, name_y]))$rotation
    hedge_ratio <- reg_model[2, 1] / reg_model[1, 1]
    
    if (flag_print) { 
      #print(summary(reg_model))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    return(list(reg_summary = summary(reg_model), 
                hedge_ratio = hedge_ratio))
    
  } else if (flag_hr_type == 2) {
    
    # no drift hedge ratio using the index (best for most cases but sometimes blows up)

    # regress x vs index
    reg_formula_x <- paste0(name_x, " ~ ", "index")
    reg_model_x <- lm(formula = reg_formula_x, data = prices_ts)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    
    # regress y vs index
    reg_formula_y <- paste0(name_y, " ~ ", "index")
    reg_model_y <- lm(formula = reg_formula_y, data = prices_ts)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    
    if (flag_print) { 
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    return(list(reg_summary_x = summary(reg_model_x), 
                reg_summary_y = summary(reg_model_y), 
                hedge_ratio = hedge_ratio))
    
  } else if (flag_hr_type == 3) {
    index_dates <- xts::.index(prices_ts)
    
    # regress x vs index_dates
    reg_model_x <- lm(prices_ts[, name_x] ~ index_dates)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    
    # regress y vs dates
    reg_model_y <- lm(prices_ts[, name_y] ~ index_dates)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    
    if (flag_print) { 
      # print(summary(reg_model_x))
      # print(summary(reg_model_y))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    return(list(reg_summary_x = summary(reg_model_x), 
                reg_summary_y = summary(reg_model_y), 
                hedge_ratio = hedge_ratio))
  } else {
    return(NULL)
  }  # end if
}  # end calc_reg


### function jk_hedge_ratio() calculates the hedge ratio for a pair of instruments
jk_hedge_ratio <- function (prices_ts_x, prices_ts_y, 
                            flag_hr_type = 1, 
                            flag_print = FALSE, 
                            flag_plot = FALSE) {
  
  # regress y vs x forcing the line to cross the origin
  if (flag_hr_type == 0) { 
    reg_formula <- as.formula(prices_ts_y ~ prices_ts_x - 1)
    reg_model <- lm(reg_formula)
    if (flag_print) {
      da_ta = cbind(prices_ts_x, prices_ts_y)
      plot(reg_formula, da_ta, 
           main=paste("Regression of", 
                      paste(colnames(da_ta), collapse=" versus ")))
      abline(reg_model, lwd=2, col="red")
      summary(reg_model)
    }
    hedge_ratio <- unname(coef(reg_model)[1])
    if (flag_print) { 
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n", 
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n", 
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n", 
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }
    return(list(reg_summary = summary(reg_model), 
                reg_formula = reg_formula, 
                hedge_ratio = hedge_ratio, 
                r_squared = summary(reg_model)$r.squared))
    
  } else if (flag_hr_type == 1) {
    # regress y vs x with free intercept
    
    reg_formula <- as.formula(paste0(name_y, " ~ ", name_x))
    reg_model <- lm(formula = reg_formula, data = prices_ts)
    x11(width=14,height=7)
    plot(reg_formula, data=prices_ts)
    abline(reg_model, lwd=2, col="red")
    hedge_ratio <- unname(coef(reg_model)[2])
    if (flag_print) { 
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n", 
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n", 
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n", 
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }
    return(list(reg_summary = summary(reg_model), 
                reg_formula = reg_formula, 
                hedge_ratio = hedge_ratio, 
                r_squared = summary(reg_model)$r.squared))

  } else if (flag_hr_type == 2) {
    # perpendicular regression or total least square fit
    # https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca
    # https://robotwealth.com/exploring-mean-reversion-and-cointegration-part-2/
    
    reg_model <- prcomp(cbind(prices_ts[, name_x], prices_ts[, name_y]))$rotation
    hedge_ratio <- reg_model[2, 1] / reg_model[1, 1]
    
    if (flag_print) { 
      #print(summary(reg_model))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    return(list(reg_summary = summary(reg_model), 
                hedge_ratio = hedge_ratio))
    
  } else if (flag_hr_type == 3) {
    
    # no drift hedge ratio using the index (best for most cases but sometimes blows up)
    # JP: does it blow up because hedge_ratio_x==0 ?
    
    # regress x vs index
    reg_formula_x <- paste0(name_x, " ~ ", "index")
    reg_model_x <- lm(formula = reg_formula_x, data = prices_ts)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    
    # regress y vs index
    reg_formula_y <- paste0(name_y, " ~ ", "index")
    reg_model_y <- lm(formula = reg_formula_y, data = prices_ts)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    
    if (flag_print) { 
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    return(list(reg_summary_x = summary(reg_model_x), 
                reg_summary_y = summary(reg_model_y), 
                hedge_ratio = hedge_ratio))
    
  } else if (flag_hr_type == 4) {
    index_dates <- xts::.index(prices_ts)
    reg_model_x <- lm(prices_ts[, name_x] ~ index_dates)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    reg_model_y <- lm(prices_ts[, name_y] ~ index_dates)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    
    if (flag_print) { 
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    return(list(reg_summary_x = summary(reg_model_x), 
                reg_summary_y = summary(reg_model_y), 
                hedge_ratio = hedge_ratio))
  } else {
    return(NULL)
  }  # end if
}  # end jk_hedge_ratio



### function trade_pair() simulates pairs trading
# JP: renamed calc_pair_stat() to trade_pair()
trade_pair <- function (prices_ts, 
                        name_x, 
                        name_y, 
                        trading_levels = list(z_long_enter = -1.5, 
                                              z_long_profit = 0, 
                                              z_long_stop = -2.5, 
                                              z_short_enter = 1.5, 
                                              z_short_profit = 0, 
                                              z_short_stop = 2.5), 
                        flag_hr_type = 0, 
                        flag_sticky_stops = TRUE, 
                        flag_print = FALSE, 
                        flag_plot = FALSE) {
  
  n_row <- NROW(prices_ts)
  index_dates <- zoo::index(prices_ts)
  
  results_val <- list()
  results_ts <- prices_ts[, "index"]
  # perform time series regression and calculate hedge ratio
  results_reg <- calc_reg(prices_ts = prices_ts, 
                          name_x = name_x, 
                          name_y = name_y, 
                          flag_hr_type, 
                          flag_print)
  
  # calculate hedge portfolio
  port_price <- prices_ts[, name_y] - results_reg$hedge_ratio * prices_ts[, name_x]
  colnames(port_price) <- "port_price"
  results_ts <- cbind(results_ts, port_price)
  
  port_price_mean <- mean(port_price)
  port_price_sd <- sd(port_price)
  results_val <- c(results_val, "port_price_mean" = port_price_mean, 
                   "port_price_sd" = port_price_sd)
  
  # calculate hedge portfolio z-score
  port_z <- (port_price - port_price_mean) / port_price_sd
  colnames(port_z) <- "port_z"
  results_ts <- cbind(results_ts, port_z)
  z_val <- as.numeric(port_z)

  ## model parameters
  # JP to-do: wrap code in with(trading_levels)
  z_long_enter <- trading_levels$z_long_enter
  z_long_profit <- trading_levels$z_long_profit
  z_long_stop <- trading_levels$z_long_stop
  z_short_enter <- trading_levels$z_short_enter
  z_short_profit <- trading_levels$z_short_profit
  z_short_stop <- trading_levels$z_short_stop
  
  if (flag_sticky_stops) {
    
    ### regular model with sticky stops
    # simulate model long positions
    pos_long_stop <- rep(NA_integer_, n_row)
    pos_long_stop[1] <- (z_val[1] < z_long_stop)
    pos_long_stop[z_val < z_long_stop] <- TRUE
    pos_long_stop[z_val > z_long_enter] <- FALSE
    pos_long_stop <- zoo::na.locf(pos_long_stop)
    pos_long_enter <- (rutils::diff_it(z_val < z_long_enter) > 0) | 
      (rutils::diff_it(pos_long_stop) < 0)
    pos_long_profit <- (rutils::diff_it(z_val > z_long_profit) > 0)
    pos_long <- rep(NA_integer_, n_row)
    pos_long[1] <- ((z_val[1] < z_long_enter) & !pos_long_stop[1])
    pos_long[pos_long_enter & !pos_long_stop] <- 1
    pos_long[pos_long_profit] <- 0
    pos_long[as.logical(pos_long_stop)] <- 0
    pos_long <- zoo::na.locf(pos_long)
    
    #cbind(round(z_val[pos_long_stop == 1], 2), pos_long_stop[pos_long_stop == 1])
    #cbind(round(z_val, 2), z_val < z_long_enter, rutils::diff_it(z_val < z_long_enter) > 0, pos_long_stop, rutils::diff_it(pos_long_stop) < 0, pos_long_enter, (z_val > z_long_profit)>0, pos_long_profit, pos_long)
    
    # simulate model short positions
    pos_short_stop <- rep(NA_integer_, n_row)
    pos_short_stop[1] <- (z_val[1] > z_short_stop)
    pos_short_stop[z_val > z_short_stop] <- TRUE
    pos_short_stop[z_val < z_short_enter] <- FALSE
    pos_short_stop <- zoo::na.locf(pos_short_stop)
    pos_enter_short <- (rutils::diff_it(z_val > z_short_enter) > 0) | 
      (rutils::diff_it(pos_short_stop) < 0)
    pos_short_profit <- (rutils::diff_it(z_val<z_short_profit) > 0)
    pos_short <- rep(NA_integer_, n_row)
    pos_short[1] <- ((z_val[1] > z_short_enter) & !pos_short_stop[1])
    pos_short[pos_enter_short & !pos_short_stop] <- (-1)
    pos_short[pos_short_profit] <- 0
    pos_short[as.logical(pos_short_stop)] <- 0
    pos_short <- zoo::na.locf(pos_short)
    
    # total positions
    pos_all <- pos_long + pos_short
    
  } else {
    
    ### simpler model without sticky stops
    # simulate model long positions
    
    pos_long_enter <- (rutils::diff_it(z_val < z_long_enter) > 0) | 
      ((rutils::diff_it(z_val < z_long_stop) < 0) & (z_val < z_long_enter))
    pos_long_profit <- (rutils::diff_it(z_val > z_long_profit) > 0)
    pos_long_stop <- (rutils::diff_it(z_val < z_long_stop) > 0)
    pos_long <- rep(NA_integer_, n_row)
    pos_long[1] <- ((z_val[1] < z_long_enter) & (z_val[1] > z_long_stop))
    pos_long[pos_long_enter & !pos_long_stop] <- 1
    pos_long[pos_long_profit] <- 0
    pos_long[pos_long_stop] <- 0
    pos_long <- zoo::na.locf(pos_long)
    
    # simulate model short positions
    pos_short_enter <- (rutils::diff_it(z_val > z_short_enter) > 0) | 
      ((rutils::diff_it(z_val > z_short_stop) < 0) & (z_val > z_short_enter))
    pos_short_profit <- (rutils::diff_it(z_val < z_short_profit) > 0)
    pos_short_stop <- (rutils::diff_it(z_val > z_short_stop) > 0)
    pos_short <- rep(NA_integer_, n_row)
    pos_short[1] <- ((z_val[1] > z_short_enter) & (z_val[1] < z_short_stop))
    pos_short[pos_short_enter & !pos_short_stop] <- -1
    pos_short[pos_short_profit] <- 0
    pos_short[pos_short_stop] <- 0
    pos_short <- zoo::na.locf(pos_short)
    
    # total positions
    pos_all <- pos_long + pos_short
  }  # end if
  
  ### calculate portfolio timeseries statistics
  port_pos <- xts::xts(pos_all, order.by = index_dates)
  # port_pos <- port_z * 0 + pos_all
  colnames(port_pos) <- "port_pos"
  
  # p&l
  port_pnl <- rutils::lag_xts(port_pos) * (rutils::diff_xts(prices_ts[, name_y]) - 
       results_reg$hedge_ratio * rutils::diff_xts(prices_ts[, name_x])) 
  colnames(port_pnl) <- "port_pnl"
  
  # absolute market value (sum of market values of longs and shorts)
  port_amv <- port_pnl
  port_amv[1] <- prices_ts[1, name_y] + results_reg$hedge_ratio * prices_ts[1, name_x]
  port_amv <- cumsum(port_amv)
  # JP: I prefer to write all commands in separate lines
  colnames(port_amv) <- "port_amv"
  
  # returns
  port_ret <- port_pnl / rutils::lag_xts(port_amv)
  colnames(port_ret) <- "port_ret"
  
  # cumulative returns
  port_cumret <- cumprod(1 + port_ret) - 1
  colnames(port_cumret) <- "port_cumret"
  
  # high watermark
  # JP: this works without loop, provided port_cumret[0]==0
  port_hwm <- cummax(port_cumret)
  colnames(port_hwm) <- "port_hwm"
  
  # drawdown
  port_draw <- (1 + port_cumret) / (1 + port_hwm) - 1
  colnames(port_draw) <- "port_draw"
  
  # drawdown duration
  # JP: this is a vectorized version of the loop, but with more R gymnastics than I would prefer
  port_drawdur <- -sign(port_draw)
  cum_sum <- cumsum(port_drawdur)
  ze_ro <- rutils::diff_xts(port_drawdur)
  # sum(ze_ro==(-1))
  whi_ch <- which(ze_ro==(-1))
  which_cumsum <- cum_sum[whi_ch]
  # head(cbind(port_drawdur, cum_sum, ze_ro, which_cumsum),200)
  lag_which_cumsum <- rutils::lag_xts(which_cumsum)
  lag_which_cumsum[1] <- 0
  port_drawdur[whi_ch] <- (lag_which_cumsum - which_cumsum)
  port_drawdur <- cumsum(port_drawdur)
  colnames(port_drawdur) <- "port_drawdur"
  # head(cbind(cum_sum,ze_ro,port_drawdur),50)
  
  results_ts <- cbind(results_ts, 
                      port_pos, 
                      port_pnl, 
                      port_amv, 
                      port_ret, 
                      port_cumret, 
                      port_hwm, 
                      port_draw, 
                      port_drawdur)
  
  ## calculate portfolio values statistics
  
  port_return <- unname(port_cumret[n_row])  # return over the period
  port_volatility <- sqrt(n_row - 1) * sd(port_ret[-1])  # volatility over the period
  port_sharpe_ratio <- sqrt(n_row - 1) * mean(port_ret[-1]) / sd(port_ret[-1]) # Sharpe ratio
  port_sharpe_ratio_dollar <- sqrt(n_row - 1) * mean(port_pnl[-1]) / sd(port_pnl[-1]) # dollar Sharp ratio
  port_maxdraw <- min(port_draw) # maximum drawdown
  port_maxdraw_dur <- max(port_drawdur) # maximum drawdown duration
  port_num_trades <- NROW(pos_all[abs(pos_all - rutils::lag_it(pos_all))]) # number of trades
  
  results_val <- c(results_val, 
                   port_return = port_return, 
                   port_volatility = port_volatility, 
                   port_sharpe_ratio = port_sharpe_ratio, 
                   port_sharpe_ratio_dollar = port_sharpe_ratio_dollar, 
                   port_maxdraw = port_maxdraw, 
                   port_maxdraw_dur = port_maxdraw_dur)
  
  # JP: plot residuals of basket and cumulative returns
  if (flag_plot) {
    # plot residuals of basket
    reg_model_plot <- lm(port_z ~ results_ts[, "index"])
    x11(width=14,height=7)
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    plot(quantmod::chart_Series(port_z, theme=plot_theme, name=NULL))
    title(main="Residuals of basket, with buys and sells", 
          adj=0.1, cex=1.5)
    # add regression line
    abline(reg_model_plot, lwd=2, col="red")
    # add vertical lines
    # abline(v=which.min(VTI_vwap), col='red')
    # add points for buys and sells
    buys_sells <- rutils::diff_it(pos_all)
    which_buys <- which(buys_sells==1)
    # add a solid red point (pch=16) for the last car
    points(x=which_buys,
           y=port_z[which_buys],
           col="green", pch=19, lwd=6)
    which_sells <- which(buys_sells==(-1))
    points(x=which_sells,
           y=port_z[which_sells],
           col="red", pch=19, lwd=6)
    text(x=0.9*max(NROW(port_z)), y=0.8*max(port_z),
         labels=paste0("y = ",
                       round(coef(reg_model_plot)[2], 4), "*x", 
                       round(coef(reg_model_plot)[1], 4), "\nR^2 = ",
                       round(summary(reg_model_plot)$r.squared, 4)),
         pos=2, cex=1.5)
    # add background shading of areas
    # plot(add_TA(port_pos > 0, on=-1, 
    #             col="lightgreen", border="lightgreen"))
    # plot(add_TA(port_pos < 0, on=-1, 
    #             col="lightpink", border="lightpink"))
    # plot(add_TA(port_pos == 0, on=-1, 
    #             col="lightgrey", border="lightgrey"))
    
    # plot cumulative returns
    x11(width=14,height=7)
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    plot(quantmod::chart_Series(port_cumret, theme=plot_theme, 
                                name="Cumulative returns"))
    points(x=which_buys,
           y=port_cumret[which_buys],
           col="green", pch=19, lwd=6)
    points(x=which_sells,
           y=port_cumret[which_sells],
           col="red", pch=19, lwd=6)
    
  }  # end if
  
  if (flag_print) {
    cat(paste0("return: \t", round(port_return * 100, digits = 2), "%\n", 
               "volatility: \t", round(port_volatility * 100, digits = 2), "%\n", 
               "sharpe ratio: \t", round(port_sharpe_ratio, digits = 6), "\n", 
               #               "sh_rt_dollar: \t", round(port_sharpe_ratio_dollar, digits = 6), "\n", 
               #               "sh_rt_ratio: \t", round(port_return / port_volatility, digits = 6), "\n", 
               "max draw: \t", round(port_maxdraw * 100, digits = 2), "%\n", 
               "max draw dur: \t", port_maxdraw_dur, "\n", 
               "# trades: \t", port_num_trades, "\n"))
  }  # end if
  
  results <- list(results_val = results_val, 
                  results_ts = results_ts, 
                  results_reg = results_reg)
  
  return(results)
}  # end trade_pair



