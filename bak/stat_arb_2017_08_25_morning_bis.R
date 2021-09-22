### stat-arb script
library(quantmod)
library(plotly)

### read data from csv files
prices_ts_all <- upload_data(c("XLU Prices 26052017.csv", "XLP Prices 26052017.csv"))
#head(prices_ts_all)
#NROW(prices_ts_all)
sum(is.na(prices_ts_all))

### cut historical window of prices
start_date <- "2015-05-29"
end_date <- "2016-05-26"
prices_ts <- prices_ts_all[paste(start_date, end_date, sep="/")]
prices_ts <- cbind(1:NROW(prices_ts), prices_ts)
colnames(prices_ts)[1] <- "index"
#head(prices_ts)
#NROW(prices_ts)

### initial parameters
name_x <- "XLU"
name_y <- "XLP"
flag_calc_type <- 0  # 0 (default) ols, 1 tls, 2 no drift index
flag_sticky_stops <- TRUE  # use sticky stops, FALSE use regular stops 
flag_print <- TRUE
flag_plot <- TRUE

# JP: no quotes needed for list element names
# trading_levels <- numeric(6)
trading_levels <- list(z_long_enter = -1.5, 
                       z_long_profit = 0, 
                       z_long_stop = -2.5, 
                       z_short_enter = 1.5, 
                       z_short_profit = 0, 
                       z_short_stop = 2.5)

results <- calc_pair_stat(prices_ts = prices_ts, 
                          name_x = name_x, 
                          name_y = name_y, 
                          trading_levels = trading_levels, 
                          flag_calc_type = flag_calc_type, 
                          flag_sticky_stops = flag_sticky_stops, 
                          flag_print = flag_print, 
                          flag_plot = flag_plot)

cat(paste0("sharpe ratio: \t", round(results$results_val$port_sharpe_ratio, digits = 6)))


### this function calculates statistics on a pair
calc_pair_stat <- function (prices_ts, 
                            name_x, 
                            name_y, 
                            trading_levels = list(z_long_enter = -1.5, 
                                                  z_long_profit = 0, 
                                                  z_long_stop = -2.5, 
                                                  z_short_enter = 1.5, 
                                                  z_short_profit = 0, 
                                                  z_short_stop = 2.5), 
                            flag_calc_type = 0, 
                            flag_sticky_stops = TRUE, 
                            flag_print = FALSE, 
                            flag_plot = FALSE) {
  
  n_row <- NROW(prices_ts)
  in_dex <- zoo::index(prices_ts)
  
  # Jerzy is this best way to initialise list?
  # JP: no need to initialise
  results_val <- list()
  results_ts <- prices_ts[, "index"]
  results_reg <- calc_reg(prices_ts = prices_ts, 
                          name_x = name_x, 
                          name_y = name_y, 
                          flag_calc_type, 
                          flag_print)

  port_price <- prices_ts[, name_y] - results_reg$hedge_ratio * prices_ts[, name_x]
  colnames(port_price) <- "port_price"
  results_ts <- cbind(results_ts, port_price)

  port_price_mean <- mean(port_price)
  port_price_sd <- sd(port_price)
  results_val <- c(results_val, "port_price_mean" = port_price_mean, 
                                "port_price_sd" = port_price_sd)

  port_z <- (port_price - port_price_mean) / port_price_sd
  colnames(port_z) <- "port_z"
  results_ts <- cbind(results_ts, port_z)

  # Jerzy, below is a band-aid solution, this should be improved
  # below is the conversion from timeseries type to vector
  # JP: use as.numeric()
  z_val <- as.numeric(port_z)
  # z_val <- numeric(n_row)
  # for(i in 1:NROW(z_val)) {
  #   z_val[i] <- port_z[i]
  # }

  # JP to-do: wrap code in with(trading_levels)
  ## model parameters
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
  # Jerzy there should be better way to initialize this
  # this is to convert vector type (pos_all) to timeseries type (port_pos)
  # convert portfolio position from index to timeseries form
  # JP: use xts::xts()
  port_pos <- xts::xts(pos_all, order.by = in_dex)
  # port_pos <- port_z * 0 + pos_all
  colnames(port_pos) <- "port_pos"
  
  # p&l
  # Jerzy, there should be a way to calculate using port_pos (time series), not pos_all (index)
  # JP: yes, done
  port_pnl <- rutils::lag_xts(port_pos) * (rutils::diff_xts(prices_ts[, name_y]) - 
              results_reg$hedge_ratio * rutils::diff_xts(prices_ts[, name_x])) 
  colnames(port_pnl) <- "port_pnl"

  # absolute market value (sum of market values of longs and shorts)
  port_amv <- port_pnl
  port_amv[1] <- prices_ts[1, name_y] + results_reg$hedge_ratio * prices_ts[1, name_x]
  port_amv <- cumsum(port_amv)
  # JP: I prefer to print all commands in separate lines
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
  # port_hwm <- xts(rep(NA_real_, n_row), order.by=zoo::index(port_pnl))
  # port_hwm[1] <- 0
  # for (i in 2:n_row) {
  #   port_hwm[i] <- max(port_hwm[i-1], port_cumret[i])
  # }
  colnames(port_hwm) <- "port_hwm"

  # drawdown
  port_draw <- (1 + port_cumret) / (1 + port_hwm) - 1
  colnames(port_draw) <- "port_draw"

  #drawdown duration
  # port_drawdur <- xts(rep(NA_integer_, n_row), order.by=zoo::index(port_pnl))
  # port_drawdur[1] <- 0
  # for (i in 2:n_row) {
  #   if (port_draw[i] == 0) {
  #     port_drawdur[i] <- 0
  #   } else {
  #     port_drawdur[i] <- port_drawdur[i-1] + 1
  #   }
  # }
  # JP: this is a vectorized version of above loop, but with more R gymnastics than I would prefer
  port_drawdur <- -sign(port_draw)
  cum_sum <- cumsum(port_drawdur)
  ze_ro <- rutils::diff_xts(port_drawdur)
  # sum(ze_ro==(-1))
  whi_ch <- which(ze_ro==(-1))
  which_cumsum <- cum_sum[whi_ch]
  lag_which_cumsum <- rutils::lag_xts(which_cumsum)
  lag_which_cumsum[1] <- 0
  port_drawdur[whi_ch] <- (lag_which_cumsum - which_cumsum)
  port_drawdur <- cumsum(port_drawdur)
  colnames(port_drawdur) <- "port_drawdur"

  # Jerzy what is the best to use here, cbind, cbind.xts, merge or merge.xts
  # JP: cbind() is best
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

  # JP: no need for quotes in names
  results_val <- c(results_val, 
                   port_return = port_return, 
                   port_volatility = port_volatility, 
                   port_sharpe_ratio = port_sharpe_ratio, 
                   port_sharpe_ratio_dollar = port_sharpe_ratio_dollar, 
                   port_maxdraw = port_maxdraw, 
                   port_maxdraw_dur = port_maxdraw_dur)

  # if (flag_plot) {plot(port_z, results_ts[, "index"])}
  # JP: plot residuals of basket and cumulative returns
  if (flag_plot) {
    # plot residuals of basket
    reg_model <- lm(port_z ~ results_ts[, "index"])
    x11()
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    quantmod::chart_Series(port_z, theme=plot_theme, 
                           name="Residuals of basket, with buys and sells")
    abline(reg_model, lwd=2, col="red")
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
    text(x=NROW(port_z)-10, y=0.8*max(port_z),
         labels=paste0("y = ",
                       round(coef(reg_model)[2], 4), "*x", 
                       round(coef(reg_model)[1], 4), "\nR^2 = ",
                       round(summary(reg_model)$r.squared, 4)),
         pos=2, cex=1.5)
    
    # plot cumulative returns
    x11()
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    quantmod::chart_Series(port_cumret, theme=plot_theme, 
                           name="Cumulative returns")
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
}  # end calc_pair_stat


### Below are the functions used

# this function needs some more work to make it more flexible
# JP: added file.path()
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

### Calculate regressions
calc_reg <- function(prices_ts, name_x, name_y, flag_calc_type = 0, flag_print = FALSE) {
  
  # JP to-do: use switch() instead of if()
  # JP to-do: add regression with fixed intercept:
  # https://stackoverflow.com/questions/7333203/linear-regression-with-a-known-fixed-intercept-in-r
  
  if (flag_calc_type == 0) {
    # regress y vs x with free intercept
    
    reg_formula <- as.formula(paste0(name_y, " ~ ", name_x))
    reg_model <- lm(formula = reg_formula, data = prices_ts)
    x11()
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
    results_reg <- list(reg_summary = summary(reg_model), 
                        reg_formula = reg_formula, 
                        hedge_ratio = hedge_ratio, 
                        r_squared = summary(reg_model)$r.squared)
    return(results_reg)
    
  } else if (flag_calc_type == (-1)) { 
    # regress y vs x forcing the line to cross the origin
    
    reg_formula <- as.formula(paste0(name_y, " ~ ", name_x, " - 1"))
    reg_model <- lm(formula = reg_formula, data = prices_ts)
    x11()
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
    results_reg <- list(reg_summary = summary(reg_model), 
                        reg_formula = reg_formula, 
                        hedge_ratio = hedge_ratio, 
                        r_squared = summary(reg_model)$r.squared)
    return(results_reg)
    
  } else if (flag_calc_type == 1) {
    # perpendicular regression or total least square fit
    # https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca
    # https://robotwealth.com/exploring-mean-reversion-and-cointegration-part-2/
    
    reg_model <- prcomp(cbind(prices_ts[, name_x], prices_ts[, name_y]))$rotation
    hedge_ratio <- reg_model[2, 1] / reg_model[1, 1]
    
    if (flag_print) { 
      #print(summary(reg_model))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    results_reg <- list(reg_summary = summary(reg_model), 
                        hedge_ratio = hedge_ratio)
    return(results_reg)
    
  } else if (flag_calc_type == 2) {
    
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
    results_reg <- list(reg_summary_x = summary(reg_model_x), 
                        reg_summary_y = summary(reg_model_y), 
                        hedge_ratio = hedge_ratio)
    return(results_reg)
    
  } else if (flag_calc_type == 3) {
    # Jerzy, this part does not work
    # no drift hedge ratio using dates (more accurate) (best for most cases but sometimes blows up)
    # JP: it was blowing up because there was no "dates" column in prices_ts
    # in_dex is numeric, not dates
    in_dex <- xts::.index(prices_ts)
    
    # regress x vs dates
    # reg_formula_x <- paste0(name_x, " ~ ", "dates")
    # reg_model_x <- lm(formula = reg_formula_x, data = prices_ts)
    reg_model_x <- lm(prices_ts[, name_x] ~ in_dex)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    
    # regress y vs dates
    # reg_formula_y <- paste0(name_y, " ~ ", "dates")
    # reg_model_y <- lm(formula = reg_formula_y, data = prices_ts)
    reg_model_y <- lm(prices_ts[, name_y] ~ in_dex)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    
    if (flag_print) { 
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }
    results_reg <- list(reg_summary_x = summary(reg_model_x), 
                        reg_summary_y = summary(reg_model_y), 
                        hedge_ratio = hedge_ratio)
    return(results_reg)
  } else {
    return(NULL)
  }  # end if
}  # end calc_reg


### ignore below

# plot interactive dygraphs plot
##dygraph(port_cumret, main="Cumulative PnL") %>%
##  dyOptions(colors=c("orange", "blue")) %>%
##  dyRangeSelector()

#z_step <- 0.1
#n_step <- -level_long_stop / z_step
#sharpe_ratio_matrix <- matrix(0, nrow = n_step, ncol = n_step)
#level_enter_long_vector <- level_long_stop + z_step * 1:n_step
#profit_long_vector <- level_long_stop + z_step * 1:n_step

#for(index_level_enter_long in 1:n_step) {
#  level_long_enter <- level_enter_long_vector[index_level_enter_long]
#  print(index_level_enter_long)

#for(level_long_profit in seq(level_long_enter + z_step, 0, by = z_step)) {
#for(index_profit_long in (index_level_enter_long +1):n_step) {
#  if (index_profit_long == index_level_enter_long) next
#  level_long_profit <- profit_long_vector[index_profit_long]
#  print(index_profit_long)



#results_reg <- calc_reg(prices_ts = prices_ts, name#print(c("z_long_enter = ", z_long_enter, "z_long_profit = ", z_long_profit, "sharpe_ratio = ", round(sharpe_ratio, 4)))
#sharpe_ratio_matrix[index_level_enter_long, index_profit_long] <- sharpe_ratio

#}

#}

#plot_ly(x = profit_long_vector, y = level_enter_long_vector, z = sharpe_ratio_matrix, type = "contour")
#plot_ly(x = ~profit_long_vector, y = ~level_enter_long_vector, z = ~sharpe_ratio_matrix, type = "contour")
#cat(sprintf("%s %.2f %s %.2f %s %.6f \n", "z_long_enter=", z_long_enter, 
#            "z_long_profit=", z_long_profit, 
#            "sharpe_ratio=", sharpe_ratio))

#results_reg <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_calc_type = -1)
#results_reg <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_calc_type = 2)
#results_reg <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_calc_type = 1)

#hr_ols <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_calc_type = FALSE)
#port_ols <- prices_ts[, name_y] - hr_ols * prices_ts[, name_x]
#colnames(port_ols) <- "port_ols"
#prices_ts <- cbind(prices_ts, port_ols)
#head(prices_ts)

#hr_ndr <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_calc_type = 2)
#port_ndr <- prices_ts[, name_y] - hr_ndr * prices_ts[, name_x]
#colnames(port_ndr) <- "port_ndr"
#prices_ts <- cbind(prices_ts, port_ndr)
#head(prices_ts)

#hr_tls <- calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = name_y, flag_calc_type = 1)
#port_tls <- prices_ts[, name_y] - hr_tls * prices_ts[, name_x]
#colnames(port_tls) <- "port_tls"
#prices_ts <- cbind(prices_ts, port_tls)
#head(prices_ts)

#calc_reg(prices_ts = prices_ts, name_x = "index", name_y = "port_ols", flag_calc_type = FALSE, flag_print = 1)
#calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = "port_ols", flag_calc_type = FALSE, flag_print = 1)
#calc_reg(prices_ts = prices_ts, name_x = "index", name_y = "port_ndr", flag_calc_type = FALSE, flag_print = 1)
#calc_reg(prices_ts = prices_ts, name_x = name_x, name_y = "port_ndr", flag_calc_type = 2, flag_print = 1)

### plot interactive dygraphs plot with data range sector
#library(dygraphs)
#dygraph(prices_ts_all, main="XLP and XLU prices") %>%
#dyOptions(colors=c("orange", "blue")) %>%
#dyRangeSelector()

### or static plot with custom line colors
#plot_theme$col$line.col <- c("orange", "blue")
#x11(width=8, height=6)
#chart_Series(prices_ts_all, theme=plot_theme, 
#             name="XLP and XLU prices")
#legend("top", legend=colnames(prices_ts_all), cex=0.8, 
#       inset=0.1, bg="white", lty=c(1, 1), lwd=c(6, 6), 
#       col=plot_theme$col$line.col, bty="n")

#dygraph(prices_ts[, c(2, 3)], main="XLP and XLU prices") %>%
#  dyOptions(colors=c("orange", "blue")) %>%
#  dyRangeSelector()

## regression of training prices
#reg_model <- lm(XLP ~ XLU, data = prices_ts)
#summary(reg_model)
#coef(reg_model)[2]
#summary(reg_model)$r.squared

## scatterplot
#plot(as.data.frame(prices_ts[, c(2, 3)]))
#abline(reg_model, lwd=2, col="blue")


## regression of training prices - zero intercept
#reg_model <- lm(XLP ~ XLU - 1, data = prices_ts)
#summary(reg_model)
#coef(reg_model)[1]

## regression of training prices versus date index
#prices_ts <- cbind(prices_ts, 1:n_row)
##coef(lm(prices_ts[, "XLP"] ~ prices_ts[, 3]))[2] / coef(lm(prices_ts[, "XLU"] ~ prices_ts[, 3]))[2]

## regression residuals and z_val
#reg_model <- lm(XLP ~ XLU, data=prices_ts)
##summary(reg_model)
#hedge_ratio <- coef(reg_model)[2]
## resi_duals <- prices_ts[, "XLP"] - coef(reg_model)[2]*prices_ts[, "XLU"]
#resi_duals <- residuals(reg_model) + coef(reg_model)[2]
#aver_age <- mean(resi_duals)
#std_dev <- sd(resi_duals)
#z_val <- (resi_duals-aver_age)/std_dev

# plot z_val
###x11(width=8, height=6)
###da_ta <- cbind(as.data.frame(prices_ts), z_val)[, c(3, 4)]
###plot(da_ta)
###abline(lm(da_ta[, 2] ~ da_ta[, 1]), lwd=2, col="blue")


## model parameters
#z_long_enter <- -1.5
#z_long_profit <- 0
#z_long_stop <- -2.5

#z_short_enter <- 1.5
#z_short_profit <- 0
#z_short_stop <- 2.5

# in-sample training set
#stop_sell_mode <- rep(NA, n_row)
#stop_sell_mode[1] <- (z_val[1] < z_long_stop)
#stop_sell_mode[z_val < z_long_stop] <- TRUE
#stop_sell_mode[z_val > z_long_enter] <- FALSE
#stop_sell_mode <- zoo::na.locf(stop_sell_mode)

#stop_sell <- rep(NA_real_, n_row)
#indic_sell <- (z_val < z_long_stop)
#stop_sell[indic_sell] <- z_val[indic_sell]
## stop_sell <- zoo::na.locf(stop_sell)

#stop_buy_mode <- rep(NA, n_row)
#stop_buy_mode[1] <- (z_val[1] < z_short_stop)
#stop_buy_mode[z_val > z_short_stop] <- TRUE
#stop_buy_mode[z_val < z_short_enter] <- FALSE
#stop_buy_mode <- zoo::na.locf(stop_buy_mode)

#z_short <- numeric(n_row)
#indic_short <- ((z_val > z_short_enter) & !stop_buy_mode) | 
#  ((!stop_buy_mode) & rutils::lag_it(stop_buy_mode))
#z_short[indic_short] <- z_val[indic_short]

#z_buy <- numeric(n_row)
#indic_buy <- ((z_val < z_long_enter) & !stop_sell_mode) | 
#  ((!stop_sell_mode) & rutils::lag_it(stop_sell_mode))
#z_buy[indic_buy] <- z_val[indic_buy]
