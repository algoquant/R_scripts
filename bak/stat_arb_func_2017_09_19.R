####################################
### file stat_arb_func.R
### Functions for performing regressions and simulating pairs trading

### Change log
## 2017-09-12: major rewrite and renaming of functions:
# run_backtest() -> run_backtest_endpoints()
# run_backtest2() -> run_backtest_subset()

### Change log
## 2017-09-14: created roll_backtest()



### function load_data() reads time series data from csv files
load_data <- function(file_names=c("XLU.csv", "XLP.csv"),
                        data_dir="C:/Develop/data_bbg_records",
                        header=TRUE,
                        date_fun=match.fun("as.Date"),
                        for_mat="%Y-%m-%d") {
  da_ta <- lapply(file_names, function(file_name) {
    xts::as.xts(zoo::read.zoo(file=file.path(data_dir, file_name),
                              header=header, sep=",",
                              FUN=date_fun,
                              format=for_mat))
  })  # end lapply
  da_ta <- rutils::do_call(cbind, da_ta)
  # colnames(da_ta) <- sapply(file_names,
  #                           function(file_name) strsplit(file_name, split=" ")[[1]][1])
  da_ta
}  # end load_data


### function hedge_ratio() calculates the hedge ratio for a pair of instruments
# JK: hedge ratio means how many units of x you need to short per one unit of y
hedge_ratio <- function (prices_ts_x, prices_ts_y,
                            model_type = 1,
                            flag_print = FALSE,
                            flag_plot = FALSE) {

  # regress y vs x forcing the line to cross the origin
  da_ta <- cbind(prices_ts_x, prices_ts_y)
  if (model_type == 0) {
    reg_formula <- paste0(names(prices_ts_y), " ~ ", names(prices_ts_x), " - 1")
    reg_model <- lm(reg_formula, da_ta)
    hedge_ratio <- unname(coef(reg_model)[1])
    if (flag_print) {
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n",
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n",
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n",
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5, height=4)
      plot(formula=as.formula(reg_formula), data=da_ta,
           main=paste("Regression through the origin",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(reg_model, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(coef(reg_model)[2], 4), "*x",
                         ifelse(coef(reg_model)[1]<0, " - ", " + "),
                         round(abs(coef(reg_model)[1]), 4), "\nR^2 = ",
                         round(summary(reg_model)$r.squared, 4)),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary = summary(reg_model),
                reg_formula = reg_formula,
                hedge_ratio = hedge_ratio,
                r_squared = summary(reg_model)$r.squared,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd = sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))

  } else if (model_type == 1) {
    # regress y vs x with free intercept
    reg_formula <- paste0(names(prices_ts_y), " ~ ", names(prices_ts_x))
    reg_model <- lm(formula = as.formula(reg_formula), data = da_ta)
    hedge_ratio <- unname(coef(reg_model)[2])
    if (flag_print) {
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n",
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n",
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n",
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(reg_formula), data = da_ta,
           main=paste("OLC regression of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(reg_model, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(coef(reg_model)[2], 4), "*x",
                         ifelse(coef(reg_model)[1]<0, " - ", " + "),
                         round(abs(coef(reg_model)[1]), 4), "\nR^2 = ",
                         round(summary(reg_model)$r.squared, 4)),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary = summary(reg_model),
                reg_formula = reg_formula,
                hedge_ratio = hedge_ratio,
                r_squared = summary(reg_model)$r.squared,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd = sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))

  } else if (model_type == 2) {
    # Total least square fit
    # https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca
    # https://robotwealth.com/exploring-mean-reversion-and-cointegration-part-2/

    reg_model <- prcomp(da_ta)$rotation
    hedge_ratio <- reg_model[2, 1] / reg_model[1, 1]
    inter_cept <- mean(prices_ts[, name_y]) - hedge_ratio*mean(prices_ts[, name_x])
    if (flag_print) {
      #print(summary(reg_model))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(paste0(names(prices_ts_y),
                                       " ~ ",
                                       names(prices_ts_x))),
           data = da_ta,
           main=paste("TLS regression of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(a = inter_cept, b = hedge_ratio, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(hedge_ratio, 4), "*x",
                         ifelse(inter_cept<0, " - ", " + "),
                         round(abs(inter_cept), 4)
                         # "\nR^2 = ",
                         # round(summary(reg_model)$r.squared, 4)
                         ),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary = summary(reg_model),
                hedge_ratio = hedge_ratio,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd = sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))

  } else if (model_type == 3) {

    # no drift hedge ratio using the index (best for most cases but sometimes blows up)

    in_dex <- 1:length(prices_ts_x)
    da_ta <- cbind(in_dex, da_ta)
    colnames(da_ta)[1]<- "index"

    # regress x vs index
    reg_formula_x <- paste0(names(prices_ts_x), " ~ ", "index")
    reg_model_x <- lm(formula = as.formula(reg_formula_x), data = da_ta)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])

    # regress y vs index
    reg_formula_y <- paste0(names(prices_ts_y), " ~ ", "index")
    reg_model_y <- lm(formula = as.formula(reg_formula_y), data = da_ta)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])

    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    inter_cept <- mean(prices_ts[, name_y]) - hedge_ratio*mean(prices_ts[, name_x])
    if (flag_print) {
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("reg_formula_x: \t", reg_formula_x, "\n",
                 "reg_formula_y: \t", reg_formula_y, "\n",
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(paste0(names(prices_ts_y),
                                       " ~ ",
                                       names(prices_ts_x))),
           data = da_ta,
           main=paste("No drift (index) reg. of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(a = inter_cept, b = hedge_ratio, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(hedge_ratio, 4), "*x",
                         ifelse(inter_cept<0, " - ", " + "),
                         round(abs(inter_cept), 4)
                         # "\nR^2 = ",
                         # round(summary(reg_model)$r.squared, 4)
           ),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary_x = summary(reg_model_x),
                reg_summary_y = summary(reg_model_y),
                hedge_ratio = hedge_ratio,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd =sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))

  } else if (model_type == 4) {
    index_dates <- xts::.index(prices_ts)
    reg_model_x <- lm(prices_ts[, name_x] ~ index_dates)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    reg_model_y <- lm(prices_ts[, name_y] ~ index_dates)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    inter_cept <- mean(prices_ts[, name_y]) - hedge_ratio*mean(prices_ts[, name_x])
    if (flag_print) {
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(paste0(names(prices_ts_y),
                                       " ~ ",
                                       names(prices_ts_x))),
           data = da_ta,
           main=paste("No drift (dates) reg. of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(a = inter_cept, b = hedge_ratio, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(hedge_ratio, 4), "*x",
                         ifelse(inter_cept<0, " - ", " + "),
                         round(abs(inter_cept), 4)
                         # "\nR^2 = ",
                         # round(summary(reg_model)$r.squared, 4)
           ),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary_x = summary(reg_model_x),
                reg_summary_y = summary(reg_model_y),
                hedge_ratio = hedge_ratio,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd =sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))

  } else {
    return(NULL)
  }  # end if
}  # end hedge_ratio


####################################
### function train_model() calculates the hedge ratio for a pair of instruments
# JP: for now train_model() replicates the function hedge_ratio() but in a more efficient way

train_model <- function (time_series,
                         model_params, ...) {
  
  # convert the list of model_params into variables (model parameters)
  list2env(model_params, envir=environment())
  
  # extract the time series
  prices_ts_y <- time_series[, symbol_s[1]]
  prices_ts_x <- time_series[, symbol_s[2]]
  da_ta <- cbind(prices_ts_x, prices_ts_y)
  
  # regress y vs x forcing the line to cross the origin
  if (model_type == 0) {
    reg_formula <- paste0(names(prices_ts_y), " ~ ", names(prices_ts_x), " - 1")
    reg_model <- lm(reg_formula, da_ta)
    hedge_ratio <- unname(coef(reg_model)[1])
    if (flag_print) {
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n",
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n",
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n",
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5, height=4)
      plot(formula=as.formula(reg_formula), data=da_ta,
           main=paste("Regression through the origin",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(reg_model, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(coef(reg_model)[2], 4), "*x",
                         ifelse(coef(reg_model)[1]<0, " - ", " + "),
                         round(abs(coef(reg_model)[1]), 4), "\nR^2 = ",
                         round(summary(reg_model)$r.squared, 4)),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary = summary(reg_model),
                reg_formula = reg_formula,
                hedge_ratio = hedge_ratio,
                r_squared = summary(reg_model)$r.squared,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd = sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))
    
  } else if (model_type == 1) {
    # regress y vs x with free intercept
    reg_formula <- paste0(names(prices_ts_y), " ~ ", names(prices_ts_x))
    reg_model <- lm(formula = as.formula(reg_formula), data = da_ta)
    hedge_ratio <- unname(coef(reg_model)[2])
    if (flag_print) {
      #print(summary(reg_model))
      cat(paste0("reg_formula: \t", reg_formula, "\n",
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n",
                 "\tR^2: \t", round(summary(reg_model)$r.squared, 6), "\n",
                 "correlation: \t", 100*round(sqrt(summary(reg_model)$r.squared), 4), "%\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(reg_formula), data = da_ta,
           main=paste("OLC regression of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(reg_model, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(coef(reg_model)[2], 4), "*x",
                         ifelse(coef(reg_model)[1]<0, " - ", " + "),
                         round(abs(coef(reg_model)[1]), 4), "\nR^2 = ",
                         round(summary(reg_model)$r.squared, 4)),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary = summary(reg_model),
                reg_formula = reg_formula,
                hedge_ratio = hedge_ratio,
                r_squared = summary(reg_model)$r.squared,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd = sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))
    
  } else if (model_type == 2) {
    # Total least square fit
    # https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca
    # https://robotwealth.com/exploring-mean-reversion-and-cointegration-part-2/
    
    reg_model <- prcomp(da_ta)$rotation
    hedge_ratio <- reg_model[2, 1] / reg_model[1, 1]
    inter_cept <- mean(prices_ts[, name_y]) - hedge_ratio*mean(prices_ts[, name_x])
    if (flag_print) {
      #print(summary(reg_model))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(paste0(names(prices_ts_y),
                                       " ~ ",
                                       names(prices_ts_x))),
           data = da_ta,
           main=paste("TLS regression of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(a = inter_cept, b = hedge_ratio, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(hedge_ratio, 4), "*x",
                         ifelse(inter_cept<0, " - ", " + "),
                         round(abs(inter_cept), 4)
                         # "\nR^2 = ",
                         # round(summary(reg_model)$r.squared, 4)
           ),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary = summary(reg_model),
                hedge_ratio = hedge_ratio,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd = sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))
    
  } else if (model_type == 3) {
    
    # no drift hedge ratio using the index (best for most cases but sometimes blows up)
    
    in_dex <- 1:length(prices_ts_x)
    da_ta <- cbind(in_dex, da_ta)
    colnames(da_ta)[1]<- "index"
    
    # regress x vs index
    reg_formula_x <- paste0(names(prices_ts_x), " ~ ", "index")
    reg_model_x <- lm(formula = as.formula(reg_formula_x), data = da_ta)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    
    # regress y vs index
    reg_formula_y <- paste0(names(prices_ts_y), " ~ ", "index")
    reg_model_y <- lm(formula = as.formula(reg_formula_y), data = da_ta)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    inter_cept <- mean(prices_ts[, name_y]) - hedge_ratio*mean(prices_ts[, name_x])
    if (flag_print) {
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("reg_formula_x: \t", reg_formula_x, "\n",
                 "reg_formula_y: \t", reg_formula_y, "\n",
                 "hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(paste0(names(prices_ts_y),
                                       " ~ ",
                                       names(prices_ts_x))),
           data = da_ta,
           main=paste("No drift (index) reg. of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(a = inter_cept, b = hedge_ratio, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(hedge_ratio, 4), "*x",
                         ifelse(inter_cept<0, " - ", " + "),
                         round(abs(inter_cept), 4)
                         # "\nR^2 = ",
                         # round(summary(reg_model)$r.squared, 4)
           ),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary_x = summary(reg_model_x),
                reg_summary_y = summary(reg_model_y),
                hedge_ratio = hedge_ratio,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd =sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))
    
  } else if (model_type == 4) {
    index_dates <- xts::.index(prices_ts)
    reg_model_x <- lm(prices_ts[, name_x] ~ index_dates)
    hedge_ratio_x <- unname(coef(reg_model_x)[2])
    reg_model_y <- lm(prices_ts[, name_y] ~ index_dates)
    hedge_ratio_y <- unname(coef(reg_model_y)[2])
    hedge_ratio <- hedge_ratio_y / hedge_ratio_x
    inter_cept <- mean(prices_ts[, name_y]) - hedge_ratio*mean(prices_ts[, name_x])
    if (flag_print) {
      #print(summary(reg_model_x))
      #print(summary(reg_model_y))
      cat(paste0("hedge_ratio: \t", round(hedge_ratio, 6), "\n"))
    }  # end if
    if (flag_plot) {
      x11(width=5,height=4)
      plot(formula = as.formula(paste0(names(prices_ts_y),
                                       " ~ ",
                                       names(prices_ts_x))),
           data = da_ta,
           main=paste("No drift (dates) reg. of",
                      colnames(prices_ts_y), "vs", colnames(prices_ts_x)))
      abline(a = inter_cept, b = hedge_ratio, lwd=2, col="red")
      text(x=min(prices_ts_x), y=max(prices_ts_y)-1,
           labels=paste0("y = ",
                         round(hedge_ratio, 4), "*x",
                         ifelse(inter_cept<0, " - ", " + "),
                         round(abs(inter_cept), 4)
                         # "\nR^2 = ",
                         # round(summary(reg_model)$r.squared, 4)
           ),
           pos=4, cex=0.9)
    }  # end if
    return(list(reg_summary_x = summary(reg_model_x),
                reg_summary_y = summary(reg_model_y),
                hedge_ratio = hedge_ratio,
                port_price_mean = mean(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1]),
                port_price_sd =sd(prices_ts_y[-1] - hedge_ratio * prices_ts_x[-1])))
    
  } else {
    return(NULL)
  }  # end if
}  # end train_model




### function trade_pair() simulates pairs trading
# hedge ratio means how many units of x you need to short per one unit of y
trade_pair <- function (prices_ts_x,
                        prices_ts_y,
                        res_hr,
                        trading_levels = list(z_long_enter = -1.5,
                                              z_long_profit = 0,
                                              z_long_stop = -2.5,
                                              z_short_enter = 1.5,
                                              z_short_profit = 0,
                                              z_short_stop = 2.5),
                        flag_sticky_stops = TRUE,
                        flag_print = FALSE,
                        flag_plot = FALSE) {

  n_row <- NROW(prices_ts_x)
  index_dates <- zoo::index(prices_ts_x)

  res_val <- list()
  res_ts <- prices_ts_x
  res_ts <- cbind(res_ts, prices_ts_y)
  res_ts <- cbind(1:n_row, res_ts)
  colnames(res_ts)[1] <- "index"

  # construct hedged portfolio
  port_price <- prices_ts_y - res_hr$hedge_ratio * prices_ts_x
  colnames(port_price) <- "port_price"
  res_ts <- cbind(res_ts, port_price)

  port_price_mean <- mean(port_price)
  port_price_sd <- sd(port_price)
  res_val <- c(res_val, "port_price_mean" = port_price_mean,
                   "port_price_sd" = port_price_sd)

  # calculate hedge portfolio z-score
#  port_z <- (port_price - port_price_mean) / port_price_sd
#  this needs to be calculated using passed on price average and sd to do out of sample calculations correctly
  port_z <- (port_price - res_hr$port_price_mean) / res_hr$port_price_sd
  colnames(port_z) <- "port_z"
  res_ts <- cbind(res_ts, port_z)
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

  ### calculate portfolio time series statistics
  port_pos <- xts::xts(pos_all, order.by = index_dates)
  # port_pos <- port_z * 0 + pos_all
  colnames(port_pos) <- "port_pos"

  # p&l
  port_pnl <- rutils::lag_xts(port_pos) * (rutils::diff_xts(prices_ts_y) -
       res_hr$hedge_ratio * rutils::diff_xts(prices_ts_x))
  colnames(port_pnl) <- "port_pnl"

  # absolute market value (sum of market values of longs and shorts)
  port_amv <- port_pnl
  port_amv[1] <- prices_ts_y[1] + res_hr$hedge_ratio * prices_ts_x[1]
  port_amv <- cumsum(port_amv)
  colnames(port_amv) <- "port_amv"

  # returns
  port_ret <- port_pnl / rutils::lag_xts(port_amv)
  colnames(port_ret) <- "port_ret"

  # cumulative returns
  port_cumret <- cumprod(1 + port_ret) - 1
  colnames(port_cumret) <- "port_cumret"

  # high watermark
  port_cumret[0] <- 0
  port_hwm <- cummax(port_cumret)
  colnames(port_hwm) <- "port_hwm"

  # drawdown
  port_draw <- (1 + port_cumret) / (1 + port_hwm) - 1
  colnames(port_draw) <- "port_draw"

  # drawdown duration
  port_drawdur <- -sign(port_draw)
  cum_sum <- cumsum(port_drawdur)
  ze_ro <- rutils::diff_xts(port_drawdur)
  # sum(ze_ro==(-1))
  whi_ch <- which(ze_ro==(-1))
  if(length(whi_ch) > 0) {
    which_cumsum <- cum_sum[whi_ch]
    # head(cbind(port_drawdur, cum_sum, ze_ro, which_cumsum),200)
    lag_which_cumsum <- rutils::lag_xts(which_cumsum)
    lag_which_cumsum[1] <- 0
    port_drawdur[whi_ch] <- (lag_which_cumsum - which_cumsum)
    port_drawdur <- cumsum(port_drawdur)
  } else {
    port_drawdur <- cum_sum
  }
  colnames(port_drawdur) <- "port_drawdur"
  # head(cbind(cum_sum,ze_ro,port_drawdur),50)

  res_ts <- cbind(res_ts,
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

  res_val <- c(res_val,
                   port_return = port_return,
                   port_volatility = port_volatility,
                   port_sharpe_ratio = port_sharpe_ratio,
                   port_sharpe_ratio_dollar = port_sharpe_ratio_dollar,
                   port_maxdraw = port_maxdraw,
                   port_maxdraw_dur = port_maxdraw_dur,
                   port_num_trades = port_num_trades)

  if (flag_plot) {
    # plot residuals of basket
    reg_model_plot <- lm(port_z ~ res_ts[, "index"])
    x11(width=5,height=4)
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    plot(quantmod::chart_Series(port_z, theme=plot_theme, name=NULL))
    title(main="Residuals of basket, with buys and sells",
          adj=0.05, cex.main=0.8)
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
    text(x=1.0*max(NROW(port_z)), y=0.8*max(port_z),
         labels=paste0("y = ",
                       round(coef(reg_model_plot)[2], 4), "*x",
                       ifelse(coef(reg_model_plot)[1]<0, " - ", " + "),
                       round(abs(coef(reg_model_plot)[1]), 4)
                       # "\nR^2 = ",
                       # round(summary(reg_model_plot)$r.squared, 4)
                       ),
         pos=2, cex=0.9)
    # add background shading of areas
    # plot(add_TA(port_pos > 0, on=-1,
    #             col="lightgreen", border="lightgreen"))
    # plot(add_TA(port_pos < 0, on=-1,
    #             col="lightpink", border="lightpink"))
    # plot(add_TA(port_pos == 0, on=-1,
    #             col="lightgrey", border="lightgrey"))

    # plot cumulative returns
    x11(width=5, height=4)
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    plot(quantmod::chart_Series(port_cumret, theme=plot_theme, name=NULL))
    title(main="Cumulative returns", adj=0.1, cex.main=0.8)
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

  res <- list(res_val = res_val,
                  res_ts = res_ts,
                  res_hr = res_hr)

  return(res)
}  # end trade_pair



### function trade_model() simulates pairs trading
# JP: for now trade_model() replicates the function trade_pair() but in a more efficient way
trade_model <- function (time_series,
                         calibrated_model,
                         trading_params, ...) {
  
  # convert the list of trading_params into variables (trading parameters)
  list2env(trading_params, envir=environment())
  
  n_row <- NROW(time_series)
  index_dates <- zoo::index(time_series)
  
  # extract the time series
  prices_ts_y <- time_series[, symbol_s[1]]
  prices_ts_x <- time_series[, symbol_s[2]]
  res_ts <- cbind(prices_ts_x, prices_ts_y)
  res_ts <- cbind(1:n_row, res_ts)
  colnames(res_ts)[1] <- "index"
  
  # construct hedged portfolio
  port_price <- prices_ts_y - calibrated_model$hedge_ratio * prices_ts_x
  colnames(port_price) <- "port_price"
  res_ts <- cbind(res_ts, port_price)
  
  port_price_mean <- mean(port_price)
  port_price_sd <- sd(port_price)
  trade_output <- list()
  trade_output <- c(trade_output, "port_price_mean" = port_price_mean,
                    "port_price_sd" = port_price_sd)
  
  # calculate hedge portfolio z-score
  #  port_z <- (port_price - port_price_mean) / port_price_sd
  #  this needs to be calculated using passed on price average and sd to do out of sample calculations correctly
  port_z <- (port_price - calibrated_model$port_price_mean) / calibrated_model$port_price_sd
  colnames(port_z) <- "port_z"
  res_ts <- cbind(res_ts, port_z)
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
  }  # end if (flag_sticky_stops)
  
  ### calculate portfolio time series statistics
  port_pos <- xts::xts(pos_all, order.by = index_dates)
  # port_pos <- port_z * 0 + pos_all
  colnames(port_pos) <- "port_pos"
  
  # p&l
  port_pnl <- rutils::lag_xts(port_pos) * (rutils::diff_xts(prices_ts_y) -
                                             calibrated_model$hedge_ratio * rutils::diff_xts(prices_ts_x))
  colnames(port_pnl) <- "port_pnl"
  
  # absolute market value (sum of market values of longs and shorts)
  port_amv <- port_pnl
  port_amv[1] <- prices_ts_y[1] + calibrated_model$hedge_ratio * prices_ts_x[1]
  port_amv <- cumsum(port_amv)
  colnames(port_amv) <- "port_amv"
  
  # returns
  port_ret <- port_pnl / rutils::lag_xts(port_amv)
  colnames(port_ret) <- "port_ret"
  
  # cumulative returns
  port_cumret <- cumprod(1 + port_ret) - 1
  colnames(port_cumret) <- "port_cumret"
  
  # high watermark
  port_cumret[0] <- 0
  port_hwm <- cummax(port_cumret)
  colnames(port_hwm) <- "port_hwm"
  
  # drawdown
  port_draw <- (1 + port_cumret) / (1 + port_hwm) - 1
  colnames(port_draw) <- "port_draw"
  
  # drawdown duration
  port_drawdur <- -sign(port_draw)
  cum_sum <- cumsum(port_drawdur)
  ze_ro <- rutils::diff_xts(port_drawdur)
  # sum(ze_ro==(-1))
  whi_ch <- which(ze_ro==(-1))
  if(length(whi_ch) > 0) {
    which_cumsum <- cum_sum[whi_ch]
    # head(cbind(port_drawdur, cum_sum, ze_ro, which_cumsum),200)
    lag_which_cumsum <- rutils::lag_xts(which_cumsum)
    lag_which_cumsum[1] <- 0
    port_drawdur[whi_ch] <- (lag_which_cumsum - which_cumsum)
    port_drawdur <- cumsum(port_drawdur)
  } else {
    port_drawdur <- cum_sum
  }
  colnames(port_drawdur) <- "port_drawdur"
  # head(cbind(cum_sum,ze_ro,port_drawdur),50)
  
  res_ts <- cbind(res_ts,
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
  
  trade_output <- c(trade_output,
               port_return = port_return,
               port_volatility = port_volatility,
               port_sharpe_ratio = port_sharpe_ratio,
               port_sharpe_ratio_dollar = port_sharpe_ratio_dollar,
               port_maxdraw = port_maxdraw,
               port_maxdraw_dur = port_maxdraw_dur,
               port_num_trades = port_num_trades)
  
  if (flag_plot) {
    # plot residuals of basket
    reg_model_plot <- lm(port_z ~ res_ts[, "index"])
    x11(width=5,height=4)
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    plot(quantmod::chart_Series(port_z, theme=plot_theme, name=NULL))
    title(main="Residuals of basket, with buys and sells",
          adj=0.05, cex.main=0.8)
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
    text(x=1.0*max(NROW(port_z)), y=0.8*max(port_z),
         labels=paste0("y = ",
                       round(coef(reg_model_plot)[2], 4), "*x",
                       ifelse(coef(reg_model_plot)[1]<0, " - ", " + "),
                       round(abs(coef(reg_model_plot)[1]), 4)
                       # "\nR^2 = ",
                       # round(summary(reg_model_plot)$r.squared, 4)
         ),
         pos=2, cex=0.9)
    # add background shading of areas
    # plot(add_TA(port_pos > 0, on=-1,
    #             col="lightgreen", border="lightgreen"))
    # plot(add_TA(port_pos < 0, on=-1,
    #             col="lightpink", border="lightpink"))
    # plot(add_TA(port_pos == 0, on=-1,
    #             col="lightgrey", border="lightgrey"))
    
    # plot cumulative returns
    x11(width=5, height=4)
    plot_theme <- quantmod::chart_theme()
    plot_theme$col$line.col <- "black"
    plot(quantmod::chart_Series(port_cumret, theme=plot_theme, name=NULL))
    title(main="Cumulative returns", adj=0.1, cex.main=0.8)
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
  
  # list(res_val = trade_output,
  #      res_ts = res_ts,
  #      res_hr = calibrated_model)
  
  c(
    # port_return = port_return,
    port_volatility = port_volatility,
    port_sharpe_ratio = port_sharpe_ratio,
    port_sharpe_ratio_dollar = port_sharpe_ratio_dollar,
    port_maxdraw = port_maxdraw,
    port_maxdraw_dur = port_maxdraw_dur,
    port_num_trades = port_num_trades)
  
}  # end trade_model



### functional run_backtest_endpoints() performs a backtest simulation over a vector of end points.
run_backtest_endpoints <- function (end_points, price_s) {
  # cat(paste0(start_dates[i_ter], "\t",
  #            paste(dim(price_s), collapse = "\t"), "\n"))
  t(sapply(1:(NROW(end_points)-1), function(i_ter) {

    prices_train <- price_s[end_points[[i_ter]], ]
    prices_train <- cbind(1:NROW(prices_train), prices_train)
    colnames(prices_train)[1] <- "index"

    prices_test <- price_s[end_points[[end_point+1]], ]
    prices_test <- cbind(1:NROW(prices_test), prices_test)
    colnames(prices_test)[1] <- "index"

    res_hr <- hedge_ratio(prices_ts_x = prices_train[, name_x],
                          prices_ts_y = prices_train[, name_y],
                          model_type = model_type,
                          flag_print = flag_print,
                          flag_plot = flag_plot)

    res_tp <- trade_pair(prices_ts_x = prices_test[, name_x],
                         prices_ts_y = prices_test[, name_y],
                         res_hr = res_hr,
                         trading_levels = trading_levels,
                         flag_sticky_stops = flag_sticky_stops,
                         flag_print = flag_print,
                         flag_plot = flag_plot)

    cat(paste0(start_dates[i_ter], "\t",
               # sum(end_points[[i_ter]]), "\t",
               # paste(dim(prices_ts_train), collapse = "\t"), "\t",
               # round(mean(prices_ts_train[, name_y]), 4), "\t",
               # paste(dim(prices_ts_test), collapse = "\t"), "\t",
               # round(mean(prices_ts_test[, name_y]), 4), "\t",
               round(res_hr$reg_summary$coefficients[1], 4), "\t",
               round(res_hr$reg_summary$coefficients[2], 4), "\t",
               round(res_tp$res_val$port_sharpe_ratio, digits = 4), "\t",
               round(res_tp$res_val$port_num_trades, digits = 4), "\n"))

    c(intercept=round(res_hr$reg_summary$coefficients[1], 4),
      slope=round(res_hr$reg_summary$coefficients[2], 4),
      sharpe_ratio=round(res_tp$res_val$port_sharpe_ratio, digits = 4),
      num_trades=round(res_tp$res_val$port_num_trades, digits = 4))

  }))  # end sapply

}  # end run_backtest_endpoints




### functional run_backtest_subset() performs a backtest simulation over a vector of end points.
run_backtest_subset <- function (end_points, look_back, look_forward, price_s) {

  sapply(seq_along(end_points), function(i_ter) {
    # cat("end_point =", end_point, "\n")
    # break()
    # match(index(prices_test), index(prices_ts_all))
    prices_train <- rutils::sub_set(price_s,
                                    start_date=end_points[i_ter],
                                    end_date=-look_back)
    # no need to add index to prices since they're not passed anywhere
    # prices_train <- cbind(1:NROW(prices_train), prices_train)
    # colnames(prices_train)[1] <- "index"

    prices_test <- rutils::sub_set(price_s,
                                   start_date=end_points[i_ter]+1,
                                   end_date=look_forward)
    # prices_test <- cbind(1:NROW(prices_test), prices_test)
    # colnames(prices_test)[1] <- "index"

    res_hr <- hedge_ratio(prices_ts_x = prices_train[, name_x],
                          prices_ts_y = prices_train[, name_y],
                          model_type = model_type,
                          flag_print = flag_print,
                          flag_plot = flag_plot)

    res_tp <- trade_pair(prices_ts_x = prices_test[, name_x],
                         prices_ts_y = prices_test[, name_y],
                         res_hr = res_hr,
                         trading_levels = trading_levels,
                         flag_sticky_stops = flag_sticky_stops,
                         flag_print = flag_print,
                         flag_plot = flag_plot)

    cat(paste0(start_dates[i_ter], "\t",
               # sum(end_points[[i_ter]]), "\t",
               # paste(dim(prices_ts_train), collapse = "\t"), "\t",
               # round(mean(prices_ts_train[, name_y]), 4), "\t",
               # paste(dim(prices_ts_test), collapse = "\t"), "\t",
               # round(mean(prices_ts_test[, name_y]), 4), "\t",
               round(res_hr$reg_summary$coefficients[1], 4), "\t",
               round(res_hr$reg_summary$coefficients[2], 4), "\t",
               round(res_tp$res_val$port_sharpe_ratio, digits = 4), "\t",
               round(res_tp$res_val$port_num_trades, digits = 4), "\n"))

    c(intercept=round(res_hr$reg_summary$coefficients[1], 4),
      slope=round(res_hr$reg_summary$coefficients[2], 4),
      sharpe_ratio=round(res_tp$res_val$port_sharpe_ratio, digits = 4),
      num_trades=round(res_tp$res_val$port_num_trades, digits = 4))

  })  # end sapply

}  # end run_backtest_subset




### functional roll_backtest() performs a backtest simulation over a vector of end points.
roll_backtest <- function (end_points, look_back, look_forward, 
                          train_model, trade_model, 
                          price_s, ...) {

  # echo to console startup fields
  cat(format(Sys.Date()), "\t", 
      "hedge_ratio", "\t",
      "sharpe_ratio", "\t",
      "num_trades", "\n")

  # perform loop over end_points
  sapply(seq_along(end_points), function(i_ter) {
    # cat("end_point =", end_point, "\n")
    
    # subset training data
    train_data <- rutils::sub_set(price_s,
                                  start_date=end_points[i_ter],
                                  end_date=-look_back)

    test_data <- rutils::sub_set(price_s,
                                 start_date=end_points[i_ter]+1,
                                 end_date=look_forward)

    calibrated_model <- train_model(time_series = train_data, ...)
    
    trade_output <- trade_model(time_series = test_data, 
                                calibrated_model = calibrated_model, ...)
    
    out_data <- c(hedge_ratio=round(calibrated_model$hedge_ratio, digits=4), 
                  sharpe_ratio=round(trade_output$res_val$port_sharpe_ratio, digits=4), 
                  num_trades=round(trade_output$res_val$port_num_trades, digits=4))
    
    cat(paste0(end_points[i_ter], "\t",
               # sum(end_points[[i_ter]]), "\t",
               # paste(dim(prices_ts_train), collapse = "\t"), "\t",
               # round(mean(prices_ts_train[, name_y]), 4), "\t",
               # paste(dim(prices_ts_test), collapse = "\t"), "\t",
               # round(mean(prices_ts_test[, name_y]), 4), "\t",
               paste(out_data, collapse="\t"), "\n"))
    
    out_data
    
  })  # end sapply
  
}  # end roll_backtest



