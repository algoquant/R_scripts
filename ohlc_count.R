###############
### Strategy using OHLC technical indicators

# Compile Rcpp functions
Rcpp::sourceCpp(file="C:/Develop/R/Rcpp/lm_arma.cpp")

library(rutils)
data_dir <- "C:/Develop/data/ib_data/"
sym_bol <- "ES"
load(paste0(data_dir, sym_bol, "_ohlc.RData"))

n_rows <- NROW(oh_lc)
oh_lc <- coredata(oh_lc)
op_en <- oh_lc[, 1]
hi_gh <- oh_lc[, 2]
lo_w <- oh_lc[, 3]
clo_se <- oh_lc[, 4]
re_turns <- rutils::diff_it(clo_se)

high_low <- (hi_gh > lo_w)
returns_pos_count <- drop(roll_count(re_turns > 0))
returns_neg_count <- drop(roll_count(re_turns < 0))
close_high <- (clo_se == hi_gh) & high_low
close_high_count <- drop(roll_count(close_high))
close_low <- (clo_se == lo_w) & high_low
close_low_count <- drop(roll_count(close_low))
open_high <- (op_en == hi_gh) & high_low
open_high_count <- drop(roll_count(open_high))
open_low <- (op_en == lo_w) & high_low
open_low_count <- drop(roll_count(open_low))

po_sit <- rep(NA_integer_, NROW(oh_lc))
po_sit[1] <- 0
po_sit[returns_pos_count > 1] <- (-1)
po_sit[returns_neg_count > 1] <- 1
# po_sit[close_high_count > 1] <- (-1)
# po_sit[close_low_count > 1] <- 1
po_sit <- zoo::na.locf(po_sit, na.rm=FALSE)
po_sit <- rutils::lag_it(po_sit, lagg=1)

# number of trades
sum(abs(rutils::diff_it(po_sit))) / NROW(po_sit)

# calculate strategy pnl_s
pnl_s <- cumsum(po_sit*re_turns)
# tail(pnl_s)
x11()
plot(pnl_s, t="l")

###

