library(Rblpapi)
library(quantmod)

bbg_connect <- blpConnect() 	
start_date <- as.Date("2017-09-01")
#start_date <- as.Date("2007-12-31")
bbg_fields <- c("PX_OPEN", "PX_HIGH", "PX_LOW", "PX_LAST")
data_dir <- "C:/Develop/data/"
bbg_name <- "SPX Index"
file_name <- paste0(data_dir,bbg_name,".csv")

bbg_history <- NULL
bbg_history <- bdh(securities = bbg_name, 
                   fields = bbg_fields, 
                   start.date = start_date)

# coerce bbg_history from data frame to xts
#bbg_history <- xts::as.xts(bbg_history)
#bbg_history <- xts::xts(bbg_history, order.by = as.Date(bbg_history[, "date"]))
#bbg_history
# write bbg_history to CSV file
zoo::write.zoo(bbg_history, file = file_name, sep=",")

