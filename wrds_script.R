### Script for pivoting CSV file with OHLC prices from WRDS

library(data.table)
library(HighFreq)
# Read a data table from CSV file
dir_name <- "C:/Develop/R/capstone/Xuewan_Zhao/data"
fil_e <- file.path(dir_name, "OHLCV.csv")
oh_lc <- data.table::fread(fil_e)
class(oh_lc)
dim(oh_lc)

# Perform the map/reduce (split-apply-combine) procedure to convert the data frame into multiple xts series, in an environment.


# Save the xts series into individual csv files in a directory.




### Script for downloading WRDS data - produces Java error

library(rJava)
# options(java.parameters='-Xmx4g')
library(RJDBC)
library(quantmod)

# WRDS credentials and jdbc drivers
user_name <- "jp3900"
pass_word <- "{SAS002}3FFAE84348C8D99611B9FE382091DEE021CB597D03364794"
driver_s <- c("C:/Develop/data/WRDS/WRDS_Drivers/sas.core.jar", "C:/Develop/data/WRDS_Drivers/sas.intrnet.javatools.jar")

# function for establishing WRDS connection
wrds_connect <- function(user_name=user_name, pass_word=pass_word, driver_s=driver_s){
  dri_ver <- RJDBC::JDBC("com.sas.net.sharenet.ShareNetDriver", driver_s[2], identifier.quote="`")
  rJava::.jaddClassPath(driver_s)
  DBI::dbConnect(dri_ver, "jdbc:sharenet://wrds-cloud.wharton.upenn.edu:8551/", user_name, pass_word)
}  # wrds_connect

# establish WRDS connection
wrds_connection <- wrds_connect(user_name, pass_word, driver_s)
# send SAS query
sas_query <- DBI::dbSendQuery(wrds_connection, "select date,dji from DJONES.DJDAILY")
# download first 10 rows of data
da_ta <- DBI::fetch(sas_query, n=10)
# download all the rows of data
# da_ta <- DBI::fetch(sas_query, n=-1)
da_ta <- xts::xts(da_ta[, 2], as.POSIXct(da_ta[, 1]))
da_ta[da_ta==0] <- NA
da_ta <- na.locf(da_ta)
colnames(da_ta) <- "wrds_data"
x11()
chart_Series(x=da_ta, name="DJIA data from WRDS")
