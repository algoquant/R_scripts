### script for downloading WRDS data

library(rJava)
# options(java.parameters='-Xmx4g')
library(RJDBC)
library(quantmod)

# WRDS credentials and jdbc drivers
user_name <- "jp3900"
pass_word <- "{SAS002}3FFAE84348C8D99611B9FE382091DEE021CB597D03364794"
dri_vers <- c("C:/Develop/data/WRDS_Drivers/sas.core.jar", "C:/Develop/data/WRDS_Drivers/sas.intrnet.javatools.jar")

# function for establishing WRDS connection
wrdsconnect <- function(user_name=user_name, pass_word=pass_word, dri_vers=dri_vers){
  dri_ver <- RJDBC::JDBC("com.sas.net.sharenet.ShareNetDriver", dri_vers[2], identifier.quote="`")
  rJava::.jaddClassPath(dri_vers)
  DBI::dbConnect(dri_ver, "jdbc:sharenet://wrds-cloud.wharton.upenn.edu:8551/", user_name, pass_word)
}  # wrdsconnect

# establish WRDS connection
wrds_connection <- wrdsconnect(user="jp3900", pass="{SAS002}3FFAE84348C8D99611B9FE382091DEE021CB597D03364794")
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
