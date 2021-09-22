cat("###################################################\n")
cat("### sourcing .Rprofile file in C:/Develop/data ###\n")
cat("###################################################\n")

library(rJava)
# options(java.parameters = '-Xmx4g')
library(RJDBC)

user_name <- "jp3900"
pass_word <- "{SAS002}3FFAE84348C8D99611B9FE382091DEE021CB597D03364794"
dri_vers <- c("C:/Develop/data/WRDS/WRDS_Drivers/sas.core.jar", "C:/Develop/data/WRDS/WRDS_Drivers/sas.intrnet.javatools.jar")

wrdsconnect <- function(user_name=user_name, pass_word=pass_word){
  dri_ver <- RJDBC::JDBC("com.sas.net.sharenet.ShareNetDriver", dri_vers[2], identifier.quote="`")
  rJava::.jaddClassPath(dri_vers)
  DBI::dbConnect(dri_ver, "jdbc:sharenet://wrds-cloud.wharton.upenn.edu:8551/", user_name, pass_word)
}  # wrdsconnect

.First <- function(){
  wrds_connection <<- wrdsconnect(user_name=user_name, pass_word=pass_word)
}

.Last <- function(){
  dbDisconnect(wrds_connection)
}
