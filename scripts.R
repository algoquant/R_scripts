# misc scripts for testing

rm(list=ls())
options(max.print=80)
options(digits=3)

library(zoo)
# good package loading script inside functions
stopifnot("package:xts" %in% search() || require("xts", quietly=TRUE))


###########
# functions under development

