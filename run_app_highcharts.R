#!/usr/bin/env Rscript
# The shebang line at the top specifies the application that should be run, 
# and ensures that the interpreter uses the first installed version of Rscript on the PATH. 

# This is a simple R script that can be run from the command line as follows:
#   /Users/jerzy/Develop/R/run_app_highcharts.R 
# This is equivalent to running:
#   Rscript -e "shiny::runApp('~/Develop/Presentations/app_highcharts.R', port=7775)"

shiny::runApp('~/Develop/Presentations/app_highcharts.R', port=7775)

