#!/usr/bin/env Rscript
# This is a simple R script that can be run from the command line as follows:
#   /Users/jerzy/Develop/R/run_Rscript.R bye
# It returns: "Hello bye"
# The shebang line at the top specifies the application that should be run, 
# and ensures that the interpreter uses the first installed version of Rscript on your PATH. 

# Read the line arguments
args = commandArgs(trailingOnly = TRUE)
# Print out a message to console
message(sprintf("Hello %s", args[1L]))

