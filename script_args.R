### script_args.R contains R script that accepts arguments

# print information about this process
cat("cat: This script was run at:", format(Sys.time()), "\n")

# read arguments supplied on the command line
arg_s <- commandArgs(TRUE)

# print the arguments
cat(paste0("arguments supplied on command line: ", paste(arg_s, collapse=", "), "\n"))

# return sum of arguments
sum(as.numeric(arg_s))

