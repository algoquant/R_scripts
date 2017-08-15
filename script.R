### script.R contains R script to demonstrate sourcing from script files

# print information about this process
print(paste0("print: This script was run at: ", format(Sys.time())))
cat("cat: This script was run at:", format(Sys.time()), "\n")

# display first 6 rows of cars data frame
head(cars)

# define a function
fun_c <- function(x) x+1

# read a line from console
readline("Press Return to continue")

# plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
      xlab="", ylab="", lwd=2, col="orange",
      main="Sine function")
