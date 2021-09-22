### plot_interactive.R contains R script to demonstrate interactive plotting

# print information
print(paste0("print: This script was run at: ", format(Sys.time())))
cat("cat: This script was run at:", format(Sys.time()), "\n")

# plot sine function in x11 window
x11()
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
      xlab="", ylab="", lwd=2, col="orange",
      main="Sine function")

# wait until x11 window is closed
while (!is.null(dev.list())) Sys.sleep(1)

