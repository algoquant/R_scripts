### plot_to_file.R contains R script to demonstrate plotting to file

# print information
print(paste0("print: This script was run at: ", format(Sys.time())))
cat("cat: This script was run at:", format(Sys.time()), "\n")

# redirect graphics output to png file
plot_dir <- "C:/Develop/data"
png(file.path(plot_dir, "r_plot.png"))

# plot sine function
curve(expr=sin, type="l", xlim=c(-2*pi, 2*pi),
      xlab="", ylab="", lwd=2, col="orange",
      main="Sine function")

# plot a shifted Normal probability distribution
# curve(expr=dnorm(x, mean=1), type="l", xlim=c(-2, 4),
#       xlab="", ylab="", lwd=2, col="orange",
#       main="shifted Normal probability distribution")

# turn png output off
dev.off()
