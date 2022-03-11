
# Perform regression on two columns of data and plot it
lm_plot <- function(datav) {
  reg_formula <- as.formula(paste(colnames(datav), collapse=" ~ "))
  reg_model <- lm(reg_formula, data=datav)
  x11()
  plot(reg_formula, data=datav, 
       main=paste("Regression of", 
                  paste(colnames(datav), collapse=" versus ")))
  abline(reg_model, lwd=2, col="red")
  summary(reg_model)
}  # end lm_plot

lm_plot(prices_ts[, c("XLP", "XLU")])

