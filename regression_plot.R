
# Perform regression on two columns of data and plot it
lm_plot <- function(da_ta) {
  reg_formula <- as.formula(paste(colnames(da_ta), collapse=" ~ "))
  reg_model <- lm(reg_formula, data=da_ta)
  x11()
  plot(reg_formula, data=da_ta, 
       main=paste("Regression of", 
                  paste(colnames(da_ta), collapse=" versus ")))
  abline(reg_model, lwd=2, col="red")
  summary(reg_model)
}  # end lm_plot

lm_plot(prices_ts[, c("XLP", "XLU")])

