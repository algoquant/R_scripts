########################
### visualize portfolio

plot_portf <- function(portfolio, 
                       rets_data=etf_rets) {
  portf_weights <- portfolio$weights
  portf_names <- names(portf_weights)
  # calculate xts of portfolio
  portf_max <- xts(
    rets_data[, portf_names] %*% portf_weights, 
    order.by=index(rets_data))
  colnames(portf_max) <- 
    deparse(substitute(portfolio))
  graph_params <- par(oma=c(1, 0, 1, 0), 
                      mgp=c(2, 1, 0), mar=c(2, 1, 2, 1), 
                      cex.lab=0.8, cex.axis=1.0, 
                      cex.main=0.8, cex.sub=0.5)
  layout(matrix(c(1,2), 2), 
         widths=c(1,1), heights=c(1,3))
  barplot(portf_weights, names.arg=portf_names, 
          las=3, ylab="", xlab="Symbol", main="")
  title(main=paste("Loadings", 
                   colnames(portf_max)), line=-1)
  chart.CumReturns(
    cbind(portf_max, rets_data[, c("IEF", "VTI")]), 
    lwd=2, ylab="", legend.loc="topleft", main="")
  title(main=paste0(colnames(portf_max), 
                    ", IEF, VTI"), line=-1)
  par(graph_params)  # restore original parameters
  invisible(portf_max)
}  # end plot_portf
