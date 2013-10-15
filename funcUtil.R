### various utility functions


# Calculate the p-value of a F-statistic for a variable in a VAR model
lmp <- function(var.object, n.vari=1) {
  if (class(var.object)!="varest") stop("Not an object of class 'varest'")
  f.stat <- (summary(var.model)$varresult[[n.vari]])$fstatistic
  p.value <- unname(pf(f.stat[1],f.stat[2],f.stat[3],lower.tail=F))
#  attributes(p.value) <- NULL
  return(p.value)
}
