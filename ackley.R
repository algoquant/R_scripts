### The Ackley function for the testing of minimization and optimization functions.

ack_ley <- function(xx, a=20, b=0.2, c=2*pi) {
  ##########################################################################
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2, ..., xd)
  # a = constant (optional), with default value 20
  # b = constant (optional), with default value 0.2
  # c = constant (optional), with default value 2*pi
  #
  ##########################################################################
	
  d <- length(xx)
  
  sum1 <- sum(xx^2)
  sum2 <- sum(cos(c*xx))

  term1 <- -a * exp(-b*sqrt(sum1/d))
  term2 <- -exp(sum2/d)

  y <- term1 + term2 + a + exp(1)
  return(y)
}  # end ack_ley
