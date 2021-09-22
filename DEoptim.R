### DEoptim example scripts

library(DEoptim)

## Rastrigin egg carton function: vector argument version for optimization
rastri_gin <- function(vec_tor, pa_ram=25){
  sum(vec_tor^2 - pa_ram*cos(vec_tor))
}  # end rastri_gin
# rastri_gin <- function(vec_tor){
#   x1 <- vec_tor[1]
#   x2 <- vec_tor[2]
#   x1^2 + x2^2 + 25*(sin(x1)^2 + sin(x2)^2)
# }  # end rastri_gin
rastri_gin(vec_tor=c(1, 2))


## optimize rastri_gin using DEoptim
op_tim <-  DEoptim(rastri_gin, 
                   upper=c(6, 6), lower=c(-6, -6), 
                   DEoptim.control(trace=FALSE, itermax=50))
# optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)


## optimize rastri_gin using parallel DEoptim - quite slow
system.time(op_tim <-  DEoptim(rastri_gin, 
                   upper=c(6, 6, 6, 6, 6, 6), lower=c(-6, -6, -6, -6, -6, -6), 
                   DEoptim.control(trace=FALSE, itermax=50, parallelType=1)))
# optimal parameters and value
op_tim$optim$bestmem
rastri_gin(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)


# vectorize and plot rastri_gin
rgl::persp3d(x=Vectorize(function(x, y) rastri_gin(vec_tor=c(x, y))), 
             xlim=c(-6, 6), ylim=c(-6, 6),
             col="green", axes=FALSE, zlab="", main="rastri_gin")


# doesn't work because function to outer() must be vectorized:
# http://stackoverflow.com/questions/31220495/dim-error-in-outer
# http://stackoverflow.com/questions/5554305/simple-question-regarding-the-use-of-outer-and-user-defined-functions
rgl::persp3d(x=function(x, y) rastri_gin(vec_tor=c(x, y)), 
             xlim=c(-6, 6), ylim=c(-6, 6),
             col="green", axes=FALSE, zlab="", main="rastri_gin")
# doesn't work because function to outer() must be vectorized:
rgl::persp3d(z=outer(seq(from=-6, to=6, by=0.1), 
                     seq(from=-6, to=6, by=0.1),
                     FUN=function(x, y) rastri_gin(vec_tor=c(x, y))), 
             col="green", axes=FALSE, zlab="", main="rastri_gin")


# rastri_gin vectorized version for plotting
rastri_gin <- function(x, y){
  x^2 + y^2 + 25*(sin(x)^2 + sin(y)^2)
}  # end rastri_gin
rastri_gin(1, 2)
rastri_gin(1, 2:3)

# draw 3d surface plot of function
rgl::persp3d(x=rastri_gin, 
             xlim=c(-6, 6), ylim=c(-6, 6),
             col="green", axes=FALSE, zlab="", main="rastri_gin")



## Rosenbrock banana function: vector argument version for optimization
Rosenbrock <- function(vec_tor){
  x1 <- vec_tor[1]
  x2 <- vec_tor[2]
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}  # end Rosenbrock
Rosenbrock(vec_tor=c(1, 2))


## optimize Rosenbrock
op_tim <-  DEoptim(Rosenbrock, 
                   upper=c(2, 2), lower=c(-2, -2), 
                   DEoptim.control(trace=FALSE, NP=80, itermax=400, F=1.2, CR=0.7))
# optimal parameters and value
op_tim$optim$bestmem
Rosenbrock(op_tim$optim$bestmem)
summary(op_tim)
plot(op_tim)


## Rosenbrock vectorized version for plotting
Rosenbrock <- function(x, y){
  100 * (y - x * x)^2 + (1 - x)^2
}  # end Rosenbrock
Rosenbrock(1, 2)

# draw 3d surface plot of function
rgl::persp3d(x=Rosenbrock, 
             xlim=c(-1, 1), ylim=c(-0.5, 1),
             col="green", axes=FALSE, zlab="", main="Rosenbrock")



### below is taken from C:\Users\Jerzy\Documents\R\win-library\3.3\DEoptim\demo\DEoptim.R

demo.DEoptim <- function(){

  'print.comments' <- function(str){
    star <- "**********"
    cat(paste("\n",star,"\n",str,"\n",star,"\n",sep=""))
  }
  
  'wait' <- function(){
    t <- readline("\nPlease 'q' to quit the demo or any other key to continue...\n")
    if (t == "q") TRUE else FALSE
  }
    
  'Rosenbrock' <- function(x){
    x1 <- x[1]
    x2 <- x[2]
    100 * (x2 - x1 * x1)^2 + (1 - x1)^2
  }

  'Wild' <- function(x)
    10 * sin(0.3*x) * sin(1.3*x^2) + 
      0.00001 * x^4 + 0.2 * x + 80

  'demo.1' <- function(){
    r <- DEoptim(Rosenbrock, rep(-10,2), rep(10,2))
    summary(r)
  }

  'demo.2' <- function(){
    r <- DEoptim(Rosenbrock, rep(-10,2), rep(10,2), 
                 control = list(NP = 100, trace = 1))
    summary(r)
  }

  'demo.3' <- function(){
    r <- DEoptim(Rosenbrock, rep(-10,2), rep(10,2), 
                 control = list(NP = 50, itermax = 300, F = 1.5, 
                   CR = 0.2, trace = 1))
    summary(r)
    plot(r, type = 'b')
  }

  'demo.4' <- function(){
    r <- DEoptim(Wild, lower = -50, upper = 50,
                 control = list(NP = 50, trace = 1))
    par(mfrow = c(2,1))
    plot(r, type = 'b')
    plot(r, plot.type = "bestvalit", type = 'l')
  }

  'demo.5' <- function(){
    r <- DEoptim(Wild, lower = -50, upper = 50,
                 control = list(NP = 50, trace = 1, digits = 8))
  }

  str.stop <- "end of the demo"
  tstr <- "\nRun the optimization process for the 'Rosenbrock'"
  tstr <- paste(tstr, "\nBanana function. Search space [-10,10]^2.\n", sep = "")
  print.comments(tstr)
  print(Rosenbrock)
  print(demo.1)
  if (wait()) stop(str.stop) else demo.1()
  
  tstr <- "\nDecrease to 100 the members in the population.\n"
  print.comments(tstr)
  print(demo.2)
  if (wait()) stop(str.stop) else demo.2()
  
  tstr <- "\nIncrease the number of iterations to 300, and"
  tstr <- paste(tstr, "\nmodify crossover and F parameters.\n", sep = "")
  tsts <- paste(tstr, "the result")
  print.comments(tstr)
  print(demo.3)
  if (wait()) stop(str.stop) else demo.3()
  
  tstr <- "\nRun the optimization process for the 'Wild' function."
  tstr <- paste(tstr, "\nSearch space [-50,50].\n", sep = "")
  print.comments(tstr)
  print(Wild)
  plot(Wild, -50, 50, n = 1000,
       main = "DEoptim minimizing 'Wild function'")
  if (wait()) stop(str.stop) else demo.4()

#  tstr <- "\nIncrease the number of printed digits"
#  print.comments(tstr)
#  if (wait()) stop(str.stop) else demo.5()
  
  cat("\n",str.stop,"\n")
}

demo.DEoptim()
  
