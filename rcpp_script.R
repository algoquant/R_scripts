# Rcpp scripts

# define Rcpp function
Rcpp::cppFunction("int timesToo(int x) { return 2 * x;}")
timesToo(3)

# source Rcpp function
Rcpp::sourceCpp(file="C:/Develop/R/scripts/rcpp_func.cpp")

timesTwo(3)

logabs2(seq(1, 15, by=2))

