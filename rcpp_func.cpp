#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// Define timesTwo()
// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// This is another example using C++ STL types
inline double f(double x) { return ::log(::fabs(x)); }
// [[Rcpp::export]]
std::vector<double> logabs2(std::vector<double> x) {
  std::transform(x.begin(), x.end(), x.begin(), f);
  return x;
}


// Then you can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

// test timesTwo() in R
/*** R
timesTwo(42)
*/


// test logabs2() in R
/*** R
logabs2(seq(-5, 5, by=2))
*/

