#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector is_inside(NumericMatrix points) {

  LogicalVector inside(points.nrow());

  for(int i(1); i<=points.nrow(); i++) {
    double x = points(i-1, 0);
    double y = points(i-1, 1);
    if (x * x + y * y <= 1) {
      inside(i-1) = true;
    } else {
      inside(i-1) = false;
    }
  }   return inside;
}


// [[Rcpp::export]]
NumericMatrix df_pi(int B){

  NumericMatrix points(B, 2);

  points(_, 0) = runif(B, -1,1);
  points(_, 1) = runif(B, -1,1);

  return points;
}




