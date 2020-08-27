#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
sp_mat cppArmadilloDot (sp_mat x, sp_mat y) {
        return(x * y.t()) ;
}
