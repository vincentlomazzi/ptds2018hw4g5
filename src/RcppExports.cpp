// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// is_inside
LogicalVector is_inside(NumericMatrix points);
RcppExport SEXP _ptds2018hw4g5_is_inside(SEXP pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type points(pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(is_inside(points));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ptds2018hw4g5_is_inside", (DL_FUNC) &_ptds2018hw4g5_is_inside, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_ptds2018hw4g5(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
