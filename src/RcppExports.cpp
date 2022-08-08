// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_stack
SEXP cpp_stack(List array_list, int along);
RcppExport SEXP _narray_cpp_stack(SEXP array_listSEXP, SEXP alongSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type array_list(array_listSEXP);
    Rcpp::traits::input_parameter< int >::type along(alongSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_stack(array_list, along));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_narray_cpp_stack", (DL_FUNC) &_narray_cpp_stack, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_narray(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
