// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// updateAndSubset
List updateAndSubset(Rcpp::DateVector From, Rcpp::DateVector To, int max_gap, Rcpp::Nullable<Rcpp::List> startObjects, Rcpp::Nullable<Rcpp::List> endObjects);
RcppExport SEXP _neatRanges_updateAndSubset(SEXP FromSEXP, SEXP ToSEXP, SEXP max_gapSEXP, SEXP startObjectsSEXP, SEXP endObjectsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DateVector >::type From(FromSEXP);
    Rcpp::traits::input_parameter< Rcpp::DateVector >::type To(ToSEXP);
    Rcpp::traits::input_parameter< int >::type max_gap(max_gapSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::List> >::type startObjects(startObjectsSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::List> >::type endObjects(endObjectsSEXP);
    rcpp_result_gen = Rcpp::wrap(updateAndSubset(From, To, max_gap, startObjects, endObjects));
    return rcpp_result_gen;
END_RCPP
}
// updateAndSubsetTime
List updateAndSubsetTime(Rcpp::DatetimeVector From, Rcpp::DatetimeVector To, int max_gap, Rcpp::Nullable<Rcpp::List> startObjects, Rcpp::Nullable<Rcpp::List> endObjects);
RcppExport SEXP _neatRanges_updateAndSubsetTime(SEXP FromSEXP, SEXP ToSEXP, SEXP max_gapSEXP, SEXP startObjectsSEXP, SEXP endObjectsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DatetimeVector >::type From(FromSEXP);
    Rcpp::traits::input_parameter< Rcpp::DatetimeVector >::type To(ToSEXP);
    Rcpp::traits::input_parameter< int >::type max_gap(max_gapSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::List> >::type startObjects(startObjectsSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<Rcpp::List> >::type endObjects(endObjectsSEXP);
    rcpp_result_gen = Rcpp::wrap(updateAndSubsetTime(From, To, max_gap, startObjects, endObjects));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_neatRanges_updateAndSubset", (DL_FUNC) &_neatRanges_updateAndSubset, 5},
    {"_neatRanges_updateAndSubsetTime", (DL_FUNC) &_neatRanges_updateAndSubsetTime, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_neatRanges(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
