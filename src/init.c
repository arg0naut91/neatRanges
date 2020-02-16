#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _neatRanges_BezRcpp(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_neatRanges_BezRcpp", (DL_FUNC) &_neatRanges_BezRcpp, 1},
    {NULL, NULL, 0}
};

void R_init_neatRanges(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
