// This file was automatically generated.
// tools::package_native_routine_registration_skeleton

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP ompr_check_for_unknown_vars_impl(SEXP, SEXP);
extern SEXP ompr_is_non_linear_impl(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"ompr_check_for_unknown_vars_impl", (DL_FUNC) &ompr_check_for_unknown_vars_impl, 2},
    {"ompr_is_non_linear_impl",          (DL_FUNC) &ompr_is_non_linear_impl,          2},
    {NULL, NULL, 0}
};

void R_init_ompr(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
