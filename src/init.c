#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP ibgs_glm(SEXP y_, SEXP X_, SEXP weights_, SEXP niter_, SEXP H_, SEXP kapp_, SEXP tau_, SEXP perm_, SEXP fast_, SEXP start_, SEXP len_, SEXP k_, SEXP gamma_, SEXP info_, SEXP family_, SEXP nthr_, SEXP nmodels_);

extern SEXP gibbs_sampler_glm(SEXP y_, SEXP X_, SEXP weights_, SEXP nvars_, SEXP perm_, SEXP fast_, SEXP start_, SEXP len_, SEXP k_, SEXP gamma_, SEXP info_, SEXP family_, SEXP nmodels_);

extern SEXP cox_ibgs_glm(SEXP y_, SEXP status_, SEXP X_, SEXP weights_, SEXP niter_, SEXP H_, SEXP kapp_, SEXP tau_, SEXP perm_, SEXP start_, SEXP len_, SEXP k_, SEXP gamma_, SEXP info_, SEXP nthr_, SEXP nmodels_);

extern SEXP cox_gibbs_glm(SEXP y_, SEXP status_, SEXP X_, SEXP weights_, SEXP nvars_, SEXP perm_, SEXP start_, SEXP len_, SEXP k_, SEXP gamma_, SEXP info_, SEXP nmodels_);

extern SEXP rlm_ibgs_glm(SEXP ystar_, SEXP Xstar_, SEXP istar_, SEXP niter_, SEXP H_, SEXP kapp_, SEXP tau_, SEXP perm_, SEXP start_, SEXP len_, SEXP k_, SEXP gamma_, SEXP info_, SEXP logdetV0_, SEXP nthr_, SEXP nmodels_);

extern SEXP rlm_gibbs_glm(SEXP ystar_, SEXP Xstar_, SEXP istar_, SEXP nvars_, SEXP perm_, SEXP start_, SEXP len_, SEXP k_, SEXP gamma_, SEXP info_, SEXP logdetV0_, SEXP nmodels_);

static const R_CallMethodDef CallEntries[] = {
    {"ibgs_glm",          (DL_FUNC) &ibgs_glm,          17},
    {"gibbs_sampler_glm", (DL_FUNC) &gibbs_sampler_glm, 13},
    {"cox_ibgs_glm",      (DL_FUNC) &cox_ibgs_glm,      16},
    {"cox_gibbs_glm",     (DL_FUNC) &cox_gibbs_glm,     12},
    {"rlm_ibgs_glm",      (DL_FUNC) &rlm_ibgs_glm,      16},
    {"rlm_gibbs_glm",     (DL_FUNC) &rlm_gibbs_glm,     12},
    {NULL, NULL, 0}
};

void R_init_IBGS(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
