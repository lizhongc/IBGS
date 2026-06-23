/*
 * R_export.c -- the single R interface layer: every .Call wrapper for the IBGS
 * package lives here.
 *
 * These functions only marshal R objects: they unpack the SEXP arguments,
 * coerce each data array to the storage mode the kernels read (REAL/INTEGER do
 * not coerce, so this is done here with coerceVector -- a no-op when the input
 * already has the right type), bracket the random-number usage with
 * GetRNGstate()/PutRNGstate(), call the pure-C computation (glm.c / cox.c /
 * rlm.c), and pack the results back into a named list.  Scalar arguments are
 * coerced inline by asInteger/asReal/asLogical.  All of the actual computation
 * lives in those translation units; no statistics happen here.  The entry
 * points are registered in init.c.
 *
 * The per-generation indicator matrix is NOT returned to R: it is allocated as
 * wrapper-owned scratch (R_Calloc), filled by the run, summarized in C
 * (summnm + glmsumm/coxsumm/rlmsumm refit the best n.models models), then freed.
 * Each sampler therefore returns the compact list(coef, model.ic, model.freq,
 * ic.trace, v.prob[, sel]); R only assembles the "IBGS" object around it.
 *
 * Two block + one standalone wrapper per family:
 *   gaussian / binomial / poisson : ibgs_glm, gibbs_sampler_glm
 *   Cox proportional hazards      : cox_ibgs_glm, cox_gibbs_glm
 *   linear mixed model (whitened) : rlm_ibgs_glm, rlm_gibbs_glm
 */
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h>
#include <string.h>

#include "ibgs.h"

/* ==========================================================================
 * Gaussian / binomial / poisson (computation in glm.c)
 * ======================================================================== */

/* ------------------------------------------------------------------ */
/* .Call: the whole iterated block Gibbs search + model-averaging summary. */
/*                                                                     */
/* Inputs: SEXP args mirror the registered signature in init.c; X is    */
/*   the n x p column-major design, pw the case weights, nmod the        */
/*   number of top models to summarize.                                  */
/* Side effect: brackets the RNG use with GetRNGstate()/PutRNGstate();   */
/*   the indicator matrix is scratch, summarized then freed in C.        */
/* Returns list(coef, model.ic, model.freq, ic.trace, v.prob, sel):    */
/*   coef       double[(1+p) x nm] top-model coefficients (intercept row)*/
/*   model.ic   double[nm]   criterion of each top model (ascending)     */
/*   model.freq double[nm]   visit frequency of each top model           */
/*   ic.trace   double[4*len] criterion of every recorded sample         */
/*   v.prob     double[p]    marginal inclusion probability (all columns) */
/*   sel        int[p.s]     1-based original indices of the x.s columns  */
/* ------------------------------------------------------------------ */
SEXP ibgs_glm(SEXP y, SEXP X, SEXP pw, SEXP niter, SEXP H, SEXP kapp, SEXP tau, SEXP perm, SEXP len, SEXP k, SEXP gam, SEXP info, SEXP fam, SEXP nthr, SEXP nmod)
{
    int n = nrows(X), p = ncols(X), family = asInteger(fam);
    SEXP y2 = PROTECT(coerceVector(y, REALSXP));
    SEXP X2 = PROTECT(coerceVector(X, REALSXP));
    SEXP pw2 = PROTECT(coerceVector(pw, REALSXP));
    int *xs = R_Calloc((size_t) p, int);
    int ps = 0, lenf = 0;

    GetRNGstate();
    int fail = ibgssel(REAL(y2), REAL(X2), REAL(pw2), n, p, asInteger(niter), asInteger(H), asInteger(kapp), asReal(tau), asLogical(perm), asInteger(len), asReal(k), asReal(gam), asInteger(info), family, asInteger(nthr), xs, &ps, &lenf);
    if (fail) {
        PutRNGstate();
        R_Free(xs);
        UNPROTECT(3);
        error("ibgs_glm: out of memory or numerical failure");
    }

    /* the per-generation indicator matrix is wrapper-owned scratch */
    int *omat = R_Calloc((size_t) lenf * (1 + ps), int);
    SEXP msic  = PROTECT(allocVector(REALSXP, lenf));
    SEXP vprob = PROTECT(allocVector(REALSXP, p));
    SEXP sel   = PROTECT(allocVector(INTSXP, ps));

    fail = ibgsrun(REAL(y2), REAL(X2), REAL(pw2), n, p, xs, ps, lenf, asLogical(perm), asReal(k), asReal(gam), asInteger(info), family, omat, REAL(msic), REAL(vprob), INTEGER(sel));
    PutRNGstate();
    if (fail) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(6);
        error("ibgs_glm: out of memory or numerical failure");
    }

    int nm_req = summnm(REAL(msic), lenf, asInteger(nmod));
    if (nm_req < 0) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(6);
        error("ibgs_glm: out of memory");
    }
    int nr = p + 1;
    SEXP coef = PROTECT(allocMatrix(REALSXP, nr, nm_req));
    SEXP mic  = PROTECT(allocVector(REALSXP, nm_req));
    SEXP mfrq = PROTECT(allocVector(REALSXP, nm_req));
    int nm = 0;
    fail = glmsumm(REAL(y2), REAL(X2), REAL(pw2), n, p, xs, ps, omat, REAL(msic), lenf, family, nm_req, REAL(coef), REAL(mic), REAL(mfrq), &nm);
    R_Free(omat);
    R_Free(xs);
    if (fail) {
        UNPROTECT(9);
        error("ibgs_glm: out of memory or numerical failure");
    }

    SEXP out = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(out, 0, coef);
    SET_VECTOR_ELT(out, 1, mic);
    SET_VECTOR_ELT(out, 2, mfrq);
    SET_VECTOR_ELT(out, 3, msic);
    SET_VECTOR_ELT(out, 4, vprob);
    SET_VECTOR_ELT(out, 5, sel);
    SEXP nms = PROTECT(allocVector(STRSXP, 6));
    SET_STRING_ELT(nms, 0, mkChar("coef"));
    SET_STRING_ELT(nms, 1, mkChar("model.ic"));
    SET_STRING_ELT(nms, 2, mkChar("model.freq"));
    SET_STRING_ELT(nms, 3, mkChar("ic.trace"));
    SET_STRING_ELT(nms, 4, mkChar("v.prob"));
    SET_STRING_ELT(nms, 5, mkChar("sel"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(11);
    return out;
}

/* ------------------------------------------------------------------ */
/* .Call: the standalone (non-block) Gibbs sampler + summary.          */
/*   Inputs mirror init.c; X is the n x p design, pw the weights, nmod  */
/*   the number of top models.  Returns list(coef, model.ic, model.freq,*/
/*   ic.trace, v.prob) (no sel: the search ranges over all p columns).  */
/* ------------------------------------------------------------------ */
SEXP gibbs_sampler_glm(SEXP y, SEXP X, SEXP pw, SEXP nvar, SEXP perm, SEXP len, SEXP k, SEXP gam, SEXP info, SEXP fam, SEXP nmod)
{
    int n = nrows(X), p = ncols(X), nlen = asInteger(len), family = asInteger(fam);
    SEXP y2 = PROTECT(coerceVector(y, REALSXP));
    SEXP X2 = PROTECT(coerceVector(X, REALSXP));
    SEXP pw2 = PROTECT(coerceVector(pw, REALSXP));

    int *omat = R_Calloc((size_t) nlen * (1 + p), int);
    int *xs   = R_Calloc((size_t) p, int);
    for (int j = 0; j < p; j++) xs[j] = j;
    SEXP msic  = PROTECT(allocVector(REALSXP, nlen));
    SEXP vprob = PROTECT(allocVector(REALSXP, p));

    GetRNGstate();
    int fail = gibbssam(REAL(y2), REAL(X2), REAL(pw2), n, p, asInteger(nvar), asLogical(perm), nlen, asReal(k), asReal(gam), asInteger(info), family, omat, REAL(msic), REAL(vprob));
    PutRNGstate();
    if (fail) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(5);
        error("gibbs_sampler_glm: out of memory or numerical failure");
    }

    int nm_req = summnm(REAL(msic), nlen, asInteger(nmod));
    if (nm_req < 0) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(5);
        error("gibbs_sampler_glm: out of memory");
    }
    int nr = p + 1;
    SEXP coef = PROTECT(allocMatrix(REALSXP, nr, nm_req));
    SEXP mic  = PROTECT(allocVector(REALSXP, nm_req));
    SEXP mfrq = PROTECT(allocVector(REALSXP, nm_req));
    int nm = 0;
    fail = glmsumm(REAL(y2), REAL(X2), REAL(pw2), n, p, xs, p, omat, REAL(msic), nlen, family, nm_req, REAL(coef), REAL(mic), REAL(mfrq), &nm);
    R_Free(omat);
    R_Free(xs);
    if (fail) {
        UNPROTECT(8);
        error("gibbs_sampler_glm: out of memory or numerical failure");
    }

    SEXP out = PROTECT(allocVector(VECSXP, 5));
    SET_VECTOR_ELT(out, 0, coef);
    SET_VECTOR_ELT(out, 1, mic);
    SET_VECTOR_ELT(out, 2, mfrq);
    SET_VECTOR_ELT(out, 3, msic);
    SET_VECTOR_ELT(out, 4, vprob);
    SEXP nms = PROTECT(allocVector(STRSXP, 5));
    SET_STRING_ELT(nms, 0, mkChar("coef"));
    SET_STRING_ELT(nms, 1, mkChar("model.ic"));
    SET_STRING_ELT(nms, 2, mkChar("model.freq"));
    SET_STRING_ELT(nms, 3, mkChar("ic.trace"));
    SET_STRING_ELT(nms, 4, mkChar("v.prob"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(10);
    return out;
}

/* ==========================================================================
 * Cox proportional hazards (computation in cox.c).
 * The survival response is the event-time vector `y` plus an integer `status`
 * vector (1 = event, 0 = censored).  The Cox model has no intercept, so the
 * summarized coef matrix is p (not 1+p) rows.
 * ======================================================================== */

/* ------------------------------------------------------------------ */
/* .Call: the whole Cox iterated block Gibbs search + summary.         */
/*   Returns list(coef, model.ic, model.freq, ic.trace, v.prob, sel).  */
/* ------------------------------------------------------------------ */
SEXP cox_ibgs_glm(SEXP y, SEXP st, SEXP X, SEXP pw, SEXP niter, SEXP H, SEXP kapp, SEXP tau, SEXP perm, SEXP len, SEXP k, SEXP gam, SEXP info, SEXP nthr, SEXP nmod)
{
    int n = nrows(X), p = ncols(X);
    SEXP y2 = PROTECT(coerceVector(y, REALSXP));
    SEXP st2 = PROTECT(coerceVector(st, INTSXP));
    SEXP X2 = PROTECT(coerceVector(X, REALSXP));
    SEXP pw2 = PROTECT(coerceVector(pw, REALSXP));
    int *xs = R_Calloc((size_t) p, int);
    int ps = 0, lenf = 0;

    GetRNGstate();
    int fail = coxibgsel(REAL(y2), INTEGER(st2), REAL(X2), REAL(pw2), n, p, asInteger(niter), asInteger(H), asInteger(kapp), asReal(tau), asLogical(perm), asInteger(len), asReal(k), asReal(gam), asInteger(info), asInteger(nthr), xs, &ps, &lenf);
    if (fail) {
        PutRNGstate();
        R_Free(xs);
        UNPROTECT(4);
        error("cox_ibgs_glm: out of memory or numerical failure");
    }

    int *omat = R_Calloc((size_t) lenf * (1 + ps), int);
    SEXP msic  = PROTECT(allocVector(REALSXP, lenf));
    SEXP vprob = PROTECT(allocVector(REALSXP, p));
    SEXP sel   = PROTECT(allocVector(INTSXP, ps));

    fail = coxibgrun(REAL(y2), INTEGER(st2), REAL(X2), REAL(pw2), n, p, xs, ps, lenf, asLogical(perm), asReal(k), asReal(gam), asInteger(info), omat, REAL(msic), REAL(vprob), INTEGER(sel));
    PutRNGstate();
    if (fail) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(7);
        error("cox_ibgs_glm: out of memory or numerical failure");
    }

    int nm_req = summnm(REAL(msic), lenf, asInteger(nmod));
    if (nm_req < 0) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(7);
        error("cox_ibgs_glm: out of memory");
    }
    SEXP coef = PROTECT(allocMatrix(REALSXP, p, nm_req));
    SEXP mic  = PROTECT(allocVector(REALSXP, nm_req));
    SEXP mfrq = PROTECT(allocVector(REALSXP, nm_req));
    int nm = 0;
    fail = coxsumm(REAL(y2), INTEGER(st2), REAL(pw2), REAL(X2), n, p, xs, ps, omat, REAL(msic), lenf, nm_req, REAL(coef), REAL(mic), REAL(mfrq), &nm);
    R_Free(omat);
    R_Free(xs);
    if (fail) {
        UNPROTECT(10);
        error("cox_ibgs_glm: out of memory or numerical failure");
    }

    SEXP out = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(out, 0, coef);
    SET_VECTOR_ELT(out, 1, mic);
    SET_VECTOR_ELT(out, 2, mfrq);
    SET_VECTOR_ELT(out, 3, msic);
    SET_VECTOR_ELT(out, 4, vprob);
    SET_VECTOR_ELT(out, 5, sel);
    SEXP nms = PROTECT(allocVector(STRSXP, 6));
    SET_STRING_ELT(nms, 0, mkChar("coef"));
    SET_STRING_ELT(nms, 1, mkChar("model.ic"));
    SET_STRING_ELT(nms, 2, mkChar("model.freq"));
    SET_STRING_ELT(nms, 3, mkChar("ic.trace"));
    SET_STRING_ELT(nms, 4, mkChar("v.prob"));
    SET_STRING_ELT(nms, 5, mkChar("sel"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(12);
    return out;
}

/* ------------------------------------------------------------------ */
/* .Call: the standalone (non-block) Cox Gibbs sampler + summary.      */
/*   Returns list(coef, model.ic, model.freq, ic.trace, v.prob).       */
/* ------------------------------------------------------------------ */
SEXP cox_gibbs_glm(SEXP y, SEXP st, SEXP X, SEXP pw, SEXP nvar, SEXP perm, SEXP len, SEXP k, SEXP gam, SEXP info, SEXP nmod)
{
    int n = nrows(X), p = ncols(X), nlen = asInteger(len);
    SEXP y2 = PROTECT(coerceVector(y, REALSXP));
    SEXP st2 = PROTECT(coerceVector(st, INTSXP));
    SEXP X2 = PROTECT(coerceVector(X, REALSXP));
    SEXP pw2 = PROTECT(coerceVector(pw, REALSXP));

    int *omat = R_Calloc((size_t) nlen * (1 + p), int);
    int *xs   = R_Calloc((size_t) p, int);
    for (int j = 0; j < p; j++) xs[j] = j;
    SEXP msic  = PROTECT(allocVector(REALSXP, nlen));
    SEXP vprob = PROTECT(allocVector(REALSXP, p));

    GetRNGstate();
    int fail = coxgbsam(REAL(y2), INTEGER(st2), REAL(X2), REAL(pw2), n, p, asInteger(nvar), asLogical(perm), nlen, asReal(k), asReal(gam), asInteger(info), omat, REAL(msic), REAL(vprob));
    PutRNGstate();
    if (fail) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(6);
        error("cox_gibbs_glm: out of memory or numerical failure");
    }

    int nm_req = summnm(REAL(msic), nlen, asInteger(nmod));
    if (nm_req < 0) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(6);
        error("cox_gibbs_glm: out of memory");
    }
    SEXP coef = PROTECT(allocMatrix(REALSXP, p, nm_req));
    SEXP mic  = PROTECT(allocVector(REALSXP, nm_req));
    SEXP mfrq = PROTECT(allocVector(REALSXP, nm_req));
    int nm = 0;
    fail = coxsumm(REAL(y2), INTEGER(st2), REAL(pw2), REAL(X2), n, p, xs, p, omat, REAL(msic), nlen, nm_req, REAL(coef), REAL(mic), REAL(mfrq), &nm);
    R_Free(omat);
    R_Free(xs);
    if (fail) {
        UNPROTECT(9);
        error("cox_gibbs_glm: out of memory or numerical failure");
    }

    SEXP out = PROTECT(allocVector(VECSXP, 5));
    SET_VECTOR_ELT(out, 0, coef);
    SET_VECTOR_ELT(out, 1, mic);
    SET_VECTOR_ELT(out, 2, mfrq);
    SET_VECTOR_ELT(out, 3, msic);
    SET_VECTOR_ELT(out, 4, vprob);
    SEXP nms = PROTECT(allocVector(STRSXP, 5));
    SET_STRING_ELT(nms, 0, mkChar("coef"));
    SET_STRING_ELT(nms, 1, mkChar("model.ic"));
    SET_STRING_ELT(nms, 2, mkChar("model.freq"));
    SET_STRING_ELT(nms, 3, mkChar("ic.trace"));
    SET_STRING_ELT(nms, 4, mkChar("v.prob"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(11);   /* y2, st2, X2, pw2, msic, vprob, coef, mic, mfrq, out, nms */
    return out;
}

/* ==========================================================================
 * Linear mixed model, fixed-effect selection (computation in rlm.c).
 * The data arrive ALREADY WHITENED from R (ystar = L^-1 y,
 * Xstar = L^-1 X, istar = L^-1 1, with the held marginal covariance V = L L');
 * ldv0 = log|V0| is the constant added to the criterion.  The summarized coef is
 * 1+p rows (intercept first) and is reported in the ORIGINAL space.
 * ======================================================================== */

/* ------------------------------------------------------------------ */
/* .Call: the whole rlm iterated block Gibbs search + summary.         */
/*   Returns list(coef, model.ic, model.freq, ic.trace, v.prob, sel).  */
/* ------------------------------------------------------------------ */
SEXP rlm_ibgs_glm(SEXP ys, SEXP Xst, SEXP ist, SEXP niter, SEXP H, SEXP kapp, SEXP tau, SEXP perm, SEXP len, SEXP k, SEXP gam, SEXP info, SEXP ldv0, SEXP nthr, SEXP nmod)
{
    int n = nrows(Xst), p = ncols(Xst);
    SEXP ys2 = PROTECT(coerceVector(ys, REALSXP));
    SEXP Xst2 = PROTECT(coerceVector(Xst, REALSXP));
    SEXP ist2 = PROTECT(coerceVector(ist, REALSXP));
    int *xs = R_Calloc((size_t) p, int);
    int ps = 0, lenf = 0;

    GetRNGstate();
    int fail = rlmibgsel(REAL(ys2), REAL(Xst2), REAL(ist2), n, p, asInteger(niter), asInteger(H), asInteger(kapp), asReal(tau), asLogical(perm), asInteger(len), asReal(k), asReal(gam), asInteger(info), asReal(ldv0), asInteger(nthr), xs, &ps, &lenf);
    if (fail) {
        PutRNGstate();
        R_Free(xs);
        UNPROTECT(3);
        error("rlm_ibgs_glm: out of memory or numerical failure");
    }

    int *omat = R_Calloc((size_t) lenf * (1 + ps), int);
    SEXP msic  = PROTECT(allocVector(REALSXP, lenf));
    SEXP vprob = PROTECT(allocVector(REALSXP, p));
    SEXP sel   = PROTECT(allocVector(INTSXP, ps));

    fail = rlmibgrun(REAL(ys2), REAL(Xst2), REAL(ist2), n, p, xs, ps, lenf, asLogical(perm), asReal(k), asReal(gam), asInteger(info), asReal(ldv0), omat, REAL(msic), REAL(vprob), INTEGER(sel));
    PutRNGstate();
    if (fail) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(6);
        error("rlm_ibgs_glm: out of memory or numerical failure");
    }

    int nm_req = summnm(REAL(msic), lenf, asInteger(nmod));
    if (nm_req < 0) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(6);
        error("rlm_ibgs_glm: out of memory");
    }
    int nr = p + 1;
    SEXP coef = PROTECT(allocMatrix(REALSXP, nr, nm_req));
    SEXP mic  = PROTECT(allocVector(REALSXP, nm_req));
    SEXP mfrq = PROTECT(allocVector(REALSXP, nm_req));
    int nm = 0;
    fail = rlmsumm(REAL(ys2), REAL(Xst2), REAL(ist2), n, p, xs, ps, omat, REAL(msic), lenf, nm_req, REAL(coef), REAL(mic), REAL(mfrq), &nm);
    R_Free(omat);
    R_Free(xs);
    if (fail) {
        UNPROTECT(9);
        error("rlm_ibgs_glm: out of memory or numerical failure");
    }

    SEXP out = PROTECT(allocVector(VECSXP, 6));
    SET_VECTOR_ELT(out, 0, coef);
    SET_VECTOR_ELT(out, 1, mic);
    SET_VECTOR_ELT(out, 2, mfrq);
    SET_VECTOR_ELT(out, 3, msic);
    SET_VECTOR_ELT(out, 4, vprob);
    SET_VECTOR_ELT(out, 5, sel);
    SEXP nms = PROTECT(allocVector(STRSXP, 6));
    SET_STRING_ELT(nms, 0, mkChar("coef"));
    SET_STRING_ELT(nms, 1, mkChar("model.ic"));
    SET_STRING_ELT(nms, 2, mkChar("model.freq"));
    SET_STRING_ELT(nms, 3, mkChar("ic.trace"));
    SET_STRING_ELT(nms, 4, mkChar("v.prob"));
    SET_STRING_ELT(nms, 5, mkChar("sel"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(11);
    return out;
}

/* ------------------------------------------------------------------ */
/* .Call: the standalone (non-block) rlm Gibbs sampler + summary.      */
/*   Returns list(coef, model.ic, model.freq, ic.trace, v.prob).       */
/* ------------------------------------------------------------------ */
SEXP rlm_gibbs_glm(SEXP ys, SEXP Xst, SEXP ist, SEXP nvar, SEXP perm, SEXP len, SEXP k, SEXP gam, SEXP info, SEXP ldv0, SEXP nmod)
{
    int n = nrows(Xst), p = ncols(Xst), nlen = asInteger(len);
    SEXP ys2 = PROTECT(coerceVector(ys, REALSXP));
    SEXP Xst2 = PROTECT(coerceVector(Xst, REALSXP));
    SEXP ist2 = PROTECT(coerceVector(ist, REALSXP));

    int *omat = R_Calloc((size_t) nlen * (1 + p), int);
    int *xs   = R_Calloc((size_t) p, int);
    for (int j = 0; j < p; j++) xs[j] = j;
    SEXP msic  = PROTECT(allocVector(REALSXP, nlen));
    SEXP vprob = PROTECT(allocVector(REALSXP, p));

    GetRNGstate();
    int fail = rlmgbsam(REAL(ys2), REAL(Xst2), REAL(ist2), n, p, asInteger(nvar), asLogical(perm), nlen, asReal(k), asReal(gam), asInteger(info), asReal(ldv0), omat, REAL(msic), REAL(vprob));
    PutRNGstate();
    if (fail) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(5);
        error("rlm_gibbs_glm: out of memory or numerical failure");
    }

    int nm_req = summnm(REAL(msic), nlen, asInteger(nmod));
    if (nm_req < 0) {
        R_Free(omat);
        R_Free(xs);
        UNPROTECT(5);
        error("rlm_gibbs_glm: out of memory");
    }
    int nr = p + 1;
    SEXP coef = PROTECT(allocMatrix(REALSXP, nr, nm_req));
    SEXP mic  = PROTECT(allocVector(REALSXP, nm_req));
    SEXP mfrq = PROTECT(allocVector(REALSXP, nm_req));
    int nm = 0;
    fail = rlmsumm(REAL(ys2), REAL(Xst2), REAL(ist2), n, p, xs, p, omat, REAL(msic), nlen, nm_req, REAL(coef), REAL(mic), REAL(mfrq), &nm);
    R_Free(omat);
    R_Free(xs);
    if (fail) {
        UNPROTECT(8);
        error("rlm_gibbs_glm: out of memory or numerical failure");
    }

    SEXP out = PROTECT(allocVector(VECSXP, 5));
    SET_VECTOR_ELT(out, 0, coef);
    SET_VECTOR_ELT(out, 1, mic);
    SET_VECTOR_ELT(out, 2, mfrq);
    SET_VECTOR_ELT(out, 3, msic);
    SET_VECTOR_ELT(out, 4, vprob);
    SEXP nms = PROTECT(allocVector(STRSXP, 5));
    SET_STRING_ELT(nms, 0, mkChar("coef"));
    SET_STRING_ELT(nms, 1, mkChar("model.ic"));
    SET_STRING_ELT(nms, 2, mkChar("model.freq"));
    SET_STRING_ELT(nms, 3, mkChar("ic.trace"));
    SET_STRING_ELT(nms, 4, mkChar("v.prob"));
    setAttrib(out, R_NamesSymbol, nms);

    UNPROTECT(10);
    return out;
}
