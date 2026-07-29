/*
 * ibgs.h -- the single package header: every shared C declaration for the
 * IBGS package lives here.
 *
 * It collects the thread-safe RNG type, the cross-module constants, the shared
 * linear-algebra and fitting kernels (Cholesky solve, gaussian Gram RSS, the
 * IRLS GLM fitter, the Cox Efron-partial-likelihood fitter), the per-thread
 * sampler workspaces, the Metropolis-within-Gibbs samplers for all four
 * families (gaussian/binomial/poisson, Cox, and the whitened linear mixed
 * model), and the iterated-block-Gibbs orchestration entry points.  Every .c
 * file in src/ includes this one header.
 *
 * No R object (SEXP) handling lives here: these routines take plain C arrays and
 * write plain C arrays.  They do use R's unif_rand() for reproducible random
 * draws on the serial path, so the caller must bracket the call with
 * GetRNGstate()/PutRNGstate().  Errors are reported through the return code (no
 * R error()/longjmp).
 */
#ifndef IBGS_IBGS_H
#define IBGS_IBGS_H

/* The R and C standard headers the package's modules use, collected here rather than
 * repeated in each .c file.  stdint.h is needed by this header itself, for rngt below.
 *
 * Three groups are deliberately NOT here.  Rinternals.h and R_ext/Rdynload.h are
 * SEXP-level and belong with the .Call shims (R_export.c, init.c), per the no-R-objects
 * note above.  R_ext/BLAS.h and R_ext/Lapack.h stay in glm.c and cox.c, which must define
 * USE_FC_LEN_T immediately before them.  omp.h stays behind each file's _OPENMP guard. */
#include <R.h>
#include <Rmath.h>

#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/* ============================================================================
 * Thread-safe RNG (xoshiro256+ + splitmix64), implemented in rng.c.
 *
 * Independent of R: a plain C uniform[0,1) generator that can be driven from any
 * thread.  Each block of the parallel sampler owns its own rngt, seeded
 * deterministically from R's RNG, so results are reproducible and independent of
 * the number of threads.
 * ========================================================================== */

typedef struct { uint64_t s[4]; } rngt;

/* Seed the generator from a single 64-bit value. */
void rngseed(rngt *, uint64_t);

/* Next uniform double in [0, 1) with 53-bit resolution. */
double rngunif(rngt *);

/* response-family codes shared by the GLM fitter and the samplers */
#define FAM_GAUSSIAN 0
#define FAM_BINOMIAL 1
#define FAM_POISSON  2

/* Largest coefficient count for which the gaussian sampler scores all candidate
 * additions in one batch (see addbatch in glm.c); larger models fall back to the
 * per-proposal Cholesky.  The batch replaces one O(q^3) factorisation per
 * candidate with one O(q^2) triangular solve for all of them, so it pays exactly
 * while q is small -- which is also where it is best conditioned, and where the
 * size cap is not already discarding the additions for free.  It bounds the panel
 * workspace at capt * GBQMAX doubles instead of capt^2. */
#define GBQMAX 64

/* ============================================================================
 * Shared kernels (implemented in glm.c).
 * ========================================================================== */

/*
 * Solve the symmetric positive-definite system A x = b by Cholesky (A is q x q
 * row-major; only the lower triangle is read and is overwritten by its factor).
 * Returns 1 if a pivot is non-positive (the matrix is singular / the model has
 * collinear columns), else 0.  Shared with the single-model refit in
 * glm.c (glmcoef) for the gaussian OLS solve.
 */
int cholsolv(double *, const double *, double *, int);

/*
 * Information criterion from -2*logLik.
 *   npar  : number of estimated parameters (coefficients + dispersion for
 *           gaussian; coefficients only for binomial/poisson)
 *   npred : number of predictors excluding the intercept (for exBIC)
 * info codes: 0 = AIC, 1 = BIC, 2 = AICc, 3 = exBIC.
 */
double icval(double, int, int, int, int, double, int);

/*
 * IRLS fit of y on the columns of the design Dfull selected by `active`.  Dfull
 * is the full n x (1+P) block design (column-major) with column 0 the intercept
 * (ones) and columns 1..P the predictors; the fitted model uses the q columns
 * Dfull[, active[0..q-1]] (active[0] = 0 selects the intercept).  Indexing this
 * shared design avoids rebuilding a compacted design matrix on every fit.
 *
 * pw holds the prior weights (length n; pass all ones for the
 * unweighted fit).  For the binomial family y may be a fraction (proportion of
 * successes) with pw the number of trials.  On success writes -2*logLik
 * (matching R's glm() scale, i.e. AIC = dev2 + 2*npar) to *dev2 and returns 0;
 * returns 1 on failure (singular weighted normal equations or non-convergence).
 * family must be FAM_BINOMIAL or FAM_POISSON.
 *
 * Warm start: if b0 != NULL (length q) the linear predictor is initialised to
 * eta = D b0 instead of the family default, which lets a fit started from a
 * nearby model's coefficients converge in far fewer iterations.  The converged
 * fit (and *dev2) is unchanged.  If bout != NULL (length q) the fitted
 * coefficients are written there.
 *
 * maxit caps the IRLS iterations: pass IRLS_MAXIT for a full fit, or 1 for a
 * single warm-started Newton step (the approximate proposal score the sampler
 * uses; only meaningful with a warm start b0 close to the optimum).
 *
 * cwz supplies the first iteration's weights instead of deriving them, as 3*n
 * doubles holding u = w*z, sqrt(w) and w in that order; the fit itself reads only
 * the first two, w being there for the batched scorer that shares the same cache.
 * Pass NULL to derive them as usual.  It exists for the sampler's addition
 * proposals: a new column warm-starts at zero, so eta = D b0 is the current model's
 * linear predictor whatever column is being proposed, and every quantity the first
 * Newton step derives from eta is therefore the same for all of them.  Supplying
 * the cache skips both the warm-start eta pass and the weight pass; the values are
 * the ones those passes would have produced, so the fit is unchanged.  Only
 * meaningful with maxit == 1 and a warm start whose new coordinate is zero -- with
 * more iterations the later ones re-derive the weights from their own eta anyway.
 *
 * Scratch buffers supplied by the caller (reused across fits):
 *   wq    : at least q*q + 3*q  doubles  (XtWX, XtWz, beta, betaold)
 *   wn    : at least 5*n        doubles  (eta, mu, w, z, u)
 *   Dpack : at least n*q        doubles  (active columns gathered contiguously)
 *   Dw    : at least n*q        doubles  (sqrt(w)-scaled design for the BLAS syrk)
 */
int glmirls(int, const double *, const double *, const double *, const int *, int, int, int, double *, double *, double *, double *, double *, const double *, double *, const double *);

/*
 * Gaussian OLS residual sum of squares of the active columns, via a Cholesky on
 * the active sub-block of a precomputed Gram matrix G (ptot1 x ptot1) and
 * cross-product Gy.  active[0..q-1] indexes the q active columns; yty = y'Wy.
 * Sbuf (>= q*q) and bbuf (>= q) are caller scratch.  Returns RSS (>= 1e-12) or a
 * negative value if the active sub-block is not positive definite (collinear).
 * Shared with the lme whitened-OLS sampler (lme.c).
 */
double rsschol(const double *, const double *, int, const int *, int, double, double *, double *);

/*
 * Cox kernels (implemented in cox.c).  This family is self-contained: unlike
 * the gaussian/binomial/poisson path it does not share code with glm.c.  The
 * Cox model has no intercept (the baseline hazard absorbs it and it cancels in
 * the partial likelihood) and its response is a (time, status) pair coupled
 * across observations through risk sets, so it is fitted by Newton-Raphson here
 * rather than by IRLS.
 */

/*
 * Information criterion of a Cox model from its -2*partial-logLik.
 *   npred : number of predictors in the model (Cox has no intercept and no
 *           dispersion, so npar == npred for every criterion).
 *   d     : number of events (uncensored observations); used as the effective
 *           sample size in BIC/AICc/exBIC (Volinsky & Raftery convention).
 *   p0    : size of the candidate pool, for exBIC.
 * Lower is better.  Mirrors icval() but with the Cox parameter counts and the
 * event-based sample size baked in.
 */
double coxicval(double, int, int, int, double, int);

/*
 * Fit a Cox model by Newton-Raphson on the Efron partial log-likelihood.
 *
 * Dfull is the n x P design (column-major) of the candidate predictors -- there
 * is NO intercept column, unlike the GLM design.  The fitted model uses the q
 * columns Dfull[, active[0..q-1]]; indexing this shared design avoids rebuilding
 * a compact design on every fit.
 *
 * time   : event/censoring time of each observation (length n).
 * status : 1 = event (death), 0 = censored (length n).
 * pw     : prior case weights (length n; pass all ones for the unweighted fit).
 * order  : observation indices sorted by time DESCENDING (length n).  The caller
 *          computes this once per run and reuses it across all candidate fits so
 *          the risk-set sweep is a single linear pass.
 *
 * On success writes -2*partial-logLik to *m2pll and returns 0; returns 1 on a
 * singular Hessian (collinear model) or non-convergence, so the caller rejects
 * that model.
 *
 * Warm start: if b0 != NULL (length q) the linear predictor is initialised to
 * eta = D b0 instead of zero, so a fit started from a neighbouring model's
 * coefficients converges in a couple of Newton steps.  If bout != NULL
 * (length q) the fitted coefficients are written there.
 *
 * Scratch buffers supplied by the caller (reused across fits):
 *   wq : at least 3*q*q + 7*q  doubles
 *   wn : at least 2*n          doubles
 */
int coxfit(const double *, const int *, const double *, const double *, const int *, int, int, const int *, double *, double *, double *, const double *, double *);

/* ============================================================================
 * Per-thread sampler workspaces.
 *
 * Each packs all the scratch one sampler invocation (and one screening block's
 * column gather) needs, allocated once and reused so the OpenMP block loop
 * performs no allocation.  Allocate and free on the MAIN thread only (R_Calloc
 * is not thread-safe); worker threads merely index their own element of the
 * workspace array.
 * ========================================================================== */

/*
 * Gaussian/binomial/poisson sampler workspace, sized for a design of up to
 * capt = 1 + p1 + p2 columns over n rows.
 */
typedef struct {
    int     capt;        /* column capacity (1 + max p1 + p2) it was sized for */
    int     n;           /* rows */
    int     family;      /* FAM_* this workspace was sized for */
    int    *inc;         /* inclusion indicator scratch                  */
    int    *active;      /* active-column list scratch                   */
    int    *ord;         /* per-sweep random visit order (Fisher-Yates)  */
    double *G, *Gy, *Sbuf, *bbuf;                /* gaussian Gram path */
    double *D, *wq, *wn, *bfull, *b0, *bprop;   /* GLM IRLS path      */
    double *Dpack, *Dw;  /* GLM IRLS: packed active design + sqrt(w)-scaled copy */
    /* GLM IRLS addition proposals: the first Newton step's u = w*z and sqrt(w),
     * which depend only on the current model's linear predictor and so are shared
     * by every candidate column (see the cwz argument of glmirls), and w itself for
     * the batched scorer that shares them.  3*n doubles. */
    double *cwz;
    /* GLM IRLS batched add-scoring (see irlsbatch in glm.c): Gpan is the panel of
     * every candidate's weighted cross-products with the active columns, reduced in
     * place first to u_j then to v_j, with one extra column carrying the active
     * right-hand side; Gfac the Cholesky factor of the active block; Gwb its forward
     * solve, then the step's active coefficients; Ggd and Ggv the candidate weighted
     * diagonal and right-hand side; Gs2 and Gcj the new pivot and the step's
     * coefficient on the candidate column; GDws the weight-scaled active design;
     * Getah the step's linear predictor for the active model alone; Getac and Gmuc
     * per-candidate scratch.  Panel width is bounded by GBQMAX. */
    double *Gpan, *Gfac, *Gwb, *Ggd, *Ggv, *Gs2, *Gcj, *GDws, *Getah, *Getac, *Gmuc;
    int    *Gact;        /* the active set the panel was built from */
    int    *bcols, *s0;  /* screening: gathered column ids / start model  */
    double *fr, *Xb;     /* screening: block freqs / gathered block design */
    /* gaussian batched add-scoring (see addbatch in glm.c): Ufac holds the
     * Cholesky factor of the current model's Gram sub-block, Uw the forward
     * solve against its right-hand side, Upan the panel of all candidate
     * columns solved against Ufac, and zsq / spiv the resulting per-candidate
     * fit improvement and new Cholesky pivot.  Sized for at most GBQMAX
     * coefficients since the batch is only used for small models. */
    double *Ufac, *Uw, *Upan, *zsq, *spiv;
} gbwst;

/*
 * Allocate (R_Calloc, main thread only) the buffers of `ws` for a design of up
 * to capt columns over n rows in the given family, and free them.  On
 * allocation failure R_Calloc raises an R error (main thread), so gbwsallc
 * always returns 0 on return.
 */
int  gbwsallc(gbwst *, int, int, int);
void gbwsfree(gbwst *);

/* (time, index) pair, sorted by time descending to build the Cox risk-set order. */
typedef struct { double t; int idx; } tit;

/*
 * Cox sampler workspace, sized for a design of up to capt predictors (the
 * Cox model has no intercept) over n rows.
 */
typedef struct {
    int     capt;        /* predictor capacity (max p1 + p2) it was sized for */
    int     n;           /* rows */
    int    *inc;         /* inclusion indicator scratch       */
    int    *active;      /* active-column list scratch        */
    int    *ord;         /* per-sweep random visit order (Fisher-Yates) */
    int    *order;       /* observations sorted by time desc. */
    int    *bcols, *s0;  /* screening: gathered column ids / start model */
    double *wq, *wn, *bfull, *b0, *bprop;  /* per-fit scratch */
    double *fr, *Xb;     /* screening: block freqs / gathered block design */
    tit    *pairs;       /* time-sort scratch */
} cxwst;

/* Allocate (R_Calloc, main thread) / free a Cox workspace for up to capt
 * predictors over n rows.  On out of memory R_Calloc raises an R error. */
int  cxwsallc(cxwst *, int, int);
void cxwsfree(cxwst *);

/*
 * lme sampler workspace.  The lme fit is whitened OLS (no IRLS and no warm-start
 * state), so it needs only the gaussian Gram buffers, sized for a design of up
 * to capt = 1 + p1 + p2 columns over n rows.
 */
typedef struct {
    int     capt;        /* column capacity (1 + max p1 + p2) it was sized for */
    int     n;           /* rows */
    int    *inc;         /* inclusion indicator scratch  */
    int    *active;      /* active-column list scratch   */
    int    *ord;         /* per-sweep random visit order (Fisher-Yates) */
    double *G, *Gy, *Sbuf, *bbuf;  /* whitened Gram path */
    int    *bcols, *s0;  /* screening: gathered column ids / start model */
    double *fr, *Xb;     /* screening: block freqs / gathered block design */
} lmewst;

/* Allocate (R_Calloc, main thread) / free an lme workspace for up to capt
 * columns over n rows.  On out of memory R_Calloc raises an R error. */
int  lmewsallc(lmewst *, int, int);
void lmewsfree(lmewst *);

/* ============================================================================
 * Metropolis-within-Gibbs samplers (one step runner per family).
 * ========================================================================== */

/*
 * The gaussian/binomial/poisson Metropolis-within-Gibbs sampler (glm.c).
 *
 * X is n x (p1+p2), columns = [x1 (p1, toggleable) | x2 (p2, always in)].
 * pw holds the prior weights (length n; all ones for the unweighted fit).
 * rng == NULL uses R's unif_rand (main thread only); otherwise the supplied
 * thread-safe generator is used.
 *
 * ws is a caller-provided workspace sized for at least this design (>= 1+p1+p2
 * columns, n rows, matching family); pass NULL to have rungibbs allocate and
 * free its own (only safe on the main thread).  Worker threads must pass a
 * pre-allocated ws.
 *
 * Any of the following outputs may be filled (pass NULL to skip):
 *   omat : int[len * (1+p1+p2)] column-major indicator matrix
 *          ([intercept | x1 | x2]).
 *   ofrq : double[p1] marginal inclusion probability of the x1 block.
 *   oic  : double[len] information criterion of each recorded sample.
 *
 * nvars caps the number of selected predictors (proposals that would exceed it
 * are rejected); pass p1 + p2 to disable the cap.
 *
 * family codes: 0 = gaussian (OLS), 1 = binomial, 2 = poisson (both IRLS).
 * info codes:   0 = AIC, 1 = BIC, 2 = AICc, 3 = exBIC.
 * Returns 0 on success, 1 on allocation failure.
 */
int rungibbs(const double *, const double *, const double *, int, int, int, const int *, int, int, double, double, int, int, int, int, rngt *, int *, double *, double *, gbwst *);

/*
 * The Cox Metropolis-within-Gibbs sampler (cox.c).  The Cox parallel of
 * rungibbs: a random-scan walk over the inclusion vector that favours
 * low-information-criterion Cox models.  It differs from the GLM sampler in two
 * ways: there is NO intercept (the design is X itself, no leading ones column),
 * and the response is a (time, status) pair fitted by the Efron partial
 * likelihood in coxfit().
 *
 * X is n x (p1+p2), columns = [x1 (p1, toggleable) | x2 (p2, always in)].
 * time/status are the survival response (status: 1 = event, 0 = censored);
 * pw are the prior case weights (length n; all ones for the unweighted fit).
 * rng == NULL uses R's unif_rand (main thread only); otherwise the supplied
 * thread-safe generator is used.  ws is a caller-provided workspace
 * (NULL to self-allocate; only safe on the main thread).
 *
 * Any of the following outputs may be filled (pass NULL to skip):
 *   omat : int[len * (1 + p1+p2)] column-major indicator matrix
 *          ([leading 1 | x1 | x2]).  The leading column is a constant 1 so
 *          the matrix layout matches the GLM sampler's (the R summary code
 *          ignores it for Cox, which has no intercept coefficient).
 *   ofrq : double[p1] marginal inclusion probability of the x1 block.
 *   oic  : double[len] information criterion of each recorded sample.
 *
 * nvars caps the number of selected predictors (proposals exceeding it are
 * rejected); pass p1 + p2 to disable the cap.
 *
 * info codes: 0 = AIC, 1 = BIC, 2 = AICc, 3 = exBIC.  p0 is the candidate-pool
 * size used by exBIC.  Returns 0 on success, 1 on allocation failure.
 */
int runcoxgb(const double *, const int *, const double *, const double *, int, int, int, const int *, int, int, double, double, int, int, int, rngt *, int *, double *, double *, cxwst *);

/*
 * The linear-mixed-model fixed-effect Metropolis-within-Gibbs sampler
 * (lme.c).  With the random structure fixed, the marginal model is
 * y ~ N(X beta, V) with V known; whitening by V = L L' (ystar = L^-1 y,
 * Xstar = L^-1 X, istar = L^-1 1) reduces each candidate fit to ordinary least
 * squares on the whitened data, so runlmegb is the gaussian Gram sampler
 * (it reuses rsschol) but the intercept is the PROVIDED whitened column
 * `istar` (always included) rather than a constant ones vector, and a constant
 * log|V0| is added to the criterion.
 *
 * Xstar is n x (p1+p2) whitened predictors = [x1 (p1, toggleable) | x2 (p2,
 * always in)].  istar is the whitened intercept column (length n; always in).
 * ystar is the whitened response.  rng == NULL uses R's unif_rand (main thread).
 * ws is a caller-provided workspace (NULL to self-allocate; main thread only).
 *
 * Outputs (pass NULL to skip):
 *   omat : int[len * (1 + p1+p2)] column-major indicator matrix
 *          ([intercept | x1 | x2]); the leading column is recorded as 1.
 *   ofrq : double[p1] marginal inclusion probability of the x1 block.
 *   oic  : double[len] information criterion of each recorded sample.
 *
 * ldv0 = log|V0| (constant across models; added to the criterion for
 * reporting).  nvars caps the number of selected predictors.  info codes:
 * 0 = AIC, 1 = BIC, 2 = AICc, 3 = exBIC.  Returns 0 on success, 1 on allocation
 * failure.
 */
int runlmegb(const double *, const double *, const double *, int, int, int, const int *, int, int, double, double, int, int, double, int, rngt *, int *, double *, double *, lmewst *);

/* ============================================================================
 * Shared search scratch: types, helpers, and the per-search workspace used by
 * all three *ibgs* orchestrators (implemented in glm.c).
 * ========================================================================== */

/* (frequency, column-index) pair, sorted by frequency descending to pick the
 * most-visited predictors after a screening pass. */
typedef struct { double v; int idx; } fit;

/* qsort comparator: fit by v DESCENDING, ties broken by smaller idx first. */
int ficmpdsc(const void *, const void *);

/* qsort comparator: plain int ascending (used to sort selected column ids). */
int intcmp(const void *, const void *);

/* Lower-bound search in the ascending tail active[1..nact-1] of an active-column
 * list: returns the first index whose stored value is >= val, or nact if none.
 * When val is present this is exactly its index.  Used by the glm and lme
 * samplers to splice one coordinate into/out of the sorted list per proposal
 * instead of rebuilding the whole list from the inclusion vector. */
int actfind(const int *, int, int);

/* Gather the m columns cols[0..m-1] of the n-row column-major matrix src into
 * the caller-provided buffer dst (n x m, column-major). */
void gathcols(const double *, int, const int *, int, double *);

/* ============================================================================
 * Model-averaging summary of a finished sampler run (glm.c), called by the .Call
 * wrappers so the per-generation indicator matrix never crosses to R.  summnm /
 * summtab are family-independent; the three *summ refit a representative of each
 * of the best nm models with the family's coef kernel.
 * ========================================================================== */

/* Number of models to keep = min(n_req, #distinct criterion values in oic).
 * Lets the wrapper allocate the summary outputs at the exact width.  Returns -1
 * on allocation failure. */
int summnm(const double *, int, int);

/* Tabulate oic[0..lenf-1] into ascending distinct groups: writes the first
 * nm_in groups' distinct value (micic), count (cnt) and a representative sample
 * index (rep, = R's order()/cumsum() pick), and *nm = number written.  Returns
 * 0, or 1 on allocation failure. */
int summtab(const double *, int, int, double *, int *, int *, int *);

/* Per-family summarizers: refit the best nm models from the recorded run and
 * write coef (nr x nm_in, full length: glm/lme nr = p+1 with intercept row 0,
 * cox nr = p), model.ic (micout) and model.freq (frqout), setting *nm.  omat is
 * the column-major lenf x (1+ps) indicator matrix; xs[c] is the 0-based original
 * column of omat predictor column c+1.  Each returns 0, or 1 on allocation
 * failure. */
int glmsumm(const double *, const double *, const double *, int, int, const int *, int, const int *, const double *, int, int, int, double *, double *, double *, int *);
int coxsumm(const double *, const int *, const double *, const double *, int, int, const int *, int, const int *, const double *, int, int, double *, double *, double *, int *);
int lmesumm(const double *, const double *, const double *, int, int, const int *, int, const int *, const double *, int, int, double *, double *, double *, int *);

/*
 * Per-search workspace: every scratch buffer one *ibgs* search call needs,
 * allocated once on the main thread and reused across all refinement iterations
 * and the final long run, so the search body performs no per-iteration
 * allocation.  Sized for up to p predictors over n rows.  The output buffers
 * (mbuf/sicbuf/vpbuf/selbuf) are NOT here: they are returned to and freed by the
 * caller.
 *
 * Allocated with malloc/calloc (not R_Calloc): the search functions report
 * out-of-memory through their return code, whereas R_Calloc longjmps.
 */
typedef struct {
    int       capp;     /* predictor capacity it was sized for */
    int       n;        /* rows */
    /* per-call scratch (size p) */
    int      *inS2;     /* membership flag of the important set (zero-init) */
    int      *S2;       /* important-set column ids */
    int      *S1;       /* candidate (non-important) column ids */
    int      *assign;   /* per-candidate block label */
    double   *vfreq;    /* per-column screening frequency */
    /* per-iteration scratch, worst-case sized, reused every iteration */
    uint64_t *seeds;    /* p     (h  <= nS1 <= p) per-block RNG seeds */
    fit      *arr;      /* p     (nS1 <= p) freq/index pairs to rank */
    int      *xs;       /* p     (ps  <= p) selected column ids */
    int      *s0;       /* p     (ps  <= p) start-model indicator */
    double   *fr;       /* p     (ps  <= p) per-column frequencies */
    double   *Xs;       /* n*p   (ps cols) gathered selected design */
} srwst;

/*
 * Allocate / free a search workspace for up to p predictors over n rows.
 * srwsallc returns 0 on success and 1 on any allocation failure (freeing
 * whatever it had taken); inS2 is zero-initialised.
 */
int  srwsallc(srwst *, int, int);
void srwsfree(srwst *);

/* ============================================================================
 * Iterated-block-Gibbs orchestration (one search + one standalone sampler per
 * family).  Each *ibgs* search performs all refinement iterations, the OpenMP
 * block screening, and the final long run.
 * ========================================================================== */

/*
 * Gaussian / binomial / poisson iterated block Gibbs search (glm.c), split into a
 * select phase and a fill phase so the caller can allocate the R outputs once the
 * final width ps is known.
 *
 * ibgssel -- refinement iterations + final screening; determines the converged
 * candidate set and reports it to the caller:
 *   xs    : OUTPUT int[p] -- the chosen 0-based column indices (first *ps used).
 *   *ps   : OUTPUT number of selected columns (always <= p).
 *   *lenf : OUTPUT recorded sweeps of the final long run (= 4*len).
 * ibgsrun -- the final long Gibbs run over the xs columns, writing directly into
 * caller-provided buffers (no allocation/copy of the big outputs):
 *   omat  : OUTPUT int[lenf * (1 + ps)]  indicator matrix (column-major).
 *   oic   : OUTPUT double[lenf]          per-sample criterion.
 *   vprob : OUTPUT double[p]             marginal inclusion prob (zeroed + scattered).
 *   sel   : OUTPUT int[ps]               1-based original column indices.
 * Each returns 0 on success, 1 on failure.
 */
int ibgssel(const double *, const double *, const double *, int, int, int, int, int, double, int, int, double, double, int, int, int, int *, int *, int *);
int ibgsrun(const double *, const double *, const double *, int, int, const int *, int, int, int, double, double, int, int, int *, double *, double *, int *);

/*
 * Standalone (non-block) restricted Gibbs sampler over all p predictors, model
 * size capped at nvars.  Writes into caller-provided buffers (sizes known up
 * front):
 *   mbuf   : int   [len * (1 + p)]
 *   sicbuf : double[len]
 *   vpbuf  : double[p]
 * Returns 0 on success, 1 on failure.
 */
int gibbssam(const double *, const double *, const double *, int, int, int, int, int, double, double, int, int, int *, double *, double *);

/*
 * Single-model GLM coefficient refit behind the glm_coef() .Call wrapper.  Builds
 * the full design D = [1 | X] (intercept first) and fits all q+1 columns: the
 * gaussian family by weighted OLS (normal equations solved with cholsolv), the
 * binomial/poisson families by IRLS (glmirls).
 *   y      : the response (length n).
 *   X      : the n x q predictor design (column-major; NO intercept column).
 *   pw     : prior case weights (length n; all ones for the unweighted fit).
 *   n, q   : number of observations and predictors.
 *   family : FAM_GAUSSIAN (0), FAM_BINOMIAL (1) or FAM_POISSON (2).
 *   bout   : OUTPUT, length q+1 -- the fitted [intercept, beta_1..beta_q]; set to
 *            all zeros on a singular/failed or non-finite fit.
 * Returns nothing (writes into bout).  Allocates the design and OLS/IRLS scratch
 * with R_Calloc/R_Free (main thread only); does no SEXP handling.
 */
void glmcoef(const double *, const double *, const double *, int, int, int, double *);

/*
 * Cox iterated block Gibbs search (cox.c), split into select + fill phases (see
 * ibgssel/ibgsrun for the rationale).
 *
 * coxibgsel -- refinement + final screening; reports the candidate set:
 *   xs    : OUTPUT int[p] chosen 0-based columns (first *ps used).
 *   *ps   : OUTPUT number of selected columns (<= p); *lenf : sweeps (= 4*len).
 * coxibgrun -- final long Cox Gibbs run writing directly into caller buffers:
 *   omat  : OUTPUT int[lenf * (1 + ps)] indicator matrix (column-major).
 *   oic   : OUTPUT double[lenf] per-sample criterion.
 *   vprob : OUTPUT double[p] marginal inclusion prob (zeroed + scattered).
 *   sel   : OUTPUT int[ps] 1-based original column indices.
 * Each returns 0 on success, 1 on failure.
 */
int coxibgsel(const double *, const int *, const double *, const double *, int, int, int, int, int, double, int, int, double, double, int, int, int *, int *, int *);
int coxibgrun(const double *, const int *, const double *, const double *, int, int, const int *, int, int, int, double, double, int, int *, double *, double *, int *);

/*
 * Standalone (non-block) restricted Cox Gibbs sampler over all p predictors,
 * model size capped at nvars.  Writes into caller-provided buffers:
 *   mbuf   : int   [len * (1 + p)]
 *   sicbuf : double[len]
 *   vpbuf  : double[p]
 * Returns 0 on success, 1 on failure.
 */
int coxgbsam(const double *, const int *, const double *, const double *, int, int, int, int, int, double, double, int, int *, double *, double *);

/*
 * Single-model Cox coefficient refit behind the cox_coef() .Call wrapper.  Fits
 * the full q-column design X (no intercept) by Newton-Raphson on the Efron
 * partial likelihood (coxfit) and writes the result into caller-owned buffers.
 *   time   : event/censoring time per observation (length n).
 *   status : 1 = event, 0 = censored (length n).
 *   pw     : prior case weights (length n).
 *   X      : n x q design (column-major); no intercept column.
 *   n, q   : number of observations and predictors.
 *   bout   : OUTPUT, length q -- fitted coefficients; all zeros on a singular/
 *            failed or non-finite fit.
 *   m2pll  : OUTPUT -- the model's -2*partial-logLik (0 on a failed fit).
 * Returns nothing (writes into bout/m2pll).  Builds the descending-time order
 * once (tit/ticmpdsc) and allocates its Newton scratch with R_Calloc/R_Free (main
 * thread only); does no SEXP handling.
 */
void coxcoef(const double *, const int *, const double *, const double *, int, int, double *, double *);

/*
 * lme iterated block Gibbs search (lme.c), split into select + fill phases (see
 * ibgssel/ibgsrun).  The data are whitened in R (ystar, Xstar, istar; V = L L'),
 * so the fits are ordinary least squares on the whitened design; ldv0 = log|V0|.
 *
 * lmeibgsel -- refinement + final screening; reports the candidate set:
 *   xs    : OUTPUT int[p] chosen 0-based columns (first *ps used).
 *   *ps   : OUTPUT number of selected columns (<= p); *lenf : sweeps (= 4*len).
 * lmeibgrun -- final long whitened-OLS Gibbs run writing directly into caller
 * buffers:
 *   omat  : OUTPUT int[lenf * (1 + ps)] indicator matrix (column-major).
 *   oic   : OUTPUT double[lenf] per-sample criterion.
 *   vprob : OUTPUT double[p] marginal inclusion prob (zeroed + scattered).
 *   sel   : OUTPUT int[ps] 1-based original column indices.
 * Each returns 0 on success, 1 on failure.
 */
int lmeibgsel(const double *, const double *, const double *, int, int, int, int, int, double, int, int, double, double, int, double, int, int *, int *, int *);
int lmeibgrun(const double *, const double *, const double *, int, int, const int *, int, int, int, double, double, int, double, int *, double *, double *, int *);

/*
 * Standalone (non-block) lme sampler over all p whitened predictors, capped at
 * nvars.  Writes into caller-provided buffers (mbuf: int[len*(1+p)],
 * sicbuf: double[len], vpbuf: double[p]).  Returns 0 on success, 1 on failure.
 */
int lmegbsam(const double *, const double *, const double *, int, int, int, int, int, double, double, int, double, int *, double *, double *);

/*
 * Single-model whitened-OLS coefficient refit behind the lme_coef() .Call
 * wrapper.  Solves the normal equations (D'D) beta = D'ystar by Cholesky
 * (cholsolv) for the whitened design.
 *   ystar  : the whitened response L^-1 y (length n).
 *   D      : the whitened design [istar | active Xstar columns] (n x q,
 *            column-major); istar = L^-1 1 is the whitened intercept column.
 *   n, q   : number of observations and design columns.
 *   bout   : OUTPUT, length q -- the fitted [intercept, beta_1..beta_{q-1}] in
 *            ORIGINAL space (whitening leaves the GLS fixed effects unchanged);
 *            all zeros on a singular or non-finite fit.
 * Returns nothing (writes into bout).  Allocates its Gram/solve scratch with
 * R_Calloc/R_Free (main thread only); does no SEXP handling.
 */
void lmecoef(const double *, const double *, int, int, double *);

/* ============================================================================
 * Convergence diagnostics of the recorded information-criterion sequence
 * (ic.trace), implemented in diag.c.  Each routine works on one plain double
 * array; the .Call shim ibgs_diag (R_export.c) marshals the results to R.  The
 * formulas mirror coda and R's stats::ar.yw, so the package reports the standard
 * diagnostics without depending on coda.
 * ========================================================================== */

/* Lagged autocorrelations out[0..lag_max] (out[0] = 1); like stats::acf. */
void acf_vec(const double *, int, int, double *);

/* Spectral density at zero frequency via an AR(aic) fit; coda::spectrum0.ar.
 * *order (if non-NULL) receives the selected AR order. */
double spectrum0_ar(const double *, int, int *);

/* Effective sample size n*var(x)/spectrum0.ar(x); coda::effectiveSize. */
double ess_val(const double *, int);

/* Geweke z comparing the first frac1 and last frac2 of the chain;
 * coda::geweke.diag. */
double geweke_z(const double *, int, double, double);

/* Split-chain (m segments) univariate Gelman-Rubin PSRF point estimate (*psrf)
 * and 97.5% upper limit (*upper); coda::gelman.diag. */
void gelman1d(const double *, int, int, double *, double *);

/* Evolving shrink factor for gelman.plot: gelman1d over growing prefixes.
 * Writes *nb usable breakpoints into iters/med/upper (caller-sized to nbin). */
void gelman_shrink(const double *, int, int, int, double *, double *, double *, int *);

#endif /* IBGS_IBGS_H */
