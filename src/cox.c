/*
 * cox.c -- the Cox proportional-hazards family: the Efron partial-likelihood
 * fit (Newton-Raphson), the Metropolis-within-Gibbs sampler step, and the
 * iterated-block-Gibbs orchestration.
 *
 * Consolidates the former cox_fit.c, cox_step.c, and cox_ibgs.c.  The original
 * per-module comment blocks below act as section banners.
 */
#include "ibgs.h"

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#ifdef _OPENMP
#include <omp.h>
#endif

/*
 * cox_fit.c -- Cox proportional-hazards fit by Newton-Raphson on the Efron
 * partial likelihood, and the Cox information criteria.
 *
 * BACKGROUND: the Cox partial likelihood
 * --------------------------------------
 * The Cox model leaves the baseline hazard unspecified and estimates the
 * coefficients beta of the linear predictor eta_i = x_i' beta (NO intercept --
 * it cancels in the ratio below) by maximising the partial likelihood.  For data
 * with event/censoring times t_i and event indicator status_i, the risk set at a
 * time t is R(t) = { j : t_j >= t } (everyone still under observation).  With
 * Breslow's handling of ties the log partial likelihood is
 *
 *    l(beta) = sum_{i: event} w_i [ eta_i - log( sum_{j in R(t_i)} w_j e^{eta_j} ) ].
 *
 * Efron's approximation (the default in survival::coxph, and what we use) refines
 * the denominator when several events share the same time.  For a group D of d
 * tied events at one time, write S = sum over the whole risk set of w e^{eta} and
 * S_D = the same sum over just the d tied events; Efron replaces the single
 * log(S) by
 *
 *    sum_{l=0}^{d-1} (wbar) * log( S - (l/d) S_D ),   wbar = (sum_{i in D} w_i)/d,
 *
 * i.e. it removes the tied events from the risk pool in d equal fractional steps.
 * For the unweighted case wbar = 1 and this is the textbook Efron formula.
 *
 * FITTING: Newton-Raphson / Fisher scoring
 * ----------------------------------------
 * l(beta) is concave, so we maximise it by Newton's method: at each iteration
 * form the gradient U = dl/dbeta and the observed information I = -d^2 l/dbeta^2
 * (positive definite), solve I delta = U by Cholesky, and update beta += delta.
 * U and I are assembled in a single sweep of the observations in DESCENDING time
 * order, accumulating the running risk-set moments
 *    S   = sum_{R} w e^{eta},  Sx = sum_{R} w e^{eta} x,  Sxx = sum_{R} w e^{eta} x x',
 * so each iteration costs O(n q^2).  Warm-starting beta from a neighbouring
 * model's fit (the sampler proposes one-coordinate changes) makes most fits
 * converge in two or three steps.
 *
 * The returned -2*partial-logLik is on the same scale as survival::coxph's
 * (-2*fit$loglik[2]) so the criteria built from it reproduce R's AIC()/BIC().
 */


#define COX_MAXIT 50      /* hard cap on Newton iterations                      */
#define COX_TOL   1e-9    /* relative log-likelihood change treated as converged */
#define ETA_CLAMP 30.0    /* bound on eta so e^{eta} cannot overflow            */

/*
 * Solve the symmetric positive-definite system A x = b by Cholesky (A is q x q
 * row-major; only the lower triangle is read and is overwritten by its factor).
 * Returns 1 if a pivot is non-positive -- the information matrix is singular,
 * meaning the model's columns are collinear -- so the caller can reject the
 * model.  Self-contained copy (Cox does not share glm.c's static solver).
 */
static int coxchols(double *A, const double *b, double *x, int q)
{
    int r, s, m;
    for (r = 0; r < q; r++) {
        for (s = 0; s <= r; s++) {
            double sum = A[r * q + s];
            for (m = 0; m < s; m++) sum -= A[r * q + m] * A[s * q + m];
            if (r == s) {
                if (sum <= 1e-12) return 1;          /* not PD -> singular model */
                A[r * q + r] = sqrt(sum);
            } else {
                A[r * q + s] = sum / A[s * q + s];
            }
        }
    }
    for (r = 0; r < q; r++) {                 /* forward solve L u = b */
        double sum = b[r];
        for (m = 0; m < r; m++) sum -= A[r * q + m] * x[m];
        x[r] = sum / A[r * q + r];
    }
    for (r = q - 1; r >= 0; r--) {            /* back solve L' x = u */
        double sum = x[r];
        for (m = r + 1; m < q; m++) sum -= A[m * q + r] * x[m];
        x[r] = sum / A[r * q + r];
    }
    return 0;
}

double coxicval(double m2pll, int npred, int d, int info, double gamma, int p0)
{
    /* Cox has no intercept and no dispersion, so the parameter count is npred.
     * The effective sample size is the number of events d (BIC/AICc/exBIC). */
    switch (info) {
    case 0: return m2pll + 2.0 * npred;                                        /* AIC   */
    case 1: return m2pll + log((double) d) * npred;                            /* BIC   */
    case 2: return m2pll + 2.0 * npred + 2.0 * npred * (npred + 1.0) / ((double) d - npred - 1.0); /* AICc */
    case 3: return m2pll + log((double) d) * npred + 2.0 * gamma * npred * log((double) p0); /* exBIC */
    default: return m2pll + 2.0 * npred;
    }
}

int coxfit(const double *time, const int *status, const double *pw, const double *Dfull, const int *active, int n, int q, const int *order, double *wq, double *wn, double *m2pll, const double *b0, double *bout)
{
    /* carve the q-sized scratch out of wq (needs >= 3*q*q + 7*q doubles) */
    double *Imat  = wq;                 /* q x q observed information      */
    double *Sxx   = Imat + (size_t) q * q;  /* q x q running risk-set 2nd mom. */
    double *SDxx  = Sxx  + (size_t) q * q;  /* q x q tied-death 2nd moment     */
    double *U     = SDxx + (size_t) q * q;  /* q   gradient                    */
    double *Sx    = U    + q;               /* q   running risk-set 1st moment */
    double *SDx   = Sx   + q;               /* q   tied-death 1st moment        */
    double *dnum  = SDx  + q;               /* q   tied-death numerator sum w*x */
    double *numx  = dnum + q;               /* q   per-step numerator (temp)    */
    double *delta = numx + q;               /* q   Newton step (solution)       */
    double *beta  = delta + q;              /* q   current coefficients         */

    double *eta = wn;                   /* n   linear predictor             */
    double *r   = wn + n;               /* n   e^{eta} (clamped)            */

    int it, a, b, i, idx;
    int qq = q * q;

    #define DCOL(c) (Dfull + (size_t) active[c] * n)

    for (a = 0; a < q; a++) beta[a] = (b0 ? b0[a] : 0.0);

    double loglik = 0.0, prev = 0.0;
    int conv = 0;

    for (it = 0; it < COX_MAXIT; it++) {
        /* linear predictor and its exponential for every observation */
        for (i = 0; i < n; i++) eta[i] = 0.0;
        for (a = 0; a < q; a++) {
            const double *Da = DCOL(a); double ba = beta[a];
            for (i = 0; i < n; i++) eta[i] += Da[i] * ba;
        }
        for (i = 0; i < n; i++) {
            double e = eta[i];
            if (e >  ETA_CLAMP) e =  ETA_CLAMP;
            if (e < -ETA_CLAMP) e = -ETA_CLAMP;
            eta[i] = e;
            r[i]   = exp(e);
        }

        /* zero the accumulators that persist across the whole sweep */
        loglik = 0.0;
        for (a = 0; a < q; a++) { U[a] = 0.0; Sx[a] = 0.0; }
        for (a = 0; a < qq; a++) { Imat[a] = 0.0; Sxx[a] = 0.0; }
        double S = 0.0;

        /* tied-death accumulators (reset after each tie group is processed) */
        double SD = 0.0, dw = 0.0, ldeath = 0.0;
        int dcount = 0;
        for (a = 0; a < q; a++) { SDx[a] = 0.0; dnum[a] = 0.0; }
        for (a = 0; a < qq; a++) SDxx[a] = 0.0;

        /* sweep observations in DESCENDING time order; order[] is sorted so that
         * equal times are consecutive.  When the time changes we have a complete
         * risk set R(t) for the just-finished group, so we process its events. */
        for (idx = 0; idx < n; idx++) {
            int j  = order[idx];
            double wr = pw[j] * r[j];

            /* add j to the running risk-set moments (R(t) grows as time falls) */
            S += wr;
            for (a = 0; a < q; a++) {
                double xa = DCOL(a)[j];
                Sx[a] += wr * xa;
                double *Sxxa = Sxx + (size_t) a * q;
                for (b = 0; b < q; b++) Sxxa[b] += wr * xa * DCOL(b)[j];
            }
            if (status[j]) {                 /* accumulate the tied-death sums */
                dcount++; dw += pw[j]; ldeath += pw[j] * eta[j];
                SD += wr;
                for (a = 0; a < q; a++) {
                    double xa = DCOL(a)[j];
                    dnum[a]  += pw[j] * xa;
                    SDx[a]   += wr * xa;
                    double *SDxxa = SDxx + (size_t) a * q;
                    for (b = 0; b < q; b++) SDxxa[b] += wr * xa * DCOL(b)[j];
                }
            }

            /* is idx the last member of this time group?  (end, or next time
             * differs).  order is descending, so a new index with a smaller time
             * closes the group. */
            int last = (idx == n - 1) || (time[order[idx + 1]] != time[j]);
            if (last && dcount > 0) {
                /* Efron tie contribution to loglik, gradient U and information I */
                double phi = dw / (double) dcount;
                loglik += ldeath;
                for (a = 0; a < q; a++) U[a] += dnum[a];
                for (int l = 0; l < dcount; l++) {
                    double frac  = (double) l / (double) dcount;
                    double denom = S - frac * SD;
                    if (denom < 1e-300) denom = 1e-300;
                    loglik -= phi * log(denom);
                    for (a = 0; a < q; a++) numx[a] = Sx[a] - frac * SDx[a];
                    double inv = 1.0 / denom;
                    for (a = 0; a < q; a++) U[a] -= phi * numx[a] * inv;
                    for (a = 0; a < q; a++) {
                        double *Ia   = Imat + (size_t) a * q;
                        double *Sxxa = Sxx  + (size_t) a * q;
                        double *SDxxa= SDxx + (size_t) a * q;
                        double na = numx[a];
                        for (b = 0; b < q; b++) {
                            double mxx = Sxxa[b] - frac * SDxxa[b];
                            Ia[b] += phi * (mxx * inv - na * numx[b] * inv * inv);
                        }
                    }
                }
                /* reset the death accumulators for the next tie group */
                SD = 0.0; dw = 0.0; ldeath = 0.0; dcount = 0;
                for (a = 0; a < q; a++) { SDx[a] = 0.0; dnum[a] = 0.0; }
                for (a = 0; a < qq; a++) SDxx[a] = 0.0;
            } else if (last) {
                /* group with no events: nothing to score, just clear death sums
                 * (they are zero already, but keep the invariant explicit) */
                SD = 0.0; dw = 0.0; ldeath = 0.0; dcount = 0;
                for (a = 0; a < q; a++) { SDx[a] = 0.0; dnum[a] = 0.0; }
            }
        }

        /* convergence on the partial log-likelihood */
        if (it > 0 && fabs(loglik - prev) < COX_TOL * (fabs(loglik) + COX_TOL)) {
            conv = 1;
            break;
        }
        prev = loglik;

        /* Newton step: solve I delta = U, then beta += delta */
        if (coxchols(Imat, U, delta, q)) { return 1; }   /* singular */
        for (a = 0; a < q; a++) beta[a] += delta[a];
    }

    (void) conv;   /* best-effort: even a MAXIT exit returns a usable fit */
    #undef DCOL

    *m2pll = -2.0 * loglik;
    if (bout) for (a = 0; a < q; a++) bout[a] = beta[a];
    return 0;
}
/*
 * cox_step.c -- Metropolis-within-Gibbs sampler over Cox model indicators.
 *
 * Mirrors gibbs_step.c (the GLM sampler) but for the Cox proportional-hazards
 * model.  A model is a binary inclusion vector over the p1 toggleable predictors
 * (the trailing p2 columns are always in); runcoxgb() performs a random-scan
 * Metropolis-within-Gibbs walk over {0,1}^p1, accepting a one-coordinate flip
 * with probability A = min(1, exp{k * (IC_current - IC_proposed)}).  Lower IC is
 * better, so improving flips are always accepted and worsening ones accepted with
 * a probability that decays in the IC increase; k tunes the greediness.  Visit
 * frequencies estimate marginal inclusion probabilities.
 *
 * Each candidate model's information criterion is the Efron partial-likelihood
 * fit from coxfit() plus the Cox penalty from coxicval().  Two things keep
 * the per-model cost down, exactly as in the GLM path:
 *   (1) the observations are sorted by time ONCE per run (the risk sets are the
 *       same for every candidate model), so each fit is a single linear sweep;
 *   (2) each fit warm-starts from the current model's coefficients -- a proposal
 *       differs from the current model in only one indicator -- so Newton's
 *       method converges in a couple of steps.
 */


/* rng == NULL -> R's unif_rand (main thread); else the thread-safe generator. */
#define UNIF(rng) ((rng) ? rngunif(rng) : unif_rand())

/* Sort (time, index) pairs by time DESCENDING; stateless comparator so the sort
 * is thread-safe (no shared/global time pointer).  Ties keep equal times
 * contiguous, which is all coxfit needs to detect tie groups.  tit is declared
 * in ibgs.h (it is a field of the reusable workspace). */
static int ticmpdsc(const void *a, const void *b)
{
    const tit *x = (const tit *) a, *y = (const tit *) b;
    if (x->t < y->t) return  1;
    if (x->t > y->t) return -1;
    return (x->idx > y->idx) - (x->idx < y->idx);
}

/* Fit the q-predictor Cox model in `active` and return its information criterion;
 * sets *ok = 0 if the fit fails.  On success the fitted coefficients are left in
 * bprop for a possible warm-start commit. */
static double coxmodic(const double *time, const int *status, const double *pw, const double *X, const int *active, int n, int q, const int *order, double *wq, double *wn, int d, int info, double gamma, int p0, const double *b0, double *bprop, int *ok)
{
    double m2pll;
    if (coxfit(time, status, pw, X, active, n, q, order, wq, wn,
               &m2pll, b0, bprop)) { *ok = 0; return 0.0; }
    *ok = 1;
    return coxicval(m2pll, q, d, info, gamma, p0);
}

/* Allocate the Cox sampler workspace for up to capt predictors over n rows.
 * Uses R_Calloc (main thread only); on out of memory R_Calloc raises an R error
 * rather than returning. */
int cxwsallc(cxwst *ws, int capt, int n)
{
    if (capt < 1) capt = 1;
    ws->capt = capt;
    ws->n    = n;

    ws->inc    = R_Calloc((size_t) capt, int);
    ws->active = R_Calloc((size_t) capt, int);
    ws->ord    = R_Calloc((size_t) capt, int);
    ws->order  = R_Calloc((size_t) (n > 0 ? n : 1), int);
    ws->bcols  = R_Calloc((size_t) capt, int);
    ws->s0     = R_Calloc((size_t) capt, int);
    ws->wq     = R_Calloc((size_t) 3 * capt * capt + 7 * capt, double);
    ws->wn     = R_Calloc((size_t) 2 * (n > 0 ? n : 1), double);
    ws->bfull  = R_Calloc((size_t) capt, double);
    ws->b0     = R_Calloc((size_t) capt, double);
    ws->bprop  = R_Calloc((size_t) capt, double);
    ws->fr     = R_Calloc((size_t) capt, double);
    ws->Xb     = R_Calloc((size_t) (n > 0 ? n : 1) * capt, double);
    ws->pairs  = R_Calloc((size_t) (n > 0 ? n : 1), tit);
    return 0;
}

void cxwsfree(cxwst *ws)
{
    R_Free(ws->inc);
    R_Free(ws->active);
    R_Free(ws->ord);
    R_Free(ws->order);
    R_Free(ws->bcols);
    R_Free(ws->s0);
    R_Free(ws->wq);
    R_Free(ws->wn);
    R_Free(ws->bfull);
    R_Free(ws->b0);
    R_Free(ws->bprop);
    R_Free(ws->fr);
    R_Free(ws->Xb);
    R_Free(ws->pairs);
}

int runcoxgb(const double *time, const int *status, const double *X, const double *pw, int n, int p1, int p2, const int *smod, int perm, int len, double k, double gamma, int p0, int info, int nvars, rngt *rng, int *omat, double *ofrq, double *oic, cxwst *wsi)
{
    int ptot = p1 + p2;
    int a, i;

    /* event count d = number of uncensored observations (effective sample size
     * for the Cox criteria), and the descending-time order of the observations,
     * both computed ONCE and reused for every candidate fit. */
    int d = 0;
    for (i = 0; i < n; i++) d += (status[i] != 0);

    /* Use the caller's workspace, or allocate a private one (main thread only)
     * when wsi is NULL; pointing the original locals at the workspace fields
     * keeps the sampler body below unchanged. */
    cxwst  wsl;
    cxwst *ws    = wsi;
    int    owned = 0;
    if (!ws) {
        cxwsallc(&wsl, ptot, n);
        ws    = &wsl;
        owned = 1;
    }
    int    *inc   = ws->inc;
    int    *active = ws->active;
    int    *ord    = ws->ord;
    int    *order  = ws->order;
    double *wq     = ws->wq;
    double *wn     = ws->wn;
    double *bfull  = ws->bfull;
    double *b0     = ws->b0;
    double *bprop  = ws->bprop;
    tit    *pairs  = ws->pairs;

    /* warm-start coefficient state is reused across blocks: clear it (a fresh
     * calloc gave the same zero start). */
    for (a = 0; a < ptot; a++) bfull[a] = 0.0;

    for (i = 0; i < n; i++) { pairs[i].t = time[i]; pairs[i].idx = i; }
    qsort(pairs, n, sizeof(tit), ticmpdsc);
    for (i = 0; i < n; i++) order[i] = pairs[i].idx;

    /* Translate the inclusion vector into the active-column list (column indices,
     * no intercept); qout = number of included predictors.  Also fill b0 with
     * the current model's coefficient for each active column (warm start). */
    #define BUILD_ACTIVE(qout)                                  \
        do {                                                     \
            int _q = 0;                                          \
            for (int _j = 0; _j < ptot; _j++)                    \
                if (inc[_j]) { active[_q] = _j; b0[_q] = bfull[_j]; _q++; } \
            (qout) = _q;                                        \
        } while (0)

    /* On accepting a model, store its fitted coefficients (bprop) as the new
     * current state, indexed by design column, so the NEXT proposal can warm-start
     * from them. */
    #define COMMIT_BETA(qc)                                       \
        do {                                                      \
            for (int _a = 0; _a < ptot; _a++) bfull[_a] = 0.0;  \
            for (int _r = 0; _r < (qc); _r++)                    \
                bfull[active[_r]] = bprop[_r];                   \
        } while (0)

    /* ----- initial state ----- */
    int nsel = 0;
    for (a = 0; a < p1; a++)    { inc[a] = smod[a] ? 1 : 0; nsel += inc[a]; }
    for (a = p1; a < ptot; a++) { inc[a] = 1; nsel += 1; }

    int q, ok = 0;
    BUILD_ACTIVE(q);
    double curic = R_PosInf;
    if (q > 0)
        curic = coxmodic(time, status, pw, X, active, n, q, order,
                          wq, wn, d, info, gamma, p0, b0, bprop, &ok);
    if (!ok) curic = R_PosInf; else COMMIT_BETA(q);

    if (ofrq)
        for (a = 0; a < p1; a++) ofrq[a] = 0.0;

    /* 2*len sweeps: first len are burn-in (discarded), second len recorded.
     * One sweep = p1 single-coordinate flip attempts. */
    int nsweep = 2 * len;
    for (int sw = 0; sw < nsweep; sw++) {
        /* perm = TRUE: visit each of the p1 toggleable coordinates exactly once
         * per sweep, in a fresh random order (Fisher-Yates), i.e. without
         * replacement.  perm = FALSE: the fixed 0..p1-1 systematic sweep. */
        if (perm) {
            for (int t = 0; t < p1; t++) ord[t] = t;
            for (int t = p1 - 1; t > 0; t--) {
                int u = (int) (UNIF(rng) * (t + 1));
                if (u > t) u = t;                 /* guard the UNIF==~1 edge */
                int tmp = ord[t]; ord[t] = ord[u]; ord[u] = tmp;
            }
        }
        for (int step = 0; step < p1; step++) {
            int j = perm ? ord[step] : step;

            /* reject moves that would empty the model or exceed the size cap */
            int pnsel = nsel + (inc[j] ? -1 : 1);
            if (pnsel < 1 || pnsel > nvars) continue;

            inc[j] ^= 1;                       /* tentatively flip coordinate j */
            int pq;
            BUILD_ACTIVE(pq);
            double propic = coxmodic(time, status, pw, X, active, n, pq,
                                      order, wq, wn, d, info, gamma,
                                      p0, b0, bprop, &ok);

            int accept = 0;
            if (ok) {
                double A = exp(k * (curic - propic));
                if (A > 1.0) A = 1.0;
                if (UNIF(rng) < A) accept = 1;
                if (accept) {
                    curic = propic;
                    nsel   = pnsel;
                    COMMIT_BETA(pq);
                }
            }
            if (!accept) inc[j] ^= 1;          /* reject: undo the flip */
        }

        /* record the post-burn-in samples */
        if (sw >= len) {
            int row = sw - len;
            if (omat) {
                omat[row] = 1;           /* leading constant column */
                for (a = 0; a < ptot; a++)
                    omat[(a + 1) * len + row] = inc[a];
            }
            if (ofrq)
                for (a = 0; a < p1; a++) ofrq[a] += inc[a];
            if (oic)
                oic[row] = curic;
        }
    }

    if (ofrq)
        for (a = 0; a < p1; a++) ofrq[a] /= (double) len;

    #undef BUILD_ACTIVE
    #undef COMMIT_BETA
    if (owned) cxwsfree(&wsl);
    return 0;
}
/*
 * cox_ibgs.c -- pure-C orchestration of the iterated block Gibbs sampler for the
 * Cox proportional-hazards model.  The Cox parallel of ibgs.c: same
 * screen -> select -> threshold structure, the same reproducible parallel block
 * screening, and the same final long run -- only the within-block/within-model
 * fit differs (Efron partial likelihood via runcoxgb instead of the GLM
 * rungibbs).  See glm.c for the full description of the algorithm.
 *
 * Two Cox-specific differences:
 *   - the survival response is the (time, status) pair, threaded through to every
 *     fit;
 *   - block sizes and the model-size cap are bounded by the number of EVENTS d
 *     (not the sample size n), because for the Cox partial likelihood d is the
 *     binding identifiability limit (a model with more coefficients than events
 *     is not estimable).
 */



/* ------------------------------------------------------------------ */
/* Helpers (Cox-specific block sizing/seeding; the freq/index ranking,  */
/* column gather, and per-search workspace are shared, from ibgs.h).    */
/* ------------------------------------------------------------------ */

/* Number of blocks = ceil(nS1 / block_size), block_size = min(H, d - nS2).
 * Bounded by the event count d (not n) so a block's Cox fit stays estimable. */
static int coxnblks(int nS1, int H, int d, int nS2)
{
    int bs = (H < d - nS2) ? H : (d - nS2);
    if (bs < 1) bs = 1;
    int h = (nS1 + bs - 1) / bs;
    return (h < 1) ? 1 : h;
}

/* Draw, serially from R's RNG, a block label per candidate and one seed per
 * block (fixed before any thread starts -> reproducible parallel screen). */
static void coxdrwbl(int nS1, int h, int *assign, uint64_t *seeds)
{
    for (int i = 0; i < nS1; i++) {
        int a = (int) (unif_rand() * h);
        assign[i] = (a >= h) ? h - 1 : a;
    }
    for (int b = 0; b < h; b++) {
        uint64_t hi = (uint64_t) (unif_rand() * 4294967296.0);
        uint64_t lo = (uint64_t) (unif_rand() * 4294967296.0);
        seeds[b] = (hi << 32) ^ lo ^ (0x9E3779B97F4A7C15ULL * (uint64_t) (b + 1));
    }
}

/*
 * One screening step: split the candidate columns S1 into `h` blocks and run a
 * within-block Cox Gibbs sampler in parallel; the fixed columns S2 are always
 * included.  Writes the marginal inclusion probability of every S1 column into
 * vfreq[] at its original column position.  Returns 0 on success, 1 on failure.
 */
static int coxscrbl(const double *time, const int *status, const double *X, const double *pw, int n, const int *S1, int nS1, const int *S2, int nS2, int h, int perm, int start_full, int len, double k, double gamma, int p0, int info, int nthr, const int *assign, const uint64_t *seeds, double *vfreq)
{
    int *sz  = R_Calloc((size_t) (h > 0 ? h : 1), int);
    int *off = R_Calloc((size_t) (h + 1), int);
    int *pos = R_Calloc((size_t) (nS1 > 0 ? nS1 : 1), int);
    int *cur = R_Calloc((size_t) (h > 0 ? h : 1), int);

    for (int i = 0; i < nS1; i++) sz[assign[i]]++;
    off[0] = 0;
    for (int b = 0; b < h; b++) {
        off[b + 1] = off[b] + sz[b];
        cur[b]     = off[b];
    }
    for (int i = 0; i < nS1; i++) pos[cur[assign[i]]++] = i;

    /* size every per-thread workspace for the largest block (no intercept) */
    int maxpb = 0;
    for (int b = 0; b < h; b++) if (sz[b] > maxpb) maxpb = sz[b];
    int capt = maxpb + nS2;

    /* one reusable workspace per thread, allocated up front on the main thread
     * (R_Calloc is not thread-safe), so the parallel loop allocates nothing */
    int nws = nthr > 0 ? nthr : 1;
#ifndef _OPENMP
    nws = 1;
#endif
    cxwst *wsa = R_Calloc((size_t) nws, cxwst);
    for (int t = 0; t < nws; t++) cxwsallc(&wsa[t], capt, n);

    int fail = 0;
#ifdef _OPENMP
    #pragma omp parallel for num_threads(nthr) schedule(dynamic) shared(fail)
#endif
    for (int b = 0; b < h; b++) {
        int pb = sz[b];
        if (pb <= 0) continue;

#ifdef _OPENMP
        cxwst *ws = &wsa[omp_get_thread_num()];
#else
        cxwst *ws = &wsa[0];
#endif
        int    *bcols = ws->bcols;
        int    *s0    = ws->s0;
        double *fr    = ws->fr;
        double *Xb    = ws->Xb;

        for (int c = 0; c < pb; c++) {
            bcols[c] = S1[pos[off[b] + c]];
            s0[c]    = start_full ? 1 : 0;
        }
        for (int c = 0; c < nS2; c++) bcols[pb + c] = S2[c];
        for (int c = 0; c < pb + nS2; c++)
            memcpy(Xb + (size_t) c * n, X + (size_t) bcols[c] * n, (size_t) n * sizeof(double));

        rngt rng;
        rngseed(&rng, seeds[b]);
        int rc = runcoxgb(time, status, Xb, pw, n, pb, nS2, s0, perm,
                          len, k, gamma, p0, info, pb + nS2, &rng, NULL, fr,
                          NULL, ws);
        if (rc) {
#ifdef _OPENMP
            #pragma omp atomic write
#endif
            fail = 1;
        } else {
            for (int c = 0; c < pb; c++)
                vfreq[S1[pos[off[b] + c]]] = fr[c];
        }
    }

    for (int t = 0; t < nws; t++) cxwsfree(&wsa[t]);
    R_Free(wsa);
    R_Free(sz);
    R_Free(off);
    R_Free(pos);
    R_Free(cur);
    return fail;
}

/* ------------------------------------------------------------------ */
/* Public entry points.                                               */
/* ------------------------------------------------------------------ */

int coxgbsam(const double *time, const int *status, const double *X, const double *pw, int n, int p, int nvars, int perm, int start_full, int len, double k, double gamma, int info, int *mbuf, double *sicbuf, double *vpbuf)
{
    if (nvars < 1) nvars = 1;
    if (nvars > p) nvars = p;

    int *s0 = (int *) malloc((size_t) p * sizeof(int));
    if (!s0) return 1;
    for (int i = 0; i < p; i++) s0[i] = start_full ? ((i < nvars) ? 1 : 0) : 0;

    int fail = runcoxgb(time, status, X, pw, n, p, 0, s0, perm, len, k,
                        gamma, p, info, nvars, /*rng=*/NULL, mbuf, vpbuf,
                        sicbuf, /*ws=*/NULL);
    free(s0);
    return fail;
}

int coxibgsel(const double *time, const int *status, const double *X, const double *pw, int n, int p, int niter, int H, int kapp, double tau, int perm, int start_full, int len, double k, double gamma, int info, int nthr, int *xsout, int *psout, int *lfout)
{
    int p0 = p;

    /* event count: the effective sample size that bounds the block/model size */
    int d = 0;
    for (int i = 0; i < n; i++) d += (status[i] != 0);

#ifdef _OPENMP
    if (nthr <= 0) nthr = omp_get_max_threads();
#endif

    /* one per-search workspace, reused across every iteration and the final
     * screening (the long run that records the outputs is done by coxibgrun) */
    srwst ws;
    if (srwsallc(&ws, p, n)) return 1;
    int    *inS2   = ws.inS2;
    int    *S2     = ws.S2;
    int    *S1     = ws.S1;
    double *vfreq  = ws.vfreq;
    int    *assign = ws.assign;

    int nS2 = 0, fail = 0;

    /* ---- refinement iterations (screen -> select -> threshold) ---- */
    for (int iter = 1; iter < niter && !fail; iter++) {
        int nS1 = 0;
        for (int j = 0; j < p; j++) if (!inS2[j]) S1[nS1++] = j;

        int h = coxnblks(nS1, H, d, nS2);
        coxdrwbl(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = coxscrbl(time, status, X, pw, n, S1, nS1, S2, nS2, h,
                        perm, start_full, len, k, gamma, p0, info, nthr, assign,
                        ws.seeds, vfreq);
        if (fail) break;

        int kk = (kapp < nS1) ? kapp : nS1;
        fit *arr = ws.arr;
        for (int i = 0; i < nS1; i++) {
            arr[i].v   = vfreq[S1[i]];
            arr[i].idx = S1[i];
        }
        qsort(arr, nS1, sizeof(fit), ficmpdsc);

        int ps = kk + nS2;
        int *xs = ws.xs;
        for (int i = 0; i < kk; i++)  xs[i] = arr[i].idx;
        for (int i = 0; i < nS2; i++) xs[kk + i] = S2[i];
        qsort(xs, ps, sizeof(int), intcmp);

        double *Xs = ws.Xs;
        int    *s0 = ws.s0;
        double *fr = ws.fr;
        gathcols(X, n, xs, ps, Xs);
        for (int i = 0; i < ps; i++) s0[i] = start_full ? 1 : 0;
        fail = runcoxgb(time, status, Xs, pw, n, ps, 0, s0, perm, len, k,
                        gamma, p0, info, ps, NULL, NULL, fr, NULL, NULL);
        if (fail) break;

        int cnt = 0;
        for (int i = 0; i < ps; i++) if (fr[i] > tau) cnt++;
        memset(inS2, 0, (size_t) p * sizeof(int));
        nS2 = 0;
        for (int i = 0; i < ps; i++) {
            if (cnt > 1 ? (fr[i] > tau) : 1) {
                S2[nS2] = xs[i];
                inS2[xs[i]] = 1;
                nS2++;
            }
        }
    }

    /* ---- final screening + select: fix the converged candidate set ----
     * The long Gibbs run that records the R outputs is done by coxibgrun(), so
     * this kernel touches no R objects. */
    *lfout = 4 * len;
    int ps = 0;

    if (!fail) {
        int nS1 = 0;
        for (int j = 0; j < p; j++) if (!inS2[j]) S1[nS1++] = j;

        int h = coxnblks(nS1, H, d, nS2);
        coxdrwbl(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = coxscrbl(time, status, X, pw, n, S1, nS1, S2, nS2,
                        h, perm, start_full, len, k, gamma, p0, info, nthr,
                        assign, ws.seeds, vfreq);

        if (!fail) {
            int kk = (kapp < nS1) ? kapp : nS1;
            ps = kk + nS2;
            fit  *arr = ws.arr;
            int  *xs  = xsout;   /* build the candidate set directly into the caller buffer (ps <= p) */
            for (int i = 0; i < nS1; i++) {
                arr[i].v   = vfreq[S1[i]];
                arr[i].idx = S1[i];
            }
            qsort(arr, nS1, sizeof(fit), ficmpdsc);
            for (int i = 0; i < kk; i++)  xs[i] = arr[i].idx;
            for (int i = 0; i < nS2; i++) xs[kk + i] = S2[i];
            qsort(xs, ps, sizeof(int), intcmp);
        }
    }

    srwsfree(&ws);

    *psout = ps;
    return fail;
}

/* Final long Cox Gibbs run (the fill phase behind the cox_ibgs_glm() .Call
 * wrapper).  Given the converged candidate columns xs (ps 0-based indices from
 * coxibgsel) it runs a length-lenf Cox Gibbs sampler over those columns and
 * writes the results straight into the caller's buffers -- no R/SEXP handling.
 *   xs    : the ps candidate columns (0-based), as returned by coxibgsel.
 *   ps    : number of candidate columns; lenf : recorded sweeps (= 4*len).
 *   omat  : OUTPUT int[lenf * (1+ps)] indicator matrix (column-major).
 *   oic   : OUTPUT double[lenf] per-sample information criterion.
 *   vprob : OUTPUT double[p] marginal inclusion prob over ALL p columns (zeroed
 *           here, then the ps selected positions are scattered in).
 *   sel   : OUTPUT int[ps] 1-based original indices of the candidate columns.
 * Allocates its gather/run scratch with R_Calloc/R_Free (main thread); returns 0
 * on success, 1 on a numerical failure. */
int coxibgrun(const double *time, const int *status, const double *X, const double *pw, int n, int p, const int *xs, int ps, int lenf, int perm, int start_full, double k, double gamma, int info, int *omat, double *oic, double *vprob, int *sel)
{
    double *Xs = R_Calloc((size_t) n * ps, double);
    int    *s0 = R_Calloc((size_t) ps, int);
    double *fr = R_Calloc((size_t) ps, double);

    gathcols(X, n, xs, ps, Xs);
    for (int i = 0; i < ps; i++) s0[i] = 1;
    int fail = runcoxgb(time, status, Xs, pw, n, ps, 0, s0, perm, lenf, k, gamma, p, info, ps, NULL, omat, fr, oic, NULL);

    if (!fail) {
        for (int j = 0; j < p; j++) vprob[j] = 0.0;
        for (int i = 0; i < ps; i++) {
            vprob[xs[i]] = fr[i];
            sel[i]       = xs[i] + 1;
        }
    }

    R_Free(Xs);
    R_Free(s0);
    R_Free(fr);
    return fail;
}

/* coxcoef -- see ibgs.h for the full contract.  Builds the descending-time order
 * once (reusing tit/ticmpdsc, exactly as runcoxgb does) so coxfit()'s risk-set
 * sweep is a single linear pass, fits the q columns by Newton-Raphson, then zeros
 * the coefficients and *m2pll if the fit is singular or returns non-finite
 * values.  Scratch is R_Calloc/R_Free (main thread). */
void coxcoef(const double *time, const int *status, const double *pw, const double *X, int n, int q, double *bout, double *m2pll)
{
    *m2pll = 0.0;
    for (int a = 0; a < q; a++) bout[a] = 0.0;
    if (q < 1) return;

    int    *active = R_Calloc((size_t) q, int);
    int    *order  = R_Calloc((size_t) n, int);
    tit    *pairs  = R_Calloc((size_t) n, tit);
    double *wq = R_Calloc((size_t) 3 * q * q + 7 * q, double);
    double *wn = R_Calloc((size_t) 2 * n, double);

    for (int a = 0; a < q; a++) active[a] = a;
    for (int i = 0; i < n; i++) {
        pairs[i].t = time[i];
        pairs[i].idx = i;
    }
    qsort(pairs, n, sizeof(tit), ticmpdsc);
    for (int i = 0; i < n; i++) order[i] = pairs[i].idx;

    coxfit(time, status, pw, X, active, n, q, order, wq, wn, m2pll, NULL, bout);

    /* a rank-deficient refit can yield non-finite coefficients; zero them so
     * predictions never see NaN. */
    for (int a = 0; a < q; a++) {
        if (!R_FINITE(bout[a])) {
            for (int c = 0; c < q; c++) bout[c] = 0.0;
            *m2pll = 0.0;
            break;
        }
    }

    R_Free(active);
    R_Free(order);
    R_Free(pairs);
    R_Free(wq);
    R_Free(wn);
}

/* Cox model-averaging summary: the cox.c parallel of glmsumm.  The Cox model has
 * no intercept, so coef is nr = p rows (the predictor at original column xs[c-1]
 * lands in row xs[c-1]) and the representative refit uses coxcoef.  omat/oic/xs
 * as in glmsumm; micout/frqout length nm_in.  Writes *nm models.  summnm/summtab
 * are shared from glm.c.  Returns 0, or 1 on allocation failure. */
int coxsumm(const double *time, const int *status, const double *pw, const double *X, int n, int p, const int *xs, int ps, const int *omat, const double *oic, int lenf, int nm_in, double *coef, double *micout, double *frqout, int *nm)
{
    int nr = p;
    int cap = ps > 0 ? ps : 1;
    int *cnt = (int *) malloc((size_t) nm_in * sizeof(int));
    int *rep = (int *) malloc((size_t) nm_in * sizeof(int));
    int *act = (int *) malloc((size_t) cap * sizeof(int));
    double *Xact = (double *) malloc((size_t) n * cap * sizeof(double));
    double *bout = (double *) malloc((size_t) cap * sizeof(double));
    double m2pll = 0.0;
    if (!cnt || !rep || !act || !Xact || !bout) {
        free(cnt);
        free(rep);
        free(act);
        free(Xact);
        free(bout);
        return 1;
    }
    if (summtab(oic, lenf, nm_in, micout, cnt, rep, nm)) {
        free(cnt);
        free(rep);
        free(act);
        free(Xact);
        free(bout);
        return 1;
    }
    int m = *nm;
    for (size_t t = 0; t < (size_t) nr * m; t++) coef[t] = 0.0;
    for (int i = 0; i < m; i++) {
        int q = 0;
        for (int c = 1; c <= ps; c++) {
            if (omat[(size_t) rep[i] + (size_t) c * lenf] == 1) {
                act[q] = xs[c - 1];
                q++;
            }
        }
        gathcols(X, n, act, q, Xact);
        coxcoef(time, status, pw, Xact, n, q, bout, &m2pll);
        for (int j = 0; j < q; j++)
            coef[(size_t) act[j] + (size_t) i * nr] = bout[j];
        frqout[i] = (double) cnt[i] / (double) lenf;
    }
    free(cnt);
    free(rep);
    free(act);
    free(Xact);
    free(bout);
    return 0;
}
