/*
 * rlm.c -- the linear-mixed-model fixed-effect family (whitened OLS): the
 * Metropolis-within-Gibbs sampler step and the iterated-block-Gibbs
 * orchestration.
 *
 * Consolidates the former rlm_step.c and rlm_ibgs.c.  The original per-module
 * comment blocks below act as section banners.
 */
#include "ibgs.h"

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#ifdef _OPENMP
#include <omp.h>
#endif

/*
 * rlm_step.c -- Metropolis-within-Gibbs sampler over fixed-effect indicators for
 * a linear mixed model with a held random part.
 *
 * The data are already whitened in R (ystar = L^-1 y, Xstar = L^-1 X,
 * istar = L^-1 1 with V = L L'), so each candidate model is an ordinary least
 * squares fit on the whitened design [istar | Xstar].  The per-model cost is a
 * Cholesky on the active sub-block of the precomputed Gram matrix M*'M* -- the
 * SAME primitive (rsschol) the gaussian sampler uses.  The only differences
 * from gibbs_step.c's gaussian path are (1) the intercept is the provided
 * whitened column istar rather than a ones vector, so the Gram is built plainly,
 * and (2) the constant log|V0| is added to the criterion.
 */


#define UNIF(rng) ((rng) ? rngunif(rng) : unif_rand())

/* information criterion of the active whitened columns (defined below) */
static double rlmic(const double *G, const double *Gy, int ptot1, const int *active, int q, double yty, double *Sbuf, double *bbuf, int n, double ldv0, int info, double gamma, int p0, int *ok);

/* Allocate the rlm sampler workspace for up to capt columns over n rows.
 * Uses R_Calloc (main thread only); on out of memory R_Calloc raises an R error
 * rather than returning. */
int rlwsallc(rlwst *ws, int capt, int n)
{
    if (capt < 1) capt = 1;
    ws->capt = capt;
    ws->n    = n;

    ws->inc    = R_Calloc((size_t) capt, int);
    ws->active = R_Calloc((size_t) capt, int);
    ws->ord    = R_Calloc((size_t) capt, int);
    ws->G      = R_Calloc((size_t) capt * capt, double);
    ws->Gy     = R_Calloc((size_t) capt, double);
    ws->Sbuf   = R_Calloc((size_t) capt * capt, double);
    ws->bbuf   = R_Calloc((size_t) capt, double);
    ws->bcols  = R_Calloc((size_t) capt, int);
    ws->s0     = R_Calloc((size_t) capt, int);
    ws->fr     = R_Calloc((size_t) capt, double);
    ws->Xb     = R_Calloc((size_t) (n > 0 ? n : 1) * capt, double);
    return 0;
}

void rlwsfree(rlwst *ws)
{
    R_Free(ws->inc);
    R_Free(ws->active);
    R_Free(ws->ord);
    R_Free(ws->G);
    R_Free(ws->Gy);
    R_Free(ws->Sbuf);
    R_Free(ws->bbuf);
    R_Free(ws->bcols);
    R_Free(ws->s0);
    R_Free(ws->fr);
    R_Free(ws->Xb);
}

int runrlmgb(const double *ystar, const double *Xstar, const double *istar, int n, int p1, int p2, const int *smod, int perm, int len, double k, double gamma, int p0, int info, double ldv0, int nvars, rngt *rng, int *omat, double *ofrq, double *oic, rlwst *wsi)
{
    int ptot  = p1 + p2;
    int ptot1 = ptot + 1;                 /* + intercept (column 0 = istar) */
    int a, b, i;

    /* Use the caller's workspace, or allocate a private one (main thread only)
     * when wsi is NULL; pointing the original locals at the workspace fields
     * keeps the sampler body below unchanged. */
    rlwst  wsl;
    rlwst *ws    = wsi;
    int    owned = 0;
    if (!ws) {
        rlwsallc(&wsl, ptot1, n);
        ws    = &wsl;
        owned = 1;
    }
    int    *inc    = ws->inc;
    int    *active = ws->active;
    int    *ord    = ws->ord;
    double *G      = ws->G;
    double *Gy     = ws->Gy;
    double *Sbuf   = ws->Sbuf;
    double *bbuf   = ws->bbuf;

    /* Precompute the plain Gram of M* = [istar | Xstar] ONCE:
     *   G  = M*'M*  (ptot1 x ptot1),  Gy = M*'ystar,  yty = ystar'ystar.
     * Column 0 is the whitened intercept istar; columns 1..ptot are Xstar. */
    {
        double s00 = 0.0;
        for (i = 0; i < n; i++) s00 += istar[i] * istar[i];
        G[0] = s00;
        for (a = 0; a < ptot; a++) {
            const double *Xa = Xstar + (size_t) a * n;
            double s = 0.0;
            for (i = 0; i < n; i++) s += istar[i] * Xa[i];
            G[(a + 1) * ptot1] = s;
            G[a + 1]           = s;
        }
        for (a = 0; a < ptot; a++) {
            const double *Xa = Xstar + (size_t) a * n;
            for (b = a; b < ptot; b++) {
                const double *Xb = Xstar + (size_t) b * n;
                double s = 0.0;
                for (i = 0; i < n; i++) s += Xa[i] * Xb[i];
                G[(a + 1) * ptot1 + (b + 1)] = s;
                G[(b + 1) * ptot1 + (a + 1)] = s;
            }
        }
    }
    double yty = 0.0;
    {
        double s0 = 0.0;
        for (i = 0; i < n; i++) { s0 += istar[i] * ystar[i]; yty += ystar[i] * ystar[i]; }
        Gy[0] = s0;
    }
    for (a = 0; a < ptot; a++) {
        const double *Xa = Xstar + (size_t) a * n;
        double s = 0.0;
        for (i = 0; i < n; i++) s += Xa[i] * ystar[i];
        Gy[a + 1] = s;
    }

    /* model criterion: OLS RSS of the active whitened columns + the constant
     * log|V0|; npar = q+1 (coefs incl. intercept + sigma^2), npred = q-1. */
    #define RLM_IC(qc, okc)                                                     \
        rlmic(G, Gy, ptot1, active, (qc), yty, Sbuf, bbuf, n, ldv0, info,      \
              gamma, p0, &(okc))

    /* ----- initial state ----- */
    /* inc[] indicates the ptot predictor columns: first p1 toggleable (seeded
     * from smod), trailing p2 forced on.  The intercept (column 0 = istar) is
     * always active and is not in inc[]. */
    int nsel = 0;
    for (a = 0; a < p1; a++)    { inc[a] = smod[a] ? 1 : 0; nsel += inc[a]; }
    for (a = p1; a < ptot; a++) { inc[a] = 1; nsel += 1; }

    #define BUILD_ACTIVE(qout)                                  \
        do {                                                     \
            int _q = 1; active[0] = 0;                           \
            for (int _j = 0; _j < ptot; _j++)                    \
                if (inc[_j]) active[_q++] = _j + 1;              \
            (qout) = _q;                                        \
        } while (0)

    int q, ok;
    BUILD_ACTIVE(q);
    double curic = RLM_IC(q, ok);
    if (!ok) curic = R_PosInf;

    if (ofrq)
        for (a = 0; a < p1; a++) ofrq[a] = 0.0;

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

            int pnsel = nsel + (inc[j] ? -1 : 1);
            if (pnsel < 1 || pnsel > nvars) continue;

            inc[j] ^= 1;
            int pq;
            BUILD_ACTIVE(pq);
            double propic = RLM_IC(pq, ok);

            int accept = 0;
            if (ok) {
                double A = exp(k * (curic - propic));
                if (A > 1.0) A = 1.0;
                if (UNIF(rng) < A) accept = 1;
                if (accept) { curic = propic; nsel = pnsel; }
            }
            if (!accept) inc[j] ^= 1;
        }

        if (sw >= len) {
            int row = sw - len;
            if (omat) {
                omat[row] = 1;
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

    #undef RLM_IC
    #undef BUILD_ACTIVE
    if (owned) rlwsfree(&wsl);
    return 0;
}

/* helper kept after the entry so the macro above reads cleanly */
static double rlmic(const double *G, const double *Gy, int ptot1, const int *active, int q, double yty, double *Sbuf, double *bbuf, int n, double ldv0, int info, double gamma, int p0, int *ok)
{
    double rss = rsschol(G, Gy, ptot1, active, q, yty, Sbuf, bbuf);
    if (rss < 0.0) { *ok = 0; return 0.0; }
    *ok = 1;
    double base = (double) n * (log(2.0 * M_PI * rss / (double) n) + 1.0) + ldv0;
    return icval(base, q + 1, q - 1, n, info, gamma, p0);
}
/*
 * rlm_ibgs.c -- pure-C orchestration of the iterated block Gibbs sampler for
 * linear-mixed-model fixed-effect selection (random part held fixed).  The rlm
 * parallel of ibgs.c / cox_ibgs.c: same screen -> select -> threshold structure
 * and reproducible parallel block screening; only the per-model fit differs
 * (whitened OLS via runrlmgb).  See glm.c for the full algorithm.
 *
 * The data are whitened in R, so the whitened intercept `istar` is shared across
 * every block and threaded through unchanged.  Block sizing uses the sample size
 * n (as in the gaussian path).  Kept self-contained so the existing modules are
 * untouched.
 */



/* ------------------------------------------------------------------ */
/* Helpers (rlm-specific block sizing/seeding; the freq/index ranking,  */
/* column gather, and per-search workspace are shared, from ibgs.h).    */
/* ------------------------------------------------------------------ */

/* number of blocks = ceil(nS1 / block_size), block_size = min(H, n - nS2) */
static int rlmnblks(int nS1, int H, int n, int nS2)
{
    int bs = (H < n - nS2) ? H : (n - nS2);
    if (bs < 1) bs = 1;
    int h = (nS1 + bs - 1) / bs;
    return (h < 1) ? 1 : h;
}

static void rlmdrwbl(int nS1, int h, int *assign, uint64_t *seeds)
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
 * One screening step: split S1 into `h` blocks and run a within-block whitened
 * OLS sampler in parallel; the fixed columns S2 are always included, as is the
 * shared whitened intercept istar.  Writes each S1 column's inclusion frequency
 * into vfreq[].  Returns 0 on success, 1 on failure.
 */
static int rlmscrbl(const double *ystar, const double *Xstar, const double *istar, int n, const int *S1, int nS1, const int *S2, int nS2, int h, int perm, int len, double k, double gamma, int p0, int info, double ldv0, int nthr, const int *assign, const uint64_t *seeds, double *vfreq)
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

    /* size every per-thread workspace for the largest block (+ whitened
     * intercept istar, which is shared and passed in, not gathered) */
    int maxpb = 0;
    for (int b = 0; b < h; b++) if (sz[b] > maxpb) maxpb = sz[b];
    int capt = maxpb + nS2 + 1;

    /* one reusable workspace per thread, allocated up front on the main thread
     * (R_Calloc is not thread-safe), so the parallel loop allocates nothing */
    int nws = nthr > 0 ? nthr : 1;
#ifndef _OPENMP
    nws = 1;
#endif
    rlwst *wsa = R_Calloc((size_t) nws, rlwst);
    for (int t = 0; t < nws; t++) rlwsallc(&wsa[t], capt, n);

    int fail = 0;
#ifdef _OPENMP
    #pragma omp parallel for num_threads(nthr) schedule(dynamic) shared(fail)
#endif
    for (int b = 0; b < h; b++) {
        int pb = sz[b];
        if (pb <= 0) continue;

#ifdef _OPENMP
        rlwst *ws = &wsa[omp_get_thread_num()];
#else
        rlwst *ws = &wsa[0];
#endif
        int    *bcols = ws->bcols;
        int    *s0    = ws->s0;
        double *fr    = ws->fr;
        double *Xb    = ws->Xb;

        for (int c = 0; c < pb; c++) {
            bcols[c] = S1[pos[off[b] + c]];
            s0[c]    = 1;
        }
        for (int c = 0; c < nS2; c++) bcols[pb + c] = S2[c];
        for (int c = 0; c < pb + nS2; c++)
            memcpy(Xb + (size_t) c * n, Xstar + (size_t) bcols[c] * n, (size_t) n * sizeof(double));

        rngt rng;
        rngseed(&rng, seeds[b]);
        int rc = runrlmgb(ystar, Xb, istar, n, pb, nS2, s0, perm, len, k,
                          gamma, p0, info, ldv0, pb + nS2, &rng, NULL,
                          fr, NULL, ws);
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

    for (int t = 0; t < nws; t++) rlwsfree(&wsa[t]);
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

int rlmgbsam(const double *ystar, const double *Xstar, const double *istar, int n, int p, int nvars, int perm, int len, double k, double gamma, int info, double ldv0, int *mbuf, double *sicbuf, double *vpbuf)
{
    if (nvars < 1) nvars = 1;
    if (nvars > p) nvars = p;

    int *s0 = (int *) malloc((size_t) p * sizeof(int));
    if (!s0) return 1;
    for (int i = 0; i < p; i++) s0[i] = (i < nvars) ? 1 : 0;

    int fail = runrlmgb(ystar, Xstar, istar, n, p, 0, s0, perm, len, k, gamma,
                        p, info, ldv0, nvars, /*rng=*/NULL, mbuf, vpbuf,
                        sicbuf, /*ws=*/NULL);
    free(s0);
    return fail;
}

int rlmibgsel(const double *ystar, const double *Xstar, const double *istar, int n, int p, int niter, int H, int kapp, double tau, int perm, int len, double k, double gamma, int info, double ldv0, int nthr, int *xsout, int *psout, int *lfout)
{
    int p0 = p;

#ifdef _OPENMP
    if (nthr <= 0) nthr = omp_get_max_threads();
#endif

    /* one per-search workspace, reused across every iteration and the final
     * screening (the long run that records the outputs is done by rlmibgrun) */
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

        int h = rlmnblks(nS1, H, n, nS2);
        rlmdrwbl(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = rlmscrbl(ystar, Xstar, istar, n, S1, nS1, S2, nS2, h, perm,
                        len, k, gamma, p0, info, ldv0, nthr, assign,
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
        gathcols(Xstar, n, xs, ps, Xs);
        for (int i = 0; i < ps; i++) s0[i] = 1;
        fail = runrlmgb(ystar, Xs, istar, n, ps, 0, s0, perm, len, k, gamma,
                        p0, info, ldv0, ps, NULL, NULL, fr, NULL, NULL);
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
     * The long Gibbs run that records the R outputs is done by rlmibgrun(), so
     * this kernel touches no R objects. */
    *lfout = 4 * len;
    int ps = 0;

    if (!fail) {
        int nS1 = 0;
        for (int j = 0; j < p; j++) if (!inS2[j]) S1[nS1++] = j;

        int h = rlmnblks(nS1, H, n, nS2);
        rlmdrwbl(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = rlmscrbl(ystar, Xstar, istar, n, S1, nS1, S2, nS2, h,
                        perm, len, k, gamma, p0, info, ldv0, nthr,
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

/* Final long whitened-OLS Gibbs run (the fill phase behind the rlm_ibgs_glm()
 * .Call wrapper).  Given the converged candidate columns xs (ps 0-based indices
 * from rlmibgsel) it runs a length-lenf Gibbs sampler on the whitened design over
 * those columns and writes the results straight into the caller's buffers -- no
 * R/SEXP handling.
 *   xs    : the ps candidate columns (0-based), as returned by rlmibgsel.
 *   ps    : number of candidate columns; lenf : recorded sweeps (= 4*len).
 *   ldv0  : log|V0|, the constant added to the criterion (whitening).
 *   omat  : OUTPUT int[lenf * (1+ps)] indicator matrix (column-major).
 *   oic   : OUTPUT double[lenf] per-sample information criterion.
 *   vprob : OUTPUT double[p] marginal inclusion prob over ALL p columns (zeroed
 *           here, then the ps selected positions are scattered in).
 *   sel   : OUTPUT int[ps] 1-based original indices of the candidate columns.
 * Allocates its gather/run scratch with R_Calloc/R_Free (main thread); returns 0
 * on success, 1 on a numerical failure. */
int rlmibgrun(const double *ystar, const double *Xstar, const double *istar, int n, int p, const int *xs, int ps, int lenf, int perm, double k, double gamma, int info, double ldv0, int *omat, double *oic, double *vprob, int *sel)
{
    double *Xs = R_Calloc((size_t) n * ps, double);
    int    *s0 = R_Calloc((size_t) ps, int);
    double *fr = R_Calloc((size_t) ps, double);

    gathcols(Xstar, n, xs, ps, Xs);
    for (int i = 0; i < ps; i++) s0[i] = 1;
    int fail = runrlmgb(ystar, Xs, istar, n, ps, 0, s0, perm, lenf, k, gamma, p, info, ldv0, ps, NULL, omat, fr, oic, NULL);

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

/* rlmcoef -- see ibgs.h for the full contract.  Assembles the whitened Gram
 * G = D'D and cross-product Gy = D'ystar, solves G beta = Gy with cholsolv(),
 * then zeros any non-finite coefficient.  Because the data are pre-whitened the
 * plain (unweighted) normal equations already give the GLS fixed-effect estimate.
 * Scratch is R_Calloc/R_Free (main thread). */
void rlmcoef(const double *ystar, const double *D, int n, int q, double *bout)
{
    for (int a = 0; a < q; a++) bout[a] = 0.0;
    if (q < 1) return;

    /* normal equations (D'D) beta = D'y, solved by Cholesky */
    double *G  = R_Calloc((size_t) q * q, double);
    double *Gy = R_Calloc((size_t) q, double);
    for (int a = 0; a < q; a++) {
        const double *Daptr = D + (size_t) a * n;
        double sgy = 0.0;
        for (int i = 0; i < n; i++) sgy += Daptr[i] * ystar[i];
        Gy[a] = sgy;
        for (int c = a; c < q; c++) {
            const double *Dcptr = D + (size_t) c * n;
            double s = 0.0;
            for (int i = 0; i < n; i++) s += Daptr[i] * Dcptr[i];
            G[a * q + c] = s;
            G[c * q + a] = s;
        }
    }
    if (cholsolv(G, Gy, bout, q)) for (int a = 0; a < q; a++) bout[a] = 0.0;

    /* zero non-finite coefficients so predictions never see NaN */
    for (int a = 0; a < q; a++) {
        if (!R_FINITE(bout[a])) {
            for (int c = 0; c < q; c++) bout[c] = 0.0;
            break;
        }
    }

    R_Free(G);
    R_Free(Gy);
}

/* rlm model-averaging summary: the rlm.c parallel of glmsumm.  Refits on the
 * WHITENED data -- the representative design is D = [istar | active Xstar columns]
 * and rlmcoef returns the ORIGINAL-space [intercept, beta] -- so coef is nr = p+1
 * rows (intercept in row 0).  omat/oic/xs as in glmsumm; micout/frqout length
 * nm_in.  Writes *nm models.  summnm/summtab are shared from glm.c.  Returns 0,
 * or 1 on allocation failure. */
int rlmsumm(const double *ystar, const double *Xstar, const double *istar, int n, int p, const int *xs, int ps, const int *omat, const double *oic, int lenf, int nm_in, double *coef, double *micout, double *frqout, int *nm)
{
    int nr = p + 1;
    int cap = ps > 0 ? ps : 1;
    int *cnt = (int *) malloc((size_t) nm_in * sizeof(int));
    int *rep = (int *) malloc((size_t) nm_in * sizeof(int));
    int *act = (int *) malloc((size_t) cap * sizeof(int));
    double *D = (double *) malloc((size_t) n * (cap + 1) * sizeof(double));
    double *bout = (double *) malloc((size_t) (ps + 1) * sizeof(double));
    if (!cnt || !rep || !act || !D || !bout) {
        free(cnt);
        free(rep);
        free(act);
        free(D);
        free(bout);
        return 1;
    }
    if (summtab(oic, lenf, nm_in, micout, cnt, rep, nm)) {
        free(cnt);
        free(rep);
        free(act);
        free(D);
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
        memcpy(D, istar, (size_t) n * sizeof(double));
        for (int j = 0; j < q; j++)
            memcpy(D + (size_t)(j + 1) * n, Xstar + (size_t) act[j] * n, (size_t) n * sizeof(double));
        rlmcoef(ystar, D, n, q + 1, bout);
        coef[(size_t) i * nr] = bout[0];
        for (int j = 0; j < q; j++)
            coef[(size_t)(act[j] + 1) + (size_t) i * nr] = bout[j + 1];
        frqout[i] = (double) cnt[i] / (double) lenf;
    }
    free(cnt);
    free(rep);
    free(act);
    free(D);
    free(bout);
    return 0;
}
