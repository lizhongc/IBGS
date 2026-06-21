/*
 * glm.c -- the gaussian / binomial / poisson family: the GLM fitting kernels,
 * the Metropolis-within-Gibbs sampler step, and the iterated-block-Gibbs
 * orchestration -- plus the shared search helpers (column gather, frequency
 * ranking, per-search workspace) declared in ibgs.h and used by all three
 * families.
 *
 * Consolidates the former glm_fit.c (IRLS/OLS kernels), gibbs_step.c (sampler
 * step), and ibgs.c (orchestration + shared search scratch).  The original
 * per-module comment blocks below act as section banners.
 */
#include "ibgs.h"

#include <R.h>
#include <Rmath.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#ifdef _OPENMP
#include <omp.h>
#endif

/*
 * glm_fit.c -- IRLS fitting for binomial (logit) and poisson (log) families.
 *
 * BACKGROUND: IRLS / Fisher scoring for a GLM
 * -------------------------------------------
 * A generalized linear model relates the mean mu_i = E[y_i] to a linear
 * predictor eta_i = x_i' beta through a link g, eta_i = g(mu_i), with the
 * variance a known function of the mean, Var(y_i) = phi * V(mu_i)/w_i (w_i a
 * prior weight).  The maximum-likelihood estimate of beta has no closed form,
 * so it is found by Fisher scoring, which for a GLM is exactly *iteratively
 * reweighted least squares* (IRLS).  Each iteration:
 *
 *   1. From the current eta compute mu = g^{-1}(eta) and the derivative
 *      dmu/deta.
 *   2. Form the IRLS weight and "working response" (the local linearisation of
 *      the log-likelihood around the current fit):
 *           W_i = w_i * (dmu/deta)^2 / V(mu_i)
 *           z_i = eta_i + (y_i - mu_i) / (dmu/deta)
 *   3. Update beta by the *weighted least squares* fit of z on the design D:
 *           beta = (D'WD)^{-1} D'W z .
 *      We never form the inverse; we solve the normal equations D'WD beta=D'Wz
 *      by a Cholesky factorisation (cholsolv below).
 *
 * Iterating to convergence (here: the change in deviance becomes negligible)
 * gives the MLE.  This file specialises the above to:
 *   - binomial with the logit link  : mu = 1/(1+e^{-eta}),  V(mu)=mu(1-mu),
 *                                      dmu/deta = mu(1-mu);
 *   - poisson  with the log link     : mu = e^{eta},          V(mu)=mu,
 *                                      dmu/deta = mu.
 * For both, dmu/deta == V(mu), so the working weight simplifies to W_i = w_i *
 * V(mu_i) (binomial: w_i mu(1-mu); poisson: w_i mu) and the working response to
 * z_i = eta_i + (y_i - mu_i)/V(mu_i).
 *
 * The returned value is -2*logLik on the *same scale as R's glm()* (including
 * the binomial normalising constant R uses), so the information criteria built
 * from it in gibbs_step.c reproduce R's AIC()/BIC()/AICc()/exBIC() exactly.
 * The gaussian family is fitted directly by OLS in gibbs_step.c and never
 * enters this file.
 */


#define IRLS_MAXIT 50    /* hard cap on IRLS iterations (safety net)            */
#define IRLS_TOL   1e-10 /* relative deviance change treated as convergence     */
#define MU_EPS     1e-10 /* clamp keeping mu inside (0,1) / (0,inf) so logs and */
                         /* divisions stay finite at the boundary of the range  */

/*
 * Solve the symmetric positive-definite system A x = b by Cholesky.
 *
 * A is q x q row-major; only its lower triangle is read.  We factor A = L L'
 * with L lower-triangular (overwriting A's lower triangle in place), then solve
 * the two triangular systems L u = b (forward substitution) and L' x = u (back
 * substitution).  This is the standard O(q^3/3) SPD solver and is numerically
 * stable without pivoting because A = D'WD is symmetric positive (semi-)
 * definite by construction.
 *
 * The factorisation also doubles as a rank check: if a pivot A[r,r] is <= a
 * small tolerance the matrix is (numerically) singular -- the candidate model
 * has collinear/duplicated columns -- and we bail out with code 1 so the caller
 * can reject that model rather than divide by ~0.
 *
 * Cholesky recurrence (j = column, i = row, i >= j):
 *     L[j,j] = sqrt( A[j,j] - sum_{m<j} L[j,m]^2 )
 *     L[i,j] = ( A[i,j] - sum_{m<j} L[i,m] L[j,m] ) / L[j,j]
 */
int cholsolv(double *A, const double *b, double *x, int q)
{
    int r, s, m;
    for (r = 0; r < q; r++) {
        for (s = 0; s <= r; s++) {
            double sum = A[r * q + s];
            for (m = 0; m < s; m++) sum -= A[r * q + m] * A[s * q + m];
            if (r == s) {
                if (sum <= 1e-12) return 1;        /* not PD -> singular model */
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

/*
 * Weighted deviance for the current means (cheap; used as the convergence
 * monitor inside the IRLS loop).
 *
 * The deviance is 2*(logLik_saturated - logLik(mu)); the saturated terms cancel
 * the data-only constants, leaving an expression in y and mu only, which is why
 * it is cheaper to track than the full log-likelihood every iteration:
 *   binomial: 2 sum_i w_i [ y log(y/mu) + (1-y) log((1-y)/(1-mu)) ]
 *   poisson : 2 sum_i w_i [ y log(y/mu) - (y - mu) ]
 * The `if (y>0)` / `if (y<1)` guards implement the limit x log x -> 0 as x -> 0,
 * so the boundary cases y=0 and y=1 contribute no NaN.
 */
static double glmdev(int family, const double *y, const double *mu, const double *pw, int n)
{
    double dev = 0.0;
    int i;
    if (family == FAM_BINOMIAL) {
        for (i = 0; i < n; i++) {
            double t = 0.0;
            if (y[i] > 0.0)       t += y[i] * log(y[i] / mu[i]);
            if (y[i] < 1.0)       t += (1.0 - y[i]) * log((1.0 - y[i]) / (1.0 - mu[i]));
            dev += pw[i] * t;
        }
    } else { /* poisson */
        for (i = 0; i < n; i++) {
            double t = (y[i] > 0.0 ? y[i] * log(y[i] / mu[i]) : 0.0) - (y[i] - mu[i]);
            dev += pw[i] * t;
        }
    }
    return 2.0 * dev;
}

/*
 * -2*logLik for the converged means, matching R's glm() aic component (so that
 * AIC = glmm2ll + 2*npar reproduces R's AIC()).
 *
 * Unlike the deviance, the full log-likelihood includes the data-only
 * normalising constants (the binomial coefficient, the poisson log(y!)), which
 * R *does* include in AIC.  We therefore add them back here:
 *
 *   binomial: pw[i] are the trial counts m_i and y[i] the success *proportion*,
 *     so the count of successes is s_i = m_i*y_i.  The per-observation
 *     log-likelihood is
 *        log C(m_i,s_i) + s_i log(mu_i) + (m_i-s_i) log(1-mu_i),
 *     with log C(m,s) = lgamma(m+1) - lgamma(s+1) - lgamma(m-s+1).  m_i and s_i
 *     are rounded to the nearest integer (floor(x+0.5)) to match how R forms
 *     the binomial response from a proportion + weights.
 *
 *   poisson: log-likelihood pw[i]*( y_i log(mu_i) - mu_i - log(y_i!) ), with
 *     log(y_i!) = lgamma(y_i + 1).
 *
 * Returns -2 * sum of the per-observation log-likelihoods.
 */
static double glmm2ll(int family, const double *y, const double *mu, const double *pw, int n)
{
    double ll = 0.0;
    int i;
    if (family == FAM_BINOMIAL) {
        for (i = 0; i < n; i++) {
            double m = floor(pw[i] + 0.5);          /* trials    */
            double s = floor(pw[i] * y[i] + 0.5);   /* successes */
            double t = lgamma(m + 1.0) - lgamma(s + 1.0) - lgamma(m - s + 1.0);
            if (s > 0.0)     t += s * log(mu[i]);
            if (m - s > 0.0) t += (m - s) * log(1.0 - mu[i]);
            ll += t;
        }
    } else { /* poisson */
        for (i = 0; i < n; i++)
            ll += pw[i] * (y[i] * log(mu[i]) - mu[i] - lgamma(y[i] + 1.0));
    }
    return -2.0 * ll;
}

int glmirls(int family, const double *y, const double *pw, const double *Dfull, const int *active, int n, int q, double *wq, double *wn, double *dev2, const double *b0, double *bout)
{
    double *XtWX = wq;            /* q x q */
    double *XtWz = wq + q * q;    /* q     */
    double *beta = wq + q * q + q;/* q     */
    double *eta = wn;             /* n */
    double *mu  = wn + n;         /* n */
    double *w   = wn + 2 * n;     /* n */
    double *z   = wn + 3 * n;     /* n */

    int i, a, b, it;

    /* The model design is not materialised: column a of the q-column model is
     * column active[a] of the shared block design Dfull = [1|X] (column 0 is the
     * intercept).  Indexing avoids copying out a compact design on every fit --
     * the same Dfull is reused for every candidate model. */
    #define DCOL(a) (Dfull + (size_t) active[a] * n)

    /* Initial linear predictor.
     *   warm start (b0 != NULL): eta = D b0 starts IRLS from a nearby model's
     *     coefficients, so it converges in far fewer steps (see header);
     *   cold start: the usual GLM starting values -- a smoothed empirical logit
     *     for binomial, log(y+0.1) for poisson -- which keep eta finite even for
     *     y at the boundary. */
    if (b0 != NULL) {
        for (i = 0; i < n; i++) eta[i] = 0.0;
        for (a = 0; a < q; a++) {
            const double *Da = DCOL(a); double ba = b0[a];
            for (i = 0; i < n; i++) eta[i] += Da[i] * ba;
        }
    } else {
        for (i = 0; i < n; i++) {
            if (family == FAM_BINOMIAL) {
                double m = (pw[i] * y[i] + 0.5) / (pw[i] + 1.0);
                eta[i] = log(m / (1.0 - m));
            } else {
                eta[i] = log(y[i] + 0.1);
            }
        }
    }

    double devold = 0.0;
    for (it = 0; it < IRLS_MAXIT; it++) {
        /* Step 1+2: from the current eta compute, per observation, the mean mu,
         * the link derivative mueta = dmu/deta, the variance function var =
         * V(mu), the IRLS weight w = pw * mueta^2 / var, and the working
         * response z = eta + (y - mu)/mueta.  For both families mueta == var,
         * so w collapses to pw*var.  The clamps keep mu off the open boundary
         * (and bound eta for poisson) so exp/log never overflow or divide by 0. */
        for (i = 0; i < n; i++) {
            double e = eta[i], m, mueta, var;
            if (family == FAM_BINOMIAL) {
                m = 1.0 / (1.0 + exp(-e));                 /* logit^{-1} */
                if (m < MU_EPS) m = MU_EPS; else if (m > 1.0 - MU_EPS) m = 1.0 - MU_EPS;
                mueta = m * (1.0 - m);                     /* dmu/deta = mu(1-mu) */
                var   = m * (1.0 - m);                     /* V(mu)    = mu(1-mu) */
            } else { /* poisson */
                if (e >  30.0) e =  30.0;                  /* bound eta: e^30 ~ 1e13 */
                if (e < -30.0) e = -30.0;
                m = exp(e);                                /* log^{-1} */
                if (m < MU_EPS) m = MU_EPS;
                mueta = m;                                 /* dmu/deta = mu */
                var   = m;                                 /* V(mu)    = mu */
            }
            mu[i] = m;
            w[i]  = pw[i] * mueta * mueta / var;           /* prior weight folded in */
            z[i]  = eta[i] + (y[i] - m) / mueta;
        }

        /* Convergence test on the deviance: stop once it stops changing
         * (relative tolerance, with a small absolute floor so a near-zero
         * deviance still terminates).  Skipped on it==0 (no previous value). */
        double dev = glmdev(family, y, mu, pw, n);
        if (it > 0 && fabs(dev - devold) < IRLS_TOL * (fabs(dev) + 0.1))
            break;
        devold = dev;

        /* Step 3: assemble the weighted normal equations of the WLS update,
         * XtWX = D'WD (q x q, symmetric -- only fill b>=a then mirror) and
         * XtWz = D'Wz (q), accumulating over the n observations. */
        for (a = 0; a < q; a++) {
            const double *Da = DCOL(a);
            double sz = 0.0;
            for (i = 0; i < n; i++) sz += Da[i] * w[i] * z[i];
            XtWz[a] = sz;
            for (b = a; b < q; b++) {
                const double *Db = DCOL(b);
                double s = 0.0;
                for (i = 0; i < n; i++) s += Da[i] * w[i] * Db[i];
                XtWX[a * q + b] = s;
                XtWX[b * q + a] = s;
            }
        }

        /* solve D'WD beta = D'Wz; a singular system means a collinear model */
        if (cholsolv(XtWX, XtWz, beta, q)) return 1;   /* singular */

        for (i = 0; i < n; i++) eta[i] = 0.0;             /* eta = D beta */
        for (a = 0; a < q; a++) {
            const double *Da = DCOL(a); double ba = beta[a];
            for (i = 0; i < n; i++) eta[i] += Da[i] * ba;
        }
    }

    /* Recompute the converged means from the final eta (the loop may have exited
     * on the convergence test before refreshing mu) and report -2logLik. */
    for (i = 0; i < n; i++) {
        double e = eta[i], m;
        if (family == FAM_BINOMIAL) {
            m = 1.0 / (1.0 + exp(-e));
            if (m < MU_EPS) m = MU_EPS; else if (m > 1.0 - MU_EPS) m = 1.0 - MU_EPS;
        } else {
            if (e >  30.0) e =  30.0;
            if (e < -30.0) e = -30.0;
            m = exp(e);
            if (m < MU_EPS) m = MU_EPS;
        }
        mu[i] = m;
    }
    *dev2 = glmm2ll(family, y, mu, pw, n);
    if (bout) for (a = 0; a < q; a++) bout[a] = beta[a];
    #undef DCOL
    return 0;
}

/*
 * Information criterion from -2*logLik.  All four are "-2logLik + penalty",
 * differing only in how the model complexity is penalised:
 *   AIC   = -2logLik + 2*npar                          (Akaike)
 *   BIC   = -2logLik + log(n)*npar                     (Schwarz)
 *   AICc  = AIC + 2*npar*(npar+1)/(n-npar-1)           (small-sample AIC)
 *   exBIC = BIC + 2*gamma*npred*log(p0)                (extended BIC of Chen &
 *           Chen 2008, which adds a term for the size p0 of the candidate pool
 *           -- essential in ultrahigh dimensions where BIC under-penalises)
 * npar counts the estimated parameters (coefficients, plus the dispersion for
 * gaussian); npred is the number of predictors excluding the intercept.  Lower
 * is better: the sampler treats icval as the energy to minimise.
 */
double icval(double m2ll, int npar, int npred, int n, int info, double gamma, int p0)
{
    switch (info) {
    case 0: return m2ll + 2.0 * npar;                                        /* AIC   */
    case 1: return m2ll + log((double) n) * npar;                            /* BIC   */
    case 2: return m2ll + 2.0 * npar + 2.0 * npar * (npar + 1.0) / ((double) n - npar - 1.0); /* AICc */
    case 3: return m2ll + log((double) n) * npar + 2.0 * gamma * npred * log((double) p0); /* exBIC */
    default: return m2ll + 2.0 * npar;
    }
}
/*
 * gibbs_step.c -- the Gibbs sampler step for the generalized linear model.
 *
 * For the gaussian family every candidate model is fitted by ordinary least
 * squares via a Cholesky solve on a sub-matrix of the precomputed Gram matrix
 * M'M, M = [1|X] (intercept always included).  For the binomial and poisson
 * families each candidate model is fitted by IRLS (see glmirls).  The
 * information criteria reproduce R's glm()/AIC()/BIC()/AICc()/exBIC().
 *
 * THE SAMPLER (Metropolis-within-Gibbs over model indicators)
 * -----------------------------------------------------------
 * A model is a binary inclusion vector gamma over the p1 toggleable predictors
 * (the p2 trailing columns are always in).  rungibbs() performs a random-scan
 * Metropolis-within-Gibbs walk over this {0,1}^p1 space: each step flips one
 * coordinate j and accepts the flip with probability
 *
 *      A = min( 1, exp{ k * (IC_current - IC_proposed) } ),
 *
 * where IC is the chosen information criterion of the model.  Because lower IC
 * is better, a proposal that lowers the IC (IC_proposed < IC_current) has
 * A = 1 and is always accepted, while a worsening proposal is accepted with a
 * probability that decays in the IC increase; k>0 tunes how greedy the walk is.
 * This is a Gibbs/Metropolis sampler whose stationary distribution favours
 * low-IC models, and whose visit frequencies estimate marginal inclusion
 * probabilities.  Each candidate model's IC is what modelic() computes, by OLS
 * (gaussian) or IRLS (binomial/poisson).
 *
 * PERFORMANCE: the per-model cost is the fit.  Two ideas keep it small --
 *   (1) gaussian: build the full weighted Gram matrix M'WM and M'Wy ONCE; each
 *       candidate fit is then just a Cholesky on the active q x q sub-block, no
 *       pass over the n data rows;
 *   (2) glm: warm-start each IRLS fit from the current model's coefficients
 *       (only one indicator differs between current and proposal), so the fit
 *       converges in a couple of iterations.
 */


/* rng == NULL -> R's unif_rand (main thread); else the thread-safe generator. */
#define UNIF(rng) ((rng) ? rngunif(rng) : unif_rand())

/* ------------------------------------------------------------------ */
/* Gaussian OLS residual sum of squares via Cholesky on the active     */
/* sub-matrix of the Gram matrix.  Negative return = not PD.           */
/* ------------------------------------------------------------------ */
/*
 * For a weighted least squares fit of y on the active columns M_A (M = [1|X]),
 * the normal equations are (M_A'W M_A) beta = M_A'W y.  The full Gram matrix
 * G = M'WM and cross-product Gy = M'Wy are precomputed once by the caller, so
 * here we only:
 *   (1) gather the active q x q sub-block S = G[active,active] and sub-vector
 *       b = Gy[active] (no pass over the n rows -- this is the whole speed-up);
 *   (2) Cholesky-factor S = L L' and solve S beta = b in place (bbuf <- beta);
 *   (3) return the residual sum of squares via the identity
 *           RSS = y'Wy - beta'(M_A'Wy) = yty - sum_r Gy[active[r]] * beta[r],
 *       which avoids ever forming residuals.
 * A non-positive Cholesky pivot means the active columns are collinear; we
 * return -1 so the caller rejects the model.  The tiny RSS floor (1e-12) guards
 * log(RSS) below against an exactly-saturated fit.
 */
double rsschol(const double *G, const double *Gy, int ptot1, const int *active, int q, double yty, double *Sbuf, double *bbuf)
{
    int r, s, m;

    /* (1) copy the active sub-block S and sub-vector b out of G, Gy */
    for (r = 0; r < q; r++) {
        int ar = active[r];
        bbuf[r] = Gy[ar];
        for (s = 0; s < q; s++)
            Sbuf[r * q + s] = G[ar * ptot1 + active[s]];
    }

    /* (2) Cholesky S = L L' (lower triangle), in place into Sbuf */

    for (r = 0; r < q; r++) {
        for (s = 0; s <= r; s++) {
            double sum = Sbuf[r * q + s];
            for (m = 0; m < s; m++)
                sum -= Sbuf[r * q + m] * Sbuf[s * q + m];
            if (r == s) {
                if (sum <= 1e-10) return -1.0;
                Sbuf[r * q + r] = sqrt(sum);
            } else {
                Sbuf[r * q + s] = sum / Sbuf[s * q + s];
            }
        }
    }
    for (r = 0; r < q; r++) {                 /* forward solve L u = b  */
        double sum = bbuf[r];
        for (m = 0; m < r; m++) sum -= Sbuf[r * q + m] * bbuf[m];
        bbuf[r] = sum / Sbuf[r * q + r];
    }
    for (r = q - 1; r >= 0; r--) {            /* back solve L' beta = u */
        double sum = bbuf[r];
        for (m = r + 1; m < q; m++) sum -= Sbuf[m * q + r] * bbuf[m];
        bbuf[r] = sum / Sbuf[r * q + r];
    }
    {   /* (3) RSS = y'Wy - beta' M_A'Wy */
        double fdot = 0.0;
        for (r = 0; r < q; r++) fdot += Gy[active[r]] * bbuf[r];
        double rss = yty - fdot;
        return (rss < 1e-12) ? 1e-12 : rss;
    }
}

/* Per-run context bundling everything modelic() needs for either path. */
typedef struct {
    int family, info, p0, n, ptot1;
    double gamma;
    /* gaussian */
    const double *G, *Gy; double yty, sumlogw; double *Sbuf, *bbuf;
    /* glm (binomial/poisson) */
    const double *y, *X, *pw; double *D, *wq, *wn;
    double *bfull, *b0, *bprop;   /* warm-start coefficients */
} fitctx;

/* Information criterion of the model with the given active column set.
 * active[0] = 0 (intercept); active[r] = (X-column index)+1 for r >= 1.
 * q = number of coefficients.  Sets *ok = 0 if the fit failed. */
static double modelic(fitctx *c, const int *active, int q, int *ok)
{
    if (c->family == FAM_GAUSSIAN) {
        double rss = rsschol(c->G, c->Gy, c->ptot1, active, q, c->yty,
                             c->Sbuf, c->bbuf);
        if (rss < 0.0) { *ok = 0; return 0.0; }
        *ok = 1;
        /* -2logLik of the gaussian MLE (variance profiled out at sigma^2=RSS/n):
         *     -2logL = n*(log(2*pi*RSS/n) + 1) - sum_i log(w_i).
         * The last term is the weighted-likelihood correction sum log(w_i); it
         * is constant across models but kept so the value matches R's glm()
         * exactly.  npar = q+1 counts the q coefficients plus the variance;
         * npred = q-1 excludes the intercept. */
        double base = (double) c->n * (log(2.0 * M_PI * rss / (double) c->n) + 1.0)
                      - c->sumlogw;
        return icval(base, q + 1, q - 1, c->n, c->info, c->gamma, c->p0);
    } else {
        /* warm start from the current model's coefficients (0 for new cols);
         * the design columns are indexed from the shared block design c->D
         * (built once as [1 | X]) -- no per-fit design rebuild. */
        for (int r = 0; r < q; r++) c->b0[r] = c->bfull[active[r]];
        double dev2;
        if (glmirls(c->family, c->y, c->pw, c->D, active, c->n, q,
                    c->wq, c->wn, &dev2, c->b0, c->bprop)) {
            *ok = 0; return 0.0;
        }
        *ok = 1;
        return icval(dev2, q, q - 1, c->n, c->info, c->gamma, c->p0);
    }
}

/* Allocate the sampler workspace for a design of up to capt columns over n
 * rows in `family`.  Uses R_Calloc, so it must run on the main thread; on out of
 * memory R_Calloc raises an R error rather than returning.  The gaussian Gram
 * buffers and the GLM IRLS buffers are mutually exclusive (only the family's set
 * is allocated); the column-gather buffers are always allocated. */
int gbwsallc(gbwst *ws, int capt, int n, int family)
{
    if (capt < 1) capt = 1;
    ws->capt   = capt;
    ws->n      = n;
    ws->family = family;

    ws->inc    = R_Calloc((size_t) capt, int);
    ws->active = R_Calloc((size_t) capt, int);
    ws->bcols  = R_Calloc((size_t) capt, int);
    ws->s0     = R_Calloc((size_t) capt, int);
    ws->fr     = R_Calloc((size_t) capt, double);
    ws->Xb     = R_Calloc((size_t) n * capt, double);

    ws->G = NULL; ws->Gy = NULL; ws->Sbuf = NULL; ws->bbuf = NULL;
    ws->D = NULL; ws->wq = NULL; ws->wn = NULL;
    ws->bfull = NULL; ws->b0 = NULL; ws->bprop = NULL;

    if (family == FAM_GAUSSIAN) {
        ws->G    = R_Calloc((size_t) capt * capt, double);
        ws->Gy   = R_Calloc((size_t) capt, double);
        ws->Sbuf = R_Calloc((size_t) capt * capt, double);
        ws->bbuf = R_Calloc((size_t) capt, double);
    } else {
        ws->D     = R_Calloc((size_t) n * capt, double);
        ws->wq    = R_Calloc((size_t) capt * capt + 2 * capt, double);
        ws->wn    = R_Calloc((size_t) 4 * n, double);
        ws->bfull = R_Calloc((size_t) capt, double);
        ws->b0    = R_Calloc((size_t) capt, double);
        ws->bprop = R_Calloc((size_t) capt, double);
    }
    return 0;
}

/* Free every buffer of a workspace (R_Free is a no-op on the NULL fields of the
 * family set that was not allocated). */
void gbwsfree(gbwst *ws)
{
    R_Free(ws->inc);
    R_Free(ws->active);
    R_Free(ws->bcols);
    R_Free(ws->s0);
    R_Free(ws->fr);
    R_Free(ws->Xb);
    R_Free(ws->G);
    R_Free(ws->Gy);
    R_Free(ws->Sbuf);
    R_Free(ws->bbuf);
    R_Free(ws->D);
    R_Free(ws->wq);
    R_Free(ws->wn);
    R_Free(ws->bfull);
    R_Free(ws->b0);
    R_Free(ws->bprop);
}

int rungibbs(const double *y, const double *X, const double *pw, int n, int p1, int p2, const int *smod, int perm, int len, double k, double gamma, int p0, int info, int family, int nvars, rngt *rng, int *omat, double *ofrq, double *oic, gbwst *wsi)
{
    int ptot  = p1 + p2;
    int ptot1 = ptot + 1;
    int a, b, i;

    /* Use the caller's workspace, or allocate a private one (main thread only)
     * when wsi is NULL.  Pointing the original locals at the workspace fields
     * keeps the sampler body below unchanged. */
    gbwst  wsl;
    gbwst *ws    = wsi;
    int    owned = 0;
    if (!ws) {
        gbwsallc(&wsl, ptot1, n, family);
        ws    = &wsl;
        owned = 1;
    }
    int    *inc    = ws->inc;
    int    *active = ws->active;
    double *G = ws->G, *Gy = ws->Gy, *Sbuf = ws->Sbuf, *bbuf = ws->bbuf;
    double *D = ws->D, *wq = ws->wq, *wn = ws->wn;
    double *bfull = ws->bfull, *b0 = ws->b0, *bprop = ws->bprop;

    fitctx c;
    c.family = family; c.info = info; c.p0 = p0; c.n = n; c.ptot1 = ptot1;
    c.gamma = gamma; c.y = y; c.X = X; c.pw = pw;
    c.G = c.Gy = NULL; c.Sbuf = c.bbuf = NULL; c.D = c.wq = c.wn = NULL;
    c.bfull = c.b0 = c.bprop = NULL;
    c.yty = 0.0; c.sumlogw = 0.0;

    /* The GLM warm-start coefficient state is reused across blocks, so clear it
     * at the start of each run (a fresh calloc gave the same zero start). */
    if (family != FAM_GAUSSIAN)
        for (a = 0; a < ptot1; a++) bfull[a] = 0.0;

    if (family == FAM_GAUSSIAN) {
        /* Precompute the weighted Gram matrix ONCE for the whole run:
         *   G  = M'WM   ((1+ptot) x (1+ptot)),   M = [1 | X], W = diag(pw)
         *   Gy = M'Wy   (1+ptot),    yty = y'Wy,    sumlogw = sum_i log(w_i).
         * Layout of G: row/col 0 is the intercept, so G[0]=sum w_i, the first
         * row/col holds the weighted column sums sum w_i x_ia, and the interior
         * block holds sum w_i x_ia x_ib (symmetric).  Every later candidate fit
         * reads a sub-block of these arrays -- it never touches X or y again. */
        double yty = 0.0, sumw = 0.0, sumlogw = 0.0;
        for (i = 0; i < n; i++) { sumw += pw[i]; sumlogw += log(pw[i]); }
        G[0] = sumw;                                   /* intercept'W intercept */
        for (a = 0; a < ptot; a++) {
            const double *Xa = X + (size_t) a * n;
            double col = 0.0;
            for (i = 0; i < n; i++) col += pw[i] * Xa[i];
            G[(a + 1) * ptot1] = col;
            G[a + 1] = col;
        }
        for (a = 0; a < ptot; a++) {
            const double *Xa = X + (size_t) a * n;
            for (b = a; b < ptot; b++) {
                const double *Xb = X + (size_t) b * n;
                double s = 0.0;
                for (i = 0; i < n; i++) s += pw[i] * Xa[i] * Xb[i];
                G[(a + 1) * ptot1 + (b + 1)] = s;
                G[(b + 1) * ptot1 + (a + 1)] = s;
            }
        }
        { double sy = 0.0;
          for (i = 0; i < n; i++) { sy += pw[i] * y[i]; yty += pw[i] * y[i] * y[i]; }
          Gy[0] = sy; }
        for (a = 0; a < ptot; a++) {
            const double *Xa = X + (size_t) a * n;
            double s = 0.0;
            for (i = 0; i < n; i++) s += pw[i] * Xa[i] * y[i];
            Gy[a + 1] = s;
        }
        c.G = G; c.Gy = Gy; c.yty = yty; c.sumlogw = sumlogw;
        c.Sbuf = Sbuf; c.bbuf = bbuf;
    } else {
        /* build the block design once: Dfull = [1 | X], n x ptot1 */
        for (i = 0; i < n; i++) D[i] = 1.0;
        for (a = 0; a < ptot; a++)
            memcpy(D + (size_t) (a + 1) * n, X + (size_t) a * n, (size_t) n * sizeof(double));
        c.D = D; c.wq = wq; c.wn = wn;
        c.bfull = bfull; c.b0 = b0; c.bprop = bprop;
    }

    /* On accepting a model, store its fitted coefficients as the current state
     * so the NEXT proposal's IRLS can warm-start from them (glm only; the
     * gaussian path has no iterative fit and needs no coefficient state).  We
     * keep a full-length bfull indexed by block-design column: zero it, then
     * scatter the qc fitted values (bprop) into their active columns. */
    #define COMMIT_BETA(qc)                                              \
        do {                                                             \
            if (family != FAM_GAUSSIAN) {                                \
                for (int _a = 0; _a < ptot1; _a++) bfull[_a] = 0.0;    \
                for (int _r = 0; _r < (qc); _r++)                       \
                    bfull[active[_r]] = bprop[_r];                      \
            }                                                            \
        } while (0)

    /* ----- initial state ----- */
    /* inc[] is the inclusion indicator over all ptot columns: the first p1 are
     * toggleable and seeded from the caller's smod, the trailing p2 (the S2
     * "always-in" block) are forced on and never flipped in the loop below. */
    int nsel = 0;
    for (a = 0; a < p1; a++)    { inc[a] = smod[a] ? 1 : 0; nsel += inc[a]; }
    for (a = p1; a < ptot; a++) { inc[a] = 1; nsel += 1; }

    /* Translate the inclusion vector `inc` into the active-column list expected
     * by modelic: active[0]=0 (intercept), then (column index + 1) for every
     * included predictor; qout is the resulting coefficient count (1 + #incl). */
    #define BUILD_ACTIVE(qout)                                  \
        do {                                                     \
            int _q = 1; active[0] = 0;                           \
            for (int _j = 0; _j < ptot; _j++)                    \
                if (inc[_j]) active[_q++] = _j + 1;              \
            (qout) = _q;                                        \
        } while (0)

    /* Score the initial model; an un-fittable start is given +Inf IC so the
     * first improving proposal is certain to be accepted. */
    int q, ok;
    BUILD_ACTIVE(q);
    double curic = modelic(&c, active, q, &ok);
    if (!ok) curic = R_PosInf; else COMMIT_BETA(q);

    if (ofrq)
        for (a = 0; a < p1; a++) ofrq[a] = 0.0;

    /* Run 2*len sweeps; the first `len` are burn-in (discarded), the second
     * `len` are recorded.  One sweep = p1 single-coordinate flip attempts. */
    int nsweep = 2 * len;
    for (int sw = 0; sw < nsweep; sw++) {
        for (int step = 0; step < p1; step++) {
            /* pick the coordinate to flip: a random scan (perm) draws j
             * uniformly, otherwise sweep the p1 coords in order */
            int j = perm ? (int) (UNIF(rng) * p1) : step;
            if (j >= p1) j = p1 - 1;

            /* reject moves that would empty the model or exceed the size cap */
            int pnsel = nsel + (inc[j] ? -1 : 1);
            if (pnsel < 1 || pnsel > nvars) continue;

            inc[j] ^= 1;                       /* tentatively flip coordinate j */
            int pq;
            BUILD_ACTIVE(pq);
            double propic = modelic(&c, active, pq, &ok);

            /* Metropolis acceptance A = min(1, exp{k*(IC_cur - IC_prop)}):
             * always accept an improvement (IC_prop < IC_cur => A>=1), accept a
             * worsening move with prob shrinking in the IC increase. */
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

        /* record the post-burn-in samples: indicator row, running inclusion
         * counts (for the marginal probabilities), and the current IC */
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

    /* marginal inclusion probability = (times included) / (samples recorded) */
    if (ofrq)
        for (a = 0; a < p1; a++) ofrq[a] /= (double) len;

    #undef BUILD_ACTIVE
    #undef COMMIT_BETA
    if (owned) gbwsfree(&wsl);
    return 0;
}
/*
 * ibgs.c -- pure-C orchestration of the iterated block Gibbs sampler.
 *
 * Ties together the random generator (rng.c) and the independent sampler step
 * (gibbs_step.c).  Contains no SEXP handling: see R_export.c for the thin
 * .Call wrappers.  Uses R's unif_rand() for the serial random draws, so callers
 * must bracket with GetRNGstate()/PutRNGstate().
 *
 * THE ITERATED BLOCK GIBBS SAMPLER (IBGS)
 * ---------------------------------------
 * In ultrahigh dimensions p can be far larger than the sample size n, so a
 * single Gibbs sampler over all p indicators is both slow (each fit can use at
 * most n columns) and noisy.  IBGS instead alternates *screening* and
 * *refinement* to grow a small, stable "important set" S2:
 *
 *   1. SCREEN.  Randomly partition the not-yet-important predictors S1 into h
 *      blocks, each small enough to fit alongside S2 (block size <= n - |S2|).
 *      Run an independent within-block Gibbs sampler on each block (in parallel,
 *      one OpenMP thread per block), always including the S2 columns.  This
 *      yields a marginal inclusion probability for every S1 predictor.
 *   2. SELECT.  Take the top `kapp` S1 predictors by inclusion probability,
 *      union them with S2, and run one combined Gibbs sampler over that union.
 *   3. THRESHOLD.  Promote every predictor whose combined inclusion probability
 *      exceeds `tau` into the new S2.
 *   Repeat 1-3 for `niter` refinement rounds, then do a FINAL screen + a long
 *      Gibbs run over the final candidate set to produce the recorded sample
 *      matrix, the per-sample information criteria, and the marginal inclusion
 *      probabilities returned to R.
 *
 * Reproducibility under parallelism: the random block assignment and a per-block
 * RNG seed are drawn *serially* from R's RNG (drwblks); each block then runs
 * its own xoshiro stream from that seed, so the result does not depend on the
 * thread count or scheduling order.
 */



/* ------------------------------------------------------------------ */
/* Shared search helpers (declared in ibgs.h; used by all three        */
/* *ibgs* orchestrators).                                              */
/* ------------------------------------------------------------------ */

/* Gather the m columns cols[0..m-1] of src (n rows, column-major) into the
 * caller-provided buffer dst (n x m, column-major). */
void gathcols(const double *src, int n, const int *cols, int m, double *dst)
{
    for (int c = 0; c < m; c++)
        memcpy(dst + (size_t) c * n, src + (size_t) cols[c] * n, (size_t) n * sizeof(double));
}

/* fit (value, index) sorted by value descending (ties: smaller index first).
 * Used to rank S1 predictors by inclusion probability and take the top kapp. */
int ficmpdsc(const void *a, const void *b)
{
    const fit *x = (const fit *) a, *y = (const fit *) b;
    if (x->v < y->v) return  1;
    if (x->v > y->v) return -1;
    return (x->idx > y->idx) - (x->idx < y->idx);
}

int intcmp(const void *a, const void *b)
{
    int x = *(const int *) a, y = *(const int *) b;
    return (x > y) - (x < y);
}

/* Allocate every per-search scratch buffer for up to p predictors over n rows.
 * Worst-case sizes (see srwst): everything is bounded by p except the
 * gathered design Xs, which is n*p.  inS2 is zero-initialised (calloc); the rest
 * are fully overwritten before use each iteration.  Returns 1 (freeing any
 * partial allocation) if any allocation fails, else 0. */
int srwsallc(srwst *ws, int p, int n)
{
    int cap = p > 0 ? p : 1;
    int rows = n > 0 ? n : 1;
    ws->capp = cap;
    ws->n    = n;

    ws->inS2   = (int *)      calloc((size_t) cap, sizeof(int));
    ws->S2     = (int *)      malloc((size_t) cap * sizeof(int));
    ws->S1     = (int *)      malloc((size_t) cap * sizeof(int));
    ws->assign = (int *)      malloc((size_t) cap * sizeof(int));
    ws->vfreq  = (double *)   malloc((size_t) cap * sizeof(double));
    ws->seeds  = (uint64_t *) malloc((size_t) cap * sizeof(uint64_t));
    ws->arr    = (fit *)      malloc((size_t) cap * sizeof(fit));
    ws->xs     = (int *)      malloc((size_t) cap * sizeof(int));
    ws->s0     = (int *)      malloc((size_t) cap * sizeof(int));
    ws->fr     = (double *)   malloc((size_t) cap * sizeof(double));
    ws->Xs     = (double *)   malloc((size_t) rows * cap * sizeof(double));

    if (!ws->inS2 || !ws->S2 || !ws->S1 || !ws->assign || !ws->vfreq ||
        !ws->seeds || !ws->arr || !ws->xs || !ws->s0 || !ws->fr || !ws->Xs) {
        srwsfree(ws);
        return 1;
    }
    return 0;
}

void srwsfree(srwst *ws)
{
    free(ws->inS2);
    free(ws->S2);
    free(ws->S1);
    free(ws->assign);
    free(ws->vfreq);
    free(ws->seeds);
    free(ws->arr);
    free(ws->xs);
    free(ws->s0);
    free(ws->fr);
    free(ws->Xs);
}

/*
 * One screening step: split the candidate columns S1 into `h` blocks and run a
 * within-block Gibbs sampler in parallel; the fixed columns S2 are always
 * included.  Writes the marginal inclusion probability of every S1 column into
 * vfreq[] at its original column position.  Returns 0 on success, 1 on failure.
 */
static int scrblks(const double *y, const double *X, const double *pw, int n, const int *S1, int nS1, const int *S2, int nS2, int h, int perm, int len, double k, double gamma, int p0, int info, int family, int nthr, const int *assign, const uint64_t *seeds, double *vfreq)
{
    /* group the S1 positions by block: pos[off[b] .. off[b]+sz[b]-1] */
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

    /* Size every per-thread workspace for the largest block (its predictors plus
     * the nS2 always-in columns plus the intercept). */
    int maxpb = 0;
    for (int b = 0; b < h; b++) if (sz[b] > maxpb) maxpb = sz[b];
    int capt = maxpb + nS2 + 1;

    /* One reusable workspace per thread, allocated up front on the main thread
     * (R_Calloc is not thread-safe), so the parallel loop performs no
     * allocation; block b uses wsa[omp_get_thread_num()]. */
    int nws = nthr > 0 ? nthr : 1;
#ifndef _OPENMP
    nws = 1;
#endif
    gbwst *wsa = R_Calloc((size_t) nws, gbwst);
    for (int t = 0; t < nws; t++) gbwsallc(&wsa[t], capt, n, family);

    /* Run the h blocks in parallel: blocks are independent (each owns its own
     * thread workspace and RNG; the only shared writes are to disjoint positions
     * of vfreq), so no locking is needed.  schedule(dynamic) balances uneven
     * block sizes; `fail` is set via atomic write on any block's error. */
    int fail = 0;
#ifdef _OPENMP
    #pragma omp parallel for num_threads(nthr) schedule(dynamic) shared(fail)
#endif
    for (int b = 0; b < h; b++) {
        int pb = sz[b];                       /* predictors in this block */
        if (pb <= 0) continue;

#ifdef _OPENMP
        gbwst *ws = &wsa[omp_get_thread_num()];
#else
        gbwst *ws = &wsa[0];
#endif
        int    *bcols = ws->bcols;
        int    *s0    = ws->s0;
        double *fr    = ws->fr;
        double *Xb    = ws->Xb;

        /* this block's design = [its pb S1 columns (toggleable) | the nS2 fixed
         * S2 columns]; s0 = 1 starts every toggleable column included.  Gather
         * the columns into the workspace's reusable buffer. */
        for (int c = 0; c < pb; c++) {
            bcols[c] = S1[pos[off[b] + c]];
            s0[c]    = 1;
        }
        for (int c = 0; c < nS2; c++) bcols[pb + c] = S2[c];
        for (int c = 0; c < pb + nS2; c++)
            memcpy(Xb + (size_t) c * n, X + (size_t) bcols[c] * n, (size_t) n * sizeof(double));

        /* per-block private RNG (thread-safe), seeded deterministically; the
         * within-block sampler reports only the inclusion frequencies `fr` */
        rngt rng;
        rngseed(&rng, seeds[b]);
        int rc = rungibbs(y, Xb, pw, n, pb, nS2, s0, perm, len, k, gamma,
                          p0, info, family, pb + nS2, &rng, NULL, fr, NULL, ws);
        if (rc) {
#ifdef _OPENMP
            #pragma omp atomic write
#endif
            fail = 1;
        } else {
            for (int c = 0; c < pb; c++)
                vfreq[S1[pos[off[b] + c]]] = fr[c];   /* distinct indices: no race */
        }
    }

    for (int t = 0; t < nws; t++) gbwsfree(&wsa[t]);
    R_Free(wsa);
    R_Free(sz);
    R_Free(off);
    R_Free(pos);
    R_Free(cur);
    return fail;
}

/* Number of blocks = ceil(nS1 / block_size), block_size = min(H, n - nS2).
 * The block size is capped at n - nS2 so that a block's design (its columns plus
 * the nS2 fixed columns) stays within the n rows and the fits remain full-rank;
 * H is the user's preferred cap.  At least one block. */
static int nblks(int nS1, int H, int n, int nS2)
{
    int bs = (H < n - nS2) ? H : (n - nS2);
    if (bs < 1) bs = 1;
    int h = (nS1 + bs - 1) / bs;
    return (h < 1) ? 1 : h;
}

/*
 * Draw, serially from R's RNG, (a) a block label in [0,h) for each of the nS1
 * candidates and (b) one 64-bit seed per block.  Doing this serially on the main
 * thread is what makes the parallel screen reproducible: the seeds are fixed
 * before any thread starts.  Each seed combines two 32-bit uniform draws with a
 * golden-ratio * (b+1) term so distinct blocks get well-separated seeds even if
 * the uniform draws happen to collide.
 */
static void drwblks(int nS1, int h, int *assign, uint64_t *seeds)
{
    for (int i = 0; i < nS1; i++) {
        int a = (int) (unif_rand() * h);
        assign[i] = (a >= h) ? h - 1 : a;       /* guard the unif_rand()==1 edge */
    }
    for (int b = 0; b < h; b++) {
        uint64_t hi = (uint64_t) (unif_rand() * 4294967296.0);   /* 2^32 */
        uint64_t lo = (uint64_t) (unif_rand() * 4294967296.0);
        seeds[b] = (hi << 32) ^ lo ^ (0x9E3779B97F4A7C15ULL * (uint64_t) (b + 1));
    }
}

/* ------------------------------------------------------------------ */
/* Public entry points.                                               */
/* ------------------------------------------------------------------ */

int gibbssam(const double *y, const double *X, const double *pw, int n, int p, int nvars, int perm, int len, double k, double gamma, int info, int family, int *mbuf, double *sicbuf, double *vpbuf)
{
    if (nvars < 1) nvars = 1;
    if (nvars > p) nvars = p;

    /* standalone sampler: all p predictors are toggleable (p2 = 0), the model
     * is initialised to the first nvars columns, and the size is capped at
     * nvars.  rng = NULL means it uses R's RNG on the main thread (serial). */
    int *s0 = (int *) malloc((size_t) p * sizeof(int));
    if (!s0) return 1;
    for (int i = 0; i < p; i++) s0[i] = (i < nvars) ? 1 : 0;   /* first nvars in */

    int fail = rungibbs(y, X, pw, n, p, 0, s0, perm, len, k, gamma, p, info,
                        family, nvars, /*rng=*/NULL, mbuf, vpbuf, sicbuf,
                        /*ws=*/NULL);
    free(s0);
    return fail;
}

int ibgssel(const double *y, const double *X, const double *pw, int n, int p, int niter, int H, int kapp, double tau, int perm, int len, double k, double gamma, int info, int family, int nthr, int *xsout, int *psout, int *lfout)
{
    int p0 = p;

#ifdef _OPENMP
    if (nthr <= 0) nthr = omp_get_max_threads();
#endif

    /* one per-search workspace, reused across every iteration and the final
     * screening (the long run that records the outputs is done by ibgsrun) */
    srwst ws;
    if (srwsallc(&ws, p, n)) return 1;
    int    *inS2   = ws.inS2;
    int    *S2     = ws.S2;
    int    *S1     = ws.S1;
    double *vfreq  = ws.vfreq;
    int    *assign = ws.assign;

    /* inS2[j] marks predictor j as currently in the important set; S2 lists those
     * indices, S1 lists the rest (the screening candidates).  nS2 starts at 0. */
    int nS2 = 0, fail = 0;

    /* ---- refinement iterations (screen -> select -> threshold) ---- */
    for (int iter = 1; iter < niter && !fail; iter++) {
        /* S1 = all predictors not yet in S2 */
        int nS1 = 0;
        for (int j = 0; j < p; j++) if (!inS2[j]) S1[nS1++] = j;

        /* SCREEN: split S1 into h blocks, sample each in parallel, get vfreq[] */
        int h = nblks(nS1, H, n, nS2);
        drwblks(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = scrblks(y, X, pw, n, S1, nS1, S2, nS2, h, perm, len, k,
                       gamma, p0, info, family, nthr, assign, ws.seeds, vfreq);
        if (fail) break;

        /* SELECT: the top-kapp S1 predictors by inclusion frequency, unioned
         * with S2, sorted ascending -> the candidate set xs of size ps */
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

        /* combined Gibbs over xs (all ps columns toggleable) -> fr = inclusion
         * probability of each candidate */
        double *Xs = ws.Xs;
        int    *s0 = ws.s0;
        double *fr = ws.fr;
        gathcols(X, n, xs, ps, Xs);
        for (int i = 0; i < ps; i++) s0[i] = 1;
        fail = rungibbs(y, Xs, pw, n, ps, 0, s0, perm, len, k, gamma, p0,
                        info, family, ps, NULL, NULL, fr, NULL, NULL);
        if (fail) break;

        /* THRESHOLD: rebuild S2 from the candidates with fr > tau.  If no
         * candidate clears tau (cnt <= 1) we keep them all rather than empty S2,
         * so the important set never collapses to nothing between iterations. */
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
     * One more screen+select on the converged S2 fixes the final candidate set
     * xs (size ps) and the long-run length lenf = 4*len.  The long Gibbs run that
     * records the R outputs is done separately by ibgsrun(), so this kernel
     * touches no R objects. */
    *lfout = 4 * len;
    int ps = 0;

    if (!fail) {
        int nS1 = 0;
        for (int j = 0; j < p; j++) if (!inS2[j]) S1[nS1++] = j;

        int h = nblks(nS1, H, n, nS2);
        drwblks(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = scrblks(y, X, pw, n, S1, nS1, S2, nS2, h, perm, len,
                       k, gamma, p0, info, family, nthr, assign, ws.seeds,
                       vfreq);

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

/* Final long Gibbs run of the iterated block Gibbs search (the fill phase behind
 * the ibgs_glm() .Call wrapper).  Given the converged candidate columns xs (the
 * ps 0-based original indices from ibgssel) it runs a length-lenf Gibbs sampler
 * over those columns and writes the results straight into the caller's buffers --
 * no R/SEXP handling here.
 *   xs    : the ps candidate columns (0-based), as returned by ibgssel.
 *   ps    : number of candidate columns; lenf : recorded sweeps (= 4*len).
 *   omat  : OUTPUT int[lenf * (1+ps)] indicator matrix (column-major).
 *   oic   : OUTPUT double[lenf] per-sample information criterion.
 *   vprob : OUTPUT double[p] marginal inclusion prob over ALL p columns (zeroed
 *           here, then the ps selected positions are scattered in).
 *   sel   : OUTPUT int[ps] 1-based original indices of the candidate columns.
 * Allocates its gather/run scratch with R_Calloc/R_Free (main thread); returns 0
 * on success, 1 on a numerical failure. */
int ibgsrun(const double *y, const double *X, const double *pw, int n, int p, const int *xs, int ps, int lenf, int perm, double k, double gamma, int info, int family, int *omat, double *oic, double *vprob, int *sel)
{
    double *Xs = R_Calloc((size_t) n * ps, double);
    int    *s0 = R_Calloc((size_t) ps, int);
    double *fr = R_Calloc((size_t) ps, double);

    gathcols(X, n, xs, ps, Xs);
    for (int i = 0; i < ps; i++) s0[i] = 1;
    int fail = rungibbs(y, Xs, pw, n, ps, 0, s0, perm, lenf, k, gamma, p, info, family, ps, NULL, omat, fr, oic, NULL);

    if (!fail) {
        /* marginal probs over all p columns: zero, then scatter the ps selected
         * (R is 1-based, so the recorded indices are xs[i] + 1) */
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

/* glmcoef -- see ibgs.h for the full contract.  Builds the shared design
 * D = [1|X] once, then dispatches: the gaussian family assembles the weighted
 * Gram G = D'WD and cross-product D'Wy and solves with cholsolv(); the other
 * families call glmirls() on the same design.  A final pass zeros any non-finite
 * coefficient so the caller never sees NaN.  Scratch is R_Calloc/R_Free. */
void glmcoef(const double *y, const double *X, const double *pw, int n, int q, int family, double *bout)
{
    int p1 = q + 1;
    for (int a = 0; a < p1; a++) bout[a] = 0.0;

    /* shared design D = [1 | X], n x p1 (column-major); active = all columns */
    double *D = R_Calloc((size_t) n * p1, double);
    for (int i = 0; i < n; i++) D[i] = 1.0;
    for (int a = 0; a < q; a++) memcpy(D + (size_t)(a + 1) * n, X + (size_t) a * n, (size_t) n * sizeof(double));
    int *active = R_Calloc((size_t) p1, int);
    for (int a = 0; a < p1; a++) active[a] = a;

    if (family == FAM_GAUSSIAN) {
        /* weighted OLS: solve (D'WD) beta = D'Wy by Cholesky */
        double *G  = R_Calloc((size_t) p1 * p1, double);
        double *Gy = R_Calloc((size_t) p1, double);
        for (int a = 0; a < p1; a++) {
            const double *Daptr = D + (size_t) a * n;
            double sgy = 0.0;
            for (int i = 0; i < n; i++) sgy += Daptr[i] * pw[i] * y[i];
            Gy[a] = sgy;
            for (int c = a; c < p1; c++) {
                const double *Dcptr = D + (size_t) c * n;
                double s = 0.0;
                for (int i = 0; i < n; i++) s += Daptr[i] * pw[i] * Dcptr[i];
                G[a * p1 + c] = s;
                G[c * p1 + a] = s;
            }
        }
        if (cholsolv(G, Gy, bout, p1)) for (int a = 0; a < p1; a++) bout[a] = 0.0;
        R_Free(G);
        R_Free(Gy);
    } else {
        double *wq = R_Calloc((size_t) p1 * p1 + 2 * p1, double);
        double *wn = R_Calloc((size_t) 4 * n, double);
        double dev2;
        if (glmirls(family, y, pw, D, active, n, p1, wq, wn, &dev2, NULL, bout)) for (int a = 0; a < p1; a++) bout[a] = 0.0;
        R_Free(wq);
        R_Free(wn);
    }

    /* a rank-deficient refit can slip past the pivot guard and yield non-finite
     * coefficients (NaN compares false against the tolerance); return zeros so
     * the caller never propagates NaN into predictions. */
    for (int a = 0; a < p1; a++) {
        if (!R_FINITE(bout[a])) {
            for (int c = 0; c < p1; c++) bout[c] = 0.0;
            break;
        }
    }

    R_Free(D);
    R_Free(active);
}

/* ============================================================================
 * Model-averaging summary (moved from R's former .fit.summary).  After a sampler
 * run has filled the per-generation indicator matrix `omat` (column-major,
 * lenf x (1+ps)) and the per-sample criterion `oic` (length lenf), these helpers
 * tabulate the recorded models, keep the best nm distinct criterion values, and
 * refit a representative of each, so the .Call wrapper returns a compact summary
 * (coef / model.ic / model.freq) instead of the whole indicator matrix.  summnm
 * and summtab are family-independent and shared with cox.c / rlm.c.
 * ========================================================================== */

/* (criterion, sample-index) pair sorted by (ic asc, idx asc): within a group of
 * equal criterion the LAST element then has the largest original index, matching
 * R's stable order()/cumsum() representative pick. */
typedef struct { double ic; int idx; } sicit;

/* qsort comparator: sicit by ic ascending, ties broken by idx ascending. */
static int siccmp(const void *a, const void *b)
{
    const sicit *x = (const sicit *) a;
    const sicit *y = (const sicit *) b;
    if (x->ic < y->ic) return -1;
    if (x->ic > y->ic) return  1;
    return (x->idx > y->idx) - (x->idx < y->idx);
}

/* qsort comparator: plain double ascending. */
static int dblcmp(const void *a, const void *b)
{
    double x = *(const double *) a;
    double y = *(const double *) b;
    return (x > y) - (x < y);
}

/* Round a criterion to 15 significant digits, reproducing R's as.character()
 * (and hence table()) so that two recorded samples of the SAME model are treated
 * as one "distinct" value -- their criteria can differ in the last bits because
 * the warm-started fits (Cox Newton, IRLS) converge to slightly different
 * coefficients.  Grouping by this key matches the former R-side .fit.summary. */
static double round15(double x)
{
    char buf[32];
    snprintf(buf, sizeof buf, "%.15g", x);
    return strtod(buf, NULL);
}

/* Number of retained models = min(n_req, #distinct criterion values in
 * oic[0..lenf-1]).  The wrapper calls this to size the summary outputs exactly
 * before calling the family summarizer.  Returns -1 on allocation failure. */
int summnm(const double *oic, int lenf, int n_req)
{
    double *a = (double *) malloc((size_t) lenf * sizeof(double));
    if (!a) return -1;
    for (int s = 0; s < lenf; s++) a[s] = round15(oic[s]);
    qsort(a, (size_t) lenf, sizeof(double), dblcmp);
    int d = 0;
    int i = 0;
    while (i < lenf) {
        int j = i;
        while (j < lenf && a[j] == a[i]) j++;
        d++;
        i = j;
    }
    free(a);
    return d < n_req ? d : n_req;
}

/* Tabulate oic into ascending distinct groups and report the first nm_in:
 *   micic[g] = the g-th smallest distinct criterion value,
 *   cnt[g]   = how many samples attained it,
 *   rep[g]   = a representative sample index (the group's last element in
 *              (ic asc, idx asc) order = R's order()/cumsum() pick).
 * Writes *nm = min(nm_in, #distinct) groups; the caller sizes the buffers to
 * nm_in.  Returns 0, or 1 on allocation failure. */
int summtab(const double *oic, int lenf, int nm_in, double *micic, int *cnt, int *rep, int *nm)
{
    sicit *a = (sicit *) malloc((size_t) lenf * sizeof(sicit));
    if (!a) return 1;
    for (int s = 0; s < lenf; s++) {
        a[s].ic  = oic[s];
        a[s].idx = s;
    }
    qsort(a, (size_t) lenf, sizeof(sicit), siccmp);
    int g = 0;
    int i = 0;
    while (i < lenf && g < nm_in) {
        double key = round15(a[i].ic);   /* 15-sig-digit group key (R table()) */
        int j = i;
        while (j < lenf && round15(a[j].ic) == key) j++;
        micic[g] = key;
        cnt[g]   = j - i;
        rep[g]   = a[j - 1].idx;
        g++;
        i = j;
    }
    *nm = g;
    free(a);
    return 0;
}

/* GLM model-averaging summary: refit the best nm models from the recorded run and
 * write the compact outputs.  omat is the column-major lenf x (1+ps) indicator
 * matrix ([intercept | ps predictor columns]); xs[c] is the 0-based original
 * column of omat predictor column c+1; coef is the full nr x nm_in matrix
 * (nr = p+1, intercept in row 0, zeros at non-active rows); micout/frqout are
 * length nm_in.  Refits with glmcoef.  Writes *nm models.  Returns 0, or 1 on
 * allocation failure. */
int glmsumm(const double *y, const double *X, const double *pw, int n, int p, const int *xs, int ps, const int *omat, const double *oic, int lenf, int family, int nm_in, double *coef, double *micout, double *frqout, int *nm)
{
    int nr = p + 1;
    int cap = ps > 0 ? ps : 1;
    int *cnt = (int *) malloc((size_t) nm_in * sizeof(int));
    int *rep = (int *) malloc((size_t) nm_in * sizeof(int));
    int *act = (int *) malloc((size_t) cap * sizeof(int));
    double *Xact = (double *) malloc((size_t) n * cap * sizeof(double));
    double *bout = (double *) malloc((size_t) (ps + 1) * sizeof(double));
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
        glmcoef(y, Xact, pw, n, q, family, bout);
        coef[(size_t) i * nr] = bout[0];
        for (int j = 0; j < q; j++)
            coef[(size_t)(act[j] + 1) + (size_t) i * nr] = bout[j + 1];
        frqout[i] = (double) cnt[i] / (double) lenf;
    }
    free(cnt);
    free(rep);
    free(act);
    free(Xact);
    free(bout);
    return 0;
}
