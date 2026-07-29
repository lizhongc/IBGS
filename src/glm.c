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

#define USE_FC_LEN_T
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>

/* FCONE passes the hidden Fortran string-length arguments to the BLAS character
 * flags (no-op on toolchains without USE_FC_LEN_T). */
#ifndef FCONE
# define FCONE
#endif

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
#define IRLS_TOL   1e-8  /* relative deviance change treated as convergence,    */
                         /* matching R's glm() default (glm.control epsilon)    */
#define MU_EPS     1e-10 /* clamp keeping mu inside (0,1) / (0,inf) so logs and */
                         /* divisions stay finite at the boundary of the range  */

/*
 * Solve the symmetric positive-definite system A x = b by Cholesky.
 *
 * dpotrf then dpotrs (LAPACK) Cholesky-factor A = U'U and solve A x = b for it.
 * dpotrf is LAPACK's Cholesky factorisation of a symmetric positive-definite
 * matrix; dpotrs applies that factor to solve the linear system.  The package
 * calls them here because every candidate GLM (IRLS) and lme fit reduces to the
 * q x q normal equations A beta = b (A = D'WD, b = D'Wz); this routine is that
 * solve, shared by the glmirls, glmcoef and lmecoef paths.
 *
 * Argument map.  A is the q x q normal matrix held with its lower triangle in
 * row-major order, which is byte-for-byte the upper triangle in column-major
 * order, so we pass UPLO="U" and leading dimension q -- no transpose or copy,
 * and it matches how glmirls fills A with dsyrk(UPLO='U').  dpotrf overwrites A's
 * (upper) triangle with the factor U and returns the order of the first non-PD
 * leading minor in its info status (0 = success).  dpotrs takes the number of
 * right-hand sides nrhs=1, the same A and leading dimension q, and overwrites its
 * right-hand side -- we copy b into x first and pass x, which comes back holding
 * the solution.  FCONE passes the hidden Fortran length of the "U" flag.
 *
 * The factorisation doubles as a rank check: a non-zero info, or any pivot with
 * U[r,r]^2 <= 1e-12, means the active columns are collinear/duplicated, so we
 * return 1 and the caller rejects the model rather than divide by ~0.  The 1e-12
 * pivot floor reproduces the previous hand-rolled solver's singularity guard.
 *
 * Equivalent R operation: x <- solve(A, b) for symmetric positive-definite A
 * (chol() followed by forward/back substitution).
 * Netlib references: LAPACK dpotrf, dpotrs.
 */
int cholsolv(double *A, const double *b, double *x, int q)
{
    int r, info, one = 1;
    F77_CALL(dpotrf)("U", &q, A, &q, &info FCONE);
    if (info != 0) return 1;                       /* not PD -> singular model    */
    for (r = 0; r < q; r++)
        if (A[r * q + r] <= 1e-6) return 1;        /* pivot^2 <= 1e-12: collinear */
    for (r = 0; r < q; r++) x[r] = b[r];
    F77_CALL(dpotrs)("U", &q, &one, A, &q, x, &q, &info FCONE);
    return 0;
}

/*
 * The MODEL-DEPENDENT part of -2*logLik for the converged means.  The full
 * -2*logLik that matches R's glm() aic component (so AIC = -2logLik + 2*npar
 * reproduces R's AIC()) splits into
 *
 *     -2logLik  =  glmm2ll(mu)  +  glmllconst(y, pw)
 *
 * where glmm2ll() holds the terms that depend on the fitted means mu (and so
 * change with every candidate model) and glmllconst() holds the data-only
 * normalising constant (the binomial coefficient / poisson log(y!)) that is the
 * same for every model.  Splitting them lets the constant be precomputed once
 * per run -- mirroring the gaussian path's sumlogw -- instead of being
 * recomputed (3 lgamma/obs for binomial, 1 for poisson) on every candidate fit.
 * The caller (modelic) adds the precomputed constant back before forming the IC.
 *
 *   binomial: pw[i] are the trial counts m_i and y[i] the success *proportion*,
 *     so successes s_i = m_i*y_i, rounded (floor(x+0.5)) to match how R forms
 *     the binomial response.  Model-dependent terms:
 *        s_i log(mu_i) + (m_i - s_i) log(1 - mu_i).
 *   poisson: model-dependent terms pw[i]*( y_i log(mu_i) - mu_i ).
 *
 * Returns -2 * sum of the model-dependent per-observation log-likelihood terms.
 */
static double glmm2ll(int family, const double *y, const double *mu, const double *pw, int n)
{
    double ll = 0.0;
    int i;
    if (family == FAM_BINOMIAL)
    {
        for (i = 0; i < n; i++)
        {
            double m = floor(pw[i] + 0.5);          /* trials    */
            double s = floor(pw[i] * y[i] + 0.5);   /* successes */
            double t = 0.0;
            if (s > 0.0)     t += s * log(mu[i]);
            if (m - s > 0.0) t += (m - s) * log(1.0 - mu[i]);
            ll += t;
        }
    }
    else /* poisson */
    {
        for (i = 0; i < n; i++)
            ll += pw[i] * (y[i] * log(mu[i]) - mu[i]);
    }
    return -2.0 * ll;
}

/*
 * The DATA-ONLY normalising constant of -2*logLik (see glmm2ll): the part that
 * depends only on the response y and the weights pw, not on the fitted means, so
 * it is identical for every candidate model and is computed once per run.
 *   binomial: -2 * sum_i [ lgamma(m_i+1) - lgamma(s_i+1) - lgamma(m_i-s_i+1) ]
 *   poisson : -2 * sum_i [ -pw_i * lgamma(y_i+1) ]  =  2 * sum_i pw_i*lgamma(y_i+1)
 * For binary data (m_i = 1) the binomial term is exactly 0.  Only called for the
 * binomial/poisson families (the gaussian path uses sumlogw instead).
 */
static double glmllconst(int family, const double *y, const double *pw, int n)
{
    double c = 0.0;
    int i;
    if (family == FAM_BINOMIAL)
    {
        for (i = 0; i < n; i++)
        {
            double m = floor(pw[i] + 0.5);          /* trials    */
            double s = floor(pw[i] * y[i] + 0.5);   /* successes */
            c += lgamma(m + 1.0) - lgamma(s + 1.0) - lgamma(m - s + 1.0);
        }
    }
    else /* poisson */
    {
        for (i = 0; i < n; i++)
            c += -pw[i] * lgamma(y[i] + 1.0);
    }
    return -2.0 * c;
}

int glmirls(int family, const double *y, const double *pw, const double *Dfull, const int *active, int n, int q, int maxit, double *wq, double *wn, double *Dpack, double *Dw, double *dev2, const double *b0, double *bout, const double *cwz)
{
    double *XtWX    = wq;                  /* q x q */
    double *XtWz    = wq + q * q;          /* q     */
    double *beta    = wq + q * q + q;      /* q     */
    double *betaold = wq + q * q + 2 * q;  /* q  (previous iterate, convergence) */
    double *eta = wn;             /* n */
    double *mu  = wn + n;         /* n */
    double *w   = wn + 2 * n;     /* n */
    double *z   = wn + 3 * n;     /* n */
    double *u   = wn + 4 * n;     /* n  (u = w*z, the syrk/gemv working response) */

    int i, a, it;
    const double d_one = 1.0, d_zero = 0.0;

    /* Cached first-iteration weights, when the caller has them (see the header):
     * the working response u = w*z that the gemv needs, and the sqrt(w) the syrk
     * design is scaled by.  These are the only two quantities the first iteration
     * takes from eta, so a cached first step needs nothing else. */
    const double *cu  = cwz;
    const double *csw = cwz ? cwz + n : NULL;

    /* Gather the q active columns of the shared block design Dfull = [1|X] into a
     * contiguous n x q scratch ONCE per fit (column 0 = active[0] = intercept).
     * The IRLS iterations then stream over this packed, cache-friendly buffer
     * instead of the strided, active[]-indirected columns of Dfull, and it is the
     * matrix the BLAS kernels below operate on. */
    for (a = 0; a < q; a++)
        memcpy(Dpack + (size_t) a * n, Dfull + (size_t) active[a] * n, (size_t) n * sizeof(double));

    /* Initial linear predictor.
     *   warm start (b0 != NULL): eta = D b0 starts IRLS from a nearby model's
     *     coefficients, so it converges in far fewer steps (see header);
     *   cold start: the usual GLM starting values -- a smoothed empirical logit
     *     for binomial, log(y+0.1) for poisson -- which keep eta finite even for
     *     y at the boundary. */
    if (cwz != NULL)
    {
        /* the cache already carries everything the first iteration derives from
         * eta, and eta is overwritten from the Newton step before it is read
         * again, so the warm-start pass has nothing left to compute */
    }
    else if (b0 != NULL)
    {
        for (i = 0; i < n; i++) eta[i] = 0.0;
        for (a = 0; a < q; a++)
        {
            const double *Da = Dpack + (size_t) a * n;
            double ba = b0[a];
            for (i = 0; i < n; i++) eta[i] += Da[i] * ba;
        }
    }
    else
    {
        for (i = 0; i < n; i++)
        {
            if (family == FAM_BINOMIAL)
            {
                double m = (pw[i] * y[i] + 0.5) / (pw[i] + 1.0);
                eta[i] = log(m / (1.0 - m));
            }
            else
            {
                eta[i] = log(y[i] + 0.1);
            }
        }
    }

    for (it = 0; it < maxit; it++)
    {
        /* Step 1+2: from the current eta compute, per observation, the mean mu,
         * the link derivative mueta = dmu/deta, the variance function var =
         * V(mu), the IRLS weight w = pw * mueta^2 / var, and the working
         * response z = eta + (y - mu)/mueta.  For both families mueta == var,
         * so w collapses to pw*var.  The clamps keep mu off the open boundary
         * (and bound eta for poisson) so exp/log never overflow or divide by 0. */
        int cached = (cwz != NULL && it == 0);
        if (!cached)
        {
            for (i = 0; i < n; i++)
            {
                double e = eta[i], m, mueta, var;
                if (family == FAM_BINOMIAL)
                {
                    m = 1.0 / (1.0 + exp(-e));                 /* logit^{-1} */
                    if (m < MU_EPS)
                        m = MU_EPS;
                    else if (m > 1.0 - MU_EPS)
                        m = 1.0 - MU_EPS;
                    mueta = m * (1.0 - m);                     /* dmu/deta = mu(1-mu) */
                    var   = m * (1.0 - m);                     /* V(mu)    = mu(1-mu) */
                }
                else /* poisson */
                {
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
        }

        /* Step 3: assemble the weighted normal equations of the WLS update with
         * BLAS on the packed design.  With Dw = sqrt(w) .* Dpack and u = w .* z,
         *   XtWX = D'WD = Dw'Dw   -> dsyrk (a rank-n symmetric update), and
         *   XtWz = D'Wz = Dpack'u -> dgemv.
         * dsyrk(UPLO='U', TRANS='T') fills the col-major upper triangle of XtWX,
         * which is byte-for-byte the row-major lower triangle cholsolv() reads,
         * so no separate symmetrise step is needed. */
        const double *wuse = cached ? cu : u;               /* D'Wz right-hand side */
        for (i = 0; i < n; i++)
        {
            double sw = cached ? csw[i] : sqrt(w[i]);
            if (!cached) u[i] = w[i] * z[i];
            for (a = 0; a < q; a++)
                Dw[(size_t) a * n + i] = sw * Dpack[(size_t) a * n + i];
        }
        F77_CALL(dsyrk)("U", "T", &q, &n, &d_one, Dw, &n, &d_zero, XtWX, &q FCONE FCONE);
        int inc1 = 1;
        F77_CALL(dgemv)("T", &n, &q, &d_one, Dpack, &n, wuse, &inc1, &d_zero, XtWz, &inc1 FCONE);

        /* solve D'WD beta = D'Wz; a singular system means a collinear model */
        if (cholsolv(XtWX, XtWz, beta, q)) return 1;   /* singular */

        for (i = 0; i < n; i++) eta[i] = 0.0;             /* eta = D beta */
        for (a = 0; a < q; a++)
        {
            const double *Da = Dpack + (size_t) a * n;
            double ba = beta[a];
            for (i = 0; i < n; i++) eta[i] += Da[i] * ba;
        }

        /* Convergence test on the coefficient update: stop once the relative
         * change in beta falls below tolerance (small absolute floor for the
         * near-zero case).  This replaces the per-iteration deviance recompute,
         * removing an O(n) log pass each iteration.  Skipped on it==0 (no
         * previous beta); never reached when maxit==1 (the one-step fast
         * proposal). */
        if (it > 0)
        {
            double num = 0.0, den = 0.0;
            for (a = 0; a < q; a++)
            {
                double d = beta[a] - betaold[a];
                num += d * d;
                den += beta[a] * beta[a];
            }
            if (sqrt(num) < IRLS_TOL * (sqrt(den) + 1e-8)) break;
        }
        for (a = 0; a < q; a++) betaold[a] = beta[a];
    }

    /* Recompute the converged means from the final eta (the loop may have exited
     * on the convergence test before refreshing mu) and report the
     * model-dependent part of -2logLik; the caller adds the data-only constant
     * (glmllconst) before forming the IC. */
    for (i = 0; i < n; i++)
    {
        double e = eta[i], m;
        if (family == FAM_BINOMIAL)
        {
            m = 1.0 / (1.0 + exp(-e));
            if (m < MU_EPS)
                m = MU_EPS;
            else if (m > 1.0 - MU_EPS)
                m = 1.0 - MU_EPS;
        }
        else
        {
            if (e >  30.0) e =  30.0;
            if (e < -30.0) e = -30.0;
            m = exp(e);
            if (m < MU_EPS) m = MU_EPS;
        }
        mu[i] = m;
    }
    *dev2 = glmm2ll(family, y, mu, pw, n);
    if (bout)
        for (a = 0; a < q; a++) bout[a] = beta[a];
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
    switch (info)
    {
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
    int r, s;

    /* (1) copy the active sub-block S and sub-vector b out of G, Gy */
    for (r = 0; r < q; r++)
    {
        int ar = active[r];
        bbuf[r] = Gy[ar];
        for (s = 0; s < q; s++)
            Sbuf[r * q + s] = G[ar * ptot1 + active[s]];
    }

    /* (2) Cholesky-factor S = U'U and solve S beta = b in place (bbuf <- beta).
     *
     * dpotrf (LAPACK) Cholesky-factors the symmetric positive-definite S; dpotrs
     * applies that factor to solve the q x q system.  We call them because each
     * candidate gaussian/lme fit reduces to these whitened normal equations.
     * S is gathered with both triangles populated, so passing UPLO="U" with
     * leading dimension q factors it with no transpose; dpotrf overwrites S's
     * upper triangle with the factor U and reports a non-positive-definite leading
     * minor in info.  dpotrs takes nrhs=1 and overwrites its right-hand side, so
     * bbuf (which already holds b = Gy[active]) comes back holding beta.  FCONE
     * passes the hidden Fortran length of "U".  A non-zero info, or any pivot with
     * U[r,r]^2 <= 1e-10, marks collinear active columns and we return -1 so the
     * caller rejects the model; the 1e-10 floor matches the former hand solver.
     *
     * Equivalent R operation: beta <- solve(S, b) for symmetric positive-definite S.
     * Netlib references: LAPACK dpotrf, dpotrs. */
    int info, one = 1;
    F77_CALL(dpotrf)("U", &q, Sbuf, &q, &info FCONE);
    if (info != 0) return -1.0;                    /* not PD -> singular model     */
    for (r = 0; r < q; r++)
        if (Sbuf[r * q + r] <= 1e-5) return -1.0;  /* pivot^2 <= 1e-10: collinear  */
    F77_CALL(dpotrs)("U", &q, &one, Sbuf, &q, bbuf, &q, &info FCONE);
    /* (3) RSS = y'Wy - beta' M_A'Wy */
    double fdot = 0.0;
    for (r = 0; r < q; r++) fdot += Gy[active[r]] * bbuf[r];
    double rss = yty - fdot;
    return (rss < 1e-12) ? 1e-12 : rss;
}

/* Per-run context bundling everything modelic() needs for either path. */
typedef struct {
    int family, info, p0, n, ptot1;
    double gamma;
    /* gaussian */
    const double *G, *Gy;
    double yty, sumlogw;
    double *Sbuf, *bbuf;
    /* glm (binomial/poisson) */
    const double *y, *X, *pw;
    double *D, *wq, *wn, *Dpack, *Dw;
    double *bfull, *b0, *bprop;   /* warm-start coefficients */
    double m2ll_const;            /* data-only -2logLik constant, precomputed once */
    double *cwz;                  /* shared first-step weights for addition proposals */
    /* glm batched add-scoring (irlsbatch / irlssafe / irlsic) */
    double *Gpan, *Gfac, *Gwb, *Ggd, *Ggv, *Gs2, *Gcj, *GDws, *Getah, *Getac, *Gmuc;
    int    *Gact;                 /* active set the panel was built from */
    int    Gnq;                   /* its size, i.e. the panel's leading dimension */
    /* gaussian batched add-scoring (addbatch / addic) */
    double *Ufac, *Uw, *Upan, *zsq, *spiv;
} fitctx;

/* Information criterion of the model with the given active column set.
 * active[0] = 0 (intercept); active[r] = (X-column index)+1 for r >= 1.
 * q = number of coefficients.  maxit caps the IRLS iterations of the glm fit
 * (IRLS_MAXIT for a full fit, 1 for the one-step fast proposal; ignored on the
 * gaussian path, which has no iteration).  Sets *ok = 0 if the fit failed.
 * cached != 0 uses the shared first-step weights in c->cwz, which is valid only for
 * a one-step fit of the model those weights were built from plus one column. */
static double modelic(fitctx *c, const int *active, int q, int maxit, int *ok, int cached)
{
    if (c->family == FAM_GAUSSIAN)
    {
        double rss = rsschol(c->G, c->Gy, c->ptot1, active, q, c->yty, c->Sbuf, c->bbuf);
        if (rss < 0.0)
        {
            *ok = 0;
            return 0.0;
        }
        *ok = 1;
        /* -2logLik of the gaussian MLE (variance profiled out at sigma^2=RSS/n):
         *     -2logL = n*(log(2*pi*RSS/n) + 1) - sum_i log(w_i).
         * The last term is the weighted-likelihood correction sum log(w_i); it
         * is constant across models but kept so the value matches R's glm()
         * exactly.  npar = q+1 counts the q coefficients plus the variance;
         * npred = q-1 excludes the intercept. */
        double base = (double) c->n * (log(2.0 * M_PI * rss / (double) c->n) + 1.0) - c->sumlogw;
        return icval(base, q + 1, q - 1, c->n, c->info, c->gamma, c->p0);
    }
    else
    {
        /* warm start from the current model's coefficients (0 for new cols);
         * the design columns are indexed from the shared block design c->D
         * (built once as [1 | X]) -- no per-fit design rebuild. */
        for (int r = 0; r < q; r++) c->b0[r] = c->bfull[active[r]];
        double dev2;
        if (glmirls(c->family, c->y, c->pw, c->D, active, c->n, q, maxit, c->wq, c->wn, c->Dpack, c->Dw, &dev2, c->b0, c->bprop, cached ? c->cwz : NULL))
        {
            *ok = 0;
            return 0.0;
        }
        *ok = 1;
        /* dev2 is the model-dependent part of -2logLik; add the precomputed
         * data-only constant to recover the full -2logLik (see glmllconst). */
        return icval(dev2 + c->m2ll_const, q, q - 1, c->n, c->info, c->gamma, c->p0);
    }
}

/*
 * Build the first-step weights shared by every addition proposal off the current
 * binomial/poisson model, into c->cwz as the u = w*z and sqrt(w) pair glmirls
 * expects (see its cwz argument).
 *
 * A proposal warm-starts from the current model's coefficients with zero on the
 * column being added, so its initial linear predictor is the current model's
 * whatever column that is, and every quantity the first Newton step derives from
 * it -- the mean, the IRLS weight, the working response -- is shared by all p
 * candidates.  The sampler recomputes them per proposal today; this computes them
 * once per accepted move instead.  Removals are excluded: dropping a coefficient
 * changes the linear predictor, so their first step is genuinely their own.
 *
 * The arithmetic is glmirls' weight pass verbatim, including the mu clamps, the
 * poisson eta bound, the left-to-right pw*mueta*mueta/var, and the working response
 * being formed from the UNCLAMPED eta -- so the cached values are bit-for-bit the
 * ones the per-proposal path would have produced.
 */
static void irlscache(fitctx *c, const int *active, int q)
{
    int n = c->n;
    double *eta = c->wn;
    double *cu = c->cwz;
    double *csw = c->cwz + n;
    double *cw = c->cwz + 2 * n;

    for (int i = 0; i < n; i++) eta[i] = 0.0;
    for (int a = 0; a < q; a++)
    {
        const double *Da = c->D + (size_t) active[a] * n;
        double ba = c->bfull[active[a]];
        for (int i = 0; i < n; i++) eta[i] += Da[i] * ba;
    }

    for (int i = 0; i < n; i++)
    {
        double e = eta[i], m, mueta, var;
        if (c->family == FAM_BINOMIAL)
        {
            m = 1.0 / (1.0 + exp(-e));
            if (m < MU_EPS)
                m = MU_EPS;
            else if (m > 1.0 - MU_EPS)
                m = 1.0 - MU_EPS;
            mueta = m * (1.0 - m);
            var   = m * (1.0 - m);
        }
        else /* poisson */
        {
            if (e >  30.0) e =  30.0;
            if (e < -30.0) e = -30.0;
            m = exp(e);
            if (m < MU_EPS) m = MU_EPS;
            mueta = m;
            var   = m;
        }
        double wi = c->pw[i] * mueta * mueta / var;
        double zi = eta[i] + (c->y[i] - m) / mueta;
        cu[i] = wi * zi;
        csw[i] = sqrt(wi);
        cw[i] = wi;
    }
}

/*
 * Prepare the one-step score of EVERY single-column addition to the current
 * binomial/poisson model at once.
 *
 * irlscache() has already fixed the first Newton step's weights W and working
 * response z, which are shared by all candidates.  In that fixed metric the step for
 * A + {j} solves the weighted normal equations, and with Ghat = D'WD, ghat = D'Wz
 * those border the current model's exactly as in the gaussian case:
 *     S = Ghat[A,A] = U'U,  wbar = U^-T ghat[A],  u_j = U^-T Ghat[A,j],
 *     s_j^2 = Ghat[j,j] - u_j'u_j,  zeta_j = (ghat[j] - u_j'wbar) / s_j.
 * The step's coefficients are c_j = zeta_j/s_j on the new column and
 * bhat - c_j v_j on the active ones, with v_j = U^-1 u_j and bhat = U^-1 wbar, so the
 * candidate's linear predictor is a rank-one update of the active model's,
 *     eta_j = D_A bhat + c_j (D[,j] - D_A v_j),
 * which is what irlsic() finishes per candidate.  Unlike the gaussian path the
 * deviance is not a by-product of the algebra -- it needs that linear predictor and a
 * pass over the observations -- so only the normal equations are shared here.
 *
 * The panel is qcap x (ptot1 + 1) column-major: column j holds Ghat[A,j] and the one
 * extra column holds ghat[A], so a single triangular solve produces every u_j and
 * wbar together.  A second solve overwrites it with every v_j and bhat, which is safe
 * because s_j^2 and zeta_j are extracted in between.
 *
 * Returns 0 on success, or 1 if the current model's own block will not factor, in
 * which case the caller scores proposals one at a time.
 */
static int irlsbatch(fitctx *c, const int *active, int q)
{
    int n = c->n;
    int ptot1 = c->ptot1;
    int ldp = q;
    int ncol = ptot1 + 1;
    const double *cw = c->cwz + 2 * n;
    const double *cu = c->cwz;
    double *P = c->Gpan;
    double *U = c->Gfac;

    /* the weight-scaled active design, and the candidate weighted diagonal */
    for (int a = 0; a < q; a++)
    {
        const double *Da = c->D + (size_t) active[a] * n;
        double *dst = c->GDws + (size_t) a * n;
        for (int i = 0; i < n; i++) dst[i] = cw[i] * Da[i];
    }
    for (int j = 0; j < ptot1; j++)
    {
        const double *Dj = c->D + (size_t) j * n;
        double s = 0.0;
        for (int i = 0; i < n; i++) s += cw[i] * Dj[i] * Dj[i];
        c->Ggd[j] = s;
    }

    /*
     * dgemm (BLAS-3) forms a general matrix product.  We need the q x ptot1 panel
     * Ghat[A,] = (W D_A)' D of every candidate's weighted cross-products with the
     * active columns, which is one such product and the only O(n q p) work in the
     * batch.  TRANSA="T" and TRANSB="N" give C = A'B with A = the weight-scaled
     * active design (n x q, leading dimension n) and B = the block design (n x ptot1,
     * leading dimension n); m = q rows and ncol = ptot1 columns of output, k = n is
     * the contracted length, alpha is 1 and beta 0 so C is overwritten, and C is the
     * panel with leading dimension q.  Each of the two character flags carries its
     * own hidden Fortran length, hence two FCONE.
     *
     * Equivalent R operation: P[, 1:ptot1] <- crossprod(W * D[, A], D).
     * Netlib references: BLAS dgemm. */
    double alpha = 1.0, beta = 0.0;
    F77_CALL(dgemm)("T", "N", &q, &ptot1, &n, &alpha, c->GDws, &n, c->D, &n, &beta, P, &ldp FCONE FCONE);

    /*
     * dgemv (BLAS-2) forms a matrix-vector product.  Here it gives every candidate's
     * weighted cross-product with the working response, ghat = D'Wz, in one pass;
     * W z is already available as the cached u = w*z.  TRANS="T" contracts over the
     * n rows of the block design (leading dimension n), x is that cached vector with
     * stride 1, alpha is 1 and beta 0 so the length-ptot1 result y is overwritten,
     * also with stride 1.
     *
     * Equivalent R operation: ghat <- crossprod(D, w * z).
     * Netlib references: BLAS dgemv. */
    int inc1 = 1;
    F77_CALL(dgemv)("T", &n, &ptot1, &alpha, c->D, &n, cu, &inc1, &beta, c->Ggv, &inc1 FCONE);

    /* the active block and right-hand side, the latter as the panel's extra column */
    for (int r = 0; r < q; r++)
        for (int s = 0; s < q; s++)
            U[r + s * q] = P[r + (size_t) active[s] * ldp];
    for (int r = 0; r < q; r++)
        P[r + (size_t) ptot1 * ldp] = c->Ggv[active[r]];

    /*
     * dpotrf (LAPACK) Cholesky-factors the symmetric positive-definite active block
     * S = U'U once, for reuse by every candidate.  UPLO="U" with leading dimension q
     * factors it in place from the fully populated copy above; U overwrites the upper
     * triangle and a non-positive-definite leading minor is reported in info.  A
     * non-zero info, or a pivot at or below cholsolv's collinearity tolerance, means
     * the active columns are collinear and the caller must score proposals directly;
     * the tolerance matches cholsolv so the two paths agree on what is fittable.
     *
     * Equivalent R operation: U <- chol(S).
     * Netlib references: LAPACK dpotrf. */
    int info;
    F77_CALL(dpotrf)("U", &q, U, &q, &info FCONE);
    if (info != 0)
        return 1;
    for (int r = 0; r < q; r++)
        if (U[r + r * q] <= 1e-6)
            return 1;

    /*
     * dtrsm (BLAS-3) solves a triangular system with many right-hand sides.  The
     * first call reduces every panel column g to u = U^-T g, which is the whole
     * per-candidate cost of the normal equations, and the extra column to wbar.
     * SIDE="L" with TRANSA="T" solves U'X = B, UPLO="U" and DIAG="N" take U from the
     * factor above with leading dimension q, alpha is 1, m = q and n = ncol, and B is
     * the panel with leading dimension q, OVERWRITTEN with the solution.  The second
     * call then solves U X = B on the same panel, turning each u into v = U^-1 u and
     * wbar into bhat; s_j^2 and zeta_j are read off between the two, so overwriting is
     * safe.  Four character flags, hence four FCONE each.
     *
     * Equivalent R operation: P <- backsolve(U, P, transpose = TRUE), then
     * P <- backsolve(U, P).
     * Netlib references: BLAS dtrsm. */
    F77_CALL(dtrsm)("L", "U", "T", "N", &q, &ncol, &alpha, U, &q, P, &ldp FCONE FCONE FCONE FCONE);

    for (int r = 0; r < q; r++) c->Gwb[r] = P[r + (size_t) ptot1 * ldp];
    for (int j = 0; j < ptot1; j++)
    {
        const double *pj = P + (size_t) j * ldp;
        double uu = 0.0, uw = 0.0;
        for (int r = 0; r < q; r++)
        {
            uu += pj[r] * pj[r];
            uw += pj[r] * c->Gwb[r];
        }
        double s2 = c->Ggd[j] - uu;
        c->Gs2[j] = s2;
        c->Gcj[j] = (s2 > 0.0) ? (c->Ggv[j] - uw) / s2 : 0.0;
    }

    F77_CALL(dtrsm)("L", "U", "N", "N", &q, &ncol, &alpha, U, &q, P, &ldp FCONE FCONE FCONE FCONE);

    for (int a = 0; a < q; a++) c->Gact[a] = active[a];
    c->Gnq = q;

    /* the active model's own one-step linear predictor, which every candidate's is a
     * rank-one update of */
    for (int i = 0; i < n; i++) c->Getah[i] = 0.0;
    for (int a = 0; a < q; a++)
    {
        const double *Da = c->D + (size_t) active[a] * n;
        double ba = P[a + (size_t) ptot1 * ldp];
        for (int i = 0; i < n; i++) c->Getah[i] += Da[i] * ba;
    }
    return 0;
}

/* Is the batched score for design column j accurate enough to use?  As on the
 * gaussian path s_j^2 = Ghat[j,j] - u_j'u_j is a cancelling difference, so its
 * relative accuracy collapses for a column nearly in the span of the active set --
 * which is also where the collinearity verdict is decided, and where getting the
 * verdict wrong is worse than getting the score wrong: a proposal the direct fit
 * rejects draws no uniform, so a changed verdict shifts the random stream for the
 * whole remaining run.  Candidates inside the margin go to the direct fit. */
static int irlssafe(fitctx *c, int j)
{
    return c->Gs2[j] > 1e-3 * c->Ggd[j];
}

/* One-step information criterion of the current model plus design column j, finished
 * from the batch irlsbatch() left behind; pq is the resulting coefficient count.  The
 * caller has cleared the addition through irlssafe(), so the step is known to be
 * fittable.  The clamps and the -2logLik are glmirls' final pass verbatim, so the
 * only difference from scoring the proposal directly is the arithmetic path taken to
 * its linear predictor. */
static double irlsic(fitctx *c, int j, int pq)
{
    int n = c->n;
    double cj = c->Gcj[j];
    const double *Dj = c->D + (size_t) j * n;
    double *eta = c->Getac;
    double *mu = c->Gmuc;

    /* eta = etah + c_j (D[,j] - D_A v_j); the panel now holds every v_j, and Gact is
     * the active set the batch was built from (the sweep has since spliced active[]) */
    for (int i = 0; i < n; i++) eta[i] = c->Getah[i] + cj * Dj[i];
    for (int a = 0; a < c->Gnq; a++)
    {
        const double *Da = c->D + (size_t) c->Gact[a] * n;
        double va = cj * c->Gpan[a + (size_t) j * c->Gnq];
        for (int i = 0; i < n; i++) eta[i] -= Da[i] * va;
    }

    for (int i = 0; i < n; i++)
    {
        double e = eta[i], m;
        if (c->family == FAM_BINOMIAL)
        {
            m = 1.0 / (1.0 + exp(-e));
            if (m < MU_EPS)
                m = MU_EPS;
            else if (m > 1.0 - MU_EPS)
                m = 1.0 - MU_EPS;
        }
        else
        {
            if (e >  30.0) e =  30.0;
            if (e < -30.0) e = -30.0;
            m = exp(e);
            if (m < MU_EPS) m = MU_EPS;
        }
        mu[i] = m;
    }
    double dev2 = glmm2ll(c->family, c->y, mu, c->pw, n);
    return icval(dev2 + c->m2ll_const, pq, pq - 1, c->n, c->info, c->gamma, c->p0);
}

/*
 * Score EVERY single-column addition to the current gaussian model at once.
 *
 * Adding column j to the active set A borders the current Gram sub-block,
 *     S' = [ S   g ]   with  g = G[A,j],  d = G[j,j],
 *          [ g'  d ]
 * whose Cholesky factor extends the current one by a single row and column,
 *     U' = [ U   u ]   with  u = U^{-T} g,  s = sqrt(d - u'u).
 * Because the leading block of U' is unchanged, the forward solve against the
 * right-hand side also extends by one entry: with w = U^{-T} Gy[A] fixed,
 *     z_j = (Gy[j] - u'w) / s     and     RSS_{A+j} = yty - (||w||^2 + z_j^2).
 * So the only per-candidate work is u_j = U^{-T} G[A,j], and stacking the p+1
 * candidate columns turns that into ONE triangular solve.  This is what makes the
 * batch worth doing: a sweep visits every candidate but only changes A when a move
 * is accepted, which is rare, so the same factor serves thousands of proposals.
 *
 * Layout.  The panel P is column-major (ptot1+1) x q with leading dimension
 * ptot1+1: column c is G[active[c], 0..ptot], which is a CONTIGUOUS run of G
 * (both triangles are filled by the caller), so building the panel streams
 * instead of gathering scattered elements.  One extra final row carries
 * Gy[active], so the same solve returns w in that row.
 *
 * On success returns 0, writes ||w||^2 to *rssw, and for each design column j
 * leaves the squared new pivot in spiv[j] and z_j^2 in zsq[j] -- the two
 * quantities addic() needs.  Returns 1 if the current model's own sub-block is
 * not positive definite, in which case the caller must fall back to scoring
 * proposals one at a time.
 */
static int addbatch(fitctx *c, const int *active, int q, double *rssw)
{
    int ptot1 = c->ptot1;
    int ldp = ptot1 + 1;
    double *U = c->Ufac;
    double *P = c->Upan;
    double *w = c->Uw;

    /* S = G[active, active], both triangles, exactly as rsschol gathers it */
    for (int r = 0; r < q; r++)
        for (int s = 0; s < q; s++)
            U[r * q + s] = c->G[active[r] * ptot1 + active[s]];

    /*
     * dpotrf (LAPACK) Cholesky-factors the symmetric positive-definite S = U'U.
     * We factor the CURRENT model once here and then reuse U for every candidate
     * addition, which is the whole point of the batch.  UPLO="U" with leading
     * dimension q factors S in place (both triangles are populated, so no
     * transpose is needed); U overwrites the upper triangle and a non-positive-
     * definite leading minor is reported in info.  A non-zero info, or a pivot
     * with U[r,r]^2 <= 1e-10, means the current active columns are collinear;
     * we return 1 and the caller scores proposals individually instead.  The
     * tolerance matches rsschol so the two paths agree on what is fittable.
     *
     * Equivalent R operation: U <- chol(S).
     * Netlib references: LAPACK dpotrf. */
    int info;
    F77_CALL(dpotrf)("U", &q, U, &q, &info FCONE);
    if (info != 0)
        return 1;
    for (int r = 0; r < q; r++)
        if (U[r * q + r] <= 1e-5)
            return 1;

    /* panel column c = G[active[c], .] (contiguous), plus Gy[active[c]] last */
    for (int cc = 0; cc < q; cc++)
    {
        memcpy(P + (size_t) cc * ldp, c->G + (size_t) active[cc] * ptot1, (size_t) ptot1 * sizeof(double));
        P[(size_t) cc * ldp + ptot1] = c->Gy[active[cc]];
    }

    /*
     * dtrsm (BLAS-3) solves a triangular system with many right-hand sides.  We
     * want u_j = U^{-T} G[A,j] for every candidate j; writing those as the rows of
     * the panel P, the whole set satisfies X U = P, which is dtrsm's SIDE="R",
     * TRANSA="N" form -- one BLAS-3 call in place of one Cholesky per candidate.
     * SIDE="R" and TRANSA="N" solve X*U = alpha*P, UPLO="U" and DIAG="N" take U
     * from the upper triangle of the factor above with leading dimension q, alpha
     * is 1, m = ptot1+1 rows and n = q columns, and P is OVERWRITTEN with X, so
     * afterwards row j holds u_j' and the extra final row holds w'.  Each of the
     * four character flags needs its own hidden Fortran length, hence four FCONE.
     *
     * Equivalent R operation: X <- t(backsolve(U, t(P), transpose = TRUE)).
     * Netlib references: BLAS dtrsm. */
    double alpha = 1.0;
    F77_CALL(dtrsm)("R", "U", "N", "N", &ldp, &q, &alpha, U, &q, P, &ldp FCONE FCONE FCONE FCONE);

    for (int cc = 0; cc < q; cc++)
        w[cc] = P[(size_t) cc * ldp + ptot1];
    double ww = 0.0;
    for (int cc = 0; cc < q; cc++)
        ww += w[cc] * w[cc];
    *rssw = ww;

    /* accumulate u_j'u_j and u_j'w down each panel column (contiguous in j) */
    for (int j = 0; j < ptot1; j++)
        c->spiv[j] = 0.0;
    for (int j = 0; j < ptot1; j++)
        c->zsq[j] = 0.0;
    for (int cc = 0; cc < q; cc++)
    {
        const double *pc = P + (size_t) cc * ldp;
        double wc = w[cc];
        for (int j = 0; j < ptot1; j++)
        {
            c->spiv[j] += pc[j] * pc[j];
            c->zsq[j] += pc[j] * wc;
        }
    }

    /* turn the accumulators into the new pivot s_j^2 and the RSS drop z_j^2 */
    for (int j = 0; j < ptot1; j++)
    {
        double s2 = c->G[(size_t) j * ptot1 + j] - c->spiv[j];
        c->spiv[j] = s2;
        if (s2 > 1e-10)
        {
            double z = (c->Gy[j] - c->zsq[j]) / sqrt(s2);
            c->zsq[j] = z * z;
        }
        else
        {
            c->zsq[j] = 0.0;
        }
    }
    return 0;
}

/*
 * Is the batched score for design column j accurate enough to use?
 *
 * s_j^2 = G[j,j] - u_j'u_j is a difference of two quantities that converge as the
 * candidate column approaches the span of the active set, so it is computed with an
 * absolute error near eps*G[j,j] and its RELATIVE accuracy collapses for a nearly
 * dependent column -- which is also exactly where the collinearity verdict itself
 * is decided.  Every candidate inside the margin goes to the direct factorisation,
 * which is the authority on both the verdict and the score; without that the two
 * paths disagree about aliased columns, which moves the chain rather than only its
 * last bits.  Note s_j^2/G[j,j] is one minus the R^2 of regressing column j on the
 * active set, so the margin below excludes only columns explained to within 1e-3 by
 * the current model -- effectively aliased ones, a few per design at most -- and
 * holds the relative error in s_j^2 near eps/1e-3, small enough that the criterion
 * agrees with the direct fit far past the precision the Metropolis test can resolve.
 */
static int addsafe(fitctx *c, int j)
{
    return c->spiv[j] > 1e-3 * c->G[(size_t) j * c->ptot1 + j];
}

/* Information criterion of the current model plus design column j, read out of the
 * batch that addbatch() left behind; pq is the resulting coefficient count and rssw
 * the ||w||^2 it reported.  The caller has already cleared the addition through
 * addsafe(), so the enlarged model is known to be comfortably fittable. */
static double addic(fitctx *c, int j, int pq, double rssw)
{
    double rss = c->yty - (rssw + c->zsq[j]);
    if (rss < 1e-12)
        rss = 1e-12;
    double base = (double) c->n * (log(2.0 * M_PI * rss / (double) c->n) + 1.0) - c->sumlogw;
    return icval(base, pq + 1, pq - 1, c->n, c->info, c->gamma, c->p0);
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
    ws->ord    = R_Calloc((size_t) capt, int);
    ws->bcols  = R_Calloc((size_t) capt, int);
    ws->s0     = R_Calloc((size_t) capt, int);
    ws->fr     = R_Calloc((size_t) capt, double);
    ws->Xb     = R_Calloc((size_t) n * capt, double);

    ws->G = NULL;
    ws->Gy = NULL;
    ws->Sbuf = NULL;
    ws->bbuf = NULL;
    ws->D = NULL;
    ws->wq = NULL;
    ws->wn = NULL;
    ws->Dpack = NULL;
    ws->Dw = NULL;
    ws->bfull = NULL;
    ws->b0 = NULL;
    ws->bprop = NULL;
    ws->cwz = NULL;
    ws->Gpan = NULL;
    ws->Gfac = NULL;
    ws->Gwb = NULL;
    ws->Ggd = NULL;
    ws->Ggv = NULL;
    ws->Gs2 = NULL;
    ws->Gcj = NULL;
    ws->GDws = NULL;
    ws->Getah = NULL;
    ws->Getac = NULL;
    ws->Gmuc = NULL;
    ws->Gact = NULL;
    ws->Ufac = NULL;
    ws->Uw = NULL;
    ws->Upan = NULL;
    ws->zsq = NULL;
    ws->spiv = NULL;

    if (family == FAM_GAUSSIAN)
    {
        int qcap = (capt < GBQMAX) ? capt : GBQMAX;   /* batch is capped at GBQMAX coefficients */
        ws->G    = R_Calloc((size_t) capt * capt, double);
        ws->Gy   = R_Calloc((size_t) capt, double);
        ws->Sbuf = R_Calloc((size_t) capt * capt, double);
        ws->bbuf = R_Calloc((size_t) capt, double);
        ws->Ufac = R_Calloc((size_t) qcap * qcap, double);
        ws->Uw   = R_Calloc((size_t) qcap, double);
        ws->Upan = R_Calloc((size_t) (capt + 1) * qcap, double);   /* one extra row carries Gy[active] */
        ws->zsq  = R_Calloc((size_t) capt, double);
        ws->spiv = R_Calloc((size_t) capt, double);
    }
    else
    {
        ws->D     = R_Calloc((size_t) n * capt, double);
        ws->wq    = R_Calloc((size_t) capt * capt + 3 * capt, double);
        ws->wn    = R_Calloc((size_t) 5 * n, double);
        ws->Dpack = R_Calloc((size_t) n * capt, double);
        ws->Dw    = R_Calloc((size_t) n * capt, double);
        ws->bfull = R_Calloc((size_t) capt, double);
        ws->b0    = R_Calloc((size_t) capt, double);
        ws->bprop = R_Calloc((size_t) capt, double);
        ws->cwz   = R_Calloc((size_t) 3 * n, double);
        int qcap = (capt < GBQMAX) ? capt : GBQMAX;   /* batch is capped at GBQMAX coefficients */
        ws->Gpan  = R_Calloc((size_t) qcap * (capt + 1), double);   /* one extra column carries ghat[active] */
        ws->Gfac  = R_Calloc((size_t) qcap * qcap, double);
        ws->Gwb   = R_Calloc((size_t) qcap, double);
        ws->Ggd   = R_Calloc((size_t) capt, double);
        ws->Ggv   = R_Calloc((size_t) capt, double);
        ws->Gs2   = R_Calloc((size_t) capt, double);
        ws->Gcj   = R_Calloc((size_t) capt, double);
        ws->GDws  = R_Calloc((size_t) n * qcap, double);
        ws->Getah = R_Calloc((size_t) n, double);
        ws->Getac = R_Calloc((size_t) n, double);
        ws->Gmuc  = R_Calloc((size_t) n, double);
        ws->Gact  = R_Calloc((size_t) capt, int);
    }
    return 0;
}

/* Free every buffer of a workspace (R_Free is a no-op on the NULL fields of the
 * family set that was not allocated). */
void gbwsfree(gbwst *ws)
{
    R_Free(ws->inc);
    R_Free(ws->active);
    R_Free(ws->ord);
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
    R_Free(ws->Dpack);
    R_Free(ws->Dw);
    R_Free(ws->bfull);
    R_Free(ws->b0);
    R_Free(ws->bprop);
    R_Free(ws->cwz);
    R_Free(ws->Gpan);
    R_Free(ws->Gfac);
    R_Free(ws->Gwb);
    R_Free(ws->Ggd);
    R_Free(ws->Ggv);
    R_Free(ws->Gs2);
    R_Free(ws->Gcj);
    R_Free(ws->GDws);
    R_Free(ws->Getah);
    R_Free(ws->Getac);
    R_Free(ws->Gmuc);
    R_Free(ws->Gact);
    R_Free(ws->Ufac);
    R_Free(ws->Uw);
    R_Free(ws->Upan);
    R_Free(ws->zsq);
    R_Free(ws->spiv);
}

/* Lower bound in the ascending tail active[1..nact-1]: the first index holding a
 * value >= val, or nact when every stored value is smaller.  active[0] is the
 * intercept slot and is never a search candidate, so the scan starts at 1.  A
 * proposal changes the inclusion vector in one position only, so the sampler
 * splices that single coordinate at the index returned here rather than rebuilding
 * the whole list; the spliced list is in the same ascending order the full rebuild
 * produced, so every candidate fit still sees the same column permutation. */
int actfind(const int *active, int nact, int val)
{
    int lo = 1, hi = nact;
    while (lo < hi)
    {
        int mid = lo + ((hi - lo) >> 1);
        if (active[mid] < val)
            lo = mid + 1;
        else
            hi = mid;
    }
    return lo;
}

int rungibbs(const double *y, const double *X, const double *pw, int n, int p1, int p2, const int *smod, int perm, int len, double k, double gamma, int p0, int info, int family, int nvars, rngt *rng, int *omat, double *ofrq, double *oic, gbwst *wsi)
{
    int ptot  = p1 + p2;
    int ptot1 = ptot + 1;
    int a, b, i;

    /* Proposal scoring for the iterative families: score each single-coordinate
     * proposal with ONE warm-started IRLS step (prop_maxit = 1) -- the warm start
     * is one column from the current fit, so a single Newton step is an accurate
     * proposal score -- and re-fit the accepted model to full convergence before
     * committing, so the recorded ICs and warm-start coefficients stay exact and
     * only the accept/reject decision uses the cheap approximate score.  The
     * gaussian family has no IRLS, so its direct fit is always exact. */
    int prop_maxit = (family != FAM_GAUSSIAN) ? 1 : IRLS_MAXIT;

    /* Use the caller's workspace, or allocate a private one (main thread only)
     * when wsi is NULL.  Pointing the original locals at the workspace fields
     * keeps the sampler body below unchanged. */
    gbwst  wsl;
    gbwst *ws    = wsi;
    int    owned = 0;
    if (!ws)
    {
        gbwsallc(&wsl, ptot1, n, family);
        ws    = &wsl;
        owned = 1;
    }
    int    *inc    = ws->inc;
    int    *active = ws->active;
    int    *ord    = ws->ord;
    double *G = ws->G, *Gy = ws->Gy, *Sbuf = ws->Sbuf, *bbuf = ws->bbuf;
    double *D = ws->D, *wq = ws->wq, *wn = ws->wn;
    double *Dpack = ws->Dpack, *Dw = ws->Dw;
    double *bfull = ws->bfull, *b0 = ws->b0, *bprop = ws->bprop;

    fitctx c;
    c.family = family;
    c.info = info;
    c.p0 = p0;
    c.n = n;
    c.ptot1 = ptot1;
    c.gamma = gamma;
    c.y = y;
    c.X = X;
    c.pw = pw;
    c.G = c.Gy = NULL;
    c.Sbuf = c.bbuf = NULL;
    c.Ufac = c.Uw = c.Upan = c.zsq = c.spiv = NULL;
    c.D = c.wq = c.wn = NULL;
    c.Dpack = c.Dw = NULL;
    c.bfull = c.b0 = c.bprop = NULL;
    c.cwz = NULL;
    c.Gpan = c.Gfac = c.Gwb = c.Ggd = c.Ggv = c.Gs2 = c.Gcj = NULL;
    c.GDws = c.Getah = c.Getac = c.Gmuc = NULL;
    c.Gact = NULL;
    c.Gnq = 0;
    c.yty = 0.0;
    c.sumlogw = 0.0;
    c.m2ll_const = 0.0;

    /* The GLM warm-start coefficient state is reused across blocks, so clear it
     * at the start of each run (a fresh calloc gave the same zero start). */
    if (family != FAM_GAUSSIAN)
        for (a = 0; a < ptot1; a++) bfull[a] = 0.0;

    if (family == FAM_GAUSSIAN)
    {
        /* Precompute the weighted Gram matrix ONCE for the whole run:
         *   G  = M'WM   ((1+ptot) x (1+ptot)),   M = [1 | X], W = diag(pw)
         *   Gy = M'Wy   (1+ptot),    yty = y'Wy,    sumlogw = sum_i log(w_i).
         * Layout of G: row/col 0 is the intercept, so G[0]=sum w_i, the first
         * row/col holds the weighted column sums sum w_i x_ia, and the interior
         * block holds sum w_i x_ia x_ib (symmetric).  Every later candidate fit
         * reads a sub-block of these arrays -- it never touches X or y again. */
        double yty = 0.0, sumw = 0.0, sumlogw = 0.0;
        for (i = 0; i < n; i++)
        {
            sumw += pw[i];
            sumlogw += log(pw[i]);
        }
        G[0] = sumw;                                   /* intercept'W intercept */
        for (a = 0; a < ptot; a++)
        {
            const double *Xa = X + (size_t) a * n;
            double col = 0.0;
            for (i = 0; i < n; i++) col += pw[i] * Xa[i];
            G[(a + 1) * ptot1] = col;
            G[a + 1] = col;
        }
        for (a = 0; a < ptot; a++)
        {
            const double *Xa = X + (size_t) a * n;
            for (b = a; b < ptot; b++)
            {
                const double *Xb = X + (size_t) b * n;
                double s = 0.0;
                for (i = 0; i < n; i++) s += pw[i] * Xa[i] * Xb[i];
                G[(a + 1) * ptot1 + (b + 1)] = s;
                G[(b + 1) * ptot1 + (a + 1)] = s;
            }
        }
        double sy = 0.0;
        for (i = 0; i < n; i++)
        {
            sy += pw[i] * y[i];
            yty += pw[i] * y[i] * y[i];
        }
        Gy[0] = sy;
        for (a = 0; a < ptot; a++)
        {
            const double *Xa = X + (size_t) a * n;
            double s = 0.0;
            for (i = 0; i < n; i++) s += pw[i] * Xa[i] * y[i];
            Gy[a + 1] = s;
        }
        c.G = G;
        c.Gy = Gy;
        c.yty = yty;
        c.sumlogw = sumlogw;
        c.Sbuf = Sbuf;
        c.bbuf = bbuf;
        c.Ufac = ws->Ufac;
        c.Uw = ws->Uw;
        c.Upan = ws->Upan;
        c.zsq = ws->zsq;
        c.spiv = ws->spiv;
    }
    else
    {
        /* build the block design once: Dfull = [1 | X], n x ptot1 */
        for (i = 0; i < n; i++) D[i] = 1.0;
        for (a = 0; a < ptot; a++)
            memcpy(D + (size_t) (a + 1) * n, X + (size_t) a * n, (size_t) n * sizeof(double));
        c.D = D;
        c.wq = wq;
        c.wn = wn;
        c.Dpack = Dpack;
        c.Dw = Dw;
        c.bfull = bfull;
        c.b0 = b0;
        c.bprop = bprop;
        c.cwz = ws->cwz;
        c.Gpan = ws->Gpan;
        c.Gfac = ws->Gfac;
        c.Gwb = ws->Gwb;
        c.Ggd = ws->Ggd;
        c.Ggv = ws->Ggv;
        c.Gs2 = ws->Gs2;
        c.Gcj = ws->Gcj;
        c.GDws = ws->GDws;
        c.Getah = ws->Getah;
        c.Getac = ws->Getac;
        c.Gmuc = ws->Gmuc;
        c.Gact = ws->Gact;
        /* data-only -2logLik normalising constant: same for every candidate
         * model, so compute it once here (cf. sumlogw on the gaussian path)
         * instead of recomputing the lgamma terms on every fit. */
        c.m2ll_const = glmllconst(family, y, pw, n);
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
    for (a = 0; a < p1; a++)
    {
        inc[a] = smod[a] ? 1 : 0;
        nsel += inc[a];
    }
    for (a = p1; a < ptot; a++)
    {
        inc[a] = 1;
        nsel += 1;
    }

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
    /* active[0..nact-1] mirrors the current model from here on: the full rebuild
     * above is the only one, and it also re-seeds nact for the screening path,
     * where one workspace is reused across many runs with different ptot. */
    int nact = q;
    double curic = modelic(&c, active, q, IRLS_MAXIT, &ok, 0);   /* full fit */
    if (!ok)
        curic = R_PosInf;
    else
        COMMIT_BETA(q);

    if (ofrq)
        for (a = 0; a < p1; a++) ofrq[a] = 0.0;

    /*
     * Batched add-scoring state (gaussian only).  The batch is valid for exactly
     * one active set, so `batch` marks it stale after every accepted move and it
     * is rebuilt lazily on the next addition proposal (batch < 0 records an active
     * set whose own sub-block would not factor, so it is attempted only once).
     *
     * Building it touches ptot1*q doubles, while it saves one q x q factorisation
     * per addition scored from it, so it pays only when many additions are scored
     * before a move is accepted.  That splits on how much room the size cap leaves.
     * A criterion holding the model well below the cap accepts well under one move
     * per sweep, so a single batch serves thousands of proposals.  A chain pressed
     * against the cap is the opposite: an accepted addition puts it back at the
     * ceiling, where every further addition is discarded by the cap before any fit,
     * so the batch would serve almost nobody and the per-proposal path is already
     * cheap for exactly the same reason.  Requiring the model to be at most half
     * the cap keeps the batch to the first regime; testing the cap rather than the
     * observed accept rate keeps the decision deterministic, which matters because
     * accept intervals are geometric and a rate-based rule mistakes an ordinary
     * short interval for the wrong regime.
     */
    int usebatch = (family == FAM_GAUSSIAN);
    int batch = 0;
    double rssw = 0.0;

    /* The binomial/poisson analogue: the first Newton step of an addition proposal
     * derives its weights from the current model's linear predictor, so they are
     * shared by every candidate.  `icache` marks them valid; like the gaussian
     * batch they are built lazily on the first addition and go stale as soon as a
     * move is accepted, and they start invalid because one workspace serves many
     * runs with different designs. */
    int usecache = (family != FAM_GAUSSIAN);
    int icache = 0;

    /* The iterative families' batched one-step scorer, gated and invalidated exactly
     * like the gaussian one: it shares the cached weights above, so it goes stale on
     * the same event, and it is worth building for the same reason -- one panel serves
     * every addition until a move is accepted. */
    int useglmb = (family != FAM_GAUSSIAN);
    int gbatch = 0;

    /* Run 2*len sweeps; the first `len` are burn-in (discarded), the second
     * `len` are recorded.  One sweep = p1 single-coordinate flip attempts. */
    int nsweep = 2 * len;
    for (int sw = 0; sw < nsweep; sw++)
    {
        /* perm = TRUE: visit each of the p1 toggleable coordinates exactly once
         * per sweep, in a fresh random order (Fisher-Yates), i.e. without
         * replacement.  perm = FALSE: the fixed 0..p1-1 systematic sweep. */
        if (perm)
        {
            for (int t = 0; t < p1; t++) ord[t] = t;
            for (int t = p1 - 1; t > 0; t--)
            {
                int u = (int) (UNIF(rng) * (t + 1));
                if (u > t) u = t;                 /* guard the UNIF==~1 edge */
                int tmp = ord[t];
                ord[t] = ord[u];
                ord[u] = tmp;
            }
        }
        for (int step = 0; step < p1; step++)
        {
            /* pick the coordinate to flip: random permutation order (perm) or
             * the in-order sweep position; both are always in [0, p1-1] */
            int j = perm ? ord[step] : step;

            /* reject moves that would empty the model or exceed the size cap */
            int pnsel = nsel + (inc[j] ? -1 : 1);
            if (pnsel < 1 || pnsel > nvars) continue;

            /* Splice coordinate j into (or out of) the sorted active list at its
             * ascending position.  r and drop are kept because a rejected proposal
             * must put the list back exactly as it was: active[] mirrors the
             * current model, and only an accept makes the spliced list current. */
            int drop = inc[j];
            int val = j + 1;
            int r = actfind(active, nact, val);

            /* An addition is scored out of the batch for the current active set,
             * built here on demand -- while active[] still holds that set -- and
             * then reused until a move is accepted.  A removal is not a bordered
             * system, so it keeps the per-proposal fit; so do models too large for
             * the batch to pay, and (batch < 0) any active set whose own Gram
             * sub-block will not factor, which is recorded so it is tried once. */
            int batched = 0;
            if (usebatch && !drop && nact <= GBQMAX && 2 * pnsel <= nvars)
            {
                if (!batch)
                    batch = (addbatch(&c, active, nact, &rssw) == 0) ? 1 : -1;
                if (batch > 0 && addsafe(&c, val))
                    batched = 1;
            }

            /* the same lazy build for the iterative families' shared weights, and
             * then for the panel that scores every addition from them */
            int cached = 0;
            int gbatched = 0;
            if (usecache && !drop)
            {
                if (!icache)
                {
                    irlscache(&c, active, nact);
                    icache = 1;
                }
                cached = 1;
                /* nact*nact <= ptot the amortisation condition: building the panel
                 * costs about n*q*ptot1 and each addition scored from it saves about
                 * one n*q*q fit, so the panel repays itself within q additions and
                 * the rest of the sweep is profit only while q*q stays under the
                 * candidate count.  It is what keeps the batch out of the block
                 * screening sweeps, where a handful of candidates and a chain that
                 * moves several times per sweep invalidate the panel long before it
                 * has paid for itself -- measured at 0.89x for poisson blocks of 60
                 * without this test, against 2.3x for the full sampler. */
                if (useglmb && nact <= GBQMAX && 2 * pnsel <= nvars && nact * nact <= ptot1)
                {
                    if (!gbatch)
                        gbatch = (irlsbatch(&c, active, nact) == 0) ? 1 : -1;
                    if (gbatch > 0 && irlssafe(&c, val))
                        gbatched = 1;
                }
            }

            inc[j] ^= 1;                       /* tentatively flip coordinate j */
            int pq;
            if (drop)
            {
                memmove(active + r, active + r + 1, (size_t) (nact - r - 1) * sizeof(int));
                pq = nact - 1;
            }
            else
            {
                memmove(active + r + 1, active + r, (size_t) (nact - r) * sizeof(int));
                active[r] = val;
                pq = nact + 1;
            }
            double propic;
            if (batched)
            {
                propic = addic(&c, val, pq, rssw);
                ok = 1;
            }
            else if (gbatched)
            {
                propic = irlsic(&c, val, pq);
                ok = 1;
            }
            else
            {
                propic = modelic(&c, active, pq, prop_maxit, &ok, cached);
            }

            /* Metropolis acceptance A = min(1, exp{k*(IC_cur - IC_prop)}):
             * always accept an improvement (IC_prop < IC_cur => A>=1), accept a
             * worsening move with prob shrinking in the IC increase. */
            int accept = 0;
            if (ok)
            {
                double A = exp(k * (curic - propic));
                if (A > 1.0) A = 1.0;
                if (UNIF(rng) < A) accept = 1;
                if (accept)
                {
                    /* Any proposal scored approximately is re-fitted directly here,
                     * so the recorded criterion and the collinearity verdict come
                     * from the exact factorisation.  Two scores are approximate: the
                     * single IRLS step of the iterative families, and a batched
                     * bordered update, which differs from a direct factorisation of
                     * the same model in the last bits.  One re-fit covers both -- a
                     * batched proposal of an iterative family must not be fitted
                     * twice.  If the direct fit is singular the families part ways:
                     * the gaussian batch needs a usable factor to continue, so the
                     * move is rejected, while an iterative family keeps the one-step
                     * score and the one-step coefficients in bprop.  A batch-scored
                     * acceptance has no one-step coefficients yet -- irlsic derives
                     * the criterion from the panel without ever writing bprop -- so
                     * its one-step fit is recovered here by the same call the
                     * per-proposal path makes, keeping the score and bprop on that
                     * convention; if even the one-step system is singular the
                     * proposal was never fittable and the move is rejected. */
                    if (prop_maxit != IRLS_MAXIT || batched || gbatched)
                    {
                        int ok2;
                        double exic = modelic(&c, active, pq, IRLS_MAXIT, &ok2, 0);
                        if (ok2)
                            propic = exic;
                        else if (family == FAM_GAUSSIAN)
                            accept = 0;
                        else if (gbatched)
                        {
                            int ok3;
                            double onic = modelic(&c, active, pq, prop_maxit, &ok3, cached);
                            if (ok3)
                                propic = onic;
                            else
                                accept = 0;
                        }
                    }
                }
                if (accept)
                {
                    curic = propic;
                    nsel   = pnsel;
                    nact   = pq;               /* the spliced list is now current */
                    batch  = 0;                /* the active set moved: batch stale */
                    icache = 0;                /* and so are the shared weights */
                    gbatch = 0;                /* and the panel built from them */
                    COMMIT_BETA(pq);
                }
            }
            if (!accept)
            {
                inc[j] ^= 1;                   /* reject: undo the flip */
                if (drop)                      /* and undo the splice */
                {
                    memmove(active + r + 1, active + r, (size_t) (nact - r - 1) * sizeof(int));
                    active[r] = val;
                }
                else
                {
                    memmove(active + r, active + r + 1, (size_t) (nact - r) * sizeof(int));
                }
            }
        }

        /* record the post-burn-in samples: indicator row, running inclusion
         * counts (for the marginal probabilities), and the current IC */
        if (sw >= len)
        {
            int row = sw - len;
            if (omat)
            {
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

    if (!ws->inS2 || !ws->S2 || !ws->S1 || !ws->assign || !ws->vfreq || !ws->seeds || !ws->arr || !ws->xs || !ws->s0 || !ws->fr || !ws->Xs)
    {
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
    for (int b = 0; b < h; b++)
    {
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
    for (int b = 0; b < h; b++)
    {
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
         * S2 columns]; the toggleable columns start empty (null start).  Gather
         * the columns into the workspace's reusable buffer. */
        for (int c = 0; c < pb; c++)
        {
            bcols[c] = S1[pos[off[b] + c]];
            s0[c]    = 0;
        }
        for (int c = 0; c < nS2; c++) bcols[pb + c] = S2[c];
        for (int c = 0; c < pb + nS2; c++)
            memcpy(Xb + (size_t) c * n, X + (size_t) bcols[c] * n, (size_t) n * sizeof(double));

        /* per-block private RNG (thread-safe), seeded deterministically; the
         * within-block sampler reports only the inclusion frequencies `fr` */
        rngt rng;
        rngseed(&rng, seeds[b]);
        int rc = rungibbs(y, Xb, pw, n, pb, nS2, s0, perm, len, k, gamma, p0, info, family, pb + nS2, &rng, NULL, fr, NULL, ws);
        if (rc)
        {
            #ifdef _OPENMP
            #pragma omp atomic write
            #endif
            fail = 1;
        }
        else
        {
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
    for (int i = 0; i < nS1; i++)
    {
        int a = (int) (unif_rand() * h);
        assign[i] = (a >= h) ? h - 1 : a;       /* guard the unif_rand()==1 edge */
    }
    for (int b = 0; b < h; b++)
    {
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

    /* standalone sampler: all p predictors are toggleable (p2 = 0) and the size
     * is capped at nvars.  The start model is empty (null start, intercept only),
     * so the zero-filled s0 is the start model.  rng = NULL means it uses R's RNG
     * on the main thread (serial). */
    int *s0 = (int *) calloc((size_t) p, sizeof(int));
    if (!s0) return 1;

    int fail = rungibbs(y, X, pw, n, p, 0, s0, perm, len, k, gamma, p, info, family, nvars, /*rng=*/NULL, mbuf, vpbuf, sicbuf, /*ws=*/NULL);
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
    for (int iter = 1; iter < niter && !fail; iter++)
    {
        /* S1 = all predictors not yet in S2 */
        int nS1 = 0;
        for (int j = 0; j < p; j++) if (!inS2[j]) S1[nS1++] = j;

        /* SCREEN: split S1 into h blocks, sample each in parallel, get vfreq[] */
        int h = nblks(nS1, H, n, nS2);
        drwblks(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = scrblks(y, X, pw, n, S1, nS1, S2, nS2, h, perm, len, k, gamma, p0, info, family, nthr, assign, ws.seeds, vfreq);
        if (fail) break;

        /* SELECT: the top-kapp S1 predictors by inclusion frequency, unioned
         * with S2, sorted ascending -> the candidate set xs of size ps */
        int kk = (kapp < nS1) ? kapp : nS1;
        fit *arr = ws.arr;
        for (int i = 0; i < nS1; i++)
        {
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
        for (int i = 0; i < ps; i++) s0[i] = 0;
        fail = rungibbs(y, Xs, pw, n, ps, 0, s0, perm, len, k, gamma, p0, info, family, ps, NULL, NULL, fr, NULL, NULL);
        if (fail) break;

        /* THRESHOLD: rebuild S2 from the candidates with fr > tau.  If no
         * candidate clears tau (cnt <= 1) we keep them all rather than empty S2,
         * so the important set never collapses to nothing between iterations. */
        int cnt = 0;
        for (int i = 0; i < ps; i++) if (fr[i] > tau) cnt++;
        memset(inS2, 0, (size_t) p * sizeof(int));
        nS2 = 0;
        for (int i = 0; i < ps; i++)
        {
            if (cnt > 1 ? (fr[i] > tau) : 1)
            {
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

    if (!fail)
    {
        int nS1 = 0;
        for (int j = 0; j < p; j++) if (!inS2[j]) S1[nS1++] = j;

        int h = nblks(nS1, H, n, nS2);
        drwblks(nS1, h, assign, ws.seeds);
        for (int j = 0; j < p; j++) vfreq[j] = 0.0;
        fail = scrblks(y, X, pw, n, S1, nS1, S2, nS2, h, perm, len, k, gamma, p0, info, family, nthr, assign, ws.seeds, vfreq);

        if (!fail)
        {
            int kk = (kapp < nS1) ? kapp : nS1;
            ps = kk + nS2;
            fit  *arr = ws.arr;
            int  *xs  = xsout;   /* build the candidate set directly into the caller buffer (ps <= p) */
            for (int i = 0; i < nS1; i++)
            {
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
    int    *s0 = R_Calloc((size_t) ps, int);   /* zeroed: the chain starts empty */
    double *fr = R_Calloc((size_t) ps, double);

    gathcols(X, n, xs, ps, Xs);
    int fail = rungibbs(y, Xs, pw, n, ps, 0, s0, perm, lenf, k, gamma, p, info, family, ps, NULL, omat, fr, oic, NULL);

    if (!fail)
    {
        /* marginal probs over all p columns: zero, then scatter the ps selected
         * (R is 1-based, so the recorded indices are xs[i] + 1) */
        for (int j = 0; j < p; j++) vprob[j] = 0.0;
        for (int i = 0; i < ps; i++)
        {
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

    if (family == FAM_GAUSSIAN)
    {
        /* weighted OLS: solve (D'WD) beta = D'Wy by Cholesky */
        double *G  = R_Calloc((size_t) p1 * p1, double);
        double *Gy = R_Calloc((size_t) p1, double);
        for (int a = 0; a < p1; a++)
        {
            const double *Daptr = D + (size_t) a * n;
            double sgy = 0.0;
            for (int i = 0; i < n; i++) sgy += Daptr[i] * pw[i] * y[i];
            Gy[a] = sgy;
            for (int c = a; c < p1; c++)
            {
                const double *Dcptr = D + (size_t) c * n;
                double s = 0.0;
                for (int i = 0; i < n; i++) s += Daptr[i] * pw[i] * Dcptr[i];
                G[a * p1 + c] = s;
                G[c * p1 + a] = s;
            }
        }
        if (cholsolv(G, Gy, bout, p1))
            for (int a = 0; a < p1; a++) bout[a] = 0.0;
        R_Free(G);
        R_Free(Gy);
    }
    else
    {
        double *wq    = R_Calloc((size_t) p1 * p1 + 3 * p1, double);
        double *wn    = R_Calloc((size_t) 5 * n, double);
        double *Dpack = R_Calloc((size_t) n * p1, double);
        double *Dw    = R_Calloc((size_t) n * p1, double);
        double dev2;
        if (glmirls(family, y, pw, D, active, n, p1, IRLS_MAXIT, wq, wn, Dpack, Dw, &dev2, NULL, bout, NULL))
            for (int a = 0; a < p1; a++) bout[a] = 0.0;
        R_Free(wq);
        R_Free(wn);
        R_Free(Dpack);
        R_Free(Dw);
    }

    /* a rank-deficient refit can slip past the pivot guard and yield non-finite
     * coefficients (NaN compares false against the tolerance); return zeros so
     * the caller never propagates NaN into predictions. */
    for (int a = 0; a < p1; a++)
    {
        if (!R_FINITE(bout[a]))
        {
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
 * and summtab are family-independent and shared with cox.c / lme.c.
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
    while (i < lenf)
    {
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
    for (int s = 0; s < lenf; s++)
    {
        a[s].ic  = oic[s];
        a[s].idx = s;
    }
    qsort(a, (size_t) lenf, sizeof(sicit), siccmp);
    int g = 0;
    int i = 0;
    while (i < lenf && g < nm_in)
    {
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
    if (!cnt || !rep || !act || !Xact || !bout)
    {
        free(cnt);
        free(rep);
        free(act);
        free(Xact);
        free(bout);
        return 1;
    }
    if (summtab(oic, lenf, nm_in, micout, cnt, rep, nm))
    {
        free(cnt);
        free(rep);
        free(act);
        free(Xact);
        free(bout);
        return 1;
    }
    int m = *nm;
    for (size_t t = 0; t < (size_t) nr * m; t++) coef[t] = 0.0;
    for (int i = 0; i < m; i++)
    {
        int q = 0;
        for (int c = 1; c <= ps; c++)
        {
            if (omat[(size_t) rep[i] + (size_t) c * lenf] == 1)
            {
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
