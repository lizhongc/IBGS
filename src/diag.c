/*
 * diag.c -- convergence diagnostics for the recorded information-criterion
 * sequence (ic.trace) of an IBGS run.
 *
 * Self-contained: it reimplements the handful of coda (Plummer et al.)
 * diagnostics the package reports -- the autoregressive spectral density at zero
 * frequency (spectrum0.ar), the lagged autocorrelations (acf), the effective
 * sample size, the Geweke z-statistic, and the split-chain Gelman-Rubin
 * potential scale reduction factor -- so the package depends on nothing beyond
 * base R.  Every routine here operates on a single plain double array; the
 * .Call shim (ibgs_diag, in R_export.c) packs the results into the list stored
 * on the "IBGS" object.
 *
 * The formulas mirror coda and R's stats::ar.yw so the numbers agree with the
 * standard tools; tests/test-convergence.R cross-checks them against
 * stats::ar and stats::acf.
 */
#include "ibgs.h"

/* arithmetic mean of x[0..n-1] */
static double mean_d(const double *x, int n)
{
    double s = 0.0;
    for (int i = 0; i < n; i++)
    {
        s += x[i];
    }
    return s / n;
}

/* sample variance of x[0..n-1] (denominator n-1, as R's var) */
static double var_d(const double *x, int n)
{
    if (n < 2)
    {
        return NA_REAL;
    }
    double m = mean_d(x, n);
    double s = 0.0;
    for (int i = 0; i < n; i++)
    {
        double d = x[i] - m;
        s += d * d;
    }
    return s / (n - 1);
}

/* sample covariance of a[0..n-1], b[0..n-1] (denominator n-1, as R's cov) */
static double cov_d(const double *a, const double *b, int n)
{
    if (n < 2)
    {
        return NA_REAL;
    }
    double ma = mean_d(a, n);
    double mb = mean_d(b, n);
    double s = 0.0;
    for (int i = 0; i < n; i++)
    {
        s += (a[i] - ma) * (b[i] - mb);
    }
    return s / (n - 1);
}

/*
 * Biased autocovariances out[k] = (1/n) sum_t (x_t - xbar)(x_{t+k} - xbar) for
 * k = 0..maxlag, matching R's acf(type = "covariance") (divisor n, demeaned by
 * the whole-series mean).
 */
static void autocov(const double *x, int n, int maxlag, double *out)
{
    double m = mean_d(x, n);
    for (int k = 0; k <= maxlag; k++)
    {
        double s = 0.0;
        for (int t = 0; t + k < n; t++)
        {
            s += (x[t] - m) * (x[t + k] - m);
        }
        out[k] = s / n;
    }
}

/*
 * Lagged autocorrelations out[k] = acov[k] / acov[0], k = 0..lag_max (so
 * out[0] = 1).  Mirrors stats::acf.  out must hold lag_max+1 doubles; the caller
 * has already clamped lag_max to at most n-1.  A constant series yields NA.
 */
void acf_vec(const double *x, int n, int lag_max, double *out)
{
    double *c = R_Calloc((size_t) (lag_max + 1), double);
    autocov(x, n, lag_max, c);
    double c0 = c[0];
    for (int k = 0; k <= lag_max; k++)
    {
        out[k] = (c0 > 0.0) ? c[k] / c0 : NA_REAL;
    }
    R_Free(c);
}

/*
 * Spectral density at zero frequency via an autoregressive fit, matching
 * coda::spectrum0.ar / stats::ar(aic = TRUE, method = "yule-walker"):
 *   - order.max = floor(10 * log10(n)) (clamped to [1, n-1]);
 *   - biased autocovariances feed the Levinson-Durbin recursion, giving the
 *     prediction-error variance E_k and AR coefficients at every order;
 *   - the order minimising AIC = n*log(E_k) + 2k is chosen;
 *   - var.pred = E_order * n/(n-order-1) and spec = var.pred / (1 - sum(ar))^2.
 * A constant series returns 0 (as coda).  *order, if non-NULL, receives the
 * selected AR order.
 */
double spectrum0_ar(const double *x, int n, int *order)
{
    if (n < 2)
    {
        return NA_REAL;
    }
    int om = (int) floor(10.0 * log10((double) n));
    if (om > n - 1)
    {
        om = n - 1;
    }
    if (om < 1)
    {
        om = 1;
    }

    double *r = R_Calloc((size_t) (om + 1), double);
    autocov(x, n, om, r);
    double c0 = r[0];
    if (!(c0 > 0.0))                 /* constant series: spec 0, order 0 */
    {
        R_Free(r);
        if (order != NULL)
        {
            *order = 0;
        }
        return 0.0;
    }

    double *E = R_Calloc((size_t) (om + 1), double);
    double *a = R_Calloc((size_t) om, double);
    double *aprev = R_Calloc((size_t) om, double);
    double *acoef = R_Calloc((size_t) om * (size_t) om, double);

    /* Levinson-Durbin: prediction-error variance and coefs for each order */
    E[0] = c0;
    int kmax = om;
    for (int k = 1; k <= om; k++)
    {
        double acc = r[k];
        for (int j = 1; j <= k - 1; j++)
        {
            acc -= aprev[j - 1] * r[k - j];
        }
        double kappa = acc / E[k - 1];
        a[k - 1] = kappa;
        for (int j = 1; j <= k - 1; j++)
        {
            a[j - 1] = aprev[j - 1] - kappa * aprev[k - 1 - j];
        }
        E[k] = E[k - 1] * (1.0 - kappa * kappa);
        for (int j = 0; j < k; j++)
        {
            acoef[(size_t) (k - 1) * om + j] = a[j];
        }
        memcpy(aprev, a, (size_t) k * sizeof(double));
        if (!(E[k] > 0.0))           /* numerically degenerate: stop here */
        {
            kmax = k - 1;
            break;
        }
    }

    /* AIC order selection on the unscaled prediction variances */
    int sel = 0;
    double best = (double) n * log(E[0]);
    for (int k = 1; k <= kmax; k++)
    {
        if (!(E[k] > 0.0))
        {
            continue;
        }
        double aic = (double) n * log(E[k]) + 2.0 * k;
        if (aic < best)
        {
            best = aic;
            sel = k;
        }
    }

    double evar = E[sel] * (double) n / (double) (n - (sel + 1));
    double sumar = 0.0;
    if (sel > 0)
    {
        const double *ac = &acoef[(size_t) (sel - 1) * om];
        for (int j = 0; j < sel; j++)
        {
            sumar += ac[j];
        }
    }
    double denom = 1.0 - sumar;
    double spec = evar / (denom * denom);

    if (order != NULL)
    {
        *order = sel;
    }
    R_Free(acoef);
    R_Free(aprev);
    R_Free(a);
    R_Free(E);
    R_Free(r);
    return spec;
}

/*
 * Effective sample size = n * var(x) / spectrum0.ar(x) (coda::effectiveSize).
 * Returns 0 for a constant series, NA for too-short input.
 */
double ess_val(const double *x, int n)
{
    if (n < 2)
    {
        return NA_REAL;
    }
    double v = var_d(x, n);
    double spec = spectrum0_ar(x, n, NULL);
    if (!(spec > 0.0))
    {
        return 0.0;
    }
    return (double) n * v / spec;
}

/*
 * Geweke z-statistic comparing the first frac1 and last frac2 of the chain
 * (coda::geweke.diag).  The windows follow coda/window.mcmc indexing on the
 * default 1..n time grid:
 *   window 1 = x[0 .. floor(frac1*(n-1))]            (n1 points)
 *   window 2 = x[ceil(frac2*(n-1)) .. n-1]           (n2 points)
 * z = (mean1 - mean2) / sqrt(S1(0)/n1 + S2(0)/n2), each S(0) a spectrum0.ar.
 */
double geweke_z(const double *x, int n, double frac1, double frac2)
{
    if (n < 4)
    {
        return NA_REAL;
    }
    int n1 = (int) floor(frac1 * (n - 1)) + 1;
    int start2 = (int) ceil(frac2 * (n - 1));
    int n2 = n - start2;
    if (n1 < 2 || n2 < 2)
    {
        return NA_REAL;
    }
    const double *w1 = x;
    const double *w2 = x + start2;
    double m1 = mean_d(w1, n1);
    double m2 = mean_d(w2, n2);
    double s1 = spectrum0_ar(w1, n1, NULL) / n1;
    double s2 = spectrum0_ar(w2, n2, NULL) / n2;
    double denom = sqrt(s1 + s2);
    if (!(denom > 0.0))
    {
        return NA_REAL;
    }
    return (m1 - m2) / denom;
}

/*
 * Univariate Gelman-Rubin potential scale reduction factor from a single chain
 * split into m equal contiguous segments (split-Rhat).  Mirrors coda::gelman.diag
 * with transform = FALSE, autoburnin = FALSE, confidence = 0.95:
 *   W within-chain variance, B between-chain variance, pooled V, a Student-t
 *   degrees-of-freedom adjustment, and the upper credible limit from an F
 *   quantile.  Writes the point estimate to *psrf and the 97.5% upper limit to
 *   *upper (both NA when the chain is too short or degenerate).
 */
void gelman1d(const double *x, int n, int m, double *psrf, double *upper)
{
    *psrf = NA_REAL;
    *upper = NA_REAL;
    if (m < 2)
    {
        return;
    }
    int L = n / m;                      /* iterations per segment */
    if (L < 2)
    {
        return;
    }
    double Niter = (double) L;
    double Nchain = (double) m;

    double *xbar = R_Calloc((size_t) m, double);
    double *xbar2 = R_Calloc((size_t) m, double);
    double *s2 = R_Calloc((size_t) m, double);
    for (int j = 0; j < m; j++)
    {
        const double *seg = x + (size_t) j * L;
        double mu = mean_d(seg, L);
        double ss = 0.0;
        for (int t = 0; t < L; t++)
        {
            double d = seg[t] - mu;
            ss += d * d;
        }
        xbar[j] = mu;
        xbar2[j] = mu * mu;
        s2[j] = ss / (L - 1);
    }

    double w = mean_d(s2, m);                       /* W */
    double muhat = mean_d(xbar, m);
    double b = Niter * var_d(xbar, m);              /* B */
    if (!(w > 0.0))
    {
        R_Free(s2);
        R_Free(xbar2);
        R_Free(xbar);
        return;
    }

    double var_w = var_d(s2, m) / Nchain;
    double var_b = 2.0 * b * b / (Nchain - 1.0);
    double cov_wb = (Niter / Nchain)
        * (cov_d(s2, xbar2, m) - 2.0 * muhat * cov_d(s2, xbar, m));

    double V = (Niter - 1.0) * w / Niter + (1.0 + 1.0 / Nchain) * b / Niter;
    double var_V = ((Niter - 1.0) * (Niter - 1.0) * var_w
        + (1.0 + 1.0 / Nchain) * (1.0 + 1.0 / Nchain) * var_b
        + 2.0 * (Niter - 1.0) * (1.0 + 1.0 / Nchain) * cov_wb)
        / (Niter * Niter);
    double df_V = (2.0 * V * V) / var_V;
    double df_adj = (df_V + 3.0) / (df_V + 1.0);
    double B_df = Nchain - 1.0;
    double W_df = (2.0 * w * w) / var_w;

    double R2_fixed = (Niter - 1.0) / Niter;
    double R2_random = (1.0 + 1.0 / Nchain) * (1.0 / Niter) * (b / w);
    double R2_est = R2_fixed + R2_random;
    *psrf = sqrt(df_adj * R2_est);

    if (var_w > 0.0 && R_FINITE(W_df))
    {
        double q = qf(0.975, B_df, W_df, 1, 0);
        double R2_upper = R2_fixed + q * R2_random;
        *upper = sqrt(df_adj * R2_upper);
    }

    R_Free(s2);
    R_Free(xbar2);
    R_Free(xbar);
}

/*
 * Evolving Gelman-Rubin shrink factor for gelman.plot: gelman1d evaluated on
 * growing prefixes of the chain.  Endpoints E grow linearly from m*10 points (so
 * each of the m segments has at least ~10 iterations) up to n, over at most nbin
 * breakpoints.  Writes the prefix length, point estimate and upper limit of each
 * usable breakpoint to iters/med/upper (caller-sized to nbin) and the number
 * written to *nb.  Passing iters == NULL (md/up ignored) performs a count-only
 * pass: it skips the stores but still reports the usable breakpoint count in *nb,
 * so a caller can size exact-length outputs before a second, filling call.
 */
void gelman_shrink(const double *x, int n, int m, int nbin, double *iters, double *med, double *upper, int *nb)
{
    *nb = 0;
    if (m < 2 || nbin < 1)
    {
        return;
    }
    int emin = m * 10;
    if (emin > n)
    {
        emin = m * 2;
    }
    if (emin > n)
    {
        return;
    }

    int count = 0;
    for (int i = 0; i < nbin; i++)
    {
        int E;
        if (nbin == 1)
        {
            E = n;
        }
        else
        {
            E = emin + (int) ((double) (n - emin) * i / (nbin - 1) + 0.5);
        }
        double p, u;
        gelman1d(x, E, m, &p, &u);
        if (R_FINITE(p))
        {
            if (iters != NULL)
            {
                iters[count] = (double) E;
                med[count] = p;
                upper[count] = R_FINITE(u) ? u : NA_REAL;
            }
            count++;
        }
    }
    *nb = count;
}
