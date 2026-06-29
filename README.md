# IBGS

> Iterated Block Gibbs Sampler for ultrahigh-dimensional variable selection and model averaging

**IBGS** performs variable selection for generalized linear models and the Cox
proportional-hazards model when the number of predictors is very large. The
sampler is implemented in C with parallel block screening through OpenMP, and
returns a small set of high-scoring models together with marginal inclusion
probabilities and model-averaged predictions.
Methods are described in Chen (2022).

## Features

- **Families:** gaussian, binomial and poisson GLMs (least squares / IRLS), the
  Cox model for survival data (Efron partial likelihood), and linear mixed
  models.
- **Selection criteria:** `AIC`, `BIC`, `AICc` and extended BIC (`exBIC`).
- **Two samplers per family:** an iterated block Gibbs sampler with
  screen–select–threshold *refinement* (`*IBGS`), and a plain block Gibbs
  sampler (`*Gibbs`).
- **Model averaging built in:** `predict()`, `fitted()` and `coef()` average
  over the retained top models with smooth-SIC (BMA-style) weights.
- **Diagnostics:** a trace of the criterion sequence, marginal inclusion
  probabilities, and top-model visit frequencies.
- **Convergence diagnostics:** the Gelman–Rubin shrink-factor evolution, the Geweke
  z-statistic, the effective sample size, and the autocorrelation of the
  criterion trace.
- **Scales to large p:** the per-generation indicator matrix is summarized in C,
  so the fitted object stays compact even for thousands of predictors.

## Installation

Install the released version from CRAN:

```r
install.packages("IBGS")
```

A C compiler is required to build from source (OpenMP is used when available for
the parallel block screening). To install the development version from a local
copy of the source:

```sh
R CMD INSTALL IBGS
```

or, from within R:

```r
# install.packages("remotes")
remotes::install_local("IBGS")
```

## Quick start

```r
library(IBGS)

## 50 predictors, only the first three are active
x <- matrix(rnorm(100 * 50), 100, 50)
y <- rowSums(x[, 1:3]) + rnorm(100)

fit <- glmIBGS(y, x, criterion = "BIC")
fit                       # concise overview
summary(fit)              # selected-variable, top-model tables and convergence diagnostics
plot(fit)                 # criterion trace, marginal probabilities, model frequencies, R_hat, autocorrelation

coef(fit)                 # best model; coef(fit, average = TRUE) to average
predict(fit, x[1:5, ])    # model-averaged predictions on new data
fitted(fit)               # model-averaged fitted values
```

## Functions

| Model family | Iterated block Gibbs (with refinement) | Plain block Gibbs |
|---|---|---|
| GLM (gaussian / binomial / poisson) | `glmIBGS()` | `glmGibbs()` |
| Cox proportional hazards | `coxIBGS()` | `coxGibbs()` |
| Linear mixed model | `lmeIBGS()` | `lmeGibbs()` |

Each sampler returns an object of class `"IBGS"` with `print()`, `summary()`,
`plot()`, `coef()`, `predict()` and `fitted()` methods. The plotting helpers
`plotICtrace()`, `plotMargProb()`, `plotModelFreq()`, `plotGelman()` and
`plotAutocorr()` are also exported for drawing the individual diagnostics.

Common arguments include `criterion` (selection criterion), `n.models` (number
of top models to retain and average over), `threshold` (marginal-probability
cut-off for the reported selected variables), `inv.temp` (inverse temperature),
and `n.cores` (OpenMP threads for block screening). For survival data,
`coxIBGS(y, status, x)` takes the follow-up time `y` and the event indicator
`status`; for `lmeIBGS()`, `group`/`Z` specify the random-effects structure.

## Documentation

See the package help (`?glmIBGS`, `?coxIBGS`, `?predict.IBGS`, ...) and the
package vignette:

```r
vignette("IBGS")
```

## Reference

Chen, L. (2022). Model selection and averaging by Gibbs sampler with a tropical cyclone seasonal forecasting application, PhD thesis, The University of Melbourne.
<https://hdl.handle.net/11343/311691>

## License

GPL-3.
