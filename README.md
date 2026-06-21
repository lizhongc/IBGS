# IBGS

> Iterated Block Gibbs Sampler for ultrahigh-dimensional variable selection and model averaging

**IBGS** performs variable selection for generalized linear models and the Cox
proportional-hazards model when the number of predictors is very large. The
sampler is implemented in C with parallel block screening through OpenMP, and
returns a small set of high-scoring models together with marginal inclusion
probabilities and model-averaged predictions.

Methods are described in Chen (2022),
<https://hdl.handle.net/11343/311691>.

## Features

- **Families:** gaussian, binomial and poisson GLMs (least squares / IRLS), the
  Cox model for survival data (Efron partial likelihood), and robust / mixed
  linear models.
- **Selection criteria:** `AIC`, `BIC`, `AICc` and extended BIC (`exBIC`).
- **Two samplers per family:** an iterated block Gibbs sampler with
  screen–select–threshold *refinement* (`*IBGS`), and a plain block Gibbs
  sampler (`*Gibbs`).
- **Model averaging built in:** `predict()`, `fitted()` and `coef()` average
  over the retained top models with smooth-SIC (BMA-style) weights.
- **Diagnostics:** an I-chart of the criterion trace, marginal inclusion
  probabilities, and top-model visit frequencies.
- **Scales to large p:** the per-generation indicator matrix is summarized in C,
  so the fitted object stays compact even for thousands of predictors.

## Installation

The package is not on CRAN. A C compiler is required (OpenMP is used when
available for the parallel block screening).

From a local copy of the source:

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
summary(fit)              # selected-variable and top-model tables
plot(fit)                 # I-chart, marginal probabilities, model frequencies

coef(fit)                 # best model; coef(fit, average = TRUE) to average
predict(fit, x[1:5, ])    # model-averaged predictions on new data
fitted(fit)               # model-averaged fitted values
```

## Functions

| Model family | Iterated block Gibbs (with refinement) | Plain block Gibbs |
|---|---|---|
| GLM (gaussian / binomial / poisson) | `glmIBGS()` | `glmGibbs()` |
| Cox proportional hazards | `coxIBGS()` | `coxGibbs()` |
| Robust / mixed linear model | `rlmIBGS()` | `rlmGibbs()` |

Each sampler returns an object of class `"IBGS"` with `print()`, `summary()`,
`plot()`, `coef()`, `predict()` and `fitted()` methods. The plotting helpers
`plotIchart()`, `plotVarProb()` and `plotModelFreq()` are also exported for
drawing the individual diagnostics.

Common arguments include `criterion` (selection criterion), `n.models` (number
of top models to retain and average over), `threshold` (marginal-probability
cut-off for the reported selected variables), `inv.temp` (inverse temperature),
and `n.cores` (OpenMP threads for block screening). For survival data,
`coxIBGS(y, status, x)` takes the follow-up time `y` and the event indicator
`status`; for `rlmIBGS()`, `group`/`Z` specify the random-effects structure.

## Documentation

See the package help (`?glmIBGS`, `?coxIBGS`, `?predict.IBGS`, ...) and the
package vignette:

```r
vignette("IBGS")
```

## Reference

Chen, L. (2022). PhD thesis, The University of Melbourne.
<https://hdl.handle.net/11343/311691>

## License

GPL-3.
