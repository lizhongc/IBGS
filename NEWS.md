# IBGS 1.0.0

Initial CRAN release.

## Variable selection

* Iterated block Gibbs samplers with screen–select–threshold refinement
  (`glmIBGS()`, `coxIBGS()`, `lmeIBGS()`) and matching plain block Gibbs samplers
  (`glmGibbs()`, `coxGibbs()`, `lmeGibbs()`) for ultrahigh-dimensional problems.
* Families: gaussian, binomial and poisson generalized linear models (least
  squares / iteratively reweighted least squares), the Cox proportional-hazards
  model (Efron partial likelihood), and linear mixed models.
* Model selection by `AIC`, `BIC`, `AICc` or extended BIC (`exBIC`).

## Sampler controls

* `start = c("null", "full")` sets the initial model of the Gibbs chain(s); the
  default `"null"` starts from the empty model and grows, avoiding the
  ill-conditioned full-model start.
* `permute = TRUE` (the default) draws a fresh random coordinate permutation
  (Fisher–Yates) each Gibbs sweep, so every coordinate is updated exactly once per
  sweep; `permute = FALSE` restores a fixed in-order sweep.
* `glmGibbs()`/`glmIBGS()` accept an opt-in `fast = TRUE` for the binomial/poisson
  families, scoring each single-coordinate proposal with one warm-started IRLS step
  and re-fitting only accepted models to full convergence (reported criteria and
  coefficients stay exact).
* Parallel block screening through 'OpenMP' (`n.cores`) and an optional
  near-collinearity guard (`cor.check`).

## Model averaging

* Each fit retains the best `n.models` models, summarized in C so the returned
  object stays compact even for thousands of predictors.
* `predict()`, `fitted()` and `coef()` average over the retained models with
  smooth-SIC (BMA-style) weights, on the link or response scale; a single retained
  model can be selected with `average = FALSE`. Conditional prediction with
  random-effect BLUPs is available for `lme` fits.

## Object methods and diagnostics

* `print()` and `summary()` for the fit, with selected-variable and top-model
  tables and a convergence-diagnostics block.
* `plot()` dispatches to the diagnostics, also exported individually:
  `plotICtrace()` (criterion trace), `plotMargProb()` (marginal inclusion
  probabilities as a dot-and-whisker plot), `plotModelFreq()` (top-model visit
  frequencies), `plotGelman()` and `plotAutocorr()`. `plotMargProb()` and
  `plotModelFreq()` accept `horizontal = TRUE` for a horizontal layout.
* Convergence diagnostics computed in C: the Gelman–Rubin shrink-factor evolution,
  the Geweke z-statistic, the effective sample size, and the autocorrelation of the
  criterion trace.

## C backend

* The sampler is implemented in C with parallel block screening through 'OpenMP'.
