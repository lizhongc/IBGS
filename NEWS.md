# IBGS 1.0.1

## Performance

* The data-only normalising constant of the binomial/poisson `-2logLik` (the
  binomial coefficient `lgamma` terms / poisson `log(y!)`) is now computed once
  per run instead of on every candidate fit. It is the same for every model, so
  reported information criteria and variable selection are unchanged (still match
  R's `glm()`/`AIC()`), but the per-fit `lgamma` work is removed.
* The IRLS convergence tolerance for the binomial and poisson families was
  relaxed from `1e-10` to `1e-8` (R's `glm()` default), trimming a few unneeded
  iterations per fit. Variable selection is unchanged; fitted values match R's
  `glm()` to its own tolerance.

## Bug fixes

* `permute = TRUE` (the default) now performs a genuine random permutation of the
  predictors in every Gibbs sweep (Fisher–Yates), so each coordinate is updated
  exactly once per sweep, without replacement. Previously it drew coordinates
  independently and uniformly *with* replacement (an i.i.d. random scan), which
  visited only about 63% of the coordinates per sweep and was inconsistent with
  the documented "permutation" behaviour. Results for a given `set.seed()` will
  differ from 1.0.0.

# IBGS 1.0.0

Initial release.

## Variable selection

* Iterated block Gibbs samplers with screen–select–threshold refinement
  (`glmIBGS()`, `coxIBGS()`, `rlmIBGS()`) and matching plain block Gibbs samplers
  (`glmGibbs()`, `coxGibbs()`, `rlmGibbs()`) for ultrahigh-dimensional problems.
* Families: gaussian, binomial and poisson generalized linear models (least
  squares / iteratively reweighted least squares), the Cox proportional-hazards
  model (Efron partial likelihood), and robust / mixed linear models.
* Model selection by `AIC`, `BIC`, `AICc` or extended BIC (`exBIC`).
* C backend with parallel block screening through OpenMP (`n.cores`); an
  optional near-collinearity guard (`cor.check`).

## Model averaging

* Each fit retains the best `n.models` models, summarized in C so the returned
  object stays compact even for thousands of predictors.
* `predict()`, `fitted()` and `coef()` methods average over the retained models
  with smooth-SIC (BMA-style) weights, on the link or response scale; a single
  retained model can be selected with `average = FALSE`. Conditional prediction
  with random-effect BLUPs is available for `rlm` fits.

## Object methods and diagnostics

* `print()` and `summary()` for the fit, with selected-variable and top-model
  tables.
* `plot()` dispatches to three diagnostics, also exported individually:
  `plotIchart()` (criterion trace I-chart), `plotVarProb()` (marginal inclusion
  probabilities) and `plotModelFreq()` (top-model visit frequencies).
