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
