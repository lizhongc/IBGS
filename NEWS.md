# IBGS 1.0.1

## New features

* All six samplers (`glmGibbs`/`glmIBGS`, `coxGibbs`/`coxIBGS`,
  `rlmGibbs`/`rlmIBGS`) gain a `start = c("null", "full")` argument controlling
  the initial model of the Gibbs chain(s). The new default `"null"` starts from
  the empty model (intercept only; no covariates for the Cox model) and grows,
  avoiding the ill-conditioned full-model start where an information criterion —
  notably AICc, whose correction has denominator `n - npar - 1` — becomes
  degenerate as the model size approaches `n`. `start = "full"` restores the
  previous saturated start. **This changes the default behaviour:** results for a
  given seed differ from earlier versions unless `start = "full"` is set.

* `glmGibbs()`/`glmIBGS()` gain an opt-in `fast = FALSE` argument for the
  binomial/poisson families. With `fast = TRUE` each single-coordinate Gibbs
  proposal is scored with a single warm-started IRLS step (the warm start is one
  column from the current fit, so one Newton step is an accurate proposal score),
  and only the *accepted* models are re-fitted to full convergence. The recorded
  information criteria and coefficients therefore stay exact; only the
  accept/reject decision uses the cheap approximate score. The speedup grows with
  how much the iterative fit dominates the run (roughly 2x in fit-heavy regimes;
  little when the visited models are tiny). The gaussian family ignores it (its
  fit is direct and non-iterative).

## Performance

* The binomial/poisson IRLS fit (`glmirls`) now gathers the active columns into a
  contiguous design once per fit and assembles the weighted normal equations
  `D'WD`/`D'Wz` with the BLAS (`dsyrk`/`dgemv`) instead of hand-written strided
  loops, and tests convergence on the coefficient update rather than recomputing
  the deviance each iteration. The fitted values and information criteria still
  match R's `glm()`/`AIC()` (to its own tolerance); the gain on the per-fit
  linear algebra grows with the model size and sample size.
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
