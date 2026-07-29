# IBGS 1.1.1

## Bug fixes

* **A batch-scored acceptance whose exact re-fit is singular now recovers its own
  one-step fit.** The batched scorer for the binomial and poisson families derives a
  proposal's criterion from the bordered panel without fitting coefficients. When such
  a proposal was accepted and its exact re-fit turned out singular, the sampler
  committed the coefficient vector left over from the last successfully fitted model —
  a different model's coefficients, mapped positionally onto the accepted model's
  columns — and every subsequent one-step score warm-started from that state until the
  next successful re-fit. The sampler now recovers the accepted model's own one-step
  fit through the same call the per-proposal path makes, keeping the convention that an
  iterative family falls back to its one-step score and coefficients; if even the
  one-step system is singular the move is rejected. The condition requires a candidate
  to pass the batch's relative pivot margin while the enlarged model fails the absolute
  pivot floor — for example a genuinely predictive column of very small magnitude — and
  cannot arise for the gaussian family, whose batched acceptances were already revoked
  on a singular re-fit. Output is unchanged on every design where the condition never
  fires. A regression test (`tests/test-refit.R`) pins the corrected behaviour.

# IBGS 1.1.0

## Performance

An optimisation pass over the Metropolis-within-Gibbs sweep. No interface changes and
no change to what any criterion reports: the samplers do the same search without
repeating work that a single-coordinate proposal cannot have changed.

* **The active-column list is spliced rather than rebuilt.** The sweep used to
  re-derive its sorted list of active columns from the inclusion vector on every
  proposal, a scan over all `p` candidates, although a single-coordinate flip changes
  that list in one position. The coordinate is now spliced in or out at its sorted
  position, making the per-proposal bookkeeping linear in the model size instead of in
  `p`. Applied to the gaussian/binomial/poisson sweep and to the linear-mixed sweep,
  which carried a duplicate of the same code.
* **The gaussian sampler scores every candidate addition in one batch.** Adding a
  column borders the current Gram sub-block, so its Cholesky factor extends the current
  one by a single row and column and the only per-candidate work is one triangular
  solve; stacking all candidates turns that into a single BLAS-3 `dtrsm`. A sweep visits
  every candidate but changes the model only when a move is accepted, which for a
  criterion that fits the data is well under once per sweep, so one factorisation serves
  the whole sweep.
* **The binomial and poisson samplers share the first Newton step's weights.** A
  proposed column warm-starts at zero, so the step's linear predictor is the current
  model's whatever column is being added, and the mean, IRLS weight and working
  response derived from it are the same for every candidate. They are now computed once
  per accepted move rather than once per proposal.
* **Their normal equations are batched on the same principle.** In those fixed weights,
  adding a column borders the current model's weighted system exactly as in the
  gaussian case, so one Cholesky and two triangular solves give every candidate's
  coefficients, and each candidate's linear predictor follows as a rank-one update of
  the current model's. Unlike the gaussian case the criterion does not fall out of the
  algebra: the deviance still needs the proposal's own linear predictor and a pass over
  the observations.

Each batch is used only where it repays its own construction and is well conditioned:
for models at most half the size cap, for at most 64 coefficients, for candidate
columns not already explained to within 1e-3 by the current model, and -- for the
iterative families, whose panel is more expensive to build -- only while the squared
model size stays below the candidate count, which keeps it out of the short
block-screening sweeps of `glmIBGS()`. Every removal, the Cox sampler, and any run
gated out by those conditions keep their existing path.

## Reproducibility of this release

The spliced list and the shared IRLS weights are bit-identical by construction. The
spliced list is in the same ascending order the rescan produced, so every candidate fit
sees the same matrix, and the cached weights are the values the per-proposal code
computed.

The two batched scorers compute the same quantities by a different arithmetic path and
so are not guaranteed bit-identical, although in testing they were. Accepted models are
re-fitted directly, as the iterative families already were, so every recorded criterion
and every collinearity verdict still comes from the same factorisation as before and
only an accept/reject decision can move -- and only where the two agree to within
rounding of a Metropolis threshold, after which the chain is a different realisation of
the same target.

## Internal

* Fixed a latent double fit in the accept path: a proposal both scored by one IRLS step
  and scored from a batch would have been re-fitted twice. The two re-fit branches are
  now one, keeping each family's convention -- the gaussian batch rejects a model the
  direct fit will not take, since it needs a usable factor to continue, while an
  iterative family keeps the one-step score as it always has. No released version could
  reach this, batched scoring having been gaussian-only and gaussian never taking the
  one-step path.

# IBGS 1.0.1

## Model selection criteria

* `ebic.gamma` now defaults to 1, the value Chen & Chen (2012) show to be
  selection-consistent for any `p = O(n^kappa)`; it was 0.5, which qualifies only
  while `p < n` and so was below the threshold in the ultrahigh-dimensional
  problems the package targets.
* A supplied `ebic.gamma` is held in `[max(0, 1 - log(n)/(2*log(p))), 1]` — a value
  below that consistency floor is raised to it, one above 1 lowered, with a message
  reporting the change. The floor uses the event count for the Cox samplers,
  matching the `log(d)` in their own penalty.
* The `exBIC` documentation now cites Chen & Chen (2012), *Extended BIC for
  small-n-large-P sparse GLM*, whose `2 nu(s) gamma log P` form is the one
  implemented; the 2008 Biometrika paper it previously cited uses the
  binomial-coefficient form. Both are listed in the package references.

## Sampler controls

* `start = c("null", "full")` removed. Every chain starts from the empty model,
  which was already the default and avoids the ill-conditioned saturated start
  where the criterion (notably AICc) can be degenerate.
* Fixed: the final long run of `coxIBGS()` and `lmeIBGS()` ignored `start` and
  began from the full candidate set. Under heavy censoring that set can reach the
  event count `d`, where the AICc correction `2*npred*(npred+1)/(d - npred - 1)`
  changes sign and a model saturated in the events scores far below any honest
  one, trapping the chain. Those runs now start empty like every other chain.
* `fast` removed from `glmGibbs()`/`glmIBGS()`. Binomial and poisson proposals are
  always scored with one warm-started IRLS step, with accepted models re-fitted to
  full convergence — the former default, and strictly faster than the alternative
  for the same reported criteria.

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
