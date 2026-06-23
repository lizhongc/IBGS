# The iterated block Gibbs sampler for the Cox model
#
# Variable selection for the Cox proportional-hazards model in ultrahigh
# dimensions.  This is the survival-analysis counterpart of
# glmIBGS(): the whole search (refinement iterations,
# random block screening and the final long run) is performed in a single
# multicore C routine (cox_ibgs_glm), and this R function is a thin wrapper
# that assembles the summary (refitting the top models in C).  Each candidate
# model is fitted by its Efron partial likelihood; the Cox model has no
# intercept, and the information criteria use the number of events as the
# effective sample size.
#
# Arguments:
#   y          the event/censoring time (a non-negative numeric vector)
#   status     the event indicator: 1 = event (death), 0 = censored
#   x          the predictors
#   n.refine   the number of refinement iterations, default is 3
#   n.models   the number of top selected models, default is 10
#   block.size the number of predictors in small groups, default is 30
#   n.keep     the number of selected predictors in the first step, default 20
#   threshold  the threshold to select the important predictors, default 0.9
#   permute    the coordinate visiting order within each Gibbs sweep. TRUE
#              (default) draws a fresh random permutation each sweep, so every
#              predictor is updated exactly once per sweep (without
#              replacement); FALSE uses a fixed in-order systematic sweep.
#   n.draws    the half number of generated samples, default is 250
#   inv.temp the tuning parameter, default is 1
#   ebic.gamma the parameter for extended BIC, default is 0.5
#   criterion  the model selection criterion: AIC, BIC, AICc or exBIC
#   weights    optional prior case weights (length nrow(x)); defaults
#              to all ones
#   n.cores    the number of OpenMP threads for the block screening
#              (0 = use all available cores), default is 1 (serial)
#   cor.check optional correlation threshold; NULL (default) skips the
#            check.  A scalar such as 0.9999 runs an O(p^2) scan of the predictors
#            and stops if any pair has absolute correlation above it (naming the
#            offenders), since near-duplicate columns destabilise the fits.
#
# Value: an object of class "IBGS" summarising the search, with
#   components marginal.prob (the marginal inclusion probability of each
#   predictor), selected.vars (the names of the predictors above the
#   threshold), var.names, coef (the coefficient matrix of the top
#   n.models models), model.ic and model.freq (the criterion
#   and visit frequency of each top model), ic.trace (the criterion at
#   every generation) and criterion (the criterion name).  Has
#   print, summary, coef and plot methods.
coxIBGS <- function(y, status, x, n.refine = 3, n.models = 10,
                                 block.size = 30, n.keep = 20, threshold = 0.9,
                                 permute = TRUE, n.draws = 250, inv.temp = 1,
                                 ebic.gamma = 0.5,
                                 criterion = c("AIC", "BIC", "AICc", "exBIC"),
                                 weights = NULL, n.cores = 1L, cor.check = NULL){
  criterion <- match.arg(criterion)
  info.code <- match(criterion, c("AIC", "BIC", "AICc", "exBIC")) - 1L

  x <- as.matrix(x)
  p <- ncol(x)
  var.names <- colnames(x)
  if (is.null(var.names)) var.names <- paste0("V", seq_len(p))

  if (length(status) != nrow(x)) stop("'status' must have length nrow(x)")
  if (is.null(weights)) weights <- rep(1, nrow(x))

  # optional fail-fast on near-duplicate covariates (cor.check = NULL skips it)
  if (!is.null(cor.check)) .check.high.cor(x, var.names, cor.check)

  # all of the search (refinement, parallel screening, final long run) happens
  # inside this single C call; see cox_ibgs_search() in src/cox_ibgs.c.
  out <- .Call("cox_ibgs_glm",
               y, status, x, weights,
               n.refine, block.size, n.keep, threshold, permute, n.draws,
               inv.temp, ebic.gamma, info.code, n.cores, n.models,
               PACKAGE = "IBGS")

  # the C call returns the model-averaging summary; the Cox model has no
  # intercept (has.intercept = FALSE)
  result <- .ibgs.object(out, inv.temp, criterion, "cox", FALSE)
  result$marginal.prob     <- out$v.prob
  result$selected.vars     <- var.names[out$v.prob > threshold]
  result$threshold         <- threshold
  result$var.names         <- var.names
  result$linear.predictors <- .ibgs.lp(result, x)
  return(result)
}
