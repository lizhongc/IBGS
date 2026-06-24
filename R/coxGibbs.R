# The restricted Gibbs sampler for the Cox model
#
# The standalone (non-block) Metropolis-within-Gibbs sampler for the Cox
# proportional-hazards model that searches the model space directly over all
# predictors, with the model size capped at max.size.  This is the
# survival-analysis counterpart of glmGibbs(): every candidate
# Cox model is fitted by its Efron partial likelihood in a fast C routine
# (cox_gibbs_glm), and the summary is assembled by refitting the top
# models in C.  Intended for problems where the full design is
# manageable; use coxIBGS() in ultrahigh dimensions.
#
# Arguments:
#   y          the event/censoring time (a non-negative numeric vector)
#   status     the event indicator: 1 = event (death), 0 = censored
#   x          the predictors
#   max.size   the maximal number of predictors in a candidate model,
#              default is ncol(x)
#   permute    the coordinate visiting order within each Gibbs sweep. TRUE
#              (default) draws a fresh random permutation each sweep, so every
#              predictor is updated exactly once per sweep (without
#              replacement); FALSE uses a fixed in-order systematic sweep.
#   n.models   the number of top selected models, default is 10
#   threshold  the threshold to select the important predictors, default 0.9
#   n.draws    the half number of generated samples, default is 1000
#   inv.temp the tuning parameter, default is 1
#   ebic.gamma the parameter for extended BIC, default is 0.5
#   criterion  the model selection criterion: AIC, BIC, AICc or exBIC
#   weights    optional prior case weights (length nrow(x)); defaults
#              to all ones
#   cor.check optional correlation threshold; NULL (default) skips the
#            check.  A scalar such as 0.9999 runs an O(p^2) scan of the predictors
#            and stops if any pair has absolute correlation above it (naming the
#            offenders), since near-duplicate columns destabilise the fits.
#   start    the initial model for the Gibbs chain(s): "null" (default) starts
#            empty (no covariates; Cox has no intercept) and grows, avoiding the
#            ill-conditioned full-model start where the criterion (notably AICc)
#            can be degenerate; "full" starts from the saturated model.
#
# Value: an object of class "IBGS" summarising the search, with
#   components marginal.prob (the marginal inclusion probability of each
#   predictor), selected.vars (the names of the predictors above the
#   threshold), var.names, coef (the coefficient matrix of the top
#   n.models models), model.ic and model.freq (the criterion
#   and visit frequency of each top model), ic.trace (the criterion at
#   every generation) and criterion (the criterion name).  Has
#   print, summary, coef and plot methods.
coxGibbs <- function(y, status, x, max.size = ncol(x), permute = TRUE,
                            n.models = 10, threshold = 0.9, n.draws = 1000,
                            inv.temp = 1, ebic.gamma = 0.5,
                            criterion = c("AIC", "BIC", "AICc", "exBIC"),
                            weights = NULL, cor.check = NULL,
                            start = c("null", "full")){
  criterion <- match.arg(criterion)
  start     <- match.arg(start)
  info.code <- match(criterion, c("AIC", "BIC", "AICc", "exBIC")) - 1L
  start.code <- match(start, c("null", "full")) - 1L   # 0 = null, 1 = full

  x <- as.matrix(x)
  p <- ncol(x)
  var.names <- colnames(x)
  if (is.null(var.names)) var.names <- paste0("V", seq_len(p))

  if (length(status) != nrow(x)) stop("'status' must have length nrow(x)")
  if (is.null(weights)) weights <- rep(1, nrow(x))

  # optional fail-fast on near-duplicate covariates (cor.check = NULL skips it)
  if (!is.null(cor.check)) .check.high.cor(x, var.names, cor.check)

  # single, serial (non-block) Metropolis-within-Gibbs run over all p predictors
  out <- .Call("cox_gibbs_glm",
               y, status, x, weights, max.size, permute, start.code, n.draws, inv.temp,
               ebic.gamma, info.code, n.models,
               PACKAGE = "IBGS")

  result <- .ibgs.object(out, inv.temp, criterion, "cox", FALSE)
  result$marginal.prob     <- out$v.prob
  result$selected.vars     <- var.names[out$v.prob > threshold]
  result$threshold         <- threshold
  result$var.names         <- var.names
  result$linear.predictors <- .ibgs.lp(result, x)
  return(result)
}
