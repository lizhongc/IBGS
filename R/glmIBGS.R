# The iterated block Gibbs sampler algorithm
#
# Variable selection for the generalized linear model in ultrahigh dimensions.
# The whole search -- the refinement iterations, the random block screening,
# and the final long run -- is performed in a single multicore C routine
# (ibgs_glm); this R function is a thin wrapper that assembles the result
# summary (refitting the top models in C to recover their coefficients).  The
# gaussian family is fitted by ordinary least squares; the binomial and poisson
# families by IRLS.
#
# Arguments:
#   y          the response variable
#   x          the predictors
#   block.size the number of predictors in small groups, default is 30
#   n.keep     the number of selected predictors in first step, default is 20
#   threshold  the threshold to select the important predictors in second step, default is 0.9
#   permute    the permutation of Gibbs sampler, default is TRUE
#   n.draws    the half number of generated samples, default is 250
#   inv.temp the tuning parameter, default is 1
#   ebic.gamma the parameter for extended BIC, default is 0.5
#   criterion  the selected model selection criterion from AIC, AICc, BIC and exBIC
#   family     the model family: "gaussian", "binomial" or "poisson"
#   weights    optional prior weights (length nrow(x)); as in
#              glm().  For the binomial family y may be a fraction
#              (proportion of successes) with weights the number of trials.
#              Defaults to all ones.
#   n.refine   the number of refinement iterations
#   n.models   the number of top selected models
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
glmIBGS <- function(y, x, n.refine = 3, n.models = 10, block.size = 30,
                              n.keep = 20, threshold = 0.9, permute = TRUE,
                              n.draws = 250, inv.temp = 1, ebic.gamma = 0.5,
                              criterion = c("AIC", "BIC", "AICc", "exBIC"),
                              family = c("gaussian", "binomial", "poisson"),
                              weights = NULL, n.cores = 1L, cor.check = NULL){
  criterion <- match.arg(criterion)
  family    <- match.arg(family)
  # map the string options to the 0-based integer codes the C layer expects
  info.code   <- match(criterion, c("AIC", "BIC", "AICc", "exBIC")) - 1L
  family.code <- match(family, c("gaussian", "binomial", "poisson")) - 1L

  x <- as.matrix(x)
  p <- ncol(x)
  # remember the predictor names (for reporting); fall back to V1..Vp.  The C
  # code identifies predictors by integer column index, never by name.
  var.names <- colnames(x)
  if (is.null(var.names)) var.names <- paste0("V", seq_len(p))

  if (is.null(weights)) weights <- rep(1, nrow(x))   # unweighted by default

  # optional fail-fast on near-duplicate covariates (cor.check = NULL skips it)
  if (!is.null(cor.check)) .check.high.cor(x, var.names, cor.check)

  # all of the search (refinement, parallel screening, final long run) happens
  # inside this single C call; see ibgs_search() / ibgs_glm in src/.
  out <- .Call("ibgs_glm",
               y, x, weights,
               n.refine, block.size, n.keep, threshold, permute, n.draws,
               inv.temp, ebic.gamma, info.code, family.code, n.cores, n.models,
               PACKAGE = "IBGS")

  # the C call already returns the model-averaging summary (coef / model.ic /
  # model.freq / ic.trace); assemble the object and attach the marginal
  # inclusion probabilities (all p predictors) and the averaged fitted values
  result <- .ibgs.object(out, inv.temp, criterion, family, TRUE)
  result$marginal.prob     <- out$v.prob
  result$selected.vars     <- var.names[out$v.prob > threshold]
  result$threshold         <- threshold
  result$var.names         <- var.names
  result$linear.predictors <- .ibgs.lp(result, x)
  return(result)
}
