# The iterated block Gibbs sampler for the linear mixed model (fixed-effect part)
#
# Fixed-effect variable selection for a linear mixed model in ultrahigh
# dimensions, holding the random part fixed.  This is the mixed-model
# counterpart of glmIBGS(): the whole search (refinement iterations,
# random block screening and the final long run) runs in a single multicore C
# routine on the whitened data.  With the random structure held the marginal
# model is y ~ N(X beta, V) with V fixed; whitening by
# V = L Lt reduces each candidate fit to ordinary least squares.  The
# variance component(s) are estimated once (random-intercept case, by REML from
# the largest estimable fixed model) or supplied, and held fixed.
#
# Specify the random part with exactly one of: group (a grouping factor
# for a random intercept), Z + varcomp, or a marginal covariance
# V.
#
# Arguments:
#   y          the response variable
#   x          the fixed-effect predictors
#   group      a grouping factor (length nrow(x)) for a random
#              intercept
#   Z          a random-effects design matrix (general random part)
#   varcomp    a list list(sigma2b, sigma2e) of variance components
#              used with Z
#   V          the marginal covariance V (n x n), supplied directly
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
#   n.cores    the number of OpenMP threads for the block screening
#              (0 = use all available cores), default is 1 (serial)
#   cor.check optional correlation threshold; NULL (default) skips the
#            check.  A scalar such as 0.9999 stops if any pair of predictors has
#            absolute correlation above it.
#   start    the initial model for the Gibbs chain(s): "null" (default) starts
#            empty (intercept only) and grows, avoiding the ill-conditioned
#            full-model start where the criterion (notably AICc) can be
#            degenerate; "full" starts from the saturated model.
#
# Value: an object of class "IBGS" summarising the search, with
#   components marginal.prob (the marginal inclusion probability of each
#   predictor), selected.vars (the names of the predictors above the
#   threshold), var.names, coef (the coefficient matrix of the top
#   n.models models), model.ic and model.freq (the criterion
#   and visit frequency of each top model), ic.trace (the criterion at
#   every generation), criterion (the criterion name) and, when the random part
#   is given via group or Z + varcomp, re (the random-effect BLUPs used for
#   conditional prediction; a directly supplied V gives the fit without re).
#   Has print, summary, coef and plot methods.
rlmIBGS <- function(y, x, group = NULL, Z = NULL, varcomp = NULL, V = NULL,
                    n.refine = 3, n.models = 10, block.size = 30, n.keep = 20,
                    threshold = 0.9, permute = TRUE, n.draws = 250,
                    inv.temp = 1, ebic.gamma = 0.5,
                    criterion = c("AIC", "BIC", "AICc", "exBIC"), n.cores = 1L,
                    cor.check = NULL, start = c("null", "full")){
  criterion <- match.arg(criterion)
  start     <- match.arg(start)
  info.code <- match(criterion, c("AIC", "BIC", "AICc", "exBIC")) - 1L
  start.code <- match(start, c("null", "full")) - 1L   # 0 = null, 1 = full

  x <- as.matrix(x)
  p <- ncol(x)
  var.names <- colnames(x)
  if (is.null(var.names)) var.names <- paste0("V", seq_len(p))
  storage.mode(x) <- "double"

  if (!is.null(cor.check)) .check.high.cor(x, var.names, cor.check)

  # whiten by the held marginal covariance (estimating variance components once)
  wh <- .rlm.whiten(y, x, group, Z, varcomp, V)

  out <- .Call("rlm_ibgs_glm",
               wh$ystar, wh$xstar, wh$istar,
               n.refine, block.size, n.keep, threshold, permute, start.code, n.draws,
               inv.temp, ebic.gamma, info.code, wh$logdetV0, n.cores, n.models,
               PACKAGE = "IBGS")

  .rlm.result(out, y, x, var.names, wh, inv.temp, criterion, threshold)
}
