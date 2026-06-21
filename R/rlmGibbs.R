# The restricted Gibbs sampler for the linear mixed model (fixed-effect part)
#
# The standalone (non-block) Metropolis-within-Gibbs sampler that selects the
# fixed effects of a linear mixed model while holding the random part fixed.
# With the random structure held, the marginal model is y ~ N(X beta, V)
# with V fixed; whitening by V = L Lt reduces each candidate fit to
# ordinary least squares on the whitened data, performed in a fast C routine.
# The variance component(s) are estimated once (random-intercept case) or
# supplied, and held fixed across all candidate models.  This is the mixed-model
# counterpart of glmGibbs(); use rlmIBGS() in ultrahigh
# dimensions.
#
# Specify the random part with exactly one of: group (a grouping factor
# for a random intercept; its variance ratio is estimated once by REML),
# Z + varcomp, or a marginal covariance V.
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
#   max.size   the maximal number of fixed predictors in a candidate model,
#              default is ncol(x)
#   permute    the permutation of the Gibbs sampler, default is TRUE
#   n.models   the number of top selected models, default is 10
#   threshold  the threshold to select the important predictors, default 0.9
#   n.draws    the half number of generated samples, default is 1000
#   inv.temp the tuning parameter, default is 1
#   ebic.gamma the parameter for extended BIC, default is 0.5
#   criterion  the model selection criterion: AIC, BIC, AICc or exBIC
#   cor.check optional correlation threshold; NULL (default) skips the
#            check.  A scalar such as 0.9999 stops if any pair of predictors has
#            absolute correlation above it.
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
rlmGibbs <- function(y, x, group = NULL, Z = NULL, varcomp = NULL, V = NULL,
                     max.size = ncol(x), permute = TRUE, n.models = 10,
                     threshold = 0.9, n.draws = 1000, inv.temp = 1,
                     ebic.gamma = 0.5,
                     criterion = c("AIC", "BIC", "AICc", "exBIC"),
                     cor.check = NULL){
  criterion <- match.arg(criterion)
  info.code <- match(criterion, c("AIC", "BIC", "AICc", "exBIC")) - 1L

  x <- as.matrix(x)
  p <- ncol(x)
  var.names <- colnames(x)
  if (is.null(var.names)) var.names <- paste0("V", seq_len(p))
  storage.mode(x) <- "double"

  if (!is.null(cor.check)) .check.high.cor(x, var.names, cor.check)

  # whiten by the held marginal covariance (estimating variance components once)
  wh <- .rlm.whiten(y, x, group, Z, varcomp, V)

  out <- .Call("rlm_gibbs_glm",
               wh$ystar, wh$xstar, wh$istar,
               max.size, permute, n.draws, inv.temp, ebic.gamma, info.code,
               wh$logdetV0, n.models,
               PACKAGE = "IBGS")

  .rlm.result(out, y, x, var.names, wh, inv.temp, criterion, threshold)
}
