# The restricted Gibbs sampler (linear model)
#
# The standalone (non-block) Metropolis-within-Gibbs sampler that searches the
# model space directly over all predictors, with the model size capped at
# max.size.  Every candidate linear model is fitted by ordinary least
# squares in a fast C routine (gibbs_sampler_gaussian); the summary is
# assembled by refitting the top models in C to recover their coefficients.
# Unlike
# glmIBGS(), this sampler does not split the predictors
# into blocks and is intended for problems where the full design is manageable.
#
# Arguments:
#   y        the response variable
#   x        the predictors
#   max.size the maximal number of predictors in a candidate model,
#            default is ncol(x)
#   permute  the permutation of Gibbs sampler, default is TRUE
#   n.models the number of top selected models, default is 10
#   threshold the threshold to select the important predictors, default 0.9
#   n.draws  the half number of generated samples, default is 1000
#   inv.temp the tuning parameter, default is 1
#   ebic.gamma the parameter for extended BIC, default is 0.5
#   criterion the selected model selection criterion from AIC, AICc, BIC and exBIC
#   family   the model family: "gaussian", "binomial" or "poisson"
#   weights  optional prior weights (length nrow(x)); as in
#            glm().  For the binomial family y may be a fraction
#            (proportion of successes) with weights the number of trials.
#            Defaults to all ones.
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
glmGibbs <- function(y, x, max.size = ncol(x), permute = TRUE, n.models = 10,
                         threshold = 0.9, n.draws = 1000, inv.temp = 1,
                         ebic.gamma = 0.5,
                         criterion = c("AIC", "BIC", "AICc", "exBIC"),
                         family = c("gaussian", "binomial", "poisson"),
                         weights = NULL, cor.check = NULL){
  criterion <- match.arg(criterion)
  family    <- match.arg(family)
  # 0-based integer codes for the C layer
  info.code   <- match(criterion, c("AIC", "BIC", "AICc", "exBIC")) - 1L
  family.code <- match(family, c("gaussian", "binomial", "poisson")) - 1L

  x <- as.matrix(x)
  p <- ncol(x)
  var.names <- colnames(x)
  if (is.null(var.names)) var.names <- paste0("V", seq_len(p))

  if (is.null(weights)) weights <- rep(1, nrow(x))

  # optional fail-fast on near-duplicate covariates (cor.check = NULL skips it)
  if (!is.null(cor.check)) .check.high.cor(x, var.names, cor.check)

  # single, serial (non-block) Metropolis-within-Gibbs run over all p predictors
  out <- .Call("gibbs_sampler_glm",
               y, x, weights, max.size, permute, n.draws, inv.temp,
               ebic.gamma, info.code, family.code, n.models,
               PACKAGE = "IBGS")

  result <- .ibgs.object(out, inv.temp, criterion, family, TRUE)
  result$marginal.prob     <- out$v.prob
  result$selected.vars     <- var.names[out$v.prob > threshold]
  result$threshold         <- threshold
  result$var.names         <- var.names
  result$linear.predictors <- .ibgs.lp(result, x)
  return(result)
}
