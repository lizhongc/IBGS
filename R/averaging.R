# Model averaging for the "IBGS" result object.
#
# Each sampler retains the best n.models models -- their coefficient columns,
# criterion values and visit frequencies -- assembled into the "IBGS" object by
# .ibgs.object() (methods.R).  This file turns that retained set into estimates
# and predictions: the smooth-SIC weighting (.ibgs.weights), the averaged
# link-scale linear predictor (.ibgs.lp) and the family inverse link
# (.ibgs.response), and the user-facing S3 methods coef(), predict() and
# fitted().  All three model-average by default (BMA-style over the top models)
# but can also report a single retained model.

# Smooth-SIC model-averaging weights over the best n.models criterion values:
# w_i proportional to exp(-inv.temp * (IC_i - min IC)), normalised.  Shared by
# coef.IBGS(), predict.IBGS() and fitted.IBGS().
.ibgs.weights <- function(object, n.models = object$n.models) {
  n.models <- min(n.models, object$n.models)
  sic <- object$model.ic[seq_len(n.models)]
  w   <- exp(-object$inv.temp * (sic - min(sic)))
  w / sum(w)
}

# Link-scale linear predictor of a design x under the averaged model (BMA over the
# top n.models, the default) or a single retained model (average = FALSE).  x has
# the original predictor columns; an intercept column is prepended for the
# families that carry one.  Linear averaging collapses the top models' coefficient
# columns into one averaged vector, so this is a single matrix-vector product.
.ibgs.lp <- function(object, x, n.models = object$n.models, average = TRUE,
                     model = 1) {
  x <- as.matrix(x)
  if (isTRUE(average)) {
    w  <- .ibgs.weights(object, n.models)
    bw <- object$coef[, seq_len(length(w)), drop = FALSE] %*% w
  } else {
    bw <- object$coef[, model]
  }
  Xd <- if (isTRUE(object$has.intercept)) cbind(1, x) else x
  as.numeric(Xd %*% bw)
}

# Apply the family inverse link to a linear predictor.  type = "link" returns it
# unchanged; "response" maps to the response scale (identity for gaussian/rlm,
# logistic for binomial, exponential for poisson and for the cox relative-risk
# score exp(x beta)).
.ibgs.response <- function(lp, family, type) {
  if (type == "link") return(lp)
  switch(family,
         gaussian = lp,
         binomial = 1 / (1 + exp(-lp)),
         poisson  = exp(lp),
         cox      = exp(lp),
         rlm      = lp,
         lp)
}

# Coefficients of an IBGS fit
#
# Returns the coefficient vector of one of the retained top models, or the
# model-averaged coefficient vector (smooth-SIC weights over the best
# n.models, the same weighting predict()/fitted() use).  The
# vector is named by the predictors (with "(Intercept)" first for the
# families that carry an intercept).
#
# Arguments:
#   object   an "IBGS" result
#   model    which retained model (1 = best by criterion), used when
#            average = FALSE
#   average  average over the top n.models instead of a single model
#   n.models number of top models to average when average = TRUE
#   ...      ignored
# Value: a named numeric vector of coefficients
coef.IBGS <- function(object, model = 1, average = FALSE,
                      n.models = object$n.models, ...) {
  nm <- if (isTRUE(object$has.intercept)) c("(Intercept)", object$var.names)
        else object$var.names
  if (isTRUE(average)) {
    w <- .ibgs.weights(object, n.models)
    b <- as.numeric(object$coef[, seq_len(length(w)), drop = FALSE] %*% w)
  } else {
    b <- object$coef[, model]
  }
  stats::setNames(as.numeric(b), nm)
}

# Predict method for an IBGS fit
#
# Model-averaged prediction (the default) or single-model prediction on new data,
# on the response (default) or link scale.  With newdata = NULL the stored
# model-averaged fitted values on the training data are returned (see
# fitted.IBGS).  For an rlm fit, supplying group.new or Z.new adds the matching
# model-averaged random-effect BLUP (conditional prediction); unseen groups
# contribute 0 (the marginal mean).
#
# Arguments:
#   object    an "IBGS" result
#   newdata   new predictor data (matrix/data frame with the original columns);
#             NULL returns the training fitted values
#   type      "response" (default) or "link"
#   average   average over the top n.models (default) instead of one model
#   n.models  number of top models to average when average = TRUE
#   model     which retained model to use when average = FALSE (1 = best)
#   group.new rlm: grouping factor of the new data for conditional prediction
#   Z.new     rlm: random-effects design of the new data for conditional prediction
#   ...       ignored
# Value: a numeric vector of predictions
predict.IBGS <- function(object, newdata = NULL, type = c("response", "link"),
                         average = TRUE, n.models = object$n.models, model = 1,
                         group.new = NULL, Z.new = NULL, ...) {
  type <- match.arg(type)
  fam  <- if (is.null(object$family)) "gaussian" else object$family
  if (is.null(newdata)) {
    lp <- object$linear.predictors            # stored averaged training lp (link)
  } else {
    lp <- .ibgs.lp(object, newdata, n.models, average, model)
    # rlm conditional prediction: add the model-averaged random-effect BLUP
    if (fam == "rlm" && !is.null(object$re) &&
        (!is.null(group.new) || !is.null(Z.new))) {
      w    <- .ibgs.weights(object, n.models)
      blup <- object$re$blup[, seq_len(length(w)), drop = FALSE] %*% w
      if (object$re$type == "group" && !is.null(group.new)) {
        idx <- match(as.character(group.new), object$re$levels)
        lp  <- lp + ifelse(is.na(idx), 0, blup[idx])
      } else if (!is.null(Z.new)) {
        lp <- lp + as.matrix(Z.new) %*% blup
      }
    }
  }
  .ibgs.response(lp, fam, type)
}

# Fitted values of an IBGS fit
#
# The model-averaged fitted values on the training data, on the response (default)
# or link scale.  The averaged training linear predictor is computed once at fit
# time and stored on the object, so this is cheap even in high dimensions.
#
# Arguments:
#   object an "IBGS" result
#   type   "response" (default) or "link"
#   ...    ignored
# Value: a numeric vector of fitted values
fitted.IBGS <- function(object, type = c("response", "link"), ...) {
  type <- match.arg(type)
  fam  <- if (is.null(object$family)) "gaussian" else object$family
  .ibgs.response(object$linear.predictors, fam, type)
}
