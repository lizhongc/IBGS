\name{predict.IBGS}
\alias{predict.IBGS}
\title{Predict method for an IBGS fit}
\usage{
\method{predict}{IBGS}(object, newdata = NULL, type = c("response", "link"),
  average = TRUE, n.models = object$n.models, model = 1,
  group.new = NULL, Z.new = NULL, ...)
}
\arguments{
\item{object}{an \code{"IBGS"} result from \code{\link{glmGibbs}},
\code{\link{glmIBGS}}, \code{\link{coxGibbs}}, \code{\link{coxIBGS}},
\code{\link{rlmGibbs}} or \code{\link{rlmIBGS}}}

\item{newdata}{new predictor data (a matrix or data frame with the original
predictor columns).  When \code{NULL} the model-averaged fitted values on the
training data are returned (see \code{\link{fitted.IBGS}}).}

\item{type}{\code{"response"} (the default, response scale) or \code{"link"}
(the linear-predictor scale).}

\item{average}{average over the top \code{n.models} models (the default,
Bayesian-style model averaging) instead of using a single model.}

\item{n.models}{the number of top models to average over when
\code{average = TRUE}.}

\item{model}{which retained model to use when \code{average = FALSE}
(1 = the best by criterion).}

\item{group.new}{for an \code{rlm} fit, the grouping factor of the new data;
when supplied the matching model-averaged random-intercept BLUP is added
(conditional prediction).  Unseen groups contribute 0 (the marginal mean).}

\item{Z.new}{for a general \code{rlm} fit, the random-effects design of the
new data; when supplied \code{Z.new \%*\% b_hat} is added.}

\item{...}{ignored}
}
\value{
a numeric vector of predictions: the response scale for the GLM families
(probabilities in [0, 1] for binomial), the relative risk
\code{exp(x*beta)} for cox, and the fixed-effect mean -- optionally plus the
random-effect BLUP -- for rlm.
}
\description{
Model-averaged prediction (the default) or single-model prediction on new data.
The top \code{n.models} models are averaged on the linear-predictor (link) scale
with smooth-SIC weights (proportional to \code{exp(-inv.temp * (IC - min IC))})
and the family inverse link is applied once; averaging before the inverse link
keeps binomial predictions in the unit interval and poisson/cox predictions
positive.  With \code{newdata = NULL} the model-averaged fitted values stored on
the object at fit time are returned.
}
\seealso{
\code{\link{fitted.IBGS}}, \code{\link{coef.IBGS}}
}
\examples{
x <- matrix(rnorm(1000), ncol = 10)
y <- rowSums(x[, 1:5]) + rnorm(100)
fit <- glmGibbs(y, x, criterion = "BIC")
predict(fit, x[1:5, ])
}
