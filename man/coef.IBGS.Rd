\name{coef.IBGS}
\alias{coef.IBGS}
\title{Coefficients of an IBGS fit}
\usage{
\method{coef}{IBGS}(object, model = 1, average = FALSE, n.models = object$n.models, ...)
}
\arguments{
\item{object}{an \code{"IBGS"} result}

\item{model}{which retained model (1 = best by criterion), used when
\code{average = FALSE}}

\item{average}{average over the top \code{n.models} instead of a single model}

\item{n.models}{number of top models to average when \code{average = TRUE}}

\item{...}{ignored}
}
\value{
a named numeric vector of coefficients
}
\description{
Returns the coefficient vector of one of the retained top models, or the
model-averaged coefficient vector (smooth-SIC weights over the best
\code{n.models}, the same weighting \code{\link{predict.IBGS}} uses).  The
vector is named by the predictors (with \code{"(Intercept)"} first for the
families that carry an intercept).
}
