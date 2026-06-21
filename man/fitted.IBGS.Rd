\name{fitted.IBGS}
\alias{fitted.IBGS}
\title{Fitted values of an IBGS fit}
\usage{
\method{fitted}{IBGS}(object, type = c("response", "link"), ...)
}
\arguments{
\item{object}{an \code{"IBGS"} result}

\item{type}{\code{"response"} (the default, response scale) or \code{"link"}
(the linear-predictor scale).}

\item{...}{ignored}
}
\value{
a numeric vector of model-averaged fitted values on the training data (length
equal to the number of training observations).
}
\description{
The model-averaged fitted values on the training data, on the response (default)
or link scale.  The averaged training linear predictor (the
\code{linear.predictors} component) is computed once at fit time and stored on
the object, so this is cheap even in high dimensions.  Equivalent to
\code{predict(object)} with no new data.
}
\seealso{
\code{\link{predict.IBGS}}, \code{\link{coef.IBGS}}
}
\examples{
x <- matrix(rnorm(1000), ncol = 10)
y <- rowSums(x[, 1:5]) + rnorm(100)
fit <- glmGibbs(y, x, criterion = "BIC")
head(fitted(fit))
}
