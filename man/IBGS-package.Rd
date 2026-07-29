\name{IBGS-package}
\alias{IBGS-package}
\alias{IBGS}
\docType{package}
\title{Iterated Block Gibbs Sampler for Ultrahigh-Dimensional Variable Selection and Model Averaging}
\description{
Variable selection for generalized linear models, the Cox
proportional-hazards model and the linear mixed model in ultrahigh dimensions
via the iterated block Gibbs sampler (IBGS).  The sampler is implemented in C
with parallel block screening through OpenMP, and supports the gaussian,
binomial and poisson families (fitted by least squares or iteratively
reweighted least squares), the Cox model for survival analysis (fitted by its
Efron partial likelihood), and the fixed-effect part of a linear mixed model
(fitted on whitened data).  Candidate models are compared by the AIC, BIC,
AICc or extended BIC criterion.
}
\details{
Each model type comes with two samplers: a \emph{block} sampler that splits the
predictors into small groups for ultrahigh dimensions, and a \emph{standalone}
(non-block) sampler for problems where the full design is manageable.

\tabular{ll}{
  \code{\link{glmIBGS}}  \tab block sampler for generalized linear models \cr
  \code{\link{glmGibbs}} \tab standalone sampler for generalized linear models \cr
  \code{\link{coxIBGS}}  \tab block sampler for the Cox model \cr
  \code{\link{coxGibbs}} \tab standalone sampler for the Cox model \cr
  \code{\link{lmeIBGS}}  \tab block sampler for the linear mixed model \cr
  \code{\link{lmeGibbs}} \tab standalone sampler for the linear mixed model \cr
}

Every sampler returns an object of class \code{"IBGS"} with
\code{\link[=print.IBGS]{print}}, \code{\link[=summary.IBGS]{summary}},
\code{\link[=coef.IBGS]{coef}} and \code{\link[=plot.IBGS]{plot}} methods.
\code{\link[=predict.IBGS]{predict}} and \code{\link[=fitted.IBGS]{fitted}} form
model-averaged predictions, and
\code{\link{plotICtrace}}, \code{\link{plotModelFreq}} and
\code{\link{plotMargProb}} provide diagnostic plots.
}
\author{
Lizhong Chen <chen.l@wehi.edu.au>

Maintainer: Lizhong Chen <chen.l@wehi.edu.au>
}
\references{
Chen, L. (2022). \emph{Model selection and averaging by Gibbs sampler with a
tropical cyclone seasonal forecasting application}.  PhD thesis, University of
Melbourne. \url{https://hdl.handle.net/11343/311691}

Chen, J. and Chen, Z. (2008). Extended Bayesian information criteria for model
selection with large model spaces. \emph{Biometrika} \bold{95}(3), 759--771.
\doi{10.1093/biomet/asn034}

Chen, J. and Chen, Z. (2012). Extended BIC for small-n-large-P sparse GLM.
\emph{Statistica Sinica} \bold{22}(2), 555--574. \doi{10.5705/ss.2010.216}
}
\seealso{
\code{\link{glmIBGS}}, \code{\link{coxIBGS}}, \code{\link{lmeIBGS}},
\code{\link{predict.IBGS}}
}
\examples{
x <- matrix(rnorm(100 * 50), 100, 50)
y <- rowSums(x[, 1:3]) + rnorm(100)
fit <- glmIBGS(y, x, criterion = "BIC")
fit
predict(fit, x[1:5, ])
}
\keyword{package}
