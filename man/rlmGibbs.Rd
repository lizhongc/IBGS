\name{rlmGibbs}
\alias{rlmGibbs}
\title{The restricted Gibbs sampler for the linear mixed model (fixed-effect part)}
\usage{
rlmGibbs(
  y,
  x,
  group = NULL,
  Z = NULL,
  varcomp = NULL,
  V = NULL,
  max.size = ncol(x),
  permute = TRUE,
  n.models = 10,
  threshold = 0.9,
  n.draws = 1000,
  inv.temp = 1,
  ebic.gamma = 0.5,
  criterion = c("AIC", "BIC", "AICc", "exBIC"),
  cor.check = NULL
)
}
\arguments{
\item{y}{the response variable}

\item{x}{the fixed-effect predictors}

\item{group}{a grouping factor (length \code{nrow(x)}) for a random
intercept}

\item{Z}{a random-effects design matrix (general random part)}

\item{varcomp}{a list \code{list(sigma2b, sigma2e)} of variance components
used with \code{Z}}

\item{V}{the marginal covariance \code{V} (n x n), supplied directly}

\item{max.size}{the maximal number of fixed predictors in a candidate model,
default is \code{ncol(x)}}

\item{permute}{the permutation of the Gibbs sampler, default is TRUE}

\item{n.models}{the number of top selected models, default is 10}

\item{threshold}{the threshold to select the important predictors, default 0.9}

\item{n.draws}{the half number of generated samples, default is 1000}

\item{inv.temp}{the tuning parameter, default is 1}

\item{ebic.gamma}{the parameter for extended BIC, default is 0.5}

\item{criterion}{the model selection criterion: AIC, BIC, AICc or exBIC}

\item{cor.check}{optional correlation threshold; \code{NULL} (default) skips the
check.  A scalar such as \code{0.9999} stops if any pair of predictors has
absolute correlation above it.}
}
\value{
an object of class \code{"IBGS"} summarising the search, with
  components \code{marginal.prob} (the marginal inclusion probability of each
  predictor), \code{selected.vars} (the names of the predictors above
  \code{threshold}), \code{var.names}, \code{coef} (the coefficient matrix of the top
  \code{n.models} models), \code{model.ic} and \code{model.freq} (the criterion
  and visit frequency of each top model), \code{ic.trace} (the criterion at
  every generation), \code{criterion} (the criterion name) and, when the random
  part is given via \code{group} or \code{Z} + \code{varcomp}, \code{re} (the
  random-effect BLUPs used for conditional prediction; a directly supplied
  \code{V} gives the fixed-effect fit without \code{re}).  Has \code{print},
  \code{summary}, \code{coef} and \code{plot} methods.
}
\description{
The standalone (non-block) Metropolis-within-Gibbs sampler that selects the
fixed effects of a linear mixed model while holding the random part fixed.
With the random structure held, the marginal model is \code{y ~ N(X beta, V)}
with \code{V} fixed; whitening by \code{V = L Lt} reduces each candidate fit to
ordinary least squares on the whitened data, performed in a fast C routine.
The variance component(s) are estimated once (random-intercept case) or
supplied, and held fixed across all candidate models.  This is the mixed-model
counterpart of \code{\link{glmGibbs}}; use \code{\link{rlmIBGS}} in ultrahigh
dimensions.
}
\details{
\code{rlmGibbs} runs a single random-scan Metropolis-within-Gibbs walk over the
inclusion indicators of all \code{p} fixed-effect predictors, with the model size
capped at \code{max.size} and no block screening, just as \code{\link{glmGibbs}}
does for the GLM families -- but on \emph{whitened} data.  With the random part
held fixed the marginal model is \code{y ~ N(X beta, V)} with \code{V} fixed;
whitening by \code{V = L Lt} reduces each candidate fit to ordinary least
squares.  The acceptance rule
\code{min(1, exp(inv.temp * (IC.current - IC.proposed)))},
the four information criteria and the model averaging over the top \code{n.models}
models are as described under \code{\link{glmIBGS}}; the variance component(s) are
estimated once (random-intercept case, by REML) or supplied, and held fixed.  Use
\code{\link{rlmIBGS}} in ultrahigh dimensions.

Specify the random part with exactly one of: \code{group} (a grouping factor
for a random intercept; its variance ratio is estimated once by REML),
\code{Z} + \code{varcomp}, or a marginal covariance \code{V}.
}
\examples{
x <- matrix(rnorm(1000), ncol = 10)
g <- gl(10, 10)
y <- rowSums(x[, 1:5]) + rnorm(10)[g] + rnorm(100)
fit <- rlmGibbs(y, x, group = g, criterion = "BIC")
}
