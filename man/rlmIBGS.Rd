\name{rlmIBGS}
\alias{rlmIBGS}
\title{The iterated block Gibbs sampler for the linear mixed model (fixed-effect part)}
\usage{
rlmIBGS(
  y,
  x,
  group = NULL,
  Z = NULL,
  varcomp = NULL,
  V = NULL,
  n.refine = 3,
  n.models = 10,
  block.size = 30,
  n.keep = 20,
  threshold = 0.9,
  permute = TRUE,
  n.draws = 250,
  inv.temp = 1,
  ebic.gamma = 0.5,
  criterion = c("AIC", "BIC", "AICc", "exBIC"),
  n.cores = 1L,
  cor.check = NULL,
  start = c("null", "full")
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

\item{n.refine}{the number of refinement iterations, default is 3}

\item{n.models}{the number of top selected models, default is 10}

\item{block.size}{the number of predictors in small groups, default is 30}

\item{n.keep}{the number of selected predictors in the first step, default 20}

\item{threshold}{the threshold to select the important predictors, default 0.9}

\item{permute}{the coordinate visiting order within each Gibbs sweep.
\code{TRUE} (default) draws a fresh random permutation each sweep, so every
predictor is updated exactly once per sweep (without replacement);
\code{FALSE} uses a fixed in-order systematic sweep.}

\item{start}{the initial model for the Gibbs chain(s). \code{"null"} (default)
starts from the empty model (intercept only) and grows, avoiding the
ill-conditioned full-model start where the criterion (notably AICc) can be
degenerate; \code{"full"} starts from the saturated model (the previous
behaviour).}

\item{n.draws}{the half number of generated samples, default is 250}

\item{inv.temp}{the tuning parameter, default is 1}

\item{ebic.gamma}{the parameter for extended BIC, default is 0.5}

\item{criterion}{the model selection criterion: AIC, BIC, AICc or exBIC}

\item{n.cores}{the number of OpenMP threads for the block screening
(0 = use all available cores), default is 1 (serial)}

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
Fixed-effect variable selection for a linear mixed model in ultrahigh
dimensions, holding the random part fixed.  This is the mixed-model
counterpart of \code{\link{glmIBGS}}: the whole search (refinement iterations,
random block screening and the final long run) runs in a single multicore C
routine on the whitened data.  With the random structure held the marginal
model is \code{y ~ N(X beta, V)} with \code{V} fixed; whitening by
\code{V = L Lt} reduces each candidate fit to ordinary least squares.  The
variance component(s) are estimated once (random-intercept case, by REML from
the largest estimable fixed model) or supplied, and held fixed.
}
\details{
\code{rlmIBGS} runs the same iterated block Gibbs algorithm as
\code{\link{glmIBGS}} -- the screen / select / threshold refinement loop, the
final long Gibbs run, the acceptance rule \code{min(1, exp(inv.temp *
(IC.current - IC.proposed)))}, the four information criteria and the model
averaging over the top \code{n.models} models -- but on \emph{whitened} data.
With the random part
held fixed the marginal model is \code{y ~ N(X beta, V)} with \code{V} fixed;
whitening by \code{V = L Lt} (left-multiplying \code{y} and \code{X} by the
inverse root) reduces each candidate fit to ordinary least squares, so the C
sampler is the gaussian one applied to the whitened design.  The variance
component(s) are estimated once -- for a random intercept by REML from the
largest estimable fixed model -- or supplied directly, and then held fixed across
all candidate models.  See \code{\link{glmIBGS}} for the screening, sampler and
criteria details.

Specify the random part with exactly one of: \code{group} (a grouping factor
for a random intercept), \code{Z} + \code{varcomp}, or a marginal covariance
\code{V}.
}
\examples{
x <- matrix(rnorm(100 * 50), 100, 50)
g <- gl(10, 10)
y <- rowSums(x[, 1:3]) + rnorm(10)[g] + rnorm(100)
fit <- rlmIBGS(y, x, group = g, criterion = "BIC")
fit
}
