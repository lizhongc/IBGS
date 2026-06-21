\name{glmIBGS}
\alias{glmIBGS}
\title{The iterated block Gibbs sampler algorithm}
\usage{
glmIBGS(
  y,
  x,
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
  family = c("gaussian", "binomial", "poisson"),
  weights = NULL,
  n.cores = 1L,
  cor.check = NULL
)
}
\arguments{
\item{y}{the response variable}

\item{x}{the predictors}

\item{n.refine}{the number of refinement iterations, default is 3}

\item{n.models}{the number of top selected models}

\item{block.size}{the number of predictors in small groups, default is 30}

\item{n.keep}{the number of selected predictors in first step, default is 20}

\item{threshold}{the threshold to select the important predictors in second step, default is 0.9}

\item{permute}{the permutation of Gibbs sampler, default is TRUE}

\item{n.draws}{the half number of generated samples, default is 250}

\item{inv.temp}{the tuning parameter, default is 1}

\item{ebic.gamma}{the parameter for extended BIC, default is 0.5}

\item{criterion}{the selected model selection criterion from AIC, AICc, BIC and exBIC}

\item{family}{the model family: "gaussian", "binomial" or "poisson"}

\item{weights}{optional prior weights (length \code{nrow(x)}); as in
\code{\link[stats]{glm}}.  For the binomial family \code{y} may be a fraction
(proportion of successes) with \code{weights} the number of trials.
Defaults to all ones.}

\item{n.cores}{the number of OpenMP threads for the block screening
(0 = use all available cores), default is 1 (serial)}

\item{cor.check}{optional correlation threshold; \code{NULL} (default) skips the
check.  A scalar such as \code{0.9999} runs an O(p^2) scan of the predictors
and stops if any pair has absolute correlation above it (naming the
offenders), since near-duplicate columns destabilise the fits.}
}
\value{
an object of class \code{"IBGS"} summarising the search, with
  components \code{marginal.prob} (the marginal inclusion probability of each
  predictor), \code{selected.vars} (the names of the predictors above
  \code{threshold}), \code{var.names}, \code{coef} (the coefficient matrix of the
  top \code{n.models} models), \code{model.ic} and \code{model.freq} (the
  criterion and visit frequency of each top model), \code{ic.trace} (the criterion
  at every generation) and \code{criterion} (the criterion name).  Has
  \code{print}, \code{summary}, \code{coef} and \code{plot} methods.
}
\description{
Variable selection for the generalized linear model in ultrahigh dimensions.
The whole search -- the refinement iterations, the random block screening,
and the final long run -- is performed in a single multicore C routine
(\code{ibgs_glm}); this R function is a thin wrapper that assembles the result
summary (refitting the top models in C to recover their coefficients).  The
gaussian family is fitted by ordinary least squares; the binomial and poisson
families by IRLS.
}
\details{
In ultrahigh dimensions the number of predictors \code{p} can far exceed the
sample size \code{n}, so a single Gibbs sampler over all \code{p} inclusion
indicators is both slow (each candidate fit can use at most \code{n} columns)
and noisy.  IBGS instead alternates \emph{screening} and \emph{refinement} to
grow a small, stable set of important predictors:
\itemize{
  \item \emph{Screen.} Randomly partition the not-yet-important predictors into
    blocks of size \code{block.size} (capped at \code{n} minus the current
    important set, so every block design is estimable).  An independent
    within-block Gibbs sampler is run on each block -- always including the
    important set -- giving a marginal inclusion probability for every predictor
    in the block.
  \item \emph{Select.} Keep the top \code{n.keep} predictors by inclusion
    probability, union them with the important set, and run one combined Gibbs
    sampler over that union.
  \item \emph{Threshold.} Promote every predictor whose combined inclusion
    probability exceeds \code{threshold} into the important set.
}
These steps are repeated for \code{n.refine} refinement rounds; a final screen is
then followed by a long Gibbs run over the final candidate set, which produces
the recorded indicator samples, their information criteria, and the marginal
inclusion probabilities returned to R.

Each Gibbs sampler is a random-scan Metropolis-within-Gibbs walk over the model
indicator vector: at each step one coordinate is flipped and the flip is
accepted with probability
\code{min(1, exp(inv.temp * (IC.current - IC.proposed)))}.  Because a lower
criterion is better, a flip that lowers the criterion is always accepted and a
worsening flip is accepted with a probability that decays in the increase; the
tuning parameter \code{inv.temp} controls how greedy the walk is.  The
chain's stationary distribution favours low-criterion models and its visit
frequencies estimate the marginal inclusion probabilities.  Every candidate
model is fitted in C -- the gaussian family by ordinary least squares (a
Cholesky solve on the active sub-block of a precomputed Gram matrix), the
binomial and poisson families by iteratively reweighted least squares.

Models are compared by one of four information criteria, each of the form
\code{-2logLik + penalty}: \code{AIC = -2logLik + 2 npar},
\code{BIC = -2logLik + log(n) npar},
\code{AICc = AIC + 2 npar (npar + 1) / (n - npar - 1)} (a small-sample AIC), and
\code{exBIC = BIC + 2 ebic.gamma npred log(p0)} (the extended BIC of Chen and
Chen, 2008, whose extra term penalises the size \code{p0} of the candidate pool
and is essential in ultrahigh dimensions where BIC under-penalises).  Here
\code{npar} counts the estimated parameters and \code{npred} the predictors
excluding the intercept.

The returned object summarises the search: the top \code{n.models} distinct
models are refit in C to recover their coefficients, and averaged coefficients
and predictions combine them with smooth-criterion weights (see
\code{\link{coef.IBGS}} and \code{\link{predict.IBGS}}).  The block screening is
parallelised with \code{n.cores} OpenMP threads (one thread per block); the
random block assignment and the per-block seeds are drawn serially from R's
generator, so the result is reproducible and does not depend on the number of
threads.
}
\examples{
x <- matrix(rnorm(100 * 50), 100, 50)
y <- rowSums(x[, 1:3]) + rnorm(100)
fit <- glmIBGS(y, x, criterion = "BIC")
fit
}
