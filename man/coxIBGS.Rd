\name{coxIBGS}
\alias{coxIBGS}
\title{The iterated block Gibbs sampler for the Cox model}
\usage{
coxIBGS(
  y,
  status,
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
  weights = NULL,
  n.cores = 1L,
  cor.check = NULL
)
}
\arguments{
\item{y}{the event/censoring time (a non-negative numeric vector)}

\item{status}{the event indicator: 1 = event (death), 0 = censored}

\item{x}{the predictors}

\item{n.refine}{the number of refinement iterations, default is 3}

\item{n.models}{the number of top selected models, default is 10}

\item{block.size}{the number of predictors in small groups, default is 30}

\item{n.keep}{the number of selected predictors in the first step, default 20}

\item{threshold}{the threshold to select the important predictors, default 0.9}

\item{permute}{the permutation of the Gibbs sampler, default is TRUE}

\item{n.draws}{the half number of generated samples, default is 250}

\item{inv.temp}{the tuning parameter, default is 1}

\item{ebic.gamma}{the parameter for extended BIC, default is 0.5}

\item{criterion}{the model selection criterion: AIC, BIC, AICc or exBIC}

\item{weights}{optional prior case weights (length \code{nrow(x)}); defaults
to all ones}

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
  \code{threshold}), \code{var.names}, \code{coef} (the coefficient matrix of the top
  \code{n.models} models), \code{model.ic} and \code{model.freq} (the criterion
  and visit frequency of each top model), \code{ic.trace} (the criterion at
  every generation) and \code{criterion} (the criterion name).  Has
  \code{print}, \code{summary}, \code{coef} and \code{plot} methods.
}
\description{
Variable selection for the Cox proportional-hazards model in ultrahigh
dimensions.  This is the survival-analysis counterpart of
\code{\link{glmIBGS}}: the whole search (refinement iterations,
random block screening and the final long run) is performed in a single
multicore C routine (\code{cox_ibgs_glm}), and this R function is a thin wrapper
that assembles the summary (refitting the top models in C).  Each candidate
model is fitted by its Efron partial likelihood; the Cox model has no
intercept, and the information criteria use the number of events as the
effective sample size.
}
\details{
\code{coxIBGS} applies the same iterated block Gibbs algorithm as
\code{\link{glmIBGS}} -- the screen / select / threshold refinement loop, the
final long Gibbs run, the Metropolis-within-Gibbs acceptance rule
\code{min(1, exp(inv.temp * (IC.current - IC.proposed)))}, the four information
criteria
and the model averaging over the top \code{n.models} models -- but for the Cox
proportional-hazards model rather than a GLM.  Each candidate model is fitted in
C by its Efron partial likelihood; the Cox model carries no intercept, and the
information criteria use the number of events (not the sample size \code{n}) as
the effective sample size.  See \code{\link{glmIBGS}} for the screening, sampler
and criteria details.
}
\examples{
x <- matrix(rnorm(60 * 20), 60, 20)
time <- rexp(60, exp(rowSums(x[, 1:3])))
status <- rbinom(60, 1, 0.7)
fit <- coxIBGS(time, status, x, criterion = "BIC")
fit
}
