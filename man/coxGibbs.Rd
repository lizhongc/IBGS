\name{coxGibbs}
\alias{coxGibbs}
\title{The restricted Gibbs sampler for the Cox model}
\usage{
coxGibbs(
  y,
  status,
  x,
  max.size = ncol(x),
  permute = TRUE,
  n.models = 10,
  threshold = 0.9,
  n.draws = 1000,
  inv.temp = 1,
  ebic.gamma = 0.5,
  criterion = c("AIC", "BIC", "AICc", "exBIC"),
  weights = NULL,
  cor.check = NULL
)
}
\arguments{
\item{y}{the event/censoring time (a non-negative numeric vector)}

\item{status}{the event indicator: 1 = event (death), 0 = censored}

\item{x}{the predictors}

\item{max.size}{the maximal number of predictors in a candidate model,
default is \code{ncol(x)}}

\item{permute}{the coordinate visiting order within each Gibbs sweep.
\code{TRUE} (default) draws a fresh random permutation each sweep, so every
predictor is updated exactly once per sweep (without replacement);
\code{FALSE} uses a fixed in-order systematic sweep.}

\item{n.models}{the number of top selected models, default is 10}

\item{threshold}{the threshold to select the important predictors, default 0.9}

\item{n.draws}{the half number of generated samples, default is 1000}

\item{inv.temp}{the tuning parameter, default is 1}

\item{ebic.gamma}{the parameter for extended BIC, default is 0.5}

\item{criterion}{the model selection criterion: AIC, BIC, AICc or exBIC}

\item{weights}{optional prior case weights (length \code{nrow(x)}); defaults
to all ones}

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
The standalone (non-block) Metropolis-within-Gibbs sampler for the Cox
proportional-hazards model that searches the model space directly over all
predictors, with the model size capped at \code{max.size}.  This is the
survival-analysis counterpart of \code{\link{glmGibbs}}: every candidate
Cox model is fitted by its Efron partial likelihood in a fast C routine
(\code{cox_gibbs_glm}), and the summary is assembled by refitting the top
models in C.  Intended for problems where the full design is
manageable; use \code{\link{coxIBGS}} in ultrahigh dimensions.
}
\details{
\code{coxGibbs} runs a single random-permutation Metropolis-within-Gibbs walk over the
inclusion indicators of all \code{p} predictors, with the model size capped at
\code{max.size} and no block screening, just as \code{\link{glmGibbs}} does for
the GLM families.  The acceptance rule \code{min(1, exp(inv.temp *
(IC.current - IC.proposed)))}, the four information criteria and the model
averaging over the
top \code{n.models} models are as described under \code{\link{glmIBGS}}; each
candidate is fitted by its Efron partial likelihood, the model has no intercept,
and the criteria use the number of events as the effective sample size.  Use
\code{\link{coxIBGS}} in ultrahigh dimensions.
}
\examples{
x <- matrix(rnorm(2000), ncol = 10)
eta <- x[, 1:3] \%*\% c(1, -1, 0.5)
time <- rexp(200, exp(eta))
status <- rbinom(200, 1, 0.8)
m.s <- coxGibbs(time, status, x, criterion = "BIC")

}
