\name{glmGibbs}
\alias{glmGibbs}
\title{The restricted Gibbs sampler (linear model)}
\usage{
glmGibbs(
  y,
  x,
  max.size = ncol(x),
  permute = TRUE,
  n.models = 10,
  threshold = 0.9,
  n.draws = 1000,
  inv.temp = 1,
  ebic.gamma = 0.5,
  criterion = c("AIC", "BIC", "AICc", "exBIC"),
  family = c("gaussian", "binomial", "poisson"),
  weights = NULL,
  cor.check = NULL
)
}
\arguments{
\item{y}{the response variable}

\item{x}{the predictors}

\item{max.size}{the maximal number of predictors in a candidate model,
default is \code{ncol(x)}}

\item{permute}{the permutation of Gibbs sampler, default is TRUE}

\item{n.models}{the number of top selected models, default is 10}

\item{threshold}{the threshold to select the important predictors, default 0.9}

\item{n.draws}{the half number of generated samples, default is 1000}

\item{inv.temp}{the tuning parameter, default is 1}

\item{ebic.gamma}{the parameter for extended BIC, default is 0.5}

\item{criterion}{the selected model selection criterion from AIC, AICc, BIC and exBIC}

\item{family}{the model family: "gaussian", "binomial" or "poisson"}

\item{weights}{optional prior weights (length \code{nrow(x)}); as in
\code{\link[stats]{glm}}.  For the binomial family \code{y} may be a fraction
(proportion of successes) with \code{weights} the number of trials.
Defaults to all ones.}

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
The standalone (non-block) Metropolis-within-Gibbs sampler that searches the
model space directly over all predictors, with the model size capped at
\code{max.size}.  Every candidate linear model is fitted by ordinary least
squares in a fast C routine (\code{gibbs_sampler_gaussian}); the summary is
assembled by refitting the top models in C to recover their coefficients.
Unlike
\code{\link{glmIBGS}}, this sampler does not split the predictors
into blocks and is intended for problems where the full design is manageable.
}
\details{
\code{glmGibbs} runs a single random-scan Metropolis-within-Gibbs walk directly
over the inclusion indicators of all \code{p} predictors, with the model size
capped at \code{max.size}; there is no block screening or refinement.  The flip
acceptance rule \code{min(1, exp(inv.temp * (IC.current - IC.proposed)))}, the
four
information criteria (\code{AIC}, \code{BIC}, \code{AICc} and \code{exBIC}) and
the model averaging over the top \code{n.models} models are exactly as described
under \code{\link{glmIBGS}}, and each candidate is fitted in the same way
(gaussian by ordinary least squares, binomial and poisson by IRLS).  Because
every candidate fit may use all \code{p} predictors, this sampler is intended for
problems where the full design is manageable (\code{p} not much larger than
\code{n}); use \code{\link{glmIBGS}} in ultrahigh dimensions.
}
\examples{
x <- matrix(rnorm(1000), ncol = 10)
y <- rowSums(x[,1:5]) + rnorm(100)
m.s <- glmGibbs(y, x, criterion = "BIC")

}
