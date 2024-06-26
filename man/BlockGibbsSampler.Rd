% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blockgibbs.R
\name{BlockGibbsSampler}
\alias{BlockGibbsSampler}
\title{The iterated block Gibbs sampler algorithm}
\usage{
BlockGibbsSampler(
  y,
  x,
  n.iter = 3,
  n.models = 10,
  H = 30,
  kapp = 20,
  tau = 0.9,
  perm = TRUE,
  len = 250,
  k = 1,
  gamma = 0.5,
  info = c("AIC", "BIC", "AICc", "exBIC"),
  family = c("gaussian", "poisson", "binomial")
)
}
\arguments{
\item{y}{the response variable}

\item{x}{the predictors}

\item{n.iter}{the number of iterations}

\item{n.models}{the number of top selected models}

\item{H}{the number of predictors in small groups, default is 30}

\item{kapp}{the number of selected predictors in first step, default is 20}

\item{tau}{the threshold to select the important predictors in second step, default is 0.9}

\item{perm}{the permutation of Gibbs sampler, default is TRUE}

\item{len}{the half number of generated samples, default is 250}

\item{k}{the tuning parameter, default is 1}

\item{gamma}{the parameter for extended BIC, default is 0.5}

\item{info}{the selected model selection criterion from AIC, AICc, BIC and exBIC}

\item{family}{the type of model from linear, logistic, poisson}
}
\value{
a list contains a summary of final result
}
\description{
The iterated block Gibbs sampler algorithm
}
