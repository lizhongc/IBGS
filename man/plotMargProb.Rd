\name{plotMargProb}
\alias{plotMargProb}
\title{Plot the marginal inclusion probability of the top covariates}
\usage{
plotMargProb(
  result,
  n.vars = 20,
  col = NULL,
  horizontal = FALSE,
  lwd = 2,
  side = 1,
  line = 0.25,
  las = 2,
  cex = 1
)
}
\arguments{
\item{result}{an \code{"IBGS"} result from one of the samplers}

\item{n.vars}{the number of top covariates to show, default 20}

\item{col}{colour of the non-selected points, default from the palette}

\item{horizontal}{lay the predictors down the y-axis (names read horizontally on
the left) instead of along the x-axis, default \code{FALSE}}

\item{lwd}{error-bar line width}

\item{side}{margin side for the predictor labels (vertical layout; see
\code{\link{mtext}})}

\item{line}{margin line for the labels}

\item{las}{label orientation (vertical layout; 2 = perpendicular)}

\item{cex}{label character expansion}
}
\value{
invisibly, the named vector of plotted inclusion probabilities
}
\description{
Draws a dot-and-whisker plot of the \code{n.vars} covariates with the highest
marginal inclusion probability, sorted descending.  Each point carries an error
bar of plus/minus one Monte-Carlo standard error of the inclusion proportion,
\eqn{se = \sqrt{p (1 - p) / ESS}}, where \code{ESS} is the effective sample size
of the criterion trace (\code{result$convergence$ess}); the bars are clamped to
\eqn{[0, 1]}.  The selected predictors (\code{result$selected.vars}) are
highlighted and the rest are muted, so the selected set stands out.  With
\code{horizontal = TRUE} the predictors run down the y-axis (names read
horizontally on the left); otherwise they run along the x-axis (the default).
}
