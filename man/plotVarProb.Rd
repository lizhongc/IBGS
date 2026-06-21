\name{plotVarProb}
\alias{plotVarProb}
\title{Plot the marginal inclusion probability of the top covariates}
\usage{
plotVarProb(
  result,
  n.vars = 20,
  col = NULL,
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

\item{col}{colour of the below-threshold stems, default from the palette}

\item{lwd}{stem line width}

\item{side}{margin side for the predictor labels (see \code{\link{mtext}})}

\item{line}{margin line for the labels}

\item{las}{label orientation (2 = perpendicular)}

\item{cex}{label character expansion}
}
\value{
invisibly, the named vector of plotted inclusion probabilities
}
\description{
Draws a lollipop plot of the \code{n.vars} covariates with the highest marginal
inclusion probability, sorted descending.  A dashed line marks the selection
threshold; covariates above it (the selected predictors) are
highlighted and the rest are muted, so the selected set stands out.  The
predictor names are written under the axis.
}
