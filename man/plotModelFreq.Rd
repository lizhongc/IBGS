\name{plotModelFreq}
\alias{plotModelFreq}
\title{Plot the visit frequency of the top selected models}
\usage{
plotModelFreq(
  result,
  n.models = result$n.models,
  col = NULL,
  annotate = TRUE,
  cumulative = FALSE,
  ...
)
}
\arguments{
\item{result}{an \code{"IBGS"} result from one of the samplers}

\item{n.models}{the number of top models to show, default is all retained}

\item{col}{bar colour for the non-best models, default from the palette}

\item{annotate}{write each bar's criterion value above it, default \code{TRUE}}

\item{cumulative}{overlay the cumulative visit frequency, default \code{FALSE}}

\item{...}{further graphical parameters passed to \code{\link{barplot}}}
}
\value{
invisibly, the vector of plotted relative model frequencies
}
\description{
Draws a bar for each of the top \code{n.models} models, showing how often the
sampler settled on it: the relative frequency with which the Gibbs chain
visited that model (\code{result$model.freq}).  The bars are ordered best-first
(the model with the lowest information criterion on the left, highlighted), and
each bar is annotated with its criterion value.  Tall leading bars indicate a
search that concentrated sharply on a few models.
}
