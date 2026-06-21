\name{plotIchart}
\alias{plotIchart}
\title{I-chart for the generated information-criterion sequence}
\usage{
plotIchart(
  result,
  col = NULL,
  highlight = NULL,
  running.min = FALSE,
  legend = TRUE,
  ...
)
}
\arguments{
\item{result}{an \code{"IBGS"} result from one of the samplers}

\item{col}{colour of the criterion trace, default from the package
palette (grey)}

\item{highlight}{colour marking the best (lowest) value, default orange}

\item{running.min}{add a best-so-far (running minimum) trace, default
\code{FALSE}}

\item{legend}{draw an explanatory legend, default \code{TRUE}}

\item{...}{further graphical parameters passed to \code{\link{plot}}}
}
\value{
invisibly, a list with the two control limits and the best value
}
\description{
Plots the model selection criterion at each Gibbs generation together with two
upper control limits (UCLs): one estimated from the first half of the run
(the shaded region) and one from the whole run.  Once the trace settles below
the limits the sampler has stabilised.  The best (lowest) criterion value
reached is highlighted, and a horizontal guide marks the best retained model's
criterion.
}
