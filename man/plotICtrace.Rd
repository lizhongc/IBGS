\name{plotICtrace}
\alias{plotICtrace}
\title{Trace plot of the information-criterion sequence}
\usage{
plotICtrace(
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
invisibly, a list with the best value and the generation that reached it
}
\description{
Plots the model selection criterion at each Gibbs generation.  The best
(lowest) criterion value reached is highlighted and a horizontal guide marks the
best retained model's criterion, so the minimal information criterion stands out.
}
