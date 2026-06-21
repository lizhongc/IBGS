\name{plot.IBGS}
\alias{plot.IBGS}
\title{Diagnostic plots for an IBGS fit}
\usage{
\method{plot}{IBGS}(x, which = c("ichart", "varprob", "modelfreq"), ...)
}
\arguments{
\item{x}{an \code{"IBGS"} result}

\item{which}{one or more of \code{"ichart"}, \code{"varprob"},
\code{"modelfreq"}; default is all three}

\item{...}{further graphical parameters, forwarded only when a single panel
is requested}
}
\value{
\code{x}, invisibly
}
\description{
Dispatches to the diagnostic plots: the I-chart of the criterion trace
(\code{\link{plotIchart}}), the marginal inclusion probabilities
(\code{\link{plotVarProb}}), and the top-model visit frequencies
(\code{\link{plotModelFreq}}).  Several may be requested at once, in which case
they are drawn in a multi-panel layout.
}
