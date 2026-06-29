\name{plot.IBGS}
\alias{plot.IBGS}
\title{Diagnostic plots for an IBGS fit}
\usage{
\method{plot}{IBGS}(
  x,
  which = c("ictrace", "margprob", "modelfreq", "gelman", "autocorr"),
  ...
)
}
\arguments{
\item{x}{an \code{"IBGS"} result}

\item{which}{one or more of \code{"ictrace"}, \code{"margprob"},
\code{"modelfreq"}, \code{"gelman"}, \code{"autocorr"}; default is all five}

\item{...}{further graphical parameters, forwarded only when a single panel
is requested}
}
\value{
\code{x}, invisibly
}
\description{
Dispatches to the diagnostic plots: the trace of the criterion sequence
(\code{\link{plotICtrace}}), the marginal inclusion probabilities
(\code{\link{plotMargProb}}), the top-model visit frequencies
(\code{\link{plotModelFreq}}), the Gelman-Rubin shrink factor
(\code{\link{plotGelman}}), and the criterion-trace autocorrelation
(\code{\link{plotAutocorr}}).  Several may be requested at once, in which case
they are drawn in a multi-panel layout.
}
