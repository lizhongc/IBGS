\name{plotAutocorr}
\alias{plotAutocorr}
\title{Autocorrelation plot for the criterion trace}
\usage{
plotAutocorr(result, col = NULL, ...)
}
\arguments{
\item{result}{an \code{"IBGS"} result from one of the samplers}

\item{col}{colour of the sticks, default from the package palette}

\item{...}{further graphical parameters passed to \code{\link{plot}}}
}
\value{
invisibly, the named vector of autocorrelations (named by lag)
}
\description{
Draws the lagged autocorrelation of the criterion trace
(\code{result$convergence$autocorr}) as a stick plot.  A trace that mixes well
decays to zero quickly; persistent high autocorrelation signals a slowly-mixing
chain and a small effective sample size.
}
\details{
The autocorrelations are computed in C with no dependency on the \pkg{coda}
package, matching \code{stats::acf} applied to the criterion trace.
}
