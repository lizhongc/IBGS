\name{summary.IBGS}
\alias{summary.IBGS}
\alias{print.summary.IBGS}
\title{Tabular summary of an IBGS fit}
\usage{
\method{summary}{IBGS}(object, ...)

\method{print}{summary.IBGS}(x, ...)
}
\arguments{
\item{object}{an \code{"IBGS"} result from one of the samplers}

\item{...}{ignored}

\item{x}{a \code{"summary.IBGS"} object}
}
\value{
invisibly, an object of class \code{"summary.IBGS"} with the selected
  variable table, the top-model table and the convergence diagnostics of the
  criterion trace (Gelman-Rubin R-hat, Geweke z, effective sample size and
  lag-1 autocorrelation), which \code{print} displays as a "Convergence
  diagnostics" block
}
\description{
Tabular summary of an IBGS fit
}
