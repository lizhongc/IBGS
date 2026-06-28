\name{plotGelman}
\alias{plotGelman}
\title{Gelman-Rubin shrink-factor plot for the criterion trace}
\usage{
plotGelman(result, col = NULL, legend = TRUE, ...)
}
\arguments{
\item{result}{an \code{"IBGS"} result from one of the samplers}

\item{col}{colour of the median R-hat curve, default from the package palette}

\item{legend}{draw an explanatory legend, default \code{TRUE}}

\item{...}{further graphical parameters passed to \code{\link{plot}}}
}
\value{
invisibly, the \code{data.frame} of shrink-factor values
}
\description{
Plots the evolving Gelman-Rubin potential scale reduction factor (R-hat) of the
criterion trace as the run lengthens, computed from the single chain split into
equal contiguous segments (\code{result$convergence$shrink}).  The median and
the 97.5\% upper limit are drawn together with a horizontal guide at 1 (perfect
mixing) and at 1.1 (the usual convergence rule of thumb): once both curves settle
below about 1.1 the sampler has stabilised.
}
\details{
The diagnostic is computed in C with no dependency on the \pkg{coda} package, but
follows the same definition as \code{coda::gelman.diag} / \code{coda::gelman.plot}
applied to the criterion trace split into segments (split-R-hat).
}
