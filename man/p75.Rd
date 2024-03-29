% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/btools_stats.r
\name{p75}
\alias{p75}
\title{Compute the sample 75th percentile.}
\usage{
p75(x, na.rm = FALSE)
}
\arguments{
\item{x}{a numeric vector containing the values whose 75th percentile is to be computed.}

\item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
}
\value{
numeric
}
\description{
Compute the sample 75th percentile.
}
\examples{
p75(1:100)
p75(c(1:10, NA, 11:100), na.rm=TRUE)
}
