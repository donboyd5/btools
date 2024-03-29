% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/btools_stats.r
\name{p50}
\alias{p50}
\title{Compute the sample 50th percentile (median).}
\usage{
p50(x, na.rm = FALSE)
}
\arguments{
\item{x}{a numeric vector containing the values whose 50th percentile is to be computed.}

\item{na.rm}{a logical value indicating whether NA values should be stripped before the computation proceeds.}
}
\value{
numeric
}
\description{
Compute the sample 50th percentile (median).
}
\examples{
p50(1:100)
p50(c(1:10, NA, 11:100), na.rm=TRUE)
}
