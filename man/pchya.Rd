% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/btools_stats.r
\name{pchya}
\alias{pchya}
\title{Compute year-over-year percent change for annual data}
\usage{
pchya(value, year)
}
\arguments{
\item{value}{numeric vector}

\item{year}{integer or numeric vector}
}
\value{
numeric vector
}
\description{
Compute year-over-year percent change for annual data
}
\examples{
set.seed(1234)
year <- 1970:1990
value <- 100 * (1 + .05 + rnorm(length(year), 0, .002))
cbind(year, value, pchya(value, year))
}
