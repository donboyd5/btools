% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/btools_utilities.r
\name{ns}
\alias{ns}
\title{create vector of sorted names of a data frame}
\usage{
ns(df)
}
\arguments{
\item{df}{Data frame}
}
\value{
a vector of sorted names
}
\description{
create vector of sorted names of a data frame
}
\examples{
names(iris) # unsorted
ns(iris)
}
