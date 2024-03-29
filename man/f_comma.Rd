% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/btools_formatting.r
\name{f_comma}
\alias{f_comma}
\title{Format numeric vector in comma format}
\usage{
f_comma(num, ...)
}
\arguments{
\item{num}{numeric vector to format}

\item{...}{arguments passed to scales::label_comma}
}
\value{
formatted values as a character vector.
}
\description{
\code{f_comma} comma format - calls scales::label_comma
}
\details{
format the vector using scales::label_comma
}
\examples{
f_comma(1.2345)
f_comma(1.2345, scale=1000)
f_comma(1.2345, scale=1000, accuracy=0.01)
}
\keyword{f_comma}
