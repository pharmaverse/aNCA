% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{.compress_range}
\alias{.compress_range}
\title{Compresses a numeric vector into the simplest possible character string that, when evaluated,
will create the same numeric vector.}
\usage{
.compress_range(range_vector)
}
\arguments{
\item{range_vector}{numeric vector with numbers to compress into string}
}
\value{
simplest possible character string representing provided vector
}
\description{
Compresses a numeric vector into the simplest possible character string that, when evaluated,
will create the same numeric vector.
}
\examples{
\dontrun{
# Basic usage
.compress_range(c(1, 2, 3, 4)) # "1:4"
.compress_range(c(15, 1, 11, 4, 5, 10, 2, 12, 3)) # "1:5,10:12,15"
}

}
