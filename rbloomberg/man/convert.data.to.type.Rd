\name{convert.data.to.type}
\alias{convert.data.to.type}
\title{Convert a matrix to a data frame.}
\usage{convert.data.to.type(matrix.data, data_types)}

\description{
  Convert a matrix to a data frame.
}

\details{
  Take results as matrix and a vector of data types and
  coerce to a data frame with columns of the specified
  types.
}
\keyword{internal}
\arguments{
  \item{matrix.data}{the matrix from the bloomberg result}
  \item{data_types}{the data types as returned by \code{result$getDataTypes()}}
}
