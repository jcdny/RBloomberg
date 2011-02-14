\name{convert.to.type}
\alias{convert.to.type}
\title{Convert columns of a data frame to the specified types.}
\usage{convert.to.type(df.data, data_types)}

\description{
  Convert columns of a data frame to the specified types.
}
\keyword{internal}
\arguments{
  \item{df.data}{the bloomberg result set, converted to a data frame}
  \item{data_types}{the data types as returned by
\code{result$getDataTypes()}.  Types are \code{FLOAT64},
\code{INT32}, \code{INT64}, \code{STRING}, \code{DATE},
\code{DATETIME}, \code{NOT_APPLICABLE}, \code{CHAR}.}
}
