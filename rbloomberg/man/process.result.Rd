\name{process.result}
\alias{process.result}
\title{Process a result set from conn$\emph{function}...}
\usage{process.result(result, row.name.source="none")}

\description{
  Process a result set from \code{conn$\emph{function}}
}

\details{
  Given a result set, process it into a data frame.
}
\keyword{internal}
\arguments{
  \item{result}{the return from the java method}
  \item{row.name.source}{from \code{"java"},\code{"first.column"}, or \code{"none"}}
}
