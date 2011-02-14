\name{bar}
\alias{bar}
\title{Retrieve Bloomberg minute bars.}
\usage{bar(conn, security, field, start_date_time, end_date_time,
    interval)}

\description{
  Retrieve Bloomberg minute bars.
}
\seealso{\code{\link{tick}}}
\value{a data frame}
\author{Ana Nelson \email{ana@ananelson.com}}
\arguments{
  \item{conn}{a Bloomberg connection object}
  \item{security}{bloomberg ticker}
  \item{field}{field mnemonic}
  \item{start_date_time}{date object}
  \item{end_date_time}{date object}
  \item{interval}{bar interval}
}
