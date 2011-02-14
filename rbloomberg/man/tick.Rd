\name{tick}
\alias{tick}
\title{Retrieve Bloomberg tick history.}
\usage{tick(conn, security, fields, start_date_time,
    end_date_time, option_names, option_values)}

\description{
  Retrieve Bloomberg tick history.
}
\seealso{\code{\link{bar}}}
\value{a data frame}
\author{Ana Nelson \email{ana@ananelson.com}}
\arguments{
  \item{conn}{a Bloomberg connection object}
  \item{security}{bloomberg ticker}
  \item{fields}{vector of field mnemonics}
  \item{start_date_time}{date object}
  \item{end_date_time}{date object}
  \item{option_names}{vector of retrieval options}
  \item{option_values}{vector of option values}
}
