\name{bdh}
\alias{bdh}
\title{Retrieve Bloomberg historical data.}
\usage{bdh(conn, securities, fields, start_date, end_date,
    override_fields, override_values, option_names,
    option_values, always.display.tickers=FALSE,
    dates.as.row.names=(length(securities) == 1),
    include.non.trading.days)}

\description{
  Retrieve Bloomberg historical data.
}

\details{
  Pass either a single security/field or a vector of
  securities and fields. Objects are converted with .jarray
  before being passed to the Java wrapper which accesses
  the Bloomberg API and returns the result.

  Overrides which are dates must be passed in "YYYYMMDD"
  format as per Bloomberg Version 3 API.

  Pass each security+field separately. Merge resulting data
  frames if the results are conformal, raise an error if
  they're not.
}
\value{a data frame}
\author{Ana Nelson \email{ana@ananelson.com}}
\arguments{
  \item{conn}{a Bloomberg connection object}
  \item{securities}{A single ticker string or a vector of tickers.}
  \item{fields}{A single field string or a vector of field names.}
  \item{start_date}{date object, required}
  \item{end_date}{date object, optional}
  \item{override_fields}{vector of fields to overide}
  \item{override_values}{values corresponding to override fields}
  \item{option_names}{vector of retrieval options}
  \item{option_values}{vector of option values}
  \item{always.display.tickers}{force tickers to be included even if only passing one security}
  \item{dates.as.row.names}{default TRUE if 1 ticker passed}
  \item{include.non.trading.days}{TRUE includes records for all calendar days}
}
