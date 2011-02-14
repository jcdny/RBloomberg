\name{bdp}
\alias{bdp}
\title{Retrieve Bloomberg reference data.}
\usage{bdp(conn, securities, fields, override_fields,
    override_values, option_names, option_values)}

\description{
  Retrieve Bloomberg reference data.
}

\details{
  Pass either a single security/field or a vector of
  securities and fields. Objects are converted with .jarray
  before being passed to the Java wrapper which accesses
  the Bloomberg API and returns the result.

  Overrides which are dates must be passed in "YYYYMMDD"
  format as per Bloomberg Version 3 API.
}
\value{a data frame}
\author{Ana Nelson \email{ana@ananelson.com}}
\arguments{
  \item{conn}{a Bloomberg connection object}
  \item{securities}{A single ticker string or a vector of tickers.}
  \item{fields}{A single field string or a vector of field names.}
  \item{override_fields}{vector of fields to overide}
  \item{override_values}{values corresponding to override fields}
  \item{option_names}{vector of retrieval options}
  \item{option_values}{vector of option values}
}
