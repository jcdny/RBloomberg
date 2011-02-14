\name{bds}
\alias{bds}
\title{Retrieve Bloomberg bulk data fields.}
\usage{bds(conn, securities, fields, override_fields,
    override_values, option_names, option_values)}

\description{
  Retrieve Bloomberg bulk data fields.
}

\details{
  May pass either a single security/field or a vector of
  securities and fields.

  If passed multiple securities or fields they are queried
  per security+field and merged into a single data frame.
  Raise an error if the results are not conformal.

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
