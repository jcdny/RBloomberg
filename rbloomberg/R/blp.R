##' Deprecated function
##'
##' @name blp
##' @aliases blp blpGetData
##' @seealso \code{\link{bdp}}, \code{\link{bdh}}, \code{\link{tick}}, \code{\link{bar}}
##'
##' @export blp blpGetData
blp <- blpGetData <- function(conn, securities, fields, start = NULL, end = NULL,
                              barsize = NULL, barfields = NULL, retval = NULL,
                              override_fields = NULL, overrides = NULL, currency = NULL) {
  if (is.null(start)) {
    stop("The blp() function has been removed. Please consult documentation for the bdp() function, or the bds() function for making bulk data calls.")
  } else {
    if (is.null(barsize)){
      stop("The blp() function has been removed. Please consult documentation for the bdh() function.")
    } else if (barsize == 0) {
      stop("The blp() function has been removed. Please consult documentation for the tick() function.")
    } else {
      stop("The blp() function has been removed. Please consult documentation for the bar() function.")
    }
  }
}

##' Retrieve Bloomberg reference data.
##'
##' Pass either a single security/field or a vector of securities and
##' fields. Objects are converted with .jarray before being passed to
##' the Java wrapper which accesses the Bloomberg API and returns the
##' result.
##'
##' Overrides which are dates must be passed in "YYYYMMDD" format as
##' per Bloomberg Version 3 API.
##'
##' @param conn Connection object
##' @param securities A single ticker string or a vector of tickers.
##' @param fields A single field string or a vector of field names.
##' @param override_fields vector of fields to overide
##' @param override_values values corresponding to override fields
##' @param option_names vector of retrieval options
##' @param option_values vector of option values
##'
##' @return a data frame
##' @author Ana Nelson \email{ana@@ananelson.com}
##'
##' @export
bdp <- function(conn, securities, fields,
                override_fields = NULL, override_values = NULL,
                option_names = NULL, option_values = NULL) {

  stopifnot(is(conn,"jobjRef"))

  securities <- .jarray(securities)
  fields <- .jarray(fields)

  if (is.null(override_fields) && is.null(option_names)) {
    result <- conn$blp(securities, fields)
  } else if (is.null(option_names)) {
    override_fields <- .jarray(override_fields)
    override_values <- .jarray(override_values)
    result <- conn$blp(securities, fields, override_fields, override_values)
  } else if (is.null(override_fields)) {
    override_fields <- .jarray("IGNORE")
    override_values <- .jarray("IGNORE")
    option_names <- .jarray(option_names)
    option_values <- .jarray(option_values)
    result <- conn$blp(securities, fields, override_fields, override_values, option_names, option_values)
  } else {
    override_fields <- .jarray(override_fields)
    override_values <- .jarray(override_values)
    option_names <- .jarray(option_names)
    option_values <- .jarray(option_values)
    result <- conn$blp(securities, fields, override_fields, override_values, option_names, option_values)
  }

  return(process.result(result, "java"))
}

##' Retrieve Bloomberg bulk data field.
##'
##' Pass either a single security/field or a vector of securities and
##' fields. Objects are converted with .jarray before being passed to
##' the Java wrapper which accesses the Bloomberg API and returns the
##' result.
##'
##' Overrides which are dates must be passed in "YYYYMMDD" format as
##' per Bloomberg Version 3 API.
##'
##' Pass each security+field separately. Merge resulting data frames
##' if the results are conformal, raise an error if they're not.
##'
##' @param conn Connection object
##' @param securities A single ticker string or a vector of tickers.
##' @param fields A single field string or a vector of field names.
##' @param override_fields vector of fields to overide
##' @param override_values values corresponding to override fields
##' @param option_names vector of retrieval options
##' @param option_values vector of option values
##'
##' @return a data frame
##' @author Ana Nelson \email{ana@@ananelson.com}
##'
##' @export
bds <- function(conn, securities, fields,
                override_fields = NULL, override_values = NULL,
                option_names = NULL, option_values = NULL) {
  stopifnot(is(conn,"jobjRef"))

  ## Pass each security+field separately. Merge resulting data frames
  ## if the results are conformal, raise an error if they're not.
  stored.names <- NULL
  combined <- NULL

  combine.multiple = (length(securities) + length(fields) > 2)

  for (security in securities) {
    for (field in fields) {
      if (is.null(override_fields) && is.null(option_names)) {
        result <- conn$bls(security, field)
      } else if (is.null(option_names)) {
        override_fields <- .jarray(override_fields)
        override_values <- .jarray(override_values)
        result <- conn$bls(security, field, override_fields, override_values)
      } else if (is.null(override_fields)) {
        override_fields <- .jarray("IGNORE")
        override_values <- .jarray("IGNORE")
        option_names <- .jarray(option_names)
        option_values <- .jarray(option_values)
        result <- conn$bls(security, field, override_fields, override_values, option_names, option_values)
      } else {
        override_fields <- .jarray(override_fields)
        override_values <- .jarray(override_values)
        option_names <- .jarray(option_names)
        option_values <- .jarray(option_values)
        result <- conn$bls(security, field, override_fields, override_values, option_names, option_values)
      }

      if (all(dim(result$getData()) == 0)) next # Skip empty results.
      result <- process.result(result) # Convert to data frame.

      if (combine.multiple) {
        ## Prepend data frame with new row containing security ticker.
        result <- data.frame(ticker = security, result)
      }

      if (is.null(stored.names)) {
        stored.names <- colnames(result)
        if (!is.null(combined)) stop("combined should be null if stored.names is null")
        combined <- result
      } else {
        if (!all(colnames(result) == stored.names)) stop(paste("returned names", colnames(result), "do not match previous names", stored.names))
        if (!combine.multiple) stop("combine.multiple should be true if we are running through loop more than once")
        combined <- rbind(combined, result)
      }
    }
  }

  return(combined)
}

##' Retrieve Bloomberg historical data.
##'
##' Pass either a single security/field or a vector of securities and
##' fields. Objects are converted with .jarray before being passed to
##' the Java wrapper which accesses the Bloomberg API and returns the
##' result.
##'
##' Overrides which are dates must be passed in "YYYYMMDD" format as
##' per Bloomberg Version 3 API.
##'
##' Pass each security+field separately. Merge resulting data frames
##' if the results are conformal, raise an error if they're not.
##'
##' @param conn Connection object
##' @param securities A single ticker string or a vector of tickers.
##' @param fields A single field string or a vector of field names.
##' @param start_date date object, required
##' @param end_date date object, optional
##' @param override_fields vector of fields to overide
##' @param override_values values corresponding to override fields
##' @param option_names vector of retrieval options
##' @param option_values vector of option values
##' @param always.display.tickers force tickers to be included even if only passing one security
##' @param dates.as.row.names default TRUE if 1 ticker passed
##' @param include.non.trading.days TRUE includes records for all calendar days
##'
##' @return a data frame
##' @author Ana Nelson \email{ana@@ananelson.com}
##'
##' @export
bdh <- function(conn, securities, fields, start_date, end_date = NULL,
                override_fields = NULL, override_values = NULL,
                option_names = NULL, option_values = NULL,
                always.display.tickers = FALSE, dates.as.row.names = (length(securities) == 1),
                include.non.trading.days = NULL) {

  stopifnot(is(conn,"jobjRef"))

  fields <- .jarray(fields)

  if (!is.null(override_fields)) {
    override_fields <- .jarray(override_fields)
    override_values <- .jarray(override_values)
  }

  start_date = format(start_date, format="%Y%m%d")
  if (!is.null(end_date)) {
    end_date = format(end_date, format="%Y%m%d")
  }

  combined <- NULL
  combine.multiple <- (length(securities) > 1)

  if (combine.multiple && is.null(include.non.trading.days)) {
    include.non.trading.days = TRUE
    ## TODO Should raise error if set to FALSE?
  }

  if (is.null(include.non.trading.days)) {
    ## We don't want to call 'if' on a NULL value.
  } else if (include.non.trading.days) {
    option_names <- c("nonTradingDayFillOption", "nonTradingDayFillMethod", option_names)
    option_values <- c("ALL_CALENDAR_DAYS", "NIL_VALUE", option_values)
  }

  if (!is.null(option_names)) {
    option_names <- .jarray(option_names)
    option_values <- .jarray(option_values)
  }

  i <- 0

  for (security in securities) {
    i <- i+1

    if (is.null(end_date)) {
      if (is.null(override_fields) && is.null(option_names)) {
        result <- conn$blh(security, fields, start_date)
      } else if (is.null(option_names)) {
        result <- conn$blh(security, fields, start_date, override_fields, override_values)
      } else if (is.null(override_fields)) {
        override_fields <- .jarray("IGNORE")
        override_values <- .jarray("IGNORE")

        result <- conn$blh(security, fields, start_date, override_fields, override_values, option_names, option_values)
      } else {
        result <- conn$blh(security, fields, start_date, override_fields, override_values, option_names, option_values)
      }
    } else {
      if (is.null(override_fields) && is.null(option_names)) {
        result <- conn$blh(security, fields, start_date, end_date)
      } else if (is.null(option_names)) {
        result <- conn$blh(security, fields, start_date, end_date, override_fields, override_values)
      } else if (is.null(override_fields)) {
        override_fields <- .jarray("IGNORE")
        override_values <- .jarray("IGNORE")

        result <- conn$blh(security, fields, start_date, end_date, override_fields, override_values, option_names, option_values)
      } else {
        result <- conn$blh(security, fields, start_date, end_date, override_fields, override_values, option_names, option_values)
      }
    }

    matrix.data <- result$getData()
    column.names <- result$getColumnNames()
    data.types <- result$getDataTypes()

    if (combine.multiple || always.display.tickers) {
      matrix.data <- cbind(rep(security, dim(matrix.data)[1]), matrix.data)
      column.names <- c("ticker", column.names)
      data.types <- c("STRING", data.types)
    }

    num.dates <- dim(matrix.data)[1]
    num.tickers <- length(securities)
    s <- (i-1)*num.dates + 1
    f <- (i)*num.dates

    if (is.null(combined)) { # First time through loop...
      if (combine.multiple) {
        ## Allocate storage for expected number of responses.
        combined <- matrix(, ncol=dim(matrix.data)[2], nrow=num.dates * num.tickers)

        ## Store this iteration's results
        combined[s:f,] <- matrix.data
      } else { # We're only looping once...
        combined <- matrix.data
      }
    } else { # Not the first time through loop...
      if (!combine.multiple) stop("combine.multiple should be true if we are running through loop more than once")
      combined[s:f,] <- matrix.data
    }
  }

  if (is.null(combined)) {
    return(NULL)
  } else {
    if (dates.as.row.names) {
      if (combine.multiple) stop("Can't use dates as row names with multiple tickers, dates will not be unique.")
      if (always.display.tickers) {
        rownames(combined) <- matrix.data[,2]
      } else {
        rownames(combined) <- matrix.data[,1]
      }
    }

    colnames(combined) <- column.names
    combined <- convert.data.to.type(combined, data.types)
    return(combined)
  }
}

##' Retrieve Bloomberg minute bars
##'
##' Pass either a single security/field or a vector of securities and
##' fields. Objects are converted with .jarray before being passed to
##' the Java wrapper which accesses the Bloomberg API and returns the
##' result.
##'
##' Overrides which are dates must be passed in "YYYYMMDD" format as
##' per Bloomberg Version 3 API.
##'
##' Pass each security+field separately. Merge resulting data frames
##' if the results are conformal, raise an error if they're not.
##'
##' @seealso \code{\link{tick}}
##'
##' @param conn Connection object
##' @param security bloomberg ticker
##' @param field field mnemonic
##' @param start_date_time date object
##' @param end_date_time date object
##' @param interval bar interval
##'
##' @return a data frame
##'
##' @author Ana Nelson \email{ana@@ananelson.com}
##'
##' @export
bar <- function(conn, security, field, start_date_time, end_date_time, interval) {
  stopifnot(is(conn,"jobjRef"))
  result <- conn$bar(security, field, start_date_time, end_date_time, interval)
  return(process.result(result, "first.column"))
}

##' Retrieve Bloomberg tick history
##' Retrieve Bloomberg minute bars
##'
##' Pass either a single security/field or a vector of securities and
##' fields. Objects are converted with .jarray before being passed to
##' the Java wrapper which accesses the Bloomberg API and returns the
##' result.
##'
##' Overrides which are dates must be passed in "YYYYMMDD" format as
##' per Bloomberg Version 3 API.
##'
##' Pass each security+field separately. Merge resulting data frames
##' if the results are conformal, raise an error if they're not.
##'
##' @seealso \code{\link{bar}}
##'
##' @param conn Connection object
##' @param security bloomberg ticker
##' @param fields vector of field mnemonics
##' @param start_date_time date object
##' @param end_date_time date object
##' @param option_names vector of retrieval options
##' @param option_values vector of option values
##' @return a data frame
##'
##' @author Ana Nelson \email{ana@@ananelson.com}
##'
##' @export
tick <- function(conn, security, fields, start_date_time, end_date_time,
                 option_names = NULL, option_values = NULL) {
  stopifnot(is(conn,"jobjRef"))
  fields <- .jarray(fields);

  if (is.null(option_names)) {
    result <- conn$tick(security, fields, start_date_time, end_date_time)
  } else {
    option_names <- .jarray(option_names)
    option_values <- .jarray(option_values)
    result <- conn$tick(security, fields, start_date_time, end_date_time, option_names, option_values)
  }
  return(process.result(result))
}

##' Given a result set, process it into a data frame.
##'
##' @param result the return from the java method
##' @param row.name.source from \code{"java"},\code{"first.column"}, or \code{"none"}
##'
##' @keywords internal
process.result <- function(result, row.name.source = "none") {
  matrix.data <- result$getData()
  if (is.null(matrix.data)) return(NULL)

  rownames(matrix.data) <- switch(row.name.source,
                                  java = result$getRowNames(),
                                  first.column = matrix.data[,1],
                                  none = NULL,
                                  stop(paste("don't know how to handle this row name source", row.name.source))
                                  )

  colnames(matrix.data) <- result$getColumnNames()

  convert.data.to.type(matrix.data, result$getDataTypes())
}

##' Take a matrix and coerce columns to the passed vector of types
##'
##' @param matrix.data the matrix from the bloomberg result
##' @param data_types the data types as returned by \code{result$getDataTypes()}
##' @keywords internal
convert.data.to.type <- function(matrix.data, data_types) {
  df.data <- as.data.frame(matrix.data)

  if (dim(df.data)[2] > 0) {
    convert.to.type(df.data, data_types)
  } else {
    df.data
  }
}

##' Take a data frame and coerce columns to the passed vector of types
##'
##' @param df.data the bloomberg result set, converted to a data frame
##' @param data_types the data types as returned by \code{result$getDataTypes()}
##'
##' @keywords internal
convert.to.type <- function(df.data, data_types) {
  for (i in 1:(dim(df.data)[2])) {
    string_values = as.vector(df.data[,i])

    new_values <- switch(data_types[i],
                         FLOAT64 = as.numeric(string_values),
                         INT32 = as.numeric(string_values),
                         INT64 = as.numeric(string_values),
                         STRING = string_values,
                         DATE = string_values,
                         DATETIME = string_values,
                         NOT_APPLICABLE = string_values,
                         CHAR = string_values == 'Y', # Assumes CHAR is only used for Boolean values and can be trusted to return 'Y' or 'N'.
                         stop(paste("unknown type", data_types[i]))
                         )
    df.data[,i] <- new_values
  }

  return(df.data)
}

