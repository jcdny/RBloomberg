##' Get the field metadata from the bloomberg data dictionary
##'
##' @param conn a Bloomberg connection object
##' @param fields a vector of field mnemonics
##'
##' @seealso \code{\link{field.description}}
##' @export
blpFieldInfo <- function(conn, fields) {
  stopifnot(is(conn,"jobjRef"))

  ## make fields unique since otherwise dataframe has
  ## dup rownames
  fields <- unique(toupper(fields))

  jfields <- .jarray(fields)
  result <- conn$fieldInfo(jfields)
  l <- result$getData()
  colnames(l) <- result$getColumnNames()

  ## Here we just map missing mnemonics to the NA rows and assume we
  ## are 1-1.  Not sure if thats a good assumption
  rows <- l[,"mnemonic"]
  rows[which(is.na(rows))] <- fields[(! fields %in% l[,"mnemonic"] )]
  rownames(l) <- rows

  df.data <- as.data.frame(l)

  return(df.data)
}

##' Get the field descriptions from the bloomberg data dictionary
##'
##' @param conn a Bloomberg connection object
##' @param fields a vector of field mnemonics
##'
##' @seealso \code{\link{blpFieldInfo}}
##'
##' @export
field.description <- function(conn, fields) {
  as.vector(blpFieldInfo(conn, fields)["description"])
}

