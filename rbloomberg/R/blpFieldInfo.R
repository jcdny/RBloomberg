##' Get the field metadata from the bloomberg data dictionary
##'
##' @param conn a Bloomberg connection object
##' @param fields a vector of field mnemonics
##'
##' @seealso \code{\link{field.description}}
##' @export
blpFieldInfo <- function(conn, fields) {
  stopifnot(is(conn,"jobjRef"))

  jfields <- .jarray(fields)
  result <- conn$fieldInfo(jfields)

  l <- result$getData()
  colnames(l) <- result$getColumnNames()
  rownames(l) <- fields
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

