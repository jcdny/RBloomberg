##' Get the field metadata from the bloomberg data dictionary
##'
##' @param conn a Bloomberg connection object
##' @param fields a vector of field mnemonics
##'
##' @seealso field.description
##' @export
blpFieldInfo <- function(conn, fields) {
  fields <- .jarray(fields)
  result <- conn$fieldInfo(fields)

  l <- result$getData()
  colnames(l) <- result$getColumnNames()
  rownames(l) <- result$getData()[,2]
  df.data <- as.data.frame(l)

  return(df.data)
}

##' Get the field descriptions from the bloomberg data dictionary
##'
##' @param conn a Bloomberg connection object
##' @param fields a vector of field mnemonics
##'
##' @seealso blpFieldInfo
##' 
##' @export
field.description <- function(conn, fields) {
  as.vector(blpFieldInfo(conn, fields)["description"])
}

