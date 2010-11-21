##' Close the bloomberg connection.
##'
##' Closes the connection and performs a round of garbage collection.
##'
##' @param conn a Bloomberg connection object
##'
##' @seealso \code{\link{blpConnect}}
##'
##' @export
blpDisconnect <- function(conn) {
  conn$close()
  gc(verbose=FALSE)
}
