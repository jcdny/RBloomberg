##' Close the bloomberg connection and do a gc
##'
##' @param conn a Bloomberg connection object
##'
##' @seealso blpConnect
##'
##' @export
blpDisconnect <- function(conn) {
  conn$close()
  gc(verbose=FALSE)
}
