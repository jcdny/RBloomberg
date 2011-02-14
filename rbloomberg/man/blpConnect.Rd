\name{blpConnect}
\alias{blpConnect}
\title{Connect to the Bloomberg server.}
\usage{blpConnect(iface="Java", log.level="warning",
    blpapi.jar.file, throw.ticker.errors=TRUE, jvm.params,
    host, port)}

\description{
  Connect to the Bloomberg server.
}

\details{
  Create a the connection to Bloomberg by default on the
  local machine or to a SAPI server.  When the connection
  is created the jvm is also initialized.
}
\seealso{\code{\link{blpDisconnect}}}
\arguments{
  \item{iface}{The API interface to use.  Currently only \code{"Java"} supported}
  \item{log.level}{the log4j logging level from \code{"finest"}, \code{"fine"}, \code{"info"}, \code{"warning"}}
  \item{blpapi.jar.file}{explicit path the the Bloomberg java API
file.  The code looks for the jar file in likely locations
\code{/opt/local/BLP/APIv3/JavaAPI/blpjavaapi.jar} on unix and
\code{C:\\blp\\API\\APIv3\\JavaAPI\\VERSION\\blpapi3.jar} on
windows.}
  \item{throw.ticker.errors}{throw an error for invalid tickers (default \code{TRUE})}
  \item{jvm.params}{parameters passed to the jvm as a vector of strings (eg \code{jvm.params=c("-Xmx512m","-Xloggc:jvmgc.log")})}
  \item{host}{host to connect to (for SAPI)}
  \item{port}{port to connect to (for SAPI, default 8194)}
}
