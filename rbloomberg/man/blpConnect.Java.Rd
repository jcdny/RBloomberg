\name{blpConnect.Java}
\alias{blpConnect.Java}
\title{Connect to the server via the Java API library.}
\usage{blpConnect.Java(log.level, blpapi.jar.file,
    throw.ticker.errors, jvm.params, host, port)}

\description{
  Connect to the server via the Java API library.  JVM
  initialized as a side effect.
}
\seealso{blpConnect}
\keyword{internal}
\arguments{
  \item{log.level}{the log4j logging level; \code{"finest"}, \code{"fine"}, \code{"info"}, \code{"warning"}.}
  \item{blpapi.jar.file}{explicit path the the Bloomberg java API file.}
  \item{throw.ticker.errors}{throw an error for invalid tickers (default \code{TRUE})}
  \item{jvm.params}{parameters passed to the jvm as a vector of strings (for example \code{jvm.params=c("-Xmx512m", "-Xloggc:jvmgc.log")})}
  \item{host}{host to connect to (for SAPI)}
  \item{port}{port to connect to (for SAPI, default 8194)}
}
