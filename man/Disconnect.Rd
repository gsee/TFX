\name{Disconnect}
\alias{Disconnect}
\alias{Disconnect.TFXsession}
\title{Disconnect a session}
\usage{
  Disconnect(x, ...)

  \method{Disconnect}{TFXsession} (x, ...)
}
\arguments{
  \item{x}{an object to disconnect}

  \item{...}{other arguments for methods}
}
\value{
  a disconnected/inactive \code{TFXsession} object
}
\description{
  Disconnect a session (make it inactive).
}
\examples{
\dontrun{
sess <- ConnectTrueFX(username='JSTrader', password='Ou812')
isActive(sess) #TRUE
Disconnect(sess)
isActive(sess) #FALSE
}
}
\seealso{
  \code{\link{ConnectTrueFX}}, \code{\link{Reconnect}}
}

