\name{Reconnect}
\alias{Reconnect}
\alias{Reconnect.TFXsession}
\title{Reconnect a session that is no longer active}
\usage{
  Reconnect(x, ...)

  \method{Reconnect}{TFXsession} (x, ...)
}
\arguments{
  \item{x}{an object to be re-connected}

  \item{...}{other args for methods}
}
\value{
  a \code{TFXsession} object of an active/authenticated
  session.
}
\description{
  \code{Reconnect} will create a new session and update the
  `id` to the new authenticated id returned by the
  TrueFX(tm) server.
}
\details{
  After roughly 70 seconds, an authenticated TrueFX(tm)
  session will time-out. Also, a connection made with
  \code{snapshot=FALSE} will be disconnected after it is
  used once.

  A non-active TrueFX(tm) session id is treated like an
  unauthenticated session.
}
\examples{
## Cannot run because there may not be an internet connection
\dontrun{
## You must use your username and password instead of JSTrader and Ou812
sess <- ConnectTrueFX("USD/JPY", username='JSTrader', password='Ou812')
Disconnect(sess)
isActive(sess) #FALSE
Reconnect(sess)
isActive(sess) #TRUE
}
}

