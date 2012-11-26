\name{isActive}
\alias{isActive}
\alias{isActive.TFXsession}
\title{Is a session active?}
\usage{
  isActive(x, ...)

  \method{isActive}{TFXsession} (x, ...)
}
\arguments{
  \item{x}{an object to test}

  \item{...}{other arguments for methods}
}
\value{
  logical
}
\description{
  Test to see if a session is active
}
\details{
  In order for a TFXsession to be active, it must have been
  authenticated less than 1 minute ago.  If it was created
  with \code{snapshot=TRUE} it will become inactive after
  it is used once.
}
\note{
  This function assumes that if the session has not been
  used in 60 seconds is not active, even though TrueFX(tm)
  sessions actually stay active for roughly 70 seconds.
}
\examples{
\dontrun{
sess <- ConnectTrueFX("GBP/JPY", username='JSTrader', password='Ou812')
isActive(sess) #TRUE
}
}

