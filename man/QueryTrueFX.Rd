\name{ConnectTrueFX}
\alias{ConnectTrueFX}
\alias{QueryTrueFX}
\title{Query TrueFX(tm)}
\usage{
  ConnectTrueFX(currency.pairs, username, password,
    qualifier = "default", format, snapshot = FALSE)

  QueryTrueFX(session, parse.response = TRUE,
    pretty = TRUE, reconnect = TRUE)
}
\arguments{
  \item{currency.pairs}{character vector, or comma
  delimited string of Symbols (ISO names) of currency
  pairs.  (e.g. \code{"EUR/USD,AUD/USD"}, or
  \code{c("EUR/USD", "AUD/USD")}).  If \code{missing} or if
  \code{nchar(currency.pairs) < 1}, the Symbols of all
  currency pairs for which TrueFX(tm) provides historical
  data will be used (see references section).}

  \item{username}{character.  A registered TrueFX(tm) user
  name; required to establish an authenticated session.}

  \item{password}{character. A registered TrueFX(tm)
  password; required to establish an authenticated session}

  \item{qualifier}{any string; required to establish an
  authenticated session.  (\dQuote{default} by default)}

  \item{format}{One of \dQuote{default}, \dQuote{csv}, or
  \dQuote{html}.  Indicates the format for the HTTP
  Response.}

  \item{snapshot}{logical.  No incremental updates if
  \code{TRUE}}

  \item{session}{a \code{TFXsession} object created by
  \code{ConnectTrueFX}.}

  \item{parse.response}{logical. Should the results be
  passed through \code{ParseTrueFX} before returning?}

  \item{pretty}{logical.  Passed to \code{ParseTrueFX}.
  Indicates whether to format the parsed results and
  convert to \code{data.frame}.  Ignored if
  \code{parse.response} is not \code{TRUE}}

  \item{reconnect}{logical.  If the TFXsession has timed
  out, should it be reconnected?}
}
\value{
  \code{ConnectTrueFX} returns a \code{TFXsession} object
  that is a TrueFX(tm) server-generated session ID returned
  with a successful authenticated session request.  It is a
  colon delimited string with username, password,
  qualifier, and the time (in milliseconds) that the
  session was created.

  \code{QueryTrueFX} returns the results of a TrueFX(tm)
  request using
}
\description{
  Create a session with TrueFX(tm) and request market data.
}
\details{
  If no \code{currency.pairs} are provided to
  \code{ConnectTrueFX}, the 15 pairs for which TrueFX(tm)
  offers historical data will be used.  Note that only the
  first 10 of these are returned in an unauthenticated
  session.

  \code{ConnectTrueFX} will create a \code{TFXsession}
  classed object that can be used in calls to
  \code{QueryTrueFX} to request market data.

  Of the three \code{format}s, \dQuote{default} is the most
  timely (updates first)and \dQuote{csv} is the most
  delayed (updates last)

  the \dQuote{csv} and \dQuote{html} formats have the
  \dQuote{High} and \dQuote{Low} columns backwards.
  (\dQuote{default} does not).  This may be corrected for
  in a future release if the TrueFX(tm) Web service doesn't
  correct it first.
}
\note{
  the formal arguments start with the same lowercase letter
  as their corresponding TrueFX(tm) Market Data Web Query
  Parameters
}
\examples{
## Cannot run these because there may not be an internet connection
\dontrun{
QueryTrueFX()  #unauthenticated
QueryTrueFX(pretty=FALSE)
QueryTrueFX(parse=FALSE)

## For authenticated session, you must have a username and password (it's free).
## Use your username and passward instead of JSTrader and Ou812
id <- ConnectTrueFX('EUR/USD,GBP/USD', username='JSTrader', password='Ou812')
QueryTrueFX(id)
QueryTrueFX(ConnectTrueFX(username='JSTrader', password='Ou812',
                          format='csv'), parse=FALSE)

QueryTrueFX(ConnectTrueFX(username='JSTrader', password='Ou812',
                          format='html'), parse=FALSE)

## If you have shiny installed
## install.packages("shiny", repos="http://rstudio.org/_packages")
library(shiny)
runGist("4122626")
## view the code for this shiny app at
#browseURL("https://gist.github.com/4122626")
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.truefx.com/dev/data/TrueFX_MarketDataWebAPI_DeveloperGuide.pdf}

  \url{http://truefx.com/?page=downloads} to see for which
  pairs TrueFX(tm) provides historical data.
}
\seealso{
  \code{\link{ParseTrueFX}}, \code{\link{Reconnect}},
  \code{\link{TrueFXRef}}
}

