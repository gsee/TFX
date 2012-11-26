\name{ParseTrueFX}
\alias{ParseTrueFX}
\title{Parse TrueFX(tm) response}
\usage{
  ParseTrueFX(x, pretty = TRUE)
}
\arguments{
  \item{x}{The response from a TrueFX(tm) request.  Can be
  any of the three formats: \code{default}, \code{csv} or
  \code{html}}

  \item{pretty}{logical. If \code{TRUE} (Default), output
  will be converted to a data.frame and columns will be
  converted from character to the appropriate classes and
  combined.}
}
\value{
  By default, a \code{data.frame} is returned that has
  columns \dQuote{Bid.Price}, \dQuote{Ask.Price},
  \dQuote{High}, \dQuote{Low}, and \dQuote{TimeStamp}.  If
  called with \code{pretty=FALSE}, a list of character
  vectors -- named \dQuote{Symbol}, \dQuote{BidBigNumber},
  \dQuote{BidPip}, \dQuote{OfferBigNumber},
  \dQuote{OfferPip}, \dQuote{High}, \dQuote{Low},
  \dQuote{TimeStamp} -- will be returned.

  If the format is \dQuote{html}, there will also be an
  \dQuote{Open} column
}
\description{
  Parse the results of a TrueFX(tm) query.
}
\details{
  This function will parse the results of a call to
  \code{\link{QueryTrueFX}}. It can handle any of the three
  TrueFX(tm) response formats: \dQuote{default},
  \dQuote{csv}, or \dQuote{html}.  By default, it will
  convert the results into a nicely formatted
  \code{data.frame}.  If, called with \code{pretty=FALSE},
  a list of strings will be returned.

  All times are in GMT
}
\note{
  Although the TrueFX(tm) Market Data Web API Developer
  Guide indicates that both the \dQuote{csv} and
  \dQuote{html} formats include values for \dQuote{Open},
  only the \dQuote{html} format actually does.
}
\examples{
# x <- QueryTrueFX()  #Cannot run this if no internet connection
x <- paste0("EUR/USDUSD/JPY1.31#81.9085661.31#81.9435941.31990#81.6421.3182",
            "1#81.50413351311514701335131150004")
ParseTrueFX(x)
ParseTrueFX(x, pretty=FALSE)
}
\author{
  Garrett See
}
\references{
  \url{http://www.truefx.com/dev/data/TrueFX_MarketDataWebAPI_DeveloperGuide.pdf}
}
\seealso{
  \code{\link{QueryTrueFX}}, \code{\link{TrueFXRef}}
}

