\name{TrueFXRef}
\alias{TrueFXRef}
\title{Download and view the TrueFX(tm) Market Data Web API Developer Guide}
\usage{
  TrueFXRef(show = c("pdf", "webpage"))
}
\arguments{
  \item{show}{What to show. Either \dQuote{pdf} or
  \dQuote{webpage} (\dQuote{web} and \dQuote{url} will also
  be recognized as \dQuote{webpage}).  Alternatively, can
  be numeric: 1 for \dQuote{pdf}, 2 for \dQuote{webpage})}
}
\value{
  called for side-effect.  Opens the TrueFX(tm) Market Data
  Web API Developer Guide in a pdf-viewer or web browser.
}
\description{
  Download and view the TrueFX(tm) Market Data Web API
  Developer Guide
}
\note{
  the idea for this function came from the \pkg{IBrokers}
  function \code{IBrokersRef} written by Jeff Ryan. Thanks
  Jeff!
}
\examples{
\dontrun{
TrueFXRef()
TrueFXRef("web")
}
}
\author{
  Garrett See
}
\references{
  \url{http://www.truefx.com/dev/data/TrueFX_MarketDataWebAPI_DeveloperGuide.pdf}
}

