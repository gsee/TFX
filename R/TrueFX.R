
#' @export
#' @rdname QueryTrueFX
ConnectTrueFX <- function(currency.pairs, username, password, 
                          qualifier='default', format, snapshot=FALSE) {
  if (missing(format)) format <- "default"
  if (!substr(format[[1L]], 1, 1) %in% c("d", "c", "h")) {
    warning("unrecognized format. Using default")
    format <- 'default'
  }
  if (missing(currency.pairs)) {
    ## If missing, use the 15 pairs for which TrueFX offers historical data
    currency.pairs <- c("EUR/USD", "USD/JPY", "GBP/USD", "EUR/GBP", "USD/CHF", 
                        "EUR/JPY", "EUR/CHF", "USD/CAD", "AUD/USD", "GBP/JPY", 
                        "AUD/JPY", "AUD/NZD", "CAD/JPY", "CHF/JPY", "NZD/USD")
    ## However, an unauthenticated request only returns the first 10 of those
  }
  x <- unlist(strsplit(gsub(" ", "", currency.pairs), ","))
  cp <- paste(paste(substring(x, 1, 3), 
                    substring(x, nchar(x)-2, nchar(x)), sep="/"), 
              collapse=",")
  base.url <- "http://webrates.truefx.com/rates/connect.html"
  if (missing(username) || missing(password)) {
    stop("missing username or password.")
  } else {
    URL <- paste0(base.url, 
           "?u=", username, 
           "&p=", password, 
           "&q=", qualifier, 
           "&c=", cp)
    if (format != 'default') {
      URL <- paste0(URL, "&f=", format)
    }
    if (isTRUE(snapshot) || tolower(substr(snapshot, 1, 1)) == "y") {
      URL <- paste0(URL, "&s=y")
    }
    structure(readLines(URL), class='TFXid')#returns the session id
  }
}

is.TFXid <- function(x) {
  inherits(x, 'TFXid')
}

# This should disconnect a session; however, it is not advisable to use it
# because a request with a sessionID that has been disconnected will not fail,
# but instead will return the response of an unauthenticated session without
# warning or error.  In a future release, the TFXid object will keep track of
# whether it has been closed (as well as other info, like which currency.pairs
# are associated with it)
DisconnectTrueFX <- function(id) {
  stopifnot(inherits(id, 'TFXid'))
  readLines(paste0("http://webrates.truefx.com/rates/connect.html?di=", id))
  id
}


#' Query TrueFX
#' 
#' Create a session with TrueFX and request market data.
#' 
#' If no \code{currency.pairs} provided to \code{ConnectTrueFX}, the 15 pairs
#' for which TrueFX offers historical data will be used.  Note that only 
#' the first 10 of these are returned in an unauthenticated session.
#' 
#' \code{ConnectTrueFX} will create a \code{TFXid} classed object that can be 
#' used in calls to \code{QueryTrueFX} to request market data.
#' 
#' @param currency.pairs character vector, or comma delimited string of Symbols
#'   (ISO names) of currency pairs.  (e.g. \dQuote{EUR/USD,AUD/USD}, or 
#'   \code{c("EUR/USD", "AUD/USD")})
#' @param username character.  A registered TrueFX user name; required to 
#'   establish an authenticated session.
#' @param password character. A registered TrueFX password; required to 
#'   establish an authenticated session
#' @param qualifier any string; required to establish an authenticated session.
#'   (\dQuote{default} by default)
#' @param format One of \dQuote{default}, \dQuote{csv}, or \dQuote{html}. 
#'   Indicates the format for the HTTP Response.
#' @param snapshot logical.  No incremental updates if \code{TRUE}
#' 
#' @param id a \code{TFXid} object created by \code{ConnectTrueFX}.  
#' @param parse.response logical. Should the results be passed through 
#'   \code{ParseTrueFX} before returning?
#' @param pretty logical.  Passed to \code{ParseTrueFX}.  Indicates whether to
#'   format the parsed results and convert to \code{data.frame}. 
#'   Ignored if \code{parse.response} is not \code{TRUE}
#' @return \code{ConnectTrueFX} returns a \code{TFXid} object that is a 
#'   TrueFX server-generated session ID returned with a successful 
#'   authenticated session request.  It is a colon delimited string with 
#'   username, password, qualifier, and the time (in milliseconds) that the 
#'   session was created.  
#'
#'   \code{QueryTrueFX} returns the results of a 
#'   TrueFX request using \code{TFXid} object returned by \code{ConnectTrueFX}
#' @author Garrett See
#' @references 
#' \url{http://www.truefx.com/dev/data/TrueFX_MarketDataWebAPI_DeveloperGuide.pdf}
#' @seealso \code{\link{ParseTrueFX}}, \code{\link{TrueFXRef}}
#' @note the formal argument start with the same lowercase letter as their 
#'   corresponding TrueFX Market Data Web Query Parameters
#' @examples
#' ## Cannot run these because there may not be an internet connection
#' \dontrun{
#' QueryTrueFX()  #unauthenticated
#' QueryTrueFX(pretty=FALSE)
#' QueryTrueFX(parse=FALSE)
#' 
#' ## For authenticated session, you must have a username and password (it's free).
#' ## Use your username and passward instead of JSTrader and Ou812
#' id <- ConnectTrueFX('EUR/USD,GBP/USD', username='JSTrader', password='Ou812')
#' QueryTrueFX(id)
#' QueryTrueFX(ConnectTrueFX(username='JSTrader', password='Ou812', 
#'                           format='csv'), parse=FALSE)
#' 
#' QueryTrueFX(ConnectTrueFX(username='JSTrader', password='Ou812', 
#'                           format='html'), parse=FALSE)
#' }
#' @export
#' @rdname QueryTrueFX
QueryTrueFX <- function(id, parse.response=TRUE, pretty=TRUE) {
  if (missing(id)) {
    if (isTRUE(parse.response)) {
      return(ParseTrueFX(readLines(
        "http://webrates.truefx.com/rates/connect.html"), pretty=pretty))
    } else return(readLines("http://webrates.truefx.com/rates/connect.html")) 
  }
  if (!inherits(id, "TFXid")) {
    stop("id is not a TFXid object created by ConnectTrueFX")
    # or should it warn and
    # return(readLines("http://webrates.truefx.com/rates/connect.html"))
  }
  if (id == "not authorized") stop("not authorized")
  ## request
  if (isTRUE(parse.response)) {
    return(ParseTrueFX(readLines(paste0(
      "http://webrates.truefx.com/rates/connect.html?id=", id)), pretty=pretty))
  }
  readLines(paste0("http://webrates.truefx.com/rates/connect.html?id=", id))
  # The next line would disconnect
  #readLines(paste0("http://webrates.truefx.com/rates/connect.html?di=", id))
}


#' Parse TrueFX response
#' 
#' Parse the results of a TrueFX query.  
#' 
#' This function will parse the results of a call to \code{\link{QueryTrueFX}}. 
#' It can handle any of the three TrueFX response formats: \dQuote{default}, 
#' \dQuote{csv}, or \dQuote{html}.  By default, it will convert the results 
#' into a nicely formatted \code{data.frame}.  If, called with 
#' \code{pretty=FALSE}, a list of strings will be returned.
#' 
#' All times are in GMT
#' 
#' @param x The response from a TrueFX request.  Can be any of the three
#'   formats: \code{default}, \code{csv} or \code{html}
#' @param pretty logical. If \code{TRUE} (Default), output will be converted to 
#'   a data.frame and columns will be converted from character to the 
#'   appropriate classes and combined.
#' @return By default, a \code{data.frame} is returned that has columns 
#'   \dQuote{Bid.Price}, \dQuote{Ask.Price}, \dQuote{High}, \dQuote{Low},
#'   and \dQuote{TimeStamp}.  If called with \code{pretty=FALSE}, a list of 
#'   character vectors -- named \dQuote{Symbol}, \dQuote{BidBigNumber}, 
#'   \dQuote{BidPip}, \dQuote{OfferBigNumber}, \dQuote{OfferPip}, 
#'   \dQuote{High}, \dQuote{Low}, \dQuote{TimeStamp} -- will be returned.
#'   
#'   If the format is \dQuote{html}, there will also be an \dQuote{Open} column
#' @author Garrett See
#' @references 
#' \url{http://www.truefx.com/dev/data/TrueFX_MarketDataWebAPI_DeveloperGuide.pdf}
#' @seealso \code{\link{QueryTrueFX}}, \code{\link{TrueFXRef}}
#' @note Although the TrueFX Market Data Web API Developer Guide indicates that
#'   both the \dQuote{csv} and \dQuote{html} formats include values for 
#'   \dQuote{Open}, only the \dQuote{html} format actually does.
#' @examples 
#' # x <- QueryTrueFX()  #Cannot run this if no internet connection
#' x <- paste0("EUR/USDUSD/JPY1.31#81.9085661.31#81.9435941.31990#81.6421.3182",
#'             "1#81.50413351311514701335131150004")
#' ParseTrueFX(x)
#' ParseTrueFX(x, pretty=FALSE)
#' @importFrom XML readHTMLTable
#' @export
ParseTrueFX <- function(x, pretty=TRUE) {
  PasteFigurePip <- function(figure, pip) {
    out <- gsub("\\.", "", paste0(sprintf("%04s", as.numeric(figure)), 
                                  sprintf("%03s", as.numeric(pip))))
      tmp <- sprintf("%04s", as.numeric(figure))
      loc <- -grep("\\.", tmp)
      # if it doesn't have a dot, add one at the end
      tmp[loc] <- paste0(tmp[loc], ".")
    as.numeric(paste0(tmp, sprintf("%03s", as.numeric(pip))))
  }
  
  if (grepl(",", x)) {  # It's in csv format
    if (!isTRUE(pretty)) {
      return(as.list(read.csv(text=x, header=FALSE, stringsAsFactors=FALSE, 
                              col.names = c("Symbol", "TimeStamp", 
                                            "BidBigNumber", 
                                            "BidPip", "OfferBigNumber", 
                                            "OfferPip", "High", "Low", "Open"),
                              colClasses = 'character')))
    } else {
      tmp <- read.csv(text=x, header=FALSE, stringsAsFactors=FALSE, 
                      col.names = c("Symbol", "TimeStamp", "BidBigNumber", 
                                    "BidPip", "OfferBigNumber", "OfferPip", 
                                    "High", "Low", "Open"))
      return(data.frame(Symbol = tmp[["Symbol"]],
        Bid.Price = PasteFigurePip(tmp[["BidBigNumber"]], tmp[["BidPip"]]),
        Ask.Price = PasteFigurePip(tmp[["OfferBigNumber"]], tmp[["OfferPip"]]),
        High = tmp[["High"]],
        Low = tmp[["Low"]],
        TimeStamp = as.POSIXct(as.numeric(tmp[["TimeStamp"]]) / 1000, 
                               origin='1970-01-01', tz='GMT'), 
        stringsAsFactors=FALSE))
    }
  } else if (substr(x, 1, 7) == "<table>") { # It's an HTML table
    out <- readHTMLTable(x, as.data.frame=FALSE)[[1]]
    names(out) <- c("Symbol", "TimeStamp", "BidBigNumber", "BidPip", 
                    "OfferBigNumber", "OfferPip", "High", "Low", "Open")
    if (!isTRUE(pretty)) {
      return(out)
    } else {
      return(data.frame(Symbol=out[["Symbol"]],
                        Bid.Price=as.numeric(paste0(out[["BidBigNumber"]],
                                                    out[["BidPip"]])),
                        Ask.Price=as.numeric(paste0(out[["OfferBigNumber"]],
                                                    out[["OfferPip"]])),
                        High=as.numeric(out[["High"]]),
                        Low=as.numeric(out[["Low"]]),
                        Open=as.numeric(out[["Open"]]),
                        TimeStamp=as.POSIXct(as.numeric(out$TimeStamp) / 1000, 
                                             origin='1970-01-01', tz='GMT'),
                        stringsAsFactors=FALSE))
    }
  }
  # Otherwise, it's a concatenated string
  npairs <- nchar(gsub("[0-9.#]", "", x)) / 7
  .ReadSection <- function(string, by) {
    end <- by * npairs
    if (end > 0) substring(string, seq(1, end, by), seq(by, end, by))
  }
  # See page 3 of
  #http://www.truefx.com/dev/data/TrueFX_MarketDataWebAPI_DeveloperGuide.pdf
  Ns <- c(7, 4, 3, 4, 3, 7, 7, 13)
  ep <- c(0, cumsum(npairs * Ns))
  out <- lapply(1:(length(ep) - 1), function(i) {
    beg <- (ep[i] + 1)
    end <- ep[i + 1]
    .string <- substr(x, beg, end)
    .ReadSection(.string, Ns[i])
  })
  names(out) <- c("Symbol", "BidBigNumber", "BidPip", "OfferBigNumber", 
                  "OfferPip", "High", "Low", "TimeStamp")
  if (!isTRUE(pretty)) {
    return(out)
  }
  out <- lapply(out, gsub, pattern="#", replacement=0)  
  data.frame(Symbol=out[["Symbol"]],
             Bid.Price=as.numeric(paste0(out[["BidBigNumber"]],
                                         out[["BidPip"]])),
             Ask.Price=as.numeric(paste0(out[["OfferBigNumber"]],
                                         out[["OfferPip"]])),
             High=as.numeric(out[["High"]]),
             Low=as.numeric(out[["Low"]]),
             TimeStamp=as.POSIXct(as.numeric(out$TimeStamp) / 1000, 
                                  origin='1970-01-01', tz='GMT'),
             stringsAsFactors=FALSE)
}


