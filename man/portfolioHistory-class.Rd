\name{portfolioHistory-class}
\docType{class}
\alias{portfolioHistory-class}
\alias{add,portfolioHistory,portfolioBasic-method}
\alias{contribution,portfolioHistory-method}
\alias{exposure,portfolioHistory-method}
\alias{performance,portfolioHistory-method}
\alias{show,portfolioHistory-method}
\alias{summary,portfolioHistory-method}

\alias{add}

\title{Class "portfolioHistory" }
\description{ Perform exposure, performance, and contribution analysis
  on all of the \code{portfolioBasic} objects that have been added to
  this object with the \code{add} method. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("portfolioHistory", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} containing
      the name of this object.}
    \item{\code{freq}:}{Object of class \code{"numeric"} containing the
      frequency to be used in annualization calculations, as periods per
      year.  Defaults to 1. }
    \item{\code{exp.var}:}{Object of class \code{"character"} containing
      the vector of exposure variables to compute. }
    \item{\code{exposures}:}{Object of class \code{"list"} containing a
      list of exposure objects, one for each added portfolio. }
    \item{\code{ret}:}{Object of class \code{"list"} ~~ }
    \item{\code{ret.detail}:}{Object of class \code{"list"} ~~ }
    \item{\code{contrib.var}:}{Object of class \code{"character"}
      containing the vector of contribution variables to compute. }
    \item{\code{contribution}:}{Object of class \code{"list"} containing
      a list of contribution objects, one for each added portfolio.}
  }
}
\section{Methods}{
  \describe{
    \item{add}{\code{signature(object = "portfolioHistory", x =
	"portfolioBasic")}: add portfolio \code{x} to history object
      \code{object}.  The history object collects and stores exposure,
      contribution, etc. information from portfolio \code{x} upon add. }
    \item{contribution}{\code{signature(object = "portfolioHistory")}:
      display historical contribution. }
    \item{exposure}{\code{signature(object = "portfolioHistory")}:
      display historical exposure. }
    \item{performance}{\code{signature(object = "portfolioHistory")}:
      display historical performace information. }
    \item{show}{\code{signature(object = "portfolioHistory")}: show
      this object, briefly.}
    \item{summary}{\code{signature(object = "portfolioHistory")}: show
      descriptive information about this \code{portfolioHistory} object.}
  }
}
\author{ Jeff Enos \email{jeff@kanecap.com}}
\examples{
data(global.2004)

portfolios <- lapply(split(global.2004, global.2004$date),
                     function(x){
                       new("portfolioBasic",
                           name    = "test",
                           date    = as.Date(unique(x$date)),
                           id.var  = "symbol",
                           in.var  = "cap.bil",
                           ret.var = "ret.0.1.m",
                           type    = "sigmoid",
                           size    = "quintile",
                           sides   = c("long", "short"),
                           data    = x)

                     })

h    <- new("portfolioHistory",
            freq        = 12,
            exp.var     = c("currency", "sector", "liq.w", "cap.bil"),
            contrib.var = c("currency", "sector", "liq.w"))


for(i in 1:length(portfolios)) h <- add(h, portfolios[[i]])

exposure(h)
performance(h)
contribution(h)

}
\keyword{classes}
