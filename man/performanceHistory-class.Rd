\name{performanceHistory-class}
\docType{class}
\alias{performanceHistory-class}
\alias{plot,performanceHistory,missing-method}
\alias{summary,performanceHistory-method}

\title{Class "performanceHistory"}
\description{A time series of objects of class "performance"}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("performanceHistory", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{freq}:}{Object of class \code{"numeric"} containing the
    frequency of the time series of added objects, as periods per year.
    Defaults to 1. }
    \item{\code{data}:}{Object of class \code{"list"} containing list of
    objects in the history.  The names of this list are the objects' time periods. }
  }
}
\section{Extends}{
Class \code{"objectHistory"}, directly.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "performanceHistory", y = "missing")}: Plot
      this object. }
    \item{summary}{\code{signature(object = "performanceHistory")}:
      display of summary of this time series of performance objects. }
  }
}
\author{ Jeff Enos \email{jeff@kanecap.com}}
\keyword{classes}
