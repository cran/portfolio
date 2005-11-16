\name{exposureHistory-class}
\docType{class}
\alias{exposureHistory-class}
\alias{mean,exposureHistory-method}
\alias{show,exposureHistory-method}

\title{Class "exposureHistory"}
\description{ A time series of objects of class "exposure". }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("exposureHistory", ...)}.
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
    \item{mean}{\code{signature(x = "exposureHistory")}: calculate the mean
      of the exposures in this object. }
    \item{show}{\code{signature(object = "exposureHistory")}: show the
      object, briefly. }
  }
}
\author{ Jeff Enos \email{jeff@kanecap.com}}
\keyword{classes}
