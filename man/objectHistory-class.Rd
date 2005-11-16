\name{objectHistory-class}
\docType{class}
\alias{objectHistory-class}
\alias{add,objectHistory,ANY-method}
\alias{length,objectHistory-method}
\alias{show,objectHistory-method}

\title{Class "objectHistory" }
\description{ An object containing a time series of other objects, as a
  list. }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("objectHistory", ...)}.
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
\section{Methods}{
  \describe{
    \item{add}{\code{signature(object = "objectHistory", x = "ANY")}:
      add an object of any class to the history object. }
    \item{length}{\code{signature(x = "objectHistory")}: return the
      number of periods. }
    \item{show}{\code{signature(object = "objectHistory")}: show the
      object, briefly.}
  }
}
\author{ Jeff Enos \email{jeff@kanecap.com}}
\examples{
x <- new("objectHistory")
x <- add(x, 1, "2005-01-01")
x <- add(x, 2, "2005-01-02")
length(x)
}
\keyword{classes}
