\name{contributionHistory-class}
\docType{class}
\alias{contributionHistory-class}
\alias{mean,contributionHistory-method}
\alias{plot,contributionHistory,missing-method}
\alias{show,contributionHistory-method}
\alias{summary,contributionHistory-method}

\title{Class "contributionHistory"}
\description{A time series of objects of class "contribution".}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("contributionHistory", ...)}.
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
    \item{mean}{\code{signature(x = "contributionHistory")}: calculate the mean
      of the contributions in this object. }
    \item{plot}{\code{signature(x = "contributionHistory", y = "missing")}: Plot
      this object. }
    \item{show}{\code{signature(object = "contributionHistory")}: show the
      object, briefly. }
    \item{summary}{\code{signature(object = "contributionHistory")}:
      display a summary of this object. }
  }
}
\author{ Jeff Enos \email{jeff@kanecap.com}}
\keyword{classes}
