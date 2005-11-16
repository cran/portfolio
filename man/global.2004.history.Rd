\name{global.2004.history}
\alias{global.2004.history}
\docType{data}
\title{portfolioHistory object formed from the global.2004 dataset}
\description{
Contains an object of class \code{portfolioHistory} that contains
exposure, performance, and contribution data for twelve portfolios
formed from the twelve time periods in the \code{global.2004} dataset.

}
\usage{data(global.2004.history)}
\format{
Formal class 'portfolioHistory' [package "portfolio"] with 7 slots.
}
\details{

  The portfolios added to this \code{portfolioHistory} object were long
  short, of type sigmoid, and were
  formed using the top and bottom quintiles of the \code{cap.bil}
  variable.

}
\examples{
data(global.2004.history)
}
\keyword{datasets}
