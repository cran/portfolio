\name{assay}
\alias{assay}
\docType{data}
\title{Assay Research rankings as of 2004-12-31}
\description{
  A universe of the 5000 largest global stocks as of 2004-12-31, and a
  flag indicating whether a security was ranked by Assay Research at
  that time.

  Special thanks to Assay Research for granting us permission to release
  this data.
}
\usage{data(assay)}
\format{
  A data frame with 5000 observations on the following 11 variables.
  \describe{
    \item{\code{id}}{a character vector}
    \item{\code{symbol}}{a character vector}
    \item{\code{name}}{a character vector}
    \item{\code{country}}{a factor with levels \code{AUS} \code{AUT} \code{BEL} \code{CHE} \code{DEU} \code{DNK} \code{ESP} \code{FIN} \code{FRA} \code{GBR} \code{HKG} \code{ITA} \code{JPN} \code{NLD} \code{NOR} \code{NZL} \code{SGP} \code{SWE} \code{USA}}
    \item{\code{currency}}{a factor with levels \code{AUD} \code{CHF} \code{DKK} \code{EUR} \code{GBP} \code{HKD} \code{JPY} \code{NOK} \code{NZD} \code{SEK} \code{SGD} \code{USD}}
    \item{\code{price}}{a numeric vector}
    \item{\code{sector}}{a factor with levels \code{Communications} \code{Conglomerates} \code{Cyclicals} \code{Energy} \code{Financials} \code{Industrials} \code{Materials} \code{Staples} \code{Technology} \code{Utilities}}
    \item{\code{liquidity}}{a numeric vector}
    \item{\code{assay}}{a logical vector indicating presence on the
      Assay Focus List as of 2004-12-31.}
    \item{\code{ret.0.3.m}}{a numeric vector}
    \item{\code{ret.0.6.m}}{a numeric vector}
  }
}
\examples{
data(assay)
}
\keyword{datasets}
