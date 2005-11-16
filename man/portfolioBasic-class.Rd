\name{portfolioBasic-class}
\docType{class}
\alias{portfolioBasic-class}
\alias{balance,portfolioBasic,character-method}
\alias{contribution,portfolioBasic,character-method}
\alias{create,portfolioBasic-method}
\alias{exposure,portfolioBasic,character-method}
\alias{performance,portfolioBasic-method}
\alias{portfolioDiff,portfolioBasic,portfolioBasic-method}
\alias{scaleWeights,portfolioBasic-method}
\alias{show,portfolioBasic-method}
\alias{summary,portfolioBasic-method}
\alias{initialize,portfolioBasic-method}

\alias{balance}
\alias{contribution}
\alias{create}
\alias{exposure}
\alias{performance}
\alias{portfolioDiff}
\alias{scaleWeights}

\title{Class "portfolioBasic"}
\description{An object of the lightweight class "portfolioBasic" contains a data frame
  of weights and a data frame of supplementary information.}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("portfolioBasic", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} containing
      the name of this portfolio. }
    \item{\code{date}:}{Object of class \code{"Date"} containing a date
      for this portfolio. }
    \item{\code{data}:}{Object of class \code{"data.frame"} containing
      supplementary information about the positions in this portfolio.
      Must include a unique column specified in the \code{id.var} slot. }
    \item{\code{id.var}:}{Object of class \code{"character"}
      containing the name of the column in the \code{data} slot to be used as
      a unique identifier. }
    \item{\code{symbol.var}:}{Object of class \code{"character"}
      containing the name of the column in the \code{data} slot to be used as
      a descriptive symbol. }
    \item{\code{in.var}:}{Object of class \code{"character"} containing
      the name of the column in the \code{data} slot to be used as a rank
      vector in calls to \code{create}.}
    \item{\code{weight.var}:}{Object of class \code{"character"}
      containing the name of the column in the \code{data} slot to be used as
      weight overrides in calls to \code{create}. }
    \item{\code{ret.var}:}{Object of class \code{"character"} containing
    the name of the column in the \code{data} slot to be used as the return
    in calls to \code{performance}.}
    \item{\code{type}:}{Object of class \code{"character"} containing
      the type of weight formation to use in calls to \code{create}.
      May be one of "relative", "equal", "linear", "sigmoid",
      "centroid", or "complex".
      Defaults to \code{equal}. }
    \item{\code{size}:}{Object of class \code{"characterOrNumeric"}
      containing the size of the portfolio to use in calls to
      \code{create}.  May either contain the number of securities per
      side or one of "decile", "quintile", "quartile", "tercile", or
      "demile".  Defaults to \code{quintile}. }
    \item{\code{weights}:}{Object of class \code{"data.frame"}
      containing the data frame of weights for this portfolio's
      positions.  Must contain a unique column called "id". }
  }
}
\section{Methods}{
  \describe{
    \item{balance}{\code{signature(object = "portfolioBasic", in.var = "character")}:
      balances the positions in portfolio \code{object} to be neutral to
    the categories specified by column \code{in.var} in the \code{data} slot.}
    \item{contribution}{\code{signature(object = "portfolioBasic",
	contrib.var = "character")}: returns one data.frame with
      contribution analysis for each element of \code{contrib.var}.  All
    results are returned in a list.}
    \item{create}{\code{signature(object = "portfolioBasic")}: use this
      object's creation parameters (such as in slots \code{size} and
      \code{type}) to create and return a new object of class
      \code{portfolioBasic}.}
    \item{exposure}{\code{signature(object = "portfolioBasic", exp.var
	= "character")}: returns one data.frame with
      exposure analysis for each element of \code{contrib.var}.  All
    results are returned in a list. }
    \item{performance}{\code{signature(object = "portfolioBasic")}:
    returns a list containing performance results. }
    \item{portfolioDiff}{\code{signature(object = "portfolioBasic", x =
	"portfolioBasic")}: computes the difference, as a
      \code{portfolioBasic} object, between two portfolios. }
    \item{scaleWeights}{\code{signature(object = "portfolioBasic")}:
      scale weights to the weights supplied in the \code{target}
      parameter.  To restrict the set of positions whose weights are
      scaled, use the \code{condition} argument. }
    \item{show}{\code{signature(object = "portfolioBasic")}: display
      this object, briefly. }
    \item{summary}{\code{signature(object = "portfolioBasic")}: display
      descriptive information about this portfolio. }
    \item{initialize}{\code{signature(object = "portfolioBasic")}:
      initialize the portfolio by calling \code{create}. }
  }
}
\author{ Jeff Enos \email{jeff@kanecap.com}}
\examples{
data(dow.jan.2005)

p <- new("portfolioBasic",
         id.var = "symbol",
         in.var = "price",
         sides = "long",
         ret.var = "month.ret",
         data = dow.jan.2005)

summary(p)

exposure(p, exp.var = c("price", "sector"))
summary(performance(p))
contribution(p, contrib.var = c("cap.bil", "sector"))

p <- new("portfolioBasic",
         id.var = "symbol",
         in.var = "price",
         type = "linear",
         sides = c("long", "short"),
         ret.var = "month.ret",
         data = dow.jan.2005)

summary(p)

exposure(p, exp.var = c("price", "sector"))
summary(performance(p))
contribution(p, contrib.var = c("cap.bil","sector"))
}
\keyword{classes}
