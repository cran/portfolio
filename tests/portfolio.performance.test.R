################################################################################
##
## $Id: portfolio.performance.test.R 383 2007-01-08 22:26:03Z enos $
##
## Tests the performance method of class portfolio.
##
################################################################################

library(portfolio)

load("portfolio.performance.test.RData")

## save(test.portfolio, empty.portfolio, test.market.data, truth, empty.truth, file = "portfolio.performance.test.RData", compress = TRUE)

result <- performance(test.portfolio, test.market.data)

empty.result <- performance(empty.portfolio, test.market.data)

## Set row.names explicitly to a character vector, since the default
## is different in R 2.5.0 than in 2.4.x and we want the test to pass
## in both.

row.names(empty.result@ret.detail) <- as.character(row.names(empty.result@ret.detail))

stopifnot(
          all.equal(result, truth),
          all.equal(empty.result, empty.truth)
          )
