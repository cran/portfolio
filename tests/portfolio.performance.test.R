################################################################################
##
## $Id: portfolio.performance.test.R 346 2006-10-01 05:08:55Z enos $
##
## Tests the performance method of class portfolio.
##
################################################################################

library(portfolio)

load("portfolio.performance.test.RData")

## save(test.portfolio, empty.portfolio, test.market.data, truth, empty.truth, file = "portfolio.performance.test.RData", compress = TRUE)

result <- performance(test.portfolio, test.market.data)

empty.result <- performance(empty.portfolio, test.market.data)


stopifnot(
          all.equal(result, truth),
          all.equal(empty.result, empty.truth)
          )
