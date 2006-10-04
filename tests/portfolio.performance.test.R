################################################################################
##
## $Id: portfolio.performance.test.R 374 2006-10-04 13:33:28Z enos $
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
