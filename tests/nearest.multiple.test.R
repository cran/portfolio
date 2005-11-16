################################################################################
##
## $Id: $
##
## Tests for nearest multiple calculations.
##
################################################################################

library(portfolio)

load("nearest.multiple.test.RData")

stopifnot(
          all.equal(x$result, portfolio:::.nearest.multiple(x$x, x$y))
          )


