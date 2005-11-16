################################################################################
##
## $Id: $
##
## Tests for nearest multiple calculations.
##
################################################################################

library(portfolio)

load("df.category.mean.test.RData")

stopifnot(
          all.equal(x$mean.1.2, portfolio:::.df.category.mean(x$x.1, x$x.2, by.var = "by.var")),
          all.equal(x$mean.1.3, portfolio:::.df.category.mean(x$x.1, x$x.3, by.var = "by.var")),
          all.equal(x$mean.1.2.3, portfolio:::.df.category.mean(x$x.1, x$x.2, x$x.3, by.var = "by.var"))
          )

