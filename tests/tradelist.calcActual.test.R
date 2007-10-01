################################################################################
##
## $Id: tradelist.calcActual.test.R 1128 2007-08-09 18:16:56Z enos $
##
## Tests "calcActual" method of "tradelist" class
##
################################################################################

library(portfolio)

load("tradelist.calcActual.test.RData")

## save(tl, truth.actual, file = "tradelist.calcActual.test.RData", compress = TRUE)

tl <- portfolio:::calcActual(tl)

stopifnot(all.equal(tl@actual, truth.actual))

## truth.actual <- tl@actual


