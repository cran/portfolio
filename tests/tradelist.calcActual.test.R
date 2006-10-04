################################################################################
##
## $Id: tradelist.calcActual.test.R 374 2006-10-04 13:33:28Z enos $
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


