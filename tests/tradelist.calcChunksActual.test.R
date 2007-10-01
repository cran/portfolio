################################################################################
##
## $Id: tradelist.calcChunksActual.test.R 1128 2007-08-09 18:16:56Z enos $
##
## Tests "calcChunksActual" method of "tradelist" class
##
################################################################################

library(portfolio)

load("tradelist.calcChunksActual.test.RData")

## save(tl, tl.1,  truth.chunks.actual, truth.row.names, file = "tradelist.calcChunksActual.test.RData", compress = TRUE)

tl <- portfolio:::calcChunksActual(tl)

tl.1 <- portfolio:::calcChunksActual(tl.1)
tl.1@chunks.actual <- tl.1@chunks.actual[order(row.names(tl.1@chunks.actual)),]

stopifnot(all.equal(tl@chunks.actual, truth.chunks.actual))
stopifnot(all.equal(row.names(tl.1@chunks.actual), truth.row.names))

## truth.chunks.actual <- tl@chunks.actual


