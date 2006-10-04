################################################################################
##
## $Id: tradelist.calcChunks.test.R 374 2006-10-04 13:33:28Z enos $
##
## Tests "calcChunks" method of "tradelist" class
##
################################################################################

library(portfolio)

load("tradelist.calcChunks.test.RData")

## save(tl, tl.1, truth.chunks, truth.tca, file = "tradelist.calcChunks.test.RData", compress = TRUE)

tl <- portfolio:::calcChunks(tl)

stopifnot(all.equal(tl@chunks, truth.chunks))

## truth.chunks <- tl@chunks

## tests trade-cost adjustment using .tca.volume function

tl.1 <- portfolio:::calcChunks(tl.1)
tl.1@chunks <- tl.1@chunks[order(row.names(tl.1@chunks)),]

stopifnot(all.equal(tl.1@chunks$tca.rank, truth.tca))
