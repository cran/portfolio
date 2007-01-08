################################################################################
##
## $Id: portfolio.all.equal.test.R 366 2006-10-03 15:04:46Z enos $
##
## Tests "all.equal" method of "portfolio"
##
################################################################################

library(portfolio)

## save(current, target, file = "portfolio.all.equal.test.RData", compress = TRUE)

## loads 2 portfolio objects identical in all regards except
## their weights and shares slots have been ordered differently

load("portfolio.all.equal.test.RData")

stopifnot(
          isTRUE(all.equal(current, target))
          )
