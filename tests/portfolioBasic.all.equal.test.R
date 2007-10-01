################################################################################
##
## $Id: portfolioBasic.all.equal.test.R 1128 2007-08-09 18:16:56Z enos $
##
## Tests "all.equal" method of "portfolioBasic"
##
################################################################################

library(portfolio)

## save(current, target, file = "portfolioBasic.all.equal.test.RData", compress = TRUE)

## loads 2 portfolioBasic objects identical in all regards except
## their weights slots have been ordered differently

load("portfolioBasic.all.equal.test.RData")

stopifnot(
          isTRUE(all.equal(current, target))
          )
