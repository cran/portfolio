################################################################################
##
## $Id: portfolio.updatePrices.test.R 374 2006-10-04 13:33:28Z enos $
##
## Tests the updatePrices method of class portfolio.
##
################################################################################

library(portfolio)

load("portfolio.updatePrices.test.RData")

## save(test, test.id, test.price, truth, file = "portfolio.updatePrices.test.RData", compress = TRUE)

result <- updatePrices(test, test.id, test.price)

stopifnot(
          all.equal(truth, result@data[[result@price.var]])
          )
