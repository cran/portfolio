################################################################################
##
## $Id: portfolio.Arith.test.R 366 2006-10-03 15:04:46Z enos $
##
## Tests for adding portolios
##
################################################################################

library(portfolio)

load("portfolio.Arith.test.RData")

## save(p.0, p.1, truth, file = "portfolio.Arith.test.RData", compress = TRUE)

## constructs data and portfolioBasics that should sum correctly

## data.0 <- data.frame(id = 1:20, symbol.var = 1:20, in.var = 1:20,
##                      ret.var = 1:20, price.var = 1:20)

## data.1 <- data.frame(id = 10:29, symbol.var = 10:29, in.var = 10:29,
##                      ret.var = 10:29, price.var = 10:29)

## ## Creates the portfolios

## p.0 <- new("portfolio", id.var = "id", symbol.var = "symbol.var",
##                    in.var = "in.var", ret.var = "ret.var", type = "equal",
##                    size = "quintile", equity = 100000, 
##                    price.var = "price.var", data = data.0)

## p.1 <- new("portfolio", id.var = "id", symbol.var = "symbol.var",
##                    in.var = "in.var", ret.var = "ret.var", type = "equal",
##                    size = "quintile", equity = 100000, 
##                    price.var = "price.var", data = data.1)

## ## Sets the shares slots

## p.0@shares <- data.frame(id = as.character(p.0@data$id), shares = as.numeric(1:20))

## p.1@shares <- data.frame(id = as.character(p.1@data$id), shares = as.numeric(10:29))

## p.0 <- calcWeights(p.0)
## p.1 <- calcWeights(p.1)

## Sums and tests the equality of the portfolios

p.sum <- p.0 + p.1

stopifnot(
          isTRUE(all.equal(p.sum, truth)),
          nrow(p.sum@data) == length(union(p.0@data$id, p.1@data$id))
          )
