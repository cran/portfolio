################################################################################
##
## $Id: $
##
## Tests for adding portolioBasics
##
################################################################################

library(portfolio)

## constructs data and portfolioBasics that should sum correctly

data.0 <- data.frame(id = 1:20, symbol.var = 1:20, in.var = 1:20,
                     ret.var = 1:20, price.var = 1:20)
                     

data.1 <- data.frame(id = 10:29, symbol.var = 10:29, in.var = 10:29,
                     ret.var = 10:29, price.var = 10:29)

p.0 <- new("portfolio", id.var = "id", symbol.var = "symbol.var",
                   in.var = "in.var", ret.var = "ret.var", type = "equal",
                   size = "quintile", equity = 100000,
                   price.var = "price.var", data = data.0)

p.1 <- new("portfolio", id.var = "id", symbol.var = "symbol.var",
                   in.var = "in.var", ret.var = "ret.var", type = "equal",
                   size = "quintile", equity = 100000,
                   price.var = "price.var", data = data.1)

p.sum.0 <- p.0 + p.1

## Need to add test here!
