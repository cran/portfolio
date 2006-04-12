################################################################################
##
## $Id: $
##
## Tests "+" method of "portfolioBasic"
##
################################################################################

library(portfolio)

## Constructs data and portfolioBasics that should add ("+") correctly

data.0 <- data.frame(id = 1:20, symbol.var = 1:20,
                     in.var = 1:20, ret.var = 1:20)
data.1 <- data.frame(id = 10:30, symbol.var = 10:30,
                     in.var = 10:30, ret.var = 10:30)

p.0 <- new("portfolioBasic", id.var = "id", symbol.var = "symbol.var",
              in.var = "in.var", ret.var = "ret.var", type = "equal",
              size = "quintile", data = data.0)

p.1 <- new("portfolioBasic", id.var = "id", symbol.var = "symbol.var",
              in.var = "in.var", ret.var = "ret.var", type = "equal",
              size = "quintile", data = data.1)

p.sum.0 <- p.0 + p.1

## Need to add test here!
