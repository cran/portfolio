################################################################################
##
## $Id: portfolio.matching.test.R 346 2006-10-01 05:08:55Z enos $
##
## 
##
################################################################################

library(portfolio)

load("portfolio.matching.test.RData")
## save(p, truth, file = "portfolio.matching.test.RData", compress = TRUE)

p.m <- matching(p, covariates = c("country", "sector", "liquidity"))

test <- p.m@matches[p.m@matches[,1] < 0,]
test <- test[order(names(test))]

stopifnot(
          validObject(p.m),
          all.equal(truth, test)
          )

## basic test of "sample" method

p.m <- matching(p, covariates = c("sector", "liquidity"), method = "sample",
                n.matches = 5)


stopifnot(
          all.equal(dim(p.m@matches), c(4000, 5))
          )
          
################################################################################
## Subroutine tests 
################################################################################

## .matching.prep

test <- portfolio:::.matching.prep(data = p@data, weights = p@weights,
                       covariates = c("sector", "liquidity"))

stopifnot(
          all(test[test$treatment, "id"] %in% p@weights$id)
          )

## .matching.scaled.weights

id.map <- matrix(nrow = 31,
                 ncol = 1,
                 dimnames = list(p@weights$id[-(1:2)], 1)
                 )


test <- portfolio:::.matching.scale.weights(weights = p@weights, id.map = id.map)

stopifnot(
          all.equal(test$weight, rep(-0.032, length(test$weight)),
                    tolerance = 0.01)
          )

## tests the "calc.scaling.factor.R" function

orig.weights <- rep(c(0.2, -0.2), length.out = 10)
matched.weights <- rep(c(0.1, -0.1), length.out = 10)

test  <- portfolio:::.calc.scaling.factor(orig.weights, matched.weights)
truth <- c(2,2)
names(truth) <- c("-1", "1")

stopifnot(
          all.equal(test, truth)
          )

## tests the ".scale.weights" function

## long-only portfolio

scaling.factors <- 5
names(scaling.factors) <-  "1"

x <- rep(0.04, length.out = 5)

scaled.x <- portfolio:::.scale.weights(x, scaling.factors)

stopifnot(
          all.equal(sum(scaled.x), 1)
          )

## short-only portfolio

scaling.factors <- 5
names(scaling.factors) <-  "-1"

x <- rep(-0.04, length.out = 5)

scaled.x <- portfolio:::.scale.weights(x, scaling.factors)

stopifnot(
          all.equal(sum(scaled.x), -1)
          )

## long-short portfolio

scaling.factors <- c(5,5)
names(scaling.factors) <- c("-1", "1")

x <- rep(c(-0.04, 0.04), length.out = 10)

scaled.x <- portfolio:::.scale.weights(x, scaling.factors)

stopifnot(
          all.equal(sum(scaled.x), 0)
          )

## corner case where original portfolio is long-short and matched
## portfolio is long or short only

scaling.factors <- c(2,2)
names(scaling.factors) <- c("-1", "1")

x <- rep(0.1, length.out = 5)

scaled.x <- portfolio:::.scale.weights(x, scaling.factors)

stopifnot(
          all.equal(sum(scaled.x), 1)
          )
