################################################################################
##
## $Id: sample.matrix.test.R 374 2006-10-04 13:33:28Z enos $
##
## Tests that .sample.matrix correctly samples from a matrix of probabilities
##
################################################################################

library(portfolio)

## tests that the match.matrix matches the values with the highest
## probabilities

## sets all the values of i to a low probability

probs <- matrix(0, nrow = 5,
            ncol = 10,
            dimnames = list(1:5, letters[1:10])
            )

## sets letter "a" to have a high probability of being chosen

probs[,1] <- 0.9999

set.seed(1)
test <- portfolio:::.sample.matrix(probs, 100)

truth <- matrix("a", nrow = 5,
            ncol = 100,
            dimnames = list(1:5, 1:100)
            )

stopifnot(
          all.equal(test, truth)
          )

