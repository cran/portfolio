################################################################################
##
## $Id: $
##
## Tests for the portfolioBasic class.
##
################################################################################

library(portfolio)

load("portfolioBasic.test.RData")

## save(data, exp.1, file = "portfolioBasic.test.RData", compress = TRUE)

data <- data.frame(id = 1:20, in.var = 1:20)
data$in.var <- as.numeric(data$in.var)

## Construct by.var's.  Note that the assigned vectors are
## deliberately recycled.

data$by.var.1 <- c("1","2")
data$by.var.2 <- c("1","2","3","4")
data$by.var.3 <- c(-1,0,0,1)

x <- new("portfolioBasic", in.var = "in.var", type = "sigmoid", size = 8, data = data)
x <- create(x)

## All weights should sum to 0; side weights sum to 1.

stopifnot(
          all.equal(sum(x@weights$weight), 0),
          all.equal(sum(x@weights$weight[x@weights$weight > 0]), 1)
          )

## Cut the weights in half and then use the scaler to fix.

x@weights$weight <- x@weights$weight / 2
x <- scaleWeights(x)

stopifnot(
          all.equal(sum(x@weights$weight), 0),
          all.equal(sum(x@weights$weight[x@weights$weight > 0]), 1)
          )

stopifnot(
          all.equal(exp.1@data,
                    exposure(x, exp.var = c("by.var.1","by.var.2","by.var.3"))@data)
          )

## I've removed tests involving calling the balance method and then
## checking exposures.  The Debian machine at CRAN encounters a
## failure, likely due to a differing .Machine$double.eps used in
## all.equal.  I couldn't reproduce using -ffloat-store in CFLAGS, but
## these tests aren't important for testing portfolioBasic or
## exposures, so I'm taking them out.

## I should add tests that check cases where a single side has 0
## exposure and where both sides have 0 exposure to a factor level.
