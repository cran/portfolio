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

exp.1.test <- exposure(x, exp.var = c("by.var.1","by.var.2","by.var.3"))

## Function to test equality of data frame components of the exposure
## object's data slot.  Different default handling of row.names in
## 2.4.0 makes it necessary to compare column-by-column.

exp.df.equal <- function(e1, e2) {
  return(all.equal(e1$variable, e2$variable) &&
         all.equal(e1$long,     e2$long) &&
         all.equal(e1$short,    e2$short) &&
         all.equal(e1$exposure, e2$exposure))
}

stopifnot(all.equal(names(exp.1@data), names(exp.1.test@data)),
          exp.df.equal(exp.1@data$numeric,  exp.1.test@data$numeric),
          exp.df.equal(exp.1@data$by.var.1, exp.1.test@data$by.var.1),
          exp.df.equal(exp.1@data$by.var.2, exp.1.test@data$by.var.2))

## I've removed tests involving calling the balance method and then
## checking exposures.  The Debian machine at CRAN encounters a
## failure, likely due to a differing .Machine$double.eps used in
## all.equal.  I couldn't reproduce using -ffloat-store in CFLAGS, but
## these tests aren't important for testing portfolioBasic or
## exposures, so I'm taking them out.

## I should add tests that check cases where a single side has 0
## exposure and where both sides have 0 exposure to a factor level.
