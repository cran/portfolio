################################################################################
##
## $Id: $
##
## Tests for the weight function.
##
################################################################################

library(portfolio)

load("weight.test.RData")

## save(x, file = "weight.test.Rdata")

## Some long + short weight formation tests:

stopifnot(
          all.equal(x$out.1.1, weight(x, in.var = "in.1", type = "equal", size = 2, sides = c("long","short"))),
          all.equal(x$out.1.2, weight(x, in.var = "in.1", type = "equal", size = 3, sides = c("long","short"))),
          all.equal(x$out.1.3, weight(x, in.var = "in.1", type = "equal", size = "quintile", sides = c("long","short"))),
          all.equal(x$out.1.4, weight(x, in.var = "in.1", type = "equal", size = "tercile", sides = c("long","short"))),

          all.equal(x$out.1.5, weight(x, in.var = "in.1", type = "linear",   size = "tercile", sides = c("long","short"))),
          all.equal(x$out.1.6, weight(x, in.var = "in.1", type = "sigmoid",  size = "demile", sides = c("long","short"))),
          all.equal(x$out.1.7, weight(x, in.var = "in.1", type = "centroid", size = "demile", sides = c("long","short")))
          )

## The above tests repeated, but for one side only.

## Short-only:

x.s <- x
is.na(x.s) <- !is.na(x.s) & x > 0

stopifnot(
          all.equal(x.s$out.1.1, weight(x, in.var = "in.1", type = "equal", size = 2, sides = c("short"))),
          all.equal(x.s$out.1.2, weight(x, in.var = "in.1", type = "equal", size = 3, sides = c("short"))),
          all.equal(x.s$out.1.3, weight(x, in.var = "in.1", type = "equal", size = "quintile", sides = c("short"))),
          all.equal(x.s$out.1.4, weight(x, in.var = "in.1", type = "equal", size = "tercile", sides = c("short"))),

          all.equal(x.s$out.1.5, weight(x, in.var = "in.1", type = "linear",   size = "tercile", sides = c("short"))),
          all.equal(x.s$out.1.6, weight(x, in.var = "in.1", type = "sigmoid",  size = "demile", sides = c("short"))),
          all.equal(x.s$out.1.7, weight(x, in.var = "in.1", type = "centroid", size = "demile", sides = c("short")))
          )

## Long-only:

x.l <- x
is.na(x.l) <- !is.na(x.l) & x < 0

stopifnot(
          all.equal(x.l$out.1.1, weight(x, in.var = "in.1", type = "equal", size = 2, sides = c("long"))),
          all.equal(x.l$out.1.2, weight(x, in.var = "in.1", type = "equal", size = 3, sides = c("long"))),
          all.equal(x.l$out.1.3, weight(x, in.var = "in.1", type = "equal", size = "quintile", sides = c("long"))),
          all.equal(x.l$out.1.4, weight(x, in.var = "in.1", type = "equal", size = "tercile", sides = c("long"))),

          all.equal(x.l$out.1.5, weight(x, in.var = "in.1", type = "linear",   size = "tercile", sides = c("long"))),
          all.equal(x.l$out.1.6, weight(x, in.var = "in.1", type = "sigmoid",  size = "demile", sides = c("long"))),
          all.equal(x.l$out.1.7, weight(x, in.var = "in.1", type = "centroid", size = "demile", sides = c("long")))
          )
