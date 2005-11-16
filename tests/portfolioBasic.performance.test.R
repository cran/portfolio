################################################################################
##
## $Id: $
##
## Tests for performance calculation on the portfolioBasic class.
##
################################################################################

library(portfolio)

load("portfolioBasic.performance.test.RData")

## save(perf.1, perf.2, perf.3, file = "portfolioBasic.performance.test.RData")

data <- data.frame(id = 1:20, in.var = 1:20)
data$in.var <- as.numeric(data$in.var)

p <- new("portfolioBasic", in.var = "in.var", ret.var = "ret.var",
         type = "equal",
         size = 10, data = data)

p <- create(p)


## Calculate performance where every security loses 10%.

x <- data.frame(id = 1:20, ret.var = rep(-0.10, 20))
p@data$ret.var <- x$ret.var[match(p@data$id, x$id)]

perf.1.test <- performance(p)

stopifnot(
          all.equal(perf.1.test@ret, perf.1@ret),
          all.equal(perf.1.test@ret.detail, perf.1@ret.detail),
          all.equal(perf.1.test@t.plus.one@weights, perf.1@t.plus.one@weights)
          )

## NA returns are allowed, but won't contribute to the total return.
## Weights in the t+1 portfolio for stocks with NA return are
## unchanged.

is.na(x$ret.var) <- 9:12
p@data$ret.var <- x$ret.var[match(p@data$id, x$id)]

perf.2.test <- performance(p)
       
stopifnot(
          all.equal(perf.2.test@ret, perf.2@ret),
          all.equal(perf.2.test@ret.detail, perf.2@ret.detail),
          all.equal(perf.2.test@t.plus.one@weights, perf.2@t.plus.one@weights)
          )

## Now make the returns non-equal and non-symmetric.

x$ret.var <- x$ret.var / x$id
x$ret.var <- x$ret.var * c(1,-1)
p@data$ret.var <- x$ret.var[match(p@data$id, x$id)]

perf.3.test <- performance(p)

stopifnot(
          all.equal(perf.3.test@ret, perf.3@ret),
          all.equal(perf.3.test@ret.detail, perf.3@ret.detail),
          all.equal(perf.3.test@t.plus.one@weights, perf.3@t.plus.one@weights)
          )
