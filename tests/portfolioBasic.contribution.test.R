################################################################################
##
## $Id: portfolioBasic.contribution.test.R 366 2006-10-03 15:04:46Z enos $
##
## Tests for the portfolioBasic class.
##
################################################################################

library(portfolio)

load("portfolioBasic.contribution.test.RData")

## save(contrib.1, data, x, file = "portfolioBasic.contribution.test.RData", compress = TRUE)

trial.0 <- try(
               contribution(x, contrib.var = "foo"), silent = TRUE
               )

if(class(trial.0) == "try-error"){
  stopifnot(as.logical(grep("Error.*contrib\.var.\%in\%.names",trial.0[1])))
}

## stopifnot(
##           all.equal(sum(x@weights$weight), 0),
##           all.equal(sum(x@weights$weight[x@weights$weight > 0]), 1)
##           )
 
## stopifnot(
##           all.equal(contrib.1,
##                     contribution(x, contrib.var = c("by.var.1","by.var.2")))
##           )
