################################################################################
##
## $Id: matchit.test.R 346 2006-10-01 05:08:55Z enos $
##
## Tests for the matching method of portfolioBasic
##
################################################################################

library(portfolio)

## save(truth, truth.2, truth.3, file = "matchit.test.RData", compress = TRUE)

load("matchit.test.RData")

data(assay)

x <- assay
x <- assay[assay$country == "USA", c("symbol", "name", "sector", "liquidity", "on.fl")]

## universe for test case includes all US stocks, 10 from the focus
## list, 10 identified as good matches by the matchit method, and 10
## other US stocks

all.stocks <- c("76143", "18027", "14730", "6961", "6930", "69571", "71262",
"21266", "7308", "11746", "27043", "37495", "74206", "79463", "2923", "8267",
"33105", "26322", "68150", "71570", "22101", "19167", "39252", "13776",
"83265", "71301", "7631", "29780", "3604", "28225")

x <- x[all.stocks,]

for(i in names(x)){
    if(identical(class(x[[i]]), "factor")){
      x[[i]] <- as.character(x[[i]])
    }
  }

## done preparing data, tests greedy algorithm of entire "matchit"
## function.

test <- portfolio:::.matchit(on.fl ~ sector + liquidity, data = x)

## truth is a matrix

stopifnot(
          all.equal(dimnames(test)[1], dimnames(truth)[1]),
          all(mapply(all.equal, test, truth))
          )

## Adds missing data

y <- x

y[c(1, nrow(y)), "liquidity"] <- NA

test.2 <- portfolio:::.matchit(on.fl ~ sector + liquidity, data = y)

stopifnot(
          all.equal(dimnames(test.2)[1], dimnames(truth.2)[1]),
          all(mapply(all.equal, test.2, truth.2))
          )

## corner case: number of controls is less than the number of treated.

y.sub <- y[1:15,]

test.3 <- portfolio:::.matchit(on.fl ~ sector + liquidity, data = y.sub)

stopifnot(
          all.equal(dimnames(test.3)[1], dimnames(truth.3)[1]),
          all.equal(test.3[1,], truth.3[1,])
          )

## corner case: number of controls is 0

y.sub <- y[1:10,]

test <- portfolio:::.matchit(on.fl ~ sector + liquidity, data = y.sub)

stopifnot(
          all(is.na(test[1,]))
          )

################################################################################
## Sample Matching
################################################################################

## corner case: number of controls is 0

y.sub <- y[1:10,]

test <- portfolio:::.matchit(on.fl ~ sector + liquidity, data = y.sub,
                             method = "sample", n.matches = 10)

stopifnot(
          all(is.na(test[1,]))
          )

################################################################################
## Matchit subfunctions
################################################################################

## tests that .valid.data converts character vectors to factors

x <- portfolio:::.valid.data(treat.var = "on.fl", c("sector", "country"), x)

stopifnot(
          identical(class(assay$sector), "factor"),
          identical(class(assay$country), "factor")
          )

## tests .remove.nas

x <- assay

x[c(1,10,100), "sector"] <- NA

x <- portfolio:::.remove.nas(data = x, treat.var = "on.fl", c("sector", "country"),
                             verbose = TRUE)

stopifnot(
          nrow(x) < nrow(assay)
          )
