################################################################################
##
## $Id: matchedPortfolio.R 374 2006-10-04 13:33:28Z enos $
##
## Basic matchedPortfolio class.
##
## This matchedPortfolio class represents an "original" portfolioBasic that
## has been matched with one or more portfolios according to a specified
## formula.
##
################################################################################

setMethod("show",
          signature(object = "matchedPortfolio"),
          function(object){
            
            ## displays essential information

            plural <- ifelse(dim(object@matches)[2] > 1, "s", "")

            cat("An object of class \"matchedPortfolio\":\n\n",
                "slots:\n\n", sep = "")
            
            cat("formula: "); print(object@formula); cat("\n\n")

            cat("original:\n",
                sprintf("  %i %s", nrow(object@original@weights),
                        "positions."), "\n",
                sprintf("  %.3f %s", .orig.perf(object@original),
                        object@original@ret.var), "\n\n"

                )
            
            cat(paste("matches:", sep = ""), "\n",
                sprintf("  %i %s",
                        sum(object@matches[,1] != 0),
                        "positions."), "\n",
                sprintf("  %i %s%s", dim(object@matches)[2],
                        "matched portfolio", plural), "\n",
                sprintf("  %.3f %s", performance(object),
                        object@original@ret.var), "\n\n"
                )

            cat("Difference in returns:", .orig.perf(object@original)
                - performance(object), "\n")

          }
)

setMethod("summary",
          signature(object = "matchedPortfolio"),
          function(object){
            show(object)
          }
)

setMethod("performance",
          signature(object = "matchedPortfolio"),
          function(object, output = "pooled"){
            
            ## "output" is the format for returning performance.  The
            ## options are "pooled" and "individual".  "pooled"
            ## returns a scalar, the mean of the returns for all
            ## matched portfolios.  "individual" returns a vector of
            ## the returns for each portfolio

            stopifnot(validObject(object))

            .matched.perf(object@matches,
                          object@original@data[[object@original@ret.var]],
                          output)
                          

          }
          )

.orig.perf <- function(original){

  ## "original" is a "portfolioBasic".  Returns a scalar.

  data <- original@data
  weights <- original@weights
  ret.var <- original@ret.var

  res <- weights$weight * data[match(weights$id, data$id), ret.var]

  sum(res)
}

.matched.perf <- function(matches, returns, output = "individual"){

  ## "matches" is a weight matrix.  "returns" is a vector of
  ## returns. "output" is either "individual" or pooled and specifies
  ## whether to return a vector of the returns for each matched
  ## portfolio or the mean return

  ## calculates returns for each stock

  res <- apply(matches, 2, function(x){x * returns})

  ## calculates sum for each matched portfolio

  res <- apply(res, 2, sum)

  if(identical(output, "pooled")){
    res <- mean(res)
  }

  res
}

setMethod("contribution",
          signature(object = "matchedPortfolio", contrib.var = "character"),
          function(object, contrib.var, buckets = 5){
            
          }
          )

setMethod("exposure",
          signature(object = "matchedPortfolio", exp.var = "character"),
          function(object, exp.var, output = "pooled"){
            
            ## "exp.var" is a vector of variables on which to
            ## calculate exposure.  The "data" slot of the "original"
            ## portfolio must contain a column for every value in
            ## "exp.var".  Returns a list where each element in the
            ## list is the name of an element in "exp.var".

            stopifnot(validObject(object))

            data <- object@original@data
            
            ## Cannot calculate performance if there are no "exp.var"
            ## values.  "data" must contain information for at least 1
            ## stock.

            stopifnot(
                      length(exp.var) > 0,
                      nrow(data) > 0,
                      all(exp.var %in% names(data))
                      )

            exp.list <- list()

            ## calculates exposures for all elements of "exp.var"

            for(ev in exp.var){
              
              if(is.numeric(data[[ev]])){ ## numeric variables
                
                exp <- apply(object@matches, 2, function(x){
                  x * data[[ev]]})

                exp.list[[ev]] <- colMeans(exp)
                
              }
              else if(is.character(data[[ev]]) || is.factor(data[[ev]])){ # character variables

                ## builds a matrix to store the results of each call
                ## to "tapply".  "matrix" contains as many rows as
                ## their are matched portfolios and as many columns as
                ## there are levels of the current exposure variable,
                ## "ev"
                
                exp <- matrix(0,
                              nrow = dim(object@matches)[[2]],
                              ncol = length(levels(data[[ev]])),
                              dimnames = list(1:dim(object@matches)[[2]],
                                levels(data[[ev]])[order(levels(data[[ev]]))])
                              )

                for(i in 1:ncol(object@matches)){

                  exp[i,] <- tapply(object@matches[,i], data[[ev]], sum,
                                    simplify = TRUE)

                }
                
                if(identical(output, "pooled")){
                  exp <- colMeans(exp)
                }

                exp.list[[ev]] <- exp

              }
            }

            exp.list

          }
          )

setMethod("plot",
          signature(x = "matchedPortfolio", y = "missing"),
          function(x, type = "returns"){

            ## "type" is the kind of plot desired.  The only option is
            ## the default, "returns"
            
            if(identical(type, "returns")){
              .plot.returns(performance(x, output = "individual"))
            }
            
          }
          )

.plot.returns <- function(returns){
  histogram(returns)
}
