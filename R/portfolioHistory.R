################################################################################
##
## $Id: $
##
## Object encapsulating historical portfolio statistics.  In order to
## facilitate the anaysis of large amounts of data, this class is
## designed to accept portfolio objects one at a time.  The statistics
## from each portfolio are either collected or used to update running
## tallies, eliminating the need store potentially huge amounts of
## day-security information.
##
################################################################################

setMethod("add",
          signature(object = "portfolioHistory", x = "portfolioBasic"),
          function(object, x){

            i <- as.character(x@date)

            expo <- exposure(x, object@exp.var)
            object@exposure <- add(object@exposure, expo, i)
            

            perf <- performance(x)

            object@performance <- add(object@performance, perf, i)

            contrib  <- contribution(x, object@contrib.var)
            object@contribution <- add(object@contribution, contrib, i)
            
            invisible(object)
          }
          )

setMethod("show",
          signature(object = "portfolioHistory"),
          function(object){
            cat(paste("An object of class \"", class(object), "\"\n", sep = ""))
            cat("Contains:\n\n")
            show(object@exposure)
            cat("\n")
            show(object@performance)
            cat("\n")
            show(object@contribution)
          }
          )

setMethod("summary",
          signature(object = "portfolioHistory"),
          function(object){
            cat("Portfolio history summary:\n\n")

            exposure(object)
            performance(object)
            contribution(object)
          }
          )

## Right now these three functions merely calculate and display some
## useful information about the historical exposure, performance, and
## contribution components of this object, but will be the subject of
## non-trivial design modifications later on.  For instance, this
## object should be storing three objects of class "objectHistory",
## and return objects of class "exposureHistory",
## "performanceHistory", and "contributionHistory".  These objects, in
## turn, will include more slots to store summary information, such as
## min/max exposures in "exposureHistory".

setMethod("exposure",
          signature(object = "portfolioHistory"),
          function(object, exp.var = NULL){
            cat("Mean exposure:\n")
            summary(mean(object@exposure))
          }
          )

setMethod("performance",
          signature(object = "portfolioHistory"),
          function(object){
            object@performance@freq <- object@freq
            summary(object@performance)
          }
          )

setMethod("contribution",
          signature(object = "portfolioHistory"),
          function(object, contrib.var = NULL){
            cat("Mean contribution:\n")
            summary(mean(object@contribution))
          }
          )        
          
