################################################################################
##
## $Id: $
##
## Class wrapping contribution data.
##
################################################################################

setMethod("show",
          signature(object = "contribution"),
          function(object){
            cat(paste("An object of class \"", class(object), "\"\n", sep = ""))
            for(v in names(object@data)){
              cat(v, "\n")
              show(object@data[[v]])
              cat("\n")
            }
          }
          )

setMethod("summary",
          signature(object = "contribution"),
          function(object){
            for(v in names(object@data)){
              cat(v, "\n")
              show(object@data[[v]])
              cat("\n")
            }
          }
          )

