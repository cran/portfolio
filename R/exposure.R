################################################################################
##
## $Id: $
##
## Class wrapping exposure data.
##
################################################################################

setMethod("show",
          signature(object = "exposure"),
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
          signature(object = "exposure"),
          function(object){
            for(v in names(object@data)){
              cat(v, "\n")
              show(object@data[[v]])
              cat("\n")
            }
          }
          )
