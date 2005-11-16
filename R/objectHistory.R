################################################################################
##
## $Id: $
##
## A simple history object.
##
################################################################################

setMethod("show",
          signature(object = "objectHistory"),
          function(object){
            cat(paste("Object of class \"", class(object), "\"\n", sep = ""))
            cat(paste("Number of periods:", length(object), "\n"))
            if(length(object) > 0){
              if(length(object) > 1){
                cat(paste("Period range:", min(as.Date(names(object@data))), "--",
                          max(as.Date(names(object@data))), "\n"))
              }
              else{
                cat(paste("Period:", as.Date(names(object@data)), "\n"))
              }
              cat(paste("Data class:", paste(unique(sapply(object@data, class), collapse = " ")), "\n"))
            }
          }
          )

setMethod("add",
          signature(object = "objectHistory", x = "ANY"),
          function(object, x, date){

            stopifnot(!is.null(x))

            if(date %in% names(object@data)){
              stop(paste("Object of class objectHistory already contains entry for", date))
            }
            else if(length(object@data) > 0 &&
                    class(x) != class(object@data[[1]])){
              stop(paste("Cannot add object of class", class(x),
                         "to an objectHistory containing objects of class", class(object@data[[1]])))
            }
            else{
              object@data[[date]] <- x
            }

            invisible(object)
          }
          )

setMethod("length",
          signature(x = "objectHistory"),
          function(x){
            length(x@data)
          }
          )
