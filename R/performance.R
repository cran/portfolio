################################################################################
##
## $Id: $
##
## Class wrapping performance data.
##
################################################################################

setMethod("show",
          signature(object = "performance"),
          function(object){
            cat(paste("An object of class \"", class(object), "\"\n", sep = ""))
            if(length(object@ret) > 0)
              cat(paste("ret: ", round(object@ret, 4), "\n"))
            cat("ret.detail (at most 10 records):\n")
            show(object@ret.detail[1:min(10, nrow(object@ret.detail)),])
          }
          )

setMethod("summary",
          signature(object = "performance"),
          function(object){
            cat("Performance summary:\n\n")
            if(length(object@ret) > 0){

              ret <- object@ret
              ret.tag  <- ifelse(abs(ret) > 0.01, "%", "bps")
              ret <- ifelse(ret.tag == "%", ret * 100, ret * 100 * 100)
              ret <- round(ret, digits = 2)
              
              cat(paste("Total return: ", ret, ret.tag, "\n\n"))
              if(nrow(object@ret.detail) > 0){
                x <- object@ret.detail
                x <- x[order(x$contrib, na.last = NA),]
                cat("Best/Worst performers:\n")

                if(nrow(x) < 10){
                  show(x)
                }
                else{
                  show(rbind(head(x, n = 5),
                             tail(x, n = 5)))
                }
              }
            }
            else{
              cat("No data.\n")
            }
          }
          )
