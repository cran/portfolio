################################################################################
##
## $Id: $
##
## Class wrapping historical exposure data.
##
################################################################################

setMethod("show",
          signature(object = "exposureHistory"),
          function(object){
            callNextMethod()

            cat(paste("Variables:",
                      paste(unique(unlist(lapply(object@data,
                                                 function(x) {
                                                   names(x@data)
                                                 }))), collapse = " "),
                      "\n"))
            
          }
          )

setMethod("summary",
          signature(object = "exposureHistory"),
          function(object){
            cat("Mean exposure:\n")
            show(mean(object))
          }
          )

setMethod("mean",
          signature(x = "exposureHistory"),
          function(x){
            exp.obj <- new("exposure")
            
            exp.numeric <- NULL
            dfs.numeric <- lapply(x@data, function(x) { x@data$numeric })
            
            for(n in dfs.numeric){
              exp.numeric <- union(exp.numeric, as.character(n$variable))
            }

            exp.toshow <- unique(unlist(lapply(x@data, function(x) { names(x@data) })))
            exp.toshow <- exp.toshow[! exp.toshow %in% exp.numeric]
            
            for(v in exp.toshow){
              dfs <- lapply(x@data, function(x){ x@data[[v]] })
              dfs$by.var <- "variable"
              df.mean <- do.call(portfolio:::.df.category.mean, dfs)
              if(v == "numeric"){ sort.by <- "variable" } else { sort.by <- "exposure" }
              df.mean <- df.mean[order(df.mean[[sort.by]]),]

              exp.obj@data[[v]] <- df.mean
            }
            invisible(exp.obj)

          }
          )

setMethod("plot",
          signature(x = "exposureHistory", y = "missing"),
          function(x){

            tlist <- list()
            for(var in unique(unlist(lapply(x@data, function(x){ names(x@data) })))){
              title <- paste(var, "exposure")
              data <- do.call(rbind, lapply(x@data, function(x){ x@data[[var]] }))

              ## Display by variable value alphabetically.
              
              data$variable <- factor(data$variable, levels = rev(sort(unique(as.character(data$variable)))))
              tlist[[var]] <- bwplot(variable ~ exposure, data = data, main = title)
            }

            portfolio:::.trellis.multiplot(tlist)
          }
          )
