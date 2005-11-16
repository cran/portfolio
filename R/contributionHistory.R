################################################################################
##
## $Id: $
##
## Class wrapping contribution data.
##
################################################################################

setMethod("show",
          signature(object = "contributionHistory"),
          function(object){
            callNextMethod()
            
            cat(paste("Variables:",
                      paste(unique(unlist(lapply(object@data,
                                                 function(x) {
                                                   names(x@data)
                                                 }))), collapse = " "),
                      "\n"))
            
            
            ## Don't show data for now.
            
            ## cat("Data:\n\n")

            ## for(v in names(object@data)){
            ##  cat(v, "\n")
            ##  show(object@data[[v]])
            ##  cat("\n")
            ## }
          }
          )


setMethod("mean",
          signature(x = "contributionHistory"),
          function(x){
            contrib.obj <- new("contribution")

            contrib.var <- unique(unlist(lapply(x@data, function(x) { names(x@data) })))
            for(v in contrib.var){
              dfs <- lapply(x@data, function(x){ x@data[[v]] })

              ## Deal with numeric levels (where variables are numeric
              ## intervals which likely change over time) by setting
              ## variable to rank and omitting the original
              ## intervals from the summary.
              
              if(any(sapply(dfs, function(x) { "rank" %in% names(x) } ))){
                dfs <- lapply(dfs, function(x) { x$variable <- x$rank; x["rank" != names(x)] })
              }
              dfs$by.var <- "variable"
              df.mean <- do.call(portfolio:::.df.category.mean, dfs)

              contrib.obj@data[[v]] <- df.mean
              invisible(contrib.obj)
            }
          }
          )
