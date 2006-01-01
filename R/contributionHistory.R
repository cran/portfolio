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
          }
          )

setMethod("summary",
          signature(object = "contributionHistory"),
          function(object){
            cat("Mean contribution:\n")
            show(mean(object))
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

setMethod("plot",
          signature(x = "contributionHistory", y = "missing"),
          function(x){

            tlist <- list()
            for(var in unique(unlist(lapply(x@data, function(x){ names(x@data) })))){
              title <- paste("roic by", var)
              data <- do.call(rbind, lapply(x@data, function(x){ x@data[[var]] }))

              if("rank" %in% names(data)){
                data$variable <- data$rank
              }
              
              ## Display by variable value alphabetically.
              
              data$variable <- factor(data$variable, levels = rev(sort(unique(as.character(data$variable)))))
              tlist[[var]] <- bwplot(variable ~ roic, data = data, main = title)
            }

            portfolio:::.trellis.multiplot(tlist)
          }
          )
