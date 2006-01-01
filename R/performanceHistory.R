################################################################################
##
## $Id: $
##
## Class wrapping performance data.
##
################################################################################

setMethod("summary",
          signature(object = "performanceHistory"),
          function(object){

            cat("Performance summary (frequency = ", object@freq, "):\n\n", sep = "")
            
            p.ret <- data.frame(date = as.Date(names(object@data)),
                                ret  = unlist(lapply(object@data, function(x) { x@ret })))

            p     <- p.ret

            p.nodate  <- p[c("ret")]
            p.summary <- rbind(mean(p.nodate),
                               mean(p.nodate) * object@freq,
                               sd(p.nodate),
                               sd(p.nodate) * sqrt(object@freq),
                               sqrt(object@freq) * mean(p.nodate) / sd(p.nodate))
            row.names(p.summary) <- c("mean","mean (ann)", "sd", "sd (ann)", "sharpe")
            show(p.summary)

            cat("\nBest period:\n")
            show(p[which(p$ret == max(p$ret)),])

            cat("\n")
            
            cat("\nWorst period:\n")
            show(p[which(p$ret == min(p$ret)),])

            p.detail.all <- NULL
            for(i in names(object@data)){
              p.d <- object@data[[i]]@ret.detail
              p.d$date <- as.Date(i)
              p.detail.all <- rbind(p.detail.all, p.d)
            }
            p.detail.agg <- aggregate(p.detail.all[c("contrib")], by = list(id = p.detail.all$id),
                                      function(x) { prod(1 + x) - 1 })
            p.detail.agg <- p.detail.agg[c("id","contrib")]
            
            p.detail.agg <- p.detail.agg[order(p.detail.agg$contrib, na.last = NA, decreasing = TRUE),]
            
            cat("\nTop/Bottom performers (total contribution):\n")

            if(nrow(p.detail.agg) < 10){
              show(p.detail.agg)
            }
            else {
              show(rbind(head(p.detail.agg, n = 5),
                         tail(p.detail.agg, n = 5)))
            }

            cat("\n")
          }
          )

setMethod("plot",
          signature(x = "performanceHistory", y = "missing"),
          function(x){
            p.ret <- data.frame(date = as.Date(names(x@data)),
                                ret  = unlist(lapply(x@data, function(x) { x@ret })))
            p.ret <- p.ret[order(p.ret$date),]
            
            p.ret$date <- as.POSIXct(as.character(p.ret$date))

            p.ret$nav  <- cumprod(1 + p.ret$ret)
            p.ret$tret <- p.ret$nav - 1

            p.ret$ret  <- p.ret$ret  * 100
            p.ret$tret <- p.ret$tret * 100
            
            date.fmt.axis  <- "%y-%m-%d"

            main <- paste("Performance:",
                          paste(range(as.POSIXct(names(x@data))), collapse = " -- "))
            
            ## For now, plot all data points.
            
            print(xyplot(tret ~ date, data = p.ret, type = "l",
                         scales = list(cex = 0.8, x = list(at = p.ret$date)),
                         main   = main))

          }
          )
