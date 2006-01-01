################################################################################
##
## $Id: $
##
## Basic portfolio class.
##
## This portfolio class represents a portfolio in weight space only,
## and so does not rely on the presence of prices in the object's data
## slot.  Slots specifying type and size specify instructions for
## delegating to the 'weight' method for creation.
##
################################################################################

setMethod("initialize",
          signature(.Object = "portfolioBasic"),
          function(.Object, ...){

            .Object <- callNextMethod()
            create(.Object)
          }
          )

## Populate the weights slot using the slots in.var, weight.var, size,
## and type specified in this object.  Use the 'data' slot to grab
## in.var and weight.var as input.

setMethod("create",
          "portfolioBasic",
          function(object, ...){
            if(length(object@in.var) == 0){
              return(object)
            }
            else if(length(object@in.var) > 1){
              stop("Only one in.var is allowed.")
            }

            ## Both id and in.var must be present in the data slot.

            if(object@id.var != "id"){
              if("id" %in% names(object@data)){
                object@data$id.bak <- object@data$id
              }
              object@data$id <- object@data[[object@id.var]]
            }
            
            input <- object@data

            stopifnot("id"          %in% names(input))
            stopifnot(object@in.var %in% names(input))

            ## Delegate to the 'weight' function.
            
            input$weight   <- weight(input, object@in.var, object@type,
                                     object@size, object@sides,
                                     object@weight.var)

            input <- input[!is.na(input$weight),][c("id", "weight")]
            
            ## Apply min/max weights from the weight.range slot.

            min.weight <- object@weight.range[1]
            max.weight <- object@weight.range[2]
            
            input$weight[abs(input$weight) < min.weight] <-
              sign(input$weight[abs(input$weight) < min.weight]) * min.weight

            if(is.finite(max.weight)){
              input$weight[abs(input$weight) > max.weight] <-
                sign(input$weight[abs(input$weight) > max.weight]) * max.weight
            }

            object@weights <- input
                                  
            show(object)
            invisible(object)
          }
          )

## Scale weights to the long/short weight sum targets given in the
## 'target' parameter.  Specifying 'condition' will restrict operation
## to a subset of weights.  It must be possible to evaluate the
## expression 'condition' in the environment of this object's data
## slot.

setMethod("scaleWeights",
          "portfolioBasic",
          function(object, target = c(1,-1), condition){

            if(length(target) != 2){
              stop("Must supply both long and short weight sums")
            }

            if(!all(is.numeric(target))){
              stop("Invalid target parameter")
            }

            x <- merge(object@weights, object@data, by = "id", all.x = TRUE)

            ## Work within the set of weights + data as defined by the
            ## condition parameter, if any.

            if(missing(condition)){
              r <- TRUE
            }
            else{
              e <- substitute(condition)
              r <- eval(e, x, parent.frame())
              r <- r & !is.na(r)
            }
            x <- x[r,]

            ## Compute positive and negative sums, then move existing
            ## weights to their new targets.
            
            sum.pos <- sum(x$weight[x$weight > 0])
            sum.neg <- sum(x$weight[x$weight < 0])

            x$weight <- ifelse(x$weight > 0, target[1], target[2]) *
              (x$weight / ifelse(x$weight > 0, sum.pos, sum.neg))


            ## Because we may have only worked on a subset of weights,
            ## use match to update the weights slot.  Note that the
            ## id's of x are matched into the id's of the weight
            ## slot's data frame.
            
            object@weights$weight[match(x$id, object@weights$id)] <-
              x$weight

            invisible(object)
          }
          )

## Balance weights by category as defined by in.var.  Weights with NA
## in.var are left untouched.

setMethod("balance",
          signature("portfolioBasic", "character"),
          function(object, in.var){

            if(!setequal(object@sides, c("long", "short"))){
              stop("balance method only available for long-short portfolios")
            }
            stopifnot(in.var %in% names(object@data))

            ## Hack right now for numeric in.vars: destructively
            ## modify the data slot by adding a new variable with the
            ## levels of in.var.

            if(is.numeric(object@data[[in.var]])){
              in.var.lvl <- paste(in.var, ".levels", sep = "")

              object@data[[in.var.lvl]] <-
                as.character(cut(object@data[[in.var]],
                                 breaks = c(-Inf,-1,1,Inf)))

              in.var <- in.var.lvl
            } else if(!is.character(object@data[[in.var]])) {
              in.var.char <- paste(in.var, ".char", sep = "")
              object@data[[in.var.char]] <- as.character(object@data[[in.var]])
              in.var <- in.var.char
            }

            x <- merge(object@weights, object@data, by = "id", all.x = TRUE)
            
            ## Since scaleWeights does not return a vector, we
            ## interate through all possible values of in.var.
            
            for(v in unique(x[[in.var]])){

              ## Calculate the target weights by first summing weights
              ## on both sides for this value of in.var, then by
              ## moving each to the average.
              
              target <- c(sum(x$weight[x$weight > 0 & x[[in.var]] == v]),
                          sum(x$weight[x$weight < 0 & x[[in.var]] == v]))
              
              target <- sign(target) * mean(abs(target))

              object <- eval(parse(text = paste("scaleWeights(object, target = target, condition = ",
                                     in.var, " == \"" , v, "\")",
                                     sep = "")))
            }

            invisible(object)
          }
          )

## Display/analysis methods.

setMethod("show",
          signature(object = "portfolioBasic"),
          function(object){
            cat(paste("An object of class \"", class(object), "\" with ",
                      nrow(object@weights)," positions\n\n", sep = ""))
            
            cat("Selected slots:\n")
            for(v in c("name","date","symbol.var","in.var","weight.var","ret.var","type","size")){
              if(length(slot(object, v)) > 0){
                cat(paste(v, ": ", slot(object, v), "\n",sep = ""))
              }
            }
          }
          )

setMethod("summary",
          signature(object = "portfolioBasic"),
          function(object){

            weight.col <- ifelse(nrow(object@weights) > 200, "bps", "pct")
            
            columns  <- c("id", weight.col)
            disp.num <- 5

            ## Don't go around merging and reporting if there aren't
            ## any weights.

            if(nrow(object@weights) == 0){
              cat("Empty portfolio\n")
              return()
            }

            x <- merge(object@weights, object@data, by = "id", all.x = TRUE)
            
            ## Create the weight.col column to be something useful to
            ## the user, based on the number of positions in this
            ## portfolio.

            x[[weight.col]] <- x$weight * ifelse(weight.col == "bps",
                                                 100 * 100,
                                                 100)

            ## Try to set row.names to something useful if symbol.var
            ## is present.

            if(length(object@symbol.var > 1) &&
               object@symbol.var %in% names(x)){
              rn <- ifelse(is.na(x[[object@symbol.var]]), x$id, as.character(x[[object@symbol.var]]))
              if(length(unique(rn)) == length(rn)){
                row.names(x) <- rn
              }
            }
            
            long  <- subset(x, weight > 0)
            short <- subset(x, weight < 0)

            ## Print basic information about the portfolio in a nice
            ## fashion.  This is my first pass at fixed-width
            ## formatting in R, and involves using the C-style
            ## sprintf.
            
            cat("Portfolio: ", object@name, "\n\n",
                sprintf("       %6s %12s", "count", "weight"), "\n",

                ifelse("long" %in% object@sides,
                       paste(
                             sprintf("Long:  %6s %12s",
                                     prettyNum(nrow(long), big.mark = ","),
                                     prettyNum(sum(long$weight), big.mark = ",")), "\n"),
                       ""),
                ifelse("short" %in% object@sides,
                       paste(
                             sprintf("Short: %6s %12s",
                                     prettyNum(nrow(short), big.mark = ","),
                                     prettyNum(sum(short$weight), big.mark = ",")), "\n"),
                       ""),
                "\n",
                "Top/bottom positions by weight:\n",
                sep = "")

            ## The following section will be expanded to include
            ## more top/bottom summaries akin to our perl portfolio
            ## summary.  Currently, the only data frame we show is
            ## sorted by weight.

            by.weight <- x[order(-x$weight, na.last = NA),]
            if(disp.num * 2 > nrow(by.weight)){
              show(by.weight[columns])
            }
            else{
              show(rbind(head(by.weight, n = disp.num),
                         tail(by.weight, n = disp.num))[columns])
            }
            
            if(nrow(x[is.na(x$weight),]) > 0){
              cat("\nNA weight:\n")
              show(x[is.na(x$weight),][columns])
            }
          }
          )

## Returns a list of data frames containing exposures to the factors
## specified in exp.var.  All numeric exposures are collected into the
## named list element 'numeric' and placed first in the list.
## Exposures to all other variables appear sorted by variable name.

setMethod("exposure",
          signature(object = "portfolioBasic", exp.var = "character"),
          function(object, exp.var){

            ## Return NULL if there are no weights for this portfolio.
            
            if(nrow(object@weights) == 0) return(NULL)

            ## All exp.var's must be present in this object's data
            ## slot.
            
            x <- merge(object@weights, object@data, by = "id")
            if(! all(exp.var %in% names(x))){
              stop("Cannot find all exp.var's")
            }
            
            x.long    <- subset(x, weight > 0)
            x.short   <- subset(x, weight < 0)

            ## Completed exposure data frames are collected in
            ## all.exp.
            
            all.exp <- list()
            
            for(ev in exp.var){

              ## For exp.var's of all types, construct a data frame
              ## with long exposure, short exposure, with factor value
              ## or numeric exp.var name as row.names.
              
              if(is.character(x[[ev]]) || is.factor(x[[ev]])){

                ## For character and factor exp.var's, sum the weights
                ## within each factor value for each side.

                long  <- as.data.frame(as.matrix(tapply(x.long$weight,
                                                        as.character(x.long[[ev]]), sum,
                                                        na.rm = TRUE)))
                names(long) <- "long"
                long$variable <- row.names(long)
                
                short <- as.data.frame(as.matrix(tapply(x.short$weight,
                                                        as.character(x.short[[ev]]), sum,
                                                        na.rm = TRUE)))
                names(short) <- "short"
                short$variable <- row.names(short)
              }
              else if(is.numeric(x[[ev]])){

                ## For numeric exp.var's, sum the product of weights
                ## and exp.var.
                
                long  <- data.frame(variable  = ev,
                                    long  =
                                    sum(x.long$weight * x.long[[ev]],
                                        na.rm = TRUE))
                short <- data.frame(variable  = ev,
                                    short =
                                    sum(x.short$weight * x.short[[ev]],
                                        na.rm = TRUE))
              }

              ## We might not have any rows in long or short,
              ## so don't use merge here.
              
              exp <- data.frame(variable = union(as.character(long$variable), as.character(short$variable)))
              exp$long <- long$long[match(exp$variable, long$variable)]
              exp$short <- short$short[match(exp$variable, short$variable)]
              
              ## We will need to include a way to supply counts.
              ## Right now we turn NA exposures into 0's, regardless
              ## of whether the NA comes from lack of securities or an
              ## inablility to compute weight.
              
              exp$long[is.na(exp$long)]   <- 0
              exp$short[is.na(exp$short)] <- 0

              exp$exposure <- exp$long + exp$short
              
              exp$exposure <- mapply(sum, na.rm = TRUE,
                                     exp$long,
                                     exp$short)

              ## Extract desired columns from the result and sort by
              ## exposure.
              
              this.exp <- exp[c("variable","long","short","exposure")]
              this.exp <- this.exp[order(this.exp$exposure, decreasing = TRUE),]

              ## Add numeric exposures to the 'numeric' list element.
              
              if(is.numeric(x[[ev]])){
                numeric.exp <- all.exp$numeric
                if(is.null(numeric.exp)){
                  all.exp$numeric <- this.exp
                }
                else{
                  all.exp$numeric <- rbind(numeric.exp, this.exp)
                }
                all.exp$numeric <- all.exp$numeric[order(all.exp$numeric$exposure,
                                                         decreasing = TRUE),]
              }
              else{
                all.exp[[ev]] <- this.exp
              }
            }

            ## If there are numeric exposures, put them at the front
            ## of the list.  Sort the rest afterwards by name.

            if(is.null(all.exp$numeric)){
              all.exp <- all.exp[sort(names(all.exp))]
            }
            else{
              all.exp <- all.exp[c("numeric",
                                   sort(names(all.exp)[- grep("numeric", names(all.exp))]))]
            }

            ## If we're long or short only, include just the exposure
            ## column.

            if(length(unique(object@sides)) == 1){
              all.exp <- lapply(all.exp, function(x) { x[c("variable", "exposure")] })
            }

            exp.obj <- new("exposure", data = all.exp)
            exp.obj
          }
          )

setMethod("performance",
          signature(object = "portfolioBasic"),
          function(object){

            perf <- new("performance")
            d    <- as.character(object@date)
            
            x <- merge(object@weights, object@data, by = "id", all.x = TRUE)
            
            ret.var <- object@ret.var
            stopifnot(length(ret.var) == 1)

            stopifnot(all(c("id", ret.var) %in% names(x)))

            x$ret         <- x[[ret.var]]
            
            x$contrib     <- x$weight * x[[ret.var]]

            ## Collect results in an object of class
            ## portfolioPerformance.

            perf@ret             <- sum(x$contrib,     na.rm = TRUE)
            perf@ret.detail      <- x[c("id", "weight", "ret", "contrib")]
            if(length(object@symbol.var) > 0 &&
               object@symbol.var %in% names(object@data)){
              row.names(perf@ret.detail) <- object@data[[object@symbol.var]][match(perf@ret.detail$id, object@data$id)]
            }
            
            ## Now expose the portfolio wrt the returns in the column
            ## specified in this object as ret.var.

            x$weight.new <- x$weight * (1 + x[[ret.var]])

            ## Don't change a weight to NA just because its ret.var
            ## was NA.

            x$weight.new <- ifelse(is.na(x$weight.new), x$weight, x$weight.new)
            x$weight     <- x$weight.new

            ## Don't require all weights to be non-NA.
            
            ## stopifnot(all(!is.na(x$weight)))
            
            object@weights <- x[c("id","weight")]
            perf@t.plus.one <- object

            perf
          }
          )

## Perform contribution on this portfolio by means of the performance
## function.

setMethod("contribution",
          signature(object = "portfolioBasic", contrib.var = "character"),
          function(object, contrib.var, buckets = 5){

            x <- object@data

            stopifnot(all(contrib.var %in% names(x)))

            ## Call the performance method and merge results into our
            ## data.frame. 

            ## I should probably add a check here to see if there is
            ## anything in the object 'perf'.

            perf <- performance(object)
            x <- merge(x, perf@ret.detail, by = "id")
            x$weight <- abs(x$weight)
            
            result <- list()
            
            for(att in contrib.var){

              att.cut <- att
              
              if(is.numeric(x[[att]])){
                att.cut <- paste(att, "levels", sep = ".")

                ## Note that numeric category quantiles are computed
                ## over the universe, or securities in the 'data'
                ## slot, not just the values for contrib.var among
                ## this portfolio's positions.

                quantiles <- quantile(object@data[[att]],
                                      seq(0, 1, 1/buckets), na.rm = TRUE)
                
                x[[att.cut]] <- cut(x[[att]], quantiles, na.rm = TRUE)
              }
              a <- aggregate(list(x[c("weight","contrib")]),
                             list(variable = x[[att.cut]]), sum, na.rm = TRUE)
              
              ## Make sure that all levels in the universe-based
              ## intervals are present in this aggregate, and that
              ## they appear in the correct order.

              all.levels <- levels(x[[att.cut]])

              if(any(! all.levels %in% a$variable)){
                a <- rbind(a, data.frame(variable = all.levels[! all.levels %in% a$variable],
                                         weight = 0, contrib = 0))
                a <- a[match(all.levels, a$variable),]
              }
              
              ## To compute roic, return on invested capital, divide
              ## contribution by weight scaled to [0,1].

              ## ** Should I leave roic as NaN (instead of 0) for
              ## those categories with 0 weight?

              a$weight   <- a$weight / sum(a$weight)
              a$roic     <- ifelse(a$weight == 0, 0, a$contrib / a$weight)

              if(is.numeric(x[[att]])){

                a$rank <- 1:nrow(a)

                ## Don't use high/low if only one category.
                
                if(nrow(a) != 1){
                  a$rank[1] <- "1 - low"
                  a$rank[nrow(a)] <- paste(nrow(a), "high", sep = " - ")
                }

                ## Put rank first.
                
                a <- a[c(which("rank" == names(a)),
                         which("rank" != names(a)))]

              }

              result[[att]] <- a
            }

            contrib.obj <- new("contribution", data = result)
            contrib.obj
          }
          )

setMethod("portfolioDiff",
          signature(object = "portfolioBasic", x = "portfolioBasic"),
          function(object, x){

            ## Right now, only deal with the case where we have the
            ## same columns in both portfolios' data slots.
            
            stopifnot(setequal(names(object@data), names(x@data)))
            p.diff.data <- rbind(object@data,
                                 subset(x@data, ! id %in% object@data$id))

            p.diff <- new("portfolioBasic",
                          name = "Portfolio diff", data = p.diff.data)


            w.diff    <- merge(object@weights, x@weights, by = "id", all = TRUE)
            w.diff.na <- NULL

            ## If a stock enters with an NA weight, it should have an
            ## NA weight in the diff.  (This is different from having
            ## a zero weight).

            if(any(is.na(object@weights$weight)) || any(is.na(x@weights$weight))){

              ## Populate w.diff.na if there are stocks in either
              ## portfolio with an na weight.
              
              w.diff.na <- subset(w.diff,
                                  id %in% union(object@weights$id[is.na(object@weights$weight)],
                                                x@weights$id[is.na(x@weights$weight)]))

              w.diff.na$weight <- NA
              w.diff.na <- w.diff.na[c("id","weight")]
              
              w.diff <- subset(w.diff, ! id %in% w.diff.na$id)
            }
            
            ## Make NA weights 0 for those stocks that were in one
            ## portfolio but not in the other.

            if(any(is.na(w.diff$weight.x))){
              w.diff$weight.x[is.na(w.diff$weight.x)] <- 0
            }
            if(any(is.na(w.diff$weight.y))){
              w.diff$weight.y[is.na(w.diff$weight.y)] <- 0
            }
            
            w.diff$weight <- w.diff$weight.y - w.diff$weight.x

            ## Those securities that have the same weight in both
            ## portfolios should not be included in the diff
            ## portfolio.
            
            w.diff <- subset(w.diff, weight != 0)
            
            p.diff@weights <- rbind(w.diff[c("id","weight")], w.diff.na)

            invisible(p.diff)
            
          }
          )

setMethod("plot",
          signature(x = "portfolioBasic", y = "missing"),
          function(x){
            stopifnot(length(x@in.var) == 1 && x@in.var %in% names(x@data))

            grid.newpage()
            pushViewport(viewport(layout = grid.layout(2, 1)))
            pushViewport(viewport(layout.pos.row = 1))

            y <- merge(x@data, x@weights, by = "id")
            
            ## Plot weight vs. in.var values.

            print(xyplot(y$weight ~ y[[x@in.var]],
                         main = "Weight vs. in.var",
                         xlab = paste(x@in.var, "(in.var)"),
                         ylab = "weight"), newpage = FALSE)
            
            popViewport(1)
            pushViewport(viewport(layout.pos.row = 2))

            ## Plot a histogram of weights.
            
            print(histogram(~ y$weight,
                            main = "Weight distribution",
                            xlab = "weight",
                            ylab = "count"), newpage = FALSE)

            popViewport(1)
            
          }
          )

setMethod("matching",
          signature(object = "portfolioBasic", covariates = "character"),
          function(object, covariates, method = "nearest", ...){

            if (!("MatchIt" %in% .packages(all = TRUE)))
              install.packages("MatchIt")
            require(MatchIt)

            if(nrow(object@weights) == 0){
              stop("Cannot calling matching on a portfolio with no positions")
            }
            
            x <- merge(object@data, object@weights, by = "id", all = TRUE)
            x$treatment.weight <- x$weight
            x$treatment        <- !is.na(x$treatment.weight)

            f <- formula(paste("treatment ~", paste(covariates, collapse = "+")))
            m <- matchit(f, data = x, method = method, discard = "both", ...)

            ## The match.matrix result provides a mapping from
            ## treatment to control securities, by means of a
            ## row.names mapping using row.names(x).

            id.map <- data.frame(old = x[row.names(m$match.matrix),]$id,
                                 new = x[m$match.matrix,]$id)

            new.weights <- object@weights
            new.weights$id <- id.map$new[match(new.weights$id, id.map$old)]

            object@weights <- new.weights
            invisible(object)
          }
          )

setMethod("+",
          signature(e1 = "portfolioBasic", e2 = "portfolioBasic"),
          function(e1, e2){

            ## We could spend a lot of time figuring out the correct
            ## value for each slot in the sum.  For now, though,
            ## arithmetic operators will return a portfolio object of
            ## the simplest form.
            
            r <- new("portfolioBasic", type = "unknown", size = "unknown")

            stopifnot(e1@id.var == e2@id.var)
            r@id.var <- e1@id.var

            stopifnot(e1@symbol.var == e2@symbol.var)
            r@symbol.var <- e1@symbol.var

            stopifnot(e1@ret.var == e2@ret.var)
            r@ret.var <- e1@ret.var
            
            if(nrow(e1@weights) == 0 && nrow(e2@weights) == 0){
              return(r)
            }
            else if(nrow(e1@weights) == 0){
              r@data    <- e2@data
              r@weights <- e2@weights
              return(r)
            }
            else if(nrow(e2@weights) == 0){
              r@data    <- e1@data
              r@weights <- e1@weights
              return(r)
            }
            else{

              ## Currently, we allow NA weights in the weights slot of
              ## portfolioBasic.  To make matters simpler here, the
              ## portfolio sum includes a weight for a security iff
              ## the security has a non-na weight in at least one of
              ## the addends.

              w <- merge(subset(e1@weights, !is.na(weight)),
                         subset(e2@weights, !is.na(weight)),
                         suffixes = c(".e1", ".e2"), by = "id", all = TRUE)


              w$weight.e1[is.na(w$weight.e1)] <- 0
              w$weight.e2[is.na(w$weight.e2)] <- 0
              
              w$weight <- w$weight.e1 + w$weight.e2
              r@weights <- w[c("id","weight")]

              ## The data slot in the sum portfolio needs to include a
              ## row for each security in the set union of securities
              ## in the two argument portfolios' data slots.

              ## At some point I should devise a way of performing
              ## validity checks for all arithmetic operators.  For
              ## instance, for binary operators that take two
              ## portfolioBasic objects, the data slots must contain
              ## data frames with the same columns.

              if(!setequal(names(e1@data), names(e2@data))){
                stop("portfolioBasic object must contain data slots with the same columns")
              }
              
              d <- rbind(e1@data, subset(e2@data, ! id %in% e1@data$id))
              r@data <- d
              return(r)
            }
          }
          )
          
