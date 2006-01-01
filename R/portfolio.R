################################################################################
##
## $Id: portfolio.R 1499 2006-01-01 22:50:17Z enos $
##
## A more complex, full-featured portfolio object that includes
## shares, a notion of equity, and is better suited for use in
## implementation.
##
################################################################################


## Calculate weights from shares.
##
## Calculate a weight for each position and store in the 'weights'
## slot data frame.  Weight of a position is calculated with respect
## to the market value of all positions on the same side as the
## position.

setMethod("calcWeights",
          "portfolio",
          function (object){

            ## Nothing to do here if there are no positions.
            
            if(nrow(object@shares) == 0)
              return(object)

            x <- merge(object@shares, object@data, by = "id", all.x = TRUE)
            
            ## Can't calculate mv without price.var.

            if(! object@price.var %in% names(x)){
              stop("Cannot calculate shares without a valid price.var!")
            }

            x$mv <- x$shares * x[[object@price.var]]

            ## Weight is position dollars/total side dollars.
            ## Should we also include bps in the weights data frame?

            mvLong  <- mvLong(object)
            mvShort <- mvShort(object)

            x$weight <- x$mv / ifelse(x$shares > 0, mvLong, abs(mvShort))
            
            object@weights <- x[c("id","weight")]

            ## As a final check, ensure that the weights and shares
            ## slots contain exactly the same set of securities.

            xx <- merge(object@weights, object@shares, by = "id")
            stopifnot(nrow(xx) == nrow(object@shares))
            
            invisible(object)
          }
          )

## Calculate shares from weights.
##
## Shares are calculated from weights by multiplying portfolio equity
## by weight for each security.  Additional adjustments to share
## values can be made if the necessary data is present, such as
## adjusting to round lot sizes.

setMethod("calcShares",
          "portfolio",
          function(object){

            ## Nothing to do here if there are no positions.
            
            if(nrow(object@weights) == 0)
              return(object)

            if(is.na(object@equity) || object@equity <= 0){
              stop("Must have non-zero positive equity to calculate shares!")
            }

            x <- merge(object@weights, object@data, by = "id", all.x = TRUE)
            
            ## Can't calculate shares without price.var.

            if(! object@price.var %in% names(x)){
              stop("Cannot calculate shares without a valid price.var!")
            }

            x$mv     <- x$weight * object@equity
            x$shares <- x$mv / x[[object@price.var]]

            ## The variable "round.lot" is special.  If not present,
            ## set all round lots to 1 and give a warning.  We might
            ## want to add a round lot adjustment on/off switch
            ## somewhere too.

            if(! "round.lot" %in% names(x)){
              x$round.lot <- 1
              warning("Setting all round lots to 1")
            }
            x$shares <- portfolio:::.nearest.multiple(x$shares, x$round.lot)
            
            object@shares <- x[c("id","shares")]

            ## As a final check, ensure that the weights and shares
            ## slots contain exactly the same set of securities.

            xx <- merge(object@weights, object@shares, by = "id")
            stopifnot(nrow(xx) == nrow(object@weights))
            
            invisible(object)
          }
          )

setMethod("create",
          "portfolio",
          function(object, ...){

            object <- callNextMethod()
            object <- calcShares(object)

            object
          }
          )

## Long/Short market value calculations.  There are some ambiguities
## that come up when calculating mv from weights (such as what to do
## with fractional shares) so require shares for now.

setMethod("mvLong",
          "portfolio",
          function(object){

            if(nrow(object@shares) == 0)
              return(0)

            x <- merge(object@data, object@shares, by = "id")
            
            ## Can't calculate mv without price.var.

            if(! object@price.var %in% names(x)){
              stop("Cannot calculate market value without a valid price.var!")
            }

            x$mv <- x$shares * x[[object@price.var]]
            sum(x$mv[x$mv > 0], na.rm = TRUE)
          }
          )

setMethod("mvShort",
          "portfolio",
          function(object){

            if(nrow(object@shares) == 0)
              return(0)

            x <- merge(object@data, object@shares, by = "id")
            
            ## Can't calculate mv without price.var.

            if(! object@price.var %in% names(x)){
              stop("Cannot calculate market value without a valid price.var!")
            }

            x$mv <- x$shares * x[[object@price.var]]
            sum(x$mv[x$mv < 0], na.rm = TRUE)
          }
          )

## Try to look up information for this security in this portfolio
## using "x" as a lookup identifier.

setMethod("securityInfo",
          signature(object = "portfolio", id = "character"),
          function(object, id){

            x <- merge(object@data, object@weights, by = "id")
            x <- merge(x, object@shares)

            ## Try to match on the id column first, then symbol if
            ## present in the object's data slot.
            
            pat <- paste("^", id, "$", sep = "")           
            if(any(grep(pat, x$id))){
              y <- x[x$id == id,]
            }
            else if(length(object@symbol.var) > 0 &&
                    object@symbol.var %in% names(x) &&
                    any(grep(pat, x[[object@symbol.var]]))){
              y <- x[x[[object@symbol.var]] == id,]
            }
            else{
              stop("id not found")
            }

            if(length(object@price.var) == 1 &&
               object@price.var %in% names(x)){
              y$mv <- y[[object@price.var]] * y$shares
            }

            columns <- c("id", "weight", "shares",
                         "mv", object@in.var)

            if(length(object@symbol.var) > 0 &&
               object@symbol.var %in% names(x)){
              row.names(y) <- y[[object@symbol.var]]
            }

            
            show(y[columns[columns %in% names(y)]])
            
            invisible(y)
          }
          )


setMethod("+",
          signature(e1 = "portfolio", e2 = "portfolio"),
          function(e1, e2){

            r.basic <- callNextMethod()
            r <- new("portfolio", type = "unknown", size = "unknown")
            r@data <- r.basic@data
            r@weights <- r.basic@weights

            r@id.var <- r.basic@id.var
            r@symbol.var <- r.basic@symbol.var
            r@ret.var <- r.basic@ret.var

            if(nrow(e1@shares) == 0 && nrow(e2@shares) == 0){
              return(r)
            }
            else if(nrow(e1@shares) == 0){
              r@shares <- e2@shares
              return(r)
            }
            else if(nrow(e2@shares) == 0){
              r@shares <- e1@shares
              return(r)
            }
            else{
              
              w <- merge(subset(e1@shares, !is.na(shares)),
                         subset(e2@shares, !is.na(shares)),
                         suffixes = c(".e1", ".e2"), by = "id", all = TRUE)
              

              w$shares.e1[is.na(w$shares.e1)] <- 0
              w$shares.e2[is.na(w$shares.e2)] <- 0
              
              w$shares <- w$shares.e1 + w$shares.e2
              r@shares <- w[c("id","shares")]
            }
            return(r)
          }
          )
         
