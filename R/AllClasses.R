################################################################################
##
## $Id: $
##
## Class definitions for the portfolio package.
##
################################################################################

setOldClass(c("Date"))

## The size slot of the portfolioBasic class requires either a
## character or numeric vector.

setClassUnion("characterOrNumeric", c("character","numeric"))

## The portfolioBasic class contains the basic set of information
## required for working with portfolios with only weight data.

setClass("portfolioBasic",
         representation(
                        name          = "character",
                        date          = "Date",

                        ## The data slot can contain anything, but
                        ## must contain a column named 'id'.  Some
                        ## portfolio methods will require that other
                        ## columns be present in this data frame, and
                        ## will fail if they are missing.
                        
                        data          = "data.frame",

                        ## The following variable specs are character
                        ## vectors of length 1.  There can be only one
                        ## 'ret.var', for instance.

                        id.var        = "character",
                        symbol.var    = "character",
                        in.var        = "character",
                        weight.var    = "character",
                        ret.var       = "character",

                        type          = "character",
                        size          = "characterOrNumeric",
                        weight.range  = "numeric",
                        sides         = "character",
                        
                        ## Weights storage.  Contains data frame with
                        ## names c("id","weight").
                        
                        weights       = "data.frame"
                        ),

         ## in.var and weight.var are zero-length vectors to avoid a
         ## class union and to ease delegation to the 'weight'
         ## function.  It would be incorrect to default to some
         ## potentially missing (or unwanted) variable.
         
         prototype = prototype(
           name          = "Unnamed portfolio",
           date          = Sys.Date(),
           data          = data.frame(),

           id.var        = "id",
           symbol.var    = character(0),
           in.var        = character(0),
           weight.var    = character(0),
           ret.var       = character(0),
           
           type          = "equal",
           size          = "quintile",
           weight.range  = c(0, Inf),
           sides         = c("long","short"),
           
           weights       = data.frame()
           
           )
         )

## The portfolio class extends the portfolioBasic class for working
## with price and share information.

setClass("portfolio",
         representation(
                        equity    = "numeric",
                        file      = "character",
                        price.var = "character",
                        shares    = "data.frame"
                        ),
         prototype = prototype(
           equity    = 0,
           file      = "none",
           price.var = "price.usd",
           
           shares = data.frame()
           ),
         contains = "portfolioBasic",

         ## To start, the validity function checks to see whether
         ## there are the same number of rows in the weights and
         ## shares data frames.

         validity = function(object){

           ## Don't break if this is an empty portfolio.
           
           if(nrow(object@weights) == 0 && nrow(object@shares) == 0)
             return(TRUE)
           
           m <- merge(object@weights, object@shares, by = "id")
           return(nrow(object@weights) == nrow(m))
         }
         )

## Classes to encapsulate portfolio analysis data.

setClass("objectHistory",
         representation(
                        freq = "numeric",
                        data = "list"
                        ),
         prototype = prototype(
           freq = 1,
           data = list()
           ),
         validity = function(object){
           if(length(object@data) > 0){
             if(length(unique(sapply(object@data, class))) > 1){
               return(FALSE)
             }
           }
           return(TRUE)
         }
         )

setClass("exposure",
         representation(
                        data  = "list"
                        ),
         prototype = prototype(
           data = list()
           )
         )

setClass("exposureHistory",
         contains = "objectHistory",
         validity = function(object){

           if(length(object@data) > 0 &&
                     !all(sapply(object@data, class) == "exposure")){
             return(FALSE)
           }
           else{
             return(TRUE)
           }
         }
         )


setClass("performance",
         representation(
                        ret        = "numeric",
                        ret.detail = "data.frame",
                        t.plus.one = "portfolioBasic"
                        ),
         prototype = prototype(
           ret        = 0,
           ret.detail = data.frame(),
           t.plus.one = new("portfolioBasic")
           )
         )

setClass("performanceHistory",
         contains = "objectHistory",
         validity = function(object){

           if(length(object@data) > 0 &&
                     !all(sapply(object@data, class) == "performance")){
             return(FALSE)
           }
           else{
             return(TRUE)
           }
         }
         )


setClass("contribution",
         representation(
                        data  = "list"
                        ),
         prototype = prototype(
           data = list()
           )
         )

setClass("contributionHistory",
         contains = "objectHistory",
         validity = function(object){

           if(length(object@data) > 0 &&
                     !all(sapply(object@data, class) == "contribution")){
             return(FALSE)
           }
           else{
             return(TRUE)
           }
         }
         )

## The portfolioHistory object is used to collect descriptive
## information about a (time series) set of portfolioBasic objects.

setClass("portfolioHistory",
         representation(
                        name          = "character",
                        freq          = "numeric",
                        
                        ## Exposures.

                        exp.var       = "character",
                        exposure      = "exposureHistory",

                        ## Performance

                        performance   = "performanceHistory",

                        ## Contribution

                        contrib.var   = "character",
                        contribution  = "contributionHistory"
                        
                        ),

         prototype = prototype(
           name          = "Unnamed portfolio history",
           freq          = 1,
           
           exp.var       = character(0),
           exposure      = new("exposureHistory"),

           performance   = new("performanceHistory"),

           att.var       = character(0),
           contribution   = new("contributionHistory")
           )
         )

