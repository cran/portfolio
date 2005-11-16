################################################################################
##
## $Id: $
##
## All generic functions for the portfolio class.
##
################################################################################

if(!isGeneric("create"))
  setGeneric("create", function(object, ...) standardGeneric("create"))

if(!isGeneric("scaleWeights"))
  setGeneric("scaleWeights", function(object, ...) standardGeneric("scaleWeights"))

if(!isGeneric("balance"))
  setGeneric("balance", function(object, in.var) standardGeneric("balance"))

if(!isGeneric("exposure"))
  setGeneric("exposure", function(object, exp.var) standardGeneric("exposure"))

if(!isGeneric("performance"))
  setGeneric("performance", function(object) standardGeneric("performance"))

if(!isGeneric("portfolioDiff"))
  setGeneric("portfolioDiff", function(object, x) standardGeneric("portfolioDiff"))

if(!isGeneric("contribution"))
  setGeneric("contribution", function(object, contrib.var, ...) standardGeneric("contribution"))

if(!isGeneric("securityInfo"))
  setGeneric("securityInfo", function(object, id) standardGeneric("securityInfo"))

## Class portfolio only.

if(!isGeneric("calcWeights"))
  setGeneric("calcWeights", function(object) standardGeneric("calcWeights"))

if(!isGeneric("calcShares"))
  setGeneric("calcShares", function(object) standardGeneric("calcShares"))

if(!isGeneric("mvLong"))
  setGeneric("mvLong", function(object) standardGeneric("mvLong"))

if(!isGeneric("mvShort"))
  setGeneric("mvShort", function(object) standardGeneric("mvShort"))

## Class portfolioHistory

if(!isGeneric("add"))
  setGeneric("add", function(object, x, ...) standardGeneric("add"))

