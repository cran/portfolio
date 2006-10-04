################################################################################
##
## $Id: map.market.test.R 374 2006-10-04 13:33:28Z enos $
##
## Test for the map.market function
##
################################################################################

library(portfolio)

load("map.market.test.RData")

## save(test.data, truth, file = "map.market.test.RData", compress = TRUE)

result <- map.market(id = test.data$id, area = test.data$area,
                     group = test.data$group, color = test.data$color,
                     lab = TRUE, print = FALSE)

stopifnot(
          isTRUE(all.equal(getGrob(result, "MAP"), truth))
          )
