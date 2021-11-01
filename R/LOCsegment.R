# LOCsegment <- function(marketLocation) {
#   if (marketLocation == "" || is.null(marketLocation)) {
#     marketLocation <- "{{Market Location}}"
#   }
#   
#   LOC <- sprintf("LOC+172+%s'", marketLocation)
#   
#   return(LOC)
# }