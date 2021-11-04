valuesMSCONS <- function(timestamp, quantity) {
  
  # for MSCONS we need a time sequence of length 2 at least 
  if(length(timestamp) <= 1) stop("Please provide a time series with at least 2 values.")

  startsOfInterval <- timestamp[-length(timestamp)]
  endsOfInterval <- timestamp[-1L]
  
  # Map generates a list of length 3
  # the 1st element in the list will contain the values
  # the 2st element in the list will contain the starts of measurement
  # the 3st element in the list will contain the ends of measurement
  Map(sprintf,
      list(
        quantity = quantity
        ,startOfsInterval = startsOfInterval
        ,endsOfInterval = endsOfInterval),
      fmt = list("QTY+220:%s'"
                 ,"DTM+163:%s?+02:303'"
                 ,"DTM+164:%s?+02:303'")) |>
  # do.call (paste, ...) concatenates the elements of the lists into single character vector
  # for the pipe operator see ?`|>`
    (function(l) do.call(paste, c(l, sep = "\n")))()
}
