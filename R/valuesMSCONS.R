valuesMSCONS <- function(intervalStarts, intervalEnds, quantity) {
  
  # Map generates a list of length 3
  # the 1st element in the list will contain the values
  # the 2st element in the list will contain the starts of measurement
  # the 3st element in the list will contain the ends of measurement
  
  Map(sprintf,
      list(
        quantity = quantity
        ,intervalStarts = intervalStarts
        ,intervalEnds = intervalEnds),
      fmt = list("QTY+220:%s'"
                 ,"DTM+163:%s?+02:303'"
                 ,"DTM+164:%s?+02:303'")) |>
  # do.call (paste, ...) concatenates the elements of the lists into single character vector
  # for the pipe operator see ?`|>`
    (function(lst) do.call(paste, c(lst, sep = "\n")))()
}

