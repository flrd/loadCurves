# generate time series based on desired output format ---------------------
interval <- function(from,
                     to,
                     by = "60 mins",
                     timezone = "GMT",
                     msconsFormat = FALSE) {
  
  if (!inherits(from, "POSIXt")) {
    from <- as.POSIXlt(from, tz = timezone)
  }
  
  if (!inherits(to, "POSIXt")) {
    to <- as.POSIXlt(to, tz = timezone)
  }
  
  if(!msconsFormat) {
    outInterval <-
      seq.POSIXt(from = from,
                 to = to,
                 by = by) |>
      format(format = "%Y-%m-%d 00:00:00")
  } else {
    outInterval <- 
      seq.POSIXt(from = from,
                 to = to,
                 by = by) |>
      format(format = "%Y%m%d%H%M")
  }
  
  return(outInterval)
  
}
