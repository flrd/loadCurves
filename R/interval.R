interval <- function(from,
                     to,
                     by = "60 mins",
                     timezone = "GMT") {
  
  if (!inherits(from, "POSIXt")) {
    from <- as.POSIXlt(from, tz = timezone)
  }
  
  if (!inherits(to, "POSIXt")) {
    to <- as.POSIXlt(to, tz = timezone)
  }

  outInterval <- 
    seq.POSIXt(from = from,
               to = to,
               by = by)
  
  return(outInterval)
  
}
