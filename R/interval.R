interval <- function(from,
                     to,
                     by = "60 mins",
                     # 60 because default 'by' is 60 minutes
                     # offset = 60, 
                     timezone = "GMT") {
  
  if (!inherits(from, "POSIXt")) {
    from <- as.POSIXlt(from, tz = timezone)
  }
  
  if (!inherits(to, "POSIXt")) {
    to <- as.POSIXlt(to, tz = timezone)
  }

  # *60 to go from seconds to minutes, as we deal with Posix class
  outInterval <- seq.POSIXt(from = from,
                            to = to ,
                            by = by)
  
  return(outInterval)
  
}
