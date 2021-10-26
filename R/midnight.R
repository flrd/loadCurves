midnight <- function(x) {
  as.POSIXlt(paste(x, "00:00:00"),
             tz = "GMT") |>
    format("%Y-%m-%d %H:%M:%S")
}
