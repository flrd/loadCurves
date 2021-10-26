
# double check if channel is dependent on the energy type
loadCurveSets <- function(periodStart, periodEnd) {
  list(
    data.frame(
      channel = 1L
      ,periodStart = periodStart
      ,periodEnd = periodEnd
      ,quality = 1L
      ,createdBy = "string"
    )
  )
}