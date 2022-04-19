loadCurveSets_fun <- function(periodStart, periodEnd, valueInterval) {
  sprintf(
    '{"channel":1,"periodStart":"%s","periodEnd":"%s","quality":1,"valueInterval":%d,"createdBy":"string",'
    ,format(periodStart, "%Y-%m-%d 00:00:00")
    ,format(periodEnd, "%Y-%m-%d 00:00:00")
    ,valueInterval
  )
}
