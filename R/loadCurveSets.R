loadCurveSets_fun <- function(periodStart, periodEnd, valueInterval) {
  
  ISO_8601 <- "%Y-%m-%dT00:00:00+00:00"
  sprintf(
    '{"channel":2,"periodStart":"%s","periodEnd":"%s","quality":1,"valueInterval":%d,"createdBy":"string",'
    ,format(periodStart, ISO_8601)
    ,format(periodEnd, ISO_8601)
    ,valueInterval
  )
}
