loadCurves <- function(marketPartnerNumber, register, energyType, valueInterval, locationNumber, locationType, createdBy) {
  data.frame(
    marketPartnerNumber = marketPartnerNumber,
    register = register,
    energyType = energyType,
    valueInterval = valueInterval,
    locationNumber = locationNumber,
    locationType = locationType,
    createdBy = "string"
  )
}