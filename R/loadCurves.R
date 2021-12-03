loadCurves_fun <-
  function(marketPartnerNumber,
           register,
           energyType,
           valueInterval,
           marketLocation,
           locationType) {
    sprintf(
      '{"marketPartnerNumber":"%s","register":"%s","energyType":%d,"valueInterval":%d,"locationNumber":"%s","locationType":%d,"createdBy":"string",'
      ,marketPartnerNumber
      ,register
      ,energyType
      ,valueInterval
      ,marketLocation
      ,locationType
    )
  }
