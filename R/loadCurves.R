loadCurves_fun <-
  function(marketPartnerNumber,
           register,
           energyType,
           marketLocation,
           locationType) {
    sprintf(
      '{"marketPartnerNumber":"%s","register":"%s","energyType":%d,"locationNumber":"%s","locationType":%d,"createdBy":"string",'
      ,marketPartnerNumber
      ,register
      ,energyType
      ,marketLocation
      ,locationType
    )
  }
