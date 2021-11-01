msconsOutput <- function(
  sender
  ,receiver
  ,marketLocation
  ,register = NULL
  ,timeSeries
  ,quantity
  ,energyType
  ) {
  
  # we extract the first and last values from the time sequence
  periodStart <- timeSeries[1L]
  periodEnd <- timeSeries[length(timeSeries)]
  
  # UNT is the number of segment from segments UNH to UNT (both included)
  UNT <- 13L + pmax(1L, length(timeSeries) - 1L) * 3 + 1L
  
  PIA <- PIAsegment(register)
  
  # if user did not provide input, then add a {{Placeholder}}
  UNB <- UNB_NADsegments(sender, receiver, energyType)[["UNB"]]
  
  NADsegments <- UNB_NADsegments(sender, receiver, energyType)
  NAD_Sender <- NADsegments[["NAD_Sender"]]
  NAD_Receiver <- NADsegments[["NAD_Receiver"]]
  LOC <- LOCsegment(marketLocation)
  
  RFF <- RFFsegment(energyType)
  

# build the MSCONS output -------------------------------------------------

  writeLines(
    c(
      "UNA:+.? '"
      ,UNB
      ,"UNH+DCBKCICHBBCFBG+MSCONS:D:04B:UN:2.3c'"
      ,"BGM+7+04372109171600149903913000003056310+9'"
      ,"DTM+137:202103171500:203'"
      ,RFF
      ,NAD_Sender
      ,NAD_Receiver
      ,"UNS+D'"
      ,"NAD+DP'"
      ,LOC
      ,sprintf("DTM+163:%s?+01:303'", periodStart)
      ,sprintf("DTM+164:%s?+01:303'", periodEnd)
      ,"LIN+1'"
      ,PIA
      ,valuesMSCONS(
        timestamp = timeSeries,
        quantity = quantityRandom(n = length(timeSeries))
      )
      ,sprintf("UNT+%d+DCBKCICHBBCFBG'", UNT)
      ,"UNZ+1+DCBKCICHBBCECD'"
      )
  )
}
 


