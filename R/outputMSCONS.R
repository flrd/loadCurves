MSCONS <- function(
  sender
  ,receiver
  ,marketLocation
  ,register = NULL
  ,periodStart
  ,periodEnd
  ,n
  ,DTM_QTY
  ,totalConsumption
  ,energyType
  ) {
  

  periodStart <- format(periodStart, format = "%Y%m%d%H%M")
  periodEnd <- format(periodEnd, format = "%Y%m%d%H%M")
  
  # UNT is the number of segment from segments UNH to UNT (both included)
  UNT <- 13L + pmax(1L, n - 1L) * 3 + 1L
  
  # if user did not provide input, then add a {{Placeholder}}
  UNB <- UNB_NADsegments(sender, receiver, energyType)[["UNB"]]
  
  RFF <- RFFsegment(energyType)
  
  NADsegments <- UNB_NADsegments(sender, receiver, energyType)
  NAD_Sender <- NADsegments[["NAD_Sender"]]
  NAD_Receiver <- NADsegments[["NAD_Receiver"]]
  LOC <- LOCsegment(marketLocation)
  
  PIA <- PIAsegment(register)
  

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
      ,DTM_QTY
      ,sprintf("UNT+%d+DCBKCICHBBCFBG'", UNT)
      ,"UNZ+1+DCBKCICHBBCECD'"
      )
  )
}
 


