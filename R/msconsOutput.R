msconsOutput <- function(
  sender
  ,receiver
  ,marketLocation
  ,register = NULL
  ,timeSeries
  ,quantity
  ) {
  
  periodStart <- timeSeries[1L]
  periodEnd <- timeSeries[length(timeSeries)]
  
  # UNT is the number of segment from UNH to UNT (both included)
  UNT <- 13L + pmax(2, length(timeSeries) - 1L) * 3 + 1L
  
  
  
  # here we check for structure of the register
  # For OBIS-Code, see page 5/27 in:
  # https://www.edi-energy.de/index.php?id=38&tx_bdew_bdew%5Buid%5D=838&tx_bdew_bdew%5Baction%5D=download&tx_bdew_bdew%5Bcontroller%5D=Dokument&cHash=a7a251b9c22a4b3fa581f0717736a8ef
  # example: 1-1:1.29.0
  PIA <- PIAsegment(register)
  
  # if user did not provide input, then add a {{Placeholder}}
  UNB <- UNB_NADsegments(sender, receiver)[["UNB"]]
  
  NADsegments <- UNB_NADsegments(sender, receiver)
  NAD_S <- NADsegments[["NAD_S"]]
  NAD_R <- NADsegments[["NAD_R"]]
  LOC <- LOCsegment(marketLocation)
  
  # here comes single the pieces
  writeLines(
    c(
      "UNA:+.? '"
      ,UNB
      ,"UNH+DCBKCICHBBCFBG+MSCONS:D:04B:UN:2.3c'"
      ,"BGM+7+04372109171600149903913000003056310+9'"
      ,"DTM+137:202103171500:203'"
      ,"RFF+Z13:13018'"
      ,NAD_S
      ,NAD_R
      ,"UNS+D'"
      ,"NAD+DP'"
      ,LOC
      ,sprintf("DTM+163:%s?+01:303'", periodStart)
      ,sprintf("DTM+164:%s?+01:303'", periodEnd)
      ,"LIN+1'"
      ,PIA
      ,valuesMSCONS(
        timestamp = timeSeries,
        quantity = round(pi, 4)
      )
      ,sprintf("UNT+%d+DCBKCICHBBCFBG'", UNT)
      ,"UNZ+1+DCBKCICHBBCECD'"
      )
  )
}
 


