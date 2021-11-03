# OBIS validation ---------------------------------------------------------
# regex check for structure of the register, see page 5/27 in:
# https://www.edi-energy.de/index.php?id=38&tx_bdew_bdew%5Buid%5D=838&tx_bdew_bdew%5Baction%5D=download&tx_bdew_bdew%5Bcontroller%5D=Dokument&cHash=a7a251b9c22a4b3fa581f0717736a8ef

validateOBIS <- function(register) {
  grepl(pattern = "^\\d{1}-\\d{1,2}:\\d{1,2}\\.\\d{1,2}\\.\\d{1,2}$",
        x = register)
}


# RFF segment -------------------------------------------------------------

RFFsegment <- function(energyType = c("Electricity", "Gas")) {
  
  c(
    Electricity = "RFF+Z13:13018'"
    ,Gas = "RFF+Z13:13008'"
  )[[energyType]]
  
}


# PIA segment -------------------------------------------------------------

PIAsegment <- function(register) {
  
  if(validateOBIS(register)) {
    tmp <- strsplit(register, split = ":") |> unlist()
    PIA <- sprintf("PIA+5+%s?:%s:SRW'", tmp[[1]], tmp[[2]])
    
  } else if (register == "" || is.null(register)) {
    PIA <- "PIA+5+{{Register}}:SRW'"
    
  } else {
    PIA <- sprintf("PIA+5+%s:SRW'", register)
  }
  
  return(PIA)
  
}


# LOC segment -------------------------------------------------------------

LOCsegment <- function(marketLocation) {
  if (marketLocation == "" || is.null(marketLocation)) {
    marketLocation <- "{{Market Location}}"
  }
  
  LOC <- sprintf("LOC+172+%s'", marketLocation)
  
  return(LOC)
}


# UNB, NAD segments -------------------------------------------------------

UNB_NADsegments <- function(sender, receiver, energyType) {
  
  if(sender == "" || is.null(sender)) {
    sender <- "{{Sender}}"
  } 
  
  if (receiver == "" || is.null(receiver)) {
    receiver <- "{{Receiver}}"
  } 
  
  
  UNB_value <- c(
    "Electricity" = 500
    ,"Gas" = 502
  )[[energyType]]
  
  NAD_value <- c(
    "Electricity" = 293
    ,"Gas" = 332
  )[[energyType]]
  
  # the pattern %1$s reads as take the first values from the ... argument, %2$s the second, %3$d third
  UNB <- sprintf("UNB+UNOC:3+%1$s:%3$d+%2$s:%3$d+210917:%3$d+DCBKCICHBBCECD++TL'", sender, receiver, UNB_value)
  NAD_Sender <- sprintf("NAD+MS+%s::%d'", sender, NAD_value)
  NAD_Receiver <- sprintf("NAD+MR+%s::%d'", receiver, NAD_value)
  return(
    list(
      UNB = UNB
      ,NAD_Sender = NAD_Sender
      ,NAD_Receiver = NAD_Receiver
    )
  )
}