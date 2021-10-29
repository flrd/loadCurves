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
  NAD_S <- sprintf("NAD+MS+%s::%d'", sender, NAD_value)
  NAD_R <- sprintf("NAD+MR+%s::%d'", receiver, NAD_value)
  return(
    list(
      UNB = UNB
      ,NAD_S = NAD_S
      ,NAD_R = NAD_R
      )
  )
}