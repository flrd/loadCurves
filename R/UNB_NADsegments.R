UNB_NADsegments <- function(sender, receiver) {
  
  if(sender == "" || is.null(sender)) {
    sender <- "{{Sender}}"
  } 
  
  if (receiver == "" || is.null(receiver)) {
    receiver <- "{{Receiver}}"
  } 
  
  UNB <- sprintf("UNB+UNOC:3+%s:500+%s:500+210917:1500+DCBKCICHBBCECD++TL'", sender, receiver)
  NAD_S <- sprintf("NAD+MS+%s::293'", sender)
  NAD_R <- sprintf("NAD+MR+%s::293'", receiver)
  return(
    list(
      UNB = UNB
      ,NAD_S = NAD_S
      ,NAD_R = NAD_R
      )
  )
}