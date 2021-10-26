channelInformation <- function(messageReference, sender) {
  data.frame(
    messageReference = messageReference
    ,sender = sender
    ,messageDateTime = Sys.time()
  )
}