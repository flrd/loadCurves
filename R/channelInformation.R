channelInformation_fun <- function(messageReference, sender, messageDateTime) {
  sprintf(
    '{"messageReference":"%s","sender":"%s","messageDateTime":"%s"}',
    messageReference, sender, messageDateTime
  )
}
