validateOBIS <- function(register) {
  grepl(pattern = "^\\d{1}-\\d{1,2}:\\d{1,2}\\.\\d{1,2}\\.\\d{1,2}$",
        x = register)
}