validateOBIS <- function(x) {
  grepl(pattern = "^\\d{1}-\\d{1,2}:\\d{1,2}\\.\\d{1,2}\\.\\d{1,2}$",
        x = x)
}