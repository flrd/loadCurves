# PIAsegment <- function(register) {
#   
#   if(validateOBIS(register)) {
#     tmp <- strsplit(register, split = ":") |> unlist()
#     PIA <- sprintf("PIA+5+%s?:%s:SRW'", tmp[[1]], tmp[[2]])
#     
#     } else if (register == "" || is.null(register)) {
#       PIA <- "PIA+5+{{Register}}:SRW'"
#       
#       } else {
#         PIA <- sprintf("PIA+5+%s:SRW'", register)
#         }
#   
#   return(PIA)
# 
#   }