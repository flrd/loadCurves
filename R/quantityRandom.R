quantityRandom <- function(n, totalConsumption = NA_integer_) {
  
  values <- sin(seq.int(n) / 4) + 2
  
  if(is.na(totalConsumption)) {
    
    out <- as.character(round(values, 4))
  
  } else {
    
    out <- as.character(round(values / sum(values) * totalConsumption, 4))
      
    }
  
  return(out)
  
}


