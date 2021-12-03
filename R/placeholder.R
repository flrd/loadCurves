placeholder <- function(inputId, condition, valuesElec, valuesGas) {
  updateTextInput(inputId = inputId,
                  placeholder = if(condition) valuesElec else valuesGas)
}

