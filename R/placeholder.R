placeholder <- function(inputId, condition, valueTRUE, valueFALSE) {
  updateTextInput(inputId = inputId,
                  placeholder = ifelse(condition, valueTRUE, valueFALSE))
}

