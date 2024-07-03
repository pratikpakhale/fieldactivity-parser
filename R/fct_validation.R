### UNDER DEVELOPMENT

#' Validate a single input field
#'
#' @param value The input value to validate
#' @param field The field schema to validate against
#' @return A list with 'valid' (boolean) and 'message' (character) elements
validate_field <- function(value, field) {
  if (is.null(value) || value == "") {
    if (!is.null(field$required) && field$required) {
      return(list(valid = FALSE, message = "This field is required."))
    }
    return(list(valid = TRUE, message = NULL))
  }

  if (field$type == "number") {
    value <- as.numeric(value)
    if (is.na(value)) {
      return(list(valid = FALSE, message = "Must be a number."))
    }
    if (!is.null(field$minimum) && value < field$minimum) {
      return(list(valid = FALSE, message = paste("Must be at least", field$minimum)))
    }
    if (!is.null(field$maximum) && value > field$maximum) {
      return(list(valid = FALSE, message = paste("Must be at most", field$maximum)))
    }
  } else if (field$type == "string") {
    if (!is.null(field$minLength) && nchar(value) < field$minLength) {
      return(list(valid = FALSE, message = paste("Must be at least", field$minLength, "characters")))
    }
    if (!is.null(field$maxLength) && nchar(value) > field$maxLength) {
      return(list(valid = FALSE, message = paste("Must be at most", field$maxLength, "characters")))
    }
    if (!is.null(field$pattern) && !grepl(field$pattern, value)) {
      return(list(valid = FALSE, message = "Invalid format"))
    }
  } else if (field$type == "select") {
    allowed_values <- sapply(field$choices, function(choice) choice$value)
    if (!(value %in% allowed_values)) {
      return(list(valid = FALSE, message = "Invalid selection"))
    }
  }

  return(list(valid = TRUE, message = NULL))
}
