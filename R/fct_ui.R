#' Create UI elements based on parsed schema
#'
#' @param parsed_schema A parsed schema structure
#' @param ns A namespace function
#' @param language The current language
#' @return A list of UI elements
create_ui <- function(parsed_schema, ns, language = "en") {
  ui_elements <- lapply(parsed_schema, function(event) {
    event_properties <- create_properties_ui(event$properties, ns, language)
    event_oneof <- create_oneof_ui(event$oneOf, ns, language)
    tagList(event_properties, event_oneof)
  })

  return(tagList(ui_elements))
}


#' Create UI elements for properties
#'
#' @param properties A list of properties
#' @param ns A namespace function
#' @param language The current language
#' @return A list of UI elements
create_properties_ui <- function(properties, ns, language = "en") {
  div(
    # class = "flex-container",
    # style = "display: flex; flex-wrap: wrap; gap: 10px;",
    lapply(properties, function(prop) {
      if (prop$type == "array" && !is.null(prop$items) && prop$items$type == "object") {
        # Handle array of objects
        array_title <- h4(prop$title[[language]])
        array_items <- create_properties_ui(prop$items$properties, ns, language)
        div(
          class = "flex-item",
          style = "flex: 1 1 100%;",
          array_title,
          div(class = "array-items", array_items)
        )
      } else if (prop$type == "object" && !is.null(prop$properties)) {
        # Handle nested objects
        object_title <- h4(prop$title[[language]])
        object_properties <- create_properties_ui(prop$properties, ns, language)
        div(
          class = "flex-item",
          style = "flex: 1 1 100%;",
          object_title,
          div(class = "object-properties", object_properties)
        )
      } else if (!is.null(prop$oneOf)) {
        # Handle oneOf properties
        oneof_title <- h4(prop$title[[language]])
        oneof_id <- ns(paste0(make.names(prop$title[[language]]), "_oneof"))
        oneof_options <- lapply(prop$oneOf, function(option) {
          if (!is.null(option$title)) {
            option$title[[language]]
          } else {
            option$value # Use value as the label if title is missing
          }
        })
        oneof_values <- sapply(prop$oneOf, function(option) option$value)
        oneof_select <- selectInput(oneof_id,
          label = oneof_title,
          choices = setNames(oneof_values, oneof_options)
        )

        return(div(
          class = "flex-item",
          style = "flex: 1 1 100%;",
          oneof_select
        ))
      } else {
        # Create individual widget for other property types
        div(
          class = "flex-item",
          style = "flex: 1 1 300px;",
          create_widget(prop, ns, language)
        )
      }
    })
  )
}



create_oneof_ui <- function(oneof, ns, language = "en") {
  if (length(oneof) == 0) {
    return(NULL)
  }

  oneof_title <- h4("Select an option")
  oneof_id <- ns("oneof_select")
  oneof_options <- lapply(oneof, function(option) option$title[[language]])
  oneof_select <- selectInput(oneof_id, label = NULL, choices = oneof_options)

  oneof_properties_ui <- lapply(oneof, function(option) {
    option_properties <- create_properties_ui(option$properties, ns, language)
    conditionalPanel(
      condition = sprintf("input['%s'] == '%s'", oneof_id, option$title[[language]]),
      option_properties
    )
  })

  tagList(
    oneof_title,
    oneof_select,
    oneof_properties_ui
  )
}



#' Create individual widget
#'
#' @param element A parsed property structure
#' @param ns A namespace function
#' @param language The current language
#' @return A UI widget with validation
create_widget <- function(element, ns = NS(NULL), language = "en") {
  if (is.null(element$type)) {
    return(NULL)
  }

  element_label <- element$title[[language]]
  element_code_name <- ns(make.names(element_label))

  input_element <- switch(element$type,
    "select" = {
      choices <- if (!is.null(element$oneOf)) {
        print(element$oneOf)
        setNames(
          sapply(element$oneOf, function(choice) choice$value),
          sapply(element$oneOf, function(choice) choice$title[[language]])
        )
      } else if (!is.null(element$choices)) {
        setNames(
          sapply(element$choices, function(choice) choice$value),
          sapply(element$choices, function(choice) choice$title[[language]])
        )
      } else {
        NULL
      }
      selectInput(
        inputId = element_code_name, label = element_label,
        choices = choices,
        selected = NULL
      )
    },
    "number" = {
      numericInput(
        inputId = element_code_name,
        label = element_label,
        value = NULL,
        min = if (!is.null(element$minimum)) element$minimum else NA,
        max = if (!is.null(element$maximum)) element$maximum else NA
      )
    },
    "string" = {
      if (!is.null(element$ui) && !is.null(element$ui$`form-type`)) {
        if (element$ui$`form-type` == "textAreaInput") {
          textAreaInput(
            inputId = element_code_name,
            label = element_label,
            value = "",
            resize = "vertical",
            placeholder = if (!is.null(element$ui$`form-placeholder`)) element$ui$`form-placeholder` else ""
          )
        } else {
          textInput(
            inputId = element_code_name,
            label = element_label,
            value = "",
            placeholder = if (!is.null(element$ui$`form-placeholder`)) element$ui$`form-placeholder` else ""
          )
        }
      } else {
        textInput(
          inputId = element_code_name,
          label = element_label,
          value = "",
          placeholder = if (!is.null(element$ui$`form-placeholder`)) element$ui$`form-placeholder` else ""
        )
      }
    },
    NULL # Default case for unknown types
  )

  validation_id <- paste0(element_code_name, "_validation")

  tagList(
    div(
      class = "form-group",
      input_element,
      tags$div(id = validation_id, class = "invalid-feedback")
    )
  )
}



#' Get choices for select inputs
#'
#' @param element The parsed property structure
#' @param language The current language
#' @return A named vector of choices
get_select_choices <- function(element, language = "en") {
  if (element$type == "select") {
    if (!is.null(element$choices)) {
      # If choices are defined in the element, use them
      choices <- sapply(element$choices, function(choice) choice$value)
      names(choices) <- sapply(element$choices, function(choice) choice$title[[language]])
      return(choices)
    } else if (!is.null(element$ref)) {
      # If choices are referenced, implement logic to fetch them
      # For now, return NULL
      return(NULL)
    }
  }
  return(NULL)
}

#' Update UI element
#'
#' @param session The current Shiny session
#' @param element The element to update
#' @param value The new value
#' @param language The current language
update_ui_element <- function(session, element, value, language = "en") {
  if (is.null(element) || is.null(element$type)) {
    return(NULL)
  }

  element_code_name <- NS("dynamic")(names(element)[1])

  # Update the UI element based on its type
  if (element$type == "select") {
    updateSelectInput(session, element_code_name, choices = get_select_choices(element, language), selected = value)
  } else if (element$type == "number") {
    updateNumericInput(session, element_code_name, value = value)
  } else if (element$type == "string" && !is.null(element$ui) && element$ui$`form-type` == "textAreaInput") {
    updateTextAreaInput(session, element_code_name, value = value)
  } else if (element$type == "string") {
    updateTextInput(session, element_code_name, value = value)
  }
}
