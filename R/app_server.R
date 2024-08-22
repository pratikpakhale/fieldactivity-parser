#' Get the path to the schema file
#'
#' @return The path to the schema file
schema_file_path <- function() {
  system.file("extdata", "schema.json", package = "fieldactivityParser")
}

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Load and parse the JSON schema
  schema <- jsonlite::fromJSON(schema_file_path(), simplifyVector = FALSE)
  parsed_schema <- parse_json_schema(schema)

  event_data <- parsed_schema

  # Function to generate event UI
  generate_event_ui <- function(event, language) {
    if (!is.null(event)) {
      event_ui <- create_ui(list(event), ns = NS("dynamic"), language = language)
      tagList(
        h3(event$title[[language]]),
        event_ui
      )
    } else {
      p("No event data available.")
    }
  }

  # Render the dynamic UI
  output$dynamic_ui <- renderUI({
    event <- event_data
    generate_event_ui(event, input$language)
  })

  # Observer for oneOf properties
  observe({
    lapply(names(parsed_schema$properties), function(prop_name) {
      prop <- parsed_schema$properties[[prop_name]]
      if (!is.null(prop$oneOf)) {
        oneof_id <- NS("dynamic")(paste0(prop_name, "_oneof"))
        nested_properties_id <- NS("dynamic")(paste0(prop_name, "_nested"))

        output[[nested_properties_id]] <- renderUI({
          selected_option <- input[[oneof_id]]
          if (selected_option != " ") {
            selected_properties <- prop$oneOf[[which(sapply(prop$oneOf, function(option) option$value == selected_option))]]$properties

            if (!is.null(selected_properties)) {
              create_properties_ui(selected_properties, NS("dynamic"), input$language)
            }
          }
        })
      }
    })
  })


  # Observer for validation logic
  observe({
    event <- event_data
    if (!is.null(event)) {
      lapply(names(event$properties), function(prop_name) {
        field <- event$properties[[prop_name]]
        input_id <- NS("dynamic")(make.names(field$title[[input$language]]))
        # Add your validation logic here
        # For example:
        # observeEvent(input[[input_id]], {
        #   # Validation code
        # })
      })
    }
  })

  # Update UI elements when language changes
  observeEvent(input$language, {
    updateSelectInput(session, "selected_event",
      choices = sapply(parsed_schema, function(event) event$title[[input$language]])
    )

    if (!is.null(input$selected_event)) {
      # Find the event by matching the title
      selected_event_title <- input$selected_event
      event <- NULL
      for (e in parsed_schema) {
        if (e$title[[input$language]] == selected_event_title) {
          event <- e
          break
        }
      }

      if (!is.null(event)) {
        # Update UI elements for the selected event
        lapply(names(event$properties), function(prop_name) {
          element <- event$properties[[prop_name]]
          if (!is.null(element) && !is.null(element$type)) {
            tryCatch(
              {
                update_ui_element(session, element, NULL, input$language)
              },
              error = function(e) {
                warning(paste("Error updating UI element:", prop_name, "-", e$message))
              }
            )
          }
        })
      }
    }
  })
}
