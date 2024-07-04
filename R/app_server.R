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

  # Create event selector
  output$event_selector <- renderUI({
    event_choices <- sapply(parsed_schema, function(event) event$title[[input$language]])
    selectInput("selected_event", "Select Event", choices = event_choices)
  })

  # Generate UI elements for the selected event
  observeEvent(input$selected_event, {
    req(input$selected_event)

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
      # Render the UI for the selected event
      output$dynamic_ui <- renderUI({
        event_ui <- create_ui(list(event), ns = NS("dynamic"), language = input$language)
        tagList(
          h3(event$title[[input$language]]),
          event_ui
        )
      })

      # Add validation observers for each field
      lapply(names(event$properties), function(prop_name) {
        field <- event$properties[[prop_name]]
        input_id <- NS("dynamic")(make.names(field$title[[input$language]]))

        # observeEvent(input[[input_id]], {
        #   value <- input[[input_id]]
        #   validation_result <- validate_field(value, field)

        #   # Update UI based on validation result
        #   if (!validation_result$valid) {
        #     shinyjs::addClass(input_id, "is-invalid")
        #     shinyjs::html(paste0(input_id, "_validation"), validation_result$message)
        #   } else {
        #     shinyjs::removeClass(input_id, "is-invalid")
        #     shinyjs::html(paste0(input_id, "_validation"), "")
        #   }
        # })
      })
    } else {
      output$dynamic_ui <- renderUI({
        p("No matching event found.")
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
