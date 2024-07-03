#' Get the path to the schema file
#'
#' @return The path to the schema file
schema_file_path <- function() {
  system.file("extdata", "schema_test.json", package = "fieldactivityParser")
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

  # Generate UI elements
  output$dynamic_ui <- renderUI({
    req(input$language)
    ui_elements <- lapply(parsed_schema, function(event) {
      event_title <- h3(event$title[[input$language]])
      event_properties <- create_properties_ui(event$properties, NS("dynamic"), input$language)
      tagList(event_title, event_properties)
    })
    tagList(ui_elements)
  })

  # Update UI elements when language changes
  observeEvent(input$language, {
    lapply(names(parsed_schema), function(event_type) {
      lapply(names(parsed_schema[[event_type]]$properties), function(prop_name) {
        element <- parsed_schema[[event_type]]$properties[[prop_name]]
        if (!is.null(element) && !is.null(element$type)) {
          tryCatch({
            update_ui_element(session, element, NULL, input$language)
          }, error = function(e) {
            warning(paste("Error updating UI element:", prop_name, "-", e$message))
          })
        }
      })
    })
  })
}
