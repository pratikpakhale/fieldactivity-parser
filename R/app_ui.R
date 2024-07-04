#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_ui <- function(request) {
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .invalid-feedback {
          display: block;
          width: 100%;
          margin-top: .25rem;
          font-size: 80%;
          color: #dc3545;
        }
        .is-invalid {
          border-color: #dc3545;
          padding-right: calc(1.5em + .75rem);
          background-image: url(\"data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' fill='none' stroke='%23dc3545' viewBox='0 0 12 12'%3e%3ccircle cx='6' cy='6' r='4.5'/%3e%3cpath stroke-linejoin='round' d='M5.8 3.6h.4L6 6.5z'/%3e%3ccircle cx='6' cy='8.2' r='.6' fill='%23dc3545' stroke='none'/%3e%3c/svg%3e\");
          background-repeat: no-repeat;
          background-position: right calc(.375em + .1875rem) center;
          background-size: calc(.75em + .375rem) calc(.75em + .375rem);
        }
      "))
    ),
    titlePanel("Field Activity Parser"),
    sidebarLayout(
      sidebarPanel(
        selectInput("language", "Select Language", choices = c("en", "fi", "sv")),
        uiOutput("event_selector")
      ),
      mainPanel(
        uiOutput("dynamic_ui")
      )
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "fieldactivityParser"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
