#' @title Import Data Module
#'
#' @description This module allow to import data into the application,
#'  to map input's columns to variable of interest for the simulation
#'  and to validate that all is in order to proceed.
#'
#' @export
#'
#' #' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name import-data
#'
#' @importFrom shiny NS fluidRow column
#' @importFrom htmltools tagList tags
import_data_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Import data",
    fluidRow(
      column(
        width = 4,
        datamods::import_file_ui(id = ns("import-file"), title = NULL)
      ),
      column(
        width = 8,

        tags$h4("Taxa column selection - select either taxa OR others columns"),
        esquisse::dragulaInput(
          inputId = ns("taxa_cols_selection"),
          label = NULL,
          choices = character(0),
          sourceLabel = "Available variables",
          targetsLabels = c(
            "Taxa",
            "Genus",
            "Species epiteth",
            "Authors",
            "Rank infra-specific level",
            "Name infra-specific level",
            "Authors infra-specific level"
          )
        ),

        tags$h4("Coordinates and altitude column selection"),
        esquisse::dragulaInput(
          inputId = ns("other_cols_selection"),
          label = NULL,
          choices = character(0),
          sourceLabel = "Available variables",
          targetsLabels = c(
            "Longitude (decimal degrees)",
            "Latitude (decimal degrees)",
            "Altitude (m)",
            "Collection year"
          )
        )

      )
    )
  )
}


#' @export
#'
#' @rdname import-data
#'
#'
#' @importFrom shiny moduleServer reactive observeEvent
import_data_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      raw_data <- datamods::import_file_server(id = "import-file")

      observeEvent(raw_data$data(), {
        imported <- raw_data$data()
        esquisse::updateDragulaInput(
          session = session,
          inputId = "taxa_cols_selection",
          choices = names(imported)
        )
        esquisse::updateDragulaInput(
          session = session,
          inputId = "other_cols_selection",
          choices = names(imported)
        )
      })


      return(reactive(NULL))
    }
  )
}

