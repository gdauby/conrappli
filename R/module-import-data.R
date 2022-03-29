#' @title Import Data Module
#'
#' @description This module allow to import data into the application,
#'  to map input's columns to variable of interest for the simulation
#'  and to validate that all is in order to proceed.
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name import-data
#'
#' @importFrom shiny NS fluidRow column uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom bslib navs_pill_card nav
import_data_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Import data",

    navs_pill_card(
      id = ns("navs"),
      nav(
        title = "Import dataset",
        value = "import_dataset",
        datamods::import_file_ui(id = ns("import-file"), title = NULL),
        uiOutput(outputId = ns("btn_nav_import_dataset"))
      ),
      nav(
        title = "Variable selection",
        value = "variable_selection",
        tags$h5("Taxa column selection:"),
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
          ),
          ncolGrid = 3,
          replace = TRUE
        ),
        uiOutput(outputId = ns("feedback_sel_taxa")),

        tags$h5("Coordinates and altitude column selection:"),
        esquisse::dragulaInput(
          inputId = ns("other_cols_selection"),
          label = NULL,
          choices = character(0),
          sourceLabel = "Available variables",
          targetsLabels = c(
            "Longitude",
            "Latitude",
            "Altitude (m)",
            "Collection year"
          ),
          replace = TRUE
        ),
        uiOutput(outputId = ns("feedback_sel_other"))
      ),
      nav(
        title = "Data validation",
        value = "data_validation"
      )
    )
  )
}


#' @export
#'
#' @rdname import-data
#'
#'
#' @importFrom shiny moduleServer reactive reactiveValues
#'  observeEvent renderUI actionButton icon
#' @importFrom bslib nav_select
import_data_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns

      dataset_rv <- reactiveValues(value = NULL)

      raw_data_file <- datamods::import_file_server(id = "import-file", trigger_return = "change")

      observeEvent(raw_data_file$data(), {
        dataset_rv$value <- raw_data_file$data()
      })


      output$btn_nav_import_dataset <- renderUI({
        if (is.data.frame(dataset_rv$value)) {
          actionButton(
            inputId = ns("go_to_variable_selection"),
            label = "Go to variable selection",
            icon = icon("arrow-circle-right"),
            class = "float-end"
          )
        }
      })
      observeEvent(input$go_to_variable_selection, nav_select("navs", "variable_selection"))

      observeEvent(dataset_rv$value, {
        imported <- dataset_rv$value
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

      output$feedback_sel_taxa <- renderUI({
        var_sel <- input$taxa_cols_selection$target
        vars_other <- c(
          "Genus",
          "Species epiteth",
          "Authors",
          "Rank infra-specific level",
          "Name infra-specific level",
          "Authors infra-specific level"
        )
        if (!is.null(var_sel[["Taxa"]]) | all(lengths(var_sel[vars_other]) > 0)) {
          tags$div()
        } else {
          shinyWidgets::alert(
            status = "info",
            icon("info-circle"), "Select either taxa OR others columns."
          )
        }
      })

      output$feedback_sel_other <- renderUI({
        shinyWidgets::alert(
          status = "info",
          icon("info-circle"), "Longitude and latitude are required."
        )
      })


      return(reactive(NULL))
    }
  )
}

