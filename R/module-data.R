#' @title Data Module
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
#' @name module-data
#'
#' @importFrom shiny NS fluidRow column uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom bslib navs_pill navs_hidden nav nav_content
data_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Import & validate data",

    navs_pill(
      id = ns("navs"),
      header = tags$hr(),
      nav(
        title = "Import dataset",
        value = "import_dataset",
        fluidRow(
          column(
            width = 3,
            shinyWidgets::radioGroupButtons(
              inputId = ns("type_import"),
              label = NULL,
              choices = c("From file", "From copy/paste"),
              direction = "vertical",
              width = "100%"
            )
          ),
          column(
            width = 9,
            navs_hidden(
              id = ns("navs_type_import"),
              nav_content(
                value = "From file",
                datamods::import_file_ui(id = ns("file"), title = NULL)
              ),
              nav_content(
                value = "From copy/paste",
                datamods::import_copypaste_ui(id = ns("copypaste"), title = NULL)
              )
            )
          )
        ),
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
        uiOutput(outputId = ns("feedback_sel_other")),
        uiOutput(outputId = ns("btn_nav_variable_selection"))
      ),
      nav(
        title = "Data validation",
        value = "data_validation"
      ),
      nav(
        title = "Map",
        value = "map"
      )
    )
  )
}


#' @export
#'
#' @rdname module-data
#'
#'
#' @importFrom shiny moduleServer reactive reactiveValues
#'  observeEvent renderUI actionButton icon
#' @importFrom bslib nav_select
data_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns

      # Data Import ---- 
      
      dataset_rv <- reactiveValues(value = NULL)

      observeEvent(input$type_import, nav_select("navs_type_import", input$type_import))
      raw_data_file <- datamods::import_file_server(id = "file", trigger_return = "change")
      raw_data_copypaste <- datamods::import_copypaste_server(id = "copypaste", trigger_return = "change")

      observeEvent(raw_data_file$data(), {
        dataset_rv$value <- raw_data_file$data()
      })
      observeEvent(raw_data_copypaste$data(), {
        dataset_rv$value <- raw_data_copypaste$data()
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

      
      
      # Variable Selection ---- 
      
      var_sel_rv <- reactiveValues(taxa = FALSE, other  = FALSE)
      
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
      
      observeEvent(input$taxa_cols_selection$target, {
        var_sel <- input$taxa_cols_selection$target
        vars_other <- c(
          "Genus",
          "Species epiteth",
          "Authors",
          "Rank infra-specific level",
          "Name infra-specific level",
          "Authors infra-specific level"
        )
        var_sel_rv$taxa <- !is.null(var_sel[["Taxa"]]) | all(lengths(var_sel[vars_other]) > 0)
      })
      output$feedback_sel_taxa <- renderUI({
        if (isTRUE(var_sel_rv$taxa)) {
          tags$div()
        } else {
          shinyWidgets::alert(
            status = "info",
            icon("info-circle"), "Select either taxa OR others columns."
          )
        }
      })
      
      
      observeEvent(input$other_cols_selection$target, {
        var_oth <- input$other_cols_selection$target
        var_sel_rv$other <- !is.null(var_oth[["Longitude"]]) & !is.null(var_oth[["Latitude"]])
      })
      output$feedback_sel_other <- renderUI({
        if (isTRUE(var_sel_rv$other)) {
          tags$div()
        } else {
          shinyWidgets::alert(
            status = "info",
            icon("info-circle"), "Longitude and latitude are required."
          )
        }
      })
      
      
      output$btn_nav_variable_selection <- renderUI({
        if (isTRUE(var_sel_rv$taxa) & isTRUE(var_sel_rv$other)) {
          actionButton(
            inputId = ns("go_to_data_validation"),
            label = "Go to data validation",
            icon = icon("arrow-circle-right"),
            class = "float-end"
          )
        }
      })
      observeEvent(input$go_to_data_validation, nav_select("navs", "data_validation"))


      return(reactive(NULL))
    }
  )
}

