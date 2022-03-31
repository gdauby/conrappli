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
#' @importFrom shiny NS uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom bslib navs_pill nav
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
        data_import_ui(ns("import")),
        uiOutput(outputId = ns("btn_nav_import_dataset"))
      ),
      nav(
        title = "Variable selection",
        value = "variable_selection",
        data_variable_ui(ns("variable")),
        uiOutput(outputId = ns("btn_nav_variable_selection"))
      ),
      nav(
        title = "Data validation",
        value = "data_validation",
        shinyWidgets::alert(
          status = "info",
          class = "alert-no-data-no-variables",
          icon("info-circle"), "You need to import data and select variable."
        ),
        data_validation_ui(ns("validation")),
        uiOutput(outputId = ns("btn_nav_data_validation"))
      ),
      nav(
        title = "Map",
        value = "map",
        shinyWidgets::alert(
          status = "info",
          class = "alert-no-data-no-variables",
          icon("info-circle"), "You need to import data and select variable."
        ),
        data_map_ui(ns("map"))
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

      data_r <- data_import_server("import")

      output$btn_nav_import_dataset <- renderUI({
        shinyjs::show(selector = ".alert-no-data-no-variables")
        req(data_r())
        if (is.data.frame(data_r())) {
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

      variable_r <- data_variable_server("variable", data_r = data_r)

      output$btn_nav_variable_selection <- renderUI({
        vars <- variable_r()
        if (isTRUE(vars$taxa) & isTRUE(vars$other)) {
          shinyjs::hide(selector = ".alert-no-data-no-variables")
          actionButton(
            inputId = ns("go_to_data_validation"),
            label = "Go to data validation",
            icon = icon("arrow-circle-right"),
            class = "float-end"
          )
        }
      })
      observeEvent(input$go_to_data_validation, nav_select("navs", "data_validation"))



      # Data validation ----

      data_validated_r <- data_validation_server(
        id = "validation",
        data_r = reactive({
          input$navs
          variable_r()$data
        })
      )
      output$btn_nav_data_validation <- renderUI({
        validated <- data_validated_r()
        if (nrow(validated) > 0) {
          actionButton(
            inputId = ns("go_to_map"),
            label = "Go to mapping",
            icon = icon("arrow-circle-right"),
            class = "float-end"
          )
        }
      })
      observeEvent(input$go_to_map, nav_select("navs", "map"))



      # Map validation ----

      data_map_server(
        id = "map",
        data_r = data_validated_r
      )

      return(reactive(NULL))
    }
  )
}

