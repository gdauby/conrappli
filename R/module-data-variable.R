
#' @title Variable selection Module
#'
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `list`.

#'
#' @name module-data-variable
#'
#' @importFrom shiny NS uiOutput
#' @importFrom htmltools tagList tags 
data_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h5("Taxa column selection:"),
    esquisse::dragulaInput(
      inputId = ns("taxa_cols"),
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
      inputId = ns("other_cols"),
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
  )
}


#' @param data_r A `reactive` function returning a `data.frame`.
#' 
#' @export
#'
#' @rdname module-data-variable
#' 
#' @importFrom shiny moduleServer observeEvent reactiveValues
#'  reactive reactiveValuesToList renderUI req bindEvent observe isTruthy
#'
data_variable_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      var_sel_rv <- reactiveValues(taxa = FALSE, other  = FALSE)
      
      observeEvent(data_r(), {
        imported <- data_r()
        esquisse::updateDragulaInput(
          session = session,
          inputId = "taxa_cols",
          choices = names(imported)
        )
        esquisse::updateDragulaInput(
          session = session,
          inputId = "other_cols",
          choices = names(imported)
        )
      })
      
      observeEvent(input$taxa_cols$target, {
        var_sel <- input$taxa_cols$target
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
      
      
      observeEvent(input$other_cols$target, {
        var_oth <- input$other_cols$target
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
      
      bindEvent(observe({
        if (isTruthy(data_r()) & isTRUE(var_sel_rv$other) & isTRUE(var_sel_rv$taxa)) {
          imported <- data_r()
          var_sel <- c(input$taxa_cols$target, input$other_cols$target)
          var_sel_rv$data <- dplyr::select(imported, !!!var_sel)
        }
      }), input$taxa_cols$target, input$other_cols$target)
      
      return(reactive(reactiveValuesToList(var_sel_rv)))
    }
  )
}
