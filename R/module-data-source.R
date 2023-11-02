
#' Data source Module
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data-source
#'
#' @importFrom shiny NS numericInput
#' @importFrom htmltools tagList
data_source_ui <- function(id) {
  ns <- NS(id)
  tagList(
   shinyWidgets::awesomeRadio(
     inputId = ns("source_data"), 
     label = tagList(
       i18n("Database source"),
       btn_help(
         i18n("")
       )
     ), 
     choices = c("RAINBIO", "GBIF"), 
     selected = "RAINBIO", 
     status = "info"
   )
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-data-source
#'
#' @importFrom shiny moduleServer reactive req
data_source_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      source_data_r <- reactive({
        input$source_data
      })
      
      return(source_data_r)
    }
  )
}
