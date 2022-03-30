
#' @title Data Validation Module
#'
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data-validation
#'
#' @importFrom shiny NS 
#' @importFrom htmltools tagList
data_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    datamods::validation_ui(ns("result"), display = "inline")
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#' 
#' @export
#'
#' @rdname module-data-validation
#' 
#' @importFrom shiny moduleServer observeEvent reactive req
data_validation_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      datamods::validation_server(
        id = "result",
        data = reactive({
          req(data_r())
          prepare_data_validate(data_r())
        }),
        rules = validation_rules(),
        bs_version = 5
      )
      
      
    }
  )
}
