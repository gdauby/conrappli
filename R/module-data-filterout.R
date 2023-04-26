
#' Data filtering out Module
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
data_filterout_ui <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(
      inputId = ns("max_occ_filterout"),
      label = tagList(
        "Threshold to filter out taxa with high number of occurrences",
        btn_help(
          "This is a threshold to filter out taxa with more than X unique pair of coordinates. All taxa with number of unique coordinates above this threshold will not be considerd saving computatoion time. Beware that a low number may discard taxa that are actually threatened."
        )
      ),
      min = 20,
      max = 2000,
      value = 100,
      step = 1,
      width = "100%"
    )
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-data-validation
#'
#' @importFrom shiny moduleServer reactive req
data_filterout_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      to_filter_r <- reactive({
        req(data_r(), nrow(data_r()) > 0)
        filtering_out_data(
          data = data_r(),
          threshold = input$max_occ_filterout
        )
      })
      
      return(to_filter_r)
    }
  )
}
