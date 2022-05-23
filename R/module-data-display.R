
#' @title Display data Module
#'
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: No value.

#'
#' @name module-data-display
#'
#' @importFrom shiny NS
#' @importFrom htmltools tagList
data_display_ui <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(outputId = ns("download"), label = "Download data", class = "float-end mb-3"),
    tags$div(class = "clearfix"),
    reactable::reactableOutput(outputId = ns("table"))
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#'
#'
#' @export
#'
#' @rdname module-data-display
#'
#' @importFrom shiny moduleServer req
data_display_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$download <- downloadHandler(
        filename = function() {
          "conr-data.csv"
        },
        content = function(file) {
          data_r() %>%
            unselect_internal_vars() %>%
            write.csv(file, row.names = FALSE)
        }
      )

      output$table <- reactable::renderReactable({
        req(data_r())
        reactable::reactable(
          data = data_r() %>%
            unselect_internal_vars(),
          bordered = TRUE,
          compact = TRUE,
          columns = list(
            STATUS_CONR = colDef(name = "Status ConR", cell = function(value, index) {
              if (value == "OUT") "\u274c Out" else "\u2714\ufe0f In"
            })
          )
        )
      })

    }
  )
}
