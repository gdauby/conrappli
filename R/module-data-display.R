
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
    DT::DTOutput(outputId = ns("table"))
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

      output$table <- DT::renderDT({
        req(data_r())
        DT::datatable(
          data = data_r() %>%
            unselect_internal_vars(),
          class = "display compact cell-border",
          selection = "none",
          rownames = FALSE
        )
      })

    }
  )
}
