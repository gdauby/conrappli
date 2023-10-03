
#' Display data Module
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
#' @importFrom shiny NS downloadButton
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
#' @importFrom shiny moduleServer req downloadHandler
#' @importFrom utils write.csv
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
        if (hasName(data_r(), "STATUS_CONR")) {
          columns <- list(
            STATUS_CONR = reactable::colDef(
              name = "Status ConR",
              cell = function(value, index) {
                if (value == "OUT") "\u274c Out" else "\u2714\ufe0f In"
              }
            ),
            STATUS_DESC = reactable::colDef(name = "Status description")
          )
        } else {
          columns <- NULL
        }
        reactable::reactable(
          data = data_r() %>%
            unselect_internal_vars(),
          defaultColDef =  reactable::colDef(minWidth = 150),
          bordered = TRUE,
          compact = TRUE,
          columns = columns,
          searchable = TRUE,
          theme = reactable_theme(
            searchInputStyle = list(
              paddingLeft = "1.9rem",
              paddingTop = "0.5rem",
              paddingBottom = "0.5rem",
              width = "100%",
              border = "none",
              backgroundColor = "hsl(0, 0%, 10%)",
              backgroundSize = "1rem",
              backgroundPosition = "left 0.5rem center",
              backgroundRepeat = "no-repeat",
              "&:focus" = list(backgroundColor = "rgba(255, 255, 255, 0.1)", border = "none"),
              "::placeholder" = list(color = "#dfe2e5"),
              "&:hover::placeholder, &:focus::placeholder" = list(color = "#FFF")
            )
          )
        )
      })

    }
  )
}
