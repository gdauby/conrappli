
#' Import data from Rainbio via polygon drawing Module
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data-polygon
#'
#' @importFrom shiny NS fluidRow column checkboxInput
#' @importFrom htmltools tagList
#' @importFrom bslib navs_hidden nav_content
data_import_polygon_ui <- function(id) {
  ns <- NS(id)
  tagList(
    draw_poly_ui(id = ns("polygon")),
    uiOutput(outputId = ns("feedback")),
    reactable::reactableOutput(outputId = ns("table")),
    tags$br(),
    tags$br()
  )
}

#' @export
#'
#' @rdname module-data-polygon
#'
#' @importFrom shiny moduleServer observeEvent reactive
#' @importFrom utils read.csv
data_import_polygon_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      dataset_rv <- reactiveValues(value = NULL)

      polygon_r <- draw_poly_server(id = "polygon")

      observeEvent(polygon_r(), {
        req(polygon_r())
        shinybusy::show_modal_spinner(
          spin = "fulfilling-bouncing-circle",
          color = "#088A08",
          text = "Retrieving data, please wait..."
        )
        occdata <- shinyWidgets::execute_safely({
          query_rb_poly(poly = polygon_r())
        })
        shinybusy::remove_modal_spinner()
        dataset_rv$value <- occdata$extract_all_tax
      })

      output$feedback <- renderUI({
        if (isTruthy(dataset_rv$value)) {
          shinyWidgets::alert(
            status = "success",
            icon("check"),
            "Data successfully downloaded from Rainbio. See below, max first 100 lines displayed."
          )
        }
      })

      output$table <- reactable::renderReactable({
        req(dataset_rv$value)
        reactable::reactable(
          data = head(dataset_rv$value, 1000),
          compact = TRUE,
          bordered = TRUE,
          defaultPageSize = 10,
          searchable = TRUE
        )
      })


      return(reactive(dataset_rv$value))
    }
  )
}
