
#' @title Import data from GBIF Module
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
#' @name module-data-gbif
#'
#' @importFrom shiny NS fluidRow column
#' @importFrom htmltools tagList
#' @importFrom bslib navs_hidden nav_content
data_import_gbif_ui <- function(id) {
  ns <- NS(id)
  tagList(
    datamods::import_file_ui(
      id = ns("file"),
      preview_data = FALSE,
      title = tags$h5(
        "Import a file containing species names or enter species names in box below"
      )
    ),
    tags$div(
      style = "width: 100%; height: 25px; border-bottom: 1px solid #999999; margin-bottom: 35px; text-align: center;",
      tags$span(
        style = "font-size: 30px; background-color: #FFF; padding: 0 10px;",
        "OR"
      )
    ),
    textAreaInput(
      inputId = ns("text"),
      label = "Enter species names (one by row):",
      width = "100%"
    ),
    tags$h4("Species found"),
    checkboxInput(
      inputId = ns("exact"),
      label = "Show only exact matches",
      value = TRUE
    ),
    reactable::reactableOutput(outputId = ns("species")),
    actionButton(
      inputId = ns("import"),
      label = "Import data for selected species",
      width = "100%"
    )
  )
}

#' @export
#'
#' @rdname module-data-import
#'
#' @importFrom shiny moduleServer observeEvent reactive
data_import_gbif_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      species_rv <- reactiveValues(names = NULL)


      data_file_r <- datamods::import_file_server(
        id = "file",
        btn_show_data = FALSE,
        trigger_return = "change"
      )


    }
  )
}
