
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
    tags$style(sprintf(
      "#%s {display: none;}",
      ns("file-import-result")
    )),
    tags$h5(
      "Import a file containing species names or enter species names in box below"
    ),
    datamods::import_file_ui(
      id = ns("file"),
      preview_data = FALSE,
      title = NULL
    ),
    tags$div(
      style = "width: 100%; height: 25px; border-bottom: 1px solid #dee2e6; margin-bottom: 35px; text-align: center;",
      tags$span(
        style = "font-size: 30px; color: #dee2e6; background-color: #FFF; padding: 0 10px;",
        "OR"
      )
    ),
    datamods::import_copypaste_ui(
      id = ns("copypaste"),
      title = NULL,
      name_field = FALSE
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

      species_rv <- reactiveValues(names = data.frame(name = character(0)), data = NULL)

      species_file_r <- datamods::import_file_server(
        id = "file",
        btn_show_data = FALSE,
        trigger_return = "button"
      )
      observeEvent(species_file_r$data(), {
        species_rv$data <- species_file_r$data()
      })


      species_copypaste_r <- datamods::import_copypaste_server(
        id = "copypaste",
        btn_show_data = FALSE,
        trigger_return = "button",
        fread_args = list(header = FALSE)
      )
      observeEvent(species_copypaste_r$data(), {
        x <- species_copypaste_r$data()
        x[[1]] <- apply(x, MARGIN = 1, FUN = paste, collapse = " ")
        species_rv$data <- x
      })


      # search species names
      observeEvent(species_rv$data, {
        shinyWidgets::execute_safely({
          species_rv$names <- search_species_info(species_rv$data[[1]])
        })
      })

      output$species <- reactable::renderReactable({
        reactable::reactable(
          data = species_rv$names,
          selection = "multiple",
          defaultSelected = seq_len(nrow(species_rv$names)),
          compact = TRUE,
          bordered = TRUE
        )
      })

    }
  )
}
