
#' Import data Module
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data-import
#'
#' @importFrom shiny NS fluidRow column
#' @importFrom htmltools tagList
#' @importFrom bslib navs_hidden nav_content
data_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 2,
        shinyWidgets::radioGroupButtons(
          inputId = ns("type_import"),
          label = NULL,
          choiceNames = c(
            "GBIF",
            "Rainbio",
            "Dataset"
          ),
          choiceValues = c(
            "gbif",
            "rainbio",
            "data"
          ),
          direction = "vertical",
          width = "100%"
        )
      ),
      column(
        width = 10,
        bslib::navs_hidden(
          id = ns("navs_type_import"),
          bslib::nav_content(
            value = "gbif",
            bslib::navs_pill(
              header = tags$br(),
              bslib::nav(
                title = "From file",
                data_import_gbif_ui(id = ns("gbif_file"), from = "file")
              ),
              bslib::nav(
                title = "From copy/paste",
                data_import_gbif_ui(id = ns("gbif_copypaste"), from = "copypaste")
              )
            )
          ),
          bslib::nav_content(
            value = "rainbio",
            bslib::navs_pill(
              header = tags$br(),
              bslib::nav(
                title = "From file",
                data_import_rainbio_ui(id = ns("rainbio_file"), from = "file")
              ),
              bslib::nav(
                title = "From copy/paste",
                data_import_rainbio_ui(id = ns("rainbio_copypaste"), from = "copypaste")
              )
            )
          ),
          bslib::nav_content(
            value = "data",
            bslib::navs_pill(
              header = tags$br(),
              bslib::nav(
                title = "From file",
                datamods::import_file_ui(id = ns("file"), title = NULL)
              ),
              bslib::nav(
                title = "From copy/paste",
                datamods::import_copypaste_ui(id = ns("copypaste"), title = NULL)
              )
            )
          )
        )
      )
    )
  )
}

#' @export
#'
#' @rdname module-data-import
#'
#' @importFrom shiny moduleServer observeEvent reactive
data_import_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      dataset_rv <- reactiveValues(value = NULL)

      observeEvent(input$type_import, nav_select("navs_type_import", input$type_import))

      raw_data_gbif_file <- data_import_gbif_server(
        id = "gbif_file"
      )
      observeEvent(raw_data_gbif_file(), {
        dataset_rv$value <- raw_data_gbif_file()
      })

      raw_data_gbif_copypaste <- data_import_gbif_server(
        id = "gbif_copypaste"
      )
      observeEvent(raw_data_gbif_copypaste(), {
        dataset_rv$value <- raw_data_gbif_copypaste()
      })

      raw_data_rainbio_file <- data_import_rainbio_server(
        id = "rainbio_file"
      )
      observeEvent(raw_data_rainbio_file(), {
        dataset_rv$value <- raw_data_rainbio_file()
      })

      raw_data_rainbio_copypaste <- data_import_rainbio_server(
        id = "rainbio_copypaste"
      )
      observeEvent(raw_data_rainbio_copypaste(), {
        dataset_rv$value <- raw_data_rainbio_copypaste()
      })

      raw_data_file <- datamods::import_file_server(
        id = "file",
        trigger_return = "change",
        show_data_in = "modal"
      )
      observeEvent(raw_data_file$data(), {
        dataset_rv$value <- raw_data_file$data()
      })

      raw_data_copypaste <- datamods::import_copypaste_server(
        id = "copypaste",
        trigger_return = "change",
        show_data_in = "modal"
      )
      observeEvent(raw_data_copypaste$data(), {
        dataset_rv$value <- raw_data_copypaste$data()
      })

      return(reactive(dataset_rv$value))
    }
  )
}
