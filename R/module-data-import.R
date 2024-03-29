
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
#' @importFrom bslib navset_hidden nav_panel_hidden
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
            i18n("Polygon"),
            i18n("Dataset")
          ),
          choiceValues = c(
            "gbif",
            "rainbio",
            "polygon",
            "data"
          ),
          direction = "vertical",
          width = "100%"
        )
      ),
      column(
        width = 10,
        bslib::navset_hidden(
          id = ns("navs_type_import"),
          bslib::nav_panel_hidden(
            value = "gbif",
            tags$h3(i18n("Import data from GBIF"), class = "mt-0"),
            tags$div(
              class = "mb-1",
              i18n("Search for"), tags$a("Global Biodiversity Information Facility (GBIF)", href = "https://www.gbif.org/fr/", target = "_blank"),
              i18n("occurrences from taxonomic names, either import a excel or text file containing those names or paste a list of names to search for.")
            ),
            bslib::navset_card_pill(
              header = tags$br(),
              bslib::nav_panel(
                title = i18n("From file"),
                data_import_gbif_ui(id = ns("gbif_file"), from = "file")
              ),
              bslib::nav_panel(
                title = i18n("From copy/paste"),
                data_import_gbif_ui(id = ns("gbif_copypaste"), from = "copypaste")
              )
            )
          ),
          bslib::nav_panel_hidden(
            value = "rainbio",
            tags$h3(i18n("Import data from Rainbio database"), class = "mt-0"),
            tags$div(
              class = "mb-1",
              i18n("Extract from the Rainbio database of all records from a species list, either import a excel or text file containing those species names or paste a list of names to search for.")
            ),
            bslib::navset_card_pill(
              header = tags$br(),
              bslib::nav_panel(
                title = i18n("From file"),
                data_import_rainbio_ui(id = ns("rainbio_file"), from = "file")
              ),
              bslib::nav_panel(
                title = i18n("From copy/paste"),
                data_import_rainbio_ui(id = ns("rainbio_copypaste"), from = "copypaste")
              )
            )
          ),
          bslib::nav_panel_hidden(
            value = "polygon",
            tags$h3(i18n("Import data from polygon"), class = "mt-0"),
            data_import_polygon_ui(id = ns("polygon"))
          ),
          bslib::nav_panel_hidden(
            value = "data",
            tags$h3(i18n("Import data from a local file"), class = "mt-0"),
            tags$div(
              class = "mb-1",
              i18n("Use a ready-to-use dataset, either from an Excel or CSV file, or directly copied and pasted from a spreadsheet.")
            ),
            bslib::navset_card_pill(
              header = tags$br(),
              bslib::nav_panel(
                title = i18n("From file"),
                datamods::import_file_ui(id = ns("file"), title = NULL)
              ),
              bslib::nav_panel(
                title = i18n("From copy/paste"),
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

      raw_data_polygon <- data_import_polygon_server(
        id = "polygon"
      )
      observeEvent(raw_data_polygon$value(), {
        dataset_rv$value <- raw_data_polygon$value()
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
