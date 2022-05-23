
#' Import data from GBIF Module
#'
#' @param id Module's ID.
#' @param from Type of data input.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data-gbif
#'
#' @importFrom shiny NS fluidRow column checkboxInput
#' @importFrom htmltools tagList
#' @importFrom bslib navs_hidden nav_content
data_import_gbif_ui <- function(id, from = c("file", "copypaste")) {
  ns <- NS(id)
  from <- match.arg(from)
  tagList(
    tags$style(sprintf(
      "#%s {display: none;}",
      ns("file-import-result")
    )),
    if (identical(from, "file")) {
      tagList(
        tags$h5(
          "Import a file containing species names:"
        ),
        datamods::import_file_ui(
          id = ns("file"),
          preview_data = FALSE,
          title = NULL
        )
      )
    },
    if (identical(from, "copypaste")) {
      tagList(
        tags$h5(
          "Paste species names below:"
        ),
        datamods::import_copypaste_ui(
          id = ns("copypaste"),
          title = NULL,
          name_field = FALSE
        )
      )
    },
    tags$h4("Species found"),
    checkboxInput(
      inputId = ns("exact"),
      label = "Show only exact matches",
      value = TRUE
    ),
    reactable::reactableOutput(outputId = ns("species")),
    tags$br(),
    actionButton(
      inputId = ns("import"),
      label = "Import data for selected species",
      width = "100%",
      class = "mb-3"
    ),
    uiOutput(outputId = ns("feedback_gbif")),
    tags$br(),
    tags$br()
  )
}

#' @export
#'
#' @rdname module-data-import
#'
#' @importFrom shiny moduleServer observeEvent reactive
#' @importFrom utils read.csv
data_import_gbif_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      dataset_rv <- reactiveValues(value = NULL)
      species_rv <- reactiveValues(names = data.frame(name = character(0)), data = NULL)

      species_file_r <- datamods::import_file_server(
        id = "file",
        btn_show_data = FALSE,
        trigger_return = "change",
        read_fns = list(
          csv = function(file, sheet, skip, encoding) {
            read.csv(file = file, encoding = encoding, skip = skip)
          }
        )
      )
      observeEvent(species_file_r$data(), {
        x <- species_file_r$data()
        x[[1]] <- apply(x, MARGIN = 1, FUN = paste, collapse = " ")
        species_rv$data <- x
      })


      species_copypaste_r <- datamods::import_copypaste_server(
        id = "copypaste",
        btn_show_data = FALSE,
        trigger_return = "change",
        fread_args = list(header = FALSE, fill = TRUE, sep = NULL)
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

      species_names_r <- reactive({
        data <- req(species_rv$names, nrow(species_rv$names) > 0)
        if (isTRUE(input$exact)) {
          data <- dplyr::filter(data, matchtype == "EXACT" & status == "ACCEPTED")
        }
        data
      })

      output$species <- reactable::renderReactable({
        reactable::reactable(
          data = species_names_r(),
          selection = "multiple",
          onClick = "select",
          defaultSelected = seq_len(nrow(species_names_r())),
          compact = TRUE,
          bordered = TRUE,
          defaultPageSize = 5,
          searchable = TRUE
        )
      })

      observeEvent(input$import, {
        req(species_names_r())
        index <- reactable::getReactableState("species", "selected")
        if (length(index) < 1) {
          shinyWidgets::show_alert(
            title = "No species specified",
            text = "You must specify species for which to import data.",
            type = "warning"
          )
        } else {
          shinybusy::show_modal_spinner(
            spin = "fulfilling-bouncing-circle",
            color = "#088A08",
            text = "Retrieving data, please wait..."
          )
          keys <- species_names_r()$specieskey[index]
          occdata <- shinyWidgets::execute_safely({
            retrieve_occ_data(keys)
          })
          shinybusy::remove_modal_spinner()
          dataset_rv$value <- occdata
        }
      })

      output$feedback_gbif <- renderUI({
        if (isTruthy(dataset_rv$value)) {
          shinyWidgets::alert(
            status = "success",
            icon("check"), "Data successfully downloaded from GBIF.",
            actionLink(inputId = session$ns("see_data"), label = tagList(icon("table")))
          )
        }
      })
      observeEvent(
        input$see_data,
        datamods::show_data(dataset_rv$value, title = "GBIF data", show_classes = FALSE, type = "modal")
      )


      return(reactive(dataset_rv$value))
    }
  )
}
