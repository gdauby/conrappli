
#' Import data from Rainbio Module
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
#' @name module-data-rainbio
#'
#' @importFrom shiny NS fluidRow column checkboxInput
#' @importFrom htmltools tagList
data_import_rainbio_ui <- function(id, from = c("file", "copypaste")) {
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
          i18n("Import a file containing species names:")
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
          i18n("Paste species names below:")
        ),
        datamods::import_copypaste_ui(
          id = ns("copypaste"),
          title = NULL,
          name_field = FALSE
        )
      )
    },
    tags$h4(i18n("Species found")),
    checkboxInput(
      inputId = ns("exact"),
      label = i18n("Show only exact matches"),
      value = TRUE
    ),
    reactable::reactableOutput(outputId = ns("species")),
    tags$br(),
    actionButton(
      inputId = ns("import"),
      label = i18n("Import data for selected species"),
      width = "100%",
      class = "mb-3"
    ),
    uiOutput(outputId = ns("feedback")),
    tags$br(),
    tags$br()
  )
}

#' @export
#'
#' @rdname module-data-rainbio
#'
#' @importFrom shiny moduleServer observeEvent reactive
#' @importFrom utils read.csv
data_import_rainbio_server <- function(id) {
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
      observeEvent(list(species_rv$data, input$exact), {
        req(species_rv$data)
        shinyWidgets::execute_safely({
          species_rv$names <- query_taxa(
            species = species_rv$data[[1]],
            ids = NULL,
            check_syn = TRUE,
            class = NULL,
            exact_match = input$exact
          )
        })
      })

      output$species <- reactable::renderReactable({
        req(species_rv$names, nrow(species_rv$names) > 0)
        reactable::reactable(
          data = species_rv$names,
          selection = "multiple",
          onClick = "select",
          defaultSelected = seq_len(nrow(species_rv$names)),
          compact = TRUE,
          bordered = TRUE,
          defaultPageSize = 5,
          searchable = TRUE,
          theme = reactable_theme()
        )
      })

      observeEvent(input$import, {
        req(species_rv$names)
        index <- reactable::getReactableState("species", "selected")
        if (length(index) < 1) {
          shinyWidgets::show_alert(
            title = i18n("No species specified"),
            text = i18n("You must specify species for which to import data."),
            type = "warning"
          )
        } else {
          shinybusy::show_modal_spinner(
            spin = "fulfilling-bouncing-circle",
            color = "#088A08",
            text = i18n("Retrieving data, please wait...")
          )
          keys <- species_rv$names$idtax_n[index]
          occdata <- shinyWidgets::execute_safely({
            query_rb_taxa(idtax = keys)
          })
          shinybusy::remove_modal_spinner()
          dataset_rv$value <- occdata$extract_all_tax
        }
      })

      output$feedback <- renderUI({
        if (isTruthy(dataset_rv$value)) {
          shinyWidgets::alert(
            status = "success",
            ph("check"), i18n("Data successfully downloaded from Rainbio."),
            actionLink(inputId = session$ns("see_data"), label = tagList(ph("table")))
          )
        }
      })
      observeEvent(
        input$see_data,
        datamods::show_data(dataset_rv$value, title = "Rainbio data", show_classes = FALSE, type = "modal")
      )


      return(reactive(dataset_rv$value))
    }
  )
}
