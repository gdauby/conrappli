
#' @title Variable selection Module
#'
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `list`.

#'
#' @name module-data-variable
#'
#' @importFrom shiny NS uiOutput
#' @importFrom htmltools tagList tags
data_variable_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h5(
      "Taxa column selection:",
      btn_help(
        "Select either a column identifying species or fill all other columns to contruct a taxa column.",
        class = "float-right"
      )
    ),
    esquisse::dragulaInput(
      inputId = ns("taxa_cols"),
      label = NULL,
      choices = character(0),
      sourceLabel = "Available variables",
      targetsLabels = taxa_cols("label"),
      targetsIds = taxa_cols("id"),
      ncolGrid = 3,
      replace = TRUE
    ),
    uiOutput(outputId = ns("feedback_sel_taxa")),

    tags$h5(
      "Coordinates and altitude column selection:",
      btn_help(
        "Latitude and longitude are required for analyse, altitude and year are optionnal.",
        class = "float-right"
      )
    ),
    esquisse::dragulaInput(
      inputId = ns("other_cols"),
      label = NULL,
      choices = character(0),
      sourceLabel = "Available variables",
      targetsLabels = other_cols("label"),
      targetsIds = other_cols("id"),
      replace = TRUE
    ),
    uiOutput(outputId = ns("feedback_sel_other")),

    tags$h5(
      "Other columns of interest:",
      btn_help(
        "Thos columns won't be used in analysis but they will be kept with the data, others columns will be dropped..",
        class = "float-right"
      )
    ),
    esquisse::dragulaInput(
      inputId = ns("optionnal_cols"),
      label = NULL,
      choices = character(0),
      sourceLabel = "Available variables",
      targetsLabels = "Variables to keep",
      targetsIds = "keep"
    )
  )
}


#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-data-variable
#'
#' @importFrom shiny moduleServer observeEvent reactiveValues
#'  reactive reactiveValuesToList renderUI req bindEvent observe isTruthy
#'
data_variable_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      var_sel_rv <- reactiveValues(taxa = FALSE, other  = FALSE)

      observeEvent(data_r(), {
        imported <- data_r()
        esquisse::updateDragulaInput(
          session = session,
          inputId = "taxa_cols",
          choices = names(imported),
          selected = auto_selection_cols_taxa(imported)
        )
        esquisse::updateDragulaInput(
          session = session,
          inputId = "other_cols",
          choices = names(imported),
          selected = auto_selection_cols_other(imported)
        )
        esquisse::updateDragulaInput(
          session = session,
          inputId = "optionnal_cols",
          choices = names(imported),
          selected = auto_selection_cols_optionnal(imported)
        )
      })

      observeEvent(input$taxa_cols$target, {
        var_sel <- input$taxa_cols$target
        vars_other <- c(
          "Genus",
          "Species epiteth",
          "Authors",
          "Rank infra-specific level",
          "Name infra-specific level",
          "Authors infra-specific level"
        )
        var_sel_rv$taxa <- !is.null(var_sel[[".__taxa"]]) | all(lengths(var_sel[vars_other]) > 0)
      })
      output$feedback_sel_taxa <- renderUI({
        if (isTRUE(var_sel_rv$taxa)) {
          tags$div()
        } else {
          shinyWidgets::alert(
            status = "info",
            icon("info-circle"), "Select either taxa OR others columns."
          )
        }
      })


      observeEvent(input$other_cols$target, {
        var_oth <- input$other_cols$target
        var_sel_rv$other <- !is.null(var_oth[[".__longitude"]]) & !is.null(var_oth[[".__latitude"]])
      })
      output$feedback_sel_other <- renderUI({
        if (isTRUE(var_sel_rv$other)) {
          tags$div()
        } else {
          shinyWidgets::alert(
            status = "info",
            icon("info-circle"), "Longitude and latitude are required."
          )
        }
      })

      observe({
        if (isTruthy(data_r()) & isTRUE(var_sel_rv$other) & isTRUE(var_sel_rv$taxa)) {
          vars <- dropNulls(c(input$taxa_cols$target, input$other_cols$target))
          var_sel_rv$vars <- vars
          allvars <- dropNulls(c(input$taxa_cols$target, input$other_cols$target, input$optionnal_cols$target))
          allvars <- unlist(allvars, recursive = TRUE, use.names = FALSE)
          var_sel_rv$data <- dplyr::bind_cols(
            dplyr::select(data_r(), dplyr::any_of(allvars)),
            dplyr::select(data_r(), !!!vars)
          )
        }
      })

      return(reactive(reactiveValuesToList(var_sel_rv)))
    }
  )
}


taxa_cols <- function(x = NULL) {
  cols <- list(
    label = c(
      "Taxa",
      "Genus",
      "Species epiteth",
      "Rank infra-specific level",
      "Name infra-specific level"
    ),
    id = c(
      ".__taxa",
      ".__genus",
      ".__species_epiteth",
      ".__rank_infra_specific_level",
      ".__name_infra_specific_level"
    )
  )
  if (!is.null(x))
    cols <- cols[[x]]
  return(cols)
}

other_cols <- function(x = NULL) {
  cols <- list(
    label = c(
      "Longitude",
      "Latitude",
      "Altitude (m)",
      "Collection year"
    ),
    id = c(
      ".__longitude",
      ".__latitude",
      ".__altitude",
      ".__year"
    )
  )
  if (!is.null(x))
    cols <- cols[[x]]
  return(cols)
}


auto_selection_cols_taxa <- function(.data) {
  x <- list()
  if (hasName(.data, "scientificName"))
    x[[".__taxa"]] <- "scientificName"
  if (hasName(.data, "genus"))
    x[[".__genus"]] <- "genus"
  if (hasName(.data, "specificEpithet"))
    x[[".__species_epiteth"]] <- "specificEpithet"
  if (hasName(.data, "taxonRank"))
    x[[".__rank_infra_specific_level"]] <- "taxonRank"
  if (hasName(.data, "infraspecificEpithet"))
    x[[".__name_infra_specific_level"]] <- "infraspecificEpithet"
  if (length(x) < 1)
    return(NULL)
  x
}


auto_selection_cols_other <- function(.data) {
  x <- list()
  if (hasName(.data, "decimalLongitude"))
    x[[".__longitude"]] <- "decimalLongitude"
  if (hasName(.data, "decimalLatitude"))
    x[[".__latitude"]] <- "decimalLatitude"
  if (hasName(.data, "elevation"))
    x[[".__altitude"]] <- "elevation"
  if (hasName(.data, "year"))
    x[[".__year"]] <- "year"
  if (length(x) < 1)
    return(NULL)
  x
}


auto_selection_cols_optionnal <- function(.data) {
  x <- list()
  vars <- c(
    "recordedBy",
    "recordNumber",
    "locality",
    "verbatimLocality",
    "country",
    "habitat",
    "identifiedBy",
    "dateIdentified",
    "occurrenceRemarks",
    "fieldNotes",
    "eventRemarks",
    "iucnRedListCategory"
  )

  for (variable in vars) {
    if (hasName(.data, variable))
      x[["keep"]] <- c(x[["keep"]], variable)
  }
  if (length(x) < 1)
    return(NULL)
  x
}
