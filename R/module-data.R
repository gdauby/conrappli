#' @title Data Module
#'
#' @description This module allow to import data into the application,
#'  to map input's columns to variable of interest for the simulation
#'  and to validate that all is in order to proceed.
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data
#'
#' @importFrom shiny NS uiOutput
#' @importFrom htmltools tagList tags
#' @importFrom bslib navs_pill nav nav_spacer
#' @importFrom phosphoricons ph_i html_dependency_phosphor
data_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Import & validate data",

    html_dependency_phosphor(),

    actionButton(
      inputId = ns("go_next"),
      label = tagList(
        "You must import a dataset and select variables before proceeding to the next step.",
        ph("arrow-circle-right")
      ),
      class = "btn-outline-primary my-3",
      disabled = "disabled",
      width = "100%",
      onclick = "$(\"a[data-value='mapping']\").click();"
    ),

    navs_pill(
      id = ns("navs"),
      header = tags$hr(),
      nav(
        title = "Import dataset",
        value = "import_dataset",
        icon = ph_i("file-arrow-up", style = "vertical-align: -0.3em;"),
        data_import_ui(ns("import")),
        uiOutput(outputId = ns("btn_nav_import_dataset"))
      ),
      nav(
        title = "Variable selection",
        value = "variable_selection",
        icon = ph_i("table", style = "vertical-align: -0.3em;"),
        data_variable_ui(ns("variable")),
        uiOutput(outputId = ns("btn_nav_variable_selection"))
      ),
      nav(
        title = "Data validation",
        value = "data_validation",
        icon = ph_i("check", style = "vertical-align: -0.3em;"),
        shinyWidgets::alert(
          status = "info",
          class = "alert-no-data-no-variables",
          ph("info"), "You need to import data and select variable."
        ),
        data_validation_ui(ns("validation")),
        uiOutput(outputId = ns("btn_nav_data_validation"))
      ),
      nav_spacer(),
      nav(
        title = "Data",
        value = "data",
        icon = ph_i("database", style = "vertical-align: -0.3em;"),
        shinyWidgets::alert(
          status = "info",
          class = "alert-no-data-no-variables",
          ph("info"), "You need to import data and select variable."
        ),
        data_display_ui(ns("display"))
      )
    )
  )
}


#' @export
#'
#' @rdname module-data
#'
#'
#' @importFrom shiny moduleServer reactive reactiveValues
#'  observeEvent renderUI actionButton icon
#' @importFrom bslib nav_select
data_server <- function(id) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns

      rv <- reactiveValues(data = NULL)


      # Data Import ----

      data_imported_r <- data_import_server("import")

      output$btn_nav_import_dataset <- renderUI({
        shinyjs::show(selector = ".alert-no-data-no-variables")
        req(data_imported_r())
        if (is.data.frame(data_imported_r())) {
          actionButton(
            inputId = ns("go_to_variable_selection"),
            label = "Go to variable selection",
            icon = ph_i("arrow-circle-right"),
            class = "float-end btn-outline-primary"
          )
        }
      })
      observeEvent(input$go_to_variable_selection, nav_select("navs", "variable_selection"))

      observeEvent(data_imported_r(), {
        shinyjs::disable(id = "go_next")
        shinyjs::html(
          id = "go_next",
          html = htmltools::doRenderTags(tagList(
            "You must import a dataset and select variables before proceeding to the next step.",
            ph("arrow-circle-right")
          ))
        )
        rv$data <- data_imported_r()
      })



      # Variable Selection ----

      variable_r <- data_variable_server("variable", data_r = data_imported_r)

      output$btn_nav_variable_selection <- renderUI({
        vars <- variable_r()
        if (isTRUE(vars$taxa) & isTRUE(vars$other)) {
          shinyjs::hide(selector = ".alert-no-data-no-variables")
          actionButton(
            inputId = ns("go_to_data_validation"),
            label = "Go to data validation",
            icon = ph_i("arrow-circle-right"),
            class = "float-end btn-outline-primary"
          )
        }
      })
      observeEvent(input$go_to_data_validation, nav_select("navs", "data_validation"))

      observeEvent(variable_r()$data, {
        rv$data <- variable_r()$data
      })



      # Data validation ----

      data_validated_r <- data_validation_server(
        id = "validation",
        data_r = reactive({
          # input$navs
          variable_r()$data
        })
      )
      output$btn_nav_data_validation <- renderUI({
        validated <- data_validated_r()
        if (nrow(validated) > 0) {
          actionButton(
            inputId = ns("go_to_data"),
            label = "See data",
            icon = ph_i("arrow-circle-right"),
            class = "float-end btn-outline-primary"
          )
        }
      })
      observeEvent(input$go_to_data, nav_select("navs", "data"))

      observeEvent(data_validated_r(), {
        shinyjs::enable(id = "go_next")
        shinyjs::html(
          id = "go_next",
          html = htmltools::doRenderTags(tagList(
            "Go to mapping", ph("arrow-circle-right")
          ))
        )
        rv$data <- data_validated_r()
      })


      # Data display ----

      data_display_server(
        id = "display",
        data_r = reactive(rv$data)
      )

      return(reactive(rv$data))
    }
  )
}

