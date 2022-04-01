
#' @title Data Validation Module
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
#' @name module-data-validation
#'
#' @importFrom shiny NS
#' @importFrom htmltools tagList
data_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    datamods::validation_ui(ns("result"), display = "inline"),
    uiOutput(outputId = ns("alert_result"))
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-data-validation
#'
#' @importFrom shiny moduleServer observeEvent reactive req actionLink
data_validation_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      to_validate_r <- reactive({
        req(data_r())
        prepare_data_validate(data_r())
      })

      datamods::validation_server(
        id = "result",
        data = to_validate_r,
        rules = validation_rules(),
        bs_version = 5
      )

      validated_r <- reactive({
        req(to_validate_r())
        exclude_violating_records(to_validate_r())
      })

      output$alert_result <- renderUI({
        to_validate <- to_validate_r()
        validated <- validated_r()
        if (nrow(validated) < 1) {
          shinyWidgets::alert(
            status = "error",
            icon("exclamation-triangle"), "There are not enough rows to proceed."
          )
        } else if (nrow(validated) < nrow(to_validate)) {
          shinyWidgets::alert(
            status = "warning",
            icon("exclamation-triangle"),
            nrow(to_validate) - nrow(validated),
            "rows will be discarded in order to proceed.",
            actionLink(inputId = session$ns("see_data"), label = "click to see problematic rows.")
          )
        } else {
          shinyWidgets::alert(
            status = "success",
            icon("check"),
            "All rows are OK."
          )
        }
      })

      observeEvent(input$see_data, {
        datamods::show_data(
          data = extract_violating_records(to_validate_r()),
          title = "Violating validation rules records",
          type = "modal"
        )
      })

      return(validated_r)
    }
  )
}
