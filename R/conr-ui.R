
#' UI - ConR user interface
#'
#'
#' @return An UI definition.
#' @export
#'
#' @importFrom bslib page_navbar nav
#'
#' @seealso
#'  * [conr_server()] for server part.
#'  * [launch()] to launch application.
#'
conr_ui <- function() {
  function(request) {
    page_navbar(
      theme = bs_theme_conr(),
      title = "ConR",
      header = tagList(
        shinyjs::useShinyjs(),
        tags$style(
          ".swal2-popup {font-size: 1rem !important;}",
          ".badge-dragula {font-size: 1rem !important;}",
          ".container-drag-source {border-style: solid !important; border-color: #9b9b9b !important;}",
          ".box-dad {border-color: #9b9b9b !important; margin: 1px !important;}"
        ),
        tags$script(src = "conrappli/js/script.js")
      ),
      nav(
        title = "Data",
        value = "data"
        , data_ui("data")
      ),
      nav(
        title = "Mapping",
        value = "mapping"
        , mapping_ui("mapping")
      ),
      nav(
        title = "Evaluation - Criterion B",
        value = "evaluation"
        , criterion_b_ui("criterion_b")
      ),
      nav(
        title = "Habitat quality/population decline",
        value = "habitat"
      ),
      nav(
        title = "Summary report",
        value = "summary"
      )
    )
  }
}
