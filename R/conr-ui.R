
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
        tags$style(
          ".swal2-popup {font-size: 1rem !important;}",
          ".badge-dragula {font-size: 1rem !important;}"
        )
      ),
      nav(
        title = "Data Import",
        value = "data_import",
        import_data_ui("data-import")
      )
    )
  }
}
