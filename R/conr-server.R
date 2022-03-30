
#' Application's server
#'
#'
#' @return No value.
#' @export
#'
#' @importFrom shiny reactive reactiveValuesToList
#'
#' @seealso
#'  * [conr_ui()] for UI part.
#'  * [launch()] to launch application.
#'
conr_server <- function() {
  function(input, output, session) {

    data_server("data")

  }
}
