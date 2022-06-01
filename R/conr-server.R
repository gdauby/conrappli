
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

    data_r <- data_server("data")

    mapping_server(id = "mapping", data_r = data_r)

    criterion_b_server(id = "criterion_b", data_r = reactive({
      data_r() %>%
        filter(STATUS_CONR == "IN")
    }))

  }
}
