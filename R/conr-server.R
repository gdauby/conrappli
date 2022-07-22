
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

    data_mapped_r <- mapping_server(id = "mapping", data_r = reactive({
      req(data_r(), hasName(data_r(), "STATUS_CONR")) %>%
        dplyr::filter(STATUS_CONR == "IN")
    }))

    criterion_b_server(id = "criterion_b", data_r = reactive({
      check_data <<- req(data_mapped_r(), hasName(data_r(), "STATUS_CONR")) %>%
        dplyr::filter(STATUS_CONR == "IN")
    }))

  }
}
