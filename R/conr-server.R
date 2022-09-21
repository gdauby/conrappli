
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

    mapping_l <- mapping_server(id = "mapping", data_r = reactive({
      req(data_r(), hasName(data_r(), "STATUS_CONR")) %>%
        dplyr::filter(STATUS_CONR == "IN")
    }))

    criterion_b_server(
      id = "criterion_b",
      data_r = reactive({
        req(mapping_l$data(), hasName(mapping_l$data(), "STATUS_CONR")) %>%
          dplyr::filter(STATUS_CONR == "IN")
      }),
      spatial_data_r = reactive({
        check_spatial_data <<- mapping_l$spatial_data()
        mapping_l$spatial_data()
      })
    )

  }
}
