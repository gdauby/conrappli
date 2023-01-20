
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

    home_server(id = "home", main_session = session)

    data_rv <- reactiveValues(x = NULL)

    shp_r <- data_shapefile_server(id = "shp")
    data_r <- data_server(id = "data")

    observeEvent(shp_r(), {
      data_rv$x <- shp_r()
      bslib::nav_select(id = "navbar", selected = "evaluation_criterion_b")
    })
    observeEvent(data_r(), data_rv$x <- data_r())

    mapping_l <- mapping_server(
      id = "mapping",
      data_r = reactive({
        req(data_rv$x, hasName(data_rv$x, "STATUS_CONR")) %>%
          dplyr::filter(STATUS_CONR == "IN")
      })
    )

    criterion_b <- criterion_b_server(
      id = "criterion_b",
      data_r = reactive({
        req(mapping_l$data(), hasName(mapping_l$data(), "STATUS_CONR")) %>%
          dplyr::filter(STATUS_CONR == "IN")
      }),
      threat_sig_r = reactive({
        mapping_l$threat_sig()
      }),
      taxa_selected_r = reactive({
        mapping_l$taxa()
      })
    )

    observeEvent(criterion_b(), {
      bslib::nav_select(id = "navbar", selected = "summary")
    })

    summary_report_server(
      id = "report",
      results_r = criterion_b,
      data_sf_r = reactive({
        mapping_l$data_sf()
      }),
      threat_sig_r = reactive({
        mapping_l$threat_sig()
      })
    )

  }
}
