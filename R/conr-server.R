
#' Application's server
#'
#'
#' @return No value.
#' @export
#'
#' @importFrom shiny reactive reactiveValuesToList
#' @importFrom bslib sidebar_toggle nav_select
#' @importFrom shinyWidgets updateRadioGroupButtons
#'
#' @seealso
#'  * [conr_ui()] for UI part.
#'  * [launch()] to launch application.
#'
conr_server <- function() {
  function(input, output, session) {

    observeEvent(input$navigation, {
      bslib::nav_select(id = "navbar", selected = input$navigation)
    })
    observeEvent(input$toggle_sidebar, {
      bslib::sidebar_toggle(id = "sidebar")
    })

    observeEvent(input$app_lang, {
      if (input$app_lang == "fr") {
        datamods::set_i18n("fr", packages = c("conrappli", "datamods"))
      } else {
        datamods::set_i18n(NULL, packages = c("conrappli", "datamods"))
      }
      session$reload()
    }, ignoreInit = TRUE)

    home_server(id = "home", main_session = session)
    
    data_country_server("data_country")

    data_rv <- reactiveValues(x = NULL, polygon = NULL, latlon = NULL)

    shp_lr <- data_poly_server(id = "shp")
    data_lr <- data_full_server(id = "data", main_session = session)

    observeEvent(shp_lr$data(), {
      data_rv$x <- shp_lr$data()
      data_rv$polygon <- shp_lr$poly()
      updateRadioGroupButtons(session = session, inputId = "navigation", selected = "evaluation_criterion_b")
    })
    observeEvent(data_lr$data(), data_rv$x <- data_lr$data())
    observeEvent(data_lr$data_latlon(), data_rv$latlon <- data_lr$data_latlon())

    observeEvent(shp_lr$data_latlon(), data_rv$latlon <- shp_lr$data_latlon())

    mapping_l <- mapping_server(
      id = "mapping",
      data_r = reactive({
        req(data_rv$x, hasName(data_rv$x, "STATUS_CONR")) %>%
          dplyr::filter(STATUS_CONR == "IN")
      }),
      data_latlon_r = reactive({
        req(data_rv$latlon) %>%
          filter(
            !is.na(.__longitude), !is.na(.__latitude)
          )
      }),
      trigger_map_r = reactive(identical(input$navbar, "mapping")),
      main_session = session
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
      }),
      table_overlap_r = reactive({
        mapping_l$table_overlap()
      })
    )

    observeEvent(criterion_b(), {
      updateRadioGroupButtons(session = session, inputId = "navigation", selected = "summary_report")
    })

    summary_report_server(
      id = "report",
      data_r = reactive({
        req(mapping_l$data(), hasName(mapping_l$data(), "STATUS_CONR")) %>%
          dplyr::filter(STATUS_CONR == "IN")
      }),
      results_r = criterion_b,
      data_sf_r = reactive({
        mapping_l$data_sf()
      }),
      # threat_sig_r = reactive({
      #   mapping_l$threat_sig()
      # }),
      polygon_r = reactive(data_rv$polygon)
    )

  }
}
