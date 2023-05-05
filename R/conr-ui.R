
#' UI - ConR user interface
#'
#'
#' @return An UI definition.
#' @export
#'
#' @importFrom bslib page_fluid nav navs_hidden
#'
#' @seealso
#'  * [conr_server()] for server part.
#'  * [launch()] to launch application.
#'
conr_ui <- function() {
  function(request) {

    tagList(

      navigation(
        inputId = "nav",
        choices = list(
          "Home" = "home",
          "Import data from SHP" = "data_from_shp",
          "Import data from other sources" = "data_other_options",
          "Mapping" = "mapping",
          "Evaluation - Criterion B" = "evaluation_criterion_b",
          "Habitat quality/population decline" = "habitat",
          "Summary report" = "report"
        ),
        title = "ConR"
      ),

      page_fluid(
        theme = bs_theme_conr(),
        title = "ConR",
        tagList(
          shinyjs::useShinyjs(),
          shinyWidgets::useSweetAlert(),
          shinybusy::add_busy_bar(color = "#088A08", height = "7px"),
          tags$style(
            ".swal2-popup {font-size: 1rem !important;}",
            ".badge-dragula {font-size: 1rem !important;}",
            ".container-drag-source {border-style: solid !important; border-color: #9b9b9b !important;}",
            ".box-dad {border-color: #9b9b9b !important; margin: 1px !important;}"
          ),
          tags$script(src = "conrappli/js/script.js")
        ),

        navs_hidden(
          id = "navbar",
          nav(
            title = "",
            value = "home",
            icon = ph_i("house"),
            home_ui("home")
          ),
          nav(
            title = "From SHP",
            value = "data_from_shp"
            , data_2_ui("shp")
          ),
          nav(
            title = "Other options",
            value = "data_other_options"
            , data_ui("data")
          ),
          nav(
            title = "Mapping",
            value = "mapping"
            , mapping_ui("mapping")
          ),
          nav(
            title = "Evaluation - Criterion B",
            value = "evaluation_criterion_b"
            , criterion_b_ui("criterion_b")
          ),
          nav(
            title = "Habitat quality/population decline",
            value = "habitat"
          ),
          nav(
            title = "Summary report",
            value = "summary",
            summary_report_ui("report")
          )
        )
      )
    )

  }
}
