
#' UI - ConR user interface
#'
#'
#' @return An UI definition.
#' @export
#'
#' @importFrom bslib page_fluid nav_panel navset_hidden
#' @importFrom datamods i18n
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
        choices = setNames(
          list("home", "data_from_shp", "data_other_options",
            "mapping", "evaluation_criterion_b", "habitat", "report"),
          c(
            i18n("Home"),
            i18n("Import data from SHP"),
            i18n("Import data from other sources"),
            i18n("Mapping"),
            i18n("Evaluation - Criterion B"),
            i18n("Habitat quality/population decline"),
            i18n("Summary report")
          )
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

        navset_hidden(
          id = "navbar",
          nav_panel(
            title = "",
            value = "home",
            icon = ph_i("house"),
            home_ui("home")
          ),
          nav_panel(
            title = "From SHP",
            value = "data_from_shp"
            , data_2_ui("shp")
          ),
          nav_panel(
            title = "Other options",
            value = "data_other_options"
            , data_ui("data")
          ),
          nav_panel(
            title = "Mapping",
            value = "mapping"
            , mapping_ui("mapping")
          ),
          nav_panel(
            title = "Evaluation - Criterion B",
            value = "evaluation_criterion_b"
            , criterion_b_ui("criterion_b")
          ),
          nav_panel(
            title = "Habitat quality/population decline",
            value = "habitat"
          ),
          nav_panel(
            title = "Summary report",
            value = "summary",
            summary_report_ui("report")
          )
        )
      )
    )

  }
}
