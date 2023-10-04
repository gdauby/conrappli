
#' UI - ConR user interface
#'
#' @param lang Language to be used in the application : english ("en") or french ("fr").
#'
#' @return An UI definition.
#' @export
#'
#' @importFrom bslib page_navbar nav_item navset_hidden sidebar nav_item nav_panel_hidden
#' @importFrom datamods i18n
#' @importFrom shiny selectInput
#' @importFrom htmltools tags tagAppendAttributes
#' @importFrom phosphoricons ph
#' @importFrom utils packageVersion
#' @importFrom shinyWidgets radioGroupButtons
#'
#' @seealso
#'  * [conr_server()] for server part.
#'  * [launch()] to launch application.
#'
conr_ui <- function(lang = c("en", "fr")) {
  lang <- match.arg(lang)
  if (lang == "en")
    lang <- NULL
  datamods::set_i18n(lang, packages = c("conrappli", "datamods"))
  function(request) {

    page_navbar(
      title = tagList(
        actionButton(
          inputId = "toggle_sidebar",
          label = ph("list", weight = "bold", title = "Toggle sidebar"),
          class = "btn-sm btn-outline-primary me-3"
        ),
        tags$b("ConApp"),
        tags$span(
          style = css(fontSize = "small"),
          paste0("v", packageVersion("conrappli"))
        )
      ),
      window_title = "ConApp",
      theme = bs_theme_conr(),
      fillable = FALSE,
      id = "navbar",
      sidebar = bslib::sidebar(
        open = "closed",
        id = "sidebar",
        radioGroupButtons(
          inputId = "navigation",
          label = "Menu",
          choices = setNames(
            list(
              "home",
              "data_from_shp",
              "data_other_options",
              "mapping",
              "evaluation_criterion_b",
              "habitat",
              "summary_report"
            ),
            c(
              i18n("Home"),
              i18n("Import data from SHP"),
              i18n("Import data from other sources"),
              i18n("Mapping"),
              i18n("Evaluation - Criterion B"),
              i18n("Distribution of threatened tree in Gabon"),
              i18n("Summary report")
            )
          ),
          direction = "vertical",
          status = "navs rounded-0 text-start fw-bold py-2"
        )
      ),
      header = tagList(
        shinyjs::useShinyjs(),
        shinyWidgets::useSweetAlert(),
        shinybusy::add_busy_bar(color = "#088A08", height = "7px"),
        tags$style(
          ".swal2-popup {font-size: 1rem !important;}",
          ".badge-dragula {font-size: 1rem !important;}",
          ".container-drag-source {border-style: solid !important; border-color: #9b9b9b !important;}",
          ".box-dad {border-color: #9b9b9b !important; margin: 1px !important;}",
          ".sidebar { z-index: 101; }"
        ),
        tags$script(src = "conrappli/js/script.js"),
        tags$style(
          ".btn-navs { border: none; }",
          ".btn-check:hover+.btn.btn-navs { background: #D8D8D8; }",
          ".btn-check:checked+.btn.btn-navs { border-left: 5px solid #088A08; background: #454545; }"
        )
      ),

      nav_panel_hidden(
        value = "home",
        # icon = ph_i("house"),
        home_ui("home")
      ),
      nav_panel_hidden(
        value = "data_from_shp"
        , data_poly_ui("shp")
      ),
      nav_panel_hidden(
        value = "data_other_options"
        , data_full_ui("data")
      ),
      nav_panel_hidden(
        value = "mapping"
        , mapping_ui("mapping")
      ),
      nav_panel_hidden(
        value = "evaluation_criterion_b"
        , criterion_b_ui("criterion_b")
      ),
      nav_panel_hidden(
        value = "habitat"
        , data_country_ui("data_country")
      ),
      nav_panel_hidden(
        value = "summary_report"
        , summary_report_ui("report")
      ),
      nav_spacer(),
      nav_item(
        tags$label(
          style = css(verticalAlign = "top", marginTop = "0.5rem"),
          "Language:",
          `for` = "app_lang"
        ),
        tagAppendAttributes(
          selectInput(
            inputId = "app_lang",
            label = NULL,
            choices = c("en", "fr"),
            selected = lang,
            width = "80px"
          ),
          style = css(display = "inline-block"),
          class = "mt-1 mb-0"
        )
      )
    )

  }
}
