
#' Summary report
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-report
#'
#' @importFrom shiny NS fluidRow column sliderInput actionButton radioButtons
#' @importFrom htmltools tagList
summary_report_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Summary report",

    alert_no_data(id = ns("no-data"), text = "You must perform the criterion b analysis before you can generate the report."),

    downloadButton(
      outputId = ns("download"),
      label = "Download results",
      class = "mb-3 disabled",
      style = "width: 100%;"
    ),

    uiOutput(outputId = ns("report"))
  )
}

#' @export
#'
#' @rdname module-report
summary_report_server <- function(id, results_r = reactive(NULL), data_sf_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      observeEvent(results_r(), {
        req(
          results_r(),
          length(results_r()) > 0
        )
        shinyjs::addClass(id = "no-data", class = "d-none")
        shinyjs::removeCssClass(id = "download", class = "disabled")
      })

      output$report <- renderUI({
        check_data_sf_r <<- data_sf_r()
        check_results_r <<- results_r()
        data_sf <- req(data_sf_r())
        results <- req(results_r())
        taxa <- results$taxa
        report_file <- if (identical(taxa, "All") | length(taxa) > 1) {
          system.file(package = "conrappli", "reports/all_tax_report.Rmd")
        } else {
          system.file(package = "conrappli", "reports/species_report.Rmd")
        }

        tmp <- tempfile(fileext = ".html")

        rmarkdown::render(
          input = report_file,
          output_format = rmarkdown::html_fragment(),
          params = list(
            tax = results$taxa,
            data = NULL,
            data_sf = data_sf,
            res_aoo = results$aoo_res$AOO_poly,
            res_eoo = results$eoo_res$spatial,
            threat_sig = NULL,
            parameters = results$parameters,
            res_loc = results$locations,
            results = results$results
          ),
          output_file = tmp
        )
        includeHTML(tmp)
      })

    }
  )
}

