
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
#' @importFrom shiny NS fluidRow column actionButton downloadButton uiOutput
#' @importFrom htmltools tagList HTML
#' @importFrom shinyWidgets virtualSelectInput
#' @importFrom bslib navset_card_pill nav_panel
summary_report_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = i18n("Summary report"),

    alert_no_data(
      id = ns("no-data"),
      text = i18n("You must perform the criterion b analysis before you can generate the report.")
    ),

    bslib::navset_card_pill(
      # title = "Report",
      nav_panel(
        title = "global",
        fluidRow(
          class = "mb-3",
          column(
            width = 4,
            offset = 8,
            tags$label(class = "control-label", HTML("&nbsp;")),
            downloadButton(
              outputId = ns("download_all_taxa"),
              label = i18n("Download the report"),
              class = "disabled",
              style = "width: 100%;"
            )
          )
        ),
        uiOutput(outputId = ns("report_all_taxa"))
      ),
      nav_panel(
        title = i18n("by species"),
        fluidRow(
          class = "mb-3",
          column(
            width = 4,
            shinyWidgets::virtualSelectInput(
              inputId = ns("taxa"),
              label = i18n("Select the taxa for which to generate the report:"),
              choices = NULL,
              search = TRUE,
              width = "100%"
            )
          ),
          column(
            width = 4,
            offset = 4,
            tags$label(class = "control-label", HTML("&nbsp;")),
            downloadButton(
              outputId = ns("download_taxa"),
              label = i18n("Download the report"),
              class = "disabled",
              style = "width: 100%;"
            )
          )
        ),
        uiOutput(outputId = ns("report_taxa"))
      )
    )

  )
}

#' @param data_r,results_r,data_sf_r,polygon_r `reactive` functions returning data used to generate reports.
#' @export
#'
#' @rdname module-report
#'
#' @importFrom shiny moduleServer observeEvent req reactive renderUI downloadHandler includeHTML
#' @importFrom shinyWidgets execute_safely
summary_report_server <- function(id,
                                  data_r = reactive(NULL),
                                  results_r = reactive(NULL),
                                  data_sf_r = reactive(NULL),
                                  # threat_sig_r = reactive(NULL),
                                  polygon_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      report_dir <- tempfile(pattern = "ConRAppReport")
      dir.create(report_dir)
      # shiny::addResourcePath(prefix = "ConRAppReport", directoryPath = report_dir)

      observeEvent(results_r(), {
        req(
          results_r(),
          length(results_r()) > 0,
          !is.null(results_r()$results$taxa)
        )

        species <- results_r()$results$taxa
        shinyWidgets::updateVirtualSelect(
          inputId = "taxa",
          choices = sort(unique(species)),
          selected = species[1]
        )
        shinyjs::addClass(id = "no-data", class = "d-none")
        shinyjs::removeCssClass(id = "download_taxa", class = "disabled")
        shinyjs::removeCssClass(id = "download_all_taxa", class = "disabled")
      })


      # one taxa ----

      output$report_taxa <- renderUI({

        data_sf <- req(data_sf_r())
        results <- req(results_r())
        req(input$taxa)

        tmp <- tempfile(tmpdir = report_dir, fileext = ".html")
        
        shinyWidgets::execute_safely({
          rmarkdown::render(
            input =  system.file(package = "conrappli", "reports/species_report.Rmd"),
            output_format = rmarkdown::html_fragment(),
            params = list(
              tax = input$taxa,
              data = NULL,
              data_sf = data_sf %>%
                filter(.__taxa == input$taxa),
              res_aoo = results$aoo_res$AOO_poly %>%
                filter(tax == input$taxa),
              res_eoo = results$eoo_res$spatial %>%
                filter(tax == input$taxa),
              threat_sig = results$locations$threat_list,
              parameters = results$parameters,
              res_loc = results$locations$locations_poly %>%
                filter(tax == input$taxa),
              results = results$results %>%
                filter(taxa == input$taxa)
            ),
            output_file = tmp,
            intermediates_dir = dirname(tmp)
          )
        })
        includeHTML(tmp)
      })

      output$download_taxa <- downloadHandler(
        filename = function() {
          paste0("ConR-report-", input$taxa, "-", Sys.Date(), ".html")
        },
        content = function(file) {
          data_sf <- req(data_sf_r())
          results <- req(results_r())
          shinybusy::show_modal_spinner(
            spin = "half-circle",
            color = "#088A08",
            text = i18n("Generating report...")
          )
          tmp <- tempfile(tmpdir = report_dir, fileext = ".html")
          shinyWidgets::execute_safely({
            rmarkdown::render(
              input =  system.file(package = "conrappli", "reports/species_report.Rmd"),
              output_format = rmarkdown::html_document(
                theme = bs_theme_conr(),
                number_sections = TRUE,
                toc = TRUE,
                toc_float = TRUE,
                toc_depth = 5,
                self_contained = TRUE
              ),
              params = list(
                tax = input$taxa,
                data = NULL,
                data_sf = data_sf %>%
                  filter(.__taxa == input$taxa),
                res_aoo = results$aoo_res$AOO_poly %>%
                  filter(tax == input$taxa),
                res_eoo = results$eoo_res$spatial %>%
                  filter(tax == input$taxa),
                threat_sig = results$locations$threat_list,
                parameters = results$parameters,
                res_loc = results$locations$locations_poly %>%
                  filter(tax == input$taxa),
                results = results$results %>%
                  filter(taxa == input$taxa)
              ),
              output_file = tmp,
              intermediates_dir = dirname(tmp)
            )
          })
          shinybusy::remove_modal_spinner()
          file.copy(from = tmp, to = file)
        }
      )


      

      # global ----

      output$report_all_taxa <- renderUI({

        data_sf <- req(data_sf_r())
        results <- req(results_r())

        tmp <- tempfile(tmpdir = report_dir, fileext = ".html")

        shinyWidgets::execute_safely({
          rmarkdown::render(
            input =  system.file(package = "conrappli", "reports/all_tax_report.Rmd"),
            output_format = rmarkdown::html_fragment(),
            params = list(
              data = data_r(),
              polygon_rv = polygon_r(),
              threat_sig = results$locations$threat_list,
              parameters = results$parameters,
              results = results$results
            ),
            output_file = tmp,
            intermediates_dir = dirname(tmp)
          )
        })
        includeHTML(tmp)
      })

      output$download_all_taxa <- downloadHandler(
        filename = function() {
          paste0("ConR-report-", "-", Sys.Date(), ".html")
        },
        content = function(file) {
          data_sf <- req(data_sf_r())
          results <- req(results_r())
          shinybusy::show_modal_spinner(
            spin = "half-circle",
            color = "#088A08",
            text = i18n("Generating report...")
          )
          tmp <- tempfile(tmpdir = report_dir, fileext = ".html")
          shinyWidgets::execute_safely({
            rmarkdown::render(
              input =  system.file(package = "conrappli", "reports/all_tax_report.Rmd"),
              output_format = rmarkdown::html_document(
                theme = bs_theme_conr(),
                number_sections = TRUE,
                toc = TRUE,
                toc_float = TRUE,
                toc_depth = 5,
                self_contained = TRUE
              ),
              params = list(
                data = data_r(),
                polygon_rv = polygon_r(),
                threat_sig = results$locations$threat_list,
                parameters = results$parameters,
                results = results$results
              ),
              output_file = tmp,
              intermediates_dir = dirname(tmp)
            )
          })
          shinybusy::remove_modal_spinner()
          file.copy(from = tmp, to = file)
        }
      )

    }
  )
}

