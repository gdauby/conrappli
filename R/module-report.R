
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
#' @importFrom htmltools tagList
#' @importFrom shinyWidgets virtualSelectInput
summary_report_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Summary report",

    alert_no_data(
      id = ns("no-data"), 
      text = "You must perform the criterion b analysis before you can generate the report."
    ),

    
    bslib::navs_pill_card(
      title = "Report",
      nav(
        title = "by species",
        fluidRow(
          class = "mb-3",
          column(
            width = 4,
            shinyWidgets::virtualSelectInput(
              inputId = ns("taxa"),
              label = "Select the taxa for which to generate the report:",
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
              outputId = ns("download"),
              label = "Download the report",
              class = "disabled",
              style = "width: 100%;"
            )
          )
        ),
        uiOutput(outputId = ns("report_taxa"))
      ),
      nav(
        title = "global",
        fluidRow(
          class = "mb-3",
          column(
            width = 4,
            offset = 8,
            tags$label(class = "control-label", HTML("&nbsp;")),
            downloadButton(
              outputId = ns("download_all"),
              label = "Download the report",
              class = "disabled",
              style = "width: 100%;"
            )
          )
        ),
        uiOutput(outputId = ns("report_all_taxa"))
      )
    )
    
  )
}

#' @export
#'
#' @rdname module-report
#' 
#' @importFrom shiny moduleServer observeEvent req reactive renderUI downloadHandler includeHTML
#' @importFrom shinyWidgets execute_safely 
summary_report_server <- function(id, 
                                  results_r = reactive(NULL),
                                  data_sf_r = reactive(NULL),
                                  threat_sig_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

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
        shinyjs::removeCssClass(id = "download", class = "disabled")
        shinyjs::removeCssClass(id = "download_all", class = "disabled")
      })

      
      # one taxa ----
      
      output$report_taxa <- renderUI({
        check_data_sf_r <<- data_sf_r()
        check_results_r <<- results_r()
        data_sf <- req(data_sf_r())
        results <- req(results_r())
        req(input$taxa)

        tmp <- tempfile(fileext = ".html")

        shinyWidgets::execute_safely({
          rmarkdown::render(
            input =  system.file(package = "conrappli", "reports/species_report.Rmd"),
            output_format = rmarkdown::html_fragment(number_sections = TRUE),
            params = list(
              tax = input$taxa,
              data = NULL,
              data_sf = data_sf %>%
                filter(.__taxa == input$taxa),
              res_aoo = results$aoo_res$AOO_poly %>%
                filter(tax == input$taxa),
              res_eoo = results$eoo_res$spatial %>%
                filter(tax == input$taxa),
              threat_sig = threat_sig_r(),
              parameters = results$parameters,
              res_loc = results$locations$locations_poly %>%
                filter(tax == input$taxa),
              results = results$results %>%
                filter(taxa == input$taxa)
            ),
            output_file = tmp
          )
        })
        includeHTML(tmp)
      })

      output$download <- downloadHandler(
        filename = function() {
          paste0("ConR-report-", input$taxa, "-", Sys.Date(), ".html")
        },
        content = function(file) {
          shinybusy::show_modal_spinner(
            spin = "half-circle",
            color = "#088A08",
            text = "Generating report"
          )
          data_sf <- req(data_sf_r())
          results <- req(results_r())
          tmp <- tempfile(fileext = ".html")
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
                threat_sig = threat_sig_r(),
                parameters = results$parameters,
                res_loc = results$locations$locations_poly %>%
                  filter(tax == input$taxa),
                results = results$results %>%
                  filter(taxa == input$taxa)
              ),
              output_file = tmp
            )
          })
          shinybusy::remove_modal_spinner()
          file.copy(from = tmp, to = file)
        }
      )
      
      
      
      # global ----
      
      output$report_all_taxa <- renderUI({
        check_data_sf_r <<- data_sf_r()
        check_results_r <<- results_r()
        data_sf <- req(data_sf_r())
        results <- req(results_r())
        
        tmp <- tempfile(fileext = ".html")
        
        shinyWidgets::execute_safely({
          rmarkdown::render(
            input =  system.file(package = "conrappli", "reports/all_tax_report.Rmd"),
            output_format = rmarkdown::html_fragment(number_sections = TRUE),
            params = list(
              polygon_rv = NULL,
              threat_sig = threat_sig_r(),
              parameters = results$parameters,
              results = results$results %>%
                filter(taxa == input$taxa)
            ),
            output_file = tmp
          )
        })
        includeHTML(tmp)
      })

    }
  )
}

