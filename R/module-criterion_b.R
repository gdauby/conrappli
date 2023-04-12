
#' Criterion B analysis
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-analysis
#'
#' @importFrom shiny NS fluidRow column sliderInput actionButton radioButtons
#' @importFrom htmltools tagList
criterion_b_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = "Evaluation - Criterion B",

    alert_no_data(id = ns("no-data")),

    fluidRow(

      column(
        width = 4,
        shinyWidgets::panel(
          status = "primary",
          shinyWidgets::virtualSelectInput(
            inputId = ns("taxa"),
            label = "Taxa:",
            choices = NULL,
            search = TRUE,
            width = "100%"
          ),
          radioButtons(
            inputId = ns("mode_eoo"),
            label = tagList(
              "EOO mode:",
              btn_help(
                "When spheroid, geodetic coordinates are used and EOO is calculated following great circle distances.
                When planar, projected coordinates are used and euclidian distances are used."
              )
            ) ,
            choices = c("spheroid", "planar"),
            inline = TRUE
          ),
          sliderInput(
            inputId = ns("aoo_size"),
            label = tagList(
              "AOO grid size:",
              btn_help(
                "Value indicating the grid size in kilometers used for estimating Area of Occupancy"
              )
            ),
            min = 0.1,
            max = 50,
            value = 2,
            round = TRUE,
            step = 1,
            width = "100%"
          ),
          sliderInput(
            inputId = ns("rep_rast"),
            label = tagList(
              "Number of grid replicates with random position:",
              btn_help(
                "Indicate the number of raster with random starting position for estimating the AOO and the number of locations"
              )
            ),
            min = 0,
            max = 30,
            value = 5,
            round = TRUE,
            step = 1,
            width = "100%"
          ),
          sliderInput(
            inputId = ns("locations_size"),
            label = tagList(
              "Locations grid size:",
              btn_help(
                "Value indicating the grid size in kilometers used for estimating the number of location"
              )
            ),
            min = 0.1,
            max = 50,
            value = 10,
            round = TRUE,
            step = 1,
            width = "100%"
          )
        )
      ),

      column(
        width = 8,

        actionButton(
          inputId = ns("launch"),
          label = tagList(
            ph("play"),
            "Launch Criterion B analysis"
          ),
          class = "mb-4",
          width = "100%",
          class = "btn-outline-primary d-block"
        ),

        tags$div(
          downloadButton(
            outputId = ns("download"),
            label = "Download results",
            class = "float-end mb-3 disabled"
          ),
          tags$div(class = "clearfix"),
          reactable::reactableOutput(outputId = ns("results")),
          actionButton(
            inputId = ns("go_report"),
            label = tagList(
              ph("file-text"),
              "Go to summary report"
            ),
            class = "my-4",
            width = "100%",
            class = "btn-outline-primary d-block disabled"
          )
        )
      )
    )
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#' @param threat_sig_r A `reactive` function returning spatial data to use in analysis.
#' @param taxa_selected_r A `reactive` function returning the taxa to select by default.
#'
#' @export
#'
#' @rdname module-analysis
#'
#' @importFrom shiny moduleServer observeEvent reactive req actionLink
#' @importFrom ConR EOO.computing AOO.computing cat_criterion_b locations.comp
criterion_b_server <- function(id,
                               data_r = reactive(NULL),
                               threat_sig_r = reactive(NULL),
                               taxa_selected_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns
      jns <- function(x) {
        paste0("#", ns(x))
      }

      rv <- reactiveValues(
        results = data.frame(
          taxa = character(0),
          EOO = character(0),
          AOO = character(0),
          locations = character(0),
          category = character(0),
          cat_codes = character(0),
          issue_aoo = character(0),
          issue_eoo = character(0),
          issue_locations = character(0)
        )
      )

      observeEvent(data_r(), {
        req(
          data_r(),
          hasName(data_r(), ".__latitude"),
          hasName(data_r(), ".__longitude"),
          hasName(data_r(), "STATUS_CONR")
        )
        shinyjs::addClass(id = "no-data", class = "d-none")
      })


      observeEvent(data_r(), {
        data <- req(data_r())
        taxas <- unique(data$.__taxa)
        choices <- list(
          "All" = list("All"),
          "Species" = as.list(taxas)
        )
        shinyWidgets::updateVirtualSelect(
          inputId = "taxa",
          choices = choices,
          selected = taxa_selected_r()
        )
      })


      observeEvent(input$launch, {
        data <- req(data_r())

        shinybusy::show_modal_spinner(
          spin = "half-circle",
          color = "#088A08",
          text = "Launching calculation"
        )

        shinyWidgets::execute_safely({
          data <- data %>%
            dplyr::select(.__latitude, .__longitude, .__taxa)

          if (isTruthy(input$taxa) && !identical(input$taxa, "All")) {
            data <- dplyr::filter(data, .__taxa == input$taxa)
          }

          spatial_data <- threat_sig_r()

          # browser()

          shinybusy::update_modal_spinner("Extent of Occurrences multi-taxa computation")
          eoo_res <- EOO.computing(
            XY = data,
            mode = input$mode_eoo,
            export_shp = TRUE
          )

          shinybusy::update_modal_spinner("Area of occupancy computation")
          aoo_res <- AOO.computing(
            XY = data,
            Cell_size_AOO = input$aoo_size,
            nbe.rep.rast.AOO = input$rep_rast,
            export_shp = TRUE
          )

          shinybusy::update_modal_spinner("Number of locations computation")
          locations <- locations.comp(
            XY = data,
            Cell_size_locations = input$locations_size,
            threat_list = spatial_data,
            threat_weight = rep(1, length(spatial_data)),
            method_polygons = "no_more_than_one",
            nbe_rep = input$rep_rast
          )
          
          print(locations$locations)

          shinybusy::update_modal_spinner("Categorize taxa according to IUCN criterion B")
          categories <- cat_criterion_b(
            EOO = eoo_res$results$eoo,
            AOO = aoo_res$AOO$aoo,
            locations = locations$locations$locations
          )

          parameters <- data.frame(
            EOO_mode = input$mode_eoo,
            AOO_size = input$aoo_size,
            locations_size = input$locations_size,
            nbe_rep_grid = input$rep_rast,
            threat_data = !is.null(spatial_data)
          )

          results <- data.frame(
            taxa = row.names(aoo_res$AOO),
            EOO = eoo_res$results$eoo,
            AOO = aoo_res$AOO$aoo,
            locations = locations$locations$locations,
            category = categories$ranks_B,
            cat_codes = categories$cats_code,
            issue_aoo = aoo_res$AOO$issue_aoo,
            issue_eoo = eoo_res$results$issue_eoo,
            issue_locations = locations$locations$issue_locations,
            main_threat = ifelse(rep(is.null(locations$locations$main_threat), nrow(locations$locations)), 
                                 NA, 
                                 locations$locations$main_threat),
            locations$locations[colnames(locations$locations) %in% names(spatial_data)]
          )
          
          
          results <- results %>% 
            as_tibble() %>% 
            mutate(category = replace(category, locations == 6, "VU+")) %>% 
            mutate(category = replace(category, locations %in% c(11, 12, 13), "NT")) %>% 
            mutate(category = replace(category, category == "LC or NT", "LC")) %>% 
            mutate(range_restricted = ifelse(EOO < 50000, TRUE, FALSE))
          
          
          
          shinyjs::removeCssClass(id = "download", class = "disabled")
          shinyjs::removeCssClass(id = "go_report", class = "disabled")
          rv$eoo_res <- eoo_res
          rv$aoo_res <- aoo_res
          rv$locations <- locations
          rv$categories <- categories
          rv$results <- results
          rv$parameters <- parameters
          rv$taxa <- input$taxa
        })
        shinybusy::remove_modal_spinner()
      })

      output$download <- downloadHandler(
        filename = function() {
          "conr-criterion_b.csv"
        },
        content = function(file) {
          write.csv(x = rv$results, file = file, row.names = FALSE)
        }
      )

      output$results <- reactable::renderReactable({
        req(rv$results)
        reactable::reactable(
          data = rv$results,
          rownames = FALSE,
          bordered = TRUE,
          compact = TRUE,
          language = reactable::reactableLang(
            noData = "No results of criterion b analysis to display"
          )
        )
      })

      results_r <- eventReactive(input$go_report, {
        reactiveValuesToList(rv)
      })

      return(results_r)
    }
  )
}
