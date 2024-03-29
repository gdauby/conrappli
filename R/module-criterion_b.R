
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
    title = i18n("Evaluation - Criterion B"),

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
              i18n("Method for estimating the EOO:"),
              btn_help(
                "When spheroid, geodetic coordinates are used and EOO is calculated following great circle distances.
                When planar, projected coordinates are used and euclidian distances are used."
              )
            ) ,
            choices = c("planar", "spheroid")
          ),
          shinyWidgets::searchInput(
            inputId = ns("projection"),
            label = tagList(
              i18n("EPSG or ESRI code"),
              btn_help(
                "Projection used for estimating EOO, if the mode is 'planar'. By default it is a Cylindrical Equal Area projection recommanded by the IUCN, but others projections may be more suited for specific areas.
                Check https://epsg.io/"
              )
            ) ,
            value = "ESRI:54034",
            resetValue = "ESRI:54034",
            btnSearch = icon("magnifying-glass"),
            btnReset = icon("xmark")
          ),
          sliderInput(
            inputId = ns("aoo_size"),
            label = tagList(
              i18n("AOO grid resolution:"),
              btn_help(
                i18n("Value indicating the grid resolution in kilometers used for estimating the Area of Occupancy")
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
              i18n("Number of grid replicates with random starting position:"),
              btn_help(
                i18n("Indicate the number of grid with random starting position for estimating the AOO and the number of locations")
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
              i18n("Locations grid resolution:"),
              btn_help(
                i18n("Value indicating the grid resolution in kilometers used for estimating the number of location")
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
            i18n("Launch Criterion B analysis")
          ),
          class = "mb-4",
          width = "100%",
          class = "btn-outline-primary d-block"
        ),

        tags$div(
          downloadButton(
            outputId = ns("download"),
            label = i18n("Download results"),
            class = "float-end mb-3 disabled"
          ),
          tags$div(class = "clearfix"),
          reactable::reactableOutput(outputId = ns("results")),
          actionButton(
            inputId = ns("go_report"),
            label = tagList(
              ph("file-text"),
              i18n("Go to summary report")
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
#' @param table_overlap_r A `reactive` function.
#'
#' @export
#'
#' @rdname module-analysis
#'
#' @importFrom shiny moduleServer observeEvent reactive req actionLink
#' @importFrom ConR EOO.computing AOO.computing cat_criterion_b locations.comp
#' @importFrom dplyr select filter group_by mutate n_distinct across everything ungroup distinct left_join relocate as_tibble
#' @importFrom reactable renderReactable colDef reactable reactableLang
#' @importFrom shinybusy show_modal_spinner update_modal_spinner remove_modal_spinner
#' @importFrom shinyjs addClass removeCssClass
#' @importFrom shinyWidgets updateVirtualSelect execute_safely
criterion_b_server <- function(id,
                               data_r = reactive(NULL),
                               threat_sig_r = reactive(NULL),
                               taxa_selected_r = reactive(NULL),
                               table_overlap_r = reactive(NULL)) {
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
        addClass(id = "no-data", class = "d-none")
      })


      observeEvent(data_r(), {
        data <- req(data_r())
        taxas <- unique(data$.__taxa)
        choices <- list(
          "All" = list("All"),
          "Species" = as.list(taxas)
        )
        updateVirtualSelect(
          inputId = "taxa",
          choices = choices,
          selected = taxa_selected_r()
        )
      })


      observeEvent(input$launch, {
        data <- req(data_r())

        show_modal_spinner(
          spin = "half-circle",
          color = "#088A08",
          text = i18n("Launching calculation")
        )

        execute_safely({
          data <- data %>%
            select(.__latitude, .__longitude, .__taxa)

          if (isTruthy(input$taxa) && !identical(input$taxa, "All")) {
            data <- filter(data, .__taxa == input$taxa)
          }

          spatial_data <- threat_sig_r()

          table_overlap <- table_overlap_r()

          print(table_overlap)

          # browser()

          update_modal_spinner(i18n("Extent of Occurrences multi-taxa computation"))
          eoo_res <- EOO.computing(
            XY = data,
            mode = input$mode_eoo,
            export_shp = TRUE,
            proj_type = input$projection
          )

          update_modal_spinner(i18n("Area of occupancy computation"))
          aoo_res <- AOO.computing(
            XY = data,
            cell_size_AOO = input$aoo_size,
            nbe.rep.rast.AOO = input$rep_rast,
            export_shp = TRUE
          )
          

          update_modal_spinner(i18n("Number of locations computation"))
          locations <- locations.comp(
            XY = data,
            cell_size_locations = input$locations_size,
            threat_list = spatial_data,
            threat_weight = table_overlap$priority,
            method_polygons = table_overlap$polygon_method,
            nbe_rep = input$rep_rast
          )


          update_modal_spinner(i18n("Categorize taxa according to IUCN criterion B"))
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
            threat_data = !is.null(spatial_data),
            projection = input$projection
          )

          count_unique_coord <- data %>%
            group_by(.__taxa) %>%
            mutate(pair_unique_coordinates = n_distinct(across(everything())))%>%
            ungroup()

          count_unique_coord <-
            distinct(count_unique_coord %>%
                       select(.__taxa, pair_unique_coordinates))


          results <- data.frame(
            taxa = aoo_res$AOO$tax,
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

          results <-
            results %>%
            left_join(count_unique_coord %>%
                        select(.__taxa, pair_unique_coordinates),
                      by = c("taxa" = ".__taxa"))

          results <- results %>%
            as_tibble() %>%
            mutate(category = replace(category, locations == 6, "VU+")) %>%
            mutate(category = replace(category, locations %in% c(11, 12, 13), "NT")) %>%
            mutate(category = replace(category, category == "LC or NT", "LC")) %>%
            mutate(range_restricted = ifelse(EOO < 50000, TRUE, FALSE))

          if (any(names(results) == "protected"))
            results <-
            results %>%
            relocate(protected, .before = pair_unique_coordinates)

          removeCssClass(id = "download", class = "disabled")
          removeCssClass(id = "go_report", class = "disabled")
          rv$eoo_res <- eoo_res
          rv$aoo_res <- aoo_res
          rv$locations <- locations
          rv$categories <- categories
          rv$results <- results
          rv$parameters <- parameters
          rv$taxa <- input$taxa
        })
        remove_modal_spinner()
      })

      output$download <- downloadHandler(
        filename = function() {
          "conr-criterion_b.csv"
        },
        content = function(file) {
          write.csv(x = rv$results, file = file, row.names = FALSE, na = "")
        }
      )

      output$results <- renderReactable({
        req(rv$results)

        col_defs <- list(
          EOO = colDef(name = i18n("Extent of occurence (EOO)")),
          AOO = colDef(name = i18n("Area of occupancy (AOO)")),
          locations = colDef(name = i18n("Estimated number of location")),
          category = colDef(name = i18n("IUCN preliminary category")),
          pair_unique_coordinates = colDef(name = "Number of unique occurences"),
          range_restricted = colDef(name = i18n("True if taxon is range-restricted")),
          cat_codes = colDef(name = i18n("IUCN code of assessment")),
          issue_aoo = colDef(name = i18n("Potential issue in AOO estimation")),
          issue_eoo = colDef(name = i18n("Potential issue in EOO estimation")),
          main_threat = colDef(name = i18n("Main threat identified")),
          mining = colDef(name = i18n("Number of location threatened by mining")),
          cities = colDef(name = "Number of location threatened by urban areas"),
          agroindustry = colDef(name = i18n("Number of location threatened by agroindustry")),
          logging = colDef(name = i18n("Number of location threatened by industrial logging")),
          cropland = colDef(name = i18n("Number of location threatened by small-scale agriculture")),
          protected = colDef(name = i18n("Number of location in protected areas"))
        )

        col_defs <- col_defs[names(col_defs) %in% names(rv$results)]

        reactable(
          data = rv$results,
          rownames = FALSE,
          bordered = TRUE,
          compact = TRUE,
          pagination = FALSE,
          language = reactableLang(
            noData = i18n("No results of criterion b analysis to display")
          ),
          theme = reactable_theme(),
          height = 500,
          columns = col_defs)
      })

      results_r <- eventReactive(input$go_report, {
        reactiveValuesToList(rv)
      })

      return(results_r)
    }
  )
}

