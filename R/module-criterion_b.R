
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

    fluidRow(

      column(
        width = 4,
        shinyWidgets::panel(
          status = "primary",
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
              "AOO raster number:",
              btn_help(
                "Indicate the number of raster with random starting position for estimating the AOO"
              )
            ),
            min = 0,
            max = 30,
            value = 10,
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

        tags$div(
          id = ns("container-spatial-data"),
          # class = "d-none",
          tags$p("Overlaping spatial data available:"),
          reactable::reactableOutput(outputId = ns("spatial_data")),
          shinyWidgets::virtualSelectInput(
            inputId = ns("spatial_data_select"),
            label = "Spatial data to use (select in order):",
            choices = NULL,
            multiple = TRUE,
            hasOptionDescription = TRUE,
            showValueAsTags = TRUE,
            disableSelectAll = TRUE,
            zIndex = 10,
            width = "100%"
          )
        ),

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
          reactable::reactableOutput(outputId = ns("results"))
        )
      )
    )
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-analysis
#'
#' @importFrom shiny moduleServer observeEvent reactive req actionLink
#' @importFrom ConR EOO.computing AOO.computing cat_criterion_b locations.comp
criterion_b_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns
      jns <- function(x) {
        paste0("#", ns(x))
      }

      rv <- reactiveValues()

      observeEvent(data_r(), {
        data <- req(data_r())
        check_overlap <- extract_overlap_shp(XY = test_data)
        rv$check_overlap <- check_overlap
        if (!(all(check_overlap$shp_tables$overlap))) {
          rv$spatial_data <- NULL
          rv$all_shp <- NULL
          shinyjs::addClass(id = "container-spatial-data", class = "d-none")
        } else {
          rv$spatial_data <- check_overlap$shp_tables %>%
            dplyr::select(table_name, type, description, reference)
          shinyWidgets::updateVirtualSelect(
            inputId = "spatial_data_select",
            choices = rv$spatial_data %>%
              shinyWidgets::prepare_choices(label = table_name, value = table_name, description = description)
          )
          shinyjs::removeClass(id = "container-spatial-data", class = "d-none")
        }
      })

      output$spatial_data <- reactable:::renderReactable({
        req(rv$spatial_data) %>%
          reactable::reactable(
            compact = TRUE,
            columns = list(
              table_name = reactable::colDef(name = "Name", minWidth = 100),
              type = reactable::colDef(name = "Type", minWidth = 50),
              description = reactable::colDef(name = "Description", minWidth = 200),
              reference = reactable::colDef(
                name = "Reference", minWidth = 100,
                cell = function(value, index) {
                  htmltools::tags$a(href = value, target = "_blank", as.character(value))
                }
              )
            )
          )
      })


      observeEvent(input$launch, {
        data <- req(data_r())

        shinybusy::show_modal_spinner(
          spin = "half-circle",
          color = "#2472b5",
          text = "Launching calculation"
        )

        data <- data %>%
          dplyr::select(.__latitude, .__longitude, .__taxa)


        shp_tbls <- rv$check_overlap$shp_tables
        if (is.null(input$spatial_data_select)) {
          all_shp <- NULL
        } else {
          shinybusy::update_modal_spinner("Collecting spatial data")
          tbls_nms <- shp_tbls[match(x = input$spatial_data_select, table = shp_tbls$table_name), ]
          all_shp <- collect_shp(
            table_names = tbls_nms,
            XY_sf = rv$check_overlap$XY_sf
          )
        }

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
          threat_list = all_shp,
          method_polygons = "no_more_than_one"
        )

        shinybusy::update_modal_spinner("Categorize taxa according to IUCN criterion B")
        categories <- cat_criterion_b(
          EOO = eoo_res$results$eoo,
          AOO = aoo_res$AOO$aoo,
          locations = locations$locations$locations
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
          issue_locations = locations$locations$issue_locations
        )
        shinybusy::remove_modal_spinner()
        shinyjs::removeCssClass(id = "download", class = "disabled")
        rv$eoo_res <- eoo_res
        rv$aoo_res <- aoo_res
        rv$locations <- locations
        rv$categories <- categories
        rv$results <- results
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
          bordered = TRUE,
          compact = TRUE
        )
      })

    }
  )
}
