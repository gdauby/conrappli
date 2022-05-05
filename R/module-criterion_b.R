
#' @title Data Validation Module
#'
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
#' @importFrom shiny NS
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
            label = "EOO mode:",
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
        actionButton(
          inputId = ns("launch"),
          label = "Launch analysis",
          icon = icon("play"),
          class = "mb-4",
          width = "100%",
          class = "btn-outline-primary"
        ),
        reactable::reactableOutput(outputId = ns("results"))
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
criterion_b_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      rv <- reactiveValues()

      observeEvent(input$launch, {
        data <- req(data_r())
        data <- data %>%
          dplyr::select(.__latitude, .__longitude, .__taxa)
        shinybusy::show_modal_spinner(
          spin = "half-circle",
          color = "#2472b5",
          text = "Extent of Occurrences multi-taxa computation"
        )
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
          Cell_size_locations = input$locations_size
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
        rv$eoo_res <- eoo_res
        rv$aoo_res <- aoo_res
        rv$locations <- locations
        rv$categories <- categories
        rv$results <- results
      })


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
