
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton
draw_poly_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    leaflet::leafletOutput(outputId = ns("map"), height = "500px"),
    tags$div(
      class = "mb-3 mt-3",
      actionButton(
        inputId = ns("confirm"),
        label = "Confirm",
        class = "btn-outline-primary",
        width = "100%"
      )
    )
  )
}

#' @importFrom leaflet renderLeaflet leaflet leafletOptions setView invokeMethod addProviderTiles providers addLayersControl layersControlOptions
#' @importFrom leafpm addPmToolbar pmToolbarOptions pmDrawOptions pmEditOptions pmCutOptions
#' @importFrom sf st_combine
#' @importFrom shiny moduleServer reactiveValues observeEvent eventReactive reactiveValuesToList
draw_poly_server <- function(id) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {

      polys_rv <- shiny::reactiveValues()

      output$map <- leaflet::renderLeaflet({
        base_map() %>%
          leafpm::addPmToolbar(
            toolbarOptions = leafpm::pmToolbarOptions(
              drawMarker = FALSE,
              drawPolyline = FALSE,
              drawPolygon = TRUE,
              drawCircle = FALSE,
              drawRectangle = TRUE,
              cutPolygon = FALSE,
              removalMode = TRUE,
              position = "topright"
            ),
            drawOptions = leafpm::pmDrawOptions(
              snappable = FALSE,
              allowSelfIntersection = FALSE
            ),
            editOptions = leafpm::pmEditOptions(
              preventVertexEdit = TRUE
            ),
            cutOptions = leafpm::pmCutOptions(
              snappable = FALSE,
              allowSelfIntersection = FALSE
            )
          )
      })

      shiny::observeEvent(input$map_draw_new_feature, {
        poly <- input$map_draw_new_feature
        polys_rv$x[[paste0("poly", poly$properties$edit_id)]] <- poly
      })
      observeEvent(input$map_draw_edited_features, {
        poly <- input$map_draw_edited_features
        polys_rv$x[[paste0("poly", poly$properties$edit_id)]] <- poly
      })
      shiny::observeEvent(input$map_draw_deleted_features, {
        poly <- input$map_draw_deleted_features
        polys_rv$x[[paste0("poly", poly$properties$edit_id)]] <- NULL
      })

      poly_r <- shiny::eventReactive(input$confirm, {
        polys <- shiny::reactiveValuesToList(polys_rv)$x
        if (length(polys) > 0) {
          shinyWidgets::execute_safely(
            geojson_to_sf(polys) %>%
              sf::st_combine() %>%
              sf::st_cast(to = "POLYGON") %>%
              sf::st_cast(to = "MULTIPOLYGON")
          )
        } else {
          NULL
        }
      })

      return(poly_r)
    }
  )
}

