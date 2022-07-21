
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton
draw_poly_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$h4("Draw polygon"),
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
        leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
          leaflet::setView(0, 0, 2) %>%
          leaflet::invokeMethod(data = NULL, method = "addZoom", list(position = "topright")) %>%
          leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri") %>%
          leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "Open Topo Map") %>%
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Esri", "Open Topo Map"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          ) %>%
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
          geojson_to_sf(polys) %>%
            sf::st_combine() %>%
            sf::st_cast(to = "POLYGON")
        } else {
          NULL
        }
      })

      return(poly_r)
    }
  )
}
