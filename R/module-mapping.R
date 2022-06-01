
#' Mapping module
#'
#' @param id Module's ID.
#'
#' @export
#' @name module-mapping
mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = htmltools::css(
        position = "fixed",
        top = "65px",
        left = 0,
        right = 0,
        bottom = 0,
        overflow = "hidden",
        padding = 0
      ),
      leafletOutput(outputId = ns("map"), width = "100%", height = "100%")
    )
  )
}

#' @export
#'
#' @rdname module-mapping
mapping_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      output$map <- renderLeaflet({
        req(data_r())
        pal <- leaflet::colorFactor(
          palette = c("navy", "red"),
          domain = c("IN", "OUT")
        )
        donnees_popup <- data_r() %>%
          dplyr::select(!dplyr::starts_with(".__"))

        labels_variables <- lapply(
          X = names(donnees_popup),
          FUN = function(x) {
            paste0(x, ": ", donnees_popup[[x]])
          })
        tableau_donnees_popup <- data.frame(labels_variables)
        colnames(tableau_donnees_popup) <- names(donnees_popup)

        resume_donnees_popup <- tidyr::unite(tableau_donnees_popup, col = resume, everything(), sep = "<br/>")
        popup <- resume_donnees_popup$resume
        popup <- lapply(popup, htmltools::HTML)

        leaflet::leaflet(data = data_r()) %>%
          leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri") %>%
          leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "Open Topo Map") %>%
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Esri", "Open Topo Map"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          ) %>%
          leaflet::setView(lng = 11.5, lat = 2, zoom = 8) %>%
          leaflet::addCircleMarkers(
            lng = ~.__longitude,
            lat = ~.__latitude,
            radius = ~ifelse(STATUS_CONR == "TRUE", 4, 6),
            color = ~pal(STATUS_CONR),
            stroke = FALSE,
            opacity = 0.7,
            fillOpacity = 0.7,
            popup = popup,
          ) %>%
          leaflet::addLegend(
            pal = pal,
            values = c("IN", "OUT"),
            opacity = 1,
            title = "Status",
            position = "bottomright",
            na.label = "N/A",
          )
      })
    }
  )
}

