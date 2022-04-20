
#' @title Map point selection Module
#'
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `list`.

#'
#' @name module-data-map
#'
#' @importFrom shiny NS uiOutput fluidRow column actionButton
#' @importFrom htmltools tagList tags
#' @importFrom leaflet leafletOutput
data_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(outputId = ns("map"), height = "500px"),
    fluidRow(
      class = "mt-3",
      column(
        width = 6,
        actionButton(
          inputId = ns("remove"),
          label = "Remove selected points",
          width = "100%",
          icon = icon("ban"),
          class = "btn-outline-danger"
        )
      ),
      column(
        width = 6,
        actionButton(
          inputId = ns("cancel"),
          label = "Restore original data",
          width = "100%",
          icon = icon("undo"),
          class = "btn-outline-primary"
        )
      )
    )
  )
}


#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-data-map
#'
#' @importFrom shiny moduleServer reactive observeEvent reactiveValues observe updateActionButton req outputOptions
#' @importFrom leaflet renderLeaflet leaflet addTiles leafletProxy clearMarkers addMarkers
data_map_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      data_rv <- reactiveValues(init = NULL, select = NULL, ts = NULL)

      observeEvent(data_r(), {
        # datamap <- req(data_r()) %>%
        #   dplyr::group_by(.__latitude, .__longitude) %>%
        #   dplyr::summarise(n = dplyr::n(), id = dplyr::cur_group_id())
        datamap <- req(data_r()) %>%
          dplyr::mutate(id = seq_len(dplyr::n()))
        pts_sf <- sf::st_as_sf(datamap, coords = c(".__latitude", ".__longitude"))
        pts_sf <- crosstalk::SharedData$new(pts_sf, key = ~id)
        data_rv$init <- data_rv$select <- pts_sf
      })



      output$map <- renderLeaflet({
        basemap <- leaflet::leaflet(data = data_rv$init) %>%
          leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri") %>%
          leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "Open Topo Map") %>%
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Esri", "Open Topo Map"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
        if (!is.null(data_rv$init)) {
          basemap <- basemap %>%
            leaflet::addCircleMarkers(fillOpacity = 0, opacity = 0) %>%
            leaflet::addMarkers(clusterOptions = leaflet::markerClusterOptions())
        }
        basemap
      })

      observe({
        req(data_rv$select)
        data_sel <- data_rv$select$data(withSelection = TRUE)
        n <- sum(data_sel$selected_)
        if (is.na(n)) {
          label <- "No points selected"
          shinyjs::disable(id = "remove")
        } else {
          label <- sprintf("Remove %s selected point(s)", n)
          shinyjs::enable(id = "remove")
        }
        updateActionButton(
          session = session,
          inputId = "remove",
          label = label
        )
      })

      # Cancel selection
      observeEvent(input$cancel, {
        req(data_rv$init)
        data_rv$select <- data_rv$init
        leafletProxy("map", data = data_rv$select) %>%
          leaflet::clearMarkers() %>%
          leaflet::clearMarkerClusters() %>%
          leaflet::addCircleMarkers(fillOpacity = 0, opacity = 0) %>%
          leaflet::addMarkers(clusterOptions = leaflet::markerClusterOptions())
      })

      # Remove points
      observeEvent(input$remove, {
        req(data_rv$select)
        data_sel <- data_rv$select$data(withSelection = TRUE)
        new_data <- data_sel %>% dplyr::filter(selected_ == FALSE)
        data_rv$select <- crosstalk::SharedData$new(new_data, key = ~id)
        leafletProxy("map", data = data_rv$select) %>%
          leaflet::clearMarkers() %>%
          leaflet::clearMarkerClusters() %>%
          leaflet::addCircleMarkers(fillOpacity = 0, opacity = 0) %>%
          leaflet::addMarkers(clusterOptions = leaflet::markerClusterOptions())
      })

      return(reactive(NULL))
    }
  )
}
