
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
    fluidRow(
      column(
        width = 4,
        uiOutput(outputId = ns("filter_year"))
      ),
      column(
        width = 4,
        uiOutput(outputId = ns("filter_coord_accuracy"))
      )
    ),
    leafletOutput(outputId = ns("map"), height = "500px"),
    fluidRow(
      class = "mt-3 mb-3",
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
    ),
    actionButton(
      inputId = ns("validate"),
      label = "Validate selection",
      width = "100%",
      icon = icon("check"),
      class = "btn-outline-primary"
    )
  )
}


#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-data-map
#'
#' @importFrom shiny moduleServer reactive observeEvent reactiveValues observe
#'  updateActionButton req outputOptions selectizeInput sliderInput
#' @importFrom leaflet renderLeaflet leaflet addTiles leafletProxy clearMarkers addMarkers
data_map_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues()

      observeEvent(list(data_r(), input$cancel), {
        req(data_r(), hasName(data_r(), ".__latitude"), hasName(data_r(), ".__longitude"))
        datamap <- data_r() %>%
          dplyr::mutate(
            id = seq_len(dplyr::n()),
            display_year = TRUE,
            display_coord_accuracy = TRUE,
            selected = TRUE
          ) %>%
          coord_accuracy(col_x = ".__longitude", col_y = ".__latitude")
        pts_sf <- sf::st_as_sf(datamap, coords = c(".__latitude", ".__longitude"))
        data_rv$map <- pts_sf
        returned_rv$x <- NULL
      })


      shared_map <- crosstalk::SharedData$new(reactive({
        req(data_rv$map) %>%
          dplyr::filter(
            display_year == TRUE,
            display_coord_accuracy == TRUE,
            selected == TRUE
          )
      }), key = ~id)

      output$filter_year <- renderUI({
        datamap <- req(data_r())
        if (hasName(datamap, ".__year")) {
          selectizeInput(
            inputId = ns("year"),
            label = "Year:",
            choices = sort(unique(datamap$.__year)),
            selected = sort(unique(datamap$.__year)),
            multiple = TRUE,
            width = "100%",
            options = list(
              plugins = list("remove_button")
            )
          )
        }
      })

      output$filter_coord_accuracy <- renderUI({
        datamap <- req(data_r())
        sliderInput(
          inputId = ns("coord_accuracy"),
          label = "Accuracy of coordinates:",
          min = 1,
          max = 8,
          value = c(1, 8),
          step = 1,
          width = "100%"
        )
      })


      output$map <- renderLeaflet({
        leaflet::leaflet(data = shared_map) %>%
          leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri") %>%
          leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "Open Topo Map") %>%
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Esri", "Open Topo Map"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          ) %>%
          leaflet::addCircleMarkers(fillOpacity = 0, opacity = 0) %>%
          leaflet::addMarkers(clusterOptions = leaflet::markerClusterOptions())
      })


      observeEvent(input$year, {
        req(input$year)
        data_rv$map <- data_rv$map %>%
          dplyr:::mutate(display_year = .__year %in% input$year)
      })

      observeEvent(input$coord_accuracy, {
        req(input$coord_accuracy)
        data_rv$map <- data_rv$map %>%
          dplyr:::mutate(
            display_coord_accuracy = dplyr:::between(calc_accuracy, input$coord_accuracy[1], input$coord_accuracy[2])
          )
      })


      observe({
        req(shared_map)
        data_sel <- shared_map$data(withSelection = TRUE)
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

      # Remove points
      observeEvent(input$remove, {
        req(data_rv$map)
        data_sel <- shared_map$data(withSelection = TRUE)
        id_selected <- data_sel %>% dplyr::filter(selected_ == TRUE) %>% pull(id)

        data_map <- data_rv$map
        data_map$selected[data_map$id %in% id_selected] <- FALSE
        data_rv$map <- data_map

      })


      returned_rv <- reactiveValues(x = NULL)

      observeEvent(input$validate, {
        returned_rv$x <- data_rv$map %>%
          dplyr::filter(selected == TRUE) %>%
          dplyr::select(-dplyr::any_of(c("selected", "display"))) %>%
          sf::st_drop_geometry()
      })

      return(reactive(returned_rv$x))
    }
  )
}
