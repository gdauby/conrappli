
#' Map point selection Module
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
        uiOutput(outputId = ns("filter_coord_accuracy"))
      ),
      column(
        width = 4,
        uiOutput(outputId = ns("filter_taxa"))
      ),
      column(
        width = 4,
        uiOutput(outputId = ns("filter_year"))
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
#' @importFrom utils hasName
data_map_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns

      data_rv <- reactiveValues()

      observeEvent(list(data_r(), input$cancel), {
        req(
          data_r(),
          hasName(data_r(), ".__latitude"),
          hasName(data_r(), ".__longitude"),
          hasName(data_r(), "STATUS_CONR")
        )
        datamap <- data_r() %>%
          dplyr::mutate(
            id = seq_len(dplyr::n()),
            display_year = TRUE,
            display_coord_accuracy = TRUE,
            display_taxa = TRUE,
            selected = TRUE,
            .__longitude = ifelse(is.na(.__longitude), 0, .__longitude),
            .__latitude = ifelse(is.na(.__latitude), 0, .__latitude)
          ) %>%
          coord_accuracy(col_x = ".__longitude", col_y = ".__latitude")
        pts_sf <- sf::st_as_sf(datamap, coords = c(".__longitude", ".__latitude"))
        data_rv$map <- pts_sf
        returned_rv$x <- NULL
      })


      shared_map <- crosstalk::SharedData$new(reactive({
        req(data_rv$map) %>%
          dplyr::filter(
            STATUS_CONR == "IN",
            display_year == TRUE,
            display_coord_accuracy == TRUE,
            display_taxa == TRUE,
            selected == TRUE
          )
      }), key = ~id)

      output$filter_year <- renderUI({
        datamap <- req(data_r())
        if (is_valid_year_col(datamap)) {
          values <- range(datamap$.__year, na.rm = TRUE)
          sliderInput(
            inputId = ns("year"),
            label = "Year:",
            min = values[1],
            max = values[2],
            value = values,
            sep = "",
            width = "100%"
          )
        }
      })

      output$filter_taxa <- renderUI({
        datamap <- req(data_r())
        selectizeInput(
          inputId = ns("taxa"),
          label = "Taxa:",
          choices = list(
            " " = list("All"),
            "Species" = as.list(unique(datamap$.__taxa))
          ),
          width = "100%"
        )
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
        years <- req(input$year)
        data_rv$map <- data_rv$map %>%
          dplyr::mutate(
            display_year = dplyr::between(.__year, years[1], years[2])
          )
      })

      observeEvent(input$coord_accuracy, {
        coords <- req(input$coord_accuracy)
        data_rv$map <- data_rv$map %>%
          dplyr::mutate(
            display_coord_accuracy = dplyr::between(calc_accuracy, coords[1], coords[2])
          )
      })
      observeEvent(input$taxa, {
        req(input$taxa)
        if (identical(input$taxa, "All")) {
          data_rv$map <- data_rv$map %>%
            dplyr::mutate(
              display_taxa = TRUE
            )
        } else {
          data_rv$map <- data_rv$map %>%
            dplyr::mutate(
              display_taxa = .__taxa == input$taxa
            )
        }
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
        req(data_rv$map)
        selected <- data_rv$map %>%
          dplyr::mutate(STATUS_CONR = ifelse(selected == TRUE, STATUS_CONR, "OUT"))
        returned_rv$x <- data_r() %>%
          dplyr::mutate(STATUS_CONR = selected$STATUS_CONR)
      })

      return(reactive(returned_rv$x))
    }
  )
}
