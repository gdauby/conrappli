
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
    
    tags$div(
      style = htmltools::css(
        position = "fixed",
        top = "65px",
        left = "0",
        right = "0",
        bottom = "0",
        overflow = "hidden",
        padding = "0"
      ),
      leafletOutput(outputId = ns("map"), height = "100%")
    ),
    
    absolutePanel(
      bottom = "20px", 
      left = "20px",
      style = htmltools::css(
        background = "#FFF",
        borderRadius = "5px",
        padding = "10px"
      ),
      # verbatimTextOutput(ns("test")),
      uiOutput(outputId = ns("summary")),
      uiOutput(outputId = ns("filter_coord_accuracy")),
      uiOutput(outputId = ns("filter_taxa")),
      uiOutput(outputId = ns("filter_year")),
      fluidRow(
        class = "mt-3 mb-3",
        column(
          width = 6,
          actionButton(
            inputId = ns("remove"),
            label = "Remove points",
            width = "100%",
            icon = icon("ban"),
            class = "btn-outline-danger"
          )
        ),
        column(
          width = 6,
          actionButton(
            inputId = ns("cancel"),
            label = "Restore data",
            width = "100%",
            icon = icon("undo"),
            class = "btn-outline-primary"
          )
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
      rect_rv <- reactiveValues()

      observeEvent(list(data_r(), input$cancel), {
        req(
          data_r(),
          hasName(data_r(), ".__latitude"),
          hasName(data_r(), ".__longitude"),
          hasName(data_r(), "STATUS_CONR")
        )
        datamap <- data_r() %>%
          dplyr::mutate(
            .__id = seq_len(dplyr::n()),
            .__display_year = TRUE,
            .__display_coord_accuracy = TRUE,
            .__display_taxa = TRUE,
            .__selected = TRUE,
            .__longitude = ifelse(is.na(.__longitude), 0, .__longitude),
            .__latitude = ifelse(is.na(.__latitude), 0, .__latitude)
          ) %>%
          coord_accuracy(col_x = ".__longitude", col_y = ".__latitude")
        pts_sf <- sf::st_as_sf(datamap, coords = c(".__longitude", ".__latitude"), crs = 4326)
        data_rv$map <- pts_sf
        returned_rv$x <- NULL
      })


      data_map_r <- reactive({
        req(data_rv$map) %>%
          dplyr::mutate(
            .__selected = STATUS_CONR == "IN" &
              .__display_year == TRUE &
              .__display_coord_accuracy == TRUE &
              .__display_taxa == TRUE &
              .__selected == TRUE
          )
      })

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
        pal <- leaflet::colorFactor(
          palette = c("forestgreen", "firebrick"),
          domain = c(TRUE, FALSE),
          levels = c(TRUE, FALSE)
        )
        leaflet::leaflet(data = data_map_r()) %>%
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
              drawPolygon = FALSE,
              drawCircle = FALSE, 
              drawRectangle = TRUE,
              cutPolygon = FALSE,
              removalMode = FALSE,
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
          ) %>%
          # leaflet::addCircleMarkers(fillOpacity = 0, opacity = 0) %>%
          leaflet::addCircleMarkers(
            popup = data_map_r() %>%
              sf::st_drop_geometry() %>%
              unselect_internal_vars() %>%
              create_popup() %>%
              lapply(htmltools::HTML),
            color = ~ pal(.__selected),
            fillOpacity = 1,
            clusterOptions = leaflet::markerClusterOptions(
              maxClusterRadius = 20, 
              zoomToBoundsOnClick = FALSE
            )
          )
      })


      observeEvent(input$year, {
        years <- req(input$year)
        data_rv$map <- data_rv$map %>%
          dplyr::mutate(
            .__display_year = dplyr::between(.__year, years[1], years[2])
          )
      })

      observeEvent(input$coord_accuracy, {
        coords <- req(input$coord_accuracy)
        data_rv$map <- data_rv$map %>%
          dplyr::mutate(
            .__display_coord_accuracy = dplyr::between(calc_accuracy, coords[1], coords[2])
          )
      })
      observeEvent(input$taxa, {
        req(input$taxa)
        if (identical(input$taxa, "All")) {
          data_rv$map <- data_rv$map %>%
            dplyr::mutate(
              .__display_taxa = TRUE
            )
        } else {
          data_rv$map <- data_rv$map %>%
            dplyr::mutate(
              .__display_taxa = .__taxa == input$taxa
            )
        }
      })
      
      
      observeEvent(input$map_draw_new_feature, {
        rect <- input$map_draw_new_feature
        rect_rv$x[[paste0("rect", rect$properties$edit_id)]] <- rect
      })
      observeEvent(input$map_draw_edited_features, {
        rect <- input$map_draw_edited_features
        rect_rv$x[[paste0("rect", rect$properties$edit_id)]] <- rect
      })
      observeEvent(input$map_draw_deleted_features, {
        rect <- input$map_draw_deleted_features
        rect_rv$x[[paste0("rect", rect$properties$edit_id)]] <- NULL
      })
      output$test <- renderPrint({
        rectangles <<- reactiveValuesToList(rect_rv)$x
        length(rectangles)
      })

      observeEvent(rect_rv$x, {
        req(data_map_r())
        rect <- reactiveValuesToList(rect_rv)$x
        if (length(rect) > 0) {
          rectangles <- geojson_to_sf(rect) %>% 
            sf::st_combine()
          selected <- pts_in_poly(data_map_r(), rectangles)
          n <- sum(selected)
        } else {
          n <- 0
        }
        if (n < 1) {
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
      }, ignoreNULL = FALSE)

      # Remove points
      observeEvent(input$remove, {
        data_map <- req(data_map_r())
        rect <- reactiveValuesToList(rect_rv)$x
        req(length(rect) > 0)
        rectangles <- geojson_to_sf(rect) %>% 
          sf::st_combine()
        selected <- pts_in_poly(data_map_r(), rectangles)
        data_map$.__selected[selected] <- FALSE
        data_rv$map <- data_map
        rect_rv$x <- NULL
      })

      
      output$summary <- renderUI({
        data_map <- req(data_map_r())
        tagList(
          tags$span(
            "Points IN:", sum(data_map[[".__selected"]]),
            style = htmltools::css(color = "forestgreen", fontWeight = "bold")
          ),
          "|",
          tags$span(
            "Points OUT:", sum(!data_map[[".__selected"]]),
            style = htmltools::css(color = "firebrick", fontWeight = "bold")
          )
        )
      })

      returned_rv <- reactiveValues(x = NULL)

      observeEvent(data_map_r(), {
        req(data_map_r())
        returned_rv$x <- data_map_r() %>%
          dplyr::mutate(STATUS_CONR = ifelse(.__selected == TRUE, STATUS_CONR, "OUT"))
      })

      return(reactive(returned_rv$x))
    }
  )
}
