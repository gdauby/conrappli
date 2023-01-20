
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
#' @name module-mapping
#'
#' @importFrom shiny NS uiOutput fluidRow column actionButton absolutePanel checkboxInput
#' @importFrom htmltools tagList tags
#' @importFrom leaflet leafletOutput
mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(

    alert_no_data(id = ns("no-data")),

    tags$div(
      style = htmltools::css(
        position = "fixed",
        top = "56px",
        left = "0",
        right = "0",
        bottom = "0",
        overflow = "hidden",
        padding = "0"
      ),
      leafletOutput(outputId = ns("map"), height = "100%")
    ),

    absolutePanel(
      top = "80px",
      left = "20px",
      width = "250px",
      style = htmltools::css(
        background = "#FFF",
        borderRadius = "5px",
        padding = "7px"
      ),
      # verbatimTextOutput(ns("test")),
      uiOutput(outputId = ns("summary")),
      tags$br(),
      tags$div(
        id = ns("container-show_in"),
        checkboxInput(
          inputId = ns("show_in"),
          label = "Show only selected points (IN)",
          value = FALSE
        )
      ),
      uiOutput(outputId = ns("filter_coord_accuracy")),
      uiOutput(outputId = ns("filter_year")),
      fluidRow(
        class = "mt-3 mb-3",
        column(
          width = 6,
          actionButton(
            inputId = ns("remove"),
            label = "Remove points",
            width = "100%",
            icon = ph_i("prohibit"),
            class = "btn-outline-danger",
            style = "height: 70px; padding: 3px;"
          )
        ),
        column(
          width = 6,
          actionButton(
            inputId = ns("cancel"),
            label = "Restore original data",
            width = "100%",
            icon = ph_i("arrow-counter-clockwise"),
            class = "btn-outline-primary",
            style = "height: 70px; padding: 3px;"
          )
        )
      )
    ),

    absolutePanel(
      id = ns("container-spatial-overlap"),
      bottom = "20px",
      left = "20px",
      width = "250px",
      style = htmltools::css(
        background = "#FFF",
        borderRadius = "5px",
        padding = "7px"
      ),
      uiOutput(outputId = ns("filter_taxa")),
      tags$div(
        id = ns("container-spatial-overlap"),
        shinyWidgets::virtualSelectInput(
          inputId = ns("spatial_data_select"),
          label = "Spatial data to use :",
          choices = NULL,
          multiple = TRUE,
          hasOptionDescription = TRUE,
          showValueAsTags = TRUE,
          disableSelectAll = TRUE,
          zIndex = 10,
          width = "100%"
        )
      )
    )
  )
}


#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-mapping
#'
#' @importFrom shiny moduleServer reactive observeEvent reactiveValues observe
#'  updateActionButton req outputOptions selectizeInput sliderInput removeUI
#' @importFrom leaflet renderLeaflet leaflet addTiles leafletProxy clearMarkers addMarkers
#' @importFrom utils hasName
#' @importFrom stats setNames
mapping_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      ns <- session$ns
      jns <- function(x) paste0("#", ns(x))

      rv <- reactiveValues()
      data_rv <- reactiveValues()
      rect_rv <- reactiveValues()

      observeEvent(input$show_in, {
        datamap <- req(data_r())
        limit <- get_max_obs()
        if (isTruthy(limit) && is.numeric(limit)) {
          if (isTRUE(nrow(datamap) <= limit)) {
            rv$show_in <- input$show_in
          }
        }
      }, ignoreInit = TRUE)


       # Overlaping spatial data ----------------
      observeEvent(data_r(), {
        data <- req(data_r())
        check_overlap <- extract_overlap_shp(XY = data, col_x = ".__longitude", col_y = ".__latitude")
        rv$check_overlap <- check_overlap
        if (!(all(check_overlap$shp_tables$overlap))) {
          rv$spatial_data <- NULL
          rv$all_shp <- NULL
          shinyjs::addClass(id = "container-spatial-overlap", class = "d-none")
        } else {
          list_spatial_data <- check_overlap$shp_tables %>%
            dplyr::select(table_name, type, description, reference)
          shinyWidgets::updateVirtualSelect(
            inputId = "spatial_data_select",
            choices = list_spatial_data %>%
              shinyWidgets::prepare_choices(label = table_name, value = table_name, description = description)
          )
          shinyjs::removeClass(id = "container-spatial-data", class = "d-none")
          shinybusy::show_modal_spinner(
            spin = "half-circle",
            color = "#088A08",
            text = "Collecting spatial data"
          )
          rv$all_shp <- collect_shp(
            table_names = check_overlap$shp_tables,
            XY_sf = check_overlap$XY_sf
          )
          shinybusy::remove_modal_spinner()
        }
      })
      observeEvent(input$spatial_data_select, {
        proxy <- leaflet::leafletProxy(mapId = "map") %>%
          leaflet::clearShapes()
        rv$spatial_data <- NULL
        if (isTruthy(input$spatial_data_select)) {
          rv$spatial_data <- rv$all_shp[input$spatial_data_select]
          lapply(
            X = rv$spatial_data,
            FUN = function(x) {
              if (inherits(x, "sf")) {
                proxy %>%
                  leaflet::addPolygons(data = x, weight = 1, color = "#088A08")
              }
            }
          )
        }
      }, ignoreNULL = FALSE)


      observeEvent(list(data_r(), input$cancel), {
        req(
          data_r(),
          hasName(data_r(), ".__latitude"),
          hasName(data_r(), ".__longitude"),
          hasName(data_r(), "STATUS_CONR")
        )
        shinyjs::addClass(id = "no-data", class = "d-none")
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

        limit <- get_max_obs()
        if (isTruthy(limit) && is.numeric(limit)) {
          if (isTRUE(nrow(datamap) > limit)) {
            # removeUI(selector = jns("container-show_in"), immediate = TRUE)
            rv$show_in <- TRUE
            datamap <- dplyr::mutate(
              datamap,
              .__display_taxa = .__taxa == dplyr::first(.__taxa)
            )
          }
        }
        # check_datamap <<- datamap
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
              # .__display_taxa == TRUE &
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
        taxas <- unique(datamap$.__taxa)
        limit <- get_max_obs()
        choices <- list(
          "All" = list("All"),
          "Species" = as.list(taxas)
        )
        selected <- "All"
        if (isTruthy(limit) && is.numeric(limit)) {
          if (isTRUE(nrow(datamap) > limit)) {
            choices <- as.list(taxas)
            selected <- taxas[1]
          }
        }
        shinyWidgets::virtualSelectInput(
          inputId = ns("taxa"),
          label = "Taxa to display:",
          choices = choices,
          selected = selected,
          search = TRUE,
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
        req(data_map_r())
        pal <- leaflet::colorFactor(
          palette = c("forestgreen", "firebrick"),
          domain = c(TRUE, FALSE),
          levels = c(TRUE, FALSE)
        )
        data_map <- dplyr::filter(data_map_r(), .__display_taxa == TRUE)
        if (isTRUE(rv$show_in)) {
          data_map <- dplyr::filter(data_map, .__selected == TRUE)
        }
        leaflet::leaflet(data = data_map, options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
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
            popup = data_map %>%
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
            .__display_year = dplyr::between(.__year, years[1], years[2]) | is.na(.__year)
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
      output$test <- shiny::renderPrint({
        rectangles <<- reactiveValuesToList(rect_rv)$x
        length(rectangles)
      })

      observeEvent(rect_rv$x, {
        req(data_map_r())
        rect <- reactiveValuesToList(rect_rv)$x
        if (length(rect) > 0) {
          rectangles <- geojson_to_sf(rect) %>%
            sf::st_combine()
          selected <- data_map_r() %>%
            dplyr::filter(.__selected == TRUE) %>%
            pts_in_poly(rectangles)
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
        selected <- data_map_r() %>%
          dplyr::filter(.__selected == TRUE) %>%
          sf::st_intersection(rectangles)
        data_map$.__selected[data_map$.__id %in% selected$.__id] <- FALSE
        data_rv$map <- data_map
        rect_rv$x <- NULL
      })


      output$summary <- renderUI({
        data_map <- req(data_map_r())
        # check_summary <<- data_map
        tagList(
          tags$div(
            "Points SELECTED:", sum(data_map[[".__selected"]]),
            style = htmltools::css(color = "forestgreen", fontWeight = "bold")
          ),
          tags$div(
            "Points EXCLUDED:", sum(!data_map[[".__selected"]]),
            style = htmltools::css(color = "firebrick", fontWeight = "bold")
          )
        )
      })

      returned_rv <- reactiveValues(x = NULL)

      observeEvent(data_map_r(), {
        req(data_map_r())
        points <- data_map_r()
        returned_rv$x <- dplyr::bind_cols(
          sf::st_coordinates(points) %>%
            as_tibble() %>%
            setNames(c(".__longitude", ".__latitude")),
          sf::st_drop_geometry(points) %>%
            dplyr::mutate(STATUS_CONR = ifelse(.__selected == TRUE, STATUS_CONR, "OUT"))
        )
      })

      return(list(
        data = reactive(returned_rv$x),
        threat_sig = reactive(rv$spatial_data),
        taxa = reactive(input$taxa),
        data_sf = data_map_r
      ))
    }
  )
}
