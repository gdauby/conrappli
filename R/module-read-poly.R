
#' @importFrom leaflet leafletOutput
#' @importFrom shiny NS actionButton fileInput
read_poly_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(
      inputId = ns("file"),
      label = "Import a shapefile:",
      multiple = TRUE,
      accept = c(".shp",".dbf",".sbn",".sbx",".shx",".prj"),
      width = "100%"
    ),
    uiOutput(outputId = ns("error")),
    # verbatimTextOutput(ns("dev")),
    leafletOutput(outputId = ns("map"))
  )
}

#' @importFrom leaflet renderLeaflet leaflet leafletOptions setView invokeMethod addProviderTiles providers addLayersControl layersControlOptions
#' @importFrom leafpm addPmToolbar pmToolbarOptions pmDrawOptions pmEditOptions pmCutOptions
#' @importFrom sf st_combine
#' @importFrom shiny moduleServer reactiveValues observeEvent eventReactive reactiveValuesToList
read_poly_server <- function(id) {
  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {

      # output$dev <- renderPrint(str(rv$polygon))

      rv <- reactiveValues()

      observeEvent(input$file, {
        rv$error <- rv$polygon <- NULL
        file <- input$file
        shp <- file$datapath[endsWith(file$datapath, suffix = ".shp")]
        if (length(shp) != 1) {
          rv$error <- "Invalid shapefile, one imported file must have .shp extension."
        }
        polygon <- shinyWidgets::execute_safely(read_shapefile(input$file))
        if (inherits(polygon, c("sf", "sfc"))) {
          rv$polygon <- polygon
        } else {
          rv$error <- "File read does not return any geometry."
        }
        polygon <- sf::st_zm(polygon)
        print(polygon)
        
      })

      output$error <- renderUI({
        if (!is.null(rv$error)) {
          shinyWidgets::alert(
            status = "danger",
            ph("warning-circle"),
            rv$error
          )
        }
      })

      output$map <- renderLeaflet(base_map())

      observeEvent(rv$polygon, {
        leaflet::leafletProxy(mapId = "map") %>%
          leaflet::addPolygons(data = rv$polygon)
      })

      return(reactive(rv$polygon))
    }
  )
}

read_shapefile <- function(files) {
  file.rename(
    from = files$datapath,
    to = file.path(dirname(files$datapath[1]), files$name)
  )
  sf::read_sf(dirname(files$datapath[1]))
}
