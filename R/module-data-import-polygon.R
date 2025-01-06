
#' Import data from Rainbio via polygon drawing Module
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data-polygon
#'
#' @importFrom shiny NS fluidRow column checkboxInput
#' @importFrom htmltools tagList
#' @importFrom bslib navset_hidden nav_panel
data_import_polygon_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_pill(
      header = tags$br(),
      bslib::nav_panel(
        title = "Draw on map",
        tags$h5("Draw polygon", class = "mt-0"),
        tags$p(
          "Click buttons representing a rectangle or a polygon",
          "on the right of the map to draw a shape on the map,",
          "then click the confirm button below the map to import data about the concerned area."
        ),
        draw_poly_ui(id = ns("draw"))
      ),
      bslib::nav_panel(
        title = "Read a file",
        read_poly_ui(id = ns("read"))
      )
    ),
    uiOutput(outputId = ns("feedback")),
    uiOutput(outputId = ns("alert_max_obs")),
    reactable::reactableOutput(outputId = ns("table"), height = 500),
    tags$br(),
    tags$br()
  )
}

#' @export
#'
#' @rdname module-data-polygon
#'
#' @importFrom shiny moduleServer observeEvent reactive
#' @importFrom utils read.csv head
data_import_polygon_server <- function(id, source_r = reactive(NULL), threshold_gbif = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {

      polygon_rv <- reactiveValues(x = NULL)
      dataset_rv <- reactiveValues(value = NULL)
      source_rv <- reactiveValues(value = NULL)
      threshold_rv <- reactiveValues(value = NULL)
      
      observeEvent(source_r(), 
                   source_rv$value <- source_r())
      
      observeEvent(threshold_gbif(), 
                   threshold_rv$value <- threshold_gbif())
      
      

      
      # ddd <- isolate(source_r())
      # # 
      # print(ddd)

      polygon_draw_r <- draw_poly_server(id = "draw")
      observeEvent(polygon_draw_r(), polygon_rv$x <- polygon_draw_r())

      polygon_read_r <- read_poly_server(id = "read")
      observeEvent(polygon_read_r(), polygon_rv$x <- polygon_read_r())
      
      # observe(print(class(st_as_sf(polygon_draw_r()))))
      # 
      # observe(print(polygon_rv$x))
      
      observe(print(polygon_draw_r()))
      observe(print(lwgeom::st_is_polygon_cw(polygon_draw_r())))
      # observe(print(polygon_rv$x))
      
      observeEvent(polygon_rv$x, {
        req(polygon_rv$x)
        shinybusy::show_modal_spinner(
          spin = "fulfilling-bouncing-circle",
          color = "#088A08",
          text = "Retrieving data, please wait..."
        )
        
        if (identical(source_rv$value, "GBIF")) {
          occdata <- shinyWidgets::execute_safely({
            query_gbif_poly(poly = polygon_rv$x, threshold = threshold_rv$value)
          })
        }
        
        
        # print("check identical")
        observe(print(source_r()))
        # 
        # print(identical(source_rv$value, "RAINBIO"))
        
        if (identical(source_rv$value, "RAINBIO")) {
          occdata <- shinyWidgets::execute_safely({
            query_rb_poly(poly = polygon_rv$x)
          })
        }
        
        shinybusy::remove_modal_spinner()
        dataset_rv$value <- occdata$extract_all_tax
        
        # observe(print(dataset_rv$value))
        
        
      })

      output$feedback <- renderUI({
        if (isTruthy(dataset_rv$value)) {
          n <- nrow(dataset_rv$value)
          
          # print(dataset_rv$value)
          
          if (identical(source_rv$value, "RAINBIO")) {
            nbe_esp <- length(unique(dataset_rv$value$tax_sp_level))
            msg <- 
              "records successfully downloaded from Rainbio. Max first 1000 lines displayed below."
          }
          
          if (identical(source_rv$value, "GBIF")) {
            nbe_esp <- dataset_rv$value %>% 
              select(genus, species) %>% 
              filter(!is.na(species)) %>% 
              distinct() %>% nrow()
            msg <- 
              "records successfully downloaded from Gbif Max first 1000 lines displayed below."
            
          }
          
          shinyWidgets::alert(
            status = "success",
            ph("check"),
            format(nbe_esp, big.mark = ","), "species",
            ph("check"),
            format(n, big.mark = ","), msg
          )
        }
      })

      output$alert_max_obs <- renderUI({
        if (isTruthy(dataset_rv$value)) {
          n <- nrow(dataset_rv$value)
          limit <- get_max_obs()
          if (isTruthy(limit) && is.numeric(limit)) {
            if (isTRUE(n > limit)) {
              shinyWidgets::alert(
                status = "warning",
                ph("warning"),
                "The volume of imported data is large, which may slow down the",
                "operations performed in the other parts of the application"
              )
            }
          }
        }
      })

      output$table <- reactable::renderReactable({
        req(dataset_rv$value)
        reactable::reactable(
          data = head(dataset_rv$value, 1000),
          compact = TRUE,
          bordered = TRUE,
          pagination = FALSE,
          searchable = TRUE,
          resizable = TRUE,
          defaultColDef = reactable::colDef(
            style = list(whiteSpace = "nowrap", textOverflow = "ellipsis")
          ),
          theme = reactable_theme()
        )
      })


      return(list(
        value = reactive(dataset_rv$value),
        poly = reactive(polygon_rv$x)
      ))
    }
  )
}
