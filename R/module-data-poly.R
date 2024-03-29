
#' @importFrom shiny NS actionButton uiOutput
#' @importFrom htmltools tags tagList css
data_poly_ui <- function(id) {
  ns <- NS(id)
  template_ui(
    title = i18n("Choice of the site"),

    tags$p(
      i18n("Define your study area either by:"),
      tags$ul(
        tags$li(i18n("Drawing a polygon on the map ('Draw on map' tab and drawing tools on the right of the map to define a polygon)")),
        tags$li(i18n("By importing a shapefile from your computer ('Read a file' tab, all shapefile files must be loaded)"))
      )
    ),

    data_filterout_ui(id = ns("filterout")),
    
    data_source_ui(id = ns("source")),
    
    conditionalPanel(condition = "input['source-source_data'] == 'GBIF'", ns = ns,
                         numericInput(
                           inputId = ns("gbif_filterout"),
                           label = tagList(
                             i18n("Threshold to filter out taxa with high number of occurrences"),
                             btn_help(
                               i18n("")
                             )
                           ),
                           min = 20,
                           max = 5000,
                           value = 500,
                           step = 1,
                           width = "100%"
                         )
                     ),
    
    data_import_polygon_ui(id = ns("read")),
    uiOutput(outputId = ns("feedback"), class = "my-3"),

    actionButton(
      inputId = ns("go_next"),
      label = tagList(
        i18n("Continue to criterion B evaluation"),
        ph("arrow-circle-right")
      ),
      class = "btn-primary",
      disabled = "disabled",
      width = "100%"
    ),

    tags$br(),
    tags$br(),

    tags$button(
      class = "btn btn-outline-primary",
      role = "button",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", ns("variable-container")),
      i18n("See variable selection"),
      phosphoricons::ph("caret-down", title = i18n("See variable selection"))
    ),
    tags$button(
      class = "btn btn-outline-primary",
      role = "button",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", ns("validation-container")),
      i18n("See data validation"),
      phosphoricons::ph("caret-down", title = i18n("See data validation"))
    ),
    tags$div(
      class = "collapse",
      id = ns("variable-container"),
      data_variable_ui(ns("variable"))
    ),
    tags$div(
      class = "collapse",
      id = ns("validation-container"),
      data_validation_ui(ns("validation"))
    )
  )
}

#' @importFrom shiny moduleServer reactiveValues observeEvent req renderUI
#'  eventReactive isTruthy reactive
#' @importFrom shinyWidgets alert execute_safely
data_poly_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      dataset_rv <- reactiveValues(value = NULL)
      
      source <- reactiveValues(value = NULL)
      # gbif_threshold_rv <- reactiveValues(value = NULL)
      
      data_source <- data_source_server(id = "source")
      
      observeEvent(data_source(), 
                   source$value <- data_source())
      
      
      gbif_threshold_rv <- reactive({
        input$gbif_filterout
      })
      
      
      # observeEvent(data_source(), {
      #   if (identical("GBIF", source$value))
      #     gbif_threshold$value <- output$gbif_threshold
      #   
      # })
      
       # observe(print(gbif_threshold_rv()))
      
      # observeEvent(data_source(), 
      #              source$value <- data_source())
                    
      # ddd <- isolate(source$value())
      # 
      # print(ddd)}
      

      polygon_read_r <- data_import_polygon_server(id = "read", 
                                                   source_r = reactive({data_source()}),
                                                   threshold_gbif = reactive({gbif_threshold_rv()})
      )
      
      # ,
      # threshold_gbif = reactive({gbif_threshold$value})
      
      
      observeEvent(polygon_read_r$value(), dataset_rv$value <- polygon_read_r$value())
      observeEvent(polygon_read_r$poly(), dataset_rv$poly <- polygon_read_r$poly())

      output$feedback <- renderUI({
        if (isTruthy(dataset_rv$value)) {
          n <- nrow(dataset_rv$value)
          
          if (identical("RAINBIO", source$value)) {
            nbe_esp <- length(unique(dataset_rv$value$tax_sp_level))
            msg <- 
              "records successfully downloaded from Rainbio. Max first 1000 lines displayed below."
          }
          
          if (identical("GBIF", source$value)) {
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
            format(nbe_esp, big.mark = ","), i18n("species"),
            ph("check"),
            format(n, big.mark = ","), msg
          )
          
        }
      })

      variable_r <- data_variable_server(
        id = "variable",
        data_r = reactive({
          req(dataset_rv$value)
        })
      )

      data_validated_r <- data_validation_server(
        id = "validation",
        data_r = reactive({
          req(variable_r())
          variable_r()$data
        })
      )

      data_filterout_r <- data_filterout_server(
        id = "filterout",
        data_r = reactive({
          req(data_validated_r())
          data_validated_r()
        })
      )

      observeEvent(data_filterout_r(), {
        shinyjs::enable(id = "go_next")
      })


      final_data_r <- eventReactive(input$go_next, {
        data_filterout_r()
      })

      return(list(
        data = final_data_r,
        data_latlon = reactive(variable_r()$data_latlon),
        poly = reactive(dataset_rv$poly)
      ))
    }
  )
}



#' @importFrom shiny insertUI
#' @importFrom htmltools css tags
show_spinner <- function(text) {
  insertUI(
    selector = ".modal-content",
    immediate = TRUE,
    ui = tags$div(
      id = "conr-modal_inner-spinner",
      style = htmltools::css(
        position = "absolute",
        top = 0,
        right = 0,
        bottom = 0,
        left = 0,
        background = "#FAFAFA",
        opacity = 0.8,
        zIndex = 99999,
        display = "flex",
        justifyContent = "center",
        alignItems = "center"
      ),
      shinybusy::html_dependency_epic(),
      tags$div(
        class = "shinybusy-modal-spinner",
        style = "position: relative; margin: auto; opacity: 1;",
        htmltools::tagAppendAttributes(
          shinybusy::spin_epic(spin = "fulfilling-bouncing-circle", color = "#088A08"),
          style = "margin: auto;"
        ),
        tags$div(
          class = "shinybusy-modal-text",
          style = "text-align: center; opacity: 1;",
          text
        )
      )
    )
  )
}

#' @importFrom shiny removeUI
remove_spinner <- function() {
  removeUI(selector = "#conr-modal_inner-spinner", immediate = TRUE)
}
