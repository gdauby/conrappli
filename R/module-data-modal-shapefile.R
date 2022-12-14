
#' @importFrom shiny NS modalDialog actionButton uiOutput
#' @importFrom htmltools tags tagList css
modal_import_shapefile_ui <- function(id) {
  ns <- NS(id)
  modalDialog(
    title = tagList(
      tags$button(
        phosphoricons::ph("x", title = "Close", height = "2em"),
        class = "btn btn-link",
        style = css(border = "0 none", position = "absolute", top = "5px", right = "5px"),
        `data-bs-dismiss` = "modal",
        `aria-label` = "Close"
      ),
      "Import a shapefile to retrieve species occurences"
    ),
    size = "xl",
    footer = NULL,
    read_poly_ui(id = ns("read")),
    uiOutput(outputId = ns("feedback"), class = "my-3"),

    actionButton(
      inputId = ns("go_next"),
      label = tagList(
        "Continue to next step",
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
      "See variable selection",
      phosphoricons::ph("caret-down", title = "See variable selection")
    ),
    tags$button(
      class = "btn btn-outline-primary",
      role = "button",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = paste0("#", ns("validation-container")),
      "See data validation",
      phosphoricons::ph("caret-down", title = "See data validation")
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
modal_import_shapefile_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      polygon_rv <- reactiveValues()
      dataset_rv <- reactiveValues(value = NULL)

      polygon_read_r <- read_poly_server(id = "read")
      observeEvent(polygon_read_r(), polygon_rv$x <- polygon_read_r())

      observeEvent(polygon_rv$x, {
        req(polygon_rv$x)
        show_spinner(
          text = "Retrieving data, please wait..."
        )
        occdata <- shinyWidgets::execute_safely({
          query_rb_poly(poly = polygon_rv$x)
        })
        remove_spinner()
        dataset_rv$value <- occdata$extract_all_tax
      })

      output$feedback <- renderUI({
        if (isTruthy(dataset_rv$value)) {
          n <- nrow(dataset_rv$value)
          shinyWidgets::alert(
            status = "success",
            ph("check"),
            format(n, big.mark = ","), "rows successfully downloaded from Rainbio. Max first 1000 lines displayed below."
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


      observeEvent(data_validated_r(), {
        shinyjs::enable(id = "go_next")
      })


      final_data_r <- eventReactive(input$go_next, {
        removeModal()
        data_validated_r()
      })

      return(final_data_r)
    }
  )
}




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

remove_spinner <- function() {
  removeUI(selector = "#conr-modal_inner-spinner", immediate = TRUE)
}
