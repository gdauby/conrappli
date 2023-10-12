
library(dplyr)
pkgload::load_all()
library(shiny)
library(sf)

ui <- fluidPage(
  theme = bs_theme_conr(),
  shinyjs::useShinyjs(),
  tags$script(src = "conrappli/js/script.js"),
  tags$style(".selectize-dropdown, .selectize-dropdown.form-control {z-index: 1060 !important;}"),
  draw_poly_ui("ID"),
  plotOutput(outputId = "draw_poly")
)

server <- function(input, output, session) {

  ns <- session$ns

  result <- draw_poly_server("ID")

  output$draw_poly <- renderPlot({
    req(result())
    polygone <<- result()
    plot((st_geometry(result())))
  })

}

shinyApp(ui, server)
