library(pkgload)
library(shiny)
pkgload::load_all()
options(shiny.autoload.r = FALSE)
shiny::shinyApp(
  ui = conr_ui(),
  server = conr_server()
)
