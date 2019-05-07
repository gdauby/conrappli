#' @export
app <- function() {
  shiny::runApp(system.file("shinyApp", package = "conrappli"), launch.browser = T)
}
