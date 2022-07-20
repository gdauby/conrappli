
#' Launch ConR application
#'
#' @return A [shiny::shinyApp()] object.
#' @export
#'
#' @importFrom shiny shinyApp
#'
#' @examples
#' if (interactive()) {
#'
#'   # A Launch application in browser
#'   launch()
#'
#' }
launch <- function() {
  shiny::shinyApp(
    ui = conr_ui(),
    server = conr_server(),
    options = list(port = 5791)
  )
}
