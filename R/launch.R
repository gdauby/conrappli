
#' Launch ConR application
#'
#' @inheritParams conr_ui
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
launch <- function(lang = c("en", "fr")) {
  shiny::shinyApp(
    ui = conr_ui(lang = lang),
    server = conr_server(),
    options = list(port = 5791)
  )
}
