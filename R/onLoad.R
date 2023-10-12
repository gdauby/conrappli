#' Adds the content of assets/ to conrappli/
#'
#' @importFrom shiny addResourcePath
#'
#' @noRd
.onLoad <- function(...) {
  shiny::addResourcePath("conrappli", system.file("assets", package = "conrappli"))
}
