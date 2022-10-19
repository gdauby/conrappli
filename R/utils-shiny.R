
#' @importFrom shiny fluidRow column
#' @importFrom htmltools tags
template_ui <- function(..., title = NULL) {
  tags$div(
    class = " row mb-5",
    tags$div(
      class = "col-xl-10 offset-xl-1",

      if (!is.null(title))
        tags$h2(title, class = "conr-title text-center"),

      ...
    )
  )
}


#' @importFrom htmltools tags doRenderTags
#' @importFrom phosphoricons ph
btn_help <- function(text, ...) {
  tags$a(
    tabindex = "0",
    class = "btn btn-link conrapp-popover",
    style = "padding: 0;",
    `data-bs-content` = htmltools::doRenderTags(text),
    `data-bs-toggle` = "popover",
    ph("question"),
    ...
  )
}


alert_no_data <- function(id) {
  tags$div(
    style = htmltools::css(
      position = "fixed",
      top = "56px",
      left = "0",
      right = "0",
      bottom = "0",
      overflow = "hidden",
      padding = "0",
      zIndex = 9999,
      background = "#FFF"
    ),
    id = id,
    shinyWidgets::alert(
      status = "info",
      class = "mt-5",
      ph("info"), "You need to import data and select variable before using this tab."
    )
  )
}

