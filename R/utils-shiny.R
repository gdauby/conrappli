
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


btn_help <- function(text, ...) {
  tags$a(
    tabindex = "0",
    class = "btn btn-link conrapp-popover",
    style = "padding: 0;",
    `data-bs-content` = htmltools::doRenderTags(text),
    `data-bs-toggle` = "popover",
    icon("question-circle"),
    ...
  )
}
