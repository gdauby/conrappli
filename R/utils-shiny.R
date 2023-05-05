
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

#' @importFrom htmltools tags css
#' @importFrom shinyWidgets alert
alert_no_data <- function(id, text = "You need to import data and select variable before using this tab.") {
  tags$div(
    style = htmltools::css(
      position = "fixed",
      top = "56px",
      left = "0",
      right = "0",
      bottom = "0",
      overflow = "hidden",
      padding = "0",
      zIndex = 100,
      background = "#FFF"
    ),
    id = id,
    shinyWidgets::alert(
      status = "info",
      class = "mt-5",
      ph("info"), text
    )
  )
}



#' @importFrom htmltools tags
#' @importFrom phosphoricons ph
navigation <- function(inputId, choices, title) {
  tags$div(
    class = "mt-n1 mb-1 pb-1 border-bottom border-light-subtle",
    tags$div(
      class = "container-fluid",
      tags$button(
        class = "btn btn-sm btn-outline-primary",
        type = "button",
        `data-bs-toggle` = "offcanvas",
        `data-bs-target` = paste0("#", inputId, "_menu"),
        `aria-controls` = paste0(inputId, "_menu"),
        `aria-label` = "Toggle navigation",
        ph("list", weight = "bold")
      ),
      tags$h5(title, class = "d-inline-block ms-2"),


      tags$div(
        class = "float-right pt-2",
        tags$label(
          style = css(verticalAlign = "top", marginTop = "0.5rem"),
          "Language:",
          `for` = paste0(inputId, "_lang")
        ),
        tagAppendAttributes(
          selectInput(
            inputId = paste0(inputId, "_lang"),
            label = NULL,
            choices = c("en", "fr"),
            width = "80px"
          ),
          style = css(display = "inline-block"),
          class = "mt-1"
        )
      ),


      tags$div(
        class = "offcanvas offcanvas-start",
        tabindex = "-1",
        id = paste0(inputId, "_menu"),
        `aria-labelledby` = paste0(inputId, "_label"),
        tags$div(
          class = "offcanvas-header",
          tags$h5(
            class = "offcanvas-title",
            id = paste0(inputId, "_label"),
            "Menu"
          ),
          tags$button(
            type = "button",
            class = "btn-close",
            `data-bs-dismiss` = "offcanvas",
            `aria-label` = "Close"
          )
        ),
        tags$div(
          class = "offcanvas-body",
          tags$div(
            class = "d-grid gap-2",
            lapply(
              X = seq_along(choices),
              FUN = function(i) {
                tags$button(
                  class = "btn btn-light d-block border-bottom border-top border-light-subtle rounded-0",
                  names(choices)[i],
                  onclick = sprintf(
                    "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                    inputId, choices[[i]]
                  ),
                  `data-bs-dismiss` = "offcanvas"
                )
              }
            )
          )
        )
      )
    )
  )
}


