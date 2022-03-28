#' Bootstrap theme for ConR
#'
#' @return A [bslib:: bs_theme()] object.
#' @export
#'
#' @importFrom bslib bs_theme bs_add_rules font_google
#'
bs_theme_conr <- function() {
  theme <- bslib::bs_theme(
    version = 5
  )
  bslib::bs_add_rules(
    theme = theme,
    c(
      ".label-primary { @extend .badge }",
      ".label-primary { @extend .bg-primary }"
    )
  )
}

