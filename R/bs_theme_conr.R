#' Bootstrap theme for ConR
#'
#' @return A [bslib:: bs_theme()] object.
#' @export
#'
#' @importFrom bslib bs_theme bs_add_rules font_google
#'
bs_theme_conr <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    "modal-lg" = "1000px",
    primary = "#088A08",
    "navbar-light-color" = "#088A08",
    "navbar-bg" = "#FFF",
    "enable-negative-margins" = TRUE
  )
  bslib::bs_add_rules(
    theme = theme,
    c(
      ".label-primary { @extend .badge }",
      ".label-primary { @extend .bg-primary }",
      ".nav-hidden {height: 0;}",
      ".navbar { @extend .border-bottom }",
      ".navbar { @extend .shadow }",
      # ".navbar { @extend .text-primary }",
      # ".navbar>.nav-link.active { @extend .text-primary }",
      ".nav-link.active { @extend .border-bottom }",
      ".nav-link.active { @extend .border-primary }"
    )
  )
}

