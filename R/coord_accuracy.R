#' compute accuracy of coordinates
#'
#' convert into degrees minutes seconds and give code from 1 (low accuracy) to 8 (high accuracy) of accuracy
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param .data A `data.frame` with longitude and latitude in decimal degrees
#' @param col_x Column name containing the longitude.
#' @param col_y Column name containing the latitude.
#' @param rounding Round or not secondes.
#'
#' @return tibble provided with an additional column `calc_accuracy`.
#'
#' @examples
#' test <- dplyr::tibble(x = c(8.8633, 6.2, 4), y = c(0.1321, 0.22, 1))
#' coord_accuracy(test, col_x = "x", col_y = "y")
#'
#'
#' @importFrom dplyr mutate filter pull select
#'
#' @export
coord_accuracy <- function(.data, col_x, col_y, rounding = TRUE) {
  .data %>%
    dplyr::mutate(
      code_x = coord_accuracy_vec(!!dplyr::sym(col_x), rounding = rounding),
      code_y = coord_accuracy_vec(!!dplyr::sym(col_y), rounding = rounding),
      calc_accuracy = pmax(code_x, code_y)
    ) %>%
    dplyr::select(-code_x, -code_y)
}


#' @importFrom measurements conv_unit
#' @importFrom dplyr near tibble mutate pull
#' @importFrom tidyr separate
coord_accuracy_vec <- function(x, rounding = TRUE) {
  if (isTRUE(rounding)) {
    rounding_fun <- function(x) {
      round(x, digits = 2)
    }
  } else {
    rounding_fun <- identity
  }
  dplyr::tibble(x = x) %>%
    dplyr::mutate(deg_min_sec = measurements::conv_unit(x, from = "dec_deg", to = "deg_min_sec")) %>%
    tidyr::separate(deg_min_sec, c("degrees", "minutes", "secondes"), sep = " ", convert = TRUE)%>%
    dplyr::mutate(
      secondes = rounding_fun(secondes),
      code = dplyr::case_when(
        near(minutes, 0) & near(secondes, 0) ~ 1 ,
        near(minutes/15, round(minutes/15, 0)) & (near(secondes, 0) | near(secondes, 60)) & !near(minutes, 0) ~ 2,
        near(minutes/5, round(minutes/5, 0)) & (near(secondes, 0) | near(secondes, 60)) & !near(minutes, 0) ~ 3,
        near(minutes/2, round(minutes/2, 0)) & (near(secondes, 0) | near(secondes, 60)) & !near(minutes, 0) ~ 4,
        near(secondes, 0) | near(secondes, 60) ~ 5,
        near(secondes, round(secondes, 1)) ~ 6,
        near(secondes, round(secondes, 10)) ~ 7,
        near(secondes, round(secondes, 100)) ~ 8
      )
    ) %>%
    dplyr::pull(code)
}


