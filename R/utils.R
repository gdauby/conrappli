
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

unselect_internal_vars <- function(data) {
  dplyr::select(data, !dplyr::starts_with(".__"))
}
