
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

unselect_internal_vars <- function(data) {
  dplyr::select(data, !dplyr::starts_with(".__"))
}

is_valid_year_col <- function(.data) {
  if (!hasName(.data, ".__year"))
    return(FALSE)
  values <- unique(.data[[".__year"]])
  values <- as.numeric(values)
  if (all(is.na(values)))
    return(FALSE)
  if (length(values) < 2)
    return(FALSE)
  return(TRUE)
}

create_popup <- function(.data) {
  template <- glue::glue("<b>{column}:</b>", column = names(.data))
  template <- glue::glue("{template} {valeur}</br>", template = template, valeur = sprintf("{%s}", names(.data)))
  template <- paste(template, collapse = "")
  glue::glue_data(.data, template)
}

geojson_to_sf = function(x) {
  do.call(
    rbind,
    lapply(x, function(x) {
      # x <- lapply(x, fix_geojson_coords)
      sf::read_sf(
        jsonlite::toJSON(x, force=TRUE, auto_unbox=TRUE, digits = NA)
      )
    })
  )
}

pts_in_poly <- function(points, poly) {
  x <- sf::st_intersects(
    y = poly,
    x = points,
    sparse = FALSE
  )
  return(x[,1])
}

get_max_obs <- function() {
  getOption("conrappli.max_obs", default = 5000)
}
