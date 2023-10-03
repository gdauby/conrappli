
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

create_popup <- function(.data, n_col = 1) {
  template <- glue::glue("<b>{column}:</b>", column = names(.data))
  template <- glue::glue("{template} {valeur}</br>", template = template, valeur = sprintf("{%s}", names(.data)))
  template <- paste(sprintf("<div>%s</div>", template), collapse = "")
  template <- sprintf("<div style='display: grid; grid-template-columns: repeat(%s, 1fr); grid-column-gap: 10px;'>%s</div>", n_col, template)
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



#' @importFrom leaflet leaflet leafletOptions invokeMethod addProviderTiles providers addLayersControl layersControlOptions
base_map <- function(..., zoom_topright = TRUE) {
  if (isTRUE(zoom_topright)) {
    leaflet(..., options = leafletOptions(zoomControl = FALSE)) %>%
      invokeMethod(data = NULL, method = "addZoom", list(position = "topright"))
  } else {
    leaflet(...)
  } %>%
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
    addLayersControl(
      baseGroups = c("OSM", "Esri", "Open Topo Map"),
      options = layersControlOptions(collapsed = FALSE)
    )
}


#' @importFrom reactable reactableTheme
reactable_theme <- function(...) {
  reactableTheme(
    ...,
    color = "hsl(233, 9%, 87%)",
    backgroundColor = "hsl(233, 9%, 19%)",
    borderColor = "hsl(233, 9%, 22%)",
    stripedColor = "hsl(233, 12%, 22%)",
    highlightColor = "hsl(233, 12%, 24%)",
    headerStyle = list(
      color = "#dfe2e5",
      fontWeight = 400,
      fontSize = "0.9rem",
      letterSpacing = "1px",
      textTransform = "uppercase",
      "&:hover, &:focus" = list(color = "#FFF")
    ),
    inputStyle = list(backgroundColor = "#dfe2e5"),
    selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
    pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
  )
}

