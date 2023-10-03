
get_bbox_country <- function(country = "Gabon") {
  bbox_gab <- sf::st_geometry(rnaturalearth::ne_countries(scale = 110, country = country, returnclass = "sf"))
  st_crs(bbox_gab) <- 4326
  bbox_gab <- st_transform(bbox_gab, "EPSG:6933")
  return(bbox_gab)
}


prepare_map_data <- function(.data) {
  data_latlon_sf <- .data %>%
    filter(!is.na(.__longitude), !is.na(.__latitude)) %>%
    st_as_sf(coords = c(".__longitude", ".__latitude"))
  st_crs(data_latlon_sf) <- 4326
  data_latlon_sf <- st_transform(data_latlon_sf, "EPSG:6933")
  return(data_latlon_sf)
}

#' @importFrom dplyr filter mutate group_by summarise n_distinct left_join
#' @importFrom leaflet colorNumeric leaflet addProviderTiles providers addLayersControl layersControlOptions addPolygons
#' @importFrom sf st_as_sf st_crs st_transform st_sf st_geometry st_polygon st_make_grid st_intersection st_set_geometry
#' @importFrom rnaturalearth ne_countries
draw_map_grid <- function(.data,
                          bbox_country = get_bbox_country(),
                          resolution = 10) {

  data_latlon_sf <- prepare_map_data(.data)

  grid <- st_make_grid(
    x = bbox_country,
    cellsize = resolution * 1000
  )
  grid <- st_as_sf(grid) %>%
    mutate(id_grid = dplyr::row_number())


  data_latlon_sf <- sf::st_intersection(data_latlon_sf, bbox_country)
  intersect_grid <- st_intersection(data_latlon_sf, grid)

  sampling_cell <-
    intersect_grid %>%
    group_by(id_grid) %>%
    summarise(
      nbe_esp = n_distinct(.__taxa),
      n = length(.__taxa)
    ) %>%
    st_set_geometry(NULL)

  grid_not_null <- grid %>%
    left_join(sampling_cell) %>%
    filter(!is.na(n))

  grid_not_null <- st_transform(grid_not_null, 4326)

  pal <- colorNumeric(
    palette = "Reds",
    domain = grid_not_null$nbe_esp
  )

  # create_popup <- function(.data) {
  #   template <- glue::glue("<b>{column}:</b>", column = names(.data))
  #   template <- glue::glue("{template} {valeur}</br>", template = template, valeur = sprintf("{%s}", names(.data)))
  #   template <- paste(template, collapse = "")
  #   glue::glue_data(.data, template)
  # }

  leaflet() %>%
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
    addLayersControl(
      baseGroups = c("OSM", "Esri", "Open Topo Map"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addPolygons(
      data = grid_not_null,
      weight = 1,
      opacity = 0.7,
      color = ~pal(nbe_esp),
      fill =  "black",
      popup = paste("Number of species: ", grid_not_null$nbe_esp),
      fillOpacity = 0.5,
      highlightOptions = leaflet::highlightOptions(
        color = "black",
        weight = 4
      )
    )
}




#' @importFrom leaflet leaflet addProviderTiles providers addCircles
#' @importFrom dplyr select starts_with
draw_map_occ <- function(.data,
                         bbox_country = get_bbox_country()) {

  data_latlon_sf <- prepare_map_data(.data)

  intersect_bbox <- sf::st_intersection(data_latlon_sf, bbox_country)

  intersect_bbox <- sf::st_transform(intersect_bbox, 4326)

  leaflet(
    data = intersect_bbox
  ) %>%
    addProviderTiles(providers$OpenStreetMap, group = "OSM") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Open Topo Map") %>%
    addCircles(
      popup = intersect_bbox %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-starts_with(".__")) %>%
        create_popup(n_col = 2) %>%
        lapply(htmltools::HTML),
      group = "Occurences"
    )
}





