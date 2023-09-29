

#' @importFrom dplyr filter mutate group_by summarise n_distinct left_join
#' @importFrom leaflet colorNumeric leaflet addProviderTiles providers addLayersControl layersControlOptions addPolygons
#' @importFrom sf st_as_sf st_crs st_transform st_sf st_geometry st_polygon st_make_grid st_intersection st_set_geometry
draw_map <- function(.data, resolution = 10) {
  data_latlon_sf <- .data %>%
    filter(!is.na(.__longitude), !is.na(.__latitude)) %>%
    st_as_sf(coords = c(".__longitude", ".__latitude"))
  st_crs(data_latlon_sf) <- 4326
  data_latlon_sf <- st_transform(data_latlon_sf, "EPSG:6933")
  
  bbox_gab <- st_sf(
    1,
    geom = st_geometry(st_polygon(list(
      matrix(
        c(8,-4, 15,-4, 15, 3, 8, 3, 8,-4),
        ncol = 2,
        byrow = TRUE
      )
    )))
  )
  
  st_crs(bbox_gab) <- 4326
  bbox_gab <- st_transform(bbox_gab, "EPSG:6933")
  
  grid <- st_make_grid(
    x = bbox_gab,
    cellsize = resolution * 1000
  )
  grid <- st_as_sf(grid) %>%
    mutate(id_grid = dplyr::row_number())
  
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
    palette = "Blues",
    domain = grid_not_null$nbe_esp
  )
  
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
      fillOpacity = 0.7,
      color = ~pal(nbe_esp),
      fill =  ~pal(nbe_esp),
      popup = paste("Region: ", grid_not_null$nbe_esp)
    )
}
