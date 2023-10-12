
get_bbox_country <- function(country = "Gabon") {
  bbox_gab <- sf::st_geometry(rnaturalearth::ne_countries(scale = 110, country = country, returnclass = "sf"))
  st_crs(bbox_gab) <- 4326
  bbox_gab <- st_transform(bbox_gab, "EPSG:6933")
  return(bbox_gab)
}


prepare_map_data <- function(.data) {
  data_latlon_sf <- .data %>%
    filter(!is.na(.__longitude), !is.na(.__latitude)) %>%
    mutate(.__longitude = jitter(.__longitude, factor = 0.2),
           .__latitude = jitter(.__latitude, factor = 0.2)) %>%
    st_as_sf(coords = c(".__longitude", ".__latitude"))
  st_crs(data_latlon_sf) <- 4326
  data_latlon_sf <- st_transform(data_latlon_sf, "EPSG:6933")
  return(data_latlon_sf)
}

#' @importFrom dplyr filter mutate group_by summarise n_distinct left_join rename
#' @importFrom leaflet colorNumeric leaflet addProviderTiles providers addLayersControl layersControlOptions addPolygons
#' @importFrom sf st_as_sf st_crs st_transform st_sf st_geometry st_polygon st_make_grid st_intersection st_set_geometry
#' @importFrom rnaturalearth ne_countries
draw_map_grid <- function(.data,
                          bbox_country = get_bbox_country(),
                          resolution = 10,
                          categories) {

  data_latlon_sf <- prepare_map_data(.data)
  
  # print(data_latlon_sf %>% dplyr::select(redlistcategory))
  
  data_latlon_sf <-
    data_latlon_sf %>% dplyr::filter(redlistcategory %in% categories)

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
  
  grid_not_null <- 
    grid_not_null %>% 
    dplyr::rename(number_of_species = nbe_esp,
           number_of_occurences = n)

  base_map(zoom_topright = FALSE) %>%
    addPolygons(
      data = grid_not_null,
      weight = 1,
      opacity = 0.7,
      color = ~pal(number_of_species),
      fill =  "black",
      popup = 
        grid_not_null %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-id_grid) %>%
        create_popup(n_col = 1) %>%
        lapply(htmltools::HTML),
        # 
        # paste("Number of species: ", grid_not_null$nbe_esp),
      fillOpacity = 0.5,
      highlightOptions = leaflet::highlightOptions(
        color = "black",
        weight = 4
      )
    )
}




#' @importFrom leaflet leaflet addProviderTiles providers addCircles addScaleBar addLayersControl
#' @importFrom dplyr select starts_with filter
draw_map_occ <- function(.data,
                         bbox_country = get_bbox_country(),
                         categories) {

  data_latlon_sf <- prepare_map_data(.data)

  intersect_bbox <- sf::st_intersection(data_latlon_sf, bbox_country)

  intersect_bbox <- sf::st_transform(intersect_bbox, 4326)

  bs_mp <- base_map(data = intersect_bbox, zoom_topright = FALSE)
  
  if (any(categories == "VU")) {
    intersect_bbox_sub <- 
      intersect_bbox %>% dplyr::filter(redlistcategory == "VU")
    bs_mp <- 
      bs_mp %>% 
      addCircles(data = intersect_bbox_sub,
                 popup = intersect_bbox_sub %>%
                   sf::st_drop_geometry() %>%
                   dplyr::select(-starts_with(".__")) %>%
                   create_popup(n_col = 2) %>%
                   lapply(htmltools::HTML),
                 group = "VU",
                 color = "yellow"
      )
  }
  
  if (any(categories == "EN")) {
    intersect_bbox_sub <- 
      intersect_bbox %>% dplyr::filter(redlistcategory == "EN")
    bs_mp <- 
      bs_mp %>% 
      addCircles(data = intersect_bbox_sub,
                 popup = intersect_bbox_sub %>%
                   sf::st_drop_geometry() %>%
                   dplyr::select(-starts_with(".__")) %>%
                   create_popup(n_col = 2) %>%
                   lapply(htmltools::HTML),
                 group = "EN",
                 color = "orange"
      )
  }
  
  if (any(categories == "CR")) {
    intersect_bbox_sub <- 
      intersect_bbox %>% dplyr::filter(redlistcategory == "CR")
    bs_mp <- 
      bs_mp %>% 
      addCircles(data = intersect_bbox_sub,
        popup = intersect_bbox_sub %>%
          sf::st_drop_geometry() %>%
          dplyr::select(-starts_with(".__")) %>%
          create_popup(n_col = 2) %>%
          lapply(htmltools::HTML),
        group = "CR",
        color = "red"
      )
  }

  
  bs_mp %>% 
    addLayersControl(
      baseGroups = c("OSM", "Esri", "Open Topo Map"),
    overlayGroups = categories,
    options = layersControlOptions(collapsed = FALSE)
  )
  # %>%
  #   addCircles(
  #     popup = intersect_bbox %>%
  #       sf::st_drop_geometry() %>%
  #       dplyr::select(-starts_with(".__")) %>%
  #       create_popup(n_col = 2) %>%
  #       lapply(htmltools::HTML),
  #     group = "Occurences"
  #   )
}





