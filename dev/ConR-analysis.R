
library(ggplot2)
pkgload::load_all()

library(shiny)
library(dplyr)
library(leaflet)
library(crosstalk)
library(sf)



# RV ----------------------------------------------------------------------

original.dataset <- list(df = NULL)
original.dataset.accuracy <- list(df = NULL)
list.names <- list(df = NULL)
raster_mayaux_cropped <- list(df = NULL)
raster_hansen_cropped <- list(df = NULL)
mineral_cropped <- list(df = NULL)
protected_areas_cropped <- list(df = NULL)
logging_concessions_CA_cropped <- list(df = NULL)
alpha_raster_deforest <- list(df = NULL)
alpha_raster_mayaux <- list(df = NULL)


# Data --------------------------------------------------------------------

xxx <- rio::import("../exemple_homemade_data.xlsx")
data_validated <- dplyr::filter(
  prepare_data_validate(xxx, lat = "lat", lon = "long", sci_names = "family"),
  dplyr::if_all(dplyr::all_of(validation_cols()), ~ . == TRUE)
)

count(data_validated, family, sort = TRUE)



# Select taxa -------------------------------------------------------------

data_select <- data_validated %>%
  filter(family == "Rubiaceae") %>%
  select(lat, long, family)



# Graphiques exploratoires ------------------------------------------------

alt_range <- datamap$alt
ggplot2::ggplot(data_select, mapping = ggplot2::aes(factor(family), alt)) +
  ggplot2::geom_violin(trim = TRUE)+
  ggplot2::geom_jitter(height = 0, width = 0.05) +
  # ggplot2::ylim(range(alt_range)[1]-100, range(alt_range)[2]+100) +
  ggplot2::labs(x = "", y="Altitude (m)")+
  ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                 axis.title=ggplot2::element_text(size=14,face="bold"))


ggplot2::ggplot(data_select, mapping = ggplot2::aes(factor(family), col_yr)) +
  ggplot2::geom_violin(trim = TRUE)+
  ggplot2::geom_jitter(height = 0, width = 0.05) +
  # ggplot2::ylim(range(year_range)[1]-5, range(year_range)[2]+5) +
  ggplot2::labs(x = "", y="Collection year")+
  ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                 axis.title=ggplot2::element_text(size=14,face="bold"))






# Preparation analyse -----------------------------------------------------

dataset_sf <-  sf::st_as_sf(data_select, coords = c("long", "lat"), crs = 4326)


data("protected_areas")
# test
protected_areas <- sf::st_transform(protected_areas, sf::st_crs(dataset_sf))
sf::st_crop(sf::st_as_sf(protected_areas), sf::st_bbox(sf::st_buffer(dataset_sf, 2)))

for (radius in 2:50) {
  protected_areas_bbox <- sf::st_crop(protected_areas, sf::st_bbox(sf::st_buffer(dataset_sf, radius)))
  if(nrow(protected_areas_bbox)>3) break
}

protected_areas_bbox_sp <- as(protected_areas_bbox, 'Spatial')





data("logging_concessions_CA")
logging_concessions_CA <- sf::st_transform(logging_concessions_CA, sf::st_crs(dataset_sf))
for (radius in 2:50) {
  logging_concessions_CA_bbox <- sf::st_crop(logging_concessions_CA, sf::st_bbox(sf::st_buffer(dataset_sf, radius)))
  if(nrow(logging_concessions_CA_bbox)>3) break
}
logging_concessions_CA_cropped$df <- logging_concessions_CA_bbox

data("rast_mayaux")
threshold_land_cov <- input$threshold_mayaux
rast_mayaux_crop <- raster::crop(rast_mayaux, raster::extent(dataset_sf)+3)
raster::values(rast_mayaux_crop)[which(raster::values(rast_mayaux_crop)<threshold_land_cov)] <- NA
if(all(is.na(raster::values(rast_mayaux_crop)))) {
  raster::values(rast_mayaux_crop) <- 0
  alpha_raster_mayaux$df <- 0
}else{
  alpha_raster_mayaux$df <- 0.5
}
raster_mayaux_cropped$df <- rast_mayaux_crop

data("hansen_deforestation_aggreg")
threshold_deforest <- input$deforest

for (radius in 2:50) {
  if(tryCatch(!is.null(crop(hansen_deforestation_aggreg, raster::extent(dataset_sf) + radius)),
              error=function(e) return(FALSE))) break
}

hansen_deforestation_aggreg_crop <- raster::crop(hansen_deforestation_aggreg, raster::extent(dataset_sf) + radius + 5)
raster::values(hansen_deforestation_aggreg_crop)[which(raster::values(hansen_deforestation_aggreg_crop)<threshold_deforest)] <- NA
if(all(is.na(raster::values(hansen_deforestation_aggreg_crop)))) {
  raster::values(hansen_deforestation_aggreg_crop) <- 0
  alpha_raster_deforest$df <- 0
}else{
  alpha_raster_deforest$df <- 0.5
}

# raster_hansen_cropped$df <- hansen_deforestation_aggreg_crop


data("mineral_deposit")
for (radius in 2:50) {
  mineral_deposit_crop_bbox <-
    sf::st_crop(mineral_deposit, sf::st_bbox(sf::st_buffer(dataset_sf, radius)))
  if(nrow(mineral_deposit_crop_bbox)>0) break
}
# mineral_cropped$df <- mineral_deposit_crop_bbox






# Analyse ConR ------------------------------------------------------------


conr_results <- ConR::IUCN.eval(
  DATA = data_select,
  Cell_size_AOO = 2, #input$aoo_km_res, # min=0.1, max=50, value = 2, round=TRUE, step=1
  Cell_size_locations = 10, #input$locations_km_res, # min=0.1, max=50, value = 10, round=TRUE, step=1
  DrawMap = FALSE# ,
  # protec.areas = protected_areas_bbox_sp
)

eoo <- ConR::EOO.computing(
  XY = data_select,
  export_shp = TRUE
)

locations <- ConR::locations.comp(
  XY = data_select,
  nbe_rep = 50 # input$repeat_pos_aoo
)

aoo <- ConR::AOO.computing(
  XY = data_select,
  export_shp = TRUE,
  nbe.rep.rast.AOO = 50, # input$repeat_pos_aoo,
  Cell_size_AOO = 2 # input$aoo_km_res # min=0.1, max=50, value = 2, round=TRUE, step=1
)

subpop <- ConR::subpop.comp(
  XY = data_select,
  Resol_sub_pop = 10 #input$sub_pop_resol # min=1, max=200, value = 10, round=TRUE, step=1
)







# CRITERION A evaluation ------------------------------------------------------------


criterionA_reduction <- IUCN_eval_CA(
  data = data_select,
  rasters = c(raster_mayaux_cropped$df, raster_hansen_cropped$df),
  mineral = mineral_cropped$df,
  protected_areas = protected_areas_cropped$df,
  thresholds_rasters = c(
    0.5, # input$threshold_mayaux_CA, # min=0, max=1, value = 0.5, round=FALSE, step=0.1
    0.5 # input$deforest_CA # min=0, max=1, value = 0.5, round=FALSE, step=0.1
  ),
  col_coordinates = c("long", "lat"),#c(input$sel_LONG, input$sel_LAT),
  col_tax = "taxa",
  cells_size_aoo = 2, # input$aoo_km_res # min=0.1, max=50, value = 2, round=TRUE, step=1
  export_shp_eoo = TRUE
)



