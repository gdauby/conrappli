library(shiny)
library(shinydashboard)
library(dplyr)
library(magrittr)
library(shinyjs)
library(shinyalert)
library(shinyFeedback)


# ajoute un id a un box de maniere a pouvoir le montrer/cacher
boxWithId <- function(..., title = NULL, footer = NULL, status = NULL,
                      solidHeader = FALSE, background = NULL, width = 6, height = NULL,
                      collapsible = FALSE, collapsed = FALSE, id = NULL) {
  b <- match.call(expand.dots = TRUE)
  bid <- id
  b$id <- NULL
  b[[1]] <- as.name("box")
  b <- eval(b, parent.frame())
  b$attribs$id <- bid
  b
}

hideMenuItem <- function(tabName) {
  shinyjs::hide(selector = sprintf("a[data-value='%s']", tabName))
}

showMenuItem <- function(tabName) {
  shinyjs::show(selector = sprintf("a[data-value='%s']", tabName))
}





#' compute accuracy of coordinates
#'
#' convert into degrees minutes seconds and give code from 1 (low accuracy) to 8 (high accuracy) of accuracy
#'
#' @return tibble provided with additional columns
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
coord_accuracy <- function(xy, col_x, col_y, rounding=TRUE) {

  xy <-
    xy %>%
    tibble::add_column(id_accur = seq(1, nrow(.), 1))

  var1 <- rlang::enquo(col_x)

  select_lon <-
    xy %>%
    dplyr::select(!!var1) %>%
    filter(!is.na(!!rlang::sym(col_x))) %>%
    pull()

  # if(rounding) select_lon <- round(select_lon, 2)

  longitude_char <-
    select_lon %>%
    measurements::conv_unit(from = "dec_deg", to = "deg_min_sec")

  longitudes_split <- strsplit(longitude_char, split = " ")

  longitudes_tb <-
    tibble(id = xy %>% filter(!is.na(!!rlang::sym(col_x))) %>% pull(id_accur),
           degrees_lon = as.numeric(unlist(lapply(longitudes_split, function(x) x[1]))),
           minutes_lon = as.numeric(unlist(lapply(longitudes_split, function(x) x[2]))),
           secondes_lon = as.numeric(unlist(lapply(longitudes_split, function(x) x[3]))))

  if(rounding) longitudes_tb <-
    longitudes_tb %>%
    mutate(secondes_lon = round(secondes_lon, 2))

  longitudes_tb <-
    longitudes_tb %>%
    mutate( code_lon = case_when(
      near(minutes_lon, 0) & near(secondes_lon, 0)    ~   1 ,
      near(minutes_lon/15, round(minutes_lon/15, 0)) & (near(secondes_lon, 0) | near(secondes_lon, 60)) & !near(minutes_lon, 0)      ~   2,
      near(minutes_lon/5, round(minutes_lon/5, 0)) & (near(secondes_lon, 0) | near(secondes_lon, 60)) & !near(minutes_lon, 0)      ~   3,
      near(minutes_lon/2, round(minutes_lon/2, 0)) & (near(secondes_lon, 0) | near(secondes_lon, 60)) & !near(minutes_lon, 0)      ~   4,
      near(secondes_lon, 0) | near(secondes_lon, 60)      ~   5,
      near(secondes_lon, round(secondes_lon, 1))      ~   6,
      near(secondes_lon, round(secondes_lon, 10))      ~   7,
      near(secondes_lon, round(secondes_lon, 100))      ~   8
    ))

  if(longitudes_tb  %>%
     filter(is.na(code_lon)) %>%
     nrow()>0) stop("coordinates not coded")

  var2 <- rlang::enquo(col_y)

  select_lat <-
    xy %>%
    dplyr::select(!!var2) %>%
    filter(!is.na(!!rlang::sym(col_y))) %>%
    pull()

  # if(rounding) select_lat <- round(select_lat, 2)

  latitude_char <-
    select_lat %>%
    measurements::conv_unit(from = "dec_deg", to = "deg_min_sec")

  latitudes_split <- strsplit(latitude_char, split = " ")

  latitudes_tb <-
    tibble(id = xy %>% filter(!is.na(!!rlang::sym(col_y))) %>% pull(id_accur),
           degrees_lat = as.numeric(unlist(lapply(latitudes_split, function(x) x[1]))),
           minutes_lat = as.numeric(unlist(lapply(latitudes_split, function(x) x[2]))),
           secondes_lat = as.numeric(unlist(lapply(latitudes_split, function(x) x[3]))))


  if(rounding) latitudes_tb <-
    latitudes_tb %>%
    mutate(secondes_lat = round(secondes_lat, 2))

  latitudes_tb <-
    latitudes_tb %>%
    mutate( code_lat = case_when(
      near(minutes_lat, 0) & near(secondes_lat, 0)    ~   1 ,
      near(minutes_lat/15, round(minutes_lat/15, 0)) & (near(secondes_lat, 0) | near(secondes_lat, 60)) & !near(minutes_lat, 0)      ~   2,
      near(minutes_lat/5, round(minutes_lat/5, 0)) & (near(secondes_lat, 0) | near(secondes_lat, 60)) & !near(minutes_lat, 0)      ~   3,
      near(minutes_lat/2, round(minutes_lat/2, 0)) & (near(secondes_lat, 0) | near(secondes_lat, 60)) & !near(minutes_lat, 0)      ~   4,
      near(secondes_lat, 0) | near(secondes_lat, 60)      ~   5,
      near(secondes_lat, round(secondes_lat, 1))      ~   6,
      near(secondes_lat, round(secondes_lat, 10))      ~   7,
      near(secondes_lat, round(secondes_lat, 100))      ~   8
    ))

  if(latitudes_tb  %>%
     filter(is.na(code_lat)) %>%
     nrow()>0) stop("coordinates not coded")

  xy <-
    xy %>%
    left_join(longitudes_tb, by=c("id_accur"="id"))

  xy <-
    xy %>%
    left_join(latitudes_tb, by=c("id_accur"="id"))

  xy <-
    xy %>%
    mutate(calc_accuracy = ifelse(code_lat<code_lon, code_lon, code_lat))
  xy <-
    xy %>%
    dplyr::select(-id_accur)
  return(xy)
}


circles_accuracy_sf <- function(xy, col_x, col_y) {
  xy <-
    coord_accuracy(xy = xy, col_x = col_x, col_y = col_y)
  xy <-
    xy %>%
    tibble::add_column(id_dataset = seq(1, nrow(.), 1))

  xy <-
    xy %>%
    dplyr::filter(!is.na(!!rlang::sym(col_x)) | !is.na(!!rlang::sym(col_y)))

  # xy_sf <-
  #   sf::st_as_sf(xy, coords = c(col_x, col_y), crs = 4326)
  # bbox_data <- sf::st_bbox(xy_sf)

  accuracy_table <-
    dplyr::tibble(code_accuracy = seq(1, 8, 1),
                  precision_km = c(110, 30, 10, 4, 2, 0.2, 0.02, 0.002),
                  precision_dec_deg = c(1, 15/60, 5/60, 2/60, 1/60, 0.1/60, 0.01/60, 0.001/60))
  print(accuracy_table)

  all_circles <- list()
  for (i in unique(xy$calc_accuracy)) {
    precision_km <-
      accuracy_table %>%
      dplyr::filter(code_accuracy ==i) %>%
      dplyr::pull(precision_km)

    xy_subset <-
      xy %>%
      dplyr::filter(calc_accuracy==i)

    xy_subset_sf <-
      sf::st_as_sf(xy_subset, coords = c(col_x, col_y), crs = 4326)

    xy_subset_sf_proj <-
      sf::st_transform(xy_subset_sf, 54032) # World Azimuthal Equidistant

    xy_subset_sf <-
      sf::st_buffer(xy_subset_sf_proj, precision_km*1000) %>%
      sf::st_transform(4326)

    all_circles[[length(all_circles)+1]] <-
      xy_subset_sf
  }

  xy_circles <-
    do.call(rbind, all_circles)

  return(xy_circles)
}


# data <-
#   dataset %>%
#   dplyr::select(LAT_DD, LONG_DD, taxa) %>%
#   filter(taxa=="Bikinia congensis")
# #
# #
# dataset_sf <-
#   sf::st_as_sf(data %>%
#                  filter(!is.na(LONG_DD), !is.na(LAT_DD)),
#                coords = c("LONG_DD", "LAT_DD"), crs = 4326)
#
#
# data("rast_mayaux")
# threshold_land_cov <- 0.5
# rast_mayaux_crop <- raster::crop(rast_mayaux, raster::extent(dataset_sf)+1)
# raster::values(rast_mayaux_crop)[which(raster::values(rast_mayaux_crop)<threshold_land_cov)] <- NA
#
# data("hansen_deforestation_aggreg")
# threshold_deforest <- 0.5
# hansen_deforestation_aggreg_crop <- raster::crop(hansen_deforestation_aggreg, raster::extent(dataset_sf)+1)
# raster::values(hansen_deforestation_aggreg_crop)[which(raster::values(hansen_deforestation_aggreg_crop)<threshold_deforest)] <- NA
# if(all(is.na(raster::values(hansen_deforestation_aggreg_crop)))) raster::values(hansen_deforestation_aggreg_crop) <- 0
#
# data("protected_areas")
# for (radius in 2:50) {
#   protected_areas_bbox <- sf::st_crop(protected_areas, sf::st_bbox(sf::st_buffer(dataset_sf, radius)))
#   if(nrow(protected_areas_bbox)>0) break
# }
#
# rasters <- c(rast_mayaux_crop, hansen_deforestation_aggreg_crop)
#
# thresholds_rasters <- c(0.5, 0.5)
# col_coordinates = c("LONG_DD", "LAT_DD")

IUCN_eval_CA <- function(data,
                         rasters = NULL,
                         mineral = NULL,
                         protected_areas = NULL,
                         thresholds_rasters  = NULL,
                         col_coordinates = c("ddlon", "ddlat"),
                         col_tax= "taxa",
                         cells_size_aoo = 2,
                         export_shp_eoo = FALSE) {
  data <-
    data %>%
    tibble::add_column(ID=1:nrow(data))

  extract.rasts.hum.impacted <- list()
  for (i in 1:length(rasters)) {
    extract.rasts.hum.impacted[[i]] <-
      raster::extract(rasters[[i]],
                      dplyr::select(data,
                                    !!rlang::sym(col_coordinates[1]),
                                    !!rlang::sym(col_coordinates[2])) %>%
                        tidyr::drop_na())
  }

  sp.foc.sf<-
    sf::st_as_sf(dplyr::select(data, !!rlang::sym(col_coordinates[1]),
                               !!rlang::sym(col_coordinates[2])) %>%
               tidyr::drop_na(), coords = c(col_coordinates[1], col_coordinates[2]),
             crs = 4326)

  # plot(sp.foc.sf$geometry, col="blue", pch=19)
  # plot(africa$geometry, add=T)
  # plot(Mineral_deposit$geometry, add=T, col="red", fill="red")

  ids_impacted <- list()

  if(!is.null(mineral))
    suppressMessages(suppressWarnings(ids_impacted[[length(ids_impacted)+1]] <-
                                        sf::st_intersection(sp.foc.sf, mineral)$ID))

  if(!is.null(protected_areas)) {
    suppressMessages(suppressWarnings(Intersect_protected_areas <-
                                        sf::st_intersection(sp.foc.sf, protected_areas)))
    ### ID of all points within protected areas
    ID.occ.protected.areas <- Intersect_protected_areas$ID
  }

  # plot(Intersect$geometry, col="green", pch=19, add=T)

  ### ID of all points impacted
  for (i in 1:length(extract.rasts.hum.impacted))
    ids_impacted[[length(ids_impacted)+1]] <-
    data$ID[which(extract.rasts.hum.impacted[[i]]>thresholds_rasters[i])]

  ids_impacted <- data$ID[unique(do.call(c, ids_impacted))]

  # ID.occ.impacted <- unique(c(data$ID[which(extract.rasts.hum.impacted[[1]]>0.5)], Intersect$ID))

  # ID.occ.impacted  <-
  #   ID.occ.impacted[!ID.occ.impacted %in% ID.occ.protected.areas]

  ### AOO REDUCTION
  full_aoo <-
    ConR::AOO.computing(XY = data %>%
                        dplyr::select(!!rlang::sym(col_coordinates[2]),
                                         !!rlang::sym(col_coordinates[1]),
                                      !!rlang::sym(col_tax)) %>%
                        tidyr::drop_na(),
                      Cell_size_AOO = cells_size_aoo)

  # AOO.all <- .AOO.estimation(coordEAC=sp.foc.sf, Cell_size_AOO = 2, nbe.rep.rast.AOO = NULL)[1]

  if(length(ids_impacted)>0) {

    left_aoo <-
      ConR::AOO.computing(XY = data %>%
                            dplyr::select(!!rlang::sym(col_coordinates[2]),
                                          !!rlang::sym(col_coordinates[1]),
                                          !!rlang::sym(col_tax),
                                          ID) %>%
                            dplyr::filter(!ID %in% ids_impacted) %>%
                            tidyr::drop_na(),
                          Cell_size_AOO = cells_size_aoo)

  }else{
    left_aoo <- full_aoo
  }


  ### EOO REDUCTION
  full_eoo <-
    ConR::EOO.computing(XY = data %>%
                          dplyr::select(!!rlang::sym(col_coordinates[2]),
                                        !!rlang::sym(col_coordinates[1]),
                                        !!rlang::sym(col_tax)) %>%
                          tidyr::drop_na(),
                        export_shp = export_shp_eoo)
  if(export_shp_eoo & all(!is.na(full_eoo))) {
    full_eoo_shp <- full_eoo[[2]]
    full_eoo <- full_eoo[[1]]
  }

  if(length(ids_impacted)>0) {

    left_eoo <-
      ConR::EOO.computing(XY = data %>%
                            dplyr::select(!!rlang::sym(col_coordinates[2]),
                                          !!rlang::sym(col_coordinates[1]),
                                          !!rlang::sym(col_tax),
                                          ID) %>%
                            dplyr::filter(!ID %in% ids_impacted) %>%
                            tidyr::drop_na(),
                          export_shp = export_shp_eoo)

    if(export_shp_eoo & all(!is.na(left_eoo))) {
      left_eoo_shp <- left_eoo[[2]]
      left_eoo <- left_eoo[[1]]
    }

  }else{
    left_eoo <- full_eoo
    left_eoo_shp <- NA
  }

  res_table <-
    tibble(taxa =
             data %>%
             dplyr::select(!!rlang::sym(col_tax)) %>%
             dplyr::distinct() %>%
             dplyr::pull(),
           nbe_occ =
             data %>%
             dplyr::select(!!rlang::sym(col_coordinates[2]),
                           !!rlang::sym(col_coordinates[1])) %>%
             dplyr::distinct() %>%
             nrow(),
           nbe_occ_human_impacted =
             length(ids_impacted),
           AOO_full = full_aoo,
           AOO_left = left_aoo,
           EOO_full = full_eoo,
           EOO_left = left_eoo,
           nbe_occ_protected_area =
             ifelse(!is.null(protected_areas), length(ID.occ.protected.areas), NA)
           )

  if(export_shp_eoo & !is.na(res_table$EOO_full))
    return(list(results = res_table, shapefiles = list(full_eoo_shp = full_eoo_shp, left_eoo_shp = left_eoo_shp)))
  if(!export_shp_eoo | is.na(res_table$EOO_full))
    return(list(results = res_table))

}

cat_criterion_A <- function(x,...) {
  cat <- NA
  if(x>=80) cat <- "CR"
  if(x<80 & x>=50) cat <- "EN"
  if(x<50 & x>=30) cat <- "VU"
  if(x<30) cat <- "LC or NT"
  return(cat)
}


# criterionA_reduction <-
#   IUCN_eval_CA(data = data, rasters =
#                  c(rast_mayaux_crop, hansen_deforestation_aggreg_crop),
#                mineral = NULL,
#                protected_areas = protected_areas_bbox,
#                thresholds_rasters =
#                  c(0.5, 0.5),
#                col_coordinates = c("LONG_DD", "LAT_DD"),
#                col_tax = "taxa",
#                cells_size_aoo = 2,
#                export_shp_eoo = TRUE)
# #
# criterionA_reduction_results <-
#   criterionA_reduction$results
# #
# criterionA_reduction_results <-
#   criterionA_reduction_results %>%
#   mutate(AOO_decline=(AOO_full-AOO_left)/AOO_full*100) %>%
#   mutate(EOO_decline=(EOO_full-EOO_left)/EOO_full*100)
# #
# #
# criterionA_reduction_results <-
#   criterionA_reduction_results %>%
#   tibble::add_column(category_code_ca_aoo = plyr::aaply(criterionA_reduction_results$AOO_decline, 1, cat_criterion_A)) %>%
#   tibble::add_column(category_code_ca_eoo = plyr::aaply(criterionA_reduction_results$EOO_decline, 1, cat_criterion_A)) %>%
#   View()

# library(raster)
# all_files_hansen <- list.files(path = "D:/SIG/globalforestwatch/", full.names = TRUE)
# raster_list <- list()
# for (i in 1:length(all_files_hansen)) raster_list[[i]] <- raster(all_files_hansen[i])
# # raster_brick <- brick(raster_list)
#
# # levelplot(raster_list[[1]])
#
# aggreg_rast <- list()
# for (i in 1:length(raster_list)) {
#   prop_deforest <- aggregate(raster_list[[i]], fact=400, fun=function(vals, na.rm) {
#     sum(vals>0, na.rm=na.rm)/length(vals)
#   })
#   aggreg_rast[[i]] <- prop_deforest
# }
#
# merge_rast <- do.call(raster::merge, aggreg_rast)
#
#
# merge_rast <- calc(merge_rast, fun = function(x) ifelse(x<0.0001, NA, x))
#
# writeRaster(merge_rast, "hansen_aggregated.tif")
# raster("hansen_aggregated.tif")


# hansen_deforestation_aggreg <- raster("hansen_aggregated.tif")



