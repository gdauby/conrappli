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
