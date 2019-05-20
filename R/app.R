

#' Launch shiny app
#'
#' Launch the application for mapping and evaluating the IUCN status
#'
#' @return Launch in web browser the app
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @import shiny
#' @export
app <- function() {
  shiny::runApp(system.file("shinyApp", package = "conrappli"), launch.browser = T)
}


#' Protected areas of Africa
#'
#' An sf object of protected areas in Africa
#'
#' @format sf object
#'
"protected_areas"

#' Raster or impacted land cover proportion
#'
#' Raster of land cover proportion impacted by human, from Mayaux et al. 2003
#'
#' @format raster
#'
"rast_mayaux"

#' Mineral deposits in Africa with a 10 km buffer
#'
#' sf multypolygon of mineral deposits either exploited or not with a 10 km buffer
#'
#' @format sf object
#'
"mineral_deposit"

#' Deforestation estimates for Central Africa
#'
#' Raster of 0.1 decimal degrees resolution indicating proportion of cells 25m from Hansen where deforestation occur since 2001
#'
#' @format raster object
#'
"hansen_deforestation_aggreg"
