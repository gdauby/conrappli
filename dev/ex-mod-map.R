
library(dplyr)
pkgload::load_all()

donnees <- rio::import("../exemple_homemade_data.xlsx") %>%
  prepare_data_validate(lat = "lat", lon = "long", sci_names = "family") %>%
  dplyr::filter(
    dplyr::if_all(dplyr::all_of(validation_cols()), ~ . == TRUE)
  ) %>%
  select(
    .__taxa = family,
    .__longitude = long,
    .__latitude = lat,
    .__year = col_yr
  )


library(shiny)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$style(".selectize-dropdown, .selectize-dropdown.form-control {z-index: 1060 !important;}"),
  data_map_ui("map")
)

server <- function(input, output, session) {

  data_map_server("map", data_r = reactive(donnees))

}

shinyApp(ui, server)