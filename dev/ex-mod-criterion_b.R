
library(dplyr)
library(ConR)
pkgload::load_all()

set.seed(3)
nbe_occ <- sample(seq(1, 30, 1), 20, replace = TRUE) ### 500 species with number of occurrences sampled between 3 and 100
ddlat <- lapply(nbe_occ, function(x) sample(seq(-10, 15, 0.1), x))
ddlon <- lapply(nbe_occ, function(x) sample(seq(7, 25, 0.1), x))
names(ddlon) <- names(ddlat) <- paste0("tax", seq(1,length(ddlat), 1))
ddlat <- lapply(seq_along(ddlat), function(i) data.frame(ddlat =  ddlat[[i]]))
ddlon <- lapply(seq_along(ddlon), function(i) data.frame(ddlon =  ddlon[[i]], taxa = names(ddlon)[i]))
test_data <-
  data.frame(ddlat = do.call('rbind', ddlat), ddlon = do.call('rbind', ddlon)[,1], taxa = do.call('rbind', ddlon)[,2])

names(test_data) <- c(
  ".__latitude",
  ".__longitude",
  ".__taxa"
)


library(shiny)

ui <- fluidPage(
  theme = bs_theme_conr(),
  shinyjs::useShinyjs(),
  criterion_b_ui("criterion_b")
)

server <- function(input, output, session) {

  criterion_b_server("criterion_b", data_r = reactive(test_data))

}

shinyApp(ui, server)
