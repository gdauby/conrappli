
mapping_ui <- function(id) {
  
  ns <- NS(id)
  
    tagList(
      tags$div(
        #style = "position: fixed;",
        style = css(position = "fixed", top = "65px", left = "350px", right = 0, bottom = 0, overflow = "hidden", padding = 0),
        #leafletOutput(outputId = "map", width = "100%", height = "100%")
        leafletOutput(outputId = ns("map"), width = "100%", height = 600) 
      )
    )
}


mapping_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      output$map <- renderLeaflet({
        
        # Colorier les points en fonction de la variable `.__valid` ----
        pal <- colorFactor(
          palette = c("navy", "red"),
          domain = c("TRUE", "FALSE")
        )
        
        # Construction du popup ----
        # Données avec variables ne commencant pas par ".__"
        donnees_popup <- data_r() %>%
          dplyr::select(!dplyr::starts_with(".__"))
        
        # Donnees avec les noms des variables
        labels_variables <- lapply(
          X = names(donnees_popup),
          FUN = function(x) { 
            paste0(x, ": ", donnees_popup[[x]])
          })
        tableau_donnees_popup <- data.frame(labels_variables)
        colnames(tableau_donnees_popup) <- names(donnees_popup)
        
        # Vecteur popup
        resume_donnees_popup <- tidyr::unite(tableau_donnees_popup, col = resume, everything(), sep = "<br/>")
        popup <- resume_donnees_popup$resume
        popup <- lapply(popup, htmltools::HTML)
        
        # Carte ----
        leaflet(data = data_r()) %>%
          leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OSM") %>%
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri") %>%
          leaflet::addProviderTiles(leaflet::providers$OpenTopoMap, group = "Open Topo Map") %>%
          leaflet::addLayersControl(
            baseGroups = c("OSM", "Esri", "Open Topo Map"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          ) %>%
          setView(lng = 11.5, lat = 2, zoom = 8) %>% 
          addCircleMarkers(
            lng = ~.__longitude,
            lat = ~.__latitude,
            radius = ~ifelse(.__valid == "TRUE", 6, 10),
            color = ~pal(.__valid),
            stroke = FALSE,
            fillOpacity = 0.5,
            popup = popup,
          ) %>% 
          addLegend(
            pal = pal,
            values = donnees$.__valid,
            opacity = 1,
            title = "Légende",
            position = "bottomright",
            na.label = "N/A", 
          )
      })
    }
  )
}

