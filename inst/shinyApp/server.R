function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_TAXO")
    hideMenuItem("tab_MAP")
    hideMenuItem("tab_EVAL")
  })

  original.dataset <- reactiveValues(df = NULL)
  original.dataset.accuracy <- reactiveValues(df = NULL)
  list.names <- reactiveValues(df = NULL)

  # load dataset ------------------------------------------------------------

  # inv <- reactiveVal(label = "data frame")
  observeEvent(ignoreInit = T, {
    input$file_DATASET
    # input$num_skip_line
    # input$rad_decimal
  }, {
    # file importation
    if (!is.null(input$file_DATASET)) {

      dataset <-
        readr::read_csv(input$file_DATASET$datapath,
                      locale = readr::locale(encoding = stringi::stri_enc_get()))
      # inv(fread(
      #   file = input$file_DATASET$datapath,
      #   skip = ifelse(is.na(input$num_skip_line) || input$num_skip_line == 0, "__auto__", input$num_skip_line),
      #   data.table = F,
      #   dec = input$rad_decimal
      # ))

      # show the box
      showElement("box_DATASET")
      showElement("box_FIELDS")

      # show the content
      output$table_DATASET <- renderDataTable(dataset)

      int_num_col <- names(dataset)[ sapply(dataset, class) %in% c("integer", "numeric", "double") ]

      # fill the selector with the column name
      for (id in c("sel_LONG", "sel_LAT")) {
        updateSelectInput(session, id, choices = c("<unselected>", int_num_col))
      }
      #
      char_col <- names(dataset)[ sapply(dataset, class) %in% "character" ]
      #
      for (id in c("sel_taxa", "sel_species_epiteth", "sel_genus", "sel_rank", "sel_lower_infra")) {
        updateSelectInput(session, id, choices = c("<unselected>", char_col))
      }

      original.dataset$df <- dataset
      #
      # name <- names(inv())
      # updateSelectInput(session, "sel_PLOT", choices = c("<unselected>", name))
    }
  })


  # error when the user click on the button on the first page
  observeEvent(input$btn_DATASET_LOADED, {
    error <- F

    if (input$sel_taxa == "<unselected>" & input$sel_species_epiteth == "<unselected>" & input$sel_genus == "<unselected>") { # if no taxa
      error <- T
      shinyalert("Oops!", "Select taxa or both species epithet and genus", type = "error")
    }else{
      if(input$sel_taxa == "<unselected>" & (any(c(input$sel_species_epiteth, input$sel_genus)== "<unselected>"))) {
        error <- T
        shinyalert("Oops!", "Select both genus and species epithet", type = "error")
      }
    }

    if((input$sel_rank == "<unselected>" & input$sel_lower_infra != "<unselected>") | input$sel_lower_infra == "<unselected>" & input$sel_rank != "<unselected>") {
      error <- T
      shinyalert("Oops!", "Select both (or none of) name infra-specific level and rank infra-specific level", type = "error")
    }

    if((any(c(input$sel_LONG, input$sel_LAT)== "<unselected>"))) {
      error <- T
      shinyalert("Oops!", "Select both longitude and latitude", type = "error")
    }

    if (!error) {
      showMenuItem("tab_MAP")
      updateTabItems(session, "tab_MAP")

      newData <- original.dataset$df

      if(input$sel_taxa == "<unselected>" & (all(c(input$sel_species_epiteth, input$sel_genus)!= "<unselected>"))) {
        if(all(c(input$sel_rank, input$sel_lower_infra)!= "<unselected>")) {
          newData <-
            newData %>%
            mutate(taxa = paste0(!!rlang::sym(input$sel_genus)," " ,
                                 !!rlang::sym(input$sel_species_epiteth),
                                 ifelse(is.na(!!rlang::sym(input$sel_rank)), "", " ") ,
                                 ifelse(is.na(!!rlang::sym(input$sel_rank)), "", paste0(!!rlang::sym(input$sel_rank), " ")),
                                 ifelse(is.na(!!rlang::sym(input$sel_rank)), "", !!rlang::sym(input$sel_lower_infra)) ))
        }else{
          newData <-
            newData %>%
            mutate(taxa = paste0(!!rlang::sym(input$sel_genus)," " , !!rlang::sym(input$sel_species_epiteth)))
        }
      }else{
        newData <-
          newData %>%
          mutate(taxa = !!rlang::sym(input$sel_taxa))
      }

      dataset_sf_circles <-


      original.dataset.accuracy$df <-
        circles_accuracy_sf(xy = newData, col_x = input$sel_LONG, col_y = input$sel_LAT)


      list.names$df <- newData %>%
        dplyr::distinct(taxa) %>%
        dplyr::pull()

      original.dataset$df <- newData

      output$list_taxa = renderUI( {

        id.names <- as.list(seq(1, length(list.names$df), 1))
        names(id.names) <- enc2utf8(list.names$df)

        # Encoding(names(id.names)) <-  "UTF-8"

        selectInput("name_chosen","Select taxa to evaluate",
                    choices=id.names, selected=1)

      })
    }
  })

  observeEvent(input$check_species, {
    showElement("check_species_stat")

    Name <-list.names$df[as.numeric(input$name_chosen)]
    name_quo <- dplyr::enquo(Name)

    output$summary2 <- renderPrint({
      print(Name)
        # original.dataset$df %>%
        # filter(taxa==!!name_quo) %>%
        #   print()
    })

    dataset_select <-
      original.dataset$df %>%
      filter(taxa==!!name_quo)

    dataset_select_coord <-
      dataset_select  %>%
      dplyr::filter(!is.na(!!rlang::sym(input$sel_LONG)) | !is.na(!!rlang::sym(input$sel_LAT)))

    dataset_sf <-
      sf::st_as_sf(dataset_select_coord,
                   coords = c(input$sel_LONG, input$sel_LAT), crs = 4326)

    dataset_circle_select <-
      original.dataset.accuracy$df %>%
      filter(taxa==!!name_quo)

    output$nbe_occ <- renderText({
   paste("Number of occurrences", nrow(dataset_select),
         "Number of georeferenced occurrences", nrow(dataset_select_coord), sep = "\n")

    })

    output$map <- mapview::renderMapview({

      map_types <- c("Esri.NatGeoWorldMap", "Esri.WorldImagery", "OpenStreetMap.DE",
                     "Esri.WorldPhysical")

      mapview::mapview(dataset_circle_select, map.types = map_types,
                       col.regions = "red", alpha.regions = 0.1, legend =FALSE, viewer.suppress=T) +
        mapview::mapview(dataset_sf, col.regions = "red", map.types = map_types, legend =FALSE, viewer.suppress=T)
    })

  })


  observeEvent(input$btn_SELECT_TAX, {
    showMenuItem("tab_EVAL")
    updateTabItems(session, "tab_EVAL")


    observeEvent(input$eval_species, {
      Name <-list.names$df[as.numeric(input$name_chosen)]
      name_quo <- dplyr::enquo(Name)

      data_select <-
        original.dataset$df %>%
        filter(taxa==!!name_quo, !is.na(!!rlang::sym(input$sel_LAT)), !is.na(!!rlang::sym(input$sel_LONG))) %>%
        dplyr::select(!!rlang::sym(input$sel_LAT), !!rlang::sym(input$sel_LONG), taxa)

      dataset_sf <-
        sf::st_as_sf(original.dataset$df %>%
                       filter(taxa==!!name_quo, !is.na(!!rlang::sym(input$sel_LAT)), !is.na(!!rlang::sym(input$sel_LONG))),
                     coords = c(input$sel_LONG, input$sel_LAT), crs = 4326)

      dataset_circle_select <-
        original.dataset.accuracy$df %>%
        filter(taxa==!!name_quo)


      conr_results <- ConR::IUCN.eval(DATA = data_select,
                                      Cell_size_AOO = input$aoo_km_res, Cell_size_locations = input$locations_km_res,
                                      DrawMap = FALSE)

      eoo <-
        ConR::EOO.computing(XY = data_select, export_shp = TRUE)

      locations <-
        ConR::locations.comp(XY = data_select, nbe_rep = input$repeat_pos_aoo)

      aoo <-
        ConR::AOO.computing(XY = data_select, export_shp = T, nbe.rep.rast.AOO = input$repeat_pos_aoo)

      eoo_poly <-
        sf::st_as_sf(eoo$spatial.polygon_1)

      aoo_poly <-
        sf::st_as_sf(aoo[[2]][[1]])

      locations_poly <-
        sf::st_as_sf(locations[[1]])

      showElement("eval_species_res")

      output$results_conr <- renderText({
        paste("Extent of Occurrences (km2)", conr_results$EOO,
              "Area of Occupancy (km2)", aoo[[1]],
              "Number of locations", locations[[2]],
              "Number of sub-populations", conr_results$Nbe_subPop, sep = "\n")

      })

      output$map2 <- mapview::renderMapview({

        map_types <- c("Esri.NatGeoWorldMap", "Esri.WorldImagery", "OpenStreetMap.DE",
                       "Esri.WorldPhysical")

        mapview::mapview(dataset_circle_select, map.types = map_types,
                         col.regions = "red", alpha.regions = 0.1, legend =FALSE, viewer.suppress=T) +
          mapview::mapview(dataset_sf, col.regions = "red", map.types = map_types, legend =FALSE, viewer.suppress=T) +
          mapview::mapview(eoo_poly, col.regions = "blue", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T) +
          mapview::mapview(locations_poly, col.regions = "pink", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T) +
          mapview::mapview(aoo_poly, col.regions = "red", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T)



      })

    })



  })


  }
