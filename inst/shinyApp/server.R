function(input, output, session) {

  # stop the serveur in the end of the session
  session$onSessionEnded(function() {
    stopApp()
  })

  options(shiny.maxRequestSize=30*1024^2)

  observe({
    # hide few menu at the begining
    hideMenuItem("tab_TAXO")
    hideMenuItem("tab_MAP")
    hideMenuItem("tab_EVAL")
    hideMenuItem("tab_EVAL2")
    hideMenuItem("tab_SUMMARY")
  })

  original.dataset <- reactiveValues(df = NULL)
  original.dataset.accuracy <- reactiveValues(df = NULL)
  list.names <- reactiveValues(df = NULL)
  raster_mayaux_cropped <- reactiveValues(df = NULL)
  raster_hansen_cropped <- reactiveValues(df = NULL)
  mineral_cropped <- reactiveValues(df = NULL)
  protected_areas_cropped <- reactiveValues(df = NULL)
  alpha_raster_deforest <- reactiveValues(df = NULL)
  alpha_raster_mayaux <- reactiveValues(df = NULL)

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

      # show the box
      showElement("box_DATASET")
      showElement("box_FIELDS")


      output$show_col <- renderUI({
        checkboxGroupInput("show_vars", "Columns to show:",
                           colnames(dataset), selected = colnames(dataset))
      })


      # output$summary <- renderPrint({
      #   print(input$show_vars)
      #   # original.dataset$df %>%
      #   # filter(taxa==!!name_quo) %>%
      #   #   print()
      # })

      # show the content
      output$table_DATASET <- renderDataTable(dataset[,input$show_vars]) # , drop=FALSE

      int_num_col <- names(dataset)[ sapply(dataset, class) %in% c("integer", "numeric", "double")]

      # fill the selector with the column name
      for (id in c("sel_LONG", "sel_LAT", "sel_ALT", "sel_YEAR")) {
        updateSelectInput(session, id, choices = c("<unselected>", int_num_col))
      }
      #
      char_col <- names(dataset)[ sapply(dataset, class) %in% "character" ]
      #
      for (id in c("sel_taxa", "sel_species_epiteth", "sel_genus", "sel_rank", "sel_lower_infra", "sel_authors", "sel_lower_infra_authors")) {
        updateSelectInput(session, id, choices = c("<unselected>", char_col))
      }

      original.dataset$df <- dataset
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
        newData <-
          newData %>%
          mutate(taxa = paste0(!!rlang::sym(input$sel_genus)," " ,
                               !!rlang::sym(input$sel_species_epiteth)))

        if(input$sel_authors!="<unselected>") {
          newData <-
            newData %>%
            mutate(taxa = paste0(taxa," " ,
                                 ifelse(is.na(!!rlang::sym(input$sel_authors)), "",
                                        !!rlang::sym(input$sel_authors))))

        }

        if(input$sel_rank!="<unselected>") {
          newData <-
            newData %>%
            mutate(taxa = paste0(taxa," " ,
                                 ifelse(is.na(!!rlang::sym(input$sel_rank)), "",
                                        !!rlang::sym(input$sel_rank))))

        }

        if(input$sel_lower_infra!="<unselected>") {
          newData <-
            newData %>%
            mutate(taxa = paste0(taxa," " ,
                                 ifelse(is.na(!!rlang::sym(input$sel_lower_infra)), "",
                                        !!rlang::sym(input$sel_lower_infra))))

        }

        if(input$sel_lower_infra_authors!="<unselected>") {
          newData <-
            newData %>%
            mutate(taxa = paste0(taxa," " ,
                                 ifelse(is.na(!!rlang::sym(input$sel_lower_infra_authors)), "",
                                        !!rlang::sym(input$sel_lower_infra_authors))))
          }
      }else{
        newData <-
          newData %>%
          mutate(taxa = !!rlang::sym(input$sel_taxa))

      }


      original.dataset.accuracy$df <-
        circles_accuracy_sf(xy = newData, col_x = input$sel_LONG, col_y = input$sel_LAT)

      list.names$df <- newData %>%
        dplyr::distinct(taxa) %>%
        dplyr::pull()

      original.dataset$df <- newData

      output$list_taxa = renderUI( {

        id.names <- as.list(seq(1, length(list.names$df), 1))
        names(id.names) <- enc2utf8(as.character(list.names$df))

        # Encoding(names(id.names)) <-  "UTF-8"

        selectInput("name_chosen","Select taxa to evaluate",
                    choices=id.names, selected=1)

      })
    }
  })

  # CHECK species ------------------------------------------------------------

  observeEvent(input$check_species, {
    showElement("check_species_stat")

    Name <-list.names$df[as.numeric(input$name_chosen)]
    name_quo <- dplyr::enquo(Name)

    # output$summary2 <- renderPrint({
    #   print(Name)
    #
    #   alt_select <-
    #     dataset_select %>%
    #     dplyr::select(!!rlang::sym(input$sel_ALT)) %>%
    #     dplyr::filter(!!rlang::sym(input$sel_ALT)>0) %>%
    #     pull()
    #   print(alt_select)
    #     # original.dataset$df %>%
    #     # filter(taxa==!!name_quo) %>%
    #     #   print()
    # })

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
      if(input$sel_ALT!="<unselected>") {
        alt_range <-
          dataset_select %>%
          dplyr::select(!!rlang::sym(input$sel_ALT)) %>%
          dplyr::filter(!!rlang::sym(input$sel_ALT)>0) %>%
          pull()
      }
      if(input$sel_YEAR!="<unselected>") {
        year_range <-
          dataset_select %>%
          dplyr::select(!!rlang::sym(input$sel_YEAR)) %>%
          dplyr::filter(!!rlang::sym(input$sel_YEAR)>0) %>%
          pull()
      }

   paste(paste("Number of occurrences", nrow(dataset_select)),
         paste("Number of georeferenced occurrences", nrow(dataset_select_coord)),
         ifelse(input$sel_ALT!="<unselected>", paste("Altitude range",
                                                     ifelse(length(alt_range)>0,
                                                            paste(range(alt_range), collapse = "-"), NA)), ""),
         ifelse(input$sel_YEAR!="<unselected>", paste("Collecting year range",
                                                     ifelse(length(year_range)>0,
                                                            paste(range(year_range), collapse = "-"), NA)), ""),
         sep = "\n")

    })

    output$map <- mapview::renderMapview({

      map_types <- c("Esri.NatGeoWorldMap", "Esri.WorldImagery", "OpenStreetMap.DE",
                     "Esri.WorldPhysical")

      mapview::mapview(dataset_circle_select, map.types = map_types,
                       col.regions = "red", alpha.regions = 0.1, legend =FALSE, viewer.suppress=T) +
        mapview::mapview(dataset_sf, col.regions = "red", map.types = map_types, legend =FALSE, viewer.suppress=T)
    })

    if(input$sel_ALT!="<unselected>") showElement("plot_alt")

    output$plot_alt <- renderPlot({
      if(input$sel_ALT!="<unselected>") {
        alt_select <-
          dataset_select %>%
          dplyr::select(!!rlang::sym(input$sel_ALT), taxa) %>%
          dplyr::filter(!!rlang::sym(input$sel_ALT)>0)

        alt_range <-
          alt_select %>%
          dplyr::select(!!rlang::sym(input$sel_ALT)) %>%
          pull()

        gg1 <- ggplot2::ggplot(alt_select, mapping = ggplot2::aes(factor(taxa), !!rlang::sym(input$sel_ALT))) +
          ggplot2::geom_violin(trim = TRUE)+
          ggplot2::geom_jitter(height = 0, width = 0.05) +
          ggplot2::ylim(range(alt_range)[1]-100, range(alt_range)[2]+100) +
          ggplot2::labs(x = "", y="Altitude (m)")+
          ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                axis.title=ggplot2::element_text(size=14,face="bold"))

      }else{
        gg1 <- NULL
      }

      if(input$sel_YEAR!="<unselected>") {
        year_select <-
          dataset_select %>%
          dplyr::select(!!rlang::sym(input$sel_YEAR), taxa) %>%
          dplyr::filter(!!rlang::sym(input$sel_YEAR)>0)

        year_range <-
          year_select %>%
          dplyr::select(!!rlang::sym(input$sel_YEAR)) %>%
          pull()

        gg2 <- ggplot2::ggplot(year_select, mapping = ggplot2::aes(factor(taxa), !!rlang::sym(input$sel_YEAR))) +
          ggplot2::geom_violin(trim = TRUE)+
          ggplot2::geom_jitter(height = 0, width = 0.05) +
          ggplot2::ylim(range(year_range)[1]-5, range(year_range)[2]+5) +
          ggplot2::labs(x = "", y="Collection year")+
          ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                         axis.title=ggplot2::element_text(size=14,face="bold"))

      }else{
        gg2 <- NULL
      }

      if(!is.null(gg1) & !is.null(gg2)) {
        gg1 <- ggplot2::ggplotGrob(gg1)
        gg2 <- ggplot2::ggplotGrob(gg2)
        gridExtra::grid.arrange(gg1, gg2, ncol = 2)
      }
      if(!is.null(gg1) & is.null(gg2)) print(gg1)
      if(is.null(gg1) & !is.null(gg2)) print(gg2)
    })

    # show the boxes
    showElement("box_DATASET_SPECIES")
    showElement("show_col_species")

    output$show_col_species <- renderUI({
      checkboxGroupInput("show_vars_species", "Columns to show:",
                         colnames(dataset_select),
                         selected = "taxa")
    })

    # show the content
    output$table_DATASET_SPECIES <-
      renderDataTable(dataset_select[,input$show_vars_species]) # , drop=FALSE


  })

  # CRITERION B evaluation ------------------------------------------------------------

  observeEvent(input$btn_SELECT_TAX, {
    showMenuItem("tab_EVAL")
    updateTabItems(session, "tab_EVAL")


    observeEvent(input$eval_species, {
      Name <-list.names$df[as.numeric(input$name_chosen)]
      name_quo <- dplyr::enquo(Name)

      data_select <-
        original.dataset$df %>%
        filter(taxa==!!name_quo, !is.na(!!rlang::sym(input$sel_LAT)), !is.na(!!rlang::sym(input$sel_LONG))) %>%
        dplyr::select(!!rlang::sym(input$sel_LAT),
                      !!rlang::sym(input$sel_LONG), taxa)

      dataset_sf <-
        sf::st_as_sf(original.dataset$df %>%
                       filter(taxa==!!name_quo, !is.na(!!rlang::sym(input$sel_LAT)), !is.na(!!rlang::sym(input$sel_LONG))),
                     coords = c(input$sel_LONG, input$sel_LAT), crs = 4326)

      dataset_circle_select <-
        original.dataset.accuracy$df %>%
        filter(taxa==!!name_quo)

      withProgress(message = 'Making evaluation', value = 0, {
        incProgress(1/3, detail = paste("Preparing layers", 1))

        data("protected_areas")
        for (radius in 2:50) {
          protected_areas_bbox <- sf::st_crop(protected_areas, sf::st_bbox(sf::st_buffer(dataset_sf, radius)))
          if(nrow(protected_areas_bbox)>0) break
        }
        protected_areas_cropped$df <- protected_areas_bbox

        data("rast_mayaux")
        threshold_land_cov <- input$threshold_mayaux
        rast_mayaux_crop <- raster::crop(rast_mayaux, raster::extent(dataset_sf)+1)
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
        hansen_deforestation_aggreg_crop <- raster::crop(hansen_deforestation_aggreg, raster::extent(dataset_sf)+1)
        raster::values(hansen_deforestation_aggreg_crop)[which(raster::values(hansen_deforestation_aggreg_crop)<threshold_deforest)] <- NA
        if(all(is.na(raster::values(hansen_deforestation_aggreg_crop)))) {
          raster::values(hansen_deforestation_aggreg_crop) <- 0
          alpha_raster_deforest$df <- 0
        }else{
          alpha_raster_deforest$df <- 0.5
        }

        raster_hansen_cropped$df <- hansen_deforestation_aggreg_crop


        data("mineral_deposit")
        for (radius in 2:50) {
          mineral_deposit_crop_bbox <-
            sf::st_crop(mineral_deposit, sf::st_bbox(sf::st_buffer(dataset_sf, radius)))
          if(nrow(mineral_deposit_crop_bbox)>0) break
        }
        mineral_cropped$df <- mineral_deposit_crop_bbox

        protected_areas_bbox_sp <- as(protected_areas_bbox, 'Spatial')



        table_occupied_pa <-
          sf::st_intersection(protected_areas_bbox, dataset_sf) %>%
          sf::st_set_geometry(NULL) %>%
          dplyr::select(ORIG_NAME, DESIG, IUCN_CAT, INT_CRIT, ISO3)

        output$table_occupied_pa <- renderDataTable(table_occupied_pa %>% distinct())

        # protected_areas_bbox_sp <- as(protected_areas, 'Spatial')
        incProgress(2/3, detail = paste("Running ConR", 2))

        conr_results <- ConR::IUCN.eval(DATA = data_select,
                                        Cell_size_AOO = input$aoo_km_res, Cell_size_locations = input$locations_km_res,
                                        DrawMap = FALSE, protec.areas = protected_areas_bbox_sp)

        eoo <-
          ConR::EOO.computing(XY = data_select, export_shp = TRUE)

        locations <-
          ConR::locations.comp(XY = data_select, nbe_rep = input$repeat_pos_aoo)

        aoo <-
          ConR::AOO.computing(XY = data_select,
                              export_shp = T,
                              nbe.rep.rast.AOO = input$repeat_pos_aoo, Cell_size_AOO = input$aoo_km_res)


        subpop <-
          ConR::subpop.comp(XY = data_select, Resol_sub_pop = input$sub_pop_resol)


        if(length(eoo)>1) eoo_poly <-
          sf::st_as_sf(eoo$spatial.polygon_1)

        aoo_poly <-
          sf::st_as_sf(aoo[[2]][[1]])

        locations_poly <-
          sf::st_as_sf(locations[[1]])

        subpop_poly <-
          sf::st_as_sf(subpop[[2]])

        showElement("eval_species_res")

        output$title_eval <- renderText({
          paste("<i><strong>", Name, "<strong><i>")
        })

        output$results_conr <-
          renderText({
            paste(paste("Number of unique occurrences (unique georeferences):", conr_results$Nbe_unique_occ.),
                  paste("Extent of Occurrences (km2):", conr_results$EOO),
                  paste("Area of Occupancy (km2):", aoo[[1]]),
                  paste("Number of locations:", locations[[2]]),
                  paste("Number of locations as protected areas:", conr_results$Nbe_loc_PA),
                  paste("Number of sub-populations:", conr_results$Nbe_subPop),
                  paste("Number of occurrences within protected areas:", nrow(table_occupied_pa)),
                  paste("Ratio of occurrences within protected areas:", conr_results$Ratio_occ_within_PA),
                  paste("Preliminary IUCN category based on Criterion B:", conr_results$Category_CriteriaB),
                  paste("IUCN code", conr_results$Category_code),
                  sep = "\n")
          })

        incProgress(3/3, detail = paste("Mapping", 3))

        output$map2 <- mapview::renderMapview({

          map_types <- c("Esri.NatGeoWorldMap", "Esri.WorldImagery", "OpenStreetMap.DE",
                         "Esri.WorldPhysical")

          if(!is.na(conr_results$EOO)) {

              mapview::mapview(dataset_circle_select, map.types = map_types,
                             col.regions = "red", alpha.regions = 0.1, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(dataset_sf, col.regions = "red", map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(eoo_poly, col.regions = "blue", alpha.regions = 0.05, map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(locations_poly, col.regions = "pink", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(aoo_poly, col.regions = "red", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(subpop_poly, col.regions = "green", lwd =3, alpha.regions = 0.05, map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(protected_areas_bbox, col.regions = "green", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "Protected areas", viewer.suppress=T) +
              mapview::mapview(rast_mayaux_crop, col.regions = "red", alpha.regions = alpha_raster_mayaux$df, map.types = map_types, legend =FALSE, layer.name = "mayaux human dominated land cover", viewer.suppress=T)  +
              mapview::mapview(hansen_deforestation_aggreg_crop, col.regions = "purple", alpha.regions = alpha_raster_deforest$df, map.types = map_types, legend =FALSE, layer.name = "proportion of forest cover loss", viewer.suppress=T)  +
              mapview::mapview(mineral_deposit_crop_bbox, col.regions = "black", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "mineral deposit", viewer.suppress=T)# pal(100), at = seq(0, 1, 0.1)



          }else{
            mapview::mapview(dataset_circle_select, map.types = map_types,
                             col.regions = "red", alpha.regions = 0.1, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(dataset_sf, col.regions = "red", map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(locations_poly, col.regions = "pink", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(aoo_poly, col.regions = "red", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(subpop_poly, col.regions = "green", lwd =3, alpha.regions = 0.05, map.types = map_types, legend =FALSE, viewer.suppress=T) +
              mapview::mapview(protected_areas_bbox, col.regions = "green", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "Protected areas", viewer.suppress=T) +
              mapview::mapview(rast_mayaux_crop, col.regions = "red", alpha.regions = alpha_raster_mayaux$df, legend =FALSE, layer.name = "mayaux human dominated land cover", viewer.suppress=T)  +
              mapview::mapview(hansen_deforestation_aggreg_crop, col.regions = "purple", alpha.regions = alpha_raster_deforest$df, map.types = map_types, legend =FALSE, layer.name = "proportion of forest cover loss", viewer.suppress=T)  +
              mapview::mapview(mineral_deposit_crop_bbox, col.regions = "black", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "mineral deposit", viewer.suppress=T) # pal(100), at = seq(0, 1, 0.1)

          }
        })
      })

      # output$see_report <- renderUI({
      #   actionButton("see_report_tab", "Exporting summary")
      # })
      # output$evaluation_CA <- renderUI({
      #   actionButton("go_evaluation_CA", "Evaluation criterion A")
      # })



      output$evaluation_CA <- renderUI({
        shinyWidgets::actionBttn(
          inputId = "go_evaluation_CA",
          label = "Evaluation criterion A",
          color = "royal",
          style = "jelly"
        )
      })



    })

  })

  # CRITERION A evaluation ------------------------------------------------------------

  observeEvent(input$go_evaluation_CA, {
    showMenuItem("tab_EVAL2")
    updateTabItems(session, "tab_EVAL2")


    observeEvent(input$eval_species_CA, {
      Name <-list.names$df[as.numeric(input$name_chosen)]
      name_quo <- dplyr::enquo(Name)

      data_select <-
        original.dataset$df %>%
        filter(taxa==!!name_quo, !is.na(!!rlang::sym(input$sel_LAT)), !is.na(!!rlang::sym(input$sel_LONG))) %>%
        dplyr::select(!!rlang::sym(input$sel_LAT),
                      !!rlang::sym(input$sel_LONG), taxa)

      dataset_sf <-
        sf::st_as_sf(original.dataset$df %>%
                       filter(taxa==!!name_quo, !is.na(!!rlang::sym(input$sel_LAT)), !is.na(!!rlang::sym(input$sel_LONG))),
                     coords = c(input$sel_LONG, input$sel_LAT), crs = 4326)

      dataset_circle_select <-
        original.dataset.accuracy$df %>%
        filter(taxa==!!name_quo)

      criterionA_reduction <-
        IUCN_eval_CA(data = data_select, rasters =
                       c(raster_mayaux_cropped$df, raster_hansen_cropped$df),
                     mineral = mineral_cropped$df,
                     protected_areas = protected_areas_cropped$df,
                     thresholds_rasters =
                       c(input$threshold_mayaux_CA, input$deforest_CA),
                     col_coordinates = c(input$sel_LONG, input$sel_LAT),
                     col_tax = "taxa",
                     cells_size_aoo = input$aoo_km_res,
                     export_shp_eoo = TRUE)

    if(length(criterionA_reduction)>1) {
      eoo_poly_full <- sf::st_as_sf(criterionA_reduction$shapefiles$full_eoo_shp)
      if(!is.na(criterionA_reduction$shapefiles$left_eoo_shp)) {
        eoo_poly_left <- sf::st_as_sf(criterionA_reduction$shapefiles$left_eoo_shp)
      }else{
        eoo_poly_left <- eoo_poly_full
      }
    }

      criterionA_reduction_results <-
        criterionA_reduction$results

      criterionA_reduction_results <-
        criterionA_reduction_results %>%
        mutate(AOO_decline=(AOO_full-AOO_left)/AOO_full*100) %>%
        mutate(EOO_decline=(EOO_full-EOO_left)/EOO_full*100)

      criterionA_reduction_results <-
        criterionA_reduction_results %>%
        tibble::add_column(category_code_ca_aoo = plyr::aaply(criterionA_reduction_results$AOO_decline, 1, cat_criterion_A))

      if(!is.na(criterionA_reduction_results$EOO_decline)) {
        criterionA_reduction_results <-
          criterionA_reduction_results %>%
          tibble::add_column(category_code_ca_eoo = plyr::aaply(criterionA_reduction_results$EOO_decline, 1, cat_criterion_A))
      }else{
        criterionA_reduction_results <-
          criterionA_reduction_results %>%
          tibble::add_column(category_code_ca_eoo = NA)
      }

      showElement("eval_species_res_CA")

      output$title_eval_CA <- renderText({
        paste("<i><strong>", Name, "<strong><i>")
      })

      output$results_CA <-
        renderText({
          paste(paste("Number of unique occurrences (unique georeferences):",
                      criterionA_reduction_results$nbe_occ),
                paste("Number of unique occurrences impacted:",
                      criterionA_reduction_results$nbe_occ_human_impacted),
                paste("Area of Occupancy (km2):", criterionA_reduction_results$AOO_full),
                paste("Area of Occupancy excluding impacted occurrences:",
                      criterionA_reduction_results$AOO_left),
                paste("Extent of occurrences  (km2):",
                      criterionA_reduction_results$EOO_full),
                paste("Extent of occurrences excluding impacted occurrences:",
                      criterionA_reduction_results$EOO_left),
                paste("Inferred AOO decline (%):",
                      round(criterionA_reduction_results$AOO_decline, 2)),
                paste("Inferred EOO decline (%)",
                      round(criterionA_reduction_results$EOO_decline, 2)),
                paste("Category based on AOO decline:",
                      criterionA_reduction_results$category_code_ca_aoo),
                paste("Category based on EOO decline:",
                      criterionA_reduction_results$category_code_ca_eoo),
                sep = "\n")
        })

      output$map_CA <- mapview::renderMapview({

        map_types <- c("Esri.NatGeoWorldMap", "Esri.WorldImagery", "OpenStreetMap.DE",
                       "Esri.WorldPhysical")

        if(!is.na(criterionA_reduction_results$EOO_full)) {
          mapview::mapview(dataset_circle_select, map.types = map_types,
                           col.regions = "red", alpha.regions = 0.1, legend =FALSE, viewer.suppress=T) +
            mapview::mapview(dataset_sf, col.regions = "red", map.types = map_types, legend =FALSE, viewer.suppress=T) +
            mapview::mapview(eoo_poly_full, col.regions = "blue", alpha.regions = 0.05, map.types = map_types, legend =FALSE, viewer.suppress=T) +
            mapview::mapview(eoo_poly_left, col.regions = "red", alpha.regions = 0.1, map.types = map_types, legend =FALSE, viewer.suppress=T) +
            mapview::mapview(protected_areas_cropped$df, col.regions = "green", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "Protected areas", viewer.suppress=T) +
            mapview::mapview(raster_mayaux_cropped$df, col.regions = "red", alpha.regions = alpha_raster_mayaux$df, map.types = map_types, legend =FALSE, layer.name = "mayaux human dominated land cover", viewer.suppress=T)  +
            mapview::mapview(raster_hansen_cropped$df, col.regions = "purple", alpha.regions = alpha_raster_deforest$df, map.types = map_types, legend =FALSE, layer.name = "proportion of forest cover loss", viewer.suppress=T)  +
            mapview::mapview(mineral_cropped$df, col.regions = "black", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "mineral deposit", viewer.suppress=T)# pal(100), at = seq(0, 1, 0.1)

        }else{
          mapview::mapview(dataset_circle_select, map.types = map_types,
                           col.regions = "red", alpha.regions = 0.1, legend =FALSE, viewer.suppress=T) +
            mapview::mapview(dataset_sf, col.regions = "red", map.types = map_types, legend =FALSE, viewer.suppress=T) +
            mapview::mapview(protected_areas_cropped$df, col.regions = "green", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "Protected areas", viewer.suppress=T) +
            mapview::mapview(raster_mayaux_cropped$df, col.regions = "red", alpha.regions = alpha_raster_mayaux$df, map.types = map_types, legend =FALSE, layer.name = "mayaux human dominated land cover", viewer.suppress=T)  +
            mapview::mapview(raster_hansen_cropped$df, col.regions = "purple", alpha.regions = alpha_raster_deforest$df, map.types = map_types, legend =FALSE, layer.name = "proportion of forest cover loss", viewer.suppress=T)  +
            mapview::mapview(mineral_cropped$df, col.regions = "black", alpha.regions = 0.3, map.types = map_types, legend = TRUE, layer.name = "mineral deposit", viewer.suppress=T)# pal(100), at = seq(0, 1, 0.1)

        }
      })

      # output$summary_CA <- renderPrint({
      #   print(input$aoo_km_res)
      #   print(criterionA_reduction_results)
      #
      #
      # })



      output$see_report <- renderUI({
        actionButton("see_report_tab", "Exporting summary")


    })


    })
    })


  observeEvent(input$info_mayaux, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Information",
      text = "The land-cover map is the one published by Mayaux et al. 2000. \nA new raster was created for each land cover type by aggregating the original raster at 10 km resolution and computing \nthe proportion of each land cover type within 10 km² cells. We identified seven land cover types indicative of moderate to high level of human impact: degraded evergreen forests, mosaic forests/croplands, croplands (>50%), croplands with open woody vegetation, irrigated croplands, tree crops and cities.",
      type = "info"
    )
  })


  observeEvent(input$info_deforest, {
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Information",
      text = "Rasters informing on forest cover loss were downloaded on Global Forest Change 2000–2018 website. Resolution of rasters is originally 1 arc-second per pixel. Theses rasters indicate 'Forest loss during the period 2000–2018, defined as a stand-replacement disturbance, or a change from a forest to non-forest state'. There were aggregated at ca. 10 km² and the proportion of forest cover loss pixel was computed within each cell",
      type = "info"
    )
  })
  observeEvent(input$see_report_tab, {
    showMenuItem("tab_SUMMARY")
    updateTabItems(session, "tab_SUMMARY")

  })


  ##### download the report
  output$species_report <- downloadHandler(
    filename = function() {
      paste0("Report_", Sys.Date(), ".html")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(system.file("Rmardown", "species_report.Rmd", package = "conrappli"),
                tempReport,
                overwrite = TRUE
      )

      if (file.exists(file)) {
        file.remove(file)
      }

      rmarkdown::render(tempReport, output_file = file)
    },
    contentType = "text/html"
  )


  }
