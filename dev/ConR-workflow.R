library(ConR)
## generation d'un jeu de donnees artificel pour reproduire/tester le workflow
set.seed(3)
nbe_occ <- sample(seq(1, 30, 1), 20, replace = TRUE) ### 500 species with number of occurrences sampled between 3 and 100
ddlat <- lapply(nbe_occ, function(x) sample(seq(-10, 15, 0.1), x))
ddlon <- lapply(nbe_occ, function(x) sample(seq(7, 25, 0.1), x))
names(ddlon) <- names(ddlat) <- paste0("tax", seq(1,length(ddlat), 1))
ddlat <- lapply(seq_along(ddlat), function(i) data.frame(ddlat =  ddlat[[i]]))
ddlon <- lapply(seq_along(ddlon), function(i) data.frame(ddlon =  ddlon[[i]], taxa = names(ddlon)[i]))
test_data <-
  data.frame(ddlat = do.call('rbind', ddlat), ddlon = do.call('rbind', ddlon)[,1], taxa = do.call('rbind', ddlon)[,2])


eoo_res <-
  EOO.computing(XY = test_data,
                mode = "planar",  # input$mode_eoo a définir par utilisateur : 'spheroid' or 'planar'. By default 'spheroid'
                export_shp = TRUE)

eoo_res$spatial
eoo_res$results

aoo_res <- AOO.computing(XY = test_data,
                         Cell_size_AOO = 2, # input$aoo_size min=0.1, max=50, value = 2, round=TRUE, step=1
                         nbe.rep.rast.AOO = 0,  # input$rep_rast min = 0, max = 30, value = 10, round=TRUE, step=1
                         export_shp = T)



check_overlap <- extract_overlap_shp(XY = test_data)

check_overlap$shp_tables %>%
  dplyr::select(id, table_name, type, description, reference)



if (any(check_overlap$shp_tables$overlap)) {
  all_shp <-
    collect_shp(table_names = check_overlap$shp_tables[which(check_overlap$shp_tables$overlap),],
                XY_sf = check_overlap$XY_sf)
} else {
  all_shp <-
    NULL
}



locations <- locations.comp(XY = test_data,
                            Cell_size_locations = 10,  # input$locations_size min=0.1, max=50, value = 10, round=TRUE, step=1
                            threat_list =  all_shp,
                            method_polygons = "no_more_than_one"  # input$method_polygons a définir par utilisateur : 'spheroid' or 'planar'. By default 'spheroid'
)


categories <-
  cat_criterion_b(EOO = eoo_res$results$eoo,
                  AOO = aoo_res$AOO$aoo,
                  locations = locations$locations$locations)

results_full <-
  data.frame(
    taxa = row.names(aoo_res$AOO),
    EOO = eoo_res$results$eoo,
    AOO = aoo_res$AOO$aoo,
    locations = locations$locations$locations,
    category = categories$ranks_B,
    cat_codes = categories$cats_code,
    issue_aoo = aoo_res$AOO$issue_aoo,
    issue_eoo = eoo_res$results$issue_eoo,
    issue_locations = locations$locations$issue_locations
  )

