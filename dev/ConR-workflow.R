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

locations <- locations.comp(XY = test_data,
                            Cell_size_locations = 10 # input$locations_size min=0.1, max=50, value = 10, round=TRUE, step=1
)

categories <-
  cat_criterion_b(EOO = p1$results$eoo, AOO = aoo_res$AOO$aoo, locations = locations$locations$locations)

results_full <-
  data.frame(
    taxa = row.names(aoo_res),
    EOO = eoo_res$results$eoo,
    AOO = aoo_res$AOO$aoo,
    locations = locations$locations$locations,
    category = categories$ranks_B,
    cat_codes = categories$cats_code
  )
