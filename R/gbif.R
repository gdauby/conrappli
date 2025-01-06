
#' Search species information
#'
#' @param species_name Character of species names.
#' @param match_type Type of match for scientific names found: exact match or above confidence level specified.
#' @param confidence_level Confidence level to use for matching scientific names.
#'
#' @return A `tibble` with info found for each species provided.
#' @export
#'
#' @importFrom dplyr bind_rows as_tibble filter
#' @importFrom taxize get_gbifid_
#'
#' @examples
#' \dontrun{
#' search_species_info(c("Uapaca niangadoumae"))
#' search_species_info(c("Uapaca niangadoumae"), match_type = "exact")
#' search_species_info(c("Uapaca niangadoumae", "do not exist"))
#' }
search_species_info <- function(species_name, match_type = c("exact", "confidence"), confidence_level = 95) {
  match_type <- match.arg(match_type, several.ok = TRUE)
  species_name <- unique(species_name)
  species_name <- species_name[!is.na(species_name)]
  infos <- taxize::get_gbifid_(sci = species_name, method = "backbone")
  infos <- bind_rows(infos, .id = "provided_sciname")
  if (nrow(infos) < 1)
    return(infos)
  infos <- filter(infos, kingdom == "Plantae")
  infos <- as_tibble(infos)
  infos_exact <- filter(infos, matchtype == "EXACT") %>% filter(status == "ACCEPTED" | status == "SYNONYM")
  infos_conf <- filter(infos, matchtype != "EXACT") %>% filter(status == "ACCEPTED" & confidence > confidence_level |
                                                               status == "SYNONYM" & confidence > confidence_level)

  if (identical(match_type, "exact")) {
    return(infos_exact)
  } else if (identical(match_type, "exact")) {
    return(infos_conf)
  } else {
    return(bind_rows(infos_exact, infos_conf))
  }
}


#' Retrieve GBIF occurrences
#'
#' @param specieskey A vector of species key.
#'
#' @return A `tibble`.
#' @export
#' 
#' @importFrom rgbif occ_data
#' @examples
#' \dontrun{
#'
#' infos <- search_species_info(c("Uapaca niangadoumae"))
#'  retrieve_occ_data(infos$specieskey)
#'
#' }
retrieve_occ_data <- function(specieskey) {
  res <- rgbif::occ_data(taxonKey = unique(specieskey), limit = 100000, hasCoordinate = TRUE, hasGeospatialIssue = FALSE)
  if (identical(attr(res, "type"), "many")) {
    dplyr::bind_rows(lapply(X = res, FUN = `[[`, "data"))
  } else {
    res$data
  }
}

#' Retrieve GBIF occurrences from polygon
#'
#' @param poly A sf
#' @param threshold numeric
#'
#' @return A `tibble`.
#' 
#' @importFrom rgbif occ_count
#' @export
query_gbif_poly <- function(poly, threshold = 500) {
  
  poly <- sf::st_transform(sf::st_make_valid(poly), 4326)
  
  if (inherits(poly, "sf")) {
    # st_union(st_cast(p_geo, "POLYGON"))
    p_geo <- sf::st_cast(sf::st_combine(poly$geometry), to = "MULTIPOLYGON")
    sss_txt <- sf::st_as_text(p_geo)
  } else if (inherits(poly, "sfc")) {
    sss_txt <- sf::st_as_text(poly)
  } else {
    stop("'poly' must be an 'sf' or 'sfc' object.")
  }
  
  print(Sys.time())
  res <- rgbif::occ_data(geometry = sss_txt, 
                         kingdomKey = 6, 
                         limit = 100000)
  
  print(paste("Extract occurences in polygon :", nrow(res$data)))
  
  keys <- res$data %>%
    # filter(iucnRedListCategory != c("LC")) %>% 
    filter(!is.na(specificEpithet)) %>% 
    distinct(speciesKey)
  
  
  print(Sys.time())
  
  ttt <- apply(keys, MARGIN = 1, FUN = function(x) rgbif::occ_count(taxonKey = x))
  
  print(head(ttt))
  print(Sys.time())
  
  keys_filtered <- 
    keys %>% 
    mutate(count = ttt) %>% 
    filter(count < threshold)
  
  print(paste("Extract polygon gbif done,", nrow(keys_filtered), "species kept after filtering"))
  
  res_occ_full <- retrieve_occ_data(specieskey = keys_filtered$speciesKey)
  print(Sys.time())
  
  print("Extract occurences gbif done")
  
  return(list(
    extract_all_tax = res_occ_full
  ))
}




# tc <- function(l) Filter(Negate(is.null), l)
# 
# args <- tc(
#   list(
#     format = 'json',
#     name = sci,
#     nameid = nameid,
#     commonname = com,
#     orderby = orderby,
#     sortorder = sortorder,
#     pagesize = pagesize,
#     startrow = startrow,
#     type = type,
#     apikey = key
#   )
# )
# 
# 
# tax_gbif <- res$data %>% distinct(genus, specificEpithet) %>% filter(!is.na(specificEpithet)) %>% 
#   mutate(tax = paste(genus, specificEpithet)) %>% 
#   distinct(tax)
# 
# dt2df <- function(x, idcol = TRUE) {
#   (data.table::setDF(
#     data.table::rbindlist(x, use.names = TRUE, fill = TRUE, idcol = idcol)))
# }
# 
# 
# 
# load("./inst/tropicos_key/TROPICOS_KEY.RData")
# library(tidyverse)
# 
# res_tp <- list()
# for (i in 1:nrow(tax_gbif)) {
#   
#   tp_names <- taxize::tp_search(sci = tax_gbif %>% slice(i) %>% pull(tax), key = TROPICOS_KEY)
#   # tp_names <- taxize::tp_search(sci = "Aucoumea klaineana", key = TROPICOS_KEY)
#   
#   for (j in 1:nrow(tp_names)) {
#     
#     nameid=tp_names$nameid[j]
#     
#     # 'http://services.tropicos.org/Name/4700329/Specimens'
#     url = glue::glue("http://services.tropicos.org/Name/{nameid}/Specimens")
#     cli <- crul::HttpClient$new(url = url)
#     query <- tc(list(format='json',
#                      apikey=key))
#     
#     res <- cli$get(query = query)
#     res$raise_for_status()
#     if (grepl("exception occurred", res$parse("UTF-8"), ignore.case = TRUE)) {
#       stop("500 - a server error occurred, try again later")
#     }
#     tt <- res$parse("UTF-8")
#     out <- jsonlite::fromJSON(tt, FALSE)
#     ggg <- dt2df(out)
#     
#     res_tp[[length(res_tp) + 1]] <-  ggg %>% as_tibble() %>% mutate(scientificname = tp_names$scientificname[j])
#     
#   }
# }







