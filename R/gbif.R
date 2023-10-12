
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
#' @examples
#' \dontrun{
#'
#' infos <- search_species_info(c("Uapaca niangadoumae"))
#'  retrieve_occ_data(infos$specieskey)
#'
#' }
retrieve_occ_data <- function(specieskey) {
  res <- rgbif::occ_data(taxonKey = unique(specieskey), limit = 100000)
  if (identical(attr(res, "type"), "many")) {
    dplyr::bind_rows(lapply(X = res, FUN = `[[`, "data"))
  } else {
    res$data
  }
}

