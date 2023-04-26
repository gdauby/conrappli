#' @title Filtering out
#'
#' @description Filertting out based on numner of nunique occurences
#'
#' @param data A `data.frame` or similar.
#' @param threshold an integer
#'
#' @return
#'   * `data_filtered` `data.frame` same as data filtered out according to threshold
#'   * `validation_rules` a set of validation rules defined with [validate::validator()].
#'
#'
#' @export
#'
#' @name filtering_out
#'
#' @importFrom dplyr group_by count filter
filtering_out_data <- function(data, threshold) {
  
  species_to_keep <- 
    data %>% 
    dplyr::group_by(.__taxa) %>% 
    dplyr::count() %>% 
    dplyr::filter(n < threshold)
  
  data_filtered <- data %>% 
    dplyr::filter(.__taxa %in% species_to_keep$.__taxa)
  
  return(data_filtered)
}