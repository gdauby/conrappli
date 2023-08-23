
#' @title Validation of data
#'
#' @description Those functions are used to validate data imported by user:
#'   * `prepare_data_validate()` create new validation columns for relevant criteria.
#'   * `validation_rules` return validation rules.
#'
#' @param data A `data.frame` or similar.
#' @param lat,lon,sci_names Variables names used for validation.
#'
#' @return
#'   * `prepare_data_validate()` `data.frame` with new columns.
#'   * `validation_rules` a set of validation rules defined with [validate::validator()].
#'
#'
#' @export
#'
#' @name validation
#'
#' @importFrom bdc bdc_coordinates_outOfRange bdc_coordinates_empty bdc_scientificName_empty
prepare_data_validate <- function(data,
                                  lat = "Latitude",
                                  lon = "Longitude",
                                  sci_names = "Taxa") {
  data <- bdc::bdc_coordinates_outOfRange(data, lat = lat, lon = lon)
  data <- bdc::bdc_coordinates_empty(data, lat = lat, lon = lon)
  data <- bdc::bdc_scientificName_empty(data, sci_names = sci_names)
  if (hasName(data, ".__year")) {
    data <- dplyr::mutate(
      data,
      .__year = as.numeric(.__year),
      .valid_year = !is.na(.__year) & .__year >= 1700
    )
  } else {
    data <- dplyr::mutate(
      data,
      .valid_year = TRUE
    )
  }
  return(data)
}

validation_cols <- function() {
  c(".coordinates_outOfRange", ".coordinates_empty", ".scientificName_empty", ".valid_year")
}

#' @export
#'
#' @rdname validation
#'
#' @importFrom validate validator label
validation_rules <- function() {
  validation_rules <- validate::validator(
    .coordinates_outOfRange == TRUE,
    .coordinates_empty == TRUE,
    .scientificName_empty == TRUE,
    .valid_year == TRUE
  )
  validate::label(validation_rules) <- c(
    "Identify records with out-of-range geographic coordinates",
    "Identify records with empty geographic coordinates",
    "Identify records with empty scientific names",
    "Identify records with invalid year (if provided)"
  )
  return(validation_rules)
}



#' @export
#'
#' @rdname validation
#'
#' @importFrom dplyr filter if_all all_of select any_of
exclude_violating_records <- function(data) {
  dplyr::filter(
    data,
    dplyr::if_all(dplyr::all_of(validation_cols()), ~ . == TRUE)
  ) %>%
    dplyr::select(!dplyr::any_of(validation_cols()))
}

#' @export
#'
#' @rdname validation
#'
#' @importFrom dplyr mutate if_all all_of select any_of
identify_violating_records <- function(data) {
  # dplyr::mutate(
  #   data,
  #   STATUS_CONR = dplyr::if_all(dplyr::all_of(validation_cols()), ~ . == TRUE),
  #   STATUS_CONR = ifelse(STATUS_CONR == TRUE, "IN", "OUT")
  # ) %>%
  #   dplyr::select(!dplyr::any_of(validation_cols()))
  data %>%
    dplyr::mutate(
      .id = seq_len(dplyr::n()),
      STATUS_CONR = dplyr::if_all(dplyr::all_of(validation_cols()), ~ . == TRUE),
      STATUS_CONR = ifelse(STATUS_CONR == TRUE, "IN", "OUT")
    ) %>%
    tidyr::pivot_longer(
      cols = validation_cols(),
      names_to = "STATUS_DESC",
      values_to = "validation_result"
    ) %>%
    dplyr::mutate(
      STATUS_DESC = dplyr::case_when(
        validation_result == FALSE & STATUS_DESC == ".coordinates_outOfRange" ~ "out-of-range geographic coordinates",
        validation_result == FALSE & STATUS_DESC == ".coordinates_empty" ~ "empty geographic coordinates",
        validation_result == FALSE & STATUS_DESC == ".scientificName_empty" ~ "empty scientific names",
        validation_result == FALSE & STATUS_DESC == ".valid_year" ~ "invalid year",
        TRUE ~ NA_character_
      )
    ) %>%
    # dplyr::filter(validation_result == FALSE) %>%
    dplyr::group_by(dplyr::across(setdiff(names(.), c("STATUS_DESC", "validation_result")))) %>%
    dplyr::summarise(STATUS_DESC = paste(STATUS_DESC[!is.na(STATUS_DESC)], collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.id)
}

#' @export
#'
#' @rdname validation
#'
#' @importFrom dplyr mutate n case_when filter group_by across summarise select
#' @importFrom tidyr pivot_longer
extract_violating_records <- function(data) {
  tidyr::pivot_longer(
    data = data %>% dplyr::mutate(.id = seq_len(dplyr::n())),
    cols = validation_cols(),
    names_to = "validation_label",
    values_to = "validation_result"
  ) %>%
    dplyr::mutate(
      validation_label = dplyr::case_when(
        validation_label == ".coordinates_outOfRange" ~ "out-of-range geographic coordinates",
        validation_label == ".coordinates_empty" ~ "empty geographic coordinates",
        validation_label == ".scientificName_empty" ~ "empty scientific names",
        validation_label == ".valid_year" ~ "invalid year",
        TRUE ~ ""
      )
    ) %>%
    dplyr::filter(validation_result == FALSE) %>%
    dplyr::group_by(dplyr::across(setdiff(names(.), c("validation_label", "validation_result")))) %>%
    dplyr::summarise(validation_label = paste(validation_label, collapse = ", ")) %>%
    dplyr::select(-.id)
}




