
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
  return(data)
}

validation_cols <- function() {
  c(".coordinates_outOfRange", ".coordinates_empty", ".scientificName_empty")
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
    .scientificName_empty == TRUE
  )
  validate::label(validation_rules) <- c(
    "Identify records with out-of-range geographic coordinates",
    "Identify records with empty geographic coordinates",
    "Identify records with empty scientific names"
  )
  return(validation_rules)
}
