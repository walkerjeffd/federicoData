#' Clean water quality dataset from DBHYDRO
#'
#' @param x tibble returned from dbhydro_get_wq()
#'
#' @return tibble with cleaned water quality data
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' x <- dbhydro_get_wq(
#'   station_ids = "LOX3",
#'   wq_param = "TP",
#'   date_min = "2019-09-01",
#'   date_max = "2019-10-31"
#' )
#' dbhydro_clean_wq(x)
#' }
dbhydro_clean_wq <- function (x) {
  logger::log_debug("cleaning wq dataset from DBHYDRO (nrow = {nrow(x)})")

  empty_rows <- sum(is.na(x$station_id))
  if (empty_rows > 0) {
    logger::log_warn("removing {empty_rows} empty rows")
    x <- dplyr::filter(x, !is.na(station_id))
  }

  if (any(!x$test_name %in% dbhydro_wq_params$test_name)) {
    missing_values <- setdiff(unique(x$test_name), dbhydro_wq_params$test_name)
    logger::log_warn("dataset contains test_name values that are not defined in dbhydro_wq_params ({paste0(missing_values, collapse = ',')})")
  }

  x %>%
    dplyr::mutate_at(
      c(
        "project_code",
        "station_id",
        "sample_id",
        "sample_type_new",
        "collection_method",
        "depth_unit",
        "matrix",
        "test_name",
        "method",
        "uncertainty",
        "units",
        "remark_code",
        "flag",
        "lims_number",
        "collection_agency",
        "source",
        "owner",
        "validation_level",
        "validator",
        "sampling_purpose",
        "data_investigation",
        "qc_type",
        "program_type",
        "sample_comments",
        "result_comments",
        "first_trigger_date",
        "collection_date",
        "measure_date",
        "receive_date",
        "filtration_date"
      ),
      as.character
    ) %>%
    dplyr::mutate_at(
      c(
        "depth",
        "value",
        "sigfig_value",
        "mdl",
        "pql",
        "rdl",
        "t_depth",
        "upper_depth",
        "lower_depth",
        "dcs_meters"
      ),
      as.numeric
    ) %>%
    dplyr::mutate_at(
      c(
        "test_number",
        "storet_code",
        "sample_type",
        "discharge",
        "up_down_stream",
        "weather_code",
        "ndec"),
      as.integer
    ) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$date)
    ) %>%
    dplyr::mutate_at(
      c("first_trigger_date", "collection_date", "measure_date", "receive_date", "filtration_date"),
      lubridate::dmy_hm,
      tz = "US/Eastern"
    ) %>%
    dplyr::left_join(
      dbhydro_wq_params,
      by = "test_name"
    ) %>%
    dplyr::select(c(
      "station_id",
      "wq_param",
      "date",
      "value",
      "units",
      "flag",
      "sample_type_new",
      "collection_method",
      "project_code",
      "sample_id",
      "depth",
      "depth_unit",
      "matrix",
      "test_number",
      "test_name",
      "storet_code",
      "method",
      "first_trigger_date",
      "collection_date",
      "measure_date",
      "receive_date",
      "sigfig_value",
      "uncertainty",
      "mdl",
      "pql",
      "rdl",
      "remark_code",
      "lims_number",
      "collection_agency",
      "source",
      "owner",
      "validation_level",
      "validator",
      "sampling_purpose",
      "data_investigation",
      "t_depth",
      "upper_depth",
      "lower_depth",
      "dcs_meters",
      "filtration_date",
      "sample_type",
      "qc_type",
      "discharge",
      "up_down_stream",
      "weather_code",
      "program_type",
      "ndec",
      "sample_comments",
      "result_comments"
    ))
}
