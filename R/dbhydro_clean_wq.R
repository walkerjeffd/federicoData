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

  if (nrow(x) > 0 && !all(x$parameter %in% dbhydro_wq_params$test_name)) {
    missing_values <- setdiff(unique(x$parameter), dbhydro_wq_params$test_name)
    logger::log_warn("dataset contains test_name values that are not defined in dbhydro_wq_params ({paste0(missing_values, collapse = ',')})")
  }

  x <- dplyr::left_join(
    x,
    dplyr::select(dbhydro_wq_params, "test_number" = .data$param_code, "wq_param"),
    by = "test_number"
  )
  x <- dplyr::transmute(
    x, 
    "station_id" = .data$station,
    "wq_param" = .data$wq_param,
    "date" = lubridate::as_date(.data$collect_date),
    "value" = .data$value,
    "units" = .data$units,
    "flag" = .data$flag,
    "sample_type_new" = .data$sample_type,
    "collection_method" = .data$collect_method,
    "project_code" = .data$project,
    "sample_id" = .data$sample_id,
    "depth" = .data$depth,
    "depth_unit" = .data$depth_units,
    "matrix" = .data$matrix,
    "test_number" = .data$test_number,
    "test_name" = .data$parameter,
    "storet_code" = .data$storet_code,
    "method" = .data$method,
    "first_trigger_date" = .data$first_trigger_date,
    "collection_date" = .data$collect_date,
    "measure_date" = .data$measure_date,
    "receive_date" = .data$receive_date,
    "sigfig_value" = .data$sig_fig_value,
    "uncertainty" = .data$uncertainty,
    "mdl" = .data$mdl,
    "pql" = .data$pql,
    "rdl" = .data$rdl,
    "remark_code" = .data$remark_code,
    "lims_number" = .data$lims_number,
    "collection_agency" = .data$collection_agency,
    "source" = .data$source,
    "owner" = .data$owner,
    "validation_level" = .data$validation_level,
    "validator" = .data$validator,
    "sampling_purpose" = .data$sampling_purpose,
    "data_investigation" = .data$data_investigation,
    "t_depth" = .data$total_depth,
    "upper_depth" = .data$upper_depth,
    "lower_depth" = .data$lower_depth,
    "dcs_meters" = .data$dcs_meters,
    "filtration_date" = .data$filtration_date,
    # "sample_type" = .data$sample_type,
    "qc_type" = ifelse(.data$sample_type == "SAMP", NA_character_, .data$sample_type),
    "discharge" = .data$discharge,
    # "up_down_stream" = .data$up_down_stream,
    "weather_code" = .data$weather_code,
    "program_type" = .data$program_type,
    "ndec" = .data$n_dec,
    "sample_comments" = .data$sample_comment,
    "result_comments" = .data$result_comment,
    "quality_code" = .data$quality_code
  )
}
