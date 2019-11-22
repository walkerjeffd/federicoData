#' Clean water quality dataset from DBHYDRO
#'
#' @param x tibble returned from dbhydro_get_wq()
#'
#' @return tibble with primary columns
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' x <- dbhydro_get_wq(
#'   station_ids = "LOX3",
#'   date_min = "2019-09-01",
#'   date_max = "2019-10-31",
#'   test_name = "PHOSPHATE, TOTAL AS P"
#' )
#' dbhydro_clean_wq(x)
#' }
dbhydro_clean_wq <- function (x) {
  logger::log_debug("cleaning wq dataset from DBHYDRO (nrow = {nrow(x)})")

  x %>%
    dplyr::mutate_at(
      c(
        "project_code", "station_id", "sample_id", "sample_type_new", "collection_method",
        "matrix", "test_name", "qc_type", "sample_comments", "result_comments",
        "uncertainty", "units", "remark_code", "flag", "depth_unit"
      ),
      as.character
    ) %>%
    dplyr::mutate_at(
      c("depth", "value", "sigfig_value", "mdl", "pql", "rdl"),
      as.numeric
    ) %>%
    dplyr::mutate_at(c("test_number"), as.integer) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$date),
      datetime = lubridate::dmy_hm(.data$collection_date, tz = "US/Eastern"),
      measure_date = lubridate::dmy_hm(.data$measure_date, tz = "US/Eastern"),
      receive_date = lubridate::dmy_hm(.data$receive_date, tz = "US/Eastern")
    ) %>%
    dplyr::select(c(
      "project_code", "station_id",
      "sample_id", "date",
      "test_name",
      "value", "units",
      "sample_type_new", "collection_method",
      "sigfig_value", "mdl", "pql", "rdl", "uncertainty",
      "test_number", "qc_type", "matrix",
      "datetime", "depth", "depth_unit",
      "flag", "remark_code",
      "sample_comments", "result_comments"
    ))
}
