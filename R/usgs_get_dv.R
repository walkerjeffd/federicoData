#' Get daily values dataset from USGS NWIS
#'
#' @param station_ids character vector of station ID(s)
#' @param param parameter name (must exist in usgs_params$param)
#' @param date_min start date
#' @param date_max end date
#' @param stat_code statistic code (default 00003 = daily mean)
#' @param raw returns raw dataset returned from dataRetrieval::read_waterdata_daily() if True, otherwise applies usgs_clean_dv()
#'
#' @return data frame (if raw = TRUE), otherwise tibble
#' @export
#'
#' @examples
#' \dontrun{
#' usgs_get_dv(
#'   station_ids = "263180080205001",
#'   param = "stage",
#'   date_min = "2018-01-01",
#'   date_max = "2018-02-01"
#' )
#' }
usgs_get_dv <- function(station_ids, param, date_min, date_max, stat_code = "00003", raw = FALSE) {
  logger::log_debug("fetching daily data from usgs for {param} at {length(station_ids)} station(s) from {date_min} to {date_max}")

  param_code <- usgs_params$param_code[which(usgs_params$param == param)]

  if (length(param_code) == 0) {
    logger::log_error("unknown value for param ({param}), must be found in usgs_params$param")
    stop("unknown parameter")
  }

  df <- dataRetrieval::read_waterdata_daily(
    monitoring_location_id = paste0("USGS-", station_ids),
    parameter_code = param_code,
    statistic_id = stat_code,
    time = c(as.character(date_min), as.character(date_max)),
    properties = c(
      "monitoring_location_id",
      "parameter_code",
      "statistic_id",
      "time",
      "value",
      "unit_of_measure",
      "approval_status"
    ),
    skipGeometry = TRUE
  )

  if (nrow(df) == 0) {
    logger::log_warn("USGS did not return any records")
  }

  if (!raw) {
    df <- usgs_clean_dv(df)
  }

  df
}
