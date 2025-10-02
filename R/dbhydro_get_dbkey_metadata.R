#' Get DBKEY metadata from DBHYDRO
#'
#' @param dbkeys character vector of DBKEYs
#'
#' @return tibble containing metadata for specified DBKEYs
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' dbhydro_get_dbkey_metadata(dbkeys = c("KQ646"))
#' dbhydro_get_dbkey_metadata(dbkeys = c("KQ646", "HD889"))
#' }
dbhydro_get_dbkey_metadata <- function (dbkeys) {
  logger::log_info("fetching metadata for {length(dbkeys)} dbkey(s)")

  stopifnot(
    all(!is.na(dbkeys)),
    all(!duplicated(dbkeys))
  )

  x <- dplyr::bind_rows(lapply(dbkeys, dbhydroInsights::get_timeseries_metadata))

  if (nrow(x) < length(dbkeys)) {
    missing_dbkeys <- setdiff(dbkeys, x$dbkey)
    logger::log_warn("metadata not found for {length(missing_dbkeys)} dbkey(s): {paste0(missing_dbkeys, collapse = ',')}")
  }

  x <- janitor::clean_names(x)
  x <- dplyr::select(
    x,
    "dbkey" = .data$dbkey,
    "station_id" = .data$station,
    # site_group -- DBHydroInsights does not provide site_group info
    "data_type" = .data$data_type,
    "freq" = .data$frequency,
    "stat" = .data$statistic_type,
    "recorder" = .data$recorder,
    "agency" = .data$agency,
    # start_date -- DBHydroInsights does not provide start_date info
    # end_date -- DBHydroInsights does not provide end_date info
    "strata" = .data$strata,
    # op_num -- DBHydroInsights does not provide op_num info
    # struct -- DBHydroInsights does not provide struct type
  )
  x
}
