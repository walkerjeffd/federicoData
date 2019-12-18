#' Update hydrologic and water quality data for trackers
#'
#' @param con database connection object
#' @param ids character vector of tracker IDs, or NULL to update all trackers
#' @param date_min start date for period to update, or NULL to use dbkey start date
#' @param date_max end date for period to update, or NULL to use dbkey end date
#' @param dbhydro_hydro_batch_size batch size passed to \code{dbhydro_batch_get_hydro()} (# dbkeys per batch)
#' @param dbhydro_wq_batch_size batch size passed to \code{dbhydro_batch_get_wq()} (# stations per batch)
#' @param usgs_dv_batch_size batch size passed to \code{usgs_batch_get_dv()} (# stations per batch)
#'
#' @return TRUE if operation was successful
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' tracker_update(con, ids = "test-tracker", date_min = "2019-11-01", date_max = "2019-12-01")
#' }
tracker_update <- function (con, ids = NULL, date_min = NULL, date_max = NULL, dbhydro_hydro_batch_size = 5, dbhydro_wq_batch_size = 5, usgs_dv_batch_size = 5) {
  logger::log_info("updating data for trackers ({ifelse(is.null(ids), 'ALL', paste0(ids, collapse = ', '))}) over period ({ifelse(is.null(date_min), 'N/A', date_min)}, {ifelse(is.null(date_max), 'N/A', date_max)})")

  stopifnot(tracker_update_dbhydro_hydro(con, ids = ids, date_min = date_min, date_max = date_max, batch_size = dbhydro_hydro_batch_size))
  stopifnot(tracker_update_dbhydro_wq(con, ids = ids, date_min = date_min, date_max = date_max, batch_size = dbhydro_wq_batch_size))
  stopifnot(tracker_update_usgs_dv(con, ids = ids, date_min = date_min, date_max = date_max, batch_size = usgs_dv_batch_size))

  TRUE
}
