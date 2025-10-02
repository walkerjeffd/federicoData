#' Fetch tracker data from database
#'
#' Retrieves all hydrologic and water quality
#' data for specified tracker and period
#'
#' @param con database connection object
#' @param id tracker ID
#' @param date_min start date
#' @param date_max end date
#'
#' @return list with elements named \code{tracker}, \code{hydro}, and \code{wq}
#' @export
#'
#' @examples
#' \dontrun{
#' tracker_data(con, id = "test-tracker") # all data
#' tracker_data(
#'   con,
#'   id = "test-tracker",
#'   date_min = "2018-10-01",
#'   date_max = "2019-09-30"
#' ) # specific period
#' }
tracker_data <- function(con, id, date_min = NULL, date_max = NULL) {
  logger::log_info("fetching data for tracker ({id}) from {ifelse(is.null(date_min), 'N/A', date_min)} to {ifelse(is.null(date_max), 'N/A', date_max)}")

  tracker <- tracker_get(con, ids = id) |>
    purrr::transpose() |>
    purrr::pluck(1)

  df_dbhydro_hydro <- NULL
  df_dbhydro_wq <- NULL
  df_usgs_dv <- NULL

  if (!is.null(tracker$dbhydro_hydro) && nrow(tracker$dbhydro_hydro) > 0) {
    df_dbhydro_hydro <- db_get_dbhydro_hydro(con, dbkeys = tracker$dbhydro_hydro$dbkey, date_min = date_min, date_max = date_max)
  }
  if (!is.null(tracker$dbhydro_wq) && nrow(tracker$dbhydro_wq) > 0) {
    df_dbhydro_wq <- db_get_dbhydro_wq(con, station_ids = tracker$dbhydro_wq$station_id, date_min = date_min, date_max = date_max)
  }
  if (!is.null(tracker$usgs_dv) && nrow(tracker$usgs_dv) > 0) {
    df_usgs_dv <- db_get_usgs_dv(con, station_ids = tracker$usgs_dv$station_id, date_min = date_min, date_max = date_max)
  }

  list(
    tracker = tracker,
    dbhydro_hydro = df_dbhydro_hydro,
    dbhydro_wq = df_dbhydro_wq,
    usgs_dv = df_usgs_dv
  )
}
