#' Add tracker to the database
#'
#' @param con database connection object
#' @param id tracker id (character)
#' @param description tracker description
#' @param hydro data frame containing columns \code{dbkey}, \code{date_min}, \code{date_max}, or NULL
#' @param wq data frame containing columns \code{station_id}, \code{wq_param}, \code{date_min}, \code{date_max}, or NULL
#' @param replace replace tracker if it already exists in database
#'
#' @return TRUE if tracker was successfully added to the database
#' @export
#'
#' @examples
#' \dontrun{
#' tracker_add(con, "test-tracker", "A testing tracker", df_dbkeys, df_wq_stations)
#' }
tracker_add <- function(con, id, description, hydro = NULL, wq = NULL, replace = FALSE) {
  logger::log_info("adding tracker {id}")

  if (replace) {
    logger::log_debug("removing existing tracker (if exists)")
    tracker_remove(id)
  }

  if (!is.null(hydro)) {
    dbkeys <- unique(hydro[["dbkey"]])
    logger::log_debug("adding {length(dbkeys)} dbkeys for tracker ({id})")
    db_add_dbhydro_dbkeys(con, dbkeys = dbkeys)
  }

  if (!is.null(wq)) {
    if (any(is.na(wq[["wq_param"]]))) {
      logger::log_error("wq tibble cannot contain NA's in wq_param column")
      return(FALSE)
    }
    if (!all(wq[["wq_param"]] %in% dbhydro_wq_params[["wq_param"]])) {
      unknown_wq_params <- setdiff(unique(wq[["wq_param"]]), dbhydro_wq_params[["wq_param"]])
      logger::log_error("wq tibble contains unknown wq_param values ({paste0(unknown_wq_params, collapse = ', ')})")
      return(FALSE)
    }
    station_ids <- unique(wq[["station_id"]])
    logger::log_debug("adding {length(station_ids)} wq stations for tracker ({id})")
    db_add_dbhydro_stations(con, station_ids = station_ids)
  }

  logger::log_debug("inserting tracker into trackers table")
  df_tracker <- tibble::tibble(
    id = id,
    description = description
  )
  DBI::dbWriteTable(con, "trackers", df_tracker, append = TRUE, row.names = FALSE)

  if (!is.null(hydro)) {
    logger::log_debug("inserting dbkeys into trackers_hydro table")
    df_tracker_hydro <- tibble::tibble(
      tracker_id = id,
      dbkey = hydro[["dbkey"]],
      date_min = hydro[["date_min"]],
      date_max = hydro[["date_max"]]
    )
    DBI::dbWriteTable(con, "trackers_hydro", df_tracker_hydro, append = TRUE, row.names = FALSE)
  }

  if (!is.null(wq)) {
    logger::log_debug("inserting stations into trackers_wq table")
    df_tracker_wq <- tibble::tibble(
      tracker_id = id,
      station_id = wq[["station_id"]],
      wq_param = wq[["wq_param"]],
      date_min = wq[["date_min"]],
      date_max = wq[["date_max"]]
    )
    DBI::dbWriteTable(con, "trackers_wq", df_tracker_wq, append = TRUE, row.names = FALSE)
  }

  TRUE
}
