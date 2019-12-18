#' Add tracker to the database
#'
#' @param con database connection object
#' @param id tracker id (character)
#' @param description tracker description
#' @param dbhydro_hydro data frame containing columns \code{dbkey}, \code{date_min}, \code{date_max}, or NULL
#' @param dbhydro_wq data frame containing columns \code{station_id}, \code{wq_param}, \code{date_min}, \code{date_max}, or NULL
#' @param usgs_dv data frame containing columns \code{station_id}, \code{param}, \code{date_min}, \code{date_max}, or NULL
#' @param replace replace tracker if it already exists in database
#'
#' @return TRUE if tracker was successfully added to the database
#' @export
#'
#' @examples
#' \dontrun{
#' tracker_add(con, "test-tracker", "A testing tracker", df_dbkeys, df_wq_stations)
#' }
tracker_add <- function(con, id, description, dbhydro_hydro = NULL, dbhydro_wq = NULL, usgs_dv = NULL, replace = FALSE) {
  logger::log_info("adding tracker {id}")

  if (replace) {
    logger::log_debug("removing existing tracker (if exists)")
    tracker_remove(con, id)
  }

  if (!is.null(dbhydro_hydro)) {
    dbhydro_dbkeys <- unique(dbhydro_hydro[["dbkey"]])
    logger::log_debug("adding {length(dbhydro_dbkeys)} dbhydro_dbkeys for tracker ({id})")
    db_add_dbhydro_dbkeys(con, dbkeys = dbhydro_dbkeys)
  }

  if (!is.null(dbhydro_wq)) {
    if (any(is.na(dbhydro_wq[["wq_param"]]))) {
      logger::log_error("dbhydro_wq tibble cannot contain NA's in wq_param column")
      return(FALSE)
    }
    if (!all(dbhydro_wq[["wq_param"]] %in% dbhydro_wq_params[["wq_param"]])) {
      unknown_wq_params <- setdiff(unique(dbhydro_wq[["wq_param"]]), dbhydro_wq_params[["wq_param"]])
      logger::log_error("dbhydro_wq tibble contains unknown wq_param values ({paste0(unknown_wq_params, collapse = ', ')})")
      return(FALSE)
    }
    dbhydro_station_ids <- unique(dbhydro_wq[["station_id"]])
    logger::log_debug("adding {length(dbhydro_station_ids)} dbhydro wq stations for tracker ({id})")
    db_add_dbhydro_stations(con, station_ids = dbhydro_station_ids)
  }

  if (!is.null(usgs_dv)) {
    if (any(is.na(usgs_dv[["param"]]))) {
      logger::log_error("usgs_dv tibble cannot contain NA's in param column")
      return(FALSE)
    }
    if (!all(usgs_dv[["param"]] %in% usgs_params[["param"]])) {
      unknown_params <- setdiff(unique(usgs_dv[["param"]]), usgs_params[["param"]])
      logger::log_error("usgs_dv tibble contains unknown param values ({paste0(unknown_params, collapse = ', ')})")
      return(FALSE)
    }
    usgs_station_ids <- unique(usgs_dv[["station_id"]])
    logger::log_debug("adding {length(usgs_station_ids)} usgs stations for tracker ({id})")
    db_add_usgs_stations(con, station_ids = usgs_station_ids)
  }

  logger::log_debug("inserting tracker into trackers table")
  df_tracker <- tibble::tibble(
    id = id,
    description = description
  )
  DBI::dbWriteTable(con, "trackers", df_tracker, append = TRUE, row.names = FALSE)

  if (!is.null(dbhydro_hydro)) {
    logger::log_debug("inserting dbkeys into trackers_dbhydro_hydro table")
    df_tracker_dbhydro_hydro <- tibble::tibble(
      tracker_id = id,
      dbkey = dbhydro_hydro[["dbkey"]],
      date_min = dbhydro_hydro[["date_min"]],
      date_max = dbhydro_hydro[["date_max"]]
    )
    DBI::dbWriteTable(con, "trackers_dbhydro_hydro", df_tracker_dbhydro_hydro, append = TRUE, row.names = FALSE)
  }

  if (!is.null(dbhydro_wq)) {
    logger::log_debug("inserting stations into trackers_dbhydro_wq table")
    df_tracker_dbhydro_wq <- tibble::tibble(
      tracker_id = id,
      station_id = dbhydro_wq[["station_id"]],
      wq_param = dbhydro_wq[["wq_param"]],
      date_min = dbhydro_wq[["date_min"]],
      date_max = dbhydro_wq[["date_max"]]
    )
    DBI::dbWriteTable(con, "trackers_dbhydro_wq", df_tracker_dbhydro_wq, append = TRUE, row.names = FALSE)
  }

  if (!is.null(usgs_dv)) {
    logger::log_debug("inserting stations into trackers_usgs_dv table")
    df_tracker_usgs_dv <- tibble::tibble(
      tracker_id = id,
      station_id = usgs_dv[["station_id"]],
      param = usgs_dv[["param"]],
      date_min = usgs_dv[["date_min"]],
      date_max = usgs_dv[["date_max"]]
    )
    DBI::dbWriteTable(con, "trackers_usgs_dv", df_tracker_usgs_dv, append = TRUE, row.names = FALSE)
  }

  TRUE
}
