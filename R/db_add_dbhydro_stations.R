#' Add DBHYDRO stations to database
#'
#' Given a set of station_ids, identifies which station_ids do not
#' exist in the database, then fetches the metadata for those stations
#' from DBHYDRO and inserts the results to the dabatase.
#'
#' @param con database connection
#' @param station_ids character vector of station_id(s)
#'
#' @return logical indicating if operation was successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_add_dbhydro_stations(con, c("LOX2", "LOX3"))
#' }
db_add_dbhydro_stations <- function (con, station_ids) {
  logger::log_info("adding {length(station_ids)} station(s) to database")

  db_stations <- DBI::dbGetQuery(con, "SELECT station_id FROM dbhydro_stations")

  if (nrow(db_stations) > 0) {
    new_station_ids <- setdiff(station_ids, db_stations$station_id)
  } else {
    new_station_ids <- station_ids
  }

  if (length(new_station_ids) == 0) {
    logger::log_info("no new stations to add")
    return(TRUE)
  }

  dbhydro_stations <- dbhydro_get_station_metadata(station_ids = new_station_ids)

  failed_dbhydro_station_ids <- setdiff(new_station_ids, dbhydro_stations$station_id)
  if (length(failed_dbhydro_station_ids) > 0) {
    logger::log_warn("failed to get metadata for {length(failed_dbhydro_station_ids)} station(s) ({paste0(failed_dbhydro_station_ids, collapse = ', ')}), quitting")
    return(FALSE)
  }

  logger::log_debug("inserting {length(new_station_ids)} new station(s) into database")
  ok <- DBI::dbWriteTable(con, "dbhydro_stations", dbhydro_stations, append = TRUE, row.names = FALSE)

  if (!ok) {
    logger::log_warn("failed to insert stations into database")
    return(FALSE)
  }

  logger::log_info("success adding {nrow(dbhydro_stations)} new station(s)")

  TRUE
}
