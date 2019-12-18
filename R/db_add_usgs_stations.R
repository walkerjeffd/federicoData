#' Add USGS stations to database
#'
#' Given a set of station_ids, identifies which station_ids do not
#' exist in the database, then fetches the metadata for those stations
#' from USGS NWIS and inserts the results to the dabatase.
#'
#' @param con database connection
#' @param station_ids character vector of station_id(s)
#'
#' @return logical indicating if operation was successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_add_usgs_stations(con, c("263180080205001", "263000080120001"))
#' }
db_add_usgs_stations <- function (con, station_ids) {
  logger::log_info("adding {length(station_ids)} usgs station(s) to database")

  db_stations <- DBI::dbGetQuery(con, "SELECT station_id FROM usgs_stations")

  if (nrow(db_stations) > 0) {
    new_station_ids <- setdiff(station_ids, db_stations$station_id)
  } else {
    new_station_ids <- station_ids
  }

  if (length(new_station_ids) == 0) {
    logger::log_info("no new stations to add")
    return(TRUE)
  }

  usgs_stations <- usgs_get_station_metadata(station_ids = new_station_ids)

  failed_usgs_station_ids <- setdiff(new_station_ids, usgs_stations$station_id)
  if (length(failed_usgs_station_ids) > 0) {
    logger::log_warn("failed to get metadata for {length(failed_usgs_station_ids)} usgs station(s) ({paste0(failed_usgs_station_ids, collapse = ', ')}), quitting")
    return(FALSE)
  }

  logger::log_debug("inserting {length(new_station_ids)} new station(s) into database")
  ok <- DBI::dbWriteTable(con, "usgs_stations", usgs_stations, append = TRUE, row.names = FALSE)

  if (!ok) {
    logger::log_warn("failed to insert usgs stations into database")
    return(FALSE)
  }

  logger::log_info("success adding {nrow(usgs_stations)} new usgs station(s)")

  TRUE
}
