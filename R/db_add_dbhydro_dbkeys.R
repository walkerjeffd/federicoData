#' Add DBHYDRO DBKEYs to database
#'
#' Given a set of dbkeys, identifies which dbkey(s) do not
#' exist in the database, then fetches the metadata for those dbkeys
#' from DBHYDRO and inserts the results to the dabatase. Also
#' adds corresponding stations for new dbkeys, if they don't already
#' exist.
#'
#' @param con database connection
#' @param dbkeys character vector of DBKEY(s)
#'
#' @return logical indicating if operation was successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_add_dbhydro_dbkeys(con, c("91346", "91347"))
#' }
db_add_dbhydro_dbkeys <- function (con, dbkeys) {
  logger::log_info("adding {length(dbkeys)} dbkey(s) to database")

  db_stations <- DBI::dbGetQuery(con, "SELECT station_id FROM dbhydro_stations")
  db_dbkeys <- DBI::dbGetQuery(con, "SELECT dbkey FROM dbhydro_dbkeys")

  if (nrow(db_dbkeys) > 0) {
    new_dbkeys <- setdiff(dbkeys, db_dbkeys$dbkey)
  } else {
    new_dbkeys <- dbkeys
  }

  if (length(new_dbkeys) == 0) {
    logger::log_info("no new dbkeys to add")
    return(TRUE)
  }

  dbhydro_dbkeys <- dbhydro_get_dbkey_metadata(dbkeys = new_dbkeys)

  failed_dbhydro_dbkeys <- setdiff(new_dbkeys, dbhydro_dbkeys$dbkey)
  if (length(failed_dbhydro_dbkeys) > 0) {
    logger::log_warn("failed to get metadata for {length(failed_dbhydro_dbkeys)} dbkey(s) ({paste0(failed_dbhydro_dbkeys, collapse = ', ')}), quitting")
    return(FALSE)
  }

  new_station_ids <- setdiff(dbhydro_dbkeys$station_id, db_stations$station_id)
  ok <- db_add_dbhydro_stations(con = con, station_ids = new_station_ids)

  if (!ok) {
    logger::log_warn("failed to insert stations for new dbkeys into database")
    return(FALSE)
  }

  logger::log_debug("inserting {length(new_dbkeys)} new dbkey(s) into database")
  insert_dbkeys <- dplyr::select(
    dbhydro_dbkeys,
    c("dbkey", "station_id", "site_group", "data_type", "freq", "stat", "recorder", "agency", "start_date", "end_date", "strata", "op_num", "struct")
  )
  ok <- DBI::dbWriteTable(con, "dbhydro_dbkeys", insert_dbkeys, append = TRUE, row.names = FALSE)

  if (!ok) {
    logger::log_warn("failed to insert dbkeys into database")
    return(FALSE)
  }

  logger::log_info("success adding {nrow(dbhydro_dbkeys)} new dbkey(s)")

  TRUE
}
