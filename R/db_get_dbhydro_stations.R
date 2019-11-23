#' Get DBHYDRO stations from database
#'
#' @param con database connection
#' @param station_ids character vector of station_id(s) to retrieve, or NULL to retrieve all (default)
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_dbhydro_stations(con) # get all stations
#' db_get_dbhydro_stations(con, station_ids = c("LOX3", "LOX6"))
#' }
db_get_dbhydro_stations <- function(con, station_ids = NULL) {
  if (is.null(station_ids) || length(station_ids) == 0) {
    sql <- "SELECT * FROM dbhydro_stations"
    station_ids <- c()
  } else {
    sql <- glue::glue_sql("SELECT * FROM dbhydro_stations WHERE station_id IN ({ids*})", ids = station_ids, .con = con)
  }

  df <- DBI::dbGetQuery(con, sql)

  if (length(station_ids) > 0 && nrow(df) != length(station_ids)) {
    logger::log_warn("database did not return all requested stations (missing: {paste0(setdiff(station_ids, df$station_id), collapse = ',')})")
  }

  df
}
