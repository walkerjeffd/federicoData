#' Get USGS stations from database
#'
#' @param con database connection
#' @param station_ids character vector of station_id(s) to retrieve, or NULL to retrieve all (default)
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_usgs_stations(con) # get all stations
#' db_get_usgs_stations(
#'   con,
#'   station_ids = c("263180080205001", "263000080120001")
#' ) # specific stations
#' }
db_get_usgs_stations <- function(con, station_ids = NULL) {
  station_ids <- unique(station_ids)
  
  if (is.null(station_ids) || length(station_ids) == 0) {
    sql <- "SELECT * FROM usgs_stations"
    station_ids <- c()
  } else {
    sql <- glue::glue_sql("SELECT * FROM usgs_stations WHERE station_id IN ({ids*})", ids = station_ids, .con = con)
  }

  df <- DBI::dbGetQuery(con, sql)

  if (length(station_ids) > 0 && nrow(df) != length(station_ids)) {
    logger::log_warn("database did not return all requested stations (missing: {paste0(setdiff(station_ids, df$station_id), collapse = ',')})")
  }

  df
}
