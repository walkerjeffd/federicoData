#' Get DBHYDRO DBKEYs from database
#'
#' @param con database connection
#' @param dbkeys character vector of DBKEY(s) to retrieve, or NULL to retrieve all (default)
#' @param include_station include station
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_dbhydro_dbkeys(con) # get all DBKEYs
#' db_get_dbhydro_dbkeys(con, dbkeys = c("91346", "91347"))
#' db_get_dbhydro_dbkeys(con, dbkeys = c("91346", "91347"), include_stations = TRUE) # include station metadata
#' }
db_get_dbhydro_dbkeys <- function(con, dbkeys = NULL, include_stations = FALSE) {
  sql_where <- ""
  sql_join <- ""

  if (!is.null(dbkeys) && length(dbkeys) > 0) {
    sql_where <- glue::glue_sql("WHERE dbhydro_dbkeys.dbkey IN ({ids*})", ids = dbkeys, .con = con)
  }

  if (include_stations) {
    sql_join <- "LEFT JOIN dbhydro_stations ON dbhydro_dbkeys.station_id = dbhydro_stations.station_id"
  }

  sql <- glue::glue("SELECT * FROM dbhydro_dbkeys {sql_join} {sql_where}")

  df <- DBI::dbGetQuery(con, sql)

  if (length(dbkeys) > 0 && nrow(df) != length(dbkeys)) {
    logger::log_warn("database did not return all requested dbkeys (missing: {paste0(setdiff(dbkeys, df$dbkey), collapse = ',')})")
  }

  if (nrow(df) > 0 && include_stations) {
    df <- df[, -which(names(df) == "station_id")[2]]
  }

  df
}
