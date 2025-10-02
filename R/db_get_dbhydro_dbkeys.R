#' Get DBHYDRO DBKEYs from database
#'
#' @param con database connection
#' @param dbkeys character vector of DBKEY(s) to retrieve, or NULL to retrieve all (default)
#' @param include_stations include station metadata (join dbhydro_stations table)
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_dbhydro_dbkeys(con) # get all DBKEYs
#' db_get_dbhydro_dbkeys(con, dbkeys = c("15018", "15034"))
#' db_get_dbhydro_dbkeys(
#'   con,
#'   dbkeys = c("15018", "15034"),
#'   include_stations = TRUE
#' ) # include station metadata
#' }
db_get_dbhydro_dbkeys <- function(con, dbkeys = NULL, include_stations = FALSE) {
  dbkeys <- unique(dbkeys)

  sql_dbkeys <- glue::glue_sql("SELECT * FROM dbhydro_dbkeys WHERE dbhydro_dbkeys.dbkey IN ({ids*})", ids = dbkeys, .con = con)

  df_dbkeys <- DBI::dbGetQuery(con, sql_dbkeys)

  if (length(dbkeys) > 0 && nrow(df_dbkeys) != length(dbkeys)) {
    logger::log_warn("database did not return all requested dbkeys (missing: {paste0(setdiff(dbkeys, df_dbkeys$dbkey), collapse = ',')})")
  }

  if (include_stations) {
    sql_stations <- glue::glue_sql(
      "SELECT * FROM dbhydro_stations WHERE station_id IN ({ids*})",
      ids = unique(df_dbkeys$station_id),
      .con = con
    )
    df_stations <- DBI::dbGetQuery(con, sql_stations)

    df <- dplyr::left_join(df_dbkeys, df_stations, by = "station_id")
  } else {
    df <- df_dbkeys
  }

  df
}
