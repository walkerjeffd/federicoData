#' Fetch DBHYDRO hydrologic data from database
#'
#' @param con database connection object
#' @param dbkeys character vector of dbkeys
#' @param date_min start date (optional)
#' @param date_max end date (optional)
#'
#' @return tibble containing hydrologic data
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_dbhydro_hydro(con, dbkeys = c("91473", "91599"))
#' db_get_dbhydro_hydro(
#'   con,
#'   dbkeys = c("91473", "91599"),
#'   date_min = "2019-10-01",
#'   date_max = "2019-11-30"
#' ) # specific date range
#' }
db_get_dbhydro_hydro <- function(con, dbkeys, date_min = NULL, date_max = NULL) {
  if (any(is.na(dbkeys))) {
    logger::log_error("dbkeys cannot contain NA values")
  }

  logger::log_info("fetching dbhydro hydrologic data for {length(dbkeys)} dbkeys ({ifelse(is.null(date_min), 'N/A', date_min)} to {ifelse(is.null(date_max), 'N/A', date_max)}) from database")

  sql_where <- ""
  if (!is.null(date_min) || !is.null(date_max)) {
    sql_date <- c()
    if (!is.null(date_min)) {
      sql_date <- c(sql_date, glue::glue_sql("date >= {date_min}", .con = con))
    }
    if (!is.null(date_max)) {
      sql_date <- c(sql_date, glue::glue_sql("date <= {date_max}", .con = con))
    }
    if (length(sql_date) == 1) {
      sql_where <- glue::glue("WHERE {sql_date}")
    } else {
      sql_where <- glue::glue("WHERE {sql_date[1]} AND {sql_date[2]}")
    }
  }

  df <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT * FROM dbhydro_hydro {sql_where} ORDER BY dbkey, date",
      .con = con
    )
  )

  tibble::as_tibble(df)
}
