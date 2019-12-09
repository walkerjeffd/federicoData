#' Fetch DBHYDRO water quality data from database
#'
#' @param con database connection object
#' @param station_ids character vector of dbkeys
#' @param wq_params character vector of water quality parameters (optional)
#' @param date_min start date (optional)
#' @param date_max end date (optional)
#'
#' @return tibble containing water quality data
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_dbhydro_wq(con, station_ids = c("LOX3", "LOX6"))
#' db_get_dbhydro_wq(
#'   con,
#'   station_ids = c("LOX3", "LOX6"),
#'   wq_params = "TP",
#'   date_min = "2019-10-01",
#'   date_max = "2019-11-30"
#' ) # specific date range
#' }
db_get_dbhydro_wq <- function(con, station_ids, wq_params = NULL, date_min = NULL, date_max = NULL) {
  if (any(is.na(station_ids))) {
    logger::log_error("station_ids cannot contain NA values")
  }

  logger::log_info("fetching dbhydro wq data for {length(station_ids)} stations ({date_min} to {date_max}) for params {paste0(wq_params, collapse = ', ')} from database")

  sql_where_date <- NULL
  if (!is.null(date_min) || !is.null(date_max)) {
    sql_date <- c()
    if (!is.null(date_min)) {
      sql_date <- c(sql_date, glue::glue_sql("date >= {date_min}", .con = con))
    }
    if (!is.null(date_max)) {
      sql_date <- c(sql_date, glue::glue_sql("date <= {date_max}", .con = con))
    }
    if (length(sql_date) == 1) {
      sql_where_date <- glue::glue("{sql_date}")
    } else {
      sql_where_date <- glue::glue("{sql_date[1]} AND {sql_date[2]}")
    }
  }

  sql_where_param <- NULL
  if (!is.null(wq_params)) {
    sql_where_param <- glue::glue_sql("wq_param IN ({wq_params*})", .con = con)
  }

  sql_where <- ""
  if (!is.null(sql_where_date) || !is.null(sql_where_param)) {
    sql_where <- c()
    if (!is.null(sql_where_date)) {
      sql_where <- c(sql_where_date)
    }
    if (!is.null(sql_where_param)) {
      sql_where <- c(sql_where, sql_where_param)
    }
    sql_where <- glue::glue("WHERE {paste0(sql_where, collapse = ' AND ')}")
  }
  df <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT * FROM dbhydro_wq {sql_where}",
      .con = con
    )
  )

  tibble::as_tibble(df)
}
