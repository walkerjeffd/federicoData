#' Fetch USGS daily values data from database
#'
#' @param con database connection object
#' @param station_ids character vector of station_ids
#' @param params character vector of parameters
#' @param date_min start date (optional)
#' @param date_max end date (optional)
#'
#' @return tibble containing usgs data
#' @export
#'
#' @examples
#' \dontrun{
#' db_get_usgs_dv(con, station_ids = c("263180080205001", "263000080120001"))
#' db_get_usgs_dv(
#'   con,
#'   station_ids = c("263180080205001", "263000080120001"),
#'   params = c("stage"),
#'   date_min = "2019-10-01",
#'   date_max = "2019-11-30"
#' ) # specific params, date range
#' }
db_get_usgs_dv <- function(con, station_ids, params = NULL, date_min = NULL, date_max = NULL) {
  if (any(is.na(station_ids))) {
    logger::log_error("station_ids cannot contain NA values")
  }

  logger::log_info("fetching usgs dv data for {length(station_ids)} stations ({ifelse(is.null(date_min), 'N/A', date_min)} to {ifelse(is.null(date_max), 'N/A', date_max)}) for params {ifelse(is.null(params), 'ALL', paste0(params, collapse = ', '))} from database")

  sql_where_station <- glue::glue_sql("station_id IN ({station_ids*})", .con = con)

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
  if (!is.null(params)) {
    sql_where_param <- glue::glue_sql("param IN ({params*})", .con = con)
  }

  sql_where <- sql_where_station
  if (!is.null(sql_where_date) || !is.null(sql_where_param)) {
    if (!is.null(sql_where_date)) {
      sql_where <- c(sql_where, sql_where_date)
    }
    if (!is.null(sql_where_param)) {
      sql_where <- c(sql_where, sql_where_param)
    }
  }
  sql_where <- glue::glue("WHERE {paste0(sql_where, collapse = ' AND ')}")

  df <- DBI::dbGetQuery(
    con,
    glue::glue(
      "SELECT * FROM usgs_dv {sql_where} ORDER BY station_id, param, date",
      .con = con
    )
  )

  tibble::as_tibble(df)
}
