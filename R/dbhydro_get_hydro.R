#' Get hydrologic data from DBHYDRO
#'
#' Get hydrologic data from DBHYDRO for specified DBKEY(s) and date range
#'
#' @param dbkeys character vector of DBKEY(s)
#' @param date_min start date
#' @param date_max end date
#' @param raw if TRUE, return raw results from DBHYDRO, otherwise pass results through \code{dbhydro_clean_hydro()} before returning (default)
#'
#' @return tibble containing raw data, or NULL if no data found
#' @export
#'
#' @examples
#' \dontrun{
#' dbhydro_get_hydro(dbkeys = "91599", date_min = "2019-10-01", date_max = "2019-10-31")
#' }
dbhydro_get_hydro <- function (dbkeys, date_min, date_max, raw = FALSE) {
  logger::log_debug("fetching hydro data from dbhydro for {length(dbkeys)} dbkey(s) from {date_min} to {date_max}")

  stopifnot(
    all(!is.na(dbkeys)),
    all(!duplicated(dbkeys))
  )

  df_raw <- dbhydroInsights::get_timeseries_data(
    dbkey = dbkeys,
    startDate = as.character(date_min),
    endDate = as.character(date_max)
  )

  if (is.null(df_raw) || nrow(df_raw) == 0) {
    return(NULL)
  }

  df <- tibble::as_tibble(df_raw) |>
    janitor::clean_names()

  logger::log_debug("received {nrow(df)} record(s) from dbhydro")

  if (!raw && nrow(df) > 0) {
    df <- dbhydro_clean_hydro(df)
  }

  df
}
