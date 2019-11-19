#' Get water quality data from dbhydro
#'
#' Get water quality data from dbhydro for specified station(s) and date range
#'
#' @param station_ids character vector containing one or more station_id's
#' @param date_min start date
#' @param date_max end date
#' @param test_name test name
#'
#' @return tibble containing raw data, or empty tibble (no columns) if no data found
#' @export
#'
#' @examples
#' dbhydro_get_wq(
#'   station_ids = "LOX3",
#'   date_min = "2019-09-01",
#'   date_max = "2019-10-31",
#'   test_name = "PHOSPHATE, TOTAL AS P"
#' )
dbhydro_get_wq <- function (station_ids, date_min, date_max, test_name = "PHOSPHATE, TOTAL AS P") {
  logger::log_debug("fetching wq data from dbhydro for {length(station_ids)} station(s) from {date_min} to {date_max} for test {test_name}")

  df_raw <- tryCatch(
    dbhydroR::get_wq(
      station_id = station_ids,
      date_min = as.character(date_min),
      date_max = as.character(date_max),
      test_name = test_name,
      raw = TRUE
    ),
    error = function(c) {
      logger::log_warn("no data found in dbhydro, returning empty tibble (dbhydroR: {c$message})")
      tibble::tibble()
    }
  )

  df <- tibble::as_tibble(df_raw)

  if (nrow(df) > 0) {
    # clean columns
    df <- janitor::clean_names(df)
    df$date <- lubridate::as_date(df$date)
    df$datetime <- lubridate::dmy_hm(df$collection_date, tz = "US/Eastern")
    df$measure_date <- lubridate::dmy_hm(df$measure_date, tz = "US/Eastern")
    df$receive_date <- lubridate::dmy_hm(df$receive_date, tz = "US/Eastern")
    df <- df[, -which(names(df) %in% c("collection_date"))]
  }

  logger::log_debug("received {nrow(df)} record(s) from dbhydro")
  df
}
