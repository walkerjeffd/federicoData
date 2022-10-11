#' Get water quality data from DBHYDRO
#'
#' Get water quality data from DBHYDRO for specified station(s) and date range
#'
#' @param station_ids character vector containing one or more station_id's
#' @param wq_param water quality parameter (must be listed in \code{dbhydro_wq_params})
#' @param date_min start date
#' @param date_max end date
#' @param raw if TRUE, return raw results from DBHYDRO, otherwise pass results through \code{dbhydro_clean_wq()} before returning (default)
#'
#' @return tibble containing raw data, or empty tibble (no columns) if no data found
#' @export
#'
#' @examples
#' \dontrun{
#' dbhydro_get_wq(
#'   station_ids = "LOX3",
#'   wq_param = "TP",
#'   date_min = "2019-09-01",
#'   date_max = "2019-10-31"
#' )
#' }
dbhydro_get_wq <- function (station_ids, wq_param, date_min, date_max, raw = FALSE) {
  logger::log_debug("fetching wq data from dbhydro for {length(station_ids)} station(s) from {date_min} to {date_max} for {wq_param}")

  test_name <- dbhydro_wq_params$test_name[which(dbhydro_wq_params$wq_param == wq_param)]

  if (length(test_name) == 0) {
    logger::log_error("unknown value for wq_param ({wq_param}), must be found in dbhydro_wq_params$wq_param")
    stop("unknown wq parameter")
  }

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

  df <- tibble::as_tibble(df_raw) %>%
    janitor::clean_names()

  logger::log_debug("received {nrow(df)} record(s) from dbhydro")

  if (!raw && nrow(df) > 0) {
    df <- dbhydro_clean_wq(df)
  }

  df
}
