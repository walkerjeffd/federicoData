#' Get hydrologic data from DBHYDRO
#'
#' Get hydrologic data from DBHYDRO for specified DBKEY(s) and date range
#'
#' @param dbkeys character vector of DBKEY(s)
#' @param date_min start date
#' @param date_max end date
#'
#' @return tibble containing raw data, or empty tibble (no columns) if no data found
#' @export
#'
#' @examples
#' \dontrun{
#' dbhydro_get_hydro(dbkeys = "91599", date_min = "2019-10-01", date_max = "2019-10-31")
#' }
#' @importFrom dplyr %>%
dbhydro_get_hydro <- function (dbkeys, date_min, date_max) {
  logger::log_debug("fetching hydro data from dbhydro for {length(dbkeys)} dbkey(s) from {date_min} to {date_max}")

  df_raw <- tryCatch(
    dbhydroR::get_hydro(
      dbkey = dbkeys,
      date_min = as.character(date_min),
      date_max = as.character(date_max),
      raw = TRUE
    ),
    error = function(c) {
      logger::log_warn("no data found in dbhydro, returning empty tibble (dbhydroR: {c$message})")
      tibble::tibble()
    }
  )

  # remove duplicate station columns
  if (sum(names(df_raw) == "station") == 2) {
    df <- df_raw[, -which(names(df_raw) == "station")]
  } else {
    df <- df_raw
  }

  df <- tibble::as_tibble(df) %>%
    janitor::clean_names()

  logger::log_debug("received {nrow(df)} record(s) from dbhydro")
  df
}
