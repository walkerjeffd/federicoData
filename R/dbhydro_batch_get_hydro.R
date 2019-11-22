#' Get hydrologic data from DBHYDRO in batches
#'
#' Get hydrologic data from DBHYDRO for specified DBKEY(s) and date range
#' in batches of specified size
#'
#' @param dbkeys character vector of DBKEY(s)
#' @param date_min start date
#' @param date_max end date
#' @param batch_size number of DBKEYs for each batch
#'
#' @return tibble containing raw data, or empty tibble (no columns) if no data found
#' @export
#'
#' @examples
#' \dontrun{
#' dbhydro_batch_get_hydro(
#'   dbkeys = c("91599", "91473", "91663"),
#'   date_min = "2019-10-01",
#'   date_max = "2019-10-31",
#'   batch_size = 2
#' )
#' }
dbhydro_batch_get_hydro <- function (dbkeys, date_min, date_max, batch_size = 5) {
  logger::log_info("fetching hydro data from dbhydro for {length(dbkeys)} dbkey(s) from {date_min} to {date_max} in batches of size {batch_size}")

  dbkey_batches <- base::split(dbkeys, base::ceiling(base::seq_along(dbkeys) / batch_size))
  logger::log_debug("dbkeys split into {length(dbkey_batches)} batch(es)")

  df <- purrr::map_df(dbkey_batches, function(x) {
    dbhydro_get_hydro(
      dbkeys = x,
      date_min = date_min,
      date_max = date_max
    )
  })

  logger::log_debug("received {nrow(df)} record(s) from dbhydro for all batches")
  df
}
