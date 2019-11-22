#' Get water quality data from dbhydro in batches
#'
#' Get water quality data from dbhydro for specified station(s) and date range
#' in batches of specified size
#'
#' @param station_ids character vector containing one or more station_id's
#' @param date_min start date
#' @param date_max end date
#' @param test_name test name
#' @param batch_size number of stations for each batch
#'
#' @return tibble containing raw data, or empty tibble (no columns) if no data found
#' @export
#'
#' @examples
#' dbhydro_batch_get_wq(
#'   station_ids = c("LOX3", "LOX4", "LOX5"),
#'   date_min = "2019-09-01",
#'   date_max = "2019-10-31",
#'   test_name = "PHOSPHATE, TOTAL AS P",
#'   batch_size = 2
#' )
dbhydro_batch_get_wq <- function (station_ids, date_min, date_max, test_name = "PHOSPHATE, TOTAL AS P", batch_size = 5) {
  logger::log_info("fetching wq data from dbhydro for {length(station_ids)} station(s) from {date_min} to {date_max} for test {test_name} in batches of size {batch_size}")

  station_batches <- base::split(station_ids, base::ceiling(base::seq_along(station_ids) / batch_size))
  logger::log_debug("stations split into {length(station_batches)} batch(es)")

  df <- purrr::map_df(station_batches, function(x) {
    dbhydro_get_wq(
      station_ids = x,
      date_min = date_min,
      date_max = date_max,
      test_name = test_name
    )
  })

  logger::log_debug("received {nrow(df)} record(s) from dbhydro for all batches")
  df
}
