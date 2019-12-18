#' Get USGS daily values data in batches
#'
#'
#' @param station_ids character vector containing one or more station IDs
#' @param param parameter (must be listed in \code{usgs_params})
#' @param date_min start date
#' @param date_max end date
#' @param batch_size number of stations for each batch
#' @param raw if TRUE, return raw results from USGS, otherwise pass results through \code{usgs_clean_dv()} before returning (default)
#'
#' @return tibble containing raw/cleaned data, or empty tibble (no columns) if no data found
#' @export
#'
#' @examples
#' \dontrun{
#' usgs_batch_get_dv(
#'   station_ids = c("263180080205001", "263000080120001"),
#'   param = "stage",
#'   date_min = "2018-09-01",
#'   date_max = "2018-10-31",
#'   batch_size = 1
#' )
#' }
usgs_batch_get_dv <- function (station_ids, param, date_min, date_max, batch_size = 5, raw = FALSE) {
  logger::log_info("fetching usgs dv data for {length(station_ids)} station(s) from {date_min} to {date_max} for {param} in batches of size {batch_size}")

  station_batches <- base::split(station_ids, base::ceiling(base::seq_along(station_ids) / batch_size))
  logger::log_debug("stations split into {length(station_batches)} batch(es)")

  df <- purrr::map_df(station_batches, function(x) {
    y <- usgs_get_dv(
      station_ids = x,
      param = param,
      date_min = date_min,
      date_max = date_max,
      raw = raw
    )
    Sys.sleep(5)
    y
  })

  logger::log_debug("received {nrow(df)} record(s) from usgs for all batches")
  df
}
