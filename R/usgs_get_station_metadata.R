#' Get station metadata from USGS NWIS
#'
#' @param station_ids character vector of station IDs
#' @param raw return raw response from dataRetrieval::readNWISsite() if TRUE, otherwise returns tibble with subset of columns
#'
#' @return tibble with columns \code{station_id}, \code{station_name}, \code{latitude}, \code{longitude} (if \code{raw=FALSE})
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' usgs_get_station_metadata(station_ids = "263180080205001")
#' }
usgs_get_station_metadata <- function (station_ids, raw = FALSE) {
  logger::log_debug("fetching usgs station metadata for ({length(station_ids)}) station(s)")

  df <- dataRetrieval::readNWISsite(siteNumbers = station_ids)

  missing_stations <- setdiff(station_ids, df$site_no)

  if (length(missing_stations) > 0) {
    logger::log_warn("usgs did not return all stations, missing {length(missing_stations)} ({paste0(missing_stations, collapse = ', ')})")
    stop("failed to get metadata for all usgs stations")
  }

  if (raw) {
    return(df)
  }

  df <- tibble::as_tibble(df) %>%
    dplyr::select(
      station_id = .data$site_no,
      station_name = .data$station_nm,
      latitude = .data$dec_lat_va,
      longitude = .data$dec_long_va
    )

  df
}
