#' Get station metadata from USGS NWIS
#'
#' @param station_ids character vector of station IDs
#' @param raw return raw response from dataRetrieval::read_waterdata_monitoring_location() if TRUE, otherwise returns tibble with subset of columns
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

  df <- dataRetrieval::read_waterdata_monitoring_location(
    monitoring_location_id = paste0("USGS-", station_ids),
    properties = c(
      "monitoring_location_id",
      "monitoring_location_name"
    ),
    skipGeometry = FALSE
  )

  if (raw) {
    return(df)
  }

  df <- df |> 
    dplyr::select(
      station_id = .data$monitoring_location_id,
      station_name = .data$monitoring_location_name
    ) |> 
    dplyr::mutate(
      latitude = sf::st_coordinates(.data$geometry)[, 2],
      longitude = sf::st_coordinates(.data$geometry)[, 1]
    ) |> 
    sf::st_drop_geometry()

  df
}
