#' Get water quality station metadata from DBHYDRO
#'
#' @param station_ids character vector of station_id(s)
#'
#' @return tibble of metadata for each station_id
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' dbhydro_get_station_metadata(station_ids = c("L3"))
#' dbhydro_get_station_metadata(station_ids = c("L3", "G136", "G342A", "G342B", "G342C", "G342D"))
#' }
dbhydro_get_station_metadata <- function (station_ids) {
  logger::log_info("fetching metadata for {length(station_ids)} station(s)")

  stopifnot(
    all(!is.na(station_ids)),
    all(!duplicated(station_ids))
  )

  x <- dplyr::bind_rows(lapply(station_ids, dbhydroInsights::get_station_metadata))

  if (nrow(x) < length(station_ids)) {
    missing <- setdiff(station_ids, unique(x$station))
    logger::log_warn("metadata not found for {length(missing)} station_id(s): {paste0(missing, collapse = ',')}")
  }
  
  dplyr::select(
    x,
    "station_id" = .data$station,
    "site" = .data$site,
    "type" = .data$type,
    "latitude" = .data$latitude_dec,
    "longitude" = .data$longitude_dec,
    "x_coord" = .data$xcoord,
    "y_coord" = .data$ycoord,
    "county" = .data$county,
    "basin" = .data$basin,
    "sec" = .data$section,
    "twp" = .data$township,
    "rng" = .data$range,
    "description" = .data$station_type_desc
  )
}
