#' Get water quality station metadata from DBHYDRO
#'
#' @param station_ids character vector of station_id(s)
#' @param query list of additional query parameters to pass with request
#' @param batch_size number of stations per batch (API limit)
#'
#' @return tibble of metadata for each station_id
#' @export
#'
#' @examples
#' \dontrun{
#' dbhydro_get_station_metadata(station_ids = c("L3"))
#' dbhydro_get_station_metadata(station_ids = c("L3", "G136", "G342A", "G342B", "G342C", "G342D"))
#' }
dbhydro_get_station_metadata <- function (station_ids, query = list(), batch_size = 10) {
  logger::log_info("fetching metadata from dbhydro for {length(station_ids)} station(s)")

  stopifnot(all(!is.na(station_ids)))
  stopifnot(all(!duplicated(station_ids)))

  if (length(station_ids) == 0) {
    logger::log_warn("dbhydro_get_station_metadata: station_ids is empty, returning empty tibble (no columns)")
    return(tibble::tibble())
  }

  # first only first 100 (dbhydro limit)
  batch_station_ids <- station_ids[1:min(length(station_ids), batch_size)]

  logger::log_debug("dbhydro_get_station_metadata: fetching {length(batch_station_ids)} stations of {length(station_ids)} total")

  fetch_query <- c(query, list(v_station = stringr::str_c(batch_station_ids, collapse = "/")))
  response <- httr::GET(
    "https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_station_info",
    query = fetch_query
  )
  if (length(batch_station_ids) == 1) {
    tbl <- httr::content(response, "parsed") %>%
      rvest::html_node(xpath = ".//table/.//table/.//table") %>%
      rvest::html_table(fill = TRUE)
    df <- tbl[-1, ] %>%
      tidyr::pivot_wider(names_from = "X1", values_from = "X2") %>%
      janitor::clean_names() %>%
      dplyr::select_(
        station_id = ~station,
        site = ~site,
        type = ~type,
        latitude = ~latitude_ddmmss_sss,
        longitude = ~longitude_ddmmss_sss,
        x_coord = ~x_coord_ft_nad83,
        y_coord = ~y_coord_ft_nad83,
        county = ~county,
        basin = ~basin,
        sec = ~section,
        twp = ~township,
        rng = ~range,
        description = ~description
      ) %>%
      dplyr::mutate_at(c("latitude", "longitude", "x_coord", "y_coord"), as.numeric) %>%
      dplyr::mutate_at(c("sec", "twp", "rng"), as.integer) %>%
      dplyr::mutate_(
        latitude = ~dms_to_ddeg(latitude),
        longitude = ~-dms_to_ddeg(longitude)
      )
  } else {
    tbl <- httr::content(response, "parsed") %>%
      rvest::html_node(xpath = ".//table[2]") %>%
      rvest::html_table()
    df <- tbl %>%
      janitor::clean_names() %>%
      tibble::as_tibble() %>%
      dplyr::select_(~-get_data, ~-show_map, ~-nearby_stations, ~-attachments) %>%
      dplyr::rename_(
        station_id = ~station,
        latitude = ~latitude_ddmmss_sss,
        longitude = ~longitude_ddmmss_sss,
        x_coord = ~x_coord_ft,
        y_coord = ~y_coord_ft
      ) %>%
      dplyr::mutate_(
        latitude = ~dms_to_ddeg(latitude),
        longitude = ~-dms_to_ddeg(longitude)
      )
  }

  if (length(station_ids) <= batch_size) {
    return(df)
  }
  Sys.sleep(2)
  dplyr::bind_rows(
    df,
    dbhydro_get_station_metadata(station_ids[(batch_size + 1):length(station_ids)], query = query)
  )
}
