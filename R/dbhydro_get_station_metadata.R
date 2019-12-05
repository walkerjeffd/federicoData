#' Get water quality station metadata from DBHYDRO
#'
#' @param station_ids character vector of station_id(s)
#' @param query list of additional query parameters to pass with request
#' @param batch_size number of stations per batch (API limit)
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
dbhydro_get_station_metadata <- function (station_ids, query = list(), batch_size = 10) {
  logger::log_info("fetching metadata for {length(station_ids)} station(s)")

  if (any(is.na(station_ids))) {
    logger::log_error("station_ids cannot contain NAs, stopping")
    stop("station_ids contains NA values")
  }
  if (any(duplicated(station_ids))) {
    logger::log_error("station_ids cannot contain duplicates, stopping")
    stop("station_ids contains duplicates")
  }
  if (any(station_ids == "")) {
    logger::log_warn("station_ids cannot contain an empty string, stopping")
    stop("station_ids contains empty string")
  }
  if (length(station_ids) == 0) {
    logger::log_warn("station_ids is empty, returning empty tibble (no columns)")
    return(tibble::tibble())
  }

  # first only first 100 (dbhydro limit)
  batch_station_ids <- station_ids[1:min(length(station_ids), batch_size)]

  logger::log_debug("fetching metadata for {length(batch_station_ids)} stations of {length(station_ids)} total")

  fetch_query <- c(query, list(v_station = stringr::str_c(batch_station_ids, collapse = "/")))
  response <- httr::GET(
    "https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_station_info",
    query = fetch_query
  )

  content <- httr::content(response, "parsed")
  tables <- rvest::html_nodes(content, xpath = ".//table/.//table/.//table")

  if (length(tables) == 1) {
    if (length(rvest::html_children(rvest::html_node(content, xpath = ".//table/.//table/.//table"))) == 0) {
      logger::log_error("no results returned from dbhydro, stopping")
      stop("failed to get station metadata")
    }
    logger::log_debug("parsing single station metadata table")
    df <- dbhydro_parse_station_metadata_single(content)
  } else {
    logger::log_debug("parsing multiple station metadata table")
    df <- dbhydro_parse_station_metadata_multiple(content)
  }

  if (length(batch_station_ids) != nrow(df)) {
    logger::log_error("failed to get metadata for all stations, stopping")
    stop("failed to get station metadata")
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

dbhydro_parse_station_metadata_single <- function(content) {
  tbl_node <- rvest::html_node(content, xpath = ".//table/.//table/.//table")
  tbl <- rvest::html_table(tbl_node, fill = TRUE)
  tbl[-1, ] %>%
    tidyr::pivot_wider(names_from = "X1", values_from = "X2") %>%
    janitor::clean_names() %>%
    dplyr::select(
      station_id = .data$station,
      site = .data$site,
      type = .data$type,
      latitude = .data$latitude_ddmmss_sss,
      longitude = .data$longitude_ddmmss_sss,
      x_coord = .data$x_coord_ft_nad83,
      y_coord = .data$y_coord_ft_nad83,
      county = .data$county,
      basin = .data$basin,
      sec = .data$section,
      twp = .data$township,
      rng = .data$range,
      description = .data$description
    ) %>%
    dplyr::mutate_at(c("latitude", "longitude", "x_coord", "y_coord"), as.numeric) %>%
    dplyr::mutate_at(c("sec", "twp", "rng"), as.integer) %>%
    dplyr::mutate(
      latitude = dms_to_ddeg(.data$latitude),
      longitude = -dms_to_ddeg(.data$longitude)
    )
}

dbhydro_parse_station_metadata_multiple <- function(content) {
  tbl_node <- rvest::html_node(content, xpath = ".//table[2]")
  tbl <- rvest::html_table(tbl_node)
  tbl %>%
    janitor::clean_names() %>%
    tibble::as_tibble() %>%
    dplyr::select(-c("get_data", "show_map", "nearby_stations", "attachments")) %>%
    dplyr::rename(
      station_id = .data$station,
      latitude = .data$latitude_ddmmss_sss,
      longitude = .data$longitude_ddmmss_sss,
      x_coord = .data$x_coord_ft,
      y_coord = .data$y_coord_ft
    ) %>%
    dplyr::mutate(
      latitude = dms_to_ddeg(.data$latitude),
      longitude = -dms_to_ddeg(.data$longitude)
    )
}
