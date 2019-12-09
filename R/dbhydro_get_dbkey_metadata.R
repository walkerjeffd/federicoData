#' Get DBKEY metadata from DBHYDRO
#'
#' @param dbkeys character vector of DBKEYs
#' @param query list containing additional query parameters to pass with request
#' @param batch_size number of DBKEYs per batch (API limit)
#'
#' @return tibble containing metadata for specified DBKEYs
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' dbhydro_get_dbkey_metadata(dbkeys = c("KQ646"))
#' dbhydro_get_dbkey_metadata(dbkeys = c("KQ646", "HD889"))
#' }
#' @importFrom dplyr %>%
dbhydro_get_dbkey_metadata <- function (dbkeys, query = list(), batch_size = 100) {
  logger::log_info("fetching metadata for {length(dbkeys)} dbkey(s)")

  stopifnot(all(!is.na(dbkeys)))
  stopifnot(all(!duplicated(dbkeys)))

  batch_dbkeys <- dbkeys[1:min(length(dbkeys), batch_size)]

  logger::log_debug("fetching metadata for {length(batch_dbkeys)} dbkeys of {length(dbkeys)} total")

  fetch_query <- c(query, list(v_dbkey = stringr::str_c(batch_dbkeys, collapse = "/")))
  response <- httr::GET(
    "https://my.sfwmd.gov/dbhydroplsql/show_dbkey_info.show_dbkeys_matched",
    query = fetch_query
  )
  content <- httr::content(response, "parsed")
  nodes <- rvest::html_node(content, xpath = ".//form[@name='dbkeyList']/table")
  tbl <- rvest::html_table(nodes) %>%
    janitor::clean_names()
  df <- tbl %>%
    tibble::as_tibble() %>%
    dplyr::rename(
      station_id = .data$station,
      site_group = .data$group
    ) %>%
    dplyr::select(-c("get_data")) %>%
    dplyr::mutate(
      start_date = lubridate::dmy(.data$start_date),
      end_date = lubridate::dmy(.data$end_date),
      latitude = dms_to_ddeg(.data$latitude),
      longitude = -dms_to_ddeg(.data$longitude)
    )

  if (is.integer(df$dbkey)) {
    # if dbkeys contained only numeric dbkeys, then rvest::html_table() might coerce the dbkey column to type integer
    # which could remove padding zeros. So need to convert to character here.
    df$dbkey <- sprintf("%05d", df$dbkey)
  }

  if (length(dbkeys) <= batch_size) {
    return(df)
  }

  Sys.sleep(2)
  dplyr::bind_rows(
    df,
    dbhydro_get_dbkey_metadata(dbkeys[(batch_size + 1):length(dbkeys)], query = query)
  )
}
