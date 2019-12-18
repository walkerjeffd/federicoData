#' Update USGS daily values datasets for trackers
#'
#' @param con database connection object
#' @param ids character vector of tracker IDs, or NULL to update all trackers
#' @param date_min start date for period to update, or NULL to use dbkey start date
#' @param date_max end date for period to update, or NULL to use dbkey end date
#' @param batch_size batch size passed to \code{dbhydro_batch_get_wq()} (# stations per batch)
#'
#' @return TRUE if operation was successful
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' tracker_update_usgs_dv(
#'   con,
#'   ids = "test-tracker",
#'   date_min = "2019-01-01",
#'   date_max = "2019-12-01"
#' )
#' }
tracker_update_usgs_dv <- function (con, ids = NULL, date_min = NULL, date_max = NULL, batch_size = 5) {
  logger::log_info("updating usgs dv data for trackers ({ifelse(is.null(ids), 'ALL', paste0(ids, collapse = ', '))}) with period ({ifelse(is.null(date_min), 'N/A', date_min)}, {ifelse(is.null(date_max), 'N/A', date_max)})")

  df_trackers <- tracker_get(con, ids = ids) %>%
    dplyr::select(c("id", "usgs_dv")) %>%
    tidyr::unnest(.data$usgs_dv)

  if (nrow(df_trackers) == 0) {
    logger::log_warn("no usgs dv stations found for trackers, doing nothing")
    return(TRUE)
  }

  df_trackers <- df_trackers %>%
    dplyr::select(c("id", "station_id", "param", "date_min", "date_max")) %>%
    dplyr::filter(!is.na(.data$station_id)) %>%
    dplyr::mutate(
      date_max = dplyr::coalesce(.data$date_max, lubridate::today(tzone = "US/Eastern"))
    )

  if (!is.null(date_min)) {
    logger::log_debug("setting start date ({date_min})")
    date_min <- as.Date(date_min)
    df_trackers <- df_trackers %>%
      dplyr::filter(!(.data$date_max < !!date_min)) %>%
      dplyr::mutate(
        date_min = dplyr::if_else(.data$date_min > !!date_min, .data$date_min, !!date_min)
      )
  }

  if (!is.null(date_max)) {
    logger::log_debug("setting end date ({date_max})")
    date_max <- as.Date(date_max)
    df_trackers <- df_trackers %>%
      dplyr::filter(!(.data$date_min > !!date_max)) %>%
      dplyr::mutate(
        date_max = dplyr::if_else(.data$date_max < !!date_max, .data$date_max, !!date_max)
      )
  }

  stopifnot(all(!is.na(df_trackers)))

  df_stations <- df_trackers %>%
    dplyr::select(c("station_id", "param", "date_min", "date_max")) %>%
    dplyr::group_by(.data$station_id, .data$param) %>%
    dplyr::summarise(
      date_min = min(.data$date_min),
      date_max = min(.data$date_max)
    ) %>%
    dplyr::ungroup()

  df_periods <- df_stations %>%
    tidyr::nest(station_ids = -c("param", "date_min", "date_max")) %>%
    dplyr::mutate(
      station_ids = purrr::flatten(.data$station_ids)
    )

  df_fetch <- df_periods %>%
    dplyr::mutate(
      data = purrr::pmap(
        list(.data$station_ids, .data$param, .data$date_min, .data$date_max),
        function (station_ids, param, date_min, date_max) {
          usgs_batch_get_dv(
            station_ids = station_ids,
            param = param,
            date_min = date_min,
            date_max = date_max,
            batch_size = batch_size,
            raw = FALSE
          )
        })
    )

  df_data <- df_fetch %>%
    dplyr::select(.data$data) %>%
    tidyr::unnest(.data$data)

  logger::log_debug("received {nrow(df_data)} total records")

  stopifnot(db_update_usgs_dv(con, df_data))

  TRUE
}
