#' Fetch trackers from database
#'
#' @param con database object
#' @param ids character vector of tracker IDs, or NULL to fetch all trackers
#'
#' @return tibble containing trackers
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' tracker_get(con) # all trackers
#' tracker_get(con, ids = c("tracker-1", "tracker-2")) # specific trackers
#' }
tracker_get <- function (con, ids = NULL) {
  logger::log_debug("fetching trackers from database ({ifelse(is.null(ids), 'ALL', paste0(ids, collapse = ', '))})")

  if (is.null(ids)) {
    df_trackers <- DBI::dbGetQuery(con, "SELECT * FROM trackers")
  } else {
    df_trackers <- DBI::dbGetQuery(con, glue::glue_sql("SELECT * FROM trackers WHERE id IN ({ids*})", .con = con))
  }

  df_trackers <- tibble::as_tibble(df_trackers)

  if (nrow(df_trackers) == 0) {
    logger::log_warn("no trackers returned from database")
    return(df_trackers)
  }

  df_trackers_hydro <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT *
      FROM trackers_hydro
      WHERE tracker_id IN ({df_trackers$id*})",
      .con = con
    )
  ) %>%
    tibble::as_tibble()

  if (nrow(df_trackers_hydro) > 0) {
    df_dbkeys <- db_get_dbhydro_dbkeys(con = con, dbkeys = df_trackers_hydro$dbkey, include_stations = TRUE)
    df_trackers_hydro <- df_trackers_hydro %>%
      dplyr::left_join(df_dbkeys, by = "dbkey") %>%
      tidyr::nest(hydro = -c("tracker_id"))

    df_trackers <- df_trackers %>%
      dplyr::left_join(df_trackers_hydro, by = c("id" = "tracker_id"))
  } else {
    df_trackers <- df_trackers %>%
      dplyr::mutate(hydro = purrr::map(.data$id, ~ tibble::tibble()))
  }

  df_trackers_wq <- DBI::dbGetQuery(con, glue::glue_sql("SELECT * FROM trackers_wq WHERE tracker_id IN ({df_trackers$id*})", .con = con)) %>%
    tibble::as_tibble()

  if (nrow(df_trackers_wq) > 0) {
    df_stations <- db_get_dbhydro_stations(con, station_ids = df_trackers_wq$station_id)
    df_trackers_wq <- df_trackers_wq %>%
      dplyr::left_join(df_stations, by = "station_id") %>%
      tidyr::nest(wq = -c("tracker_id"))
    df_trackers <- df_trackers %>%
      dplyr::left_join(df_trackers_wq, by = c("id" = "tracker_id"))
  } else {
    df_trackers <- df_trackers %>%
      dplyr::mutate(wq = purrr::map(.data$id, ~ tibble::tibble()))
  }

  logger::log_debug("returning {nrow(df_trackers)} trackers")

  df_trackers
}
