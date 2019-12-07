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

  df_trackers_hydro <- DBI::dbGetQuery(con, glue::glue_sql("SELECT * FROM trackers_hydro WHERE tracker_id IN ({df_trackers$id})", .con = con)) %>%
    tibble::as_tibble() %>%
    tidyr::nest(hydro = -c("tracker_id"))

  df_trackers_wq <- DBI::dbGetQuery(con, glue::glue_sql("SELECT * FROM trackers_wq WHERE tracker_id IN ({df_trackers$id})", .con = con)) %>%
    tibble::as_tibble() %>%
    tidyr::nest(wq = -c("tracker_id"))

  df <- df_trackers %>%
    dplyr::left_join(df_trackers_hydro, by = c("id" = "tracker_id")) %>%
    dplyr::left_join(df_trackers_wq, by = c("id" = "tracker_id"))

  logger::log_debug("returning {nrow(df)} trackers")

  df
}
