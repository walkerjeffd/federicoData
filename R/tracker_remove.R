#' Remove tracker from database
#'
#' @param con database connection object
#' @param id tracker ID
#' @param remove_data remove all hydro and wq data for tracker
#'
#' @return TRUE if tracker was successfully removed
#' @export
#'
#' @examples
#' \dontrun{
#' tracker_remove(con, "test-tracker")
#' }
tracker_remove <- function(con, id, remove_data = FALSE) {
  logger::log_info("removing tracker {id}")

  if (remove_data) {
    stop("remove_data not yet implemented")
  }

  n_deleted <- DBI::dbExecute(con, glue::glue_sql("DELETE FROM trackers WHERE id = {id}", .con = con))

  if (n_deleted == 0) {
    logger::log_warn("tracker {id} was not deleted, may not have existed")
  }

  TRUE
}
