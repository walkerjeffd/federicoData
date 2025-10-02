#' Update preliminary water quality dataset
#'
#' Replaces the usace_wq table with new dataset
#'
#' @param con database connection object
#' @param df data frame containing preliminary wq data
#'
#' @return TRUE if operation was successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_update_usace_wq(con, df)
#' }
db_update_usace_wq <- function (con, df) {
  logger::log_info("updating preliminary usace wq data")

  logger::log_debug("adding wq_param")
  df <- df |>
    dplyr::left_join(
      dbhydro_wq_params,
      by = "test_name"
    )

  logger::log_debug("truncating usace_wq table")
  n_deleted <- DBI::dbExecute(con, "TRUNCATE TABLE usace_wq")

  if (nrow(df) == 0) {
    logger::log_debug("no new data to insert, doing nothing")
    return(TRUE)
  }

  logger::log_debug("inserting {nrow(df)} new records into usace_wq")
  ok <- DBI::dbWriteTable(con, "usace_wq", df, append = TRUE, row.names = FALSE)

  if (!ok) {
    logger::log_error("failed to insert new record(s)")
    return(FALSE)
  }

  return(TRUE)
}
