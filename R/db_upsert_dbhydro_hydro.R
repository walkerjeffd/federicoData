#' Add/update hydrologic data in database
#'
#' Fetches data from DBHYDRO for given list of dbkeys, then performs
#' upsert operation to insert/update the results in the database.
#' Existing records will be updated only if the revision date.
#'
#' @param con database connection object
#' @param dbkeys character vector of dbkeys
#' @param date_min start date
#' @param date_max end date
#' @param batch_size batch size for retrieving data from dbhydro (passed to \code{dbhydro_batch_get_hydro()})
#'
#' @return TRUE if operation was successful
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' db_upsert_dbhydro_hydro(
#'   con,
#'   dbkeys = c("91346", "91347"),
#'   date_min = "2019-10-01",
#'   date_max = "2019-11-30"
#' )
#' }
db_upsert_dbhydro_hydro <- function (con, dbkeys, date_min, date_max, batch_size = 5) {
  if (length(dbkeys) == 0) {
    logger::log_info("no dbkeys to update, doing nothing")
    return(TRUE)
  }

  dbkeys <- unique(dbkeys)

  if (any(is.na(dbkeys)) || is.na(date_min) || is.na(date_max)) {
    logger::log_error("arguments (dbkeys, date_min, date_max) cannot be NA")
    return(FALSE)
  }

  logger::log_info("upserting {length(dbkeys)} dbkeys from {date_min} to {date_max}")

  # fetch existing dbkeys
  db_dbkeys <- db_get_dbhydro_dbkeys(con, dbkeys = dbkeys)

  missing_dbkeys <- setdiff(dbkeys, db_dbkeys$dbkey)

  if (length(missing_dbkeys) > 0) {
    logger::log_error("{length(missing_dbkeys)} dbkeys are missing in the database, please add them and then try again ({paste0(missing_dbkeys, collapse = ', ')})")
    return(FALSE)
  }

  # fetch data from dbhydro
  df_fetch_raw <- dbhydro_batch_get_hydro(dbkeys = dbkeys, date_min = date_min, date_max = date_max, batch_size = batch_size)
  df_fetch <- dbhydro_clean_hydro(df_fetch_raw)

  # fetch existing data from db
  df_existing <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT id, dbkey, date, revision_date as db_revision_date FROM dbhydro_hydro WHERE dbkey IN ({dbkeys*}) AND date >= {date_min} AND date <= {date_max}",
      .con = con
    )
  )

  # generate insert/update dataframes
  if (nrow(df_existing) == 0) {
    logger::log_debug("no existing hydro data in database, inserting all records")
    df_insert <- df_fetch
    df_update <- data.frame()
  } else {
    df_merge <- df_fetch %>%
      dplyr::left_join(df_existing, by = c("dbkey", "date"))

    df_insert <- df_merge %>%
      dplyr::filter(is.na(.data$id)) %>%
      dplyr::select(-c("id", "db_revision_date"))

    df_update <- df_merge %>%
      dplyr::filter(!is.na(.data$id), .data$revision_date > .data$db_revision_date) %>%
      dplyr::select(-c("db_revision_date"))
  }

  # update existing (delete then insert)
  if (nrow(df_update) == 0) {
    logger::log_info("no existing records need to be updated")
  } else {
    logger::log_info("updating {nrow(df_update)} record(s) of {nrow(df_fetch)} total")
    logger::log_debug("deleting {nrow(df_update)} record(s)")
    n_deleted <- DBI::dbExecute(con, glue::glue_sql("DELETE FROM dbhydro_hydro WHERE id IN ({ids*})", ids = df_update$id))

    if (n_deleted != nrow(df_update)) {
      logger::log_error("failed to delete {nrow(df_update)} record(s), only {n_deleted} succeeded")
      return(FALSE)
    }

    logger::log_debug("inserting {nrow(df_update)} record(s) that were deleted")
    ok <- DBI::dbWriteTable(con, "dbhydro_hydro", df_update, append = TRUE, row.names = FALSE)

    if (!ok) {
      logger::log_error("failed to insert {nrow(df_update)} during update portion of upsert")
      return(FALSE)
    }
  }

  # insert (new records)
  if (nrow(df_insert) == 0) {
    logger::log_info("no new records to insert")
  } else {
    logger::log_info("inserting {nrow(df_insert)} new record(s)")
    ok <- DBI::dbWriteTable(con, "dbhydro_hydro", df_insert, append = TRUE, row.names = FALSE)
    if (!ok) {
      logger::log_error("failed to insert {nrow(df_insert)} new record(s) during insert portion of upsert")
    }
  }

  TRUE
}
