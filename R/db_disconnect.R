#' Disconnect from database
#'
#' @param con connection object
#'
#' @return TRUE if successfull
#' @export
#'
#' @examples
#' \dontrun{
#' con <- db_connect("localhost", "mydb", "joe", "mypassword")
#' db_disconnect(con)
#' }
db_disconnect <- function(con) {
  logger::log_info("disconnecting from database")

  DBI::dbDisconnect(con)
}
