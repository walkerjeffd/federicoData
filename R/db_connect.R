#' Connect to database
#'
#' @param host hostname
#' @param dbname database name
#' @param user username
#' @param password password (optional if .pgpass is configured)
#' @param port port (default 5432)
#'
#' @return DBI connection object
#' @export
#'
#' @examples
#' \dontrun{
#' con <- db_connect("localhost", "mydb", "joe", "mypassword")
#' }
db_connect <- function(host, dbname, user, password = NULL, port = 5432) {
  logger::log_info("connecting to postgresql://{user}@{host}:{port}/{dbname}")

  DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = host,
    port = port,
    user = user,
    password = password,
    dbname = dbname
  )
}
