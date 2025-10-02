#' Clean hydrologic dataset from DBHYDRO
#'
#' @param x tibble returned from dbhydro_get_hydro()
#'
#' @return tibble with primary columns
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' x <- dbhydro_get_hydro(dbkeys = "91599", date_min = "2019-10-01", date_max = "2019-10-31")
#' dbhydro_clean_hydro(x)
#' }
dbhydro_clean_hydro <- function (x) {
  logger::log_debug("cleaning hydro dataset from DBHYDRO (nrow = {nrow(x)})")

  x <- dplyr::transmute(
    x,
    "dbkey" = .data$timeseriesid,
    "type" = .data$parameter,
    "date" = lubridate::as_date(.data$timestamp),
    "value" = .data$value,
    "qualifier" = .data$code,
    "revision_date" = lubridate::as_date(.data$revision_date),
    "quality_code" = .data$quality_code
  )
  x
}
