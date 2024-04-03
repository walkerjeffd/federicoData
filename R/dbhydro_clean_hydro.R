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

  if (!"qualifier" %in% colnames(x)) {
    x$qualifier <- NA_character_
  }

  x %>%
    dplyr::mutate_at(c("dbkey", "type", "units", "qualifer"), as.character) %>%
    dplyr::mutate(
      date = lubridate::as_date(.data$date),
      revision_date = lubridate::dmy(.data$revision_date),
      value = as.numeric(.data$data_value),
      qualifier = dplyr::if_else(.data$qualifer == "", NA_character_, .data$qualifer)
    ) %>%
    dplyr::select(c("dbkey", "type", "units", "date", "value", "qualifier", "revision_date"))
}
