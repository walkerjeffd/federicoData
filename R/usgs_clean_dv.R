#' Clean USGS daily values dataset
#'
#' @param x data frame containing raw usgs dataset returned from usgs_get_dv()
#'
#' @return tibble
#' @export
#'
#' @examples
#' \dontrun{
#' x <- usgs_get_dv(
#'   station_ids = "263180080205001",
#'   param = "STAGE",
#'   date_min = "2018-01-01",
#'   date_max = "2018-02-01"
#' )
#' usgs_clean_dv(x)
#' }
usgs_clean_dv <- function (x) {
  logger::log_debug("cleaning USGS dataset (nrow = {nrow(x)})")

  if (length(names(x)) != 5) {
    logger::log_error("USGS dataset contains too few/many columns, expected 5 but got {length(names(x))}")
    stop("USGS dataset could not be cleaned, wrong number of columns")
  }

  y <- tibble::as_tibble(x)
  names(y) <- c("agency", "station_id", "date", "value", "flag")

  y
}
