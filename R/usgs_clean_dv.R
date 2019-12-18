#' Clean USGS daily values dataset
#'
#' Renames columns and adds new columns for param, units, stat_code
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
#'   param = "stage",
#'   date_min = "2018-01-01",
#'   date_max = "2018-02-01"
#' )
#' usgs_clean_dv(x)
#' }
usgs_clean_dv <- function (x) {
  logger::log_debug("cleaning usgs dataset (nrow = {nrow(x)})")

  if (length(names(x)) != 5) {
    logger::log_error("usgs dataset contains too few/many columns, expected 5 but got {length(names(x))}")
    stop("usgs dataset could not be cleaned, wrong number of columns")
  }

  y <- tibble::as_tibble(x)
  names(y) <- c("agency", "station_id", "date", "value", "flag")

  param_code <- attr(x, "variableInfo")$variableCode
  param <- usgs_params$param[which(usgs_params$param_code == param_code)]

  if (length(param) == 0) {
    logger::log_error("usgs dataset includes unknown parameter code")
    stop("usgs dataset includes unknown parameter code")
  }

  y$param <- param
  y$param_code <- param_code
  y$unit <- attr(x, "variableInfo")$unit
  y$stat_code <- attr(x, "statisticInfo")$statisticCd
  y
}
