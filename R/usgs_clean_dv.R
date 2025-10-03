#' Clean USGS daily values dataset
#'
#' Renames columns and adds new columns for param, units, stat_code
#'
#' @param x data frame containing raw usgs dataset returned from usgs_get_dv()
#'
#' @return tibble
#' @export
#' 
#' @importFrom rlang .data
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

  y <- dplyr::transmute(
    x,
    "station_id" = gsub("USGS-", "", .data$monitoring_location_id),
    date = .data$time,
    value = .data$value,
    flag = dplyr::case_when(
      .data$approval_status == "Approved" ~ "A",
      .data$approval_status == "Provisional" ~ "P",
      TRUE ~ .data$approval_status
    ),
    param_code = .data$parameter_code,
    units = .data$unit_of_measure,
    stat_code = .data$statistic_id
  ) |> 
    dplyr::left_join(
      dplyr::select(usgs_params, .data$param_code, .data$param),
      by = "param_code"
    ) |>
    dplyr::relocate("param", .after = "station_id")

  y
}
