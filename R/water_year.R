#' Get water year component of datetime(s)
#'
#' A water year is a 12-month period beginning in some month other than January.
#' The water year is designated by the calendar year in which it ends (e.g.
#' water year 2007 ends on 2007-09-30 if the starting month is October)
#'
#' @param x vector of datetime objects
#' @param start_month integer specifying first month of water year (default = 10 for October)
#'
#' @return numeric vector of water years
#' @export
#'
#' @examples
#' x <- seq.Date(from = as.Date("2007-09-30"), to = as.Date("2007-10-02"), by = "day")
#' water_year(x)
water_year <- function (x, start_month = 10) {
  if (start_month == 1) {
    return(lubridate::year(x))
  } else {
    return(
      ifelse(
        lubridate::month(x) >= start_month,
        lubridate::year(x) + 1,
        lubridate::year(x)
      )
    )
  }
}
