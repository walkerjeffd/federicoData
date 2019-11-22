#' Convert latitude/longitude
#'
#' Converts latitude/longitude from DDMMSS.S to decimal degrees
#'
#' @param x numeric vector of latitudes/longitudes in DDMMSS.SS format
#'
#' @return numeric vector in decimal degrees
#' @export
#'
#' @examples
#' dms_to_ddeg(262551.3)
#' dms_to_ddeg(805649.2)
#' dms_to_ddeg(c(262551.3, 805649.2))
dms_to_ddeg <- function(x) {
  stopifnot(class(x) %in% c("numeric", "integer"))
  x_deg <- floor(x / 1e4)
  x_min <- floor((x - x_deg * 1e4) / 1e2)
  x_sec <- x - x_deg * 1e4 - x_min * 1e2
  x_deg + (x_min + x_sec / 60) / 60
}
