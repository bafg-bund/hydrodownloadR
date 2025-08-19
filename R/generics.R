#' Fetch station catalog (generic)
#' @export
stations <- function(x, ...) UseMethod("stations")

#' Fetch time series (generic)
#'
#' @param parameter One of "water_discharge", "water_level", "water_temperature"
#' @param stations Optional character vector of station IDs
#' @param start_date,end_date "YYYY-MM-DD" when mode = "range"
#' @param mode "range" or "complete"
#' @export
timeseries <- function(x,
                       parameter = c("water_discharge","water_level","water_temperature"),
                       stations = NULL, start_date = NULL, end_date = NULL,
                       mode = c("range","complete"), ...) {
  UseMethod("timeseries")
}
