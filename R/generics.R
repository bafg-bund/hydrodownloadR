#' List stations for a provider
#'
#' @param x A `hydro_service` object created by [hydro_service()].
#' @param ... Passed to provider-specific methods.
#' @return A tibble with station metadata.
#' @export
#'
#' @examples
#' # Offline: enumerate providers (no network)
#' s <- hydro_services()
#' head(names(s))
#'
#' @examplesIf interactive() && requireNamespace("curl", quietly = TRUE) && curl::has_internet() && identical(Sys.getenv("HYDRO_EXAMPLES"), "true")
#' # Online (opt-in): fetch stations
#' x <- hydro_service("SE_SMHI")
#' st <- stations(x)
#' head(st)
stations <- function(x, ...) UseMethod("stations")

#' Retrieve time series for a provider
#'
#' @param x A `hydro_service` object created by [hydro_service()].
#' @param parameter One of "water_discharge","water_level","water_temperature","water_velocity".
#' @param stations Optional character vector of station IDs.
#' @param start_date,end_date `YYYY-MM-DD` strings for `mode = "range"`.
#' @param mode Either `"range"` or `"complete"` (`1900-01-01` to today).
#' @param ... Passed to provider-specific methods.
#' @return A tibble with columns:
#'   country, provider_id, provider_name, station_id, parameter,
#'   timestamp, value, unit, quality_code, source_url.
#' @export
#'
#' @examples
#' # Offline: construct a service object (no network)
#' x <- hydro_service("SE_SMHI")
#'
#' @examplesIf interactive() && requireNamespace("curl", quietly = TRUE) && curl::has_internet() && identical(Sys.getenv("HYDRO_EXAMPLES"), "true")
#' # Online (opt-in): one station for a short range
#' st <- head(stations(x)$station_id, 1)
#' ts <- timeseries(x, parameter = "water_discharge",
#'                  stations = st,
#'                  start_date = "2020-01-01", end_date = "2020-01-10")
#' head(ts)
timeseries <- function(x, parameter, stations = NULL,
                       start_date = NULL, end_date = NULL,
                       mode = c("range","complete"), ...) {
  UseMethod("timeseries")
}

#' List supported parameters/units for a provider
#'
#' @param x A `hydro_service` object.
#' @param ... Reserved for future use.
#' @return A tibble with columns: parameter, code, unit.
#' @export
timeseries_parameters <- function(x, ...) UseMethod("timeseries_parameters")

