#' Distinct list of countries
#' @export
list_countries <- function() {
  dplyr::distinct(hydro_services(), country) |>
    dplyr::arrange(country)
}

#' Convenience: fetch stations by provider_id
#' @export
get_stations <- function(provider_id) {
  s <- hydro_service(provider_id)
  stations(s)
}

#' Convenience: fetch time series by provider_id
#' @export
get_timeseries <- function(provider_id,
                           parameter = c("water_discharge","water_level","water_temperature"),
                           stations = NULL,
                           start_date = NULL, end_date = NULL,
                           mode = c("range","complete")) {
  s <- hydro_service(provider_id)
  timeseries(s, parameter = parameter, stations = stations,
             start_date = start_date, end_date = end_date, mode = mode)
}
