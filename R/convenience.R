#' List available countries
#'
#' @return A character vector of country codes (e.g. ISO 3166-1 alpha-2) for which
#'   at least one provider is available.
#' @export
list_countries <- function() {
  dplyr::distinct(hydro_services(), country) |>
    dplyr::arrange(country)
}

