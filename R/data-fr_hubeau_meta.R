#' FR_HUBEAU precomputed station metadata
#'
#' Preloaded metadata scraped from hydro.eaufrance.fr fiches to speed up
#' `stations()` for the FR_HUBEAU provider.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{code_site}{Character site code (e.g., "A1050030")}
#'   \item{station_id}{Character station code (e.g., "A105003001")}
#'   \item{area}{Catchment area in km² (numeric)}
#'   \item{altitude_api}{Altitude from Hub'Eau API (m, numeric)}
#'   \item{altitude_site}{Altitude from site fiche (m, numeric)}
#'   \item{altitude_station}{Gauge-zero altitude from station fiche (m, numeric)}
#'   \item{vertical_datum_site}{Character label of the site’s vertical datum (e.g. "IGN 1969")}
#'   \item{retrieved_at}{POSIXct timestamp when row was scraped}
#' }
#'
#' Attribute \code{metadata_date} (character "YYYY-MM-DD") records the build date.
#'
#' @usage data(fr_hubeau_meta)
#' @docType data
#' @keywords datasets
#' @source \url{https://www.hydro.eaufrance.fr}
"fr_hubeau_meta"
