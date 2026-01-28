#' Japan MLIT stations metadata snapshot
#'
#' A tibble used by the JP MLIT adapter to speed up station discovery.
#'
#' @format A tibble/data.frame with one row per station and typical columns:
#' \describe{
#'   \item{station_id}{MLIT station identifier (character)}
#'   \item{station_name}{Station name (character)}
#'   \item{river}{River name, if available (character)}
#'   \item{lat}{Latitude in WGS84 (double)}
#'   \item{lon}{Longitude in WGS84 (double)}
#'   \item{area_km2}{Drainage area in km^2, if available (double)}
#'   \item{altitude_m}{Altitude in meters, if available (double)}
#'   \item{country}{ISO country code (character)}
#'   \item{provider_id}{Adapter provider id, e.g. \code{"JP_MLIT"} (character)}
#'   \item{provider_name}{Provider name (character)}
#' }
#' @source MLIT; see package README for licensing.
#' @keywords datasets
#' @name jp_mlit_meta
#' @docType data
#' @usage data(jp_mlit_meta)
NULL
