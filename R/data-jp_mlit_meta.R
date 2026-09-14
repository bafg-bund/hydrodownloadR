#' Japan MLIT stations metadata snapshot
#'
#' A processed station metadata table used internally by the JP MLIT adapter.
#' Coordinates from the official MLIT station pages are preferred when
#' available. Coordinates from the supplied metadata CSV are used as a
#' fallback.
#'
#' @format A tibble/data.frame with one row per station:
#' \describe{
#'   \item{station_id}{
#'     MLIT station identifier (character).
#'   }
#'   \item{station_name}{
#'     English station name (character).
#'   }
#'   \item{river}{
#'     English river-system name (character).
#'   }
#'   \item{lat}{
#'     Selected latitude in WGS84 decimal degrees (double).
#'   }
#'   \item{lon}{
#'     Selected longitude in WGS84 decimal degrees (double).
#'   }
#'   \item{area}{
#'     Drainage area in square kilometres, if available (double).
#'   }
#'   \item{altitude}{
#'     Gauge-zero elevation in metres, if available (double).
#'   }
#'   \item{station_name_original}{
#'     Original Japanese station name (character).
#'   }
#'   \item{lat_website}{
#'     Latitude obtained from the official MLIT station page (double).
#'   }
#'   \item{lon_website}{
#'     Longitude obtained from the official MLIT station page (double).
#'   }
#'   \item{coordinate_source}{
#'     Source used for \code{lat} and \code{lon}. Either
#'     \code{"MLIT website"} or \code{"MLIT metadata CSV"} (character).
#'   }
#' }
#'
#' @source Japan Ministry of Land, Infrastructure, Transport and Tourism
#'   (MLIT).
#' @keywords datasets
#' @name jp_mlit_meta
#' @docType data
#' @usage data(jp_mlit_meta)
NULL
