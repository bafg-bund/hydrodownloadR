# R/data-fr_hubeau_meta.R

#' FR_HUBEAU precomputed station metadata
#'
#' Preloaded metadata for Hub'Eau stations, built offline from the Hub’Eau
#' referentials plus scraped site/station fiches (area, site altitude,
#' gauge-zero altitude, vertical datum at site). Used to speed up
#' `stations()` for the FR_HUBEAU provider.
#'
#' @format A data frame/tibble with columns:
#' \describe{
#'   \item{code_site}{Character. Hub'Eau site code.}
#'   \item{station_id}{Character. Hub'Eau station code.}
#'   \item{area}{Numeric (km\eqn{^2}). Catchment area from site fiche; may be \code{NA}.}
#'   \item{altitude_api}{Numeric (m). API referential altitude (hydrometry in mm to m; temperature in m).}
#'   \item{altitude_site}{Numeric (m). Site altitude parsed from the site fiche; may be \code{NA}.}
#'   \item{altitude_station}{Numeric (m). “Cote du zero d'echelle” from station fiche; may be \code{NA}.}
#'   \item{vertical_datum_site}{Character. Site-level vertical datum label; may be \code{NA}.}
#'   \item{retrieved_at}{POSIXct (UTC). Timestamp when the row was scraped.}
#' }
#'
#' @details
#' Built by \code{data-raw/fr_hubeau_meta_build.R}. The file
#' \code{data/fr_hubeau_meta.rda} is shipped with the package and may be refreshed
#' out-of-band. A build-date string is also stored in the object attribute
#' \code{metadata_date}.
#'
#' @source Hub'Eau APIs and https://www.hydro.eaufrance.fr/ site/station fiches.
#' @keywords datasets
#' @name fr_hubeau_meta
#' @docType data
NULL
