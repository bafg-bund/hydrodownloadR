#' SYKE runoff station metadata (area & altitude)
#'
#' Catchment area and altitude for Finnish SYKE runoff stations.
#' Area may be \code{NA} for a few stations; altitude may still be present.
#' Used to compute discharge from runoff time series:
#' \code{discharge_m3s = (value_lps_per_km2 * area_km2) / 1000}.
#'
#' @format A tibble with:
#' \describe{
#'   \item{place_id}{Character. SYKE Paikka_Id.}
#'   \item{area}{Numeric (km^2). May be NA.}
#'   \item{altitude}{Numeric (m). May be NA.}
#' }
#' @source Finnish Environment Institute (SYKE).
"fi_syke_runoff_meta"
