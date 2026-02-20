#' Internal registry for available providers
#' @keywords internal
.hydro_registry <- new.env(parent = emptyenv())

#' Register a provider (internal)
#'
#' @param provider_id Short code, e.g. "EE_EST"
#' @param provider_name Human-friendly name
#' @param country ISO country code
#' @param base_url Base URL for API calls (time series, metadata)
#' @param geo_base_url Optional base URL used only for coordinates (e.g. GeoJSON)
#' @param rate_cfg Rate limiting config, e.g. list(n = 5, period = 1)
#' @param auth Auth config
#' @param license Short license label (e.g. "CC BY 4.0", "NLOD 2.0", "Public Domain", "UNKNOWN")
#' @param license_link Canonical URL pointing to license / terms
#' @param access_class Access category (see provider_usage.R enums)
#' @param reuse_class Reuse category (see provider_usage.R enums)
#' @param is_open_data TRUE/FALSE/NA (derived/assigned classification)
#' @keywords internal
#' @noRd
#' @keywords internal
#' @noRd
register_service <- function(provider_id, provider_name, country, base_url,
                             geo_base_url = NULL,
                             rate_cfg = list(n = 5, period = 1),
                             auth = list(type = "none"),
                             license = NA_character_,
                             license_link = NA_character_,
                             access_class = "unknown",
                             reuse_class = "unknown",
                             is_open_data = NA) {

  # ---- inject from provider_usage if not explicitly provided ----
  if ((is.na(license) || identical(access_class, "unknown") || identical(reuse_class, "unknown") || is.na(is_open_data)) &&
      exists("provider_usage_get", mode = "function", inherits = TRUE)) {

    u <- tryCatch(provider_usage_get(provider_id), error = function(e) NULL)

    if (!is.null(u)) {
      if (is.na(license))       license       <- u$license
      if (is.na(license_link))  license_link  <- u$license_link
      if (identical(access_class, "unknown")) access_class <- u$access_class
      if (identical(reuse_class,  "unknown")) reuse_class  <- u$reuse_class
      if (is.na(is_open_data))  is_open_data  <- u$is_open_data
    }
  }

  .hydro_registry[[provider_id]] <- list(
    provider_id   = provider_id,
    provider_name = provider_name,
    country       = country,
    base_url      = base_url,
    geo_base_url  = geo_base_url,
    rate_cfg      = rate_cfg,
    auth          = auth,

    license       = license,
    license_link  = license_link,
    access_class  = access_class,
    reuse_class   = reuse_class,
    is_open_data  = is_open_data
  )

  invisible(TRUE)
}



#' List available providers
#'
#' @return A tibble with columns:
#'   provider_id, provider_name, country, base_url,
#'   license, license_link, access_class, reuse_class, is_open_data
#' @export
hydro_services <- function() {
  lst <- mget(ls(.hydro_registry), envir = .hydro_registry, inherits = FALSE)

  if (!length(lst)) {
    return(tibble::tibble(
      provider_id   = character(),
      provider_name = character(),
      country       = character(),
      base_url      = character(),
      license       = character(),
      license_link  = character(),
      access_class  = character(),
      reuse_class   = character(),
      is_open_data  = logical()
    ))
  }

  get_chr <- function(x, name) {
    v <- x[[name]]
    if (is.null(v) || is.na(v)) return(NA_character_)
    as.character(v)
  }
  get_lgl <- function(x, name) {
    v <- x[[name]]
    if (is.null(v)) return(NA)
    if (is.na(v)) return(NA)
    as.logical(v)
  }

  tibble::tibble(
    provider_id   = vapply(lst, `[[`, "", "provider_id"),
    provider_name = vapply(lst, `[[`, "", "provider_name"),
    country       = vapply(lst, `[[`, "", "country"),
    base_url      = vapply(lst, `[[`, "", "base_url"),

    license       = vapply(lst, get_chr, "", "license"),
    license_link  = vapply(lst, get_chr, "", "license_link"),
    access_class  = vapply(lst, get_chr, "", "access_class"),
    reuse_class   = vapply(lst, get_chr, "", "reuse_class"),
    is_open_data  = vapply(lst, get_lgl, NA, "is_open_data")
  )
}
