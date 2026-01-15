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
#' @keywords internal
register_service <- function(provider_id, provider_name, country, base_url,
                             geo_base_url = NULL,
                             rate_cfg = list(n = 5, period = 1),
                             auth = list(type = "none")) {
  .hydro_registry[[provider_id]] <- list(
    provider_id   = provider_id,
    provider_name = provider_name,
    country       = country,
    base_url      = base_url,
    geo_base_url  = geo_base_url,
    rate_cfg      = rate_cfg,
    auth          = auth
  )
  invisible(TRUE)
}


#' List available providers
#'
#' @return A tibble with columns: provider_id, provider_name, country, base_url
#' @export
hydro_services <- function() {
  lst <- mget(ls(.hydro_registry), envir = .hydro_registry, inherits = FALSE)
  if (!length(lst)) {
    return(tibble::tibble(
      provider_id = character(), provider_name = character(),
      country = character(), base_url = character()
    ))
  }
  tibble::tibble(
    provider_id   = vapply(lst, `[[`, "", "provider_id"),
    provider_name = vapply(lst, `[[`, "", "provider_name"),
    country       = vapply(lst, `[[`, "", "country"),
    base_url      = vapply(lst, `[[`, "", "base_url")
  )
}
