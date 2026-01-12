#' Create a hydro_service object
#' @param provider_id ID as listed by [hydro_services()]
#' @export
hydro_service <- function(provider_id) {
  if (missing(provider_id) || length(provider_id) != 1L ||
      is.na(provider_id) || !nzchar(provider_id)) {
    rlang::abort("provider_id is required, e.g. hydro_service('EE_EST').")
  }
  provider_id <- as.character(provider_id)

  cfg <- .hydro_registry[[provider_id]]
  if (is.null(cfg)) {
    avail <- tryCatch(paste(hydro_services()$provider_id, collapse = ", "),
                      error = function(e) "<none>")
    rlang::abort(paste0("Unknown provider_id '", provider_id,
                        "'. Available: ", avail))
  }

  obj <- list(
    provider_id   = cfg$provider_id,
    provider_name = cfg$provider_name,
    country       = cfg$country,
    base_url      = cfg$base_url,
    geo_base_url  = cfg$geo_base_url,
    rate_cfg      = cfg$rate_cfg,
    auth          = cfg$auth
  )
  class(obj) <- c(paste0("hydro_service_", provider_id), "hydro_service")
  obj
}


#' @export
print.hydro_service <- function(x, ...) {
  cli::cli_h1("hydro_service: {x$provider_id}")
  cli::cli_text("Name:    {x$provider_name}")
  cli::cli_text("Country: {x$country}")
  cli::cli_text("Base URL:{x$base_url}")
  invisible(x)
}
