#' Create a hydro_service object
#'
#' @param provider_id ID as listed by [hydro_services()]
#' @export
hydro_service <- function(provider_id) {
  cfg <- .hydro_registry[[provider_id]]
  if (is.null(cfg)) rlang::abort(paste0("Unknown provider_id: ", provider_id))

  obj <- list(
    provider_id   = cfg$provider_id,
    provider_name = cfg$provider_name,
    country       = cfg$country,
    base_url      = cfg$base_url,
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
