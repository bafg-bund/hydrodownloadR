# Registrierung beim Paket-Load
# (Die echte base_url und Limits einsetzen)
register_DK_EPA <- function() {
  register_service(
    provider_id = "DK_EPA",
    provider_name = "Danish Environmental Protection Agency",
    country = "DK",
    base_url = "https://vandah.miljoeportal.dk/api/swagger/",
    rate_cfg = list(n = 5, period = 1),
    auth = list(type = "none")
  )
}

#' @export
stations.hydro_service_DK_EPA <- function(x, ...) {
  limited <- ratelimitr::limit_rate(function() {
    req  <- build_request(x, path = "/stations")
    resp <- perform_request(req)
    dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    tibble::as_tibble(dat) |>
      dplyr::transmute(
        country = x$country,
        service_id = x$id,
        station_id = .data$id,
        station_name = .data$name,
        lat = .data$lat, lon = .data$lon,
        river = .data$river %||% NA_character_,
        catchment = .data$basin %||% NA_character_,
        timezone = .data$timezone %||% "UTC",
        extras = I(vector("list", length(.data$id)))
      )
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))
  limited()
}

#' @export
timeseries.hydro_service_DK_EPA <- function(x, parameter = c("water_discharge","water_level"),
                                            stations = NULL, start_date = NULL, end_date = NULL,
                                            mode = c("range","complete"), ...) {
  parameter <- match.arg(parameter)
  rng <- resolve_dates(match.arg(mode), start_date, end_date)

  ids <- stations %||% character()
  batches <- if (length(ids)) chunk_vec(ids, 50) else list(NULL)

  pb <- progress::progress_bar$new(total = length(batches))
  out <- lapply(batches, function(batch) {
    pb$tick()
    query <- list(
      parameter = switch(parameter,
                         water_discharge = "discharge",
                         water_level     = "level"),
      start = rng$start_date,
      end   = rng$end_date,
      ids   = if (!is.null(batch)) paste(batch, collapse = ",") else NULL
    )
    req  <- build_request(x, path = "/timeseries", query = query)
    resp <- perform_request(req)
    dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    tibble::tibble(
      country = x$country,
      service_id = x$id,
      station_id = dat$station_id,
      parameter  = parameter,
      timestamp  = lubridate::as_datetime(dat$timestamp, tz = dat$timezone %||% "UTC"),
      value      = as.numeric(dat$value),
      unit       = if (parameter == "water_discharge") "m^3/s" else "cm",
      quality_code = dat$quality %||% NA_character_,
      source_url = paste0(x$base_url, "/timeseries"),
      extras = I(vector("list", length(dat$station_id)))
    )
  })

  dplyr::bind_rows(out)
}
