# ==== Estonia (ESTMODEL) adapter ============================================
# Base: http://estmodel.envir.ee/
# Public endpoints (examples):
# - /countries
# - /countries/EE/stations
# - /stations/{code}/measurements?parameter=Q|H|T&type=MEAN[&dateFrom=YYYY-MM-DD&dateTo=YYYY-MM-DD]
#
# We try server-side date filtering; if the API ignores unknown keys we fall
# back to client-side filtering.

# -- Registration -------------------------------------------------------------

register_EE_EST <- function() {
  register_service(
    provider_id   = "EE_EST",
    provider_name = "Estonian Environment Agency – ESTMODEL API",
    country       = "EE",
    base_url      = "http://estmodel.envir.ee",
    # Conservative limits help avoid bans
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

# -- Parameter mapping --------------------------------------------------------

.ee_param_map <- function(parameter) {
  switch(parameter,
         water_discharge   = list(code = "Q", unit = "m^3/s"),
         water_level       = list(code = "H", unit = "cm"),
         water_temperature = list(code = "T", unit = "°C"),
         rlang::abort("EE_EST supports 'water_discharge', 'water_level' or 'water_temperature'.")
  )
}

# -- Stations (S3 method) ----------------------------------------------------

#' @export
stations.hydro_service_EE_EST <- function(x, ...) {
  path <- "/countries/EE/stations"

  limited <- ratelimitr::limit_rate(
    function() {
      req  <- build_request(x, path = path)
      resp <- perform_request(req)
      dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      df   <- tibble::as_tibble(dat)

      # Be defensive about field names across versions
      code   <- df$code     %||% df$id        %||% df$stationId    %||% NA_character_
      name   <- df$name     %||% df$label     %||% df$stationName  %||% NA_character_
      lat    <- df$lat      %||% df$latitude
      lon    <- df$lon      %||% df$longitude
      river  <- df$river    %||% df$waterbody %||% NA_character_
      basin  <- df$basin    %||% df$catchment %||% NA_character_
      tz     <- df$timezone %||% "UTC"
      type   <- df$type     %||% NA_character_

      keep <- rep(TRUE, length(code))
      if (!all(is.na(type))) keep <- type %in% c("HYDROLOGICAL","hydrological")

      tibble::tibble(
        country       = x$country,
        provider_id   = x$provider_id,
        provider_name = x$provider_name,
        station_id    = as.character(code)[keep],
        station_name  = as.character(name)[keep],
        lat           = suppressWarnings(as.numeric(lat))[keep],
        lon           = suppressWarnings(as.numeric(lon))[keep],
        river         = as.character(river)[keep],
        catchment     = as.character(basin)[keep],
        timezone      = as.character(tz)[keep],
        extras        = I(vector("list", length(which(keep))))
      )
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  limited()
}

# -- Time series (S3 method) -------------------------------------------------

#' @export
timeseries.hydro_service_EE_EST <- function(x,
                                            parameter = c("water_discharge","water_level","water_temperature"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("range","complete"),
                                            exclude_quality = NULL,
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .ee_param_map(parameter)

  ids <- stations %||% character()
  # The API typically serves one station per request -> iterate
  batches <- if (length(ids)) chunk_vec(ids, 50) else list(NULL)

  pb <- progress::progress_bar$new(total = length(batches))
  out <- lapply(batches, function(batch) {
    pb$tick()

    base_query <- list(parameter = pm$code, type = "MEAN")
    date_queries <- if (mode == "range") {
      list(
        # try common key variants; API will ignore unknown keys
        dateFrom  = as.character(rng$start_date),
        dateTo    = as.character(rng$end_date),
        startDate = as.character(rng$start_date),
        endDate   = as.character(rng$end_date),
        from      = as.character(rng$start_date),
        to        = as.character(rng$end_date)
      )
    } else list()

    station_vec <- if (is.null(batch)) {
      st <- stations.hydro_service_EE_EST(x)
      st$station_id
    } else batch

    one_station <- ratelimitr::limit_rate(
      function(st_id) {
        path <- paste0("/stations/", utils::URLencode(st_id, reserved = TRUE), "/measurements")
        req  <- build_request(x, path = path, query = c(base_query, date_queries))
        resp <- perform_request(req)

        status <- httr2::resp_status(resp)
        if (status == 404) return(tibble::tibble())
        if (status %in% c(401, 403)) {
          rlang::warn(paste0("EE_EST: access denied for station ", st_id, " (", status, ")."))
          return(tibble::tibble())
        }

        dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        if (is.null(dat) || length(dat) == 0) return(tibble::tibble())

        df <- tibble::as_tibble(dat)
        ts  <- df$startDate %||% df$time %||% df$timestamp %||% df$date
        val <- df$value     %||% df$val  %||% df$mean      %||% df$y
        qf  <- df$qualityFlag %||% df$quality %||% df$flag %||% NA_character_
        tz  <- df$timezone    %||% "UTC"

        ts_parsed <- suppressWarnings(lubridate::as_datetime(ts, tz = tz))

        # Client-side date filtering fallback
        if (mode == "range") {
          keep <- !is.na(ts_parsed) &
            ts_parsed >= as.POSIXct(rng$start_date) &
            ts_parsed <= as.POSIXct(rng$end_date) + 86399
        } else {
          keep <- rep(TRUE, length(ts_parsed))
        }

        if (!is.null(exclude_quality) && !all(is.na(qf))) {
          keep <- keep & !(qf %in% exclude_quality)
        }
        if (!any(keep)) return(tibble::tibble())

        tibble::tibble(
          country       = x$country,
          provider_id   = x$provider_id,
          provider_name = x$provider_name,
          station_id    = st_id,
          parameter     = parameter,
          timestamp     = ts_parsed[keep],
          value         = suppressWarnings(as.numeric(val[keep])),
          unit          = pm$unit,
          quality_code  = if (all(is.na(qf))) NA_character_ else as.character(qf[keep]),
          source_url    = paste0(x$base_url, path),
          extras        = I(vector("list", length(which(keep))))
        )
      },
      rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
    )

    dplyr::bind_rows(lapply(station_vec, one_station))
  })

  dplyr::bind_rows(out)
}
