#' LT_LHMT adapter (Lithuanian Hydrometeorological Service) -------------------
#' Base: https://api.meteo.lt/v1/
#' Hydro: /hydro-stations, /hydro-stations/{code}/observations/{measured|historical}/{date|latest}
#' Docs show: measured -> waterLevel (cm), waterTemperature (C), hourly timestamps (last 30d)
#'            historical -> waterLevel (cm, daily mean), waterDischarge (m3/s, daily mean), since 2000
#' Rate guidance in API page (IP-based) >> we'll still use list(n = 3, period = 1) as requested.
# Provider registration --------------------------------------------------------

#' @keywords internal
#' @noRd
register_LT_LHMT <- function() {
  register_service_usage(
    provider_id   = "LT_LHMT",
    provider_name = "Lithuanian Hydrometeorological Service (LHMT)",
    country       = "Lithuania",
    base_url      = "https://api.meteo.lt/v1/",     # API (time series & JSON metadata)
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_LT_LHMT <- function(x, ...) {
  c("water_discharge", "water_level")
}

# Parameter map ---------------------------------------------------------------

# Returns a small descriptor used downstream to decide which endpoint to call
# and which field to extract from the payload.
# canonical parameters: water_discharge, water_level, water_temperature,
# (water_temperature_profile, water_velocity, runoff) -> not supported (NULL)
.lt_param_map <- function(parameter) {
  # Map canonical parameter -> observedProperty + default fallback unit
  # (unitName from the chosen measure will override when present)
  switch(
    parameter,
    water_discharge = list(observedProp = "waterDischarge", unit = "m^3/s"),
    water_level = list(observedProp = "waterLevel", unit = "cm"),
    rlang::abort("LT_LHMT supports 'water_discharge', 'water_level.")
  )
}
#' @export
# Stations --------------------------------------------------------------------
stations.hydro_service_LT_LHMT <- function(x, ...) {
  STATIONS_PATH <- "/hydro-stations"

  limited <- function() {
    # --- request -----------------------------------------------------------
    req  <- build_request(x, path = STATIONS_PATH)
    resp <- perform_request(req)
    dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # Robust wrapper: accept {results=[...]} or bare array
    if (is.list(dat) && !is.null(dat$results)) dat <- dat$results
    df <- tibble::as_tibble(dat)
    if (nrow(df) == 0) {
      return(tibble::tibble(
        country            = x$country,
        provider_id        = x$provider_id,
        provider_name      = x$provider_name,
        station_id         = character(),
        station_name       = character(),
        station_name_ascii = character(),
        river              = character(),
        river_ascii        = character(),
        lat                = numeric(),
        lon                = numeric(),
        area               = numeric(),
        altitude           = numeric()
      ))
    }
    n  <- nrow(df)

    # --- columns -----------------------------------------------------------
    code   <- col_or_null(df, "code") %||% col_or_null(df, "stationId") %||% col_or_null(df, "id")
    name0  <- normalize_utf8(col_or_null(df, "name"))
    river0 <- normalize_utf8(col_or_null(df, "waterBody"))
    desc0  <- normalize_utf8(col_or_null(df, "description"))
    alt0   <- col_or_null(df, "altitude") %||%
      col_or_null(df, "elevation") %||%
      col_or_null(df, "height")    %||% NA_character_

    # --- coordinates -------------------------------------------------------
    # LHMT returns coordinates as latitude/longitude (WGS84, EPSG:4326)
    coords <- col_or_null(df, "coordinates")
    lat0 <- NA_real_; lon0 <- NA_real_
    if (is.data.frame(coords)) {
      lat0 <- suppressWarnings(as.numeric(coords[["latitude"]]))
      lon0 <- suppressWarnings(as.numeric(coords[["longitude"]]))
    } else if (is.list(coords)) {
      lat0 <- suppressWarnings(as.numeric(vapply(coords, function(z) if (is.null(z)) NA_real_ else z[["latitude"]] %||% NA_real_, 0.0)))
      lon0 <- suppressWarnings(as.numeric(vapply(coords, function(z) if (is.null(z)) NA_real_ else z[["longitude"]] %||% NA_real_, 0.0)))
    }

    lon <- suppressWarnings(as.numeric(lon0))
    lat <- suppressWarnings(as.numeric(lat0))

    # --- output schema -----------------------------------------------------
    tibble::tibble(
      country            = x$country,
      provider_id        = x$provider_id,
      provider_name      = x$provider_name,
      station_id         = as.character(code),
      station_name       = as.character(name0),
      station_name_ascii = to_ascii(name0),
      river              = as.character(river0),
      river_ascii        = to_ascii(river0),
      lat                = lat,
      lon                = lon,
      area               = parse_area_km2(desc0),
      altitude           = suppressWarnings(as.numeric(alt0))
    )
  }
  limited()
}

#' @export
timeseries.hydro_service_LT_LHMT <- function(x,
                                             parameter = c("water_discharge","water_level"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter, c("water_discharge","water_level"))
  mode      <- match.arg(mode, c("complete","range"))

  pm <- .lt_param_map(parameter)
  param_field <- pm$observedProp
  param_unit  <- pm$unit

  # Station list
  ids <- stations %||% character()
  if (!length(ids)) {
    st <- stations.hydro_service_LT_LHMT(x)
    ids <- st$station_id
  }
  ids <- unique(stats::na.omit(as.character(ids)))
  if (!length(ids)) {
    return(tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = character(),
      parameter     = character(),
      timestamp     = as.POSIXct(character()),
      value         = numeric(),
      unit          = character(),
      quality_code  = character(),
      quality_name  = character(),
      quality_desc  = character(),
      source_url    = character()
    ))
  }

  # Requested window (client-side)
  rng <- resolve_dates(mode, start_date, end_date)
  req_from <- as.Date(rng$start_date %||% "2000-01-01")
  req_to   <- as.Date(rng$end_date   %||% Sys.Date())

  # Helpers -------------------------------------------------------------------
  month_seq <- function(a, b) {
    if (is.na(a) || is.na(b) || b < a) return(character(0))
    a0 <- as.Date(format(a, "%Y-%m-01"))
    b0 <- as.Date(format(b, "%Y-%m-01"))
    format(seq(a0, b0, by = "1 month"), "%Y-%m")
  }
  day_seq <- function(a, b) {
    if (is.na(a) || is.na(b) || b < a) return(character(0))
    format(seq(as.Date(a), as.Date(b), by = "1 day"), "%Y-%m-%d")
  }

  # Low-level fetchers & probe -------------------------------------------------
  probe_range <- function(st_id, type = c("historical","measured")) {
    type <- match.arg(type)
    path <- paste0("/hydro-stations/", utils::URLencode(st_id, reserved = TRUE),
                   "/observations/", type)
    req  <- build_request(x, path = path)
    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(NULL)
    dat  <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat)) return(NULL)
    odr  <- dat$observationsDataRange
    if (is.null(odr)) return(NULL)

    s <- odr$startDateUtc %||% odr$startTimeUtc
    e <- odr$endDateUtc   %||% odr$endTimeUtc
    if (is.null(s) || is.null(e)) return(NULL)
    list(
      start = as.Date(substr(s, 1, 10)),
      end   = as.Date(substr(e, 1, 10)),
      url   = paste0(x$base_url, sub("^/", "", path))
    )
  }

  fetch_hist_month <- function(st_id, ym) {
    path <- paste0("/hydro-stations/", utils::URLencode(st_id, reserved = TRUE),
                   "/observations/historical/", ym)
    req  <- build_request(x, path = path)
    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(NULL)
    if (httr2::resp_status(resp) == 404) return(NULL)
    dat <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat)) return(NULL)
    list(obs = dat$observations %||% dat$results %||% dat,
         url = paste0(x$base_url, sub("^/", "", path)))
  }

  fetch_measured_day <- function(st_id, ymd) {
    path <- paste0("/hydro-stations/", utils::URLencode(st_id, reserved = TRUE),
                   "/observations/measured/", ymd)
    req  <- build_request(x, path = path)
    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(NULL)
    if (httr2::resp_status(resp) == 404) return(NULL)
    dat <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat)) return(NULL)
    list(obs = dat$observations %||% dat$results %||% dat,
         url = paste0(x$base_url, sub("^/", "", path)))
  }

  tidy <- function(obs, type, url, st_id) {
    if (is.null(obs) || length(obs) == 0) return(NULL)
    ts_key <- if (identical(type, "measured")) "observationTimeUtc" else "observationDateUtc"

    if (is.data.frame(obs)) {
      ts  <- obs[[ts_key]]
      val <- suppressWarnings(as.numeric(obs[[param_field]]))
    } else {
      ts  <- vapply(obs, function(z) col_or_null(z, ts_key) %||% NA_character_, character(1))
      val <- vapply(obs, function(z) suppressWarnings(as.numeric(col_or_null(z, param_field) %||% NA_real_)), numeric(1))
    }

    keep <- is.finite(val) & !is.na(ts)
    if (!any(keep)) return(NULL)

    tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = st_id,
      parameter     = parameter,
      timestamp     = as.POSIXct(ts[keep], tz = "UTC"),
      value         = val[keep],
      unit          = param_unit,
      quality_code  = NA_character_,
      quality_name  = NA_character_,
      quality_desc  = NA_character_,
      source_url    = url
    )
  }

  # Rate-limit wrappers (correct usage) ---------------------------------------
  .rate <- ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  limited_probe_range        <- ratelimitr::limit_rate(probe_range,        rate = .rate)
  limited_fetch_hist_month   <- ratelimitr::limit_rate(fetch_hist_month,   rate = .rate)
  limited_fetch_measured_day <- ratelimitr::limit_rate(fetch_measured_day, rate = .rate)

  # Batching ------------------------------------------------------------------
  chunks <- chunk_vec(ids, 50L)

  # First pass: probe ranges to plan requests + compute progress total --------
  hist_months_by_station <- vector("list", length(ids)); names(hist_months_by_station) <- ids
  meas_days_by_station   <- vector("list", length(ids)); names(meas_days_by_station)   <- ids
  total_steps <- 0L

  for (chunk in chunks) {
    for (st_id in chunk) {
      # historical range (always relevant; discharge only lives here)
      hr <- limited_probe_range(st_id, "historical")
      if (!is.null(hr)) {
        h_from <- max(req_from, hr$start)
        h_to   <- min(req_to,   hr$end)
        months <- if (!is.na(h_from) && !is.na(h_to) && h_to >= h_from) month_seq(h_from, h_to) else character(0)
      } else months <- character(0)
      hist_months_by_station[[st_id]] <- months
      total_steps <- total_steps + length(months)

      # measured range (only for water_level)
      if (identical(pm$observedProp, "waterLevel")) {
        mr <- limited_probe_range(st_id, "measured")
        if (!is.null(mr)) {
          m_from <- max(req_from, mr$start)
          m_to   <- min(req_to,   mr$end)
          days <- if (!is.na(m_from) && !is.na(m_to) && m_to >= m_from) day_seq(m_from, m_to) else character(0)
        } else days <- character(0)
        meas_days_by_station[[st_id]] <- days
        total_steps <- total_steps + length(days)
      }
    }
  }

  # Progress bar --------------------------------------------------------------
  if (total_steps <= 0L) total_steps <- 1L
  pb <- progress::progress_bar$new(
    total = total_steps,
    format = "LT_LHMT progress: [:bar] :current/:total (:percent) eta: :eta",
    clear = FALSE, width = 80
  )

  # Second pass: fetch --------------------------------------------------------
  results <- list()

  for (chunk in chunks) {
    for (st_id in chunk) {
      # historical monthly
      months <- hist_months_by_station[[st_id]]
      if (length(months)) {
        for (ym in months) {
          got <- limited_fetch_hist_month(st_id, ym)
          if (!is.null(got)) {
            dd <- tidy(got$obs, "historical", got$url, st_id)
            if (!is.null(dd)) results[[length(results) + 1L]] <- dd
          }
          pb$tick()
        }
      }

      # measured daily (only for water_level)
      if (identical(pm$observedProp, "waterLevel")) {
        days <- meas_days_by_station[[st_id]]
        if (length(days)) {
          for (d in days) {
            got <- limited_fetch_measured_day(st_id, d)
            if (!is.null(got)) {
              dd <- tidy(got$obs, "measured", got$url, st_id)
              if (!is.null(dd)) results[[length(results) + 1L]] <- dd
            }
            pb$tick()
          }
        }
      }
    }
  }

  if (!length(results)) {
    return(tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = character(),
      parameter     = character(),
      timestamp     = as.POSIXct(character()),
      value         = numeric(),
      unit          = character(),
      quality_code  = character(),
      quality_name  = character(),
      quality_desc  = character(),
      source_url    = character()
    ))
  }

  out <- dplyr::bind_rows(results)

  # Client-side range filter
  if (mode == "range") {
    from_dt <- as.POSIXct(req_from, tz = "UTC")
    to_dt   <- as.POSIXct(req_to,   tz = "UTC") + 86399
    out <- dplyr::filter(out, !is.na(timestamp), timestamp >= from_dt, timestamp <= to_dt)
  }

  # Deduplicate if overlaps exist (measured vs historical)
  out |>
    dplyr::arrange(station_id, parameter, timestamp, dplyr::desc(source_url)) |>
    dplyr::distinct(station_id, parameter, timestamp, .keep_all = TRUE) |>
    dplyr::arrange(station_id, parameter, timestamp)
}


