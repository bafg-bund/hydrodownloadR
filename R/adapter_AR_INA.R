# ==== Argentina - INA Alerta5 adapter =======================================

# If %||% is not already defined in your package, uncomment this:
# `%||%` <- function(x, y) if (is.null(x) || (is.atomic(x) && length(x) == 0L)) y else x

# -- Service registration -----------------------------------------------------

#' @keywords internal
#' @noRd
register_AR_INA <- function() {
  register_service_usage(
    provider_id   = "AR_INA",
    provider_name = "INA Alerta5 API",
    country       = "Argentina",
    base_url      = "https://alerta.ina.gob.ar/a5",
    geo_base_url  = NULL,
    rate_cfg      = list(n = 3, period = 1),  # conservative default
    auth          = list(type = "none")       # public endpoints used
  )
}

#' @export
timeseries_parameters.hydro_service_AR_INA <- function(x, ...) {
  c("water_discharge")
}

# Optional alias if your service objects use class "hydro_service_AR"
#' @export
timeseries.hydro_service_AR <- function(x, ...) {
  timeseries.hydro_service_AR_INA(x, ...)
}

# -- Internal helpers ---------------------------------------------------------

.ar_param_map <- function(parameter) {
  # Map hydrodownloadR canonical parameters to INA variable IDs
  parameter <- match.arg(parameter, c("water_discharge"))
  switch(
    parameter,
    water_discharge = list(
      parameter        = "water_discharge",
      var_id           = 40L,          # Caudal (discharge)
      general_category = "Hydrology",
      default_unit     = "m^3/s"
    )
  )
}

.ina_safe_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x[!is.finite(x)] <- NA_real_
  x
}

# Fetch GeoJSON index of punctual series (one row per series)
.ina_fetch_series_index <- function(base_url, var_cfg) {
  # Wrap base_url into a minimal "service-like" object for build_request()
  x <- list(
    base_url = base_url,
    auth     = list(type = "none")
  )

  req <- build_request(
    x,
    path  = "obs/puntual/series",
    query = list(
      format            = "geojson",
      var_id            = var_cfg$var_id,
      GeneralCategory   = var_cfg$general_category,
      data_availability = "h"  # only series that have historical data
    )
  )

  geo <- httr2::req_perform(req) |>
    httr2::resp_body_json(simplifyVector = FALSE)

  feats <- geo$features
  if (is.null(feats) || !length(feats)) {
    return(tibble::tibble())
  }

  purrr::map_dfr(feats, function(f) {
    if (is.null(f) || !is.list(f)) return(NULL)

    props  <- f$properties %||% list()
    geom   <- f$geometry   %||% list()
    coords <- geom$coordinates %||% c(NA_real_, NA_real_)

    station_id <- props$estacion_id %||% NA_integer_
    series_id  <- props$id %||% props$series_id %||% NA_integer_

    tibble::tibble(
      station_id   = as.integer(station_id),
      station_name = as.character(props$nombre),
      river        = as.character(props$rio %||% NA_character_),
      lon          = .ina_safe_num(coords[[1]]),
      lat          = .ina_safe_num(coords[[2]]),
      series_id    = as.integer(series_id),
      proc_id      = as.integer(props$proc_id %||% NA_integer_),
      var_id       = as.integer(props$var_id %||% var_cfg$var_id),
      unit         = as.character(props$unidad %||% NA_character_)
    )
  })
}

# Fetch extra station metadata (altitude, drainage basin area) from estaciones endpoint
.ina_fetch_station_details <- function(base_url, station_id) {
  # Wrap base_url into a minimal service-like object for build_request()
  svc <- list(
    base_url = base_url,
    auth     = list(type = "none")
  )

  req <- build_request(
    svc,
    path  = sprintf("obs/puntual/estaciones/%s", station_id),
    query = list(
      format             = "json",
      get_drainage_basin = "true"
    )
  )

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) {
    return(list(altitude_m = NA_real_, area_km2 = NA_real_))
  }

  j <- tryCatch(
    httr2::resp_body_json(resp, simplifyVector = TRUE, check_type = FALSE),
    error = function(e) NULL
  )
  if (is.null(j)) {
    return(list(altitude_m = NA_real_, area_km2 = NA_real_))
  }

  ## ---- altitude -----------------------------------------------------------
  # INA seems to give altitude as "altitud" in metres.
  alt_candidates <- j$altitud
  alt_vec <- suppressWarnings(as.numeric(na.omit(unlist(alt_candidates))))
  altitude_m <- if (length(alt_vec) && is.finite(alt_vec[1])) alt_vec[1] else NA_real_

  ## ---- drainage area (m^2 -> km^2) ---------------------------------------
  area_km2 <- NA_real_

  if (!is.null(j$drainage_basin) && !is.null(j$drainage_basin$properties)) {
    area_val <- suppressWarnings(
      as.numeric(j$drainage_basin$properties$area)
    )

    if (length(area_val) && is.finite(area_val[1])) {
      # Units are "m^2" in your example; convert to km^2.
      area_km2 <- area_val[1] / 1e6
    }
  }

  list(
    altitude_m = altitude_m,
    area_km2   = area_km2
  )
}

# Download a single time series as csvless and convert to tibble(time, value)
.ina_download_timeseries <- function(x,
                                     series_id,
                                     timestart,
                                     timeend) {
  # x is the hydro_service object; build_request() handles base_url + auth
  req <- build_request(
    x,
    path  = "getObservaciones",
    query = list(
      tipo      = "puntual",
      series_id = series_id,
      timestart = timestart,
      timeend   = timeend,
      format    = "csvless",
      no_id     = "true"
    )
  )

  resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) >= 400L) {
    return(tibble::tibble())
  }

  txt <- httr2::resp_body_string(resp)
  if (!nzchar(txt) || identical(trimws(txt), "null")) {
    return(tibble::tibble())
  }

  # Important: first line is data, not header
  ts_df <- tryCatch(
    readr::read_csv(
      txt,
      col_names     = FALSE,      # <- treat first row as data
      show_col_types = FALSE,
      progress      = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(ts_df) || ncol(ts_df) < 2L) {
    return(tibble::tibble())
  }

  # For csvless discharge: col1 = start time, col2 = end time, last col = value
  time_col  <- ts_df[[1]]
  value_col <- ts_df[[ncol(ts_df)]]

  if (!inherits(time_col, "POSIXt")) {
    time_col <- as.POSIXct(as.character(time_col), tz = "UTC")
  }

  out <- tibble::tibble(
    time  = time_col,
    value = .ina_safe_num(value_col)
  )

  # Ensure chronological order
  out[order(out$time), , drop = FALSE]
}

# -- Stations method ----------------------------------------------------------
#' @export
stations.hydro_service_AR_INA <- function(x,
                                          parameter = "water_discharge",
                                          stations  = NULL,
                                          ...) {
  base_url <- x$base_url %||% "https://alerta.ina.gob.ar/a5"
  pm  <- .ar_param_map(parameter)

  series_idx <- .ina_fetch_series_index(base_url, pm)
  if (!NROW(series_idx)) {
    return(tibble::tibble())
  }

  if (!is.null(stations)) {
    stations <- as.integer(stations)
    series_idx <- series_idx[series_idx$station_id %in% stations, , drop = FALSE]
  }
  if (!NROW(series_idx)) {
    return(tibble::tibble())
  }

  # One row per station for metadata, keep associated series_id/var_id/proc_id
  meta_sta <- series_idx[!duplicated(series_idx$station_id), , drop = FALSE]

  # Fetch elevation and drainage area once per station
  details_list <- purrr::map(meta_sta$station_id,
                             ~.ina_fetch_station_details(base_url, .x))
  meta_sta$altitude_m <- vapply(details_list, function(d) d$altitude_m, numeric(1))
  meta_sta$area_km2   <- vapply(details_list, function(d) d$area_km2,   numeric(1))

  tibble::tibble(
    country              = x$country %||% "AR",
    provider_id          = x$provider_id,
    provider_name        = x$provider_name,
    station_id           = meta_sta$station_id,
    station_name         = meta_sta$station_name,
    river                = meta_sta$river,
    lat                  = meta_sta$lat,
    lon                  = meta_sta$lon,
    area                 = meta_sta$area_km2,
    altitude             = meta_sta$altitude_m,
  )
}

# -- Timeseries methods -------------------------------------------------------
#' @export
timeseries.hydro_service_AR_INA <- function(x,
                                            parameter   = "water_discharge",
                                            stations    = NULL,
                                            start_date  = NULL,
                                            end_date    = NULL,
                                            mode        = c("complete", "range"),
                                            tz          = "UTC",
                                            ...) {
  mode <- match.arg(mode)
  base_url <- x$base_url %||% "https://alerta.ina.gob.ar/a5"
  pm  <- .ar_param_map(parameter)

  series_idx <- .ina_fetch_series_index(base_url, pm)
  if (!NROW(series_idx)) {
    return(tibble::tibble())
  }

  if (!is.null(stations)) {
    stations <- as.integer(stations)
    series_idx <- series_idx[series_idx$station_id %in% stations, , drop = FALSE]
  }
  if (!NROW(series_idx)) {
    return(tibble::tibble())
  }

  # Use shared helper: for complete => 1900-01-01 .. today
  date_range <- resolve_dates(mode, start_date, end_date)
  timestart  <- format(date_range$start_date, "%Y-%m-%d")
  timeend    <- format(date_range$end_date,   "%Y-%m-%d")

  purrr::map_dfr(seq_len(nrow(series_idx)), function(i) {
    m   <- series_idx[i, , drop = FALSE]
    sid <- m$series_id
    if (is.na(sid)) return(NULL)

    ts <- .ina_download_timeseries(
      x         = x,
      series_id = sid,
      timestart = timestart,
      timeend   = timeend
    )
    if (!NROW(ts)) return(NULL)

    # ensure time column has the requested tz (default UTC)
    if (!inherits(ts$time, "POSIXt")) {
      ts$time <- as.POSIXct(as.character(ts$time), tz = tz)
    } else if (!identical(attr(ts$time, "tzone"), tz)) {
      ts$time <- as.POSIXct(format(ts$time, tz = "UTC", usetz = TRUE), tz = tz)
    }


    tibble::tibble(
      country       = x$country %||% "AR",
      provider_id   = x$provider_id,
      provider_name = x$provider_name %||% "Argentina - INA Alerta5 API",
      station_id    = m$station_id,
      parameter     = parameter,
      timestamp     = ts$time,
      value         = ts$value,
      unit          = pm$default_unit
    )
  })
}


