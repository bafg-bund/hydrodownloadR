# ==== Estonia (ESTMODEL) adapter ============================================
# Base: http://estmodel.envir.ee/
# Public endpoints (examples):
# - /countries
# - /countries/EE/stations
# - /stations/{code}/measurements?parameter=Q|H|T|V&type=MEAN[&dateFrom=YYYY-MM-DD&dateTo=YYYY-MM-DD]
#

# -- Registration -------------------------------------------------------------

#' @keywords internal
#' @noRd
register_EE_EST <- function() {
  register_service_usage(
    provider_id   = "EE_EST",
    provider_name = "Estonian Environment Agency - ESTMODEL API",
    country       = "Estonia",
    base_url      = "http://estmodel.envir.ee",     # API (time series & JSON metadata)
    geo_base_url  = "https://estmodel.app",         # coordinates (GeoJSON)
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_EE_EST <- function(x, ...) {
  c("water_discharge","water_level",
    "water_temperature","water_velocity")
}



# -- Parameter mapping --------------------------------------------------------

.ee_param_map <- function(parameter) {
  switch(parameter,
         water_discharge   = list(code = "Q", unit = "m^3/s"),
         water_level       = list(code = "H", unit = "m"),      # meters
         water_temperature = list(code = "T", unit = "\u00B0C"),
         water_velocity    = list(code = "V", unit = "m/s"),
         rlang::abort("EE_EST supports 'water_discharge', 'water_level', 'water_temperature', or 'water_velocity'.")
  )
}

# -- Stations (S3 method) ----------------------------------------------------

#' @export
stations.hydro_service_EE_EST <- function(x, ...) {
  geo_base <- x$geo_base_url %||% x$base_url
  geo_path <- "/countries/EE/stations.geojson"

  limited <- ratelimitr::limit_rate(
    function() {
      # --- GeoJSON for coordinates ---
      geo_req  <- httr2::request(geo_base) |>
        httr2::req_user_agent("hydrodownloadR (+https://github.com/your-org/hydrodownloadR)") |>
        httr2::req_url_path_append(geo_path)
      geo_resp <- try(perform_request(geo_req), silent = TRUE)

      use_fallback <- inherits(geo_resp, "try-error") ||
        httr2::resp_status(geo_resp) >= 400
      if (use_fallback) {
        rlang::warn("EE_EST: GeoJSON endpoint unavailable; falling back to JSON-only stations.")
        return(.ee_est_stations_json(x))  # old JSON-only path
      }

      fc <- httr2::resp_body_json(geo_resp, simplifyVector = FALSE)
      if (is.null(fc$features) || !length(fc$features)) {
        rlang::warn("EE_EST: GeoJSON features missing; falling back to JSON-only stations.")
        return(.ee_est_stations_json(x))
      }

      # Parse GeoJSON -> coords + minimal props
      feats <- fc$features
      coords_rows <- lapply(feats, function(f) {
        props <- f$properties %||% list()
        geom  <- f$geometry   %||% list()

        lon <- lat <- NA_real_
        if (identical(geom$type, "Point") && length(geom$coordinates) >= 2) {
          lon <- suppressWarnings(as.numeric(geom$coordinates[[1]]))
          lat <- suppressWarnings(as.numeric(geom$coordinates[[2]]))
        }

        code    <- props$code %||% props$id %||% props$stationId %||% NA_character_

        # sometimes present in GeoJSON, often not:
        area_g     <- props$area %||% props$countryArea %||% props$calculationArea %||% NA_character_
        alt_g      <- props$altitude %||% props$elevation %||% props$height %||% NA_character_

        tibble::tibble(
          station_id   = as.character(code),
          lon          = lon,
          lat          = lat,
          area_g       = normalize_utf8(area_g),
          altitude_g   = normalize_utf8(alt_g)
        )
      })
      coords_tbl <- dplyr::bind_rows(coords_rows)

      # --- JSON metadata (richer) from base_url ---
      meta_tbl <- .ee_est_stations_json_raw(x)  # see helper below

      # Merge: prefer GeoJSON coords, coalesce other fields
      merged <- dplyr::left_join(meta_tbl, coords_tbl, by = "station_id")

      name0   <- merged$name0_j
      river0 <- merged$river0_j
      area0   <- dplyr::coalesce(merged$area_j, merged$area_g)
      alt0    <- dplyr::coalesce(merged$altitude_j, merged$altitude_g)
      type0   <- merged$type_j
      lon     <- merged$lon
      lat     <- merged$lat

      # Split "river: station"
      has_colon <- !is.na(name0) & grepl(":", name0, fixed = TRUE)
      river_from_name   <- ifelse(has_colon, trimws(sub("^([^:]+):.*$", "\\1", name0)), NA_character_)
      station_from_name <- ifelse(has_colon, trimws(sub("^[^:]+:\\s*(.*)$", "\\1", name0)), name0)

      river_final   <- ifelse(has_colon, river_from_name, river0)
      station_final <- station_from_name

      river_ascii   <- to_ascii(river_final)
      station_ascii <- to_ascii(station_final)
      area_num  <- suppressWarnings(as.numeric(area0))
      altitude_num  <- suppressWarnings(as.numeric(alt0))

      tibble::tibble(
        country            = x$country,
        provider_id        = x$provider_id,
        provider_name      = x$provider_name,
        station_id         = merged$station_id,
        station_name       = station_final,
        station_name_ascii = station_ascii,
        river              = river_final,
        river_ascii        = river_ascii,
        lat                = lat,
        lon                = lon,
        area               = area_num,
        altitude           = altitude_num
      )
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  limited()
}

# JSON metadata (raw) from http://estmodel.envir.ee
.ee_est_stations_json_raw <- function(x) {
  path <- "/countries/EE/stations"
  req  <- build_request(x, path = path)
  resp <- perform_request(req)
  df   <- httr2::resp_body_json(resp, simplifyVector = TRUE) |> tibble::as_tibble()

  code   <- col_or_null(df, "code")       %||% col_or_null(df, "id")          %||% col_or_null(df, "stationId")   %||% NA_character_
  name0  <- col_or_null(df, "name")       %||% col_or_null(df, "label")       %||% col_or_null(df, "stationName") %||% NA_character_
  river0 <- col_or_null(df, "river")      %||% col_or_null(df, "waterbody")   %||% NA_character_
  type   <- col_or_null(df, "type")       %||% col_or_null(df, "stationType") %||% col_or_null(df, "category")    %||% NA_character_
  area0  <- col_or_null(df, "area")       %||% col_or_null(df, "countryArea") %||% col_or_null(df, "calculationArea") %||% NA_character_
  alt0   <- col_or_null(df, "altitude")   %||% col_or_null(df, "elevation")   %||% col_or_null(df, "height")      %||% NA_character_

  tibble::tibble(
    station_id   = as.character(code),
    name0_j      = normalize_utf8(name0),
    river0_j     = normalize_utf8(river0),
    type_j       = as.character(type),
    area_j       = normalize_utf8(area0),
    altitude_j   = normalize_utf8(alt0)
  )
}


# Back-compat: JSON-only stations (used if GeoJSON fully fails)
.ee_est_stations_json <- function(x) {
  meta_tbl <- .ee_est_stations_json_raw(x)
  # Build output in the same shape (no coords if API does not have them)
  has_colon <- !is.na(meta_tbl$name0_j) & grepl(":", meta_tbl$name0_j, fixed = TRUE)
  river_from_name   <- ifelse(has_colon, trimws(sub("^([^:]+):.*$", "\\1", meta_tbl$name0_j)), NA_character_)
  station_from_name <- ifelse(has_colon, trimws(sub("^[^:]+:\\s*(.*)$", "\\1", meta_tbl$name0_j)), meta_tbl$name0_j)

  river_final   <- ifelse(has_colon, river_from_name, meta_tbl$river0_j)
  station_final <- station_from_name

  tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = meta_tbl$station_id,
    station_name       = station_final,
    station_name_ascii = to_ascii(station_final),
    river              = river_final,
    river_ascii        = to_ascii(river_final),
    lat                = NA_real_,
    lon                = NA_real_,
    area               = meta_tbl$area_j,
    altitude           = suppressWarnings(as.numeric(meta_tbl$altitude_j))
  )
}


# -- Time series (S3 method) -------------------------------------------------

#' @export
timeseries.hydro_service_EE_EST <- function(x,
                                            parameter = c("water_discharge","water_level",
                                                          "water_temperature","water_velocity"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            exclude_quality = NULL,
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .ee_param_map(parameter)

  ids <- stations %||% character()
  batches <- if (length(ids)) chunk_vec(ids, 50) else list(NULL)

  pb <- progress::progress_bar$new(total = length(batches))
  out <- lapply(batches, function(batch) {
    pb$tick()

    base_query <- list(parameter = pm$code, type = "MEAN")
    date_queries <- if (mode == "range") {
      list(
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

    one_station <- ratelimitr::limit_rate(function(st_id) {
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

      # SAFE extraction (no warnings if columns are missing)
      ts_raw  <- col_or_null(df, "startDate") %||% col_or_null(df, "time") %||%
        col_or_null(df, "timestamp") %||% col_or_null(df, "date")
      val_raw <- col_or_null(df, "value")     %||% col_or_null(df, "val")  %||%
        col_or_null(df, "mean")      %||% col_or_null(df, "y")
      qf_raw  <- col_or_null(df, "qualityFlag") %||% col_or_null(df, "quality") %||%
        col_or_null(df, "flag")
      tz_raw  <- col_or_null(df, "timezone")

      # Derive a single timezone (fallback to UTC)
      tz <- "UTC"
      if (!is.null(tz_raw)) {
        tz_first <- tz_raw[which(!is.na(tz_raw))[1]]
        if (!is.na(tz_first)) tz <- tz_first
      }

      ts_parsed <- suppressWarnings(lubridate::as_datetime(ts_raw, tz = tz))

      keep <- rep(TRUE, length(ts_parsed))
      if (mode == "range") {
        keep <- !is.na(ts_parsed) &
          ts_parsed >= as.POSIXct(rng$start_date) &
          ts_parsed <= as.POSIXct(rng$end_date) + 86399
      }
      if (!is.null(exclude_quality) && !is.null(qf_raw)) {
        keep <- keep & !(qf_raw %in% exclude_quality)
      }
      if (!any(keep)) return(tibble::tibble())

      tibble::tibble(
        country       = x$country,
        provider_id   = x$provider_id,
        provider_name = x$provider_name,
        station_id    = st_id,
        parameter     = parameter,
        timestamp     = ts_parsed[keep],
        value         = suppressWarnings(as.numeric(val_raw[keep])),
        unit          = pm$unit,
        quality_code  = if (is.null(qf_raw)) NA_character_ else as.character(qf_raw[keep]),
        quality_name  = NA_character_,
        quality_desc  = NA_character_,
        source_url    = paste0(x$base_url, path)
      )
    }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

    dplyr::bind_rows(lapply(station_vec, one_station))
  })

  dplyr::bind_rows(out)
}
