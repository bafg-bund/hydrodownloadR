# ==== Sweden (SMHI HydroObs) adapter =========================================
# Base: "https://opendata-download-hydroobs.smhi.se"
# Optional GeoJSON for coordinates: none (coords are provided as lon/lat WGS84)

register_SE_SMHI <- function() {
  register_service_usage(
    provider_id   = "SE_SMHI",
    provider_name = "Swedish Meteorological and Hydrological Institute (SMHI)",
    country       = "Sweden",
    base_url      = "https://opendata-download-hydroobs.smhi.se",   # TODO: confirm base
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_SE_SMHI <- function(x, ...) {
  c("water_discharge","water_level","water_temperature")
}

# Parameter mapping
# -----------------------------------------------------------------------------
.se_param_map <- function(parameter) {
  switch(parameter,
         water_discharge = list(ts_path = "/api/version/latest/parameter/1/station/",
                                unit = "m^3/s"),
         water_level = list(ts_path = "/api/version/latest/parameter/3/station/",
                                unit = "cm"),
         water_temperature = list(ts_path = "/api/version/latest/parameter/4/station/",
                                unit = "\u00B0C"),
         rlang::abort("FI_SYKE supports 'water_discharge', 'water_level', 'water_temperature'.")

  )
}

# -----------------------------------------------------------------------------
# Stations (S3)
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_SE_SMHI <- function(x, ..., include_params = c(1L, 3L, 4L)) {
  # By default, union stations from discharge(1), water level(3), water temperature(4)
  measuringStations <- (list(...)$measuringStations %||% "CORE")

  STATIONS_PATH_TMPL <- "/api/version/latest/parameter/%d.json"

  fetch_one <- ratelimitr::limit_rate(
    function(param_id) {
      path <- sprintf(STATIONS_PATH_TMPL, as.integer(param_id))
      req  <- build_request(x, path = path, query = list(measuringStations = measuringStations))
      resp <- perform_request(req)

      dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (is.null(dat) || length(dat) == 0L) return(tibble::tibble())

      # Robust extraction: payload may be list with 'station'
      raw <- if (!is.null(dat$station)) dat$station else dat
      df  <- tryCatch(tibble::as_tibble(raw), error = function(e) tibble::tibble())
      # --- filter: keep only stations that have an 'updated' timestamp ----------
      if ("updated" %in% names(df)) {
        na_idx <- is.na(df$updated)
        if (any(na_idx)) {
          bad_ids <- head(df$key[na_idx] %||% df$id[na_idx], 5)
          rlang::warn(paste0(
            "SE_SMHI: Skipped ", sum(na_idx), " station(s) without entries in the column 'updated',
            to prevent errors while retrieving time series data. ",
            "Examples: ", paste(bad_ids, collapse = ", "),
            if (sum(na_idx) > 5) paste0(" ... (+", sum(na_idx) - 5, " more)") else ""
          ))
          df <- df[!na_idx, , drop = FALSE]
        }
      }
      df
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  # Fetch & union for requested parameter ids
  lst <- lapply(include_params, fetch_one)
  df  <- suppressWarnings(dplyr::bind_rows(lst))
  if (!nrow(df)) {
    return(tibble::tibble(
      country            = character(),
      provider_id        = character(),
      provider_name      = character(),
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

  # --- columns (same logic as your template) ---------------------------------
  code  <- col_or_null(df, "key")
  if (is.null(code)) code <- col_or_null(df, "id")

  name  <- col_or_null(df, "name")
  name  <- normalize_utf8(name)

  river <- col_or_null(df, "river") %||% NA_character_
  lat   <- col_or_null(df, "latitude") %||% col_or_null(df, "lat")
  lon   <- col_or_null(df, "longitude") %||% col_or_null(df, "lon")

  alt   <- col_or_null(df, "altitude") %||%
    col_or_null(df, "elevation") %||%
    col_or_null(df, "height") %||% NA_character_
  alt_num <- as.numeric(alt)

  area  <- col_or_null(df, "catchmentSize") %||%
    col_or_null(df, "area") %||% NA_character_
  area_num <- as.numeric(area)

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = as.character(code),
    station_name       = as.character(name),
    station_name_ascii = to_ascii(name),
    river              = as.character(river),
    river_ascii        = to_ascii(river),
    lat                = lat,
    lon                = lon,
    area               = area_num,
    altitude           = alt_num
  )

  # Deduplicate by station_id (keep first)
  out <- out[!duplicated(out$station_id), , drop = FALSE]
  out
}


#' @export
timeseries.hydro_service_SE_SMHI <- function(x,
                                             parameter = c("water_discharge","water_level","water_temperature"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  pm <- .se_param_map(parameter)

  # Full-day UTC window (RFC3339 "without seconds")
  rng <- resolve_dates(mode, start_date, end_date)
  date_queries <- list(
    from = paste0(format(rng$start_date, "%Y-%m-%d"), "T00:00Z"),
    to   = paste0(format(rng$end_date,   "%Y-%m-%d"), "T23:59Z")
  )

  # Determine parameter id from pm$ts_path (e.g., "/parameter/3/station/")
  param_id <- as.integer(sub("^.*/parameter/([0-9]+)/.*$", "\\1", pm$ts_path))

  # Station IDs
  ids <- stations %||% character()
  if (length(ids) == 0L) {
    # No stations provided -> use parameter-specific catalog
    st_param <- stations.hydro_service_SE_SMHI(x, include_params = param_id, ...)
    ids <- st_param$station_id
  } else {
    # Validate user-provided IDs against parameter-specific catalog
    st_param <- stations.hydro_service_SE_SMHI(x, include_params = param_id, ...)
    allowed  <- unique(as.character(st_param$station_id))
    user_ids <- unique(as.character(ids))

    invalid  <- setdiff(user_ids, allowed)
    ids      <- intersect(user_ids, allowed)

    if (length(invalid)) {
      msg <- paste0(
        "SE_SMHI: ", length(invalid), " station id(s) not available for parameter '", parameter, "'. ",
        "Examples: ", paste(utils::head(invalid, 5), collapse = ", "),
        if (length(invalid) > 5) paste0(" ... (+", length(invalid) - 5, " more)") else ""
      )
      rlang::warn(msg)
    }
  }

  ids <- unique(as.character(ids))
  if (!length(ids)) {
    return(tibble::tibble(
      country            = character(),
      provider_id        = character(),
      provider_name      = character(),
      station_id         = character(),
      parameter          = character(),
      timestamp          = as.POSIXct(character()),
      value              = numeric(),
      unit               = character(),
      quality_code       = character(),
      qf_desc            = character(),
      source_url         = character(),
      value_datum        = numeric(),
      value_datum_unit   = character(),
      vertical_datum     = character()
    ))
  }

  # Inline mapping for SMHI quality flags -> description
  qf_map_desc <- c(
    G = "Checked and approved values.",
    Y = "Roughly checked / suspect / aggregated values.",
    O = "Unchecked values."
  )

  # Batch in chunks of 10
  batches <- chunk_vec(ids, 10L)
  pb <- progress::progress_bar$new(total = length(batches))

  one_station <- ratelimitr::limit_rate(function(st_id) {
    path <- paste0(
      pm$ts_path,
      utils::URLencode(as.character(st_id), reserved = TRUE),
      "/period/corrected-archive/data.json"
    )

    req  <- build_request(x, path = path, query = date_queries)
    resp <- perform_request(req)

    status <- httr2::resp_status(resp)
    if (status == 404) return(tibble::tibble())
    if (status %in% c(401, 403)) {
      rlang::warn(paste0("SE_SMHI: access denied for station ", st_id, " (", status, ")."))
      return(tibble::tibble())
    }

    dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    if (is.null(dat) || length(dat) == 0L) return(tibble::tibble())

    # Normalize payload
    raw <- if (!is.null(dat$value)) dat$value else if (!is.null(dat$results)) dat$results else dat
    df  <- tryCatch({
      if (is.data.frame(raw)) tibble::as_tibble(raw)
      else if (is.list(raw) && length(raw) > 0L) suppressWarnings(dplyr::bind_rows(raw))
      else tibble::tibble()
    }, error = function(e) tibble::tibble())
    if (!nrow(df)) return(df)

    # Timestamp/value/quality fields
    ts_raw  <- col_or_null(df, "date") %||%
      col_or_null(df, "time") %||%
      col_or_null(df, "timestamp") %||%
      col_or_null(df, "datetime")
    val_raw <- col_or_null(df, "value") %||%
      col_or_null(df, "result") %||%
      col_or_null(df, "y") %||%
      col_or_null(df, "mean")
    qf_raw  <- col_or_null(df, "quality") %||%
      col_or_null(df, "qualityCode") %||%
      col_or_null(df, "flag")

    # Epoch milliseconds + 2h shift (your requirement)
    ts_parsed <- lubridate::as_datetime(as.numeric(ts_raw) / 1000) + lubridate::hours(2)

    keep <- !is.na(ts_parsed) &
      ts_parsed >= as.POSIXct(rng$start_date, tz = "UTC") &
      ts_parsed <= as.POSIXct(rng$end_date,   tz = "UTC") + 86399
    if (!any(keep, na.rm = TRUE)) return(tibble::tibble())

    value_raw <- suppressWarnings(as.numeric(val_raw[keep]))
    qf_chr    <- if (is.null(qf_raw)) NA_character_ else as.character(qf_raw[keep])
    qf_desc   <- unname(qf_map_desc[qf_chr])  # unmapped -> NA

    tibble::tibble(
      country            = x$country,
      provider_id        = x$provider_id,
      provider_name      = x$provider_name,
      station_id         = st_id,
      parameter          = parameter,
      timestamp          = ts_parsed[keep],
      value              = value_raw,
      unit               = pm$unit,
      quality_code       = qf_chr,
      qf_desc            = ifelse(is.na(qf_desc), NA_character_, qf_desc),
      source_url         = paste0(
        x$base_url, path, "?",
        paste0(names(date_queries), "=", unlist(date_queries), collapse = "&")
      ),
      value_datum        = NA_real_,
      value_datum_unit   = NA_character_,
      vertical_datum     = NA_character_
    )
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  out <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, one_station))
  })

  dplyr::bind_rows(out)
}
