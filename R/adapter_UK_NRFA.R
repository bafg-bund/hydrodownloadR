# UK - National River Flow Archive (NRFA) adapter
# Provider: UK_NRFA
# Base URL: https://nrfaapps.ceh.ac.uk

# ---- registration -----------------------------------------------------------

#' @keywords internal
#' @noRd
register_UK_NRFA <- function() {
  register_service_usage(
    provider_id   = "UK_NRFA",
    provider_name = "National River Flow Archive (NRFA)",
    country       = "United Kingdom",
    base_url      = "https://nrfaapps.ceh.ac.uk",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_UK_NRFA <- function(x, ...) {
  c("water_discharge","water_level")
}


.uk_nrfa_param_map <- function(parameter) {
  switch(
    parameter,
    water_discharge = list(
      ts_path_prefix = "/nrfa/ws/time-series?station=",
      ts_path_suffix = "&format=json-object&data-type=gdf",
      unit           = "m^3/s",
      data_type      = "gdf"
    ),
    water_level = list(
      ts_path_prefix = "/nrfa/ws/time-series?station=",
      ts_path_suffix = "&format=json-object&data-type=gauging-stage",
      unit           = "m",
      data_type      = "gauging-stage"
    ),
    rlang::abort("UK_NRFA supports 'water_discharge' and 'water_level'.")
  )
}



# ---- stations() -------------------------------------------------------------
#' @export
stations.hydro_service_UK_NRFA <- function(x, ...) {
  limited <- ratelimitr::limit_rate(
    function() {
      fields <- paste(
        c("id", "location", "river", "lat-long", "catchment-area", "station-level"),
        collapse = ","
      )

      path <- paste0(
        "/nrfa/ws/station-info?station=*&format=json-object&fields=",
        utils::URLencode(fields, reserved = TRUE)
      )

      req  <- build_request(x, path)
      resp <- perform_request(req)

      dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (is.null(dat) || !length(dat)) return(tibble::tibble())

      # Accept either dat$data or a bare list
      df <- tryCatch(
        tibble::as_tibble(dat$data %||% dat),
        error = function(e) tibble::tibble()
      )
      if (!nrow(df)) return(tibble::tibble())

      # --- lat/lon extraction + drop `lat-long$string` -----------------------
      lat <- lon <- rep(NA_real_, nrow(df))

      if ("lat-long" %in% names(df)) {
        ll <- df[["lat-long"]]

        # If it's a data.frame column with latitude/longitude/string
        if (is.data.frame(ll)) {
          lat <- suppressWarnings(as.numeric(ll$latitude))
          lon <- suppressWarnings(as.numeric(ll$longitude))
          # drop the 'string' subfield explicitly
          if ("string" %in% names(ll)) df[["lat-long"]][["string"]] <- NULL
        } else if (is.list(ll)) {
          # list-column of small lists
          lat <- suppressWarnings(purrr::map_dbl(ll, ~ as.numeric(.x$latitude %||% NA_real_)))
          lon <- suppressWarnings(purrr::map_dbl(ll, ~ as.numeric(.x$longitude %||% NA_real_)))
          # if each entry has $string, it's not carried forward anyway
        }
      } else {
        # fallback if API ever flattens fields
        lat <- suppressWarnings(as.numeric(df$latitude %||% NA_real_))
        lon <- suppressWarnings(as.numeric(df$longitude %||% NA_real_))
      }

      # --- fields & cleaning -------------------------------------------------
      code          <- as.character(df$id %||% df$station %||% df$station_id)
      station_raw   <- df$location %||% df$name %||% NA_character_
      river_raw     <- df$river %||% NA_character_
      area_num      <- suppressWarnings(as.numeric(df$`catchment-area` %||% df$catchment_area))
      alt0          <- suppressWarnings(as.numeric(df$`station-level`   %||% df$station_level))

      # normalize strings
      station_final <- normalize_utf8(station_raw)
      river_final   <- normalize_utf8(river_raw)

      # --- output schema -----------------------------------------------------
      out <- tibble::tibble(
        country            = x$country,
        provider_id        = x$provider_id,
        provider_name      = x$provider_name,
        station_id         = code,
        station_name       = as.character(station_final),
        river              = as.character(river_final),
        lat                = lat,
        lon                = lon,
        area               = area_num,
        altitude           = as.numeric(alt0)
      )

      # Deduplicate by station_id
      out <- out[!duplicated(out$station_id), , drop = FALSE]
      out
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  limited()
}


# Small helper to parse NRFA data-stream into a tidy tibble
.parse_stream <- function(obj) {
  ds <- obj[["data-stream"]] %||% obj[["data_stream"]]
  if (is.null(ds) || !length(ds)) {
    return(tibble::tibble(timestamp = as.POSIXct(character()), value = numeric(), quality_code = character()))
  }
  out_tm <- character(0); out_val <- numeric(0); out_qf <- character(0)
  i <- 1L; n <- length(ds)
  while (i <= n) {
    tok <- ds[[i]]
    if (is.character(tok)) {
      cur_date <- tok
      i <- i + 1L
      if (i > n) break
      val <- ds[[i]]
      if (is.numeric(val)) {
        out_tm  <- c(out_tm,  cur_date)
        out_val <- c(out_val, as.numeric(val))
        out_qf  <- c(out_qf,  NA_character_)
      } else if (is.list(val) && length(val) >= 1L) {
        out_tm  <- c(out_tm,  cur_date)
        out_val <- c(out_val, suppressWarnings(as.numeric(val[[1]])))
        out_qf  <- c(out_qf,  if (length(val) >= 2L) as.character(val[[2]]) else NA_character_)
      }
    }
    i <- i + 1L
  }
  ts_parsed <- as.POSIXct(out_tm, tz = "UTC")
  tibble::tibble(timestamp = ts_parsed, value = out_val, quality_code = out_qf)
}

# ---- timeseries() -----------------------------------------------------------

#' @export
timeseries.hydro_service_UK_NRFA <- function(x,
                                             parameter = c("water_discharge","water_level"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  pm <- .uk_nrfa_param_map(parameter)

  is_wl     <- identical(parameter, "water_level")
  val_datum_unit_out <- if (is_wl) "m" else NA_character_

  # Full-day UTC window
  rng <- resolve_dates(mode, start_date, end_date)
  start_iso <- format(rng$start_date, "%Y-%m-%d")
  end_iso   <- format(rng$end_date,   "%Y-%m-%d")

  # Station IDs
  ids <- stations %||% character()
  if (length(ids) == 0L) {
    st_all <- stations.hydro_service_UK_NRFA(x, ...)
    ids <- st_all$station_id
  }

  # ensure character IDs once, up-front
  ids <- unique(as.character(ids))

  # prepare scalar-able ids
  ids_in <- unique(as.character(ids))
  idx    <- seq_along(ids_in)

  vdatum_map <- setNames(
    ifelse(grepl("^2\\d{5}$", ids_in), "Malin Head", "ODN"),
    ids_in
  )

  one_station <- ratelimitr::limit_rate(function(i) {
    sid <- ids_in[[i]]                 # scalar id
    sid <- as.character(sid)[1L]

    endpoint <- "/nrfa/ws/time-series"
    q <- list(
      station      = sid,
      format       = "json-object",
      `data-type`  = pm$data_type,     # "gdf" or "gauging-stage"
      `start-date` = start_iso,
      `end-date`   = end_iso,
      dates        = "true",
      flags        = "true"
    )

    req  <- build_request(x, path = endpoint, query = q)
    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(tibble::tibble())

    dat  <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    if (is.null(dat) || !length(dat)) return(tibble::tibble())

    ts_df <- .parse_stream(dat)        # your helper: returns timestamp/value/quality_code
    if (!nrow(ts_df)) return(tibble::tibble())

    # Clamp to full-day window
    keep <- !is.na(ts_df$timestamp) &
      ts_df$timestamp >= as.POSIXct(rng$start_date, tz = "UTC") &
      ts_df$timestamp <= as.POSIXct(rng$end_date,   tz = "UTC") + 86399
    ts_df <- ts_df[keep, , drop = FALSE]
    if (!nrow(ts_df)) return(tibble::tibble())

    # Optional quality filter
    if (!is.null(exclude_quality) && "quality_code" %in% names(ts_df)) {
      ts_df <- ts_df[is.na(ts_df$quality_code) | !(ts_df$quality_code %in% exclude_quality), , drop = FALSE]
      if (!nrow(ts_df)) return(tibble::tibble())
    }

    # NEW: look up vertical datum once per station (cached by .uk_vdatum_lookup)
    vdatum <- vdatum_map[[sid]]

    # Build traceable source URL
    qs <- paste0(names(q), "=", vapply(q, utils::URLencode, character(1), reserved = TRUE), collapse = "&")
    src_url <- paste0(x$base_url, endpoint, "?", qs)

    tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = sid,
      parameter     = parameter,
      timestamp     = ts_df$timestamp,
      value         = suppressWarnings(as.numeric(ts_df$value)),
      unit          = pm$unit,
      quality_code  = ts_df$quality_code %||% NA_character_,
      qf_desc       = NA_character_,  # fill if/when you map NRFA flags
      source_url    = src_url,
      value_datum       = as.numeric(NA),
      value_datum_unit  = val_datum_unit_out, # <- single value, no vector warning
      vertical_datum    = vdatum
    )
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  # batching over indices as you already do
  batches <- chunk_vec(idx, 10L)
  pb <- progress::progress_bar$new(total = length(batches))
  out <- lapply(batches, function(batch_i) {
    pb$tick()
    dplyr::bind_rows(lapply(batch_i, one_station))
  })
  dplyr::bind_rows(out)
}
