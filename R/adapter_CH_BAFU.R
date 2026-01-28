# ==== Switzerland (Existenz.ch Hydro / BAFU) adapter =========================
# Base: "https://api.existenz.ch/apiv1"
#
# Notes:
# - /hydro/daterange returns historical values (documented as "up to 32 days").
#   This adapter automatically splits longer ranges into <=31-day windows.
# - Data are 10-minute values; we aggregate to daily means (UTC), like DK_VANDA.
# - Parameters:
#     water_discharge -> prefer "flow" (m3/s), fallback "flow_ls" (l/s -> m3/s)
#     water_level     -> prefer "height_abs" (m), fallback "height" (m a.s.l.)
#
# API docs: https://api.existenz.ch/docs/apiv1#/hydro

# -- Registration -------------------------------------------------------------

#' @keywords internal
#' @noRd
register_CH_BAFU <- function() {
  register_service_usage(
    provider_id   = "CH_BAFU",
    provider_name = "Bundesamt f\u00FCr Umwelt (BAFU) Hydro API",
    country       = "Switzerland",
    base_url      = "https://api.existenz.ch/apiv1",
    geo_base_url  = NULL,
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_CH_BAFU <- function(x, ...) {
  c(
    "water_discharge",
    "water_level",
    "water_temperature",
    "ph",
    "conductivity",
    "oxygen",
    "turbidity"
  )
}


# -- Parameter mapping --------------------------------------------------------

.ch_param_map <- function(parameter) {
  switch(
    parameter,
    water_discharge = list(path = "/hydro/daterange",
                           prefer_par = "flow",
                           fallback_par = "flow_ls",  # l/s -> m3/s
                           unit = "m^3/s"),

    water_level     = list(path = "/hydro/daterange",
                           prefer_par = "height_abs",
                           fallback_par = "height",
                           unit = "m"),

    water_temperature = list(path = "/hydro/daterange",
                             prefer_par = "temperature",
                             fallback_par = NULL,
                             unit = "degC"),

    water_quality_ph = list(path = "/hydro/daterange",
                            prefer_par = "acidity",
                            fallback_par = NULL,
                            unit = "pH"),

    water_quality_conductivity = list(path = "/hydro/daterange",
                                      prefer_par = "conductivity",
                                      fallback_par = NULL,
                                      unit = "uS/cm"),

    water_quality_oxygen = list(path = "/hydro/daterange",
                                prefer_par = "oxygen",
                                fallback_par = NULL,
                                unit = "mg/l"),

    water_quality_turbidity = list(path = "/hydro/daterange",
                                   prefer_par = "turbidity",
                                   fallback_par = NULL,
                                   unit = "BSTU"),

    stop("Unsupported parameter for CH_BAFU: ", parameter)
  )
}


# -- Helpers -----------------------------------------------------------------

.ch_parse_tableish <- function(dat) {
  # Accept either:
  # - list of records  (list(list(k=v,...), ...))
  # - a data.frame
  # - a Dataset-ish structure with $rows (+ optional $columns / $column_names)
  if (is.data.frame(dat)) return(tibble::as_tibble(dat))

  if (is.list(dat) && !is.null(dat$rows) && is.list(dat$rows)) {
    cols <- dat$columns %||% dat$column_names
    if (is.null(cols)) {
      # hydro_locations datasette export order (fallback)
      cols <- c(
        "rowid", "station_id", "name", "water_body_name", "water_body_type",
        "lat", "lon", "chx", "chy", "link", "latest_values", "last_24h_values", "popup"
      )
    }
    rows <- lapply(dat$rows, function(r) {
      r <- as.list(r)
      length(r) <- length(cols)
      stats::setNames(r, cols)
    })
    return(dplyr::bind_rows(rows))
  }

  if (is.list(dat) && length(dat) && is.list(dat[[1]])) {
    return(dplyr::bind_rows(dat))
  }

  # last resort
  suppressWarnings(tibble::as_tibble(dat))
}

.ch_split_windows <- function(start_date, end_date, max_days = 31L) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  if (is.na(start_date) || is.na(end_date) || start_date > end_date) return(list())

  out <- list()
  cur <- start_date
  i <- 1L
  while (cur <= end_date) {
    nxt <- min(end_date, cur + (max_days - 1L))
    out[[i]] <- list(start = cur, end = nxt)
    cur <- nxt + 1L
    i <- i + 1L
  }
  out
}

# -- Stations (S3 method) -----------------------------------------------------

#' @export
stations.hydro_service_CH_BAFU <- function(x, ...) {
  STATIONS_PATH <- "/hydro/locations"

  limited <- ratelimitr::limit_rate(
    function() {
      req  <- build_request(x, path = STATIONS_PATH)
      resp <- perform_request(req)

      # IMPORTANT: keep nested lists as-is
      dat <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      payload <- dat$payload %||% dat

      # ---- Case A: payload is named list keyed by station id; each has $details
      if (is.list(payload) && length(payload) && !is.null(names(payload)) &&
          is.list(payload[[1]]) && !is.null(payload[[1]]$details)) {

        ids <- names(payload)

        rows <- lapply(ids, function(st_id) {
          z   <- payload[[st_id]]
          det <- z$details %||% list()

          # convert "water-body-name" -> "water_body_name" etc.
          nm <- names(det)
          if (!is.null(nm)) names(det) <- gsub("-", "_", nm, fixed = TRUE)

          name0 <- det$name %||% NA_character_
          wb    <- det$water_body_name %||% NA_character_
          lat0  <- det$lat %||% NA_real_
          lon0  <- det$lon %||% NA_real_

          name0 <- normalize_utf8(name0)
          wb    <- normalize_utf8(wb)

          tibble::tibble(
            country            = x$country,
            provider_id        = x$provider_id,
            provider_name      = x$provider_name,
            station_id         = as.character(det$id %||% st_id),
            station_name       = as.character(name0),
            station_name_ascii = to_ascii(name0),
            river              = as.character(wb),
            river_ascii        = to_ascii(wb),
            lat                = suppressWarnings(as.numeric(lat0)),
            lon                = suppressWarnings(as.numeric(lon0)),
            area               = NA_real_,
            altitude           = NA_real_
          )
        })

        return(dplyr::bind_rows(rows))
      }

      # ---- Case B: fallback to previous "table-ish" parsing
      df <- .ch_parse_tableish(payload)
      n  <- nrow(df)

      st_id <- col_or_null(df, "station_id") %||%
        col_or_null(df, "stationId") %||%
        col_or_null(df, "id")

      name0 <- col_or_null(df, "name") %||% rep(NA_character_, n)
      wb    <- col_or_null(df, "water_body_name") %||%
        col_or_null(df, "waterBodyName") %||% rep(NA_character_, n)

      lat0  <- col_or_null(df, "lat") %||% col_or_null(df, "latitude") %||% rep(NA_real_, n)
      lon0  <- col_or_null(df, "lon") %||% col_or_null(df, "longitude") %||% rep(NA_real_, n)

      name0 <- normalize_utf8(name0)
      wb    <- normalize_utf8(wb)

      tibble::tibble(
        country            = x$country,
        provider_id        = x$provider_id,
        provider_name      = x$provider_name,
        station_id         = as.character(st_id),
        station_name       = as.character(name0),
        station_name_ascii = to_ascii(name0),
        river              = as.character(wb),
        river_ascii        = to_ascii(wb),
        lat                = suppressWarnings(as.numeric(lat0)),
        lon                = suppressWarnings(as.numeric(lon0)),
        area               = NA_real_,
        altitude           = NA_real_
      )
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  limited()
}

.ch_parse_ts_payload_table <- function(payload) {
  if (!is.list(payload) || is.null(names(payload)) || !"timestamp" %in% names(payload)) {
    return(tibble::tibble())
  }

  ts_num <- suppressWarnings(as.numeric(payload[["timestamp"]]))
  if (!length(ts_num) || all(is.na(ts_num))) return(tibble::tibble())

  ts_pos <- suppressWarnings(lubridate::as_datetime(ts_num, tz = "UTC"))

  cols <- setdiff(names(payload), "timestamp")
  cols <- cols[grepl("\\|", cols)]
  if (!length(cols)) return(tibble::tibble())

  pieces <- lapply(cols, function(nm) {
    v <- payload[[nm]]
    if (!is.atomic(v) || length(v) != length(ts_pos)) return(NULL)

    sp <- strsplit(nm, "\\|", perl = TRUE)[[1]]
    st <- if (length(sp) >= 1) sp[[1]] else NA_character_
    par <- if (length(sp) >= 2) sp[[2]] else NA_character_

    tibble::tibble(
      station_id = as.character(st),
      par        = as.character(par),
      timestamp  = ts_pos,
      val        = suppressWarnings(as.numeric(v))
    )
  })

  dplyr::bind_rows(pieces)
}


# -- Time series (S3) ---------------------------------------------------------

#' @export
timeseries.hydro_service_CH_BAFU <- function(x,
                                             parameter = c(
                                               "water_discharge",
                                               "water_level",
                                               "water_temperature",
                                               "ph",
                                               "conductivity",
                                               "oxygen",
                                               "turbidity"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL, # kept for generic compatibility (ignored here)
                                             mode = c("complete", "range"),
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  # Respect package date handling, but guard "complete" for this API.
  rng <- resolve_dates(mode)

  if (mode == "complete") {
    # The public /hydro/daterange endpoint is meant for short windows; default to last 32 days.
    today_utc <- as.Date(as.POSIXct(Sys.time(), tz = "UTC"))
    rng$end_date   <- today_utc
    rng$start_date <- today_utc - 31L
    rlang::warn("CH_BAFU: mode='complete' without dates -> using last 32 days (UTC).")
  }

  pm <- .ch_param_map(parameter)

  # ---- helpers --------------------------------------------------------------

  split_windows <- function(start_date, end_date, max_days = 31L) {
    start_date <- as.Date(start_date); end_date <- as.Date(end_date)
    if (is.na(start_date) || is.na(end_date) || start_date > end_date) return(list())
    out <- list(); cur <- start_date; i <- 1L
    while (cur <= end_date) {
      nxt <- min(end_date, cur + (max_days - 1L))
      out[[i]] <- list(start = cur, end = nxt)
      cur <- nxt + 1L
      i <- i + 1L
    }
    out
  }

  .to_vec <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) x <- unlist(x, recursive = TRUE, use.names = FALSE)
    x
  }

  # payload is "table columns":
  #   payload$timestamp = list/int vector (epoch seconds)
  #   payload$"2009|flow" = list/num vector
  parse_payload_table_cols <- function(payload) {
    if (!is.list(payload) || is.null(names(payload)) || !"timestamp" %in% names(payload)) {
      return(tibble::tibble())
    }

    ts_raw <- .to_vec(payload[["timestamp"]])
    ts_num <- suppressWarnings(as.numeric(ts_raw))
    if (!length(ts_num) || all(is.na(ts_num))) return(tibble::tibble())

    ts_pos <- suppressWarnings(lubridate::as_datetime(ts_num, tz = "UTC"))

    cols <- setdiff(names(payload), "timestamp")
    cols <- cols[grepl("\\|", cols)]
    if (!length(cols)) return(tibble::tibble())

    split_keys <- strsplit(cols, "\\|", perl = TRUE)
    st_vec  <- vapply(split_keys, function(z) if (length(z) >= 1) z[[1]] else NA_character_, character(1))
    par_vec <- vapply(split_keys, function(z) if (length(z) >= 2) z[[2]] else NA_character_, character(1))

    pieces <- lapply(seq_along(cols), function(j) {
      v_raw <- .to_vec(payload[[ cols[[j]] ]])
      v_num <- suppressWarnings(as.numeric(v_raw))
      if (!length(v_num) || length(v_num) != length(ts_pos)) return(NULL)

      tibble::tibble(
        station_id = as.character(st_vec[[j]]),
        par        = as.character(par_vec[[j]]),
        timestamp  = ts_pos,
        val        = v_num
      )
    })

    dplyr::bind_rows(pieces)
  }

  # payload is list of per-timestamp rows:
  #   payload[[i]]$timestamp
  #   payload[[i]]$"2009|flow"
  parse_payload_rows_wide <- function(payload) {
    if (!is.list(payload) || !length(payload) || !is.list(payload[[1]])) return(tibble::tibble())
    r1 <- payload[[1]]
    if (is.null(names(r1)) || !"timestamp" %in% names(r1)) return(tibble::tibble())

    keys <- setdiff(names(r1), "timestamp")
    keys <- keys[grepl("\\|", keys)]
    if (!length(keys)) return(tibble::tibble())

    split_keys <- strsplit(keys, "\\|", perl = TRUE)
    st_keys  <- vapply(split_keys, function(z) if (length(z) >= 1) z[[1]] else NA_character_, character(1))
    par_keys <- vapply(split_keys, function(z) if (length(z) >= 2) z[[2]] else NA_character_, character(1))

    n_rows <- length(payload)
    n_keys <- length(keys)
    total  <- n_rows * n_keys

    station_id <- rep(st_keys, times = n_rows)
    par        <- rep(par_keys, times = n_rows)
    ts_num     <- numeric(total)
    val        <- numeric(total)

    idx <- 1L
    for (i in seq_len(n_rows)) {
      row <- payload[[i]]
      ts_i <- suppressWarnings(as.numeric(row[["timestamp"]]))
      for (j in seq_len(n_keys)) {
        ts_num[[idx]] <- ts_i
        v <- row[[ keys[[j]] ]]
        val[[idx]] <- suppressWarnings(as.numeric(if (is.null(v)) NA_real_ else v))
        idx <- idx + 1L
      }
    }

    tibble::tibble(
      station_id = as.character(station_id),
      par        = as.character(par),
      timestamp  = suppressWarnings(lubridate::as_datetime(ts_num, tz = "UTC")),
      val        = val
    )
  }

  # payload is already long-ish records with loc/par/val/timestamp
  parse_payload_long_records <- function(payload) {
    df <- tryCatch({
      if (is.data.frame(payload)) tibble::as_tibble(payload)
      else if (is.list(payload) && length(payload) && is.list(payload[[1]])) dplyr::bind_rows(payload)
      else tibble::tibble()
    }, error = function(e) tibble::tibble())
    if (!nrow(df)) return(tibble::tibble())

    ts_raw  <- col_or_null(df, "timestamp") %||% col_or_null(df, "time")
    loc_raw <- col_or_null(df, "loc") %||% col_or_null(df, "location") %||% col_or_null(df, "station_id")
    par_raw <- col_or_null(df, "par") %||% col_or_null(df, "parameter") %||% col_or_null(df, "name")
    val_raw <- col_or_null(df, "val") %||% col_or_null(df, "value")

    ts_num <- suppressWarnings(as.numeric(ts_raw))
    tibble::tibble(
      station_id = as.character(loc_raw),
      par        = as.character(par_raw),
      timestamp  = suppressWarnings(lubridate::as_datetime(ts_num, tz = "UTC")),
      val        = suppressWarnings(as.numeric(val_raw))
    )
  }

  parse_any_payload <- function(payload) {
    if (is.null(payload) || !length(payload)) return(tibble::tibble())

    # Case A: table/cols (most common for this API)
    if (is.list(payload) &&
        !is.null(names(payload)) &&
        "timestamp" %in% names(payload) &&
        any(grepl("\\|", setdiff(names(payload), "timestamp")))) {
      return(parse_payload_table_cols(payload))
    }

    # Case B: list of rows (timestamp + "<loc>|<par>" keys)
    if (is.list(payload) && length(payload) && is.list(payload[[1]]) &&
        !is.null(names(payload[[1]])) &&
        "timestamp" %in% names(payload[[1]]) &&
        any(grepl("\\|", setdiff(names(payload[[1]]), "timestamp")))) {
      return(parse_payload_rows_wide(payload))
    }

    # Case C: long records
    parse_payload_long_records(payload)
  }

  # ---- station ids ----------------------------------------------------------
  ids <- stations %||% character()
  if (!length(ids)) {
    st  <- stations.hydro_service_CH_BAFU(x)
    ids <- st$station_id
  }
  ids <- unique(as.character(ids))
  ids <- ids[nzchar(ids)]
  if (!length(ids)) return(tibble::tibble())

  batches <- chunk_vec(ids, 50)

  # ---- daterange windows (<=31d) -------------------------------------------
  windows <- split_windows(rng$start_date, rng$end_date, max_days = 31L)
  if (!length(windows)) return(tibble::tibble())

  # ---- API query bits -------------------------------------------------------
  TS_PATH <- pm$path
  src_url <- paste0(x$base_url, TS_PATH)

  # prefer/fallback mapping (e.g. flow + flow_ls)
  par_keep <- unique(stats::na.omit(c(pm$prefer_par, pm$fallback_par)))
  par_query <- paste(par_keep, collapse = ",")

  dots <- list(...)
  app_q <- dots$app %||% NULL
  ver_q <- dots$version %||% NULL

  # IMPORTANT: your observed payload matches "table" output -> force table
  tsformat <- dots$timeseriesformat %||% "table"

  fetch_one <- ratelimitr::limit_rate(
    function(loc_batch, win) {
      q <- list(
        locations = paste(loc_batch, collapse = ","),
        parameters = par_query,
        startdate = paste0(format(win$start, "%Y-%m-%d"), " 00:00:00"),
        enddate   = paste0(format(win$end,   "%Y-%m-%d"), " 23:59:59"),
        timeseriesformat = tsformat
      )
      if (!is.null(app_q)) q$app <- app_q
      if (!is.null(ver_q)) q$version <- ver_q

      req  <- build_request(x, path = TS_PATH, query = q)
      resp <- perform_request(req)

      status <- httr2::resp_status(resp)
      if (status == 404) return(tibble::tibble())
      if (status %in% c(401, 403)) {
        rlang::warn(paste0("CH_BAFU: access denied (", status, ")."))
        return(tibble::tibble())
      }

      dat <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      payload <- dat$payload %||% dat
      if (is.null(payload) || !length(payload)) return(tibble::tibble())

      d <- parse_any_payload(payload)
      if (!nrow(d)) return(tibble::tibble())

      # ---- window + parameter filter (robust UTC bounds) --------------------
      t0 <- as.POSIXct(paste0(format(win$start, "%Y-%m-%d"), " 00:00:00"), tz = "UTC")
      t1 <- as.POSIXct(paste0(format(win$end,   "%Y-%m-%d"), " 23:59:59"), tz = "UTC")

      d <- d |>
        dplyr::filter(!is.na(.data$timestamp)) |>
        dplyr::filter(.data$timestamp >= t0 & .data$timestamp <= t1) |>
        dplyr::filter(.data$par %in% par_keep)

      if (!nrow(d)) return(tibble::tibble())

      # ---- prefer/fallback (only if both were requested) --------------------
      if (length(par_keep) > 1L) {
        d <- d |>
          dplyr::group_by(.data$station_id, .data$timestamp) |>
          dplyr::summarise(
            par = if (any(.data$par == pm$prefer_par, na.rm = TRUE)) pm$prefer_par else pm$fallback_par,
            val = {
              if (any(.data$par == pm$prefer_par, na.rm = TRUE)) {
                .data$val[match(pm$prefer_par, .data$par)]
              } else {
                .data$val[match(pm$fallback_par, .data$par)]
              }
            },
            .groups = "drop"
          )
      }

      # ---- unit conversions -------------------------------------------------
      if (parameter == "water_discharge") {
        d$val <- ifelse(d$par == "flow_ls", d$val / 1000, d$val)
      }
      unit_out <- pm$unit

      # ---- daily means (UTC) ------------------------------------------------
      d$date <- as.Date(d$timestamp, tz = "UTC")

      d |>
        dplyr::group_by(.data$station_id, .data$date) |>
        dplyr::summarise(value = mean(.data$val, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(
          country       = x$country,
          provider_id   = x$provider_id,
          provider_name = x$provider_name,
          parameter     = parameter,
          timestamp     = as.POSIXct(.data$date, tz = "UTC"),
          unit          = unit_out,
          quality_code  = NA_character_,
          source_url    = src_url
        ) |>
        dplyr::select(country, provider_id, provider_name, station_id, parameter,
                      timestamp, value, unit, quality_code, source_url)
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  pb <- progress::progress_bar$new(total = length(windows) * length(batches))

  out <- list()
  k <- 1L
  for (win in windows) {
    for (batch in batches) {
      pb$tick()
      out[[k]] <- fetch_one(batch, win)
      k <- k + 1L
    }
  }

  res <- dplyr::bind_rows(out)

  # (optional) enforce stable sorting + drop empty
  if (!nrow(res)) return(tibble::tibble())
  res <- res |>
    dplyr::arrange(.data$station_id, .data$parameter, .data$timestamp)

  res
}
