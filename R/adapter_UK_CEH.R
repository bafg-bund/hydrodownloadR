# UK – Centre For Ecology & Hydrology (EA Hydrology API) adapter
# Provider: UK_CEH
# Base URL: http://environment.data.gov.uk/hydrology
# Docs: https://environment.data.gov.uk/hydrology/doc/reference

# ---- registration -----------------------------------------------------------

register_UK_CEH <- function() {
  register_service(
    provider_id   = "UK_CEH",
    provider_name = "UK – Centre For Ecology & Hydrology API",
    country       = "UK",
    base_url      = "http://environment.data.gov.uk/hydrology",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_UK_CEH <- function(x, ...) {
  c("water_discharge","water_level","water_temperature","dissolved_oxygen",
    "fdom","bga","turbidity","chlorophyll","conductivity",
    "ammonium","nitrate","ph")
}

# mapper: add all requested parameters
.uk_ceh_param_map <- function(parameter) {
  # Map canonical parameter -> observedProperty + default fallback unit
  # (unitName from the chosen measure will override when present)
  switch(
    parameter,
    water_discharge = list(observedProp = "waterFlow",     unit = "m^3/s"),
    water_level     = list(observedProp = "waterLevel",    unit = "m"),
    dissolved_oxygen= list(observedProp = "dissolved-oxygen", unit = "%"), # could be % or mg/L
    fdom            = list(observedProp = "fdom",          unit = "RFU"),
    bga             = list(observedProp = "bga",           unit = "RFU"),
    turbidity       = list(observedProp = "turbidity",     unit = "NTU"),
    chlorophyll     = list(observedProp = "chlorophyll",   unit = "\u00B5g/L"),
    conductivity    = list(observedProp = "conductivity",  unit = "\u00B5S/cm"),
    temperature     = list(observedProp = "water_temperature",   unit = "°C"),
    ammonium        = list(observedProp = "ammonium",      unit = "mg/L"),
    nitrate         = list(observedProp = "nitrate",       unit = "mg/L (as N)"),
    ph              = list(observedProp = "ph",            unit = NA_character_),
    rlang::abort("UK_CEH: unsupported parameter.")
  )
}



# ---- stations() -------------------------------------------------------------
#' @export
stations.hydro_service_UK_CEH <- function(x, ...) {
  limited <- ratelimitr::limit_rate(
    function() {
      # Fetch all stations (HTML default is 100; JSON/CSV support _limit)
      path <- "/id/stations"
      query <- list(`_limit` = 20000)  # service currently has ~9k stations
      req  <- build_request(x, path, query = query)
      resp <- perform_request(req)

      dat <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      if (is.null(dat) || !length(dat)) return(tibble::tibble())

      df <- tryCatch(
        tibble::as_tibble(dat$items %||% dat),
        error = function(e) tibble::tibble()
      )
      if (!nrow(df)) return(tibble::tibble())

      # Extract / normalise fields (see docs & sample station page)
      # id/notation: station identifier
      code    <- as.character(df$notation %||% NA_character_)
      nrfaStationID    <- as.character(df$nrfaStationID %||% NA_character_)
      wiskiID <- as.character(df$wiskiID %||% NA_character_)
      name    <- normalize_utf8(df$label %||% NA_character_)
      river   <- normalize_utf8(df$riverName %||% NA_character_)
      lat     <- suppressWarnings(as.numeric(df$lat %||% NA_real_))
      lon     <- suppressWarnings(as.numeric(df$long %||% NA_real_))
      area    <- suppressWarnings(as.numeric(df$catchmentArea %||% NA_real_))
      # Altitude (datum on OS datum if present). If missing, NA.
      altitude <- suppressWarnings(as.numeric(df$datum %||% NA_real_))

      out <- tibble::tibble(
        country            = x$country,
        provider_id        = x$provider_id,
        provider_name      = x$provider_name,
        station_id         = code,
        nrfaStationID      = nrfaStationID,
        wiskiID            = wiskiID,
        station_name       = as.character(name),
        river              = as.character(river),
        lat                = lat,
        lon                = lon,
        area               = area,
        altitude           = altitude
      )

      # Deduplicate by station_id and drop empties
      out <- out[!is.na(out$station_id) & nzchar(out$station_id), , drop = FALSE]
      out <- out[!duplicated(out$station_id), , drop = FALSE]
      out
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  limited()
}

# --- JSON helpers ------------------------------------------------------------
.uk_ceh_json_req <- function(x, path, query = NULL) {
  if (!grepl("\\.json$", path)) path <- paste0(path, ".json")
  req <- build_request(x, path, query = query)
  httr2::req_headers(req, Accept = "application/json")
}
.uk_ceh_read_json <- function(resp, simplify = FALSE) {
  status <- httr2::resp_status(resp)
  ct     <- httr2::resp_content_type(resp) %||% ""
  if (status >= 400L || !grepl("json", ct, ignore.case = TRUE)) {
    msg <- try(httr2::resp_body_string(resp), silent = TRUE)
    rlang::warn(sprintf(
      "Expected JSON (status=%s, content-type=%s). Body(head): %s",
      status, ct, substr(as.character(msg), 1, 200)
    ))
    return(NULL)
  }
  httr2::resp_body_json(resp, simplifyVector = simplify)
}

# --- station prefilter by observedProperty ----------------------------------
.uk_ceh_allowed_ids <- function(x, observed_prop) {
  req  <- .uk_ceh_json_req(x, "/id/stations", list(observedProperty = observed_prop, `_limit` = 20000))
  resp <- perform_request(req)
  dat  <- .uk_ceh_read_json(resp, simplify = TRUE)
  if (is.null(dat) || !length(dat)) return(list(station = character(), wiski = character()))
  df <- tryCatch(tibble::as_tibble(dat$items %||% dat), error = function(e) tibble::tibble())
  if (!nrow(df)) return(list(station = character(), wiski = character()))
  list(
    station = as.character(df$notation %||% character()),
    wiski   = as.character(df$wiskiID %||% character())
  )
}

# --- measures lookup per station --------------------------------------------
.uk_ceh_measures_for_station <- function(x, key, st_id, observed_prop) {
  q <- list(`_limit` = 2000, observedProperty = observed_prop)
  q[[key]] <- st_id  # "station" or "station.wiskiID"
  req  <- .uk_ceh_json_req(x, "/id/measures", q)
  resp <- perform_request(req)
  dat  <- .uk_ceh_read_json(resp, simplify = TRUE)
  if (is.null(dat) || !length(dat)) return(tibble::tibble())
  df <- tryCatch(tibble::as_tibble(dat$items %||% dat), error = function(e) tibble::tibble())
  df
}

.uk_ceh_measure_prefs <- function(parameter) {
  if (parameter == "water_discharge") {
    # enforce daily mean only
    list(list(period = 86400, valueType = "mean"))
  } else {
    # prefer 15 min instantaneous, then daily max, then daily min
    list(
      list(period = 900,   valueType = "instantaneous")
    )
  }
}

.uk_ceh_pick_measure <- function(meas_df, parameter) {
  if (!nrow(meas_df)) return(NULL)

  # normalise fields
  meas_df$period    <- suppressWarnings(as.numeric(meas_df$period %||% NA_real_))
  meas_df$valueType <- tolower(as.character(meas_df$valueType %||% NA_character_))
  meas_df$notation  <- as.character(meas_df$notation %||% NA_character_)
  meas_df$unitName  <- as.character(meas_df$unitName %||% NA_character_)
  meas_df$parameter <- tolower(as.character(meas_df$parameter %||% NA_character_))

  prefs <- .uk_ceh_measure_prefs(parameter)
  if (length(prefs)) {
    for (p in prefs) {
      hit <- meas_df[
        (!is.na(meas_df$period)    & meas_df$period    == (p$period %||% meas_df$period)) &
          (!is.na(meas_df$valueType) & meas_df$valueType == (tolower(p$valueType) %||% meas_df$valueType)),
        , drop = FALSE
      ]
      if (nrow(hit)) return(hit[1, , drop = FALSE])
    }
  }

  # Fallback for non-discharge: choose the highest frequency (smallest period)
  if (parameter != "water_discharge" && any(!is.na(meas_df$period))) {
    meas_df <- meas_df[order(meas_df$period), , drop = FALSE]
    return(meas_df[1, , drop = FALSE])
  }

  # Last resort
  meas_df[1, , drop = FALSE]
}


# --- timeseries(): daily mean flow via measures/{notation}/readings ---------
#' @export
timeseries.hydro_service_UK_CEH <- function(x,
                                            parameter = c("water_discharge","water_level","dissolved_oxygen","fdom","bga",
                                                          "turbidity","chlorophyll","conductivity","water_temperature",
                                                          "ammonium","nitrate","ph"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            exclude_quality = NULL,
                                            water_level_station_cap = 10,   # hard cap for water_level only
                                            ...
) {
  # normalise parameter variants: "waterLevel" / "dissolved-oxygen"
  parameter <- gsub("([a-z])([A-Z])", "\\1_\\2", parameter)
  parameter <- tolower(gsub("-", "_", parameter))
  parameter <- match.arg(parameter)

  mode <- match.arg(mode)
  pm   <- .uk_ceh_param_map(parameter)  # observedProp + default unit

  # ---- date window ----------------------------------------------------------
  rng <- resolve_dates(mode, start_date, end_date)
  start_iso <- format(as.Date(rng$start_date), "%Y-%m-%d")
  end_iso   <- format(as.Date(rng$end_date),   "%Y-%m-%d")

  # ---- allowed stations for this observedProperty ---------------------------
  allowed <- .uk_ceh_allowed_ids(x, pm$observedProp)
  allowed_station <- unique(stats::na.omit(allowed$station))
  allowed_wiski   <- unique(stats::na.omit(allowed$wiski))

  # ---- water_level policy: stations are required & capped -------------------
  if (parameter == "water_level" && (is.null(stations) || !length(stations))) {
    rlang::abort(
      paste0(
        "Water level (15-min) data are very large. ",
        "Please supply 'stations' explicitly and iterate in batches of up to ",
        water_level_station_cap, " stations per call."
      )
    )
  }

  # ---- resolve user-specified stations (SUID or WISKI) ---------------------
  if (is.null(stations) || !length(stations)) {
    # only for non-water_level we allow defaulting to "all publishers"
    ids_in <- allowed_station
    id_key <- rep("station", length(ids_in))
  } else {
    ids_in <- unique(as.character(stations))
    is_station <- ids_in %in% allowed_station
    is_wiski   <- ids_in %in% allowed_wiski
    id_key     <- ifelse(is_station, "station",
                         ifelse(is_wiski, "station.wiskiID", NA_character_))
    bad <- ids_in[is.na(id_key)]
    if (length(bad)) {
      rlang::warn(paste0(
        "UK_CEH: ", length(bad), " identifier(s) don't publish '", pm$observedProp,
        "'. Examples: ", paste(utils::head(bad, 5L), collapse = ", ")
      ))
      keep   <- !is.na(id_key)
      ids_in <- ids_in[keep]; id_key <- id_key[keep]
    }
  }

  # ---- enforce cap for water_level -----------------------------------------
  if (parameter == "water_level" && length(ids_in) > water_level_station_cap) {
    rlang::abort(
      paste0(
        "Water level (15-min) data contain a tremendous amount of points per station. ",
        "For stability, this adapter limits each request to at most ",
        water_level_station_cap, " station(s). You supplied ", length(ids_in), ".\n\n",
        "Please split your list and iterate in batches of ", water_level_station_cap
      )
    )
  }

  # Early return if nothing left
  if (!length(ids_in)) {
    return(tibble::tibble(
      country       = character(),
      provider_id   = character(),
      provider_name = character(),
      station_id    = character(),
      parameter     = character(),
      timestamp     = as.POSIXct(character()),
      value         = numeric(),
      unit          = character(),
      quality_code  = character(),
      source_url    = character()
    ))
  }

  # ---- parser keeps 15-min stamps; falls back to midnight for daily --------
  .parse_readings <- function(items) {
    if (is.null(items) || !length(items)) {
      return(tibble::tibble(timestamp = as.POSIXct(character()),
                            value = numeric(), quality_code = character()))
    }
    to_ts_str <- function(z) {
      dt <- z$dateTime %||% z[["date-time"]] %||% z$datetime
      if (is.character(dt) && length(dt) && nzchar(dt[1])) return(dt[1])
      d  <- z$date %||% z[["end-date"]] %||% z[["start-date"]]
      if (is.character(d) && length(d) && nzchar(d[1])) return(paste0(substr(d[1],1,10),"T00:00:00"))
      NA_character_
    }
    ts_chr <- vapply(items, to_ts_str, character(1), USE.NAMES = FALSE)
    ts_chr <- sub("Z$", "", ts_chr)
    ts_chr <- sub("(T\\d{2}:\\d{2}:\\d{2})\\..*$", "\\1", ts_chr)
    ts     <- as.POSIXct(ts_chr, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

    val <- suppressWarnings(as.numeric(
      vapply(items, function(z) z$value %||% NA_real_, 0, USE.NAMES = FALSE)
    ))
    q   <- vapply(items, function(z) z$quality %||% z$qflag %||% NA_character_, "", USE.NAMES = FALSE)
    tibble::tibble(timestamp = ts, value = val, quality_code = q)
  }

  # ---- per-station worker ---------------------------------------------------
  one_station <- ratelimitr::limit_rate(function(i) {
    st_id <- ids_in[[i]]
    key   <- id_key[[i]]

    meas_df <- .uk_ceh_measures_for_station(x, key, st_id, pm$observedProp)
    if (!nrow(meas_df)) return(tibble::tibble())

    # normalise
    meas_df$valueType <- tolower(as.character(meas_df$valueType %||% NA_character_))
    meas_df$period    <- suppressWarnings(as.numeric(meas_df$period %||% NA_real_))
    meas_df$notation  <- as.character(meas_df$notation %||% NA_character_)
    meas_df$unitName  <- as.character(meas_df$unitName %||% NA_character_)

    # selection: discharge → daily mean; others → 15-min instantaneous
    if (parameter == "water_discharge") {
      sel <- meas_df[
        !is.na(meas_df$valueType) & meas_df$valueType == "mean" &
          !is.na(meas_df$period)    & meas_df$period == 86400,
        , drop = FALSE
      ]
      qm <- grep("-qualified$", sel$notation %||% character(), ignore.case = TRUE)
      if (length(qm)) sel <- sel[qm, , drop = FALSE]
    } else {
      sel <- meas_df[
        !is.na(meas_df$valueType) & meas_df$valueType == "instantaneous" &
          !is.na(meas_df$period)    & meas_df$period == 900,
        , drop = FALSE
      ]
    }

    if (!nrow(sel)) return(tibble::tibble())
    chosen <- sel[1, , drop = FALSE]
    meas_notation <- chosen$notation[[1]]
    if (!nzchar(meas_notation)) return(tibble::tibble())

    # readings
    path <- paste0("/id/measures/", utils::URLencode(meas_notation, reserved = TRUE), "/readings")
    q    <- list(`mineq-date` = start_iso, `maxeq-date` = end_iso, `_limit` = 2000000)
    req  <- .uk_ceh_json_req(x, path, q)
    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) return(tibble::tibble())

    dat <- .uk_ceh_read_json(resp, simplify = FALSE)
    if (is.null(dat) || !length(dat)) return(tibble::tibble())

    items <- dat$items %||% dat
    ts_df <- .parse_readings(items)
    if (!nrow(ts_df)) return(tibble::tibble())

    # clamp and filter
    keep <- !is.na(ts_df$timestamp) &
      ts_df$timestamp >= as.POSIXct(rng$start_date, tz = "UTC") &
      ts_df$timestamp <= as.POSIXct(rng$end_date,   tz = "UTC") + 86399
    ts_df <- ts_df[keep, , drop = FALSE]
    if (!nrow(ts_df)) return(tibble::tibble())

    if (!is.null(exclude_quality)) {
      ts_df <- ts_df[is.na(ts_df$quality_code) | !(ts_df$quality_code %in% exclude_quality), , drop = FALSE]
      if (!nrow(ts_df)) return(tibble::tibble())
    }

    src_url <- paste0(
      x$base_url, "/id/measures/",
      utils::URLencode(meas_notation, reserved = TRUE),
      "/readings.json?",
      paste0(names(q), "=", utils::URLencode(unlist(q), reserved = TRUE), collapse = "&")
    )

    unit_out <- if (!is.null(chosen$unitName) && nzchar(chosen$unitName[[1]]))
      as.character(chosen$unitName[[1]]) else (pm$unit %||% NA_character_)

    tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = st_id,
      parameter     = parameter,
      timestamp     = ts_df$timestamp,
      value         = suppressWarnings(as.numeric(ts_df$value)),
      unit          = unit_out,
      quality_code  = ts_df$quality_code %||% NA_character_,
      source_url    = src_url
    )
  }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

  # ---- batching -------------------------------------------------------------
  idx     <- seq_along(ids_in)
  batches <- chunk_vec(idx, 10L)   # I/O-friendly; cap already enforced for water_level
  pb <- progress::progress_bar$new(total = length(batches))

  out <- lapply(batches, function(b) {
    pb$tick()
    dplyr::bind_rows(lapply(b, one_station))
  })
  dplyr::bind_rows(out)
}
