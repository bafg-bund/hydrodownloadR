# R/adapter_BA_AVPS.R
# ==== Bosnia and Herzegovina - AVP Sava (vodostaji.voda.ba) adapter ============
# Provider: BA_AVPS
# Base URL: https://vodostaji.voda.ba
# Metadata snapshot (layer 20): data/internet/layers/20/index.json
# Scope: water_level (snapshot feed; returns latest (and previous, if present) values)
# Notes:
# - Endpoint appears to provide a station snapshot (value + timestamp + metadata_* fields).
# - This adapter treats it as a snapshot source (not a full historical archive).
# - Helpers expected from core: register_service(), build_request(), perform_request(),
#   col_or_null(), normalize_utf8(), to_ascii(), resolve_dates(), `%||%`

# ---- registration ------------------------------------------------------------

#' @keywords internal
#' @noRd
register_BA_AVPS <- function() {
  register_service(
    provider_id   = "BA_AVPS",
    provider_name = "AVP Sava (vodostaji.voda.ba)",
    country       = "Bosnia and Herzegovina",
    base_url      = "https://vodostaji.voda.ba",
    rate_cfg      = list(n = 1, period = 1),  # polite default
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_BA_AVPS <- function(x, ...) {
  c("water_level")
}

# ---- parameter mapping -------------------------------------------------------

.ba_param_map <- function(parameter) {
  switch(
    parameter,
    water_level = list(layer_id = "20", unit = "cm"),
    rlang::abort("BA_AVPS supports only 'water_level'.")
  )
}

# ---- low-level fetch + parse -------------------------------------------------

.ba_layer_index_path <- function(layer_id) {
  sprintf("data/internet/layers/%s/index.json", layer_id)
}

.ba_fetch_layer_index <- function(x, layer_id) {
  req  <- build_request(x, path = .ba_layer_index_path(layer_id), query = list())
  resp <- perform_request(req)

  txt <- httr2::resp_body_string(resp)

  dat <- tryCatch(
    jsonlite::fromJSON(txt, flatten = TRUE, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(dat)) return(tibble::tibble())

  # Usually this endpoint is a JSON array -> data.frame/tibble
  if (is.data.frame(dat)) {
    return(tibble::as_tibble(dat, .name_repair = "minimal"))
  }

  # If it is a list-of-lists, try to rbind it
  if (is.list(dat) && length(dat) && is.list(dat[[1]])) {
    return(tibble::as_tibble(jsonlite::flatten(dat), .name_repair = "minimal"))
  }

  tibble::tibble()
}

.ba_parse_area_km2 <- function(x) {
  # Handles "123 km2", "123 km^2", "123 km\u00B2", commas, etc.
  if (is.null(x)) return(NA_real_)
  z <- as.character(x)
  z[z == ""] <- NA_character_
  z <- gsub(",", ".", z, fixed = TRUE)
  z <- gsub("km\\s*\\^?2", "", z, ignore.case = TRUE)
  z <- gsub("\u00B2", "", z, fixed = TRUE)  # safe in runtime; file still ASCII
  z <- trimws(z)
  suppressWarnings(as.numeric(z))
}

.ba_to_num <- function(x) {
  z <- as.character(x)
  z[z == ""] <- NA_character_
  suppressWarnings(as.numeric(z))
}

.ba_pick_col <- function(df, candidates) {
  if (!nrow(df)) return(NULL)
  nms <- names(df)
  key <- tolower(gsub("[^a-z0-9]+", "_", nms))
  cand_key <- tolower(gsub("[^a-z0-9]+", "_", candidates))
  hit <- match(cand_key, key)
  hit <- hit[!is.na(hit)]
  if (length(hit)) nms[hit[1]] else NULL
}

.ba_parse_time <- function(x, tz_local = "Europe/Sarajevo") {
  # Returns POSIXct in UTC (internals correct; tzone set to UTC)
  if (is.null(x)) return(as.POSIXct(NA, tz = "UTC"))
  z <- as.character(x)
  z[z == ""] <- NA_character_

  toggle_utc <- function(tt) {
    if (all(is.na(tt))) return(tt)
    attr(tt, "tzone") <- "UTC"
    tt
  }

  # epoch seconds / ms
  num <- suppressWarnings(as.numeric(z))
  if (any(!is.na(num))) {
    # heuristic: ms if too large
    tt <- ifelse(num > 1e12, num / 1000, num)
    return(toggle_utc(as.POSIXct(tt, origin = "1970-01-01", tz = "UTC")))
  }

  # normalize common ISO-ish formats
  zz <- gsub("T", " ", z, fixed = TRUE)
  zz <- sub("\\..*$", "", zz)                     # drop fractional seconds
  zz <- sub("Z$", "", zz)                         # drop trailing Z
  zz <- sub(" [+-]\\d{2}:?\\d{2}$", "", zz)       # drop timezone offsets

  fmts <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%d.%m.%Y %H:%M:%S",
    "%d.%m.%Y %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M"
  )

  tt <- rep(as.POSIXct(NA, tz = "UTC"), length(zz))
  for (f in fmts) {
    cand <- suppressWarnings(as.POSIXct(zz, format = f, tz = tz_local))
    take <- is.na(tt) & !is.na(cand)
    tt[take] <- cand[take]
  }

  toggle_utc(tt)
}

# ---- stations() --------------------------------------------------------------

#' @export
stations.hydro_service_BA_AVPS <- function(x, ...) {
  pm <- .ba_param_map("water_level")
  df <- .ba_fetch_layer_index(x, pm$layer_id)
  if (!nrow(df)) {
    return(tibble::tibble(
      country = character(0), provider_id = character(0), provider_name = character(0),
      station_id = character(0), station_name = character(0), station_name_ascii = character(0),
      river = character(0), river_ascii = character(0),
      lat = numeric(0), lon = numeric(0), area = numeric(0),
      source_url = character(0)
    ))
  }

  # rename selected fields (based on your Python mapping)
  rename_map <- c(
    metadata_station_no        = "station_id",
    metadata_station_name      = "station_name",
    metadata_river_name        = "river",
    metadata_catchment_name    = "catchment",
    metadata_station_latitude  = "lat",
    metadata_station_longitude = "lon",
    metadata_CATCHMENT_SIZE    = "area_raw",
    metadata_station_elevation = "altitude_raw"
  )
  for (nm in names(rename_map)) {
    if (nm %in% names(df)) names(df)[names(df) == nm] <- rename_map[[nm]]
  }

  st_id <- col_or_null(df, "station_id")
  st_nm <- normalize_utf8(col_or_null(df, "station_name"))
  riv   <- normalize_utf8(col_or_null(df, "river"))

  lat <- .ba_to_num(col_or_null(df, "lat"))
  lon <- .ba_to_num(col_or_null(df, "lon"))

  area <- parse_area_km2(col_or_null(df, "area_raw"))
  area[!is.na(area) & abs(area - 1) < 1e-9] <- NA_real_

  alt  <- col_or_null(df, "altitude_raw") %||% NA_real_

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = as.character(st_id),
    station_name       = st_nm,
    station_name_ascii = to_ascii(st_nm),
    river              = riv,
    river_ascii        = to_ascii(riv),
    lat                = lat,  # EPSG:4326
    lon                = lon,  # EPSG:4326
    area               = area,
    altitude           = alt,
    source_url         = paste0(x$base_url, "/", .ba_layer_index_path(pm$layer_id))
  )

  dplyr::filter(out, !is.na(.data$lat), !is.na(.data$lon), !is.na(.data$station_id), nzchar(.data$station_id))
}

# ---- timeseries() ------------------------------------------------------------

.ba_empty_ts <- function(x, parameter, unit) {
  tibble::tibble(
    country        = x$country,
    provider_id    = x$provider_id,
    provider_name  = x$provider_name,
    station_id     = character(0),
    parameter      = character(0),
    timestamp      = as.POSIXct(character(0), tz = "UTC"),
    value          = numeric(0),
    unit           = character(0),
    quality_code   = character(0),
    quality_name   = character(0),
    quality_description = character(0),
    source_url     = character(0)
  )
}

#' @export
timeseries.hydro_service_BA_AVPS <- function(x,
                                             parameter = c("water_level"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete", "range"),
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .ba_param_map(parameter)

  df <- .ba_fetch_layer_index(x, pm$layer_id)
  if (!nrow(df)) return(.ba_empty_ts(x, parameter, pm$unit))

  # ensure we have station_id
  if ("metadata_station_no" %in% names(df) && !"station_id" %in% names(df)) {
    names(df)[names(df) == "metadata_station_no"] <- "station_id"
  }
  st_id <- as.character(col_or_null(df, "station_id"))
  if (is.null(st_id)) return(.ba_empty_ts(x, parameter, pm$unit))

  # optional station filter
  if (!is.null(stations) && length(stations)) {
    stations <- unique(as.character(stations))
    keep_st  <- st_id %in% stations
    df   <- df[keep_st, , drop = FALSE]
    st_id <- st_id[keep_st]
    if (!nrow(df)) return(.ba_empty_ts(x, parameter, pm$unit))
  }

  # Try to detect (last, previous) timestamp/value columns.
  # If only one pair exists, we return a single-point "timeseries".
  ts_last_col <- .ba_pick_col(df, c(
    "timestamp", "time", "datetime",
    "last_timestamp", "last_time", "last_datetime",
    "zadnje_mjerenje_datum_i_vrijeme", "zadnje_mjerenje", "last_measurement_time"
  ))
  val_last_col <- .ba_pick_col(df, c(
    "value", "water_level", "level", "vodostaj",
    "last_value", "last_level", "zadnji_vodostaj", "zadnje_vodostaj"
  ))

  ts_prev_col <- .ba_pick_col(df, c(
    "previous_timestamp", "prev_timestamp", "previous_time", "prev_time",
    "prethodno_mjerenje_datum_i_vrijeme", "previous_measurement_time"
  ))
  val_prev_col <- .ba_pick_col(df, c(
    "previous_value", "prev_value", "prethodni_vodostaj", "previous_level"
  ))

  if (is.null(ts_last_col) || is.null(val_last_col)) {
    # We cannot safely interpret values -> return empty rather than guessing wrong columns
    return(.ba_empty_ts(x, parameter, pm$unit))
  }

  make_block <- function(ts_col, val_col) {
    tt <- .ba_parse_time(df[[ts_col]])
    vv <- suppressWarnings(as.numeric(df[[val_col]]))

    ok <- !is.na(tt) & !is.na(vv)
    if (any(ok)) {
      tibble::tibble(
        station_id = st_id[ok],
        timestamp  = tt[ok],
        value      = vv[ok]
      )
    } else {
      tibble::tibble(station_id = character(0), timestamp = as.POSIXct(character(0), tz = "UTC"), value = numeric(0))
    }
  }

  a <- make_block(ts_last_col, val_last_col)
  b <- if (!is.null(ts_prev_col) && !is.null(val_prev_col)) make_block(ts_prev_col, val_prev_col) else NULL

  core <- if (is.null(b)) a else dplyr::bind_rows(a, b)
  if (!nrow(core)) return(.ba_empty_ts(x, parameter, pm$unit))

  # apply requested time window (if any)
  in_win <- !is.na(core$timestamp) & core$timestamp >= rng$start & core$timestamp <= (rng$end + 86399)
  core <- core[in_win, , drop = FALSE]
  if (!nrow(core)) return(.ba_empty_ts(x, parameter, pm$unit))

  out <- tibble::tibble(
    country        = x$country,
    provider_id    = x$provider_id,
    provider_name  = x$provider_name,
    station_id     = core$station_id,
    parameter      = parameter,
    timestamp      = core$timestamp,
    value          = core$value,
    unit           = pm$unit,
    quality_code   = NA_character_,
    quality_name   = NA_character_,
    quality_description = NA_character_,
    source_url     = paste0(x$base_url, "/", .ba_layer_index_path(pm$layer_id))
  )

  dplyr::arrange(out, .data$station_id, .data$timestamp)
}
