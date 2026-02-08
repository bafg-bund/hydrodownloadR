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

# ---- station catalogue settings ---------------------------------------------

.ba_fetch_layer_index <- function(x, layer_id) {
  req  <- build_request(x, path = .ba_layer_index_path(layer_id), query = list())
  resp <- perform_request(req)

  txt <- httr2::resp_body_string(resp)

  js <- tryCatch(
    jsonlite::fromJSON(txt, flatten = TRUE, simplifyVector = TRUE),
    error = function(e) NULL
  )

  if (is.null(js) || !is.data.frame(js) || !nrow(js)) {
    return(tibble::tibble())
  }

  tibble::as_tibble(js, .name_repair = "minimal")
}


.ba_catalog_layer_id <- "20"

.ba_layer_index_path <- function(layer_id) {
  sprintf("data/internet/layers/%s/index.json", layer_id)
}

# ---- parameter mapping (for XLSX time series, NOT for catalogue) ------------

.ba_param_map <- function(parameter) {
  switch(
    parameter,
    water_discharge    = list(var_code = "Q",  filename = "Q_1Y.xlsx",     unit = "m^3/s"),
    water_level        = list(var_code = "H",  filename = "H_1Y.xlsx",     unit = "cm"),
    water_temperature  = list(var_code = "WT", filename = "Tvode_1Y.xlsx", unit = "\u00B0C"),
    rlang::abort("BA_AVPS supports only 'water_discharge', 'water_level', 'water_temperature'.")
  )
}

#' @export
timeseries_parameters.hydro_service_BA_AVPS <- function(x, ...) {
  c("water_discharge", "water_level", "water_temperature")
}

# ---- stations() (uses ONLY the catalogue layer) -----------------------------

#' @export
stations.hydro_service_BA_AVPS <- function(x, ...) {
  df <- .ba_fetch_layer_index(x, .ba_catalog_layer_id)

  if (!nrow(df)) {
    return(tibble::tibble(
      country = character(0), provider_id = character(0), provider_name = character(0),
      station_id = character(0), station_name = character(0), station_name_ascii = character(0),
      river = character(0), river_ascii = character(0),
      lat = numeric(0), lon = numeric(0), area = numeric(0),
      source_url = character(0)
    ))
  }

  # IMPORTANT: station_id must be the gauge_id used in the XLSX URL
  st_id     <- col_or_null(df, "metadata_station_no") %||% col_or_null(df, "metadata_station_id")
  st_nm     <- normalize_utf8(col_or_null(df, "metadata_station_name"))
  riv       <- normalize_utf8(col_or_null(df, "metadata_river_name"))

  lat       <- suppressWarnings(as.numeric(col_or_null(df, "metadata_station_latitude")))
  lon       <- suppressWarnings(as.numeric(col_or_null(df, "metadata_station_longitude")))

  catch_raw <- col_or_null(df, "metadata_CATCHMENT_SIZE")
  area_km2  <- parse_area_km2(catch_raw)  # as in BE_HIC / BE_WAL

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = as.character(st_id),
    station_name       = st_nm,
    station_name_ascii = to_ascii(st_nm),
    river              = riv,
    river_ascii        = to_ascii(riv),
    lat                = lat,
    lon                = lon,
    area               = area_km2,
    source_url         = paste0(x$base_url, "/", .ba_layer_index_path(.ba_catalog_layer_id))
  )

  dplyr::filter(out, !is.na(.data$lat), !is.na(.data$lon),
                !is.na(.data$station_id), nzchar(.data$station_id))
}

# ---- timeseries helpers (download XLSX by trying groups 1..11) --------------

.ba_try_station_xlsx <- function(x, group, station_id, var_code, filename) {
  path <- sprintf("data/internet/stations/%s/%s/%s/%s", group, station_id, var_code, filename)
  req  <- build_request(x, path = path, query = list())

  resp <- tryCatch(perform_request(req), error = function(e) NULL)
  if (is.null(resp) || httr2::resp_status(resp) != 200L) return(NULL)

  raw <- httr2::resp_body_raw(resp)
  if (!length(raw)) return(NULL)

  list(
    raw   = raw,
    group = group,
    url   = paste0(x$base_url, "/", path)
  )
}

.ba_parse_timestamp <- function(x) {
  # readxl may return POSIXct/Date/numeric/or character
  if (inherits(x, "POSIXct")) {
    tt <- x
  } else if (inherits(x, "Date")) {
    tt <- as.POSIXct(x, tz = "Europe/Sarajevo")
  } else if (is.numeric(x)) {
    # Excel serial date (days since 1899-12-30)
    tt <- as.POSIXct((x - 25569) * 86400, origin = "1970-01-01", tz = "UTC")
  } else {
    tt <- suppressWarnings(lubridate::parse_date_time(
      as.character(x),
      orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy"),
      tz = "Europe/Sarajevo"
    ))
  }
  attr(tt, "tzone") <- "UTC"
  tt
}

.ba_read_station_xlsx <- function(x_raw) {
  if (!is.raw(x_raw)) x_raw <- as.raw(x_raw)

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  writeBin(x_raw, tmp, useBytes = TRUE)

  # Read only columns A:B; keep as character (metadata + data mixed)
  dat <- readxl::read_excel(
    tmp,
    sheet = 1,
    range = cellranger::cell_cols("A:B"),
    col_names = FALSE,
    guess_max = 50000
  )
  if (!nrow(dat) || ncol(dat) < 2) {
    return(tibble::tibble(
      timestamp = as.POSIXct(character(0), tz = "UTC"),
      value     = numeric(0)
    ))
  }

  names(dat) <- c("Timestamp", "Value")

  ts_chr <- as.character(dat$Timestamp)
  val_chr <- as.character(dat$Value)

  # The actual series uses Excel serial timestamps (e.g. 45694.041666...)
  ts_num  <- suppressWarnings(as.numeric(ts_chr))
  val_num <- suppressWarnings(as.numeric(gsub(",", ".", val_chr, fixed = TRUE)))

  keep <- !is.na(ts_num) & !is.na(val_num)
  if (!any(keep)) {
    return(tibble::tibble(
      timestamp = as.POSIXct(character(0), tz = "UTC"),
      value     = numeric(0)
    ))
  }

  # Excel serial (days since 1899-12-30) -> POSIXct
  ts_posix <- as.POSIXct((ts_num - 25569) * 86400, origin = "1970-01-01", tz = "UTC")

  out <- tibble::tibble(
    timestamp = ts_posix[keep],
    value     = val_num[keep]
  ) |>
    dplyr::distinct(.data$timestamp, .keep_all = TRUE) |>
    dplyr::arrange(.data$timestamp)

  out
}


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

# ---- timeseries() ------------------------------------------------------------

#' @export
timeseries.hydro_service_BA_AVPS <- function(x,
                                             parameter = c("water_discharge","water_level","water_temperature"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             frequency = c("instantaneous","daily"),
                                             station_groups = 1:11,
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  frequency <- match.arg(frequency)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .ba_param_map(parameter)

  st_all <- stations.hydro_service_BA_AVPS(x)
  st_ids <- if (is.null(stations) || !length(stations)) unique(as.character(st_all$station_id))  else unique(as.character(stations))

  if (!length(st_ids)) return(.ba_empty_ts(x, parameter, pm$unit))

  fetch_one <- function(station_id) {
    hit <- NULL
    for (g in station_groups) {
      hit <- .ba_try_station_xlsx(x, g, station_id, pm$var_code, pm$filename)
      if (!is.null(hit)) break
      Sys.sleep(0.05)
    }
    if (is.null(hit)) return(.ba_empty_ts(x, parameter, pm$unit))

    dat <- .ba_read_station_xlsx(hit$raw)
    if (!nrow(dat)) return(.ba_empty_ts(x, parameter, pm$unit))

    # date window
    keep <- dat$timestamp >= rng$start & dat$timestamp <= (rng$end + 86399)
    dat  <- dat[keep, , drop = FALSE]
    if (!nrow(dat)) return(.ba_empty_ts(x, parameter, pm$unit))

    # daily aggregate if requested
    if (frequency == "daily") {
      dat <- dat |>
        dplyr::mutate(date = as.Date(.data$timestamp)) |>
        dplyr::group_by(.data$date) |>
        dplyr::summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(timestamp = as.POSIXct(.data$date, tz = "UTC")) |>
        dplyr::select(.data$timestamp, .data$value) |>
        dplyr::arrange(.data$timestamp)
    }

    tibble::tibble(
      country        = x$country,
      provider_id    = x$provider_id,
      provider_name  = x$provider_name,
      station_id     = as.character(station_id),
      parameter      = parameter,
      timestamp      = dat$timestamp,
      value          = dat$value,
      unit           = pm$unit,
      quality_code   = NA_character_,
      quality_name   = NA_character_,
      quality_description = NA_character_,
      source_url     = hit$url
    )
  }

  pb <- progress::progress_bar$new(
    format = "BA_AVPS  [:bar] :current/:total (:percent) eta: :eta",
    total  = length(st_ids)
  )

  res <- lapply(st_ids, function(id) { pb$tick(); fetch_one(id) }) |>
    dplyr::bind_rows()

  if (!nrow(res)) return(.ba_empty_ts(x, parameter, pm$unit))
  dplyr::arrange(res, .data$station_id, .data$timestamp)
}
