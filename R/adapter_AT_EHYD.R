# R/adapter_AT_EHYD.R
# Austria - eHYD (BMLRT) Hydrometric adapter (offline CSV bundle)
# Surface water bodies daily data:
#   https://ehyd.gv.at/services/AreaSelection/download?cat=owf&reg=10
#
# Primary path: single ZIP bundle -> cached -> unzipped -> local CSV reads
# - Detect cached bundle in user cache dir (~/.cache/hydrodownloadR/AT_EHYD)
# - Download ZIP if missing
# - Unzip to stable directory (ehyd_messstellen_all_owf)
# - Stations from messstellen_owf.csv
# - Time series from Q-Tagesmittel/ & W-Tagesmittel/ (daily)
# - Provenance via source_url = file://...root

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_AT_EHYD <- function() {
  register_service_usage(
    provider_id   = "AT_EHYD",
    provider_name = "eHYD (BMLRT) CSV Bundle",
    country       = "Austria",
    base_url      = "https://ehyd.gv.at/services/AreaSelection/download?cat=owf&reg=10",
    rate_cfg      = list(n = 10, period = 1),
    auth          = list(type = "none")
  )
}

# Keep parameter list discoverable
#' @export
timeseries_parameters.hydro_service_AT_EHYD <- function(x, ...) {
  c("water_discharge",
    "water_level",
    "water_temperature_monthly_mean",
    "suspended_sediment_daily_load",
    "bedload_daily_load")
}



# -----------------------------------------------------------------------------
# Constants / parameter mapping (private)
# -----------------------------------------------------------------------------

AT_EHYD_URL          <- "https://ehyd.gv.at/services/AreaSelection/download?cat=owf&reg=10"
AT_EHYD_Q_DIR        <- "Q-Tagesmittel"
AT_EHYD_W_DIR        <- "W-Tagesmittel"
AT_EHYD_META         <- "messstellen_owf.csv"

.at_param_map_AT <- function(parameter) {
  parameter <- match.arg(
    parameter,
    c("water_discharge",
      "water_level",
      "water_temperature_monthly_mean",
      "suspended_sediment_daily_load",
      "bedload_daily_load")
  )

  if (parameter == "water_discharge") {
    return(list(
      unit     = "m^3/s",
      subdir   = AT_EHYD_Q_DIR,          # "Q-Tagesmittel"
      prefix   = "Q-Tagesmittel-",
      param    = "water_discharge",
      to_canon = function(x, raw_unit = NULL) suppressWarnings(as.numeric(x))
    ))
  }

  if (parameter == "water_level") {
    return(list(
      unit     = "cm",
      subdir   = AT_EHYD_W_DIR,          # "W-Tagesmittel"
      prefix   = "W-Tagesmittel-",
      param    = "water_level",
      to_canon = function(x, raw_unit = NULL) suppressWarnings(as.numeric(x))
    ))
  }

  if (parameter == "water_temperature_monthly_mean") {
    return(list(
      unit     = "\u00B0C",
      subdir   = "WT-Monatsmittel",      # folder name in bundle
      prefix   = "WT-Monatsmittel-",
      param    = "water_temperature_monthly_mean",
      to_canon = function(x, raw_unit = NULL) suppressWarnings(as.numeric(x))
    ))
  }

  if (parameter == "suspended_sediment_daily_load") {
    return(list(
      unit     = "t",
      subdir   = "Schwebstoff-Tagesfracht",
      prefix   = "Schwebstoff-Tagesfracht-",
      param    = "suspended_sediment_daily_load",
      to_canon = function(x, raw_unit = NULL) suppressWarnings(as.numeric(x))
    ))
  }

  # parameter == "bedload_daily_load"
  list(
    unit     = "t",
    subdir   = "Geschiebe-Tagesfracht",
    prefix   = "Geschiebe-Tagesfracht-",
    param    = "bedload_daily_load",
    to_canon = function(x, raw_unit = NULL) suppressWarnings(as.numeric(x))
  )
}

# -----------------------------------------------------------------------------
# Bundle cache & setup (private)
# -----------------------------------------------------------------------------

.at_cache_dir <- function() {
  # Root cache dir (platform default), WITHOUT appname
  root <- tryCatch(rappdirs::user_cache_dir(), error = function(e) NULL)
  if (is.null(root)) root <- file.path(tempdir(), "hydro_cache_root")

  # Base shared by all adapters
  base <- file.path(root, "hydrodownloadR")
  if (!dir.exists(base)) dir.create(base, recursive = TRUE, showWarnings = FALSE)

  # Adapter-specific dir
  dir <- file.path(base, "AT_EHYD")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  dir
}


.at_bundle_zip_path <- function(cache_dir = .at_cache_dir()) file.path(cache_dir, "ehyd_owf_bundle.zip")


.at_download_bundle <- function(cache_dir = .at_cache_dir(), force = FALSE) {
  zip_path <- .at_bundle_zip_path(cache_dir)
  if (!isTRUE(force) && file.exists(zip_path) && file.info(zip_path)$size > 0) return(zip_path)

  url <- AT_EHYD_URL
  cli::cli_inform(c(
    "i" = "Downloading eHYD CSV bundle - size can be large...",
    " " = url,
    " " = paste0("Destination: ", zip_path)
  ))
  resp <- httr2::request(url) |> httr2::req_perform()
  if (httr2::resp_status(resp) < 200 || httr2::resp_status(resp) >= 300) {
    stop(sprintf("AT_EHYD: download failed: HTTP %s for %s", httr2::resp_status(resp), url))
  }
  writeBin(httr2::resp_body_raw(resp), zip_path)
  zip_path
}

.at_ensure_unpacked <- function(force = FALSE) {
  cache_dir <- .at_cache_dir()

  if (!isTRUE(force) && dir.exists(cache_dir) && length(list.files(cache_dir, recursive = TRUE)) > 0) {
    zip_path <- .at_bundle_zip_path(cache_dir)

    age_days <- NA_real_
    if (file.exists(zip_path)) {
      info <- file.info(zip_path)
      age_days <- as.numeric(difftime(Sys.time(), info$mtime, units = "days"))
    }

    if (!is.na(age_days) && age_days > 90) {
      .inform("i", "Using cached eHYD bundle.")
      .inform("i", paste0("Cached file age: ", round(age_days), " days."))
      .inform("i", "If you want to ensure you are using the latest eHYD release, run:")
      .inform("i", 'hydrodownloadR:::.at_ensure_unpacked(force = TRUE)')
    }

    return(cache_dir)
  }

  zip_path <- .at_download_bundle(cache_dir, force = force)
  .inform("i", "Unzipping eHYD bundle...")
  .inform("i", paste0("Extracting to: ", cache_dir))
  utils::unzip(zip_path, exdir = cache_dir)

  if (!dir.exists(cache_dir)) {
    stop("AT_EHYD: unzip succeeded but root folder was not found.")
  }

  cache_dir
}
# Robust CSV reader using base read.table; preserves umlauts
.at_read_csv2 <- function(path) {
  # eHYD uses semicolon + decimal comma; encoding is typically Latin-1
  df <- tryCatch({
    utils::read.table(
      file             = path,
      header           = TRUE,
      sep              = ";",
      dec              = ",",
      quote            = "\"",
      comment.char     = "",
      stringsAsFactors = FALSE,
      fileEncoding     = "Latin1"  # a.k.a. ISO-8859-1
    )
  }, error = function(e) NULL)

  if (is.null(df)) return(df)

  # Normalize to UTF-8 for downstream processing/printing
  for (nm in names(df)) {
    if (is.character(df[[nm]])) {
      df[[nm]] <- enc2utf8(df[[nm]])
    }
  }
  df
}


# Parse "Werte:" lines (semicolon-separated) into a tibble
.at_parse_values_lines <- function(lines, pm) {
  if (!length(lines)) return(NULL)

  # Keep non-empty lines only
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(NULL)

  # Split "date; value"
  parts   <- strsplit(lines, ";", fixed = TRUE)
  dt_str  <- trimws(vapply(parts, function(x) if (length(x) >= 1) x[1] else NA_character_, FUN.VALUE = character(1)))
  val_str <- trimws(vapply(parts, function(x) if (length(x) >= 2) x[2] else NA_character_, FUN.VALUE = character(1)))

  # Parse timestamp: typical "01.01.1996 00:00:00"
  ts <- suppressWarnings(as.POSIXct(dt_str, format = "%d.%m.%Y %H:%M:%S", tz = "UTC"))
  if (all(is.na(ts))) {
    # Do not attempt other formats; keep NA (e.g., lines like "Invalid; L\u00FCcke")
    ts <- suppressWarnings(as.POSIXct(dt_str, format = "%d.%m.%Y %H:%M:%S", tz = "UTC"))  # will remain NA for "L\u00FCcke"
  }

  # Numeric with decimal comma handling; strip spaces
  val_num <- suppressWarnings(as.numeric(gsub(",", ".", gsub("\\s+", "", val_str))))

  # Optional heuristic: if parsing level and magnitudes look like cm, convert to m
  if (identical(pm$subdir, AT_EHYD_W_DIR) && isTRUE(all(!is.na(val_num)))) {
    med <- suppressWarnings(stats::median(val_num, na.rm = TRUE))
    if (is.finite(med) && med > 10) val_num <- val_num / 100
  }

  keep <- !is.na(ts) & !is.na(val_num)
  if (!any(keep)) return(NULL)

  tibble::tibble(
    timestamp = ts[keep],
    value     = pm$to_canon(val_num[keep], NULL)
  )
}

# Parse single-station daily CSV -> tibble(timestamp (POSIXct UTC), value (numeric))
.at_parse_station_csv <- function(path, pm) {
  if (!file.exists(path)) return(NULL)

  # 1) Try "Werte:" format first (robust to long headers)
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "Latin1"), error = function(e) character())
  if (length(lines)) {
    lines <- enc2utf8(lines)
    idx <- which(grepl("^\\s*Werte\\s*:", lines))
    if (length(idx)) {
      data_lines <- lines[(idx[1] + 1L):length(lines)]
      ts_tbl <- .at_parse_values_lines(data_lines, pm)
      if (!is.null(ts_tbl) && nrow(ts_tbl)) return(ts_tbl)
    }
  }

  # 2) Fallback: treat file as a regular semicolon/decimal-comma CSV table
  df <- .at_read_csv2(path)
  if (is.null(df) || !nrow(df)) return(NULL)

  nms <- names(df)
  # date col heuristics
  date_candidates  <- c("Datum", "Date", "DATUM", "Zeitpunkt", "Zeit", "time", "TAG", "Tag")
  date_col <- intersect(date_candidates, nms)
  date_col <- if (length(date_col)) date_col[1] else nms[1]

  # value col heuristics
  value_candidates <- c("Wert", "MW", "Tagesmittel", "Value", "Q", "W")
  value_col <- intersect(value_candidates, nms)
  value_col <- if (length(value_col)) value_col[1] else if (length(nms) >= 2) nms[2] else nms[1]

  # parse date -> Date (fallback POSIX then Date)
  ts <- suppressWarnings(as.Date(df[[date_col]]))
  if (all(is.na(ts))) {
    ts <- suppressWarnings(as.POSIXct(df[[date_col]], tz = "UTC"))
    ts <- as.Date(ts)
  }

  # numeric with comma decimal handling
  val_raw <- gsub(",", ".", as.character(df[[value_col]]))
  val     <- suppressWarnings(as.numeric(val_raw))

  # Optional heuristic: if parsing level and magnitudes look like cm, convert to m
  if (identical(pm$subdir, AT_EHYD_W_DIR) && isTRUE(all(!is.na(val)))) {
    med <- suppressWarnings(stats::median(val, na.rm = TRUE))
    if (is.finite(med) && med > 10) val <- val / 100
  }

  keep <- !is.na(ts) & !is.na(val)
  if (!any(keep)) return(NULL)

  tibble::tibble(
    timestamp = as.POSIXct(ts[keep], tz = "UTC"),
    value     = pm$to_canon(val[keep], NULL)
  )
}

.as_utf8 <- function(x) {
  x <- as.character(x)
  out <- tryCatch(iconv(x, from = "", to = "UTF-8", sub = "byte"),
                  error = function(e) x)
  # If iconv returns NA for some, fall back to enc2utf8 (best effort)
  bad <- is.na(out)
  if (any(bad)) out[bad] <- enc2utf8(x[bad])
  out
}


# --- Track stations with no data (per parameter) -----------------------------

.at_param_empty_rds <- function(root, parameter) {
  tag <- gsub("[^A-Za-z0-9_.-]", "_", basename(root))
  file.path(.at_cache_dir(), "tables_cache", sprintf("%s__EMPTY_%s.rds", tag, parameter))
}

.at_get_empty_stations <- function(root, parameter) {
  rds <- .at_param_empty_rds(root, parameter)
  if (file.exists(rds)) {
    ids <- tryCatch(readRDS(rds), error = function(e) character())
    ids <- ids[is.finite(match(ids, ids))]  # ensure character vector
    return(unique(as.character(ids)))
  }
  character()
}



# Transform EPSG:31287 (MGI / Austria GK West) -> EPSG:4326 (WGS84)
.at_to_wgs84_coords <- function(x_east, y_north, from_epsg = 31287L) {
  # returns list(lat = ..., lon = ...)
  if (!requireNamespace("sf", quietly = TRUE)) {
    # No sf available; return NA with a soft warning
    lat <- rep(NA_real_, length(x_east))
    lon <- rep(NA_real_, length(x_east))
    if (any(!is.na(x_east) | !is.na(y_north))) {
      rlang::warn("AT_EHYD: package 'sf' not available; cannot reproject EPSG:31287 to WGS84.")
    }
    return(list(lat = lat, lon = lon))
  }

  # Build an sf POINT layer and transform
  suppressWarnings({
    pts <- sf::st_as_sf(
      data.frame(x = as.numeric(x_east), y = as.numeric(y_north)),
      coords = c("x","y"),
      crs    = paste0("EPSG:", as.integer(from_epsg))
    )
    pts_wgs <- sf::st_transform(pts, 4326)
    xy <- sf::st_coordinates(pts_wgs)
  })

  lon <- as.numeric(xy[,1])
  lat <- as.numeric(xy[,2])
  list(lat = lat, lon = lon)
}

# standardized empty ts
.at_empty_ts <- function(x, parameter, unit, source_url = character(0)) {
  tibble::tibble(
    country       = character(0),
    provider_id   = character(0),
    provider_name = character(0),
    station_id    = character(0),
    parameter     = character(0),
    timestamp     = as.POSIXct(character(0), tz = "UTC"),
    value         = numeric(0),
    unit          = character(0),
    quality_code  = character(0),
    qf_desc       = character(0),
    source_url    = character(0)
  )
}

# ---- Whole-parameter cache (Q or W) -----------------------------------------

.at_param_cache_dir <- function() {
  d <- file.path(.at_cache_dir(), "tables_cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

.at_param_cache_rds <- function(root, parameter) {
  # Root is currently the cache dir; still include it for future-proofing
  tag <- gsub("[^A-Za-z0-9_.-]", "_", basename(root))
  file.path(.at_param_cache_dir(), sprintf("%s__ALL_%s.rds", tag, parameter))
}

# Build the full tibble for a parameter by parsing ALL station CSVs in that subdir
.at_build_param_cache <- function(root, parameter, pm) {
  subdir_dir <- file.path(.at_cache_dir(), pm$subdir)
  if (!dir.exists(subdir_dir)) return(NULL)

  files <- list.files(
    subdir_dir,
    pattern = paste0("^", pm$prefix, "[0-9]{6}\\.csv$"),
    full.names = TRUE
  )
  files <- .as_utf8(files)  # normalize paths to UTF-8 for safe regex/basename

  if (!length(files)) return(NULL)

  rds_path <- .at_param_cache_rds(root, parameter)

  # Tell the user this is a one-time build that will be cached
  if (.use_cli()) {
    cli::cli_inform(c(
      "i" = sprintf("Building %s cache (first run only, then reused)...", pm$param),
      " " = sprintf("Target cache file: %s", rds_path)
    ))
  } else {
    message(sprintf("Building %s cache (first run only, then reused)...\nTarget cache file: %s",
                    pm$subdir, rds_path))
  }

  empty_ids <- character()
  ts_list   <- vector("list", length(files))

  if (.use_cli()) {
    # Fancy progress with cli
    pb_id <- cli::cli_progress_bar(
      name   = sprintf("Parsing %s", pm$subdir),
      total  = length(files),
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} {cli::pb_eta} {cli::pb_spin}"
    )
    for (i in seq_along(files)) {
      fp  <- files[[i]]
      bn  <- .as_utf8(basename(files[[i]]))
      sid <- sub(paste0("^", pm$prefix, "([0-9]{6})\\.csv$"), "\\1", bn)
      ts  <- .at_parse_station_csv(fp, pm)
      if (is.null(ts) || !nrow(ts)) {
        empty_ids <- c(empty_ids, sid)
        ts_list[[i]] <- NULL
      } else {
        ts$station_id <- sid
        ts_list[[i]]  <- ts
      }
      cli::cli_progress_update(id = pb_id, set = i)
    }
    cli::cli_progress_done(id = pb_id)
  } else {
    # Fallback: base text progress bar
    pb <- NULL
    if (interactive()) pb <- utils::txtProgressBar(min = 0, max = length(files), style = 3)
    for (i in seq_along(files)) {
      fp  <- files[[i]]
      sid <- sub(paste0("^", pm$prefix, "([0-9]{6})\\.csv$"), "\\1", basename(fp))
      ts  <- .at_parse_station_csv(fp, pm)
      if (is.null(ts) || !nrow(ts)) {
        empty_ids <- c(empty_ids, sid)
        ts_list[[i]] <- NULL
      } else {
        ts$station_id <- sid
        ts_list[[i]]  <- ts
      }
      if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
    }
    if (!is.null(pb)) close(pb)
  }

  # Persist the list of stations that have no usable data for this parameter
  empty_ids <- unique(empty_ids)
  saveRDS(empty_ids, .at_param_empty_rds(root, parameter))

  d <- dplyr::bind_rows(ts_list)
  if (!nrow(d)) {
    # Still inform that the cache is empty but created (with empty-station info)
    if (.use_cli()) {
      cli::cli_inform(c(
        "!" = sprintf("No usable rows found for '%s'. Cache will contain 0 rows.", pm$subdir),
        " " = if (length(empty_ids))
          sprintf("%d station file(s) had no data.", length(empty_ids))
        else "No station files parsed."
      ))
    } else {
      message(sprintf("No usable rows found for '%s'. Cache will contain 0 rows.", pm$subdir))
      if (length(empty_ids)) message(sprintf("%d station file(s) had no data.", length(empty_ids)))
    }
    return(NULL)
  }

  # Save cache and report summary
  d <- d |>
    dplyr::select(timestamp, value, station_id) |>
    dplyr::arrange(.data$station_id, .data$timestamp)

  saveRDS(d, rds_path)

  if (.use_cli()) {
    cli::cli_inform(c(
      "v" = sprintf("Cached %s: %s rows, %s stations.", pm$param,
                    format(nrow(d), big.mark = ","), format(dplyr::n_distinct(d$station_id), big.mark = ",")),
      " " = sprintf("Stored at: %s", rds_path)
    ))
  } else {
    message(sprintf("Cached %s: %s rows, %s stations.\nStored at: %s",
                    pm$subdir,
                    format(nrow(d), big.mark = ","), format(dplyr::n_distinct(d$station_id), big.mark = ","),
                    rds_path))
  }

  d
}

.use_cli <- function() {
  isTRUE(l10n_info()[["UTF-8"]]) && requireNamespace("cli", quietly = TRUE)
}

.inform <- function(type = "i", msg) {
  if (.use_cli()) {
    # type: "i" info, "!" warning, "v" success
    cli::cli_inform(setNames(list(msg), type))
  } else {
    # fall back to base message()
    prefix <- switch(type, "!" = "WARN:", "v" = "OK:", "i" = "INFO:", "INFO:")
    message(sprintf("%s %s", prefix, msg))
  }
}



# Get (or build) the full-parameter cache tibble
.at_get_param_cache <- function(root, parameter, pm, force = FALSE) {
  rds <- .at_param_cache_rds(root, parameter)
  if (!isTRUE(force) && file.exists(rds)) {
    d <- readRDS(rds)
    if (is.data.frame(d)) return(d)
  }
  d <- .at_build_param_cache(root, parameter, pm)
  if (is.null(d)) return(NULL)
  saveRDS(d, rds)
  d
}


# -----------------------------------------------------------------------------
# Stations (metadata CSV)
# -----------------------------------------------------------------------------
#' @export
stations.hydro_service_AT_EHYD <- function(x, ...) {
  root <- .at_ensure_unpacked()
  meta_path <- file.path(root, AT_EHYD_META)
  if (!file.exists(meta_path)) {
    cand <- list.files(root, pattern = "(^|/)messstellen_owf\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(cand)) meta_path <- cand[[1]] else stop("AT_EHYD: metadata file 'messstellen_owf.csv' not found.")
  }

  df <- .at_read_csv2(meta_path)
  if (is.null(df) || !nrow(df)) return(tibble::tibble())

  # Raw columns from eHYD metadata
  station_id <- col_or_null(df, "hzbnr01")
  name       <- col_or_null(df, "mstnam02")
  river      <- col_or_null(df, "gew03")
  # Note: x = Easting, y = Northing in EPSG:31287
  x_east     <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col_or_null(df, "xrkko08")))))
  y_north    <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col_or_null(df, "yhkko09")))))
  area       <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col_or_null(df, "egarea05")))))
  alt        <- suppressWarnings(as.numeric(gsub(",", ".", as.character(col_or_null(df, "mpua04")))))

  # Reproject to WGS84
  wgs <- .at_to_wgs84_coords(x_east, y_north, from_epsg = 31287L)

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = as.character(station_id),
    station_name       = as.character(name),
    station_name_ascii = to_ascii(name),
    river              = as.character(river),
    river_ascii        = to_ascii(river),
    lat                = wgs$lat,
    lon                = wgs$lon,
    area               = area,
    altitude           = alt
  )
  out
}

# -----------------------------------------------------------------------------
# Time series (per-station CSVs under Q-/W-Tagesmittel)
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_AT_EHYD <- function(x,
                                             parameter = c("water_discharge",
                                                           "water_level",
                                                           "water_temperature_monthly_mean",
                                                           "suspended_sediment_daily_load",
                                                           "bedload_daily_load"),
                                             stations = NULL,
                                             start_date = NULL,
                                             end_date   = NULL,
                                             mode = c("complete","range"),
                                             ...) {
  parameter <- match.arg(parameter)
  pm        <- .at_param_map_AT(parameter)
  mode      <- match.arg(mode)

  # Resolve date window (also validates range inputs)
  rng <- resolve_dates(mode, start_date, end_date)

  # Ensure bundle is present & capture provenance
  root       <- .at_ensure_unpacked()
  source_url <- x$base_url

  # 1) Load (or build) the full-parameter cache (ALL stations)
  all_ts <- .at_get_param_cache(root, parameter, pm)
  if (is.null(all_ts) || !nrow(all_ts)) {
    # Even if all_ts is empty, we may still have an empty-stations note
    empty_ids <- .at_get_empty_stations(root, parameter)
    if (length(empty_ids)) {
      cli::cli_inform(c("!" = sprintf("AT_EHYD: %d station file(s) in '%s' contain no usable data.",
                                      length(empty_ids), pm$subdir)))
    }
    return(.at_empty_ts(x, parameter, pm$unit, source_url))
  }

  # 2) Optional station filter + note about missing/empty ones
  if (!is.null(stations) && length(stations)) {
    stations <- unique(as.character(stations))

    # Stations present in cache vs requested
    present_ids <- unique(as.character(all_ts$station_id))
    missing_req <- setdiff(stations, present_ids)

    # If some requested ids are missing, see if they are known-empty
    if (length(missing_req)) {
      empty_ids <- .at_get_empty_stations(root, parameter)
      known_empty <- intersect(missing_req, empty_ids)
      unknown_missing <- setdiff(missing_req, empty_ids)

      if (length(known_empty)) {
        cli::cli_inform(c("!" = sprintf(
          "AT_EHYD: %d requested station(s) have no '%s' data in the bundle: %s",
          length(known_empty), parameter, paste(known_empty, collapse = ", ")
        )))
      }
      if (length(unknown_missing)) {
        # Not in cache and not in empty list -> file not in the subdir
        cli::cli_inform(c("!" = sprintf(
          "AT_EHYD: %d requested station(s) not found in '%s': %s",
          length(unknown_missing), pm$subdir, paste(unknown_missing, collapse = ", ")
        )))
      }
    }

    all_ts <- dplyr::filter(all_ts, .data$station_id %in% stations)
    if (!nrow(all_ts)) return(.at_empty_ts(x, parameter, pm$unit, source_url))
  }


  # 2) Optional station filter
  if (!is.null(stations) && length(stations)) {
    stations <- unique(as.character(stations))
    all_ts <- dplyr::filter(all_ts, .data$station_id %in% stations)
    if (!nrow(all_ts)) return(.at_empty_ts(x, parameter, pm$unit, source_url))
  }

  # 3) Optional date-range filter (day-level boundaries)
  if (identical(mode, "range")) {
    s <- as.Date(rng$start); e <- as.Date(rng$end)
    if (!is.na(s)) all_ts <- all_ts[all_ts$timestamp >= as.POSIXct(s, tz = "UTC"), , drop = FALSE]
    if (!is.na(e)) all_ts <- all_ts[all_ts$timestamp <= as.POSIXct(e + 1, tz = "UTC") - 1, , drop = FALSE]
    if (!nrow(all_ts)) return(.at_empty_ts(x, parameter, pm$unit, source_url))
  }

  # 4) Final adornments
  all_ts |>
    dplyr::arrange(.data$station_id, .data$timestamp) |>
    dplyr::mutate(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      parameter     = parameter,
      unit          = pm$unit,
      quality_code  = NA_character_,
      quality_name  = NA_character_,
      quality_desc  = NA_character_,
      source_url    = source_url,
      .before = 1
    ) |>
    dplyr::select(country, provider_id, provider_name, station_id, parameter,
                  timestamp, value, unit, quality_code, quality_name,
                  quality_desc, source_url)
}
