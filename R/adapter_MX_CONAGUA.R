# R/adapter_MX_CONAGUA.R
# Mexico - CONAGUA SIH Hydrometric adapter
#
# Source page:
#   https://sih.conagua.gob.mx/hidros.html
#
# Catalogue:
#   https://sih.conagua.gob.mx/basedatos/Hidros/0_Catalogo%20de%20estaciones%20hidrometricas.csv
#
# Station time series pattern:
#   https://sih.conagua.gob.mx/basedatos/Hidros/<Clave>.csv
#
# Example:
#   https://sih.conagua.gob.mx/basedatos/Hidros/ABSTP.csv
#
# Primary path:
# - Download/cache metadata catalogue
# - Download/cache station-level CSV files on demand
# - If stations = NULL in timeseries(), all station CSVs from the catalogue are fetched
# - Each station CSV contains both:
#     Nivel(m)     -> water_level
#     Gasto(m³/s) -> water_discharge
# - Parsed time series are cached as RDS per parameter

# -----------------------------------------------------------------------------
# Constants
# -----------------------------------------------------------------------------

MX_CONAGUA_PAGE_URL <- "https://sih.conagua.gob.mx/hidros.html"

MX_CONAGUA_HIDROS_DIR_URL <- "https://sih.conagua.gob.mx/basedatos/Hidros"

MX_CONAGUA_META_URL <- paste0(
  MX_CONAGUA_HIDROS_DIR_URL,
  "/0_Catalogo%20de%20estaciones%20hidrometricas.csv"
)

MX_CONAGUA_META_FILE <- "0_Catalogo_de_estaciones_hidrometricas.csv"


# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_MX_CONAGUA <- function() {
  register_service_usage(
    provider_id   = "MX_CONAGUA",
    provider_name = "CONAGUA SIH Hydrometric CSV",
    country       = "Mexico",
    base_url      = MX_CONAGUA_PAGE_URL,
    rate_cfg      = list(n = 5, period = 1),
    auth          = list(type = "none")
  )
}


#' @export
timeseries_parameters.hydro_service_MX_CONAGUA <- function(x, ...) {
  c("water_discharge", "water_level")
}


# -----------------------------------------------------------------------------
# Parameter mapping
# -----------------------------------------------------------------------------

.mx_param_map_MX <- function(parameter) {
  parameter <- match.arg(parameter, c("water_discharge", "water_level"))

  if (identical(parameter, "water_discharge")) {
    return(list(
      param     = "water_discharge",
      unit      = "m^3/s",
      value_col = "water_discharge",
      to_canon  = function(x) suppressWarnings(as.numeric(x))
    ))
  }

  list(
    param     = "water_level",
    unit      = "m",
    value_col = "water_level",
    to_canon  = function(x) suppressWarnings(as.numeric(x))
  )
}


# -----------------------------------------------------------------------------
# Cache directories
# -----------------------------------------------------------------------------

.mx_cache_dir <- function() {
  root <- tryCatch(rappdirs::user_cache_dir(), error = function(e) NULL)
  if (is.null(root)) root <- file.path(tempdir(), "hydro_cache_root")

  base <- file.path(root, "hydrodownloadR")
  if (!dir.exists(base)) dir.create(base, recursive = TRUE, showWarnings = FALSE)

  dir <- file.path(base, "MX_CONAGUA")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  dir
}


.mx_station_cache_dir <- function() {
  dir <- file.path(.mx_cache_dir(), "station_csv")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}


.mx_table_cache_dir <- function() {
  dir <- file.path(.mx_cache_dir(), "tables_cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}


.mx_catalog_path <- function() {
  file.path(.mx_cache_dir(), MX_CONAGUA_META_FILE)
}


.mx_station_csv_path <- function(station_id) {
  station_id <- toupper(trimws(as.character(station_id)))
  file.path(.mx_station_cache_dir(), paste0(station_id, ".csv"))
}


.mx_station_csv_url <- function(station_id) {
  station_id <- toupper(trimws(as.character(station_id)))
  paste0(MX_CONAGUA_HIDROS_DIR_URL, "/", utils::URLencode(station_id, reserved = TRUE), ".csv")
}


.mx_param_cache_rds <- function(root, parameter) {
  tag <- gsub("[^A-Za-z0-9_.-]", "_", basename(root))
  file.path(.mx_table_cache_dir(), sprintf("%s__ALL_%s.rds", tag, parameter))
}


.mx_param_empty_rds <- function(root, parameter) {
  tag <- gsub("[^A-Za-z0-9_.-]", "_", basename(root))
  file.path(.mx_table_cache_dir(), sprintf("%s__EMPTY_%s.rds", tag, parameter))
}


# -----------------------------------------------------------------------------
# Small utilities
# -----------------------------------------------------------------------------

.mx_use_cli <- function() {
  isTRUE(l10n_info()[["UTF-8"]]) && requireNamespace("cli", quietly = TRUE)
}


.mx_inform <- function(type = "i", msg) {
  if (.mx_use_cli()) {
    cli::cli_inform(setNames(list(msg), type))
  } else {
    prefix <- switch(type, "!" = "WARN:", "v" = "OK:", "i" = "INFO:", "INFO:")
    message(sprintf("%s %s", prefix, msg))
  }
}


.mx_as_utf8 <- function(x) {
  x <- as.character(x)

  out <- tryCatch(
    iconv(x, from = "", to = "UTF-8", sub = "byte"),
    error = function(e) x
  )

  bad <- is.na(out) & !is.na(x)
  if (any(bad)) out[bad] <- enc2utf8(x[bad])

  out
}


.mx_read_lines_utf8 <- function(path) {
  if (!file.exists(path) || file.info(path)$size <= 0) return(character())

  raw <- readBin(path, what = "raw", n = file.info(path)$size)
  txt <- rawToChar(raw)

  # Most files should be UTF-8 or Latin-1. We parse data by position, so this is
  # mainly to make header detection and diagnostics robust.
  out <- tryCatch(iconv(txt, from = "UTF-8", to = "UTF-8", sub = "byte"),
                  error = function(e) NA_character_)

  if (is.na(out)) {
    out <- tryCatch(iconv(txt, from = "Latin1", to = "UTF-8", sub = "byte"),
                    error = function(e) NA_character_)
  }

  if (is.na(out)) {
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
    return(.mx_as_utf8(lines))
  }

  out <- sub("^\ufeff", "", out)
  strsplit(out, "\\r?\\n", perl = TRUE)[[1]]
}


.mx_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", "NA", "N/A", "NaN", "NULL", "null")] <- NA_character_

  x <- gsub("\\s+", "", x)

  # If decimal comma occurs without a decimal point, convert to decimal point.
  comma_decimal <- grepl(",", x, fixed = TRUE) & !grepl(".", x, fixed = TRUE)
  x[comma_decimal] <- gsub(",", ".", x[comma_decimal], fixed = TRUE)

  suppressWarnings(as.numeric(x))
}


.mx_col_i <- function(df, i) {
  if (ncol(df) >= i) return(df[[i]])
  rep(NA_character_, nrow(df))
}


.mx_parse_date <- function(x) {
  x <- trimws(as.character(x))

  out <- suppressWarnings(as.Date(x, format = "%Y/%m/%d"))

  bad <- is.na(out)
  if (any(bad)) {
    out[bad] <- suppressWarnings(as.Date(x[bad], format = "%Y-%m-%d"))
  }

  bad <- is.na(out)
  if (any(bad)) {
    out[bad] <- suppressWarnings(as.Date(x[bad], format = "%d/%m/%Y"))
  }

  bad <- is.na(out)
  if (any(bad)) {
    out[bad] <- suppressWarnings(as.Date(x[bad]))
  }

  out
}


# -----------------------------------------------------------------------------
# Download helpers
# -----------------------------------------------------------------------------

.mx_download_file <- function(url, dest, force = FALSE, warn = TRUE) {
  if (!isTRUE(force) && file.exists(dest) && file.info(dest)$size > 0) {
    return(dest)
  }

  if (!dir.exists(dirname(dest))) {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  }

  resp <- tryCatch({
    httr2::request(url) |>
      httr2::req_user_agent("hydrodownloadR (https://github.com/bafg-bund/hydrodownloadR)") |>
      httr2::req_timeout(120) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  }, error = function(e) e)

  if (inherits(resp, "error")) {
    if (isTRUE(warn)) {
      warning(
        sprintf("MX_CONAGUA: download failed for %s: %s", url, conditionMessage(resp)),
        call. = FALSE
      )
    }
    return(NA_character_)
  }

  status <- httr2::resp_status(resp)
  if (status < 200 || status >= 300) {
    if (isTRUE(warn)) {
      warning(
        sprintf("MX_CONAGUA: download failed: HTTP %s for %s", status, url),
        call. = FALSE
      )
    }
    return(NA_character_)
  }

  body <- httr2::resp_body_raw(resp)
  if (!length(body)) {
    if (isTRUE(warn)) {
      warning(sprintf("MX_CONAGUA: empty response body for %s", url), call. = FALSE)
    }
    return(NA_character_)
  }

  writeBin(body, dest)
  dest
}


.mx_download_catalog <- function(force = FALSE) {
  dest <- .mx_catalog_path()

  if (!isTRUE(force) && file.exists(dest) && file.info(dest)$size > 0) {
    return(dest)
  }

  .mx_inform("i", "Downloading CONAGUA SIH hydrometric station catalogue.")
  .mx_inform("i", paste0("Destination: ", dest))

  out <- .mx_download_file(MX_CONAGUA_META_URL, dest, force = force, warn = TRUE)

  if (is.na(out) || !file.exists(out) || file.info(out)$size <= 0) {
    stop("MX_CONAGUA: metadata catalogue download failed.", call. = FALSE)
  }

  out
}


.mx_read_catalog_csv <- function(path) {
  if (!file.exists(path)) return(NULL)

  encodings <- c("UTF-8", "Latin1", "")

  for (enc in encodings) {
    args <- list(
      file              = path,
      header            = TRUE,
      stringsAsFactors  = FALSE,
      check.names       = FALSE,
      colClasses        = "character",
      na.strings        = c("", "NA", "N/A"),
      strip.white       = TRUE
    )

    if (nzchar(enc)) args$fileEncoding <- enc

    df <- tryCatch(
      do.call(utils::read.csv, args),
      error = function(e) NULL
    )

    if (!is.null(df) && nrow(df) && ncol(df) >= 5) {
      names(df) <- .mx_as_utf8(names(df))
      names(df) <- sub("^\ufeff", "", names(df))

      for (nm in names(df)) {
        if (is.character(df[[nm]])) df[[nm]] <- .mx_as_utf8(df[[nm]])
      }

      return(df)
    }
  }

  NULL
}


.mx_catalog_station_ids <- function(update = FALSE) {
  path <- .mx_download_catalog(force = isTRUE(update))
  df <- .mx_read_catalog_csv(path)

  if (is.null(df) || !nrow(df)) {
    stop("MX_CONAGUA: could not read metadata catalogue.", call. = FALSE)
  }

  ids <- toupper(trimws(as.character(.mx_col_i(df, 1))))
  ids <- ids[!is.na(ids) & nzchar(ids)]

  unique(ids)
}


.mx_ensure_station_files <- function(station_ids, force = FALSE) {
  station_ids <- unique(toupper(trimws(as.character(station_ids))))
  station_ids <- station_ids[!is.na(station_ids) & nzchar(station_ids)]

  if (!length(station_ids)) return(character())

  paths <- .mx_station_csv_path(station_ids)

  need <- isTRUE(force) |
    !file.exists(paths) |
    is.na(file.info(paths)$size) |
    file.info(paths)$size <= 0

  todo_ids <- station_ids[need]

  if (!length(todo_ids)) {
    return(paths[file.exists(paths) & file.info(paths)$size > 0])
  }

  .mx_inform("i", sprintf(
    "Downloading %s CONAGUA station CSV file(s).",
    format(length(todo_ids), big.mark = ",")
  ))

  failed <- character()

  if (.mx_use_cli()) {
    pb_id <- cli::cli_progress_bar(
      name   = "Downloading MX_CONAGUA",
      total  = length(todo_ids),
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} {cli::pb_eta} {cli::pb_spin}"
    )

    for (i in seq_along(todo_ids)) {
      sid  <- todo_ids[[i]]
      url  <- .mx_station_csv_url(sid)
      dest <- .mx_station_csv_path(sid)

      out <- .mx_download_file(url, dest, force = force, warn = FALSE)
      if (is.na(out) || !file.exists(out) || file.info(out)$size <= 0) {
        failed <- c(failed, sid)
      }

      cli::cli_progress_update(id = pb_id, set = i)
    }

    cli::cli_progress_done(id = pb_id)
  } else {
    pb <- NULL
    if (interactive()) pb <- utils::txtProgressBar(min = 0, max = length(todo_ids), style = 3)

    for (i in seq_along(todo_ids)) {
      sid  <- todo_ids[[i]]
      url  <- .mx_station_csv_url(sid)
      dest <- .mx_station_csv_path(sid)

      out <- .mx_download_file(url, dest, force = force, warn = FALSE)
      if (is.na(out) || !file.exists(out) || file.info(out)$size <= 0) {
        failed <- c(failed, sid)
      }

      if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
    }

    if (!is.null(pb)) close(pb)
  }

  if (length(failed)) {
    .mx_inform("!", sprintf(
      "MX_CONAGUA: %s station CSV download(s) failed: %s",
      length(failed),
      paste(utils::head(failed, 20), collapse = ", ")
    ))
  }

  paths <- .mx_station_csv_path(station_ids)
  paths[file.exists(paths) & file.info(paths)$size > 0]
}


# Optional manual prefetch helper, analogous to the Austrian bundle setup.
# Use:
#   hydrodownloadR:::.mx_ensure_bundle(force = TRUE)
# or for selected stations:
#   hydrodownloadR:::.mx_ensure_bundle(stations = c("ABSTP", "CNDCA"))
.mx_ensure_bundle <- function(force = FALSE, stations = NULL) {
  root <- .mx_cache_dir()
  .mx_download_catalog(force = force)

  ids <- if (is.null(stations) || !length(stations)) {
    .mx_catalog_station_ids(update = force)
  } else {
    unique(toupper(trimws(as.character(stations))))
  }

  .mx_ensure_station_files(ids, force = force)

  root
}


# -----------------------------------------------------------------------------
# Station CSV parser
# -----------------------------------------------------------------------------

.mx_parse_station_csv <- function(path) {
  if (!file.exists(path) || file.info(path)$size <= 0) return(NULL)

  lines <- .mx_read_lines_utf8(path)
  lines <- lines[nzchar(trimws(lines))]

  if (!length(lines)) return(NULL)

  # Station files have a short metadata header, then:
  # Fecha, Nivel(m), Gasto(m³/s)
  idx <- which(grepl("^\\s*Fecha\\s*,", lines, ignore.case = TRUE))

  if (!length(idx)) return(NULL)

  txt <- paste(lines[idx[1]:length(lines)], collapse = "\n")

  df <- tryCatch({
    utils::read.csv(
      text              = txt,
      header            = TRUE,
      sep               = ",",
      dec               = ".",
      stringsAsFactors  = FALSE,
      check.names       = FALSE,
      na.strings        = c("", "-", "NA", "N/A"),
      strip.white       = TRUE
    )
  }, error = function(e) NULL)

  if (is.null(df) || !nrow(df) || ncol(df) < 2) return(NULL)

  date <- .mx_parse_date(.mx_col_i(df, 1))

  level <- if (ncol(df) >= 2) {
    .mx_num(.mx_col_i(df, 2))
  } else {
    rep(NA_real_, length(date))
  }

  discharge <- if (ncol(df) >= 3) {
    .mx_num(.mx_col_i(df, 3))
  } else {
    rep(NA_real_, length(date))
  }

  keep <- !is.na(date) & (!is.na(level) | !is.na(discharge))

  if (!any(keep)) return(NULL)

  tibble::tibble(
    timestamp       = as.POSIXct(date[keep], tz = "UTC"),
    water_level     = level[keep],
    water_discharge = discharge[keep]
  )
}


# -----------------------------------------------------------------------------
# Parameter cache
# -----------------------------------------------------------------------------

.mx_empty_ts <- function(x, parameter, unit) {
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
    quality_name  = character(0),
    quality_desc  = character(0),
    source_url    = character(0)
  )
}


.mx_get_empty_stations <- function(root, parameter) {
  rds <- .mx_param_empty_rds(root, parameter)

  if (!file.exists(rds)) return(character())

  ids <- tryCatch(readRDS(rds), error = function(e) character())
  unique(as.character(ids))
}


.mx_build_param_cache_from_files <- function(station_ids,
                                             parameter,
                                             pm,
                                             save_rds = NULL,
                                             root = .mx_cache_dir()) {
  station_ids <- unique(toupper(trimws(as.character(station_ids))))
  station_ids <- station_ids[!is.na(station_ids) & nzchar(station_ids)]

  if (!length(station_ids)) return(NULL)

  .mx_inform("i", sprintf(
    "Parsing CONAGUA '%s' data for %s station file(s).",
    parameter,
    format(length(station_ids), big.mark = ",")
  ))

  ts_list   <- vector("list", length(station_ids))
  empty_ids <- character()

  if (.mx_use_cli()) {
    pb_id <- cli::cli_progress_bar(
      name   = paste0("Parsing ", parameter),
      total  = length(station_ids),
      format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total} {cli::pb_eta} {cli::pb_spin}"
    )

    for (i in seq_along(station_ids)) {
      sid  <- station_ids[[i]]
      path <- .mx_station_csv_path(sid)

      wide <- .mx_parse_station_csv(path)

      if (is.null(wide) || !nrow(wide) || !pm$value_col %in% names(wide)) {
        empty_ids <- c(empty_ids, sid)
        ts_list[[i]] <- NULL
      } else {
        val <- wide[[pm$value_col]]
        keep <- !is.na(wide$timestamp) & !is.na(val)

        if (!any(keep)) {
          empty_ids <- c(empty_ids, sid)
          ts_list[[i]] <- NULL
        } else {
          ts_list[[i]] <- tibble::tibble(
            timestamp  = wide$timestamp[keep],
            value      = pm$to_canon(val[keep]),
            station_id = sid,
            source_url = .mx_station_csv_url(sid)
          )
        }
      }

      cli::cli_progress_update(id = pb_id, set = i)
    }

    cli::cli_progress_done(id = pb_id)
  } else {
    pb <- NULL
    if (interactive()) pb <- utils::txtProgressBar(min = 0, max = length(station_ids), style = 3)

    for (i in seq_along(station_ids)) {
      sid  <- station_ids[[i]]
      path <- .mx_station_csv_path(sid)

      wide <- .mx_parse_station_csv(path)

      if (is.null(wide) || !nrow(wide) || !pm$value_col %in% names(wide)) {
        empty_ids <- c(empty_ids, sid)
        ts_list[[i]] <- NULL
      } else {
        val <- wide[[pm$value_col]]
        keep <- !is.na(wide$timestamp) & !is.na(val)

        if (!any(keep)) {
          empty_ids <- c(empty_ids, sid)
          ts_list[[i]] <- NULL
        } else {
          ts_list[[i]] <- tibble::tibble(
            timestamp  = wide$timestamp[keep],
            value      = pm$to_canon(val[keep]),
            station_id = sid,
            source_url = .mx_station_csv_url(sid)
          )
        }
      }

      if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
    }

    if (!is.null(pb)) close(pb)
  }

  d <- dplyr::bind_rows(ts_list)

  if (!nrow(d)) {
    d <- tibble::tibble(
      timestamp  = as.POSIXct(character(0), tz = "UTC"),
      value      = numeric(0),
      station_id = character(0),
      source_url = character(0)
    )
  } else {
    d <- d |>
      dplyr::select(.data$timestamp, .data$value, .data$station_id, .data$source_url) |>
      dplyr::arrange(.data$station_id, .data$timestamp)
  }

  empty_ids <- unique(empty_ids)

  if (!is.null(save_rds)) {
    saveRDS(d, save_rds)
    saveRDS(empty_ids, .mx_param_empty_rds(root, parameter))

    .mx_inform("v", sprintf(
      "Cached MX_CONAGUA %s: %s rows, %s stations.",
      parameter,
      format(nrow(d), big.mark = ","),
      format(dplyr::n_distinct(d$station_id), big.mark = ",")
    ))
    .mx_inform("i", paste0("Stored at: ", save_rds))
  }

  d
}


.mx_get_param_cache <- function(root,
                                parameter,
                                pm,
                                stations = NULL,
                                force = FALSE) {
  rds <- .mx_param_cache_rds(root, parameter)

  # If full cache exists, use it and filter if requested.
  if (!isTRUE(force) && file.exists(rds)) {
    d <- tryCatch(readRDS(rds), error = function(e) NULL)

    if (is.data.frame(d)) {
      if (!is.null(stations) && length(stations)) {
        stations <- unique(toupper(trimws(as.character(stations))))
        d <- dplyr::filter(d, .data$station_id %in% stations)
      }

      return(d)
    }
  }

  # If stations are supplied and no full cache exists, parse only requested files.
  # This avoids downloading the whole CONAGUA dataset for a small request.
  if (!is.null(stations) && length(stations)) {
    stations <- unique(toupper(trimws(as.character(stations))))
    stations <- stations[!is.na(stations) & nzchar(stations)]

    .mx_ensure_station_files(stations, force = force)

    return(.mx_build_param_cache_from_files(
      station_ids = stations,
      parameter   = parameter,
      pm          = pm,
      save_rds    = NULL,
      root        = root
    ))
  }

  # No stations supplied: build the full parameter cache from all catalogue IDs.
  ids <- .mx_catalog_station_ids(update = force)

  .mx_ensure_station_files(ids, force = force)

  .mx_build_param_cache_from_files(
    station_ids = ids,
    parameter   = parameter,
    pm          = pm,
    save_rds    = rds,
    root        = root
  )
}


# -----------------------------------------------------------------------------
# Stations metadata
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_MX_CONAGUA <- function(x, update = FALSE, ...) {
  path <- .mx_download_catalog(force = isTRUE(update))
  df <- .mx_read_catalog_csv(path)

  if (is.null(df) || !nrow(df)) return(tibble::tibble())

  # Catalogue structure:
  # 1 Clave
  # 2 Nombre de la estación
  # 3 Latitud
  # 4 Longitud
  # 5 Altitud
  # 6 Estado
  # 7 Municipio
  # 8 R.H.
  # 9 Cuenca
  station_id <- toupper(trimws(as.character(.mx_col_i(df, 1))))
  name       <- trimws(as.character(.mx_col_i(df, 2)))
  lat        <- .mx_num(.mx_col_i(df, 3))
  lon        <- .mx_num(.mx_col_i(df, 4))
  altitude   <- .mx_num(.mx_col_i(df, 5))

  # Keep Cuenca internally, but do not expose it as a separate output column.
  river0     <- trimws(as.character(.mx_col_i(df, 9)))

  keep <- !is.na(station_id) & nzchar(station_id)

  tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = station_id[keep],
    station_name       = name[keep],
    station_name_ascii = to_ascii(name[keep]),
    river              = river0[keep],
    river_ascii        = to_ascii(river0[keep]),
    lat                = lat[keep],
    lon                = lon[keep],
    area               = NA_real_,
    altitude           = altitude[keep]
  )
}

# -----------------------------------------------------------------------------
# Time series
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_MX_CONAGUA <- function(x,
                                                parameter = c("water_discharge",
                                                              "water_level"),
                                                stations = NULL,
                                                start_date = NULL,
                                                end_date   = NULL,
                                                mode = c("complete", "range"),
                                                update = FALSE,
                                                ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  pm        <- .mx_param_map_MX(parameter)

  rng <- resolve_dates(mode, start_date, end_date)

  root <- .mx_cache_dir()

  if (!is.null(stations) && length(stations)) {
    stations <- unique(toupper(trimws(as.character(stations))))
    stations <- stations[!is.na(stations) & nzchar(stations)]
  }

  all_ts <- .mx_get_param_cache(
    root      = root,
    parameter = parameter,
    pm        = pm,
    stations  = stations,
    force     = isTRUE(update)
  )

  if (is.null(all_ts) || !nrow(all_ts)) {
    empty_ids <- .mx_get_empty_stations(root, parameter)

    if (length(empty_ids)) {
      .mx_inform("!", sprintf(
        "MX_CONAGUA: %s station(s) have no usable '%s' data in the cached files.",
        length(empty_ids),
        parameter
      ))
    }

    return(.mx_empty_ts(x, parameter, pm$unit))
  }

  if (identical(mode, "range")) {
    s <- as.Date(rng$start)
    e <- as.Date(rng$end)

    if (!is.na(s)) {
      all_ts <- all_ts[
        all_ts$timestamp >= as.POSIXct(s, tz = "UTC"),
        ,
        drop = FALSE
      ]
    }

    if (!is.na(e)) {
      all_ts <- all_ts[
        all_ts$timestamp <= as.POSIXct(e + 1, tz = "UTC") - 1,
        ,
        drop = FALSE
      ]
    }

    if (!nrow(all_ts)) return(.mx_empty_ts(x, parameter, pm$unit))
  }

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
      .before       = 1
    ) |>
    dplyr::select(
      .data$country,
      .data$provider_id,
      .data$provider_name,
      .data$station_id,
      .data$parameter,
      .data$timestamp,
      .data$value,
      .data$unit,
      .data$quality_code,
      .data$quality_name,
      .data$quality_desc,
      .data$source_url
    )
}
