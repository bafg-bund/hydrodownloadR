# ==== Poland (IMGW Public Data) adapter ======================================
# Base: "https://danepubliczne.imgw.pl"
# Near-real-time JSON + embedded metadata: /api/data/hydro
# Historical daily (per-year zip): /data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/{YYYY}/zjaw_{YYYY}.zip

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_PL_IMGW <- function() {
  register_service_usage(
    provider_id   = "PL_IMGW",
    provider_name = "IMGW Public Data",
    country       = "Poland",
    base_url      = "https://danepubliczne.imgw.pl",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_PL_IMGW <- function(x, ...) {
  c("water_discharge", "water_level", "water_temperature")
}
# -----------------------------------------------------------------------------
# Parameter mapping (private)
# -----------------------------------------------------------------------------

.pl_param_map <- function(parameter) {
  switch(parameter,
         water_level = list(
           unit            = "cm",
           hist_file_month = function(y, m)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d_%02d.zip", y, y, as.integer(m)),
           hist_file_year  = function(y)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d.zip", y, y),
           hist_value_col  = "Water level [cm]"
         ),
         water_discharge = list(
           unit            = "m^3/s",
           hist_file_month = function(y, m)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d_%02d.zip", y, y, as.integer(m)),
           hist_file_year  = function(y)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d.zip", y, y),
           hist_value_col  = "Flow [m^3/s]"
         ),
         water_temperature = list(
           unit            = "\u00B0C",
           hist_file_month = function(y, m)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d_%02d.zip", y, y, as.integer(m)),
           hist_file_year  = function(y)
             sprintf("/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/dobowe/%d/codz_%04d.zip", y, y),
           hist_value_col  = "Water temperature [deg. C]"
         ),
         rlang::abort("PL_IMGW supports 'water_discharge', 'water_level', 'water_temperature'.")
  )
}

# -----------------------------------------------------------------------------
# Small helpers (private)
# -----------------------------------------------------------------------------

.mv_level_cm <- function(x) { x <- suppressWarnings(as.numeric(x)); x[x == 9999] <- NA_real_; x }
.mv_flow_ms  <- function(x) { x <- suppressWarnings(as.numeric(x)); x[x %in% c(99999.999, 999)] <- NA_real_; x }
.mv_temp_c   <- function(x) { x <- suppressWarnings(as.numeric(x)); x[x == 99.9] <- NA_real_; x }

# Cache location resolver (shared layout like AT_EHYD)
.pl_cache_base <- function(cache_dir = NULL) {
  # 1) User override
  if (is.character(cache_dir) && nzchar(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    return(normalizePath(cache_dir, winslash = "/", mustWork = FALSE))
  }

  # 2) Platform cache root (no appname)
  root <- tryCatch(rappdirs::user_cache_dir(), error = function(e) NULL)
  if (is.null(root)) root <- file.path(tempdir(), "hydro_cache_root")

  # 3) Shared base dir for hydrodownloadR
  base <- file.path(root, "hydrodownloadR")
  dir.create(base, recursive = TRUE, showWarnings = FALSE)

  # 4) Adapter-specific dir
  dir <- file.path(base, "PL_IMGW")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  normalizePath(dir, winslash = "/", mustWork = FALSE)
}



# Read + normalize a historical IMGW daily CSV (handles headerless files & 2024 fully-quoted rows)
.pl_read_hist_csv <- function(csv_file) {
  expected_names <- c(
    "Station code",
    "Station name",
    "River",
    "Hydrological year",
    "Month indicator in the hydrological year",
    "Day",
    "Water level [cm]",
    "Flow [m^3/s]",
    "Water temperature [deg. C]",
    "Calendar month"
  )

  loc <- readr::locale(encoding = "CP1250", decimal_mark = ".")
  first_line <- tryCatch(readr::read_lines(csv_file, n_max = 1, locale = loc), error = function(e) "")
  has_semicol <- is.character(first_line) && length(first_line) && grepl(";", first_line, fixed = TRUE)
  has_comma   <- is.character(first_line) && length(first_line) && grepl(",", first_line, fixed = TRUE)
  delim <- if (has_semicol) ";" else ","

  .norm_chr_cols <- function(df) {
    chr_cols <- names(df)[vapply(df, is.character, logical(1))]
    for (cc in chr_cols) df[[cc]] <- normalize_utf8(df[[cc]])
    df
  }

  # --- Strategy: always read headerless, then assign names; special 2024 fix ---
  # 1) Try readr, headerless
  df0 <- suppressWarnings(
    readr::read_delim(
      file = csv_file, delim = delim, locale = loc,
      col_names = FALSE, show_col_types = FALSE, trim_ws = TRUE,
      quote = "\"", na = c("", "NA")
    )
  )

  # 1a) 2024-style fully-quoted single-column fallback
  if (is.data.frame(df0) && ncol(df0) == 1 && (has_comma || has_semicol)) {
    lines <- readr::read_lines(csv_file, locale = loc)
    # Strip only one leading and one trailing quote if present
    lines <- sub('^"', '', lines)
    lines <- sub('"$', '', lines)
    lines <- gsub('""', '"', lines, fixed = TRUE)
    sep <- if (any(grepl(";", lines, fixed = TRUE))) ";" else ","
    df0 <- utils::read.table(text = lines, sep = sep, quote = "\"",
                             dec = ".", header = FALSE, fill = TRUE,
                             comment.char = "", stringsAsFactors = FALSE,
                             na.strings = c("", "NA"))
  }

  # If column count is off (rare), try base read.table headerless as a last resort
  if (!is.data.frame(df0) || ncol(df0) != length(expected_names)) {
    df0 <- tryCatch(
      utils::read.table(file = csv_file, sep = delim, quote = "\"",
                        dec = ".", header = FALSE, fill = TRUE,
                        comment.char = "", stringsAsFactors = FALSE,
                        na.strings = c("", "NA")),
      error = function(e) data.frame()
    )
  }

  if (!nrow(df0) || ncol(df0) != length(expected_names)) {
    # Give up gracefully
    return(tibble::tibble())
  }

  # Assign canonical names
  names(df0) <- expected_names
  df <- tibble::as_tibble(df0)

  # Drop first row ONLY if it is an exact header row (position-wise match)
  is_exact_header <- {
    r1 <- as.character(unlist(df[1, ], use.names = FALSE))
    all(trimws(r1) == expected_names)
  }
  if (is_exact_header) {
    df <- dplyr::slice(df, -1)
  }

  # Normalize strings to UTF-8
  df <- .norm_chr_cols(df)

  # Build Date and drop the source columns + "Calendar month"
  df <- df |>
    dplyr::mutate(
      `Hydrological year` = suppressWarnings(as.integer(`Hydrological year`)),
      `Month indicator in the hydrological year` = suppressWarnings(as.integer(`Month indicator in the hydrological year`)),
      Day = suppressWarnings(as.integer(Day)),

      # Map hydrological month index (01=Nov … 12=Oct) to calendar month/year
      .cal_month = ((`Month indicator in the hydrological year` + 9L) %% 12L) + 1L,
      .cal_year  = dplyr::if_else(.cal_month >= 11L, `Hydrological year` - 1L, `Hydrological year`),

      Date = as.Date(
        sprintf("%04d-%02d-%02d", .cal_year, .cal_month, Day),
        format = "%Y-%m-%d"
      )
    ) |>
    dplyr::select(
      -`Hydrological year`,
      -`Month indicator in the hydrological year`,
      -Day,
      -`Calendar month`,
      -.cal_month,
      -.cal_year
    )

  # Move Date after "River" (if present)
  if ("River" %in% names(df)) {
    df <- dplyr::relocate(df, Date, .after = "River")
  }

  tibble::as_tibble(df)
}


# Read IMGW station list CSV (headerless, CP1250) from HTTP response
.pl_read_station_list_csvtext <- function(txt) {
  # text already decoded to UTF-8; parse quoted CSV, 4 columns, no header
  df <- suppressWarnings(
    readr::read_csv(
      I(txt),
      col_names = FALSE,
      locale = readr::locale(encoding = "UTF-8"),
      show_col_types = FALSE,
      trim_ws = TRUE,
      quote = "\""
    )
  )
  # Expect at least first 3 columns: id, name, river (4th often a code we ignore)
  if (!nrow(df)) return(tibble::tibble())
  n <- ncol(df)
  if (n < 3) return(tibble::tibble())
  names(df)[1:min(4, n)] <- c("station_id", "station_name", "river", "col4")[1:min(4, n)]
  tibble::as_tibble(df[, c("station_id","station_name","river")[1:min(3, n)], drop = FALSE])
}

# -----------------------------------------------------------------------------
# Stations (S3)
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_PL_IMGW <- function(x, ...) {
  # --- Base station list (public CSV) ----------------------------------------
  path <- "/data/dane_pomiarowo_obserwacyjne/dane_hydrologiczne/lista_stacji_hydro.csv"
  req  <- build_request(x, path = path)
  resp <- perform_request(req)

  csv_txt <- tryCatch(
    httr2::resp_body_string(resp, encoding = "CP1250"),
    error = function(e) ""
  )

  base_df <- if (nzchar(csv_txt)) .pl_read_station_list_csvtext(csv_txt) else tibble::tibble()
  if (!nrow(base_df)) {
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

  base_df$station_name <- normalize_utf8(base_df$station_name)
  base_df$river        <- normalize_utf8(base_df$river)
  base_df$station_id   <- as.character(base_df$station_id)

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = base_df$station_id,
    station_name       = base_df$station_name,
    station_name_ascii = to_ascii(base_df$station_name),
    river              = base_df$river,
    river_ascii        = to_ascii(base_df$river),
    lat                = NA_real_,
    lon                = NA_real_,
    area               = NA_real_,
    altitude           = NA_real_
  )

  # after you've built the base_df from the public CSV...
  md <- .pkg_data("pl_imgw_meta")  # <- loads the packaged tibble from /data
  if (is.data.frame(md) && nrow(md)) {
    out <- dplyr::left_join(out, md, by = "station_id", suffix = c("", "_md"))

    out$station_name <- ifelse(!is.na(out$station_name_md) & nzchar(out$station_name_md), out$station_name_md, out$station_name)
    out$river        <- ifelse(!is.na(out$river_md)        & nzchar(out$river_md),        out$river_md,        out$river)
    out$lat          <- ifelse(!is.na(out$lat_md),       out$lat_md,       out$lat)
    out$lon          <- ifelse(!is.na(out$lon_md),       out$lon_md,       out$lon)
    out$area         <- ifelse(!is.na(out$area_md),      out$area_md,      out$area)
    out$altitude     <- ifelse(!is.na(out$altitude_md),  out$altitude_md,  out$altitude)

    out$station_name_ascii <- to_ascii(out$station_name)
    out$river_ascii        <- to_ascii(out$river)

    # One-time info message with provenance
    stamp <- attr(md, "source_stamp")
    if (!is.null(stamp)) {
      rlang::inform(paste0(
        "PL_IMGW: station metadata enriched from packaged IMGW dataset (",
        format(stamp, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC"),
        "). These metadata were provided by IMGW."
      ))
    } else {
      rlang::inform("PL_IMGW: station metadata enriched from packaged IMGW dataset. These metadata were provided by IMGW.")
    }

    out <- dplyr::select(out, -dplyr::ends_with("_md"))
  } else {
    rlang::inform("PL_IMGW: packaged IMGW metadata not found; returning base list.")
  }


  # Final dedup + NA id guard
  out <- out[!is.na(out$station_id) & nzchar(out$station_id) & !duplicated(out$station_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}


# -----------------------------------------------------------------------------
# Timeseries (S3)
# -----------------------------------------------------------------------------
# --- Safe GET for historical ZIPs: don't error on 404 etc. -------------------
.pl_safe_get <- function(x, path) {
  req <- build_request(x, path = path)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)  # don't throw on 4xx/5xx
  resp <- perform_request(req)
  list(resp = resp, status = httr2::resp_status(resp))
}
# ---------- MASTER (parameter-agnostic, WIDE) --------------------------------
# Wide schema columns (minimal):
#   station_id (chr), timestamp (POSIXct, UTC date),
#   wl_cm (dbl), q_m3s (dbl), tw_c (dbl),
#   source_url (chr)
# We store provider/country in the object header to avoid repeating per row.

.pl_master_cache_path <- function(cache_dir = NULL) {
  base <- .pl_cache_base(cache_dir)
  file.path(base, "PL_IMGW_all_timeseries.rds")
}

.pl_master_wide_save <- function(ts_wide, x, cache_dir = NULL, compress = "xz") {
  path <- .pl_master_cache_path(cache_dir)
  obj  <- list(
    type          = "wide",
    stamp         = Sys.time(),
    meta          = list(country = x$country, provider_id = x$provider_id, provider_name = x$provider_name),
    data_wide     = ts_wide
  )
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(obj, path, compress = compress)
  ts <- format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
  rlang::inform(sprintf(
    "PL_IMGW: saved WIDE master with %s rows at %s\n to %s\nUse update = TRUE to refresh.",
    format(nrow(ts_wide), big.mark = ","), ts, path
  ))
  invisible(path)
}

.pl_master_load <- function(cache_dir = NULL) {
  path <- .pl_master_cache_path(cache_dir)
  if (!file.exists(path)) return(NULL)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  obj
}

.pl_master_note <- function(cache_dir = NULL) {
  path <- .pl_master_cache_path(cache_dir)
  if (!file.exists(path)) return(invisible())
  ts <- format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
  rlang::inform(sprintf("PL_IMGW: using WIDE master saved %s. Set update = TRUE to rebuild.", ts))
}

# ---------- Convert a parsed CSV (wide) into master-wide row(s) --------------
# df is result of .pl_read_hist_csv (has Date, 'Station code', and 3 value cols)
# Returns tibble: station_id, timestamp, wl_cm, q_m3s, tw_c, source_url
.pl_chunk_to_wide <- function(df, src_url) {
  if (!nrow(df)) return(tibble::tibble())
  if (!all(c("Date","Station code") %in% names(df))) return(tibble::tibble())

  ts  <- as.POSIXct(df$Date, tz = "UTC")

  wl  <- if ("Water level [cm]" %in% names(df)) df[["Water level [cm]"]] else NA_real_
  q   <- if ("Flow [m^3/s]" %in% names(df))     df[["Flow [m^3/s]"]]     else NA_real_
  tw  <- if ("Water temperature [deg. C]" %in% names(df)) df[["Water temperature [deg. C]"]] else NA_real_

  # Sentinels + numeric
  wl <- .mv_level_cm(wl)
  q  <- .mv_flow_ms(q)
  tw <- .mv_temp_c(tw)

  tibble::tibble(
    station_id = as.character(df[["Station code"]]),
    timestamp  = ts,
    wl_cm      = suppressWarnings(as.numeric(wl)),
    q_m3s      = suppressWarnings(as.numeric(q)),
    tw_c       = suppressWarnings(as.numeric(tw)),
    source_url = src_url
  )
}

# ---------- Build the WIDE master from source (no per-month/year caches) -----
.pl_build_master_from_source <- function(
    x,
    cache_dir = NULL,
    from_year = 1951L,
    to_year   = as.integer(format(Sys.Date(), "%Y"))
) {
  pm_url <- .pl_param_map("water_level")  # URL builders only
  years  <- seq.int(as.integer(from_year), as.integer(to_year))

  pby <- progress::progress_bar$new(
    total  = length(years),
    format = "PL_IMGW master build [:bar] :current/:total (:percent) Year=:current_year"
  )

  .lim_get <- ratelimitr::limit_rate(
    function(path) .pl_safe_get(x, path),
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  parts <- list()

  for (yy in years) {
    pby$tick(tokens = list(current_year = yy))

    # Prefer YEAR ZIP
    year_path <- pm_url$hist_file_year(yy)
    res_y <- .lim_get(year_path)
    if (res_y$status == 200) {
      df_wide <- .pl_fetch_zip_to_df(x, year_path)
      if (nrow(df_wide)) {
        parts[[length(parts)+1L]] <- .pl_chunk_to_wide(df_wide, paste0(x$base_url, year_path))
        next
      }
    } else if (res_y$status >= 400 && res_y$status != 404) {
      rlang::warn(sprintf("PL_IMGW: HTTP %d for %s", res_y$status, year_path))
    }

    # Fallback: MONTH ZIPs
    pbm <- progress::progress_bar$new(total = 12, format = sprintf("  %d monthly files [:bar] :current/:total", yy))
    any_ok <- FALSE
    for (mm in 1:12) {
      pbm$tick()
      month_path <- pm_url$hist_file_month(yy, mm)
      res_m <- .lim_get(month_path)
      if (res_m$status == 200) {
        df_wide_m <- .pl_fetch_zip_to_df(x, month_path)
        if (nrow(df_wide_m)) {
          parts[[length(parts)+1L]] <- .pl_chunk_to_wide(df_wide_m, paste0(x$base_url, month_path))
          any_ok <- TRUE
        }
      } else if (res_m$status >= 400 && res_m$status != 404) {
        rlang::warn(sprintf("PL_IMGW: HTTP %d for %s", res_m$status, month_path))
      }
    }
    if (!any_ok) {
      rlang::inform(sprintf("PL_IMGW: no data found for year %d (no year ZIP and no month ZIPs).", yy))
    }
  }

  master_wide <- suppressWarnings(dplyr::bind_rows(parts))
  if (!nrow(master_wide)) {
    rlang::warn("PL_IMGW: master build produced no rows.")
    return(invisible(NULL))
  }

  # Deduplicate if a date/station appears multiple times (prefer non-NA values)
  master_wide <- master_wide |>
    dplyr::arrange(.data$station_id, .data$timestamp) |>
    dplyr::distinct(.data$station_id, .data$timestamp, .keep_all = TRUE)

  # Save single WIDE master
  .pl_master_wide_save(master_wide, x, cache_dir = cache_dir, compress = "xz")
  invisible(master_wide)
}

.pl_fetch_zip_to_df <- function(x, url_path) {
  res <- .pl_safe_get(x, url_path)
  if (res$status != 200) return(tibble::tibble())
  # temp zip and dir
  zf <- tempfile(fileext = ".zip")
  on.exit(try(unlink(zf), silent = TRUE), add = TRUE)
  writeBin(httr2::resp_body_raw(res$resp), zf)
  exdir <- tempfile("pl_imgw_zip_")
  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  on.exit(try(unlink(exdir, recursive = TRUE, force = TRUE), silent = TRUE), add = TRUE)
  utils::unzip(zf, exdir = exdir, overwrite = TRUE)
  csvs <- list.files(exdir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(csvs)) return(tibble::tibble())
  .pl_read_hist_csv(csvs[1])
}

# Project a WIDE master object to DK schema for a single parameter + window + stations
.project_master <- function(master_obj, parameter, st_ids, win_start, win_end, x, restrict = FALSE) {
  wide <- master_obj$data_wide
  meta <- master_obj$meta %||% list(country = x$country, provider_id = x$provider_id, provider_name = x$provider_name)

  # Station filter (only if user actually restricted)
  if (isTRUE(restrict)) {
    wide <- wide[wide$station_id %in% st_ids, , drop = FALSE]
    if (!nrow(wide)) return(.empty_ts_PL_IMGW(x))
  }

  # Time window
  keep <- wide$timestamp >= win_start & wide$timestamp <= win_end
  wide <- wide[keep, , drop = FALSE]
  if (!nrow(wide)) return(.empty_ts_PL_IMGW(x))

  # Pick the value column + unit for requested parameter
  col_map <- list(
    water_level       = list(col = "wl_cm", unit = "cm"),
    water_discharge   = list(col = "q_m3s", unit = "m3/s"),
    water_temperature = list(col = "tw_c",  unit = "\u00B0C")
  )
  cm   <- col_map[[parameter]]
  vals <- wide[[cm$col]]

  # Build DK schema
  out <- tibble::tibble(
    country       = meta$country,
    provider_id   = meta$provider_id,
    provider_name = meta$provider_name,
    station_id    = as.character(wide$station_id),
    parameter     = parameter,
    timestamp     = as.POSIXct(wide$timestamp, tz = "UTC"),
    value         = suppressWarnings(as.numeric(vals)),
    unit          = cm$unit,
    quality_code  = NA_character_,
    quality_name  = NA_character_,
    quality_desc  = NA_character_,
    source_url    = as.character(wide$source_url)
  )

  # Drop all-NA values for this parameter
  out <- out[!is.na(out$value), , drop = FALSE]
  if (!nrow(out)) return(.empty_ts_PL_IMGW(x))

  # Order + dedup (safety)
  out <- out[order(out$station_id, out$timestamp), , drop = FALSE]
  out <- out[!duplicated(out[c("station_id", "timestamp", "parameter")]), , drop = FALSE]
  out
}



#' @export
timeseries.hydro_service_PL_IMGW <- function(x,
                                             parameter = c("water_discharge","water_level","water_temperature"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             cache_dir = NULL, update = FALSE,
                                             prefer_master = TRUE, save_master = FALSE, ...
) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  # ---- Window (force earliest start to 1951-01-01) ----
  rng <- resolve_dates(mode, start_date, end_date)
  rng$start_date <- as.Date("1951-01-01")
  win_start <- as.POSIXct(paste0(format(rng$start_date, "%Y-%m-%d"), " 00:00:00"), tz = "UTC")
  win_end   <- as.POSIXct(paste0(format(rng$end_date,   "%Y-%m-%d"), " 23:59:59"), tz = "UTC")

  # ---- Stations & validation ----
  st_all <- stations.hydro_service_PL_IMGW(x)
  if (!nrow(st_all)) return(.empty_ts_PL_IMGW(x))

  if (is.null(stations) || !length(stations)) {
    st_ids <- unique(as.character(st_all$station_id))
  } else {
    user_ids <- unique(as.character(stations))
    allowed  <- unique(as.character(st_all$station_id))
    invalid  <- setdiff(user_ids, allowed)
    if (length(invalid)) {
      rlang::warn(paste0(
        "PL_IMGW: dropped ", length(invalid), " invalid station id(s). Examples: ",
        paste(utils::head(invalid, 5), collapse = ", "),
        if (length(invalid) > 5) paste0(" ... (+", length(invalid) - 5, " more)") else ""
      ))
    }
    st_ids <- intersect(user_ids, allowed)
  }
  if (!length(st_ids)) return(.empty_ts_PL_IMGW(x))
  restrict <- length(stations) > 0L

  # ---- Fast path: use existing WIDE master (unless update=TRUE or prefer_master=FALSE) ----
  if (!update && prefer_master) {
    master <- .pl_master_load(cache_dir)
    if (!is.null(master) && identical(master$type, "wide") && !is.null(master$data_wide)) {
      .pl_master_note(cache_dir)
      return(.project_master(master, parameter, st_ids, win_start, win_end, x, restrict))
    }
  }

  # ---- Build/refresh master from source when:
  #      - update = TRUE, OR
  #      - no master exists (regardless of prefer_master)
  need_build <- isTRUE(update)
  if (!need_build) {
    cur0 <- .pl_master_load(cache_dir)
    need_build <- is.null(cur0)
  }

  if (need_build) {
    y_from <- 1951L
    y_to   <- as.integer(format(win_end, "%Y"))
    rlang::inform(sprintf(
      "PL_IMGW: building WIDE master cache from %d to %d (one-time build; later calls use the cache).",
      y_from, y_to
    ))
    invisible(.pl_build_master_from_source(x, cache_dir = cache_dir, from_year = y_from, to_year = y_to))
  }

  # ---- Use (new or existing) master
  cur <- .pl_master_load(cache_dir)
  if (is.null(cur) || is.null(cur$data_wide)) return(.empty_ts_PL_IMGW(x))
  .pl_master_note(cache_dir)
  .project_master(cur, parameter, st_ids, win_start, win_end, x, restrict)
}


# -----------------------------------------------------------------------------
# Empty TS helper (private)
# -----------------------------------------------------------------------------

.empty_ts_PL_IMGW <- function(x) {
  tibble::tibble(
    country      = x$country %||% "PL",
    provider_id  = x$provider_id %||% "PL_IMGW",
    provider_name= x$provider_name %||% "Poland - IMGW Public Data",
    station_id   = character(),
    parameter    = character(),
    timestamp    = as.POSIXct(character()),
    value        = numeric(),
    unit         = character(),
    quality_code = character(),
    quality_name = character(),
    quality_desc = character(),
    source_url   = character()
  )
}
