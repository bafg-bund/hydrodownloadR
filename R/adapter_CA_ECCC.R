# R/adapter_CA_ECCC.R
# Canada - Environment & Climate Change Canada (ECCC) Hydrometric adapter
# Option A (default now): HYDAT (SQLite) - offline, cached
# Option B (fallback): OGC API - Features (stations + time series)
#
# This update adds HYDAT database bootstrapping:
# - Detect cached DB (user cache dir) or download the latest date-stamped zip
# - Unzip to cache and return the SQLite path
# - Expose DB path via `source_url = file://...` in the timeseries tibble
#
# Notes
# - Authentication: none
# - Time zone: UTC
# - Parameters supported (initial scaffolding): water_discharge (m^3/s), water_level (m)
# - Intervals scaffolded: "daily" (mean). (Hourly/instantaneous via OGC can be re-enabled later.)
# - We keep the OGC helpers for future use, but HYDAT is now the primary path.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_CA_ECCC <- function() {
  register_service_usage(
    provider_id   = "CA_ECCC",
    provider_name = "ECCC Hydrometric (HYDAT/OGC)",
    country       = "Canada",
    base_url      = "https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/Hydat_sqlite3_YYYYMMDD.zip",
    rate_cfg      = list(n = 10, period = 1),
    auth          = list(type = "none")
  )
}

# Keep parameter list discoverable
#' @export
timeseries_parameters.hydro_service_CA_ECCC <- function(x, ...) {
  c("water_discharge", "water_level")
}

# -----------------------------------------------------------------------------
# Constants / parameter mapping (private)
# -----------------------------------------------------------------------------

# HYDAT zip URL template (date-stamped): YYYYMMDD
HYDAT_URL_TEMPLATE <- "https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/Hydat_sqlite3_%s.zip"
HYDAT_SQLITE_BASENAMES <- c("Hydat.sqlite3", "hydat.sqlite3", "HYDAT.sqlite3")

# OGC (kept for future online pulls)
COLLECTION_STATIONS       <- "stations"
COLLECTION_HOURLY_MEAN    <- "hydrometric-mean-hourly"
COLLECTION_DAILY_MEAN     <- "hydrometric-daily-mean"

.ca_param_map <- function(parameter) {
  parameter <- match.arg(parameter, c("water_discharge", "water_level"))
  if (parameter == "water_discharge") {
    list(
      unit = "m^3/s",
      to_canon = function(x, raw_unit = NULL) suppressWarnings(as.numeric(x)),
      table = "DLY_FLOWS",
      value_pref = "FLOW",
      qf_pref = "FLOW_SYMBOL"
    )
  } else { # water_level
    list(
      unit = "m",
      to_canon = function(x, raw_unit = NULL) suppressWarnings(as.numeric(x)),
      table = "DLY_LEVELS",
      value_pref = "LEVEL",
      qf_pref = "LEVEL_SYMBOL"
    )
  }
}

# -----------------------------------------------------------------------------
# HYDAT cache & setup (private)
# -----------------------------------------------------------------------------

.ca_hydat_cache_dir <- function() {
  # Canonical path: <cache-base>/hydrodownloadR/CA_ECCC
  # On Windows this is typically: C:/Users/<you>/AppData/Local/Cache/hydrodownloadR/CA_ECCC
  base <- tryCatch(rappdirs::user_cache_dir(), error = function(e) NULL)
  if (is.null(base) || !nzchar(base)) base <- file.path(tempdir(), "hydrodownloadR_cache_base")
  canonical <- file.path(base, "hydrodownloadR", "CA_ECCC")

  # Legacy paths created by earlier versions
  legacy_paths <- unique(na.omit(c(
    tryCatch(rappdirs::user_cache_dir("hydrodownloadR/CA_ECCC"), error = function(e) NA_character_),
    tryCatch(file.path(rappdirs::user_cache_dir("hydrodownloadR"), "CA_ECCC"), error = function(e) NA_character_)
  )))

  # Prefer an existing legacy dir to avoid breaking existing caches
  dir <- canonical
  for (lp in legacy_paths) {
    if (!is.na(lp) && dir.exists(lp)) { dir <- lp; break }
  }

  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}



.ca_hydat_sqlite_path <- function(cache_dir = .ca_hydat_cache_dir()) {
  # return first matching sqlite path in cache
  cand <- file.path(cache_dir, HYDAT_SQLITE_BASENAMES)
  cand[file.exists(cand)][1]
}

.ca_format_hydat_url <- function(date_ymd) {
  sprintf(HYDAT_URL_TEMPLATE, format(as.Date(date_ymd), "%Y%m%d"))
}

.ca_find_latest_hydat_url <- function(max_back_days = 365) {
  # Strategy: if the user supplies options("CA_ECCC.hydat_date"), use it.
  # Otherwise, probe backwards by day until HEAD 200 (up to max_back_days).
  opt_date <- getOption("CA_ECCC.hydat_date", default = NA)
  if (!is.na(opt_date)) return(.ca_format_hydat_url(as.Date(opt_date)))

  today <- Sys.Date()
  for (d in 0:max_back_days) {
    url <- .ca_format_hydat_url(today - d)
    ok <- try({
      resp <- httr2::request(url) |> httr2::req_method("HEAD") |> httr2::req_perform()
      status <- httr2::resp_status(resp)
      status >= 200 && status < 300
    }, silent = TRUE)
    if (isTRUE(ok)) return(url)
  }
  # fall back to today even if not verified; download may 404 (we'll error then)
  .ca_format_hydat_url(today)
}

.ca_download_and_unzip_hydat <- function(cache_dir = .ca_hydat_cache_dir(), force = FALSE) {
  sqlite <- .ca_hydat_sqlite_path(cache_dir)
  if (!isTRUE(force) && length(sqlite) == 1 && file.exists(sqlite)) return(sqlite)

  url <- .ca_find_latest_hydat_url()
  zip_path <- file.path(cache_dir, basename(url))

  # download (inform user - large file, slow)
  cli::cli_inform(c(
    "i" = "Downloading HYDAT SQLite archive - this can take ~10 minutes...",
    " " = url,
    " " = paste0("Destination: ", zip_path)
  ))

  resp <- httr2::request(url) |> httr2::req_perform()
  if (httr2::resp_status(resp) < 200 || httr2::resp_status(resp) >= 300) {
    stop(sprintf("HYDAT download failed: HTTP %s for %s", httr2::resp_status(resp), url))
  }
  writeBin(httr2::resp_body_raw(resp), zip_path)

  # unzip: look for any *.sqlite3 inside (inform user)
  cli::cli_inform(c(
    "i" = "Unzipping HYDAT archive...",
    " " = paste0("Extracting to: ", cache_dir)
  ))
  utils::unzip(zip_path, exdir = cache_dir)

  # after unzip, re-check for sqlite
  sqlite <- .ca_hydat_sqlite_path(cache_dir)
  if (is.na(sqlite) || is.null(sqlite) || !file.exists(sqlite)) {
    # try to locate any sqlite3 file
    all <- list.files(cache_dir, pattern = "\\.sqlite3$", full.names = TRUE, recursive = TRUE)
    if (length(all)) sqlite <- all[1]
  }
  if (is.na(sqlite) || is.null(sqlite) || !file.exists(sqlite)) {
    stop("HYDAT unzip succeeded but no *.sqlite3 file was found in the archive.")
  }

  cli::cli_inform(c(
    "i" = "HYDAT ready.",
    " " = paste0("SQLite path: ", sqlite)
  ))

  sqlite
}

.ca_ensure_hydat_sqlite <- function(force = FALSE) {
  sqlite <- .ca_hydat_sqlite_path()

  if (!isTRUE(force) && !is.null(sqlite) && file.exists(sqlite)) {
    age_days <- as.numeric(Sys.time() - file.info(sqlite)$mtime, units = "days")

    if (!is.na(age_days) && age_days > 90) {
      cli::cli_inform(c(
        "i" = "Using cached HYDAT database.",
        " " = paste0("Cached file age: ", round(age_days), " days."),
        " " = "If you want to ensure you are using the latest HYDAT release, run:",
        " " = "hydrodownloadR:::.ca_ensure_hydat_sqlite(force = TRUE)"
      ))
    }

    return(sqlite)
  }

  .ca_download_and_unzip_hydat(force = force)
}

# ---- HYDAT in-memory / on-disk table cache ----------------------------------

.ca_cache_state <- local({
  e <- new.env(parent = emptyenv())
  e$db_tag <- NULL         # identifies which HYDAT DB the cache corresponds to
  e$tables <- new.env(parent = emptyenv())  # per-table in-memory cache
  e
})

.ca_db_tag <- function(sqlite) {
  # Tag changes when the SQLite file changes; used to invalidate caches
  info <- file.info(sqlite)
  paste0(basename(sqlite), "::", as.integer(info$size), "::", as.integer(info$mtime))
}

.ca_table_cache_dir <- function() {
  dir <- file.path(.ca_hydat_cache_dir(), "tables_cache")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

.ca_table_cache_path <- function(sqlite, table) {
  tag <- gsub("[^A-Za-z0-9_.-]", "_", .ca_db_tag(sqlite))
  file.path(.ca_table_cache_dir(), sprintf("%s__%s.rds", tag, table))
}

.ca_load_table_cached <- function(sqlite, table, con = NULL) {
  # invalidate session cache if db changed
  tag <- .ca_db_tag(sqlite)
  if (!identical(.ca_cache_state$db_tag, tag)) {
    .ca_cache_state$tables <- new.env(parent = emptyenv())
    .ca_cache_state$db_tag <- tag
  }

  # 1) In-memory cache?
  if (exists(table, envir = .ca_cache_state$tables, inherits = FALSE)) {
    return(get(table, envir = .ca_cache_state$tables, inherits = FALSE))
  }

  # 2) On-disk cache?
  rds <- .ca_table_cache_path(sqlite, table)
  if (file.exists(rds)) {
    df <- readRDS(rds)
    assign(table, df, envir = .ca_cache_state$tables)
    return(df)
  }

  # 3) Read from SQLite once, then persist to RDS and memory
  own_con <- is.null(con)
  if (own_con) {
    con <- DBI::dbConnect(RSQLite::SQLite(), sqlite)
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)
  }

  cli::cli_inform(c("i" = sprintf("CA_ECCC: reading '%s' from HYDAT (first use)...", table)))
  df <- DBI::dbReadTable(con, table)

  saveRDS(df, rds)
  assign(table, df, envir = .ca_cache_state$tables)
  df
}

# --- persistent SQLite connection cache (per HYDAT file) ---------------------
.ca_con_state <- local({
  e <- new.env(parent = emptyenv())
  e$sqlite_path <- NULL
  e$con <- NULL
  e
})

.ca_get_sqlite_con <- function(sqlite) {
  # If we already have a live connection to the same file, reuse it
  if (!is.null(.ca_con_state$con) &&
      DBI::dbIsValid(.ca_con_state$con) &&
      identical(.ca_con_state$sqlite_path, normalizePath(sqlite, winslash = "/", mustWork = FALSE))) {
    return(.ca_con_state$con)
  }

  # Otherwise, close any old one and open a new one
  if (!is.null(.ca_con_state$con)) {
    try(DBI::dbDisconnect(.ca_con_state$con), silent = TRUE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite)
  .ca_con_state$con <- con
  .ca_con_state$sqlite_path <- normalizePath(sqlite, winslash = "/", mustWork = FALSE)

  # Ensure we clean up on R session end
  reg.finalizer(.ca_con_state, function(e) {
    if (!is.null(e$con)) try(DBI::dbDisconnect(e$con), silent = TRUE)
  }, onexit = TRUE)

  con
}


#Return a standardized empty time series tibble (zero rows)
.ca_empty_ts <- function(x, parameter, unit, source_url = character(0)) {
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
    quality_name  = NA_character_,
    quality_desc  = NA_character_,
    source_url    = character(0)
  )
}


# -----------------------------------------------------------------------------
# Stations (HYDAT)
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_CA_ECCC <- function(x, stations = NULL, ...) {
  # HYDAT: read from STATIONS table via DBI if available
  sqlite <- .ca_ensure_hydat_sqlite()
  con <- DBI::dbConnect(RSQLite::SQLite(), sqlite)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE))

  # STATIONS schema varies slightly by vintage; we pick common columns.
  cols <- c("STATION_NUMBER", "STATION_NAME", "LATITUDE", "LONGITUDE",
            "HYD_STATUS", "DRAINAGE_AREA_GROSS", "REAL_TIME")
  have <- intersect(cols, DBI::dbListFields(con, "STATIONS"))
  df <- DBI::dbReadTable(con, "STATIONS")[, have, drop = FALSE]

  # Optional columns that may not exist in STATIONS; provide safe fallbacks
  if (!"masl" %in% names(df)) df$masl <- NA_real_

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = df$STATION_NUMBER,
    station_name  = df$STATION_NAME,
    lat           = df$LATITUDE,
    lon           = df$LONGITUDE,
    area          = suppressWarnings(as.numeric(df$DRAINAGE_AREA_GROSS)),
    altitude      = suppressWarnings(as.numeric(df$masl)),
    real_time     = df$REAL_TIME,
    status        = df$HYD_STATUS,
    status_desc   = dplyr::case_when(
      df$HYD_STATUS == "A" ~ "Active",
      df$HYD_STATUS == "D" ~ "Discontinued",
      TRUE ~ NA_character_
    )
  )
  if (!is.null(stations)) out <- dplyr::filter(out, .data$station_id %in% stations)
  out
}


#' @export
timeseries.hydro_service_CA_ECCC <- function(x,
                                             parameter = c("water_discharge","water_level"),
                                             stations = NULL,
                                             start_date = NULL,
                                             end_date   = NULL,
                                             mode = c("complete","range"),
                                             station_cap = 100,
                                             ...) {
  pm   <- .ca_param_map(parameter)
  mode <- match.arg(mode)

  # Resolve the date window like other adapters
  rng <- resolve_dates(mode, start_date, end_date)

  # Enforce range requirements
  if (identical(mode, "range") && (is.null(start_date) || is.null(end_date))) {
    rlang::abort("CA_ECCC: For mode='range', both start_date and end_date must be provided (YYYY-MM-DD).")
  }

  # Determine station set with cap handling (similar to AU adapter)
  if (is.null(stations) || !length(stations)) {
    st  <- stations.hydro_service_CA_ECCC(x)
    ids <- unique(stats::na.omit(as.character(st$station_id)))
    if (!length(ids)) return(.ca_empty_ts(x, parameter, pm$unit))
    if (length(ids) > station_cap) {
      rlang::warn(sprintf("CA_ECCC: %s stations available; limiting to the first %s for this call.",
                          format(length(ids), big.mark=","), format(station_cap, big.mark=",")))
      rlang::inform("Tip: pass 'stations' explicitly and iterate in batches to cover all stations.")
      ids <- ids[seq_len(station_cap)]
    }
    stations <- ids
  } else {
    stations <- unique(as.character(stations))
    if (length(stations) > station_cap) {
      rlang::abort(sprintf("CA_ECCC: Too many stations in one call (%s). Split into batches of up to %s.",
                           format(length(stations), big.mark=","), format(station_cap, big.mark=",")))
    }
  }

  # Ensure HYDAT is ready and capture provenance
  sqlite     <- .ca_ensure_hydat_sqlite()
  source_url <- paste0("file://", normalizePath(sqlite, winslash = "/", mustWork = FALSE))  # <- replace call


  con <- .ca_get_sqlite_con(sqlite)

  day_cols  <- paste0(pm$value_pref, 1:31)  # FLOW1..FLOW31 or LEVEL1..31
  sym_cols  <- paste0(pm$qf_pref,    1:31)  # FLOW_SYMBOL1..31 or LEVEL_SYMBOL1..31
  base_cols <- c("STATION_NUMBER", "YEAR", "MONTH", "NO_DAYS")
  want_cols <- c(base_cols, day_cols, sym_cols)

  # Cached load of whole table (fast after first use)
  df_full <- .ca_load_table_cached(sqlite, pm$table, con)
  have    <- intersect(want_cols, names(df_full))
  if (!length(have)) {
    return(.ca_empty_ts(x, parameter, pm$unit, source_url))
  }

  df <- df_full[, have, drop = FALSE]

  # ---- FILTER EARLY: only requested stations ----
  if (!is.null(stations) && length(stations)) {
    df <- df[df$STATION_NUMBER %in% stations, , drop = FALSE]
  }
  if (!nrow(df)) {
    return(.ca_empty_ts(x, parameter, pm$unit, source_url))
  }

  # ---- OPTIONAL PRE-FILTER BY YEAR-MONTH FOR RANGE ----
  if (identical(mode, "range")) {
    s <- as.Date(rng$start); e <- as.Date(rng$end)
    if (!is.na(s) || !is.na(e)) {
      ym <- df$YEAR * 100L + df$MONTH
      if (!is.na(s)) {
        s_ym <- as.integer(format(s, "%Y")) * 100L + as.integer(format(s, "%m"))
        df   <- df[ym >= s_ym, , drop = FALSE]
      }
      if (!is.na(e) && nrow(df)) {
        ym   <- df$YEAR * 100L + df$MONTH
        e_ym <- as.integer(format(e, "%Y")) * 100L + as.integer(format(e, "%m"))
        df   <- df[ym <= e_ym, , drop = FALSE]
      }
    }
    if (!nrow(df)) {
      return(.ca_empty_ts(x, parameter, pm$unit, source_url))
    }
  }

  # ---- FAST WIDE -> LONG (vectorized base R; no per-row loop, no data.table) ----
  n <- nrow(df)
  if (n == 0L) {
    return(.ca_empty_ts(x, parameter, pm$unit, source_url))
  }

  # Ensure all expected day/symbol columns exist and map them to indices 1..31
  present_day <- intersect(day_cols, names(df))
  present_sym <- intersect(sym_cols, names(df))

  vals_m <- matrix(NA_real_,      nrow = n, ncol = 31)
  qfs_m  <- matrix(NA_character_, nrow = n, ncol = 31)

  if (length(present_day)) {

    day_idx <- as.integer(gsub("[^0-9]", "", present_day))
    vals_m[, day_idx] <- as.matrix(df[, present_day, drop = FALSE])
  }

  if (length(present_sym)) {
    sym_idx <- as.integer(gsub("[^0-9]", "", present_sym))
    qfs_m[, sym_idx] <- as.matrix(df[, present_sym, drop = FALSE])
  }

  nd <- as.integer(df$NO_DAYS)
  nd[is.na(nd)] <- 31L

  # Matrix of day indices: 1..31 repeated for each row (no rep.int(each=...))
  day_idx_mat <- matrix(1:31, nrow = n, ncol = 31, byrow = TRUE)

  keep <- (day_idx_mat <= nd) & !is.na(vals_m)
  if (!any(keep)) {
    return(.ca_empty_ts(x, parameter, pm$unit, source_url))
  }

  idx   <- which(keep, arr.ind = TRUE)
  row_i <- idx[, 1]
  day_i <- idx[, 2]

  # Vectorized date construction
  month_start <- as.Date(sprintf("%04d-%02d-01", as.integer(df$YEAR), as.integer(df$MONTH)))
  ts_vec <- month_start[row_i] + (day_i - 1L)

  val_vec <- pm$to_canon(vals_m[keep], NULL)
  qf_vec  <- qfs_m[keep]

  long <- tibble::tibble(
    STATION_NUMBER = df$STATION_NUMBER[row_i],
    timestamp      = as.POSIXct(ts_vec, tz = "UTC"),
    value          = val_vec,
    quality_code   = if (length(qf_vec)) as.character(qf_vec) else NA_character_
  )

  # Apply range filter precisely (day-level) if requested
  if (identical(mode, "range")) {
    s <- as.Date(rng$start); e <- as.Date(rng$end)
    if (!is.na(s)) long <- long[long$timestamp >= as.POSIXct(s, tz = "UTC"), , drop = FALSE]
    if (!is.na(e)) long <- long[long$timestamp <= as.POSIXct(e + 1, tz = "UTC") - 1, , drop = FALSE]
  }

  if (!nrow(long)) {
    return(.ca_empty_ts(x, parameter, pm$unit, source_url))
  }

  # quality flag descriptions: DATA_SYMBOLS(SYMBOL_ID -> SYMBOL_EN)
  desc_map <- tryCatch({
    lut <- .ca_load_table_cached(sqlite, "DATA_SYMBOLS", con)
    lut <- lut[, intersect(c("SYMBOL_ID","SYMBOL_EN"), names(lut)), drop = FALSE]
    if (!all(c("SYMBOL_ID","SYMBOL_EN") %in% names(lut))) NULL else stats::setNames(lut$SYMBOL_EN, lut$SYMBOL_ID)
  }, error = function(e) NULL)

  long |>
    dplyr::arrange(.data$STATION_NUMBER, .data$timestamp) |>
    dplyr::rename(station_id = .data$STATION_NUMBER) |>
    dplyr::mutate(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      parameter     = parameter,
      unit          = pm$unit,
      quality_code  = if (is.null(desc_map)) NA_character_ else unname(desc_map[.data$quality_code]),
      quality_name  = NA_character_,
      quality_desc  = NA_character_,
      .before = 1
    ) |>
    dplyr::mutate(source_url = source_url) |>
    dplyr::select(country, provider_id, provider_name, station_id, parameter,
                  timestamp, value, unit, quality_code, quality_name,
                  quality_desc, source_url)
}
