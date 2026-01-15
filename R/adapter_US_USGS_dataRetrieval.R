# R/adapter_US_USGS_DR.R
# United States – USGS NWIS adapter (via dataRetrieval)
# Docs: https://doi-usgs.github.io/dataRetrieval/
#
# Auth (recommended): set a PAT for modern USGS APIs
#   Sys.setenv(API_USGS_PAT = "<your-token>")
#   options(API_USGS_PAT = "<your-token>")
#
# Notes
# - Stations: iterate over STATE (ANSI/FIPS two-digit; "01".."78"), no county fallback.
#   If a state request fails or returns zero rows, we wait 300 seconds and continue.
# - First build or update=TRUE writes a dated snapshot under ~/hydrodownloadR/data.
# - Daily Values (DV) via readNWISdv(); timestamps standardized to UTC midnight.
# - Parameters: water_discharge (00060 ft^3/s -> m^3/s), water_level (00065 ft -> m).
#
# Depends: dataRetrieval, tibble, dplyr, rlang, rappdirs
# Optional: httr, sf, cli, purrr

# ------------------------------------------------------------------------------
# Registration
# ------------------------------------------------------------------------------
#' @export
register_US_USGS_DR <- function() {
  register_service(
    provider_id   = "US_USGS_DR",
    provider_name = "United States – USGS dataRetrieval",
    country       = "US",
    base_url      = "https://waterservices.usgs.gov/nwis/",
    rate_cfg      = list(n = 10, period = 1),
    auth          = list(type = "header", header = "X-Api-Key")
  )
}

#' @export
timeseries_parameters.hydro_service_US_USGS_DR <- function(x, ...) {
  c("water_discharge", "water_level")
}

# ------------------------------------------------------------------------------
# Parameter mapping (switch)
# ------------------------------------------------------------------------------
.usgs_param_map <- function(parameter) {
  parameter <- match.arg(parameter, c("water_discharge", "water_level"))
  switch(
    parameter,
    water_discharge = list(
      parameter  = "water_discharge",
      nwis_pcode = "00060",
      unit       = "m^3/s",
      to_canon   = function(val) as.numeric(val) * 0.028316846592
    ),
    water_level = list(
      parameter  = "water_level",
      nwis_pcode = "00065",
      unit       = "m",
      to_canon   = function(val) as.numeric(val) * 0.3048
    )
  )
}

# ------------------------------------------------------------------------------
# API token passthrough
# ------------------------------------------------------------------------------
.usgs_pat <- function() {
  tok <- getOption("API_USGS_PAT", Sys.getenv("API_USGS_PAT", ""))
  if (!nzchar(tok)) return(NULL)
  tok
}

.usgs_with_key <- function(expr) {
  tok <- .usgs_pat()
  if (is.null(tok)) return(force(expr))
  if (requireNamespace("httr", quietly = TRUE)) {
    httr::with_config(
      httr::add_headers("X-Api-Key" = tok, "X-API-Key" = tok),
      force(expr)
    )
  } else force(expr)
}

# ------------------------------------------------------------------------------
# Cache dirs (canonical + legacy fallback; like CA_ECCC)
# ------------------------------------------------------------------------------
.usgs_cache_dir <- function() {
  base <- tryCatch(rappdirs::user_cache_dir(), error = function(e) NULL)
  if (is.null(base) || !nzchar(base)) base <- file.path(tempdir(), "hydrodownloadR_cache_base")
  canonical <- file.path(base, "hydrodownloadR", "US_USGS")

  legacy_paths <- unique(na.omit(c(
    tryCatch(rappdirs::user_cache_dir("hydrodownloadR/US_USGS"), error = function(e) NA_character_),
    tryCatch(file.path(rappdirs::user_cache_dir("hydrodownloadR"), "US_USGS"), error = function(e) NA_character_)
  )))

  dir <- canonical
  for (lp in legacy_paths) if (!is.na(lp) && dir.exists(lp)) { dir <- lp; break }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}


# ------------------------------------------------------------------------------
# Persistent snapshots under ~/hydrodownloadR/data
# ------------------------------------------------------------------------------
# Dated snapshots now live in the cache dir alongside stations_usgs_cache.rds
.usgs_snapshot_path <- function(date = Sys.Date()) {
  fn <- paste0("US_USGS_stations_", format(as.Date(date), "%Y-%m-%d"), ".rds")
  file.path(.usgs_cache_dir(), fn)
}

.usgs_list_snapshots <- function() {
  list.files(.usgs_cache_dir(),
             pattern = "^US_USGS_stations_\\d{4}-\\d{2}-\\d{2}\\.rds$",
             full.names = TRUE)
}

.usgs_latest_snapshot_file <- function() {
  files <- .usgs_list_snapshots()
  if (!length(files)) return(NA_character_)
  dates <- sub("^.*US_USGS_stations_(\\d{4}-\\d{2}-\\d{2})\\.rds$", "\\1", files)
  files[which.max(as.Date(dates, format = "%Y-%m-%d"))]
}

.usgs_latest_snapshot_date <- function() {
  files <- .usgs_list_snapshots()
  if (!length(files)) return(NA)
  dates <- sub("^.*US_USGS_stations_(\\d{4}-\\d{2}-\\d{2})\\.rds$", "\\1", files)
  max(as.Date(dates, format = "%Y-%m-%d"), na.rm = TRUE)
}

.usgs_save_rds_atomic <- function(obj, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".tmp")
  saveRDS(obj, tmp)
  if (file.exists(path)) unlink(path)
  file.rename(tmp, path)
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Helpers for station fetching
# ------------------------------------------------------------------------------
.usgs_has_monitoring_location <- function() {
  tryCatch({
    exists("read_waterdata_monitoring_location", where = asNamespace("dataRetrieval"), inherits = FALSE)
  }, error = function(e) FALSE)
}

# state table: drop NA STUSAB; keep 50 states + DC + GU/MP/PR/VI
.usgs_state_table <- function() {
  tbl <- tryCatch(get("stateCd", envir = asNamespace("dataRetrieval")), error = function(e) NULL)
  if (is.null(tbl)) tbl <- tryCatch(dataRetrieval::stateCd, error = function(e) NULL)
  if (is.null(tbl)) rlang::abort("US_USGS: could not access dataRetrieval::stateCd.")

  allowed <- c(state.abb, "DC", "GU", "MP", "PR", "VI")
  tbl <- subset(tbl, !is.na(STUSAB) & STUSAB %in% allowed)
  tbl <- tbl[!is.na(tbl$STATE), , drop = FALSE]
  tbl <- tbl[order(as.integer(tbl$STATE)), , drop = FALSE]
  tbl
}

# one state request (auto-paginated by dataRetrieval)
.usgs_request_state <- function(state_code_numeric,
                                limit_per_page = 10000,
                                sleep_between = 0.4) {
  st_code <- sprintf("%02d", as.integer(state_code_numeric))
  props <- c("monitoring_location_number","monitoring_location_name",
             "state_name","drainage_area","altitude")
  one <- try({
    .usgs_with_key({
      dataRetrieval::read_waterdata_monitoring_location(
        state_code  = st_code,
        agency_code = "USGS",
        properties  = props,
        limit       = limit_per_page
      )
    })
  }, silent = TRUE)
  if (!inherits(one, "try-error") && !is.null(one)) Sys.sleep(sleep_between)
  if (inherits(one, "try-error")) NULL else one
}


# All states: no county fallback. If a state fails/returns 0 → wait 300s and queue it for a later pass.
# We run multiple passes with a long cooldown between passes to let the rate limit reset,
# and we SAVE PARTIAL RESULTS after each successful state.
.usgs_fetch_sites_by_state <- function(limit_per_page = 10000,
                                       sleep_between = 1.0,
                                       message_once   = TRUE,
                                       max_passes     = 3,
                                       fail_wait      = 300,
                                       pass_cooldown  = 900) {
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) {
    rlang::abort("US_USGS requires the 'dataRetrieval' package. Install with install.packages('dataRetrieval').")
  }
  if (!.usgs_has_monitoring_location()) return(NULL)

  st_tbl  <- .usgs_state_table()

  # keep STATE as character to preserve leading zeros ("01", "02", ...)
  pending <- st_tbl$STATE

  accum   <- NULL

  if (isTRUE(message_once)) {
    msg <- sprintf("Building USGS monitoring-location index across %d states/territories; this is large and can take ~1 hour...", length(pending))
    if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info(msg) else message(msg)
  }

  for (pass in seq_len(max_passes)) {
    if (pass > 1 && length(pending)) {
      if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info("Cooldown between passes: %d seconds", pass_cooldown)
      Sys.sleep(pass_cooldown)
    }

    if (!length(pending)) break
    if (requireNamespace("cli", quietly = TRUE)) cli::cli_alert_info("Pass {pass}/{max_passes} over {length(pending)} pending states...")

    # character vector to match `pending`
    next_pending <- character(0)

    for (st_num in pending) {
      # st_num is like "01"; keep a two-digit code for display
      st_code <- st_num
      st_name <- st_tbl$STATE_NAME[match(st_num, st_tbl$STATE)]

      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert("Requesting state {st_code} ({st_name})")
      } else {
        message("Requesting state ", st_code, " (", st_name, ")")
      }

      one <- .usgs_request_state(st_num, limit_per_page = limit_per_page, sleep_between = sleep_between)

      if (is.null(one) || !inherits(one, "sf") || nrow(one) == 0) {
        if (requireNamespace("cli", quietly = TRUE)) {
          cli::cli_alert_warning("State {st_code} returned no data or failed. Waiting {fail_wait}s, then queuing for later pass...")
        } else {
          message("State ", st_code, " failed/empty. Waiting ", fail_wait, "s, then queuing for later pass...")
        }
        Sys.sleep(fail_wait)
        next_pending <- c(next_pending, st_num) # keep as character
        next
      }

      coords <- sf::st_coordinates(one)
      st_df <- tibble::tibble(
        station_id       = as.character(one$monitoring_location_number),
        station_name     = as.character(one$monitoring_location_name),
        state_name       = as.character(one$state_name),
        state_code       = st_code,  # already "01".."78"
        lon              = suppressWarnings(as.numeric(coords[, 1])),
        lat              = suppressWarnings(as.numeric(coords[, 2])),
        area             = suppressWarnings(as.numeric(one$drainage_area)),
        altitude         = suppressWarnings(as.numeric(one$altitude))
      )

      accum <- .usgs_bind_dedupe(list(accum), st_df)
      .usgs_save_cache_partial(st_df)

      Sys.sleep(sleep_between)
    }

    pending <- unique(next_pending)

    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_info("End of pass {pass}. Remaining pending states: {length(pending)}")
    }
    if (!length(pending)) break
  }

  accum
}


.usgs_bind_dedupe <- function(df_list, extra_df = NULL) {
  parts <- Filter(Negate(is.null), df_list)
  if (!is.null(extra_df)) parts <- c(parts, list(extra_df))
  if (!length(parts)) return(NULL)
  out <- if (requireNamespace("purrr", quietly = TRUE)) purrr::list_rbind(parts) else do.call(rbind, parts)
  dplyr::distinct(out, .data$site_no, .keep_all = TRUE)
}

.usgs_save_cache_partial <- function(add_df) {
  # Append to cache incrementally so we never lose progress mid-run
  p <- .usgs_cache_path()
  cur <- if (file.exists(p)) tryCatch(readRDS(p), error = function(e) NULL) else NULL
  merged <- .usgs_bind_dedupe(list(cur), add_df)
  if (!is.null(merged)) saveRDS(merged, p)
  invisible(TRUE)
}

.usgs_latest_snapshot_file <- function() {
  files <- .usgs_list_snapshots()
  if (!length(files)) return(NA_character_)
  # pick file with the newest date in its name
  dates <- sub("^.*US_USGS_stations_(\\d{4}-\\d{2}-\\d{2})\\.rds$", "\\1", files)
  files[which.max(as.Date(dates, format = "%Y-%m-%d"))]
}

# Where a bundled snapshot would live inside the installed package
.usgs_bundled_snapshot_path <- function() {
  # Accept either dated or "latest" filenames
  p1 <- system.file("extdata", "US_USGS_stations_latest.rds.xz", package = utils::packageName())
  p2 <- system.file("extdata", "US_USGS_stations_latest.rds",    package = utils::packageName())
  if (nzchar(p1)) return(p1)
  if (nzchar(p2)) return(p2)
  # Fall back to first dated file if present
  all <- list.files(system.file("extdata", package = utils::packageName()),
                    pattern = "^US_USGS_stations_\\d{4}-\\d{2}-\\d{2}\\.rds(\\.xz)?$",
                    full.names = TRUE)
  if (length(all)) all[[which.max(as.numeric(gsub("\\D", "", basename(all))))]] else ""
}

# Best-effort date extraction from filename (e.g., US_USGS_stations_2025-11-18.rds.xz)
.usgs_date_from_filename <- function(path) {
  m <- regmatches(basename(path), regexpr("\\d{4}-\\d{2}-\\d{2}", basename(path)))
  if (length(m)) as.Date(m) else NA
}

.usgs_bundled_snapshot_data <- function() {
  # Try to access the lazy-loaded data object from the package namespace.
  # Works whether users attach the package or not.
  obj <- tryCatch(get("us_usgs_stations_meta", envir = asNamespace(utils::packageName())),
                  error = function(e) NULL)
  if (is.null(obj)) {
    # Fallback if the package name differs or in devtools load_all context
    obj <- tryCatch(get("us_usgs_stations_meta", envir = parent.env(environment())),
                    error = function(e) NULL)
  }
  obj
}


# ------------------------------------------------------------------------------
# Stations (cache + dated snapshot on first build or update)
# ------------------------------------------------------------------------------
#' @export
stations.hydro_service_US_USGS_DR <- function(x, stations = NULL, update = FALSE, ...) {
  snapshot_written <- FALSE
  snapshot_date    <- NA
  snapshot_path    <- NA_character_

  # 1) Fast path: use the latest on-disk snapshot (if not updating)
  if (!isTRUE(update)) {
    latest <- .usgs_latest_snapshot_file()
    if (!is.na(latest) && file.exists(latest)) {
      st <- tryCatch(readRDS(latest), error = function(e) NULL)
      if (!is.null(st) && nrow(st)) {
        snapshot_path <- latest
        snapshot_date <- .usgs_latest_snapshot_date()
      } else {
        st <- NULL
      }
    } else {
      st <- NULL
    }
  } else {
    st <- NULL
  }

  # 2) If no snapshot found and not updating, seed from packaged data (.rda)
  if (is.null(st) && !isTRUE(update)) {
    seed <- .usgs_bundled_snapshot_data()
    if (!is.null(seed) && nrow(seed)) {
      st <- seed
      sdate <- attr(st, "source_date", exact = TRUE)
      if (!is.null(sdate) && !is.na(sdate)) {
        snapshot_path <- .usgs_snapshot_path(as.Date(sdate))
        if (!file.exists(snapshot_path)) .usgs_save_rds_atomic(st, snapshot_path)
        snapshot_date <- as.Date(sdate)
      } else {
        snapshot_date <- NA
        snapshot_path <- NA_character_
      }
      if (requireNamespace("cli", quietly = TRUE)) {
        cli::cli_alert_success("Seeded USGS stations from packaged data (data/us_usgs_stations_meta.rda).")
        if (!is.na(snapshot_date)) cli::cli_alert_info("Snapshot date (packaged): {snapshot_date}.")
        if (!is.na(snapshot_path)) cli::cli_alert_info("Snapshot: {.path {normalizePath(snapshot_path, mustWork = FALSE)}}")
        cli::cli_alert_info("Tip: Use update = TRUE to rebuild from the API (may take ~1 hour).")
      }
    }
  }

  # 3) Build fresh from API when update = TRUE, or when nothing found to seed
  if (is.null(st)) {
    st <- try(.usgs_fetch_sites_by_state(), silent = TRUE)
    if (inherits(st, "try-error") || is.null(st) || !nrow(st)) {
      return(tibble::tibble())
    }
    st <- dplyr::distinct(st, .data$site_no, .keep_all = TRUE)

    snapshot_path <- .usgs_snapshot_path(Sys.Date())  # only on update/fresh build
    .usgs_save_rds_atomic(st, snapshot_path)
    snapshot_date   <- as.Date(Sys.Date())
    snapshot_written <- TRUE

    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_info("USGS stations were freshly built.")
      cli::cli_alert_info("Snapshot: {.path {normalizePath(snapshot_path, mustWork = FALSE)}}")
    }
  }

  # 4) Harmonize output (no HUC; includes state_name)
  out <- tibble::tibble(
    country       = "US",
    provider_id   = "US_USGS_DR",
    provider_name = "United States – USGS NWIS (dataRetrieval)",
    station_id    = as.character(st$station_id),
    station_name  = as.character(st$station_name),
    lat           = suppressWarnings(as.numeric(st$lat)),
    lon           = suppressWarnings(as.numeric(st$lon)),
    area          = suppressWarnings(as.numeric(st$area)),
    altitude      = suppressWarnings(as.numeric(st$altitude )),
    state_code      = as.character(st$state_code),
    state_name    = as.character(st$state_name)
  )

  if (!is.null(stations)) {
    ids <- unique(as.character(stations))
    out <- dplyr::filter(out, .data$station_id %in% ids)
  }

  # 5) Attach metadata (paths, date, and directory)
  attr(out, "stations_source_date")   <- snapshot_date
  attr(out, "stations_snapshot_path") <- snapshot_path
  attr(out, "stations_store_dir")     <- .usgs_cache_dir()

  if (requireNamespace("cli", quietly = TRUE)) {
    if (!is.na(snapshot_date))  cli::cli_alert_info("USGS stations snapshot date: {snapshot_date}.")
    if (!is.na(snapshot_path))  cli::cli_alert_info("Snapshot file: {.path {normalizePath(snapshot_path, mustWork = FALSE)}}")
    cli::cli_alert_info("Store dir: {.path {normalizePath(.usgs_cache_dir(), mustWork = FALSE)}}")
    if (isTRUE(update) && snapshot_written) cli::cli_alert_success("Wrote new snapshot.")
  }

  out
}


# ------------------------------------------------------------------------------
# Timeseries (Daily Values via readNWISdv)# --- helpers ------------------------------------------------------------------
.usgs_has_read_waterdata_daily <- function() {
  tryCatch(
    exists("read_waterdata_daily", where = asNamespace("dataRetrieval"), inherits = FALSE),
    error = function(e) FALSE
  )
}

.usgs_to_monitoring_ids <- function(stations) {
  ids <- as.character(stations)
  ifelse(grepl("^USGS-", ids), ids, paste0("USGS-", ids))
}

# --- timeseries (prefer new OGC API; fallback to legacy NWIS dv) ---------------
#' @export
timeseries.hydro_service_US_USGS_DR <- function(x,
                                             parameter = c("water_discharge","water_level"),
                                             stations = NULL,
                                             mode = c("complete","range"),
                                             start_date = NULL,
                                             end_date = NULL,
                                             ...) {
  if (!requireNamespace("dataRetrieval", quietly = TRUE)) {
    rlang::abort("US_USGS requires the 'dataRetrieval' package. Install with install.packages('dataRetrieval').")
  }
  parameter <- match.arg(parameter)
  pm        <- .usgs_param_map(parameter)
  mode      <- match.arg(mode)

  # date window
  rng <- resolve_dates(mode, start_date, end_date)
  start_str <- if (!is.null(rng$start)) format(as.Date(rng$start), "%Y-%m-%d") else "1900-01-01"
  end_str   <- if (!is.null(rng$end))   format(as.Date(rng$end),   "%Y-%m-%d") else format(Sys.Date(), "%Y-%m-%d")
  time_arg  <- paste0(start_str, "/", end_str)

  # station set
  if (is.null(stations) || !length(stations)) {
    st <- stations.hydro_service_US_USGS_DR(x)
    stations <- unique(stats::na.omit(as.character(st$station_id)))
    if (!length(stations)) {
      return(tibble::tibble(
        country      = character(0),
        provider_id  = character(0),
        provider_name= character(0),
        station_id   = character(0),
        parameter    = character(0),
        timestamp    = as.POSIXct(character(0), tz = "UTC"),
        value        = numeric(0),
        unit         = character(0),
        quality_code = character(0),
        qf_desc      = character(0),
        source_url   = character(0)
      ))
    }
  } else {
    stations <- unique(as.character(stations))
  }

  # ---------------- new API path (preferred) -----------------------------------
  if (.usgs_has_read_waterdata_daily()) {
    mloc_ids <- .usgs_to_monitoring_ids(stations)
    props <- c("monitoring_location_id","time","value","approval_status","qualifier")
    dv_raw <- .usgs_with_key({
      dataRetrieval::read_waterdata_daily(
        monitoring_location_id = mloc_ids,
        parameter_code         = pm$nwis_pcode,
        statistic_id           = "00003",           # daily mean
        time                   = time_arg,
        properties             = props,
        skipGeometry           = TRUE,
        convertType            = TRUE
      )
    })

    # If nothing (or error), fall back to legacy
    if (!inherits(dv_raw, "try-error") && !is.null(dv_raw) && nrow(dv_raw)) {
      # Normalize columns
      sid <- sub("^USGS-", "", as.character(dv_raw$monitoring_location_id))
      vals <- pm$to_canon(dv_raw$value)
      qual <- if ("qualifier" %in% names(dv_raw)) as.character(dv_raw$qualifier) else NA_character_
      appr <- if ("approval_status" %in% names(dv_raw)) as.character(dv_raw$approval_status) else NA_character_

      out <- tibble::tibble(
        country       = x$country,
        provider_id   = x$provider_id,
        provider_name = x$provider_name,
        station_id    = sid,
        parameter     = pm$parameter,
        timestamp     = as.POSIXct(as.Date(dv_raw$time), tz = "UTC"),
        value         = as.numeric(vals),
        unit          = pm$unit,
        quality_code  = appr,       # "Approved" / "Provisional"
        qf_desc       = qual,       # any additional qualifiers
        source_url    = "https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily"
      )

      if (identical(mode, "range")) {
        s <- as.Date(rng$start); e <- as.Date(rng$end)
        if (!is.na(s)) out <- out[out$timestamp >= as.POSIXct(s, tz = "UTC"), , drop = FALSE]
        if (!is.na(e)) out <- out[out$timestamp <= as.POSIXct(e + 1, tz = "UTC") - 1, , drop = FALSE]
      }

      return(dplyr::arrange(out, .data$station_id, .data$timestamp))
    }
  }

  # ---------------- legacy fallback (readNWISdv) --------------------------------
  # (kept for older dataRetrieval versions or temporary outages)
  dv_raw_old <- .usgs_with_key({
    dataRetrieval::readNWISdv(
      siteNumbers = stations,
      parameterCd = pm$nwis_pcode,
      startDate   = start_str,
      endDate     = end_str
    )
  })

  if (is.null(dv_raw_old) || !nrow(dv_raw_old)) {
    return(tibble::tibble(
      country      = character(0),
      provider_id  = character(0),
      provider_name= character(0),
      station_id   = character(0),
      parameter    = character(0),
      timestamp    = as.POSIXct(character(0), tz = "UTC"),
      value        = numeric(0),
      unit         = character(0),
      quality_code = character(0),
      qf_desc      = character(0),
      source_url   = character(0)
    ))
  }

  dv <- dataRetrieval::renameNWISColumns(dv_raw_old)

  value_col <- if (pm$nwis_pcode == "00060") {
    if ("Flow" %in% names(dv)) "Flow" else grep("^Flow", names(dv), value = TRUE)[1]
  } else {
    if ("Gage height" %in% names(dv)) "Gage height" else grep("^Gage height", names(dv), value = TRUE)[1]
  }

  qual_col <- {
    qs <- grep("_cd$|_cd\\b|Qual", names(dv), value = TRUE)
    if (!is.null(value_col) && nzchar(value_col)) {
      pref <- qs[startsWith(qs, value_col)]
      if (length(pref)) pref[1] else qs[1]
    } else qs[1]
  }

  if (is.null(value_col) || !nzchar(value_col)) {
    rlang::abort("US_USGS: could not locate the value column in NWIS DV response.")
  }

  vals <- pm$to_canon(dv[[value_col]])
  qf   <- if (!is.null(qual_col) && nzchar(qual_col) && qual_col %in% names(dv)) as.character(dv[[qual_col]]) else NA_character_

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = as.character(dv$site_no),
    parameter     = pm$parameter,
    timestamp     = as.POSIXct(as.Date(dv$Date), tz = "UTC"),
    value         = as.numeric(vals),
    unit          = pm$unit,
    quality_code  = qf,
    qf_desc       = NA_character_,
    source_url    = x$base_url
  )

  if (identical(mode, "range")) {
    s <- as.Date(rng$start); e <- as.Date(rng$end)
    if (!is.na(s)) out <- out[out$timestamp >= as.POSIXct(s, tz = "UTC"), , drop = FALSE]
    if (!is.na(e)) out <- out[out$timestamp <= as.POSIXct(e + 1, tz = "UTC") - 1, , drop = FALSE]
  }

  dplyr::arrange(out, .data$station_id, .data$timestamp)
}
