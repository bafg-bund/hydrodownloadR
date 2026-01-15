# ==== South Korea (WAMIS) adapter ============================================
# Stations:
#   - WAMIS water level observatories list:
#       http://www.wamis.go.kr:8444/wamis/openapi/wkw/wl_dubwlobs
#     Returns a list of observatory codes (obscd).
#
#   - Detailed observatory metadata:
#       http://www.wamis.go.kr:8444/wamis/openapi/wkw/wl_obsinfo?obscd=XXXX
#     Provides:
#       - wlobscd  : gauge id
#       - obsnmeng : station name (English)
#       - rivnm    : river name
#       - lat, lon : coordinates in D-M-S format "DDD-MM-SS"
#       - gdt      : zero-level elevation (EL.m)
#       - bsnara   : catchment area (km²)
#
#   Coordinates are converted from DMS to decimal degrees (WGS84).
#
# Time series:
#   - Daily discharge (flow, m³/s):
#       http://www.wamis.go.kr:8444/wamis/openapi/wkw/flw_dtdata
#       Parameters:
#         obscd = gauge id
#         year  = YYYY
#         output= json
#       Fields:
#         ymd : date (YYYYMMDD)
#         fw  : discharge (m³/s)
#
#   We loop by year between start_date and end_date and then
#   filter to the requested date range in R.
#
# Notes:
#   - Adapter currently exposes only parameter = "water_discharge".
#   - Station metadata from WAMIS water-level metadata endpoint; discharge
#     series from WAMIS discharge endpoint.
#   - Daily data are returned with timestamp at midnight UTC.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @export
register_KR_WAMIS <- function() {
  register_service(
    provider_id   = "KR_WAMIS",
    provider_name = "South Korea – Wter Management Information System (WAMIS) Open API",
    country       = "KR",
    base_url      = "http://www.wamis.go.kr:8444/wamis/openapi",
    rate_cfg      = list(n = 5L, period = 1),  # up to 5 req / second
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_KR_WAMIS <- function(x, ...) {
  # For now we only expose daily discharge.
  c("water_discharge")
}


# -----------------------------------------------------------------------------
# Station helpers (metadata via wl_dubwlobs + wl_obsinfo)
# -----------------------------------------------------------------------------

.kr_wamis_dms_to_dd <- function(x) {
  # Convert "DDD-MM-SS" string to decimal degrees (positive for N/E).
  if (is.null(x)) return(NA_real_)
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  out <- rep(NA_real_, length(x))
  ok  <- !is.na(x)
  if (!any(ok)) return(out)

  parts <- strsplit(x[ok], "-")
  idx_ok <- which(ok)

  for (i in seq_along(parts)) {
    p <- suppressWarnings(as.numeric(parts[[i]]))
    if (length(p) != 3L || any(is.na(p))) next
    out[idx_ok[i]] <- p[1] + p[2] / 60 + p[3] / 3600
  }

  out
}

.kr_wamis_base_url <- function(x = NULL) {
  if (!is.null(x) && !is.null(x$base_url) && nzchar(x$base_url)) {
    x$base_url
  } else {
    "http://www.wamis.go.kr:8444/wamis/openapi"
  }
}

.kr_wamis_station_list_json <- function(x) {
  base_url <- .kr_wamis_base_url(x)
  url      <- paste0(base_url, "/wkw/wl_dubwlobs")

  req <- httr2::request(url) |>
    httr2::req_url_query(output = "json") |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort("KR_WAMIS: failed to fetch wl_dubwlobs station list.")
  }

  body <- try(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    silent = TRUE
  )
  if (inherits(body, "try-error") || !is.list(body) || is.null(body$list)) {
    rlang::abort("KR_WAMIS: unexpected JSON structure for wl_dubwlobs.")
  }

  st_list <- body$list
  if (!inherits(st_list, "data.frame") || !NROW(st_list)) {
    rlang::abort("KR_WAMIS: wl_dubwlobs returned no station records.")
  }

  tibble::as_tibble(st_list)
}

.kr_wamis_fetch_obsinfo_one <- function(base_url, obscd) {
  url <- paste0(base_url, "/wkw/wl_obsinfo")

  req <- httr2::request(url) |>
    httr2::req_url_query(obscd = obscd, output = "json") |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(NULL)
  }

  body <- try(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    silent = TRUE
  )
  if (inherits(body, "try-error") || !is.list(body)) {
    return(NULL)
  }

  # WAMIS usually returns result$code == "success" and list = data.frame
  result_code <- tryCatch(body$result$code, error = function(e) NULL)
  if (!identical(result_code, "success") || is.null(body$list)) {
    return(NULL)
  }

  info <- body$list
  if (!inherits(info, "data.frame")) {
    info <- tibble::as_tibble(info)
  }

  if (!NROW(info)) return(NULL)

  # Keep first record if multiple
  info[1, , drop = FALSE]
}

#' @export
stations.hydro_service_KR_WAMIS <- function(x, ...) {
  base_url <- .kr_wamis_base_url(x)

  # --- 1) list of observatory codes (wl_dubwlobs) ---------------------------
  st_list <- .kr_wamis_station_list_json(x)

  if (!"obscd" %in% names(st_list)) {
    rlang::abort("KR_WAMIS: 'obscd' column missing in wl_dubwlobs station list.")
  }

  obscd_vec <- unique(trimws(as.character(st_list$obscd)))
  obscd_vec <- obscd_vec[nzchar(obscd_vec)]

  if (!length(obscd_vec)) {
    return(tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = character(),
      station_name  = character(),
      river         = character(),
      lat           = numeric(),
      lon           = numeric(),
      area          = numeric(),
      altitude      = numeric()
    ))
  }

  # --- 2) fetch detailed obsinfo per code -----------------------------------
  pb <- progress::progress_bar$new(
    total  = length(obscd_vec),
    format = "KR_WAMIS (stations) [:bar] :current/:total (:percent) eta: :eta"
  )

  info_list <- vector("list", length(obscd_vec))

  for (i in seq_along(obscd_vec)) {
    pb$tick()
    info_list[[i]] <- .kr_wamis_fetch_obsinfo_one(base_url, obscd_vec[i])
    # Be a bit polite, but not too slow
    Sys.sleep(0.02)
  }

  info_list <- info_list[!vapply(info_list, is.null, logical(1))]
  if (!length(info_list)) {
    rlang::warn("KR_WAMIS: no obsinfo records retrieved; returning empty station table.")
    return(tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = character(),
      station_name  = character(),
      river         = character(),
      lat           = numeric(),
      lon           = numeric(),
      area          = numeric(),
      altitude      = numeric()
    ))
  }

  meta_raw <- dplyr::bind_rows(info_list)

  # --- 3) standardise columns -----------------------------------------------
  station_id   <- trimws(as.character(meta_raw$wlobscd %||% meta_raw$obscd))
  station_name <- trimws(as.character(meta_raw$obsnmeng %||% meta_raw$obsnm))
  river        <- trimws(as.character(meta_raw$rivnm))

  lon_dd <- .kr_wamis_dms_to_dd(meta_raw$lon)
  lat_dd <- .kr_wamis_dms_to_dd(meta_raw$lat)

  altitude <- suppressWarnings(as.numeric(meta_raw$gdt))
  area     <- suppressWarnings(as.numeric(meta_raw$bsnara))

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = station_id,
    station_name  = station_name,
    river         = river,
    lat           = lat_dd,
    lon           = lon_dd,
    area          = area,
    altitude      = altitude
  )

  # Drop rows without usable ID
  out <- out[!is.na(out$station_id) & nzchar(out$station_id), , drop = FALSE]

  out
}


# -----------------------------------------------------------------------------
# Internal helpers for time series (daily discharge)
# -----------------------------------------------------------------------------

.kr_wamis_fetch_q_daily <- function(x,
                                    station_id,
                                    start_dt,
                                    end_dt) {
  # station_id: WAMIS obscd / wlobscd
  base_url <- .kr_wamis_base_url(x)
  site     <- trimws(as.character(station_id))

  if (!nzchar(site)) {
    return(tibble::tibble())
  }

  # years to loop over
  start_year <- as.integer(format(start_dt, "%Y"))
  end_year   <- as.integer(format(end_dt,   "%Y"))
  years      <- seq.int(start_year, end_year)

  url <- paste0(base_url, "/wkw/flw_dtdata")

  chunks <- vector("list", length(years))

  for (i in seq_along(years)) {
    yr <- years[i]

    params <- list(
      obscd  = site,
      year   = as.character(yr),
      output = "json"
    )

    req <- httr2::request(url) |>
      httr2::req_url_query(!!!params) |>
      httr2::req_user_agent(
        "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
      )

    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
      next
    }

    body <- try(
      httr2::resp_body_json(resp, simplifyVector = TRUE),
      silent = TRUE
    )
    if (inherits(body, "try-error") || !is.list(body) || is.null(body$list)) {
      next
    }

    df <- body$list
    if (!inherits(df, "data.frame") || !NROW(df)) {
      next
    }

    df <- tibble::as_tibble(df)

    if (!("ymd" %in% names(df) && "fw" %in% names(df))) {
      next
    }

    # Parse dates + values
    date <- suppressWarnings(as.Date(df$ymd, format = "%Y%m%d"))
    val  <- suppressWarnings(as.numeric(df$fw))

    # Sentinel missing / invalid
    val[val <= -777] <- NA_real_

    keep <- !is.na(date) & !is.na(val) &
      date >= start_dt & date <= end_dt

    if (!any(keep)) next

    ts <- as.POSIXct(date[keep], tz = "UTC")

    chunks[[i]] <- tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = site,
      parameter     = "water_discharge",
      timestamp     = ts,
      value         = val[keep],
      unit          = "m^3/s",
      quality_code  = NA_character_,
      source_url    = url
    )
  }

  out <- dplyr::bind_rows(chunks)
  if (!NROW(out)) {
    return(tibble::tibble())
  }

  # Remove duplicate timestamps, keep first
  out <- out[!duplicated(out$timestamp), , drop = FALSE]
  out[order(out$timestamp), , drop = FALSE]
}


# -----------------------------------------------------------------------------
# Public time series interface
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_KR_WAMIS <- function(x,
                                              parameter = c("water_discharge"),
                                              stations    = NULL,
                                              start_date  = NULL,
                                              end_date    = NULL,
                                              mode        = c("complete", "range"),
                                              exclude_quality = NULL,
                                              ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  if (!identical(parameter, "water_discharge")) {
    rlang::abort("KR_WAMIS: only parameter = 'water_discharge' is supported currently.")
  }

  # Use same helper as other adapters (if available)
  rng <- resolve_dates(mode, start_date, end_date)

  # Fallbacks if resolve_dates leaves NAs / NULLs
  today <- Sys.Date()

  start_dt <- rng$start_date
  if (is.null(start_dt) || anyNA(start_dt)) {
    start_dt <- as.Date(sprintf("%d-01-01", as.integer(format(today, "%Y"))))
  } else {
    start_dt <- as.Date(start_dt)
  }

  end_dt <- rng$end_date
  if (is.null(end_dt) || anyNA(end_dt)) {
    end_dt <- today
  } else {
    end_dt <- as.Date(end_dt)
  }

  if (end_dt < start_dt) {
    rlang::abort("KR_WAMIS: end_date is earlier than start_date.")
  }

  # --------------------------------------------------------------------------
  # station_id vector
  # --------------------------------------------------------------------------
  if (is.null(stations)) {
    st <- stations.hydro_service_KR_WAMIS(x)

    station_vec <- st$station_id
  } else {
    station_vec <- stations
  }

  station_vec <- unique(trimws(as.character(station_vec)))
  station_vec <- station_vec[nzchar(station_vec)]

  if (!length(station_vec)) {
    return(tibble::tibble())
  }

  # batching + rate limit -----------------------------------------------------
  batches <- chunk_vec(station_vec, 20L)

  pb <- progress::progress_bar$new(
    total  = length(batches),
    format = "KR_WAMIS (Q) [:bar] :current/:total (:percent) eta: :eta"
  )

  fetch_one <- function(st_id) {
    .kr_wamis_fetch_q_daily(
      x         = x,
      station_id = st_id,
      start_dt  = start_dt,
      end_dt    = end_dt
    )
  }

  limited <- ratelimitr::limit_rate(
    fetch_one,
    rate = ratelimitr::rate(
      n      = x$rate_cfg$n %||% 5L,
      period = x$rate_cfg$period %||% 1
    )
  )

  out <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, limited))
  })

  dplyr::bind_rows(out)
}
