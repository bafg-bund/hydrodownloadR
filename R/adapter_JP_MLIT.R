# ==== Japan (MLIT River gauges) adapter ======================================
# Base: "http://www1.river.go.jp"
#
# Data access:
#   Daily mean water level / discharge are obtained via the MLIT
#   DspWaterData CGI:
#     http://www1.river.go.jp/cgi-bin/DspWaterData.exe
#
#   For a given station + year + KIND, the CGI returns an HTML page that
#   contains a link to a yearly .dat file under /dat/dload/download/*.dat.
#   The .dat file encodes daily values per month:
#     - One line per month
#     - First field: month indicator (e.g. "1EZ")
#     - Remaining fields: value, flag, value, flag, ...
#   We:
#     - detect the .dat link from the HTML
#     - download the .dat
#     - parse month lines into daily Date / Value
#     - treat -9999.99 (Q) and -9999.00 (H) as NA and drop fully missing months
#
# Limitations:
#   - No public station catalog (coords/area) is used yet; `stations()` returns
#     an empty tibble and you must supply station ids explicitly to
#     `timeseries()`.
#
# Packages:
#   Uses httr2 + xml2 + rvest + lubridate + tibble + dplyr + ratelimitr.

# ---- registration ------------------------------------------------------------

#' @keywords internal
#' @noRd
register_JP_MLIT <- function() {
  register_service_usage(
    provider_id   = "JP_MLIT",
    provider_name = "Ministry of Land, Infrastructure, Transport and Tourism (MLIT)",
    country       = "Japan",
    base_url      = "http://www1.river.go.jp",
    # Be conservative here; the CGI is not a modern bulk API.
    rate_cfg      = list(n = 1, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_JP_MLIT <- function(x, ...) {
  c("water_discharge","water_level")
}

# ---- parameter mapping -------------------------------------------------------

.jp_param_map <- function(parameter) {
  # Map hydrodownloadR-style parameter to MLIT "KIND" code and unit
  #
  # KIND codes:
  #   3 = water level (daily)
  #   7 = discharge   (daily)
  switch(
    parameter,
    water_discharge = list(
      kind = 7L,
      unit = "m^3/s"
    ),
    water_level = list(
      kind = 3L,
      unit = "m"
    ),
    rlang::abort("JP_MLIT supports 'water_discharge' and 'water_level'.")
  )
}

# ---- stations (S3) -----------------------------------------------------------

# There is currently no programmatic station list; this is a stub kept
# for interface consistency and can later be wired to a static dataset
# (e.g. jp_mlit_stations) once available.

#' @export
stations.hydro_service_JP_MLIT <- function(x,
                                           stations = NULL,
                                           ...) {
  # ---------------------------------------------------------------------------
  # Load precomputed station metadata (jp_mlit_meta) via .pkg_data()
  # ---------------------------------------------------------------------------
  ms <- .pkg_data("jp_mlit_meta")

  if (is.null(ms)) {
    rlang::abort(
      "JP_MLIT: internal dataset 'jp_mlit_meta' not found. ",
      "Make sure you ran data-raw/jp_mlit_meta_build.R so that ",
      "data/jp_mlit_meta.rda is available."
    )
  }

  # ---------------------------------------------------------------------------
  # Optional filter by station_id argument
  # ---------------------------------------------------------------------------
  if (!is.null(stations)) {
    stations_chr <- unique(as.character(stations))
    ms <- ms[ms$station_id %in% stations_chr, , drop = FALSE]

    if (!nrow(ms)) {
      rlang::warn(
        paste0(
          "JP_MLIT: none of the requested station_id values were found in ",
          "jp_mlit_meta. Returning an empty tibble."
        )
      )
    }
  }

  # ---------------------------------------------------------------------------
  # Empty template if no stations left
  # ---------------------------------------------------------------------------
  if (!nrow(ms)) {
    return(tibble::tibble(
      country       = character(),
      provider_id   = character(),
      provider_name = character(),
      station_id    = character(),
      station_name  = character(),
      river         = character(),
      lat           = numeric(),
      lon           = numeric(),
      area          = numeric(),
      altitude      = numeric()
    ))
  }

  # ---------------------------------------------------------------------------
  # Final output: x$ fields first, then station fields
  # ---------------------------------------------------------------------------
  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = ms$station_id,
    station_name  = ms$station_name,  # English from jp_mlit_meta
    river         = ms$river,         # English watersystem/river name
    lat           = ms$lat,
    lon           = ms$lon,
    area          = ms$area,
    altitude      = ms$altitude
  )
}



# ---- internal helper ---------------------------------------------------------

# Fetch one year of daily data via the .dat download linked in the HTML
.jp_mlit_fetch_year_dat <- function(
    x,
    station_id,
    kind,
    year,
    start_date,
    end_date
) {
  base_path  <- "/cgi-bin/DspWaterData.exe"
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  # Year window [year-01-01, year-12-31]
  bgn_year <- as.Date(sprintf("%d-01-01", year))
  end_year <- as.Date(sprintf("%d-12-31", year))

  # Skip if this year does not intersect requested range at all
  if (end_year < start_date || bgn_year > end_date) {
    return(tibble::tibble())
  }

  BGNDATE <- format(bgn_year, "%Y%m%d")   # must be 01-01 of this year
  ENDDATE <- format(end_year, "%Y%m%d")   # must be 12-31 of this year

  query <- list(
    KIND    = kind,
    ID      = station_id,
    BGNDATE = BGNDATE,
    ENDDATE = ENDDATE,
    KAWABOU = "NO"
  )

  # --- Step 1: HTML to discover .dat link --------------------------------
  req_html  <- build_request(x, path = base_path, query = query)
  resp_html <- perform_request(req_html)

  status <- httr2::resp_status(resp_html)
  if (status != 200L) {
    rlang::warn(
      paste0(
        "JP_MLIT: HTML request returned status ", status,
        " for station ", station_id,
        " year ", year, "."
      )
    )
    return(tibble::tibble())
  }

  body_raw <- httr2::resp_body_raw(resp_html)
  if (!length(body_raw)) {
    rlang::warn(
      paste0(
        "JP_MLIT: empty HTML body for station ", station_id,
        " year ", year, "."
      )
    )
    return(tibble::tibble())
  }

  doc <- tryCatch(
    xml2::read_html(body_raw),
    error = function(e) {
      rlang::warn(
        paste0(
          "JP_MLIT: failed to parse HTML for station ", station_id,
          " year ", year, ": ", conditionMessage(e)
        )
      )
      NULL
    }
  )
  if (is.null(doc)) return(tibble::tibble())

  # Find first <a href="...dat">
  anchors <- rvest::html_elements(doc, "a")
  if (!length(anchors)) {
    rlang::warn(
      paste0(
        "JP_MLIT: no <a> links found in HTML for station ", station_id,
        " year ", year, "."
      )
    )
    return(tibble::tibble())
  }

  hrefs    <- rvest::html_attr(anchors, "href")
  href_dat <- hrefs[grepl("\\.dat$", hrefs)]
  if (!length(href_dat)) {
    rlang::warn(
      paste0(
        "JP_MLIT: no .dat link found for station ", station_id,
        " year ", year, "."
      )
    )
    return(tibble::tibble())
  }

  dat_rel <- href_dat[[1]]  # e.g. "/dat/dload/download/2730....dat"

  # --- Step 2: download and parse the .dat -------------------------------

  req_dat  <- build_request(x, path = dat_rel)
  resp_dat <- perform_request(req_dat)

  status2 <- httr2::resp_status(resp_dat)
  if (status2 != 200L) {
    rlang::warn(
      paste0(
        "JP_MLIT: .dat request returned status ", status2,
        " for station ", station_id,
        " year ", year, "."
      )
    )
    return(tibble::tibble())
  }

  dat_raw <- httr2::resp_body_raw(resp_dat)
  if (!length(dat_raw)) {
    rlang::warn(
      paste0(
        "JP_MLIT: empty .dat body for station ", station_id,
        " year ", year, "."
      )
    )
    return(tibble::tibble())
  }

  dat_str <- rawToChar(dat_raw)
  lines   <- strsplit(dat_str, "\r\n|\n|\r")[[1]]
  lines   <- lines[nzchar(trimws(lines))]

  if (!length(lines)) {
    return(tibble::tibble())
  }

  # Station id from header (typically line 5: "<label>,3010...")
  if (length(lines) >= 5L) {
    st_header <- lines[5L]
    parts     <- strsplit(st_header, ",", fixed = TRUE)[[1]]

    if (length(parts) >= 2L) {
      meta_id <- trimws(parts[2L])

      if (nzchar(meta_id) && grepl("^[0-9]+$", meta_id)) {
        if (!identical(meta_id, station_id)) {
          rlang::warn(
            paste0(
              "JP_MLIT: station id in .dat header (", meta_id,
              ") differs from requested id (", station_id, ")."
            )
          )
        }
      }
    }
  }

  # Optional: check that the file year matches the requested year
  file_year_line <- grep("^\\s*[0-9]{4}", lines, value = TRUE)[1]
  if (!is.na(file_year_line)) {
    file_year <- suppressWarnings(
      as.integer(sub("^\\s*([0-9]{4}).*", "\\1", file_year_line))
    )
    if (!is.na(file_year) && file_year != year) {
      rlang::warn(
        paste0(
          "JP_MLIT: year in .dat (", file_year,
          ") differs from requested year (", year, ")."
        )
      )
    }
  }

  # Month rows: start with 1-2 digits (month number) + a non-digit
  month_idx <- grep("^\\s*[0-9]{1,2}[^0-9]", lines)
  if (!length(month_idx)) {
    rlang::warn(
      paste0(
        "JP_MLIT: no month rows found in .dat for station ",
        station_id, " year ", year, "."
      )
    )
    return(tibble::tibble())
  }

  out_list <- lapply(lines[month_idx], function(line) {
    parts <- strsplit(line, ",", fixed = TRUE)[[1]]
    parts <- trimws(parts)

    if (!length(parts)) return(tibble::tibble())

    month <- suppressWarnings(
      as.integer(sub("^\\s*([0-9]{1,2}).*", "\\1", parts[1]))
    )
    if (is.na(month)) return(tibble::tibble())

    # Remaining entries: value, flag, value, flag, ...
    vals_raw <- parts[-1]
    if (!length(vals_raw)) return(tibble::tibble())

    vals_raw <- vals_raw[seq(1, length(vals_raw), by = 2)]  # keep only values

    ndays <- lubridate::days_in_month(
      as.Date(sprintf("%04d-%02d-01", year, month))
    )
    vals_raw <- vals_raw[seq_len(min(ndays, length(vals_raw)))]

    # Blank / special symbols -> NA
    vals_raw[vals_raw %in% c("", "$", "-")] <- NA_character_

    vals_num <- suppressWarnings(as.numeric(vals_raw))

    # Treat both Q and H missing codes as NA
    vals_num[vals_num %in% c(-9999.99, -9999.00)] <- NA_real_

    # If the whole month is only missing values, skip it entirely
    if (all(is.na(vals_num))) {
      return(tibble::tibble())
    }

    day_num <- seq_along(vals_num)
    dates   <- as.Date(sprintf("%04d-%02d-%02d", year, month, day_num))

    tibble::tibble(Date = dates, Value = vals_num)
  })

  out <- suppressWarnings(dplyr::bind_rows(out_list))
  if (!nrow(out)) return(out)

  # Clip to requested range
  keep <- !is.na(out$Value) &
    out$Date >= start_date &
    out$Date <= end_date

  out[keep, , drop = FALSE]
}

# ---- time series (S3) --------------------------------------------------------

#' @export
timeseries.hydro_service_JP_MLIT <- function(
    x,
    parameter   = c("water_discharge", "water_level"),
    stations    = NULL,
    start_date  = NULL,
    end_date    = NULL,
    mode        = c("complete", "range"),
    ...
) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  pm  <- .jp_param_map(parameter)
  rng <- resolve_dates(mode, start_date, end_date)

  start_date <- as.Date(rng$start_date)
  end_date   <- as.Date(rng$end_date)

  # ---------------------------------------------------------------------------
  # Station ids
  # ---------------------------------------------------------------------------

  ids <- stations %||% character()
  if (!length(ids)) {
    rlang::abort(
      "JP_MLIT: `stations` must be supplied explicitly; ",
      "no programmatic station metadata catalogue is available."
    )
  }
  ids <- unique(as.character(ids))

  # ---------------------------------------------------------------------------
  # Empty result template (match final schema exactly)
  # ---------------------------------------------------------------------------

  empty <- tibble::tibble(
    country        = character(),
    provider_id    = character(),
    provider_name  = character(),
    station_id     = character(),
    parameter      = character(),
    timestamp      = as.POSIXct(character()),
    value          = numeric(),
    unit           = character(),
    quality_code   = character(),
    qf_desc        = character(),
    source_url     = character(),
    vertical_datum = character()
  )

  if (!length(ids)) {
    return(empty)
  }

  # ---------------------------------------------------------------------------
  # Per-station fetcher (rate-limited)
  # ---------------------------------------------------------------------------

  years <- seq(lubridate::year(start_date), lubridate::year(end_date))

  one_station <- ratelimitr::limit_rate(
    function(st_id) {
      # Fetch all relevant years for this station
      yearly <- lapply(
        years,
        function(yy) {
          .jp_mlit_fetch_year_dat(
            x          = x,
            station_id = st_id,
            kind       = pm$kind,
            year       = yy,
            start_date = start_date,
            end_date   = end_date
          )
        }
      )

      df <- suppressWarnings(dplyr::bind_rows(yearly))
      if (!nrow(df)) {
        rlang::warn(
          paste0(
            "JP_MLIT: no data returned for station ", st_id,
            " and parameter '", parameter, "'."
          )
        )
        return(tibble::tibble())
      }

      # Build representative source_url (first year)
      first_year <- min(years)
      src_url <- paste0(
        x$base_url,
        "/cgi-bin/DspWaterData.exe?",
        "KIND=", pm$kind,
        "&ID=", utils::URLencode(st_id, reserved = TRUE),
        "&BGNDATE=", sprintf("%d0101", first_year),
        "&ENDDATE=", sprintf("%d1231", first_year),
        "&KAWABOU=NO"
      )

      tibble::tibble(
        country        = x$country,
        provider_id    = x$provider_id,
        provider_name  = x$provider_name,
        station_id     = st_id,
        parameter      = parameter,
        # Daily values - store as UTC midnights; original is JST but date-only
        timestamp      = as.POSIXct(df$Date, tz = "UTC"),
        value          = df$Value,
        unit           = pm$unit,
        quality_code   = NA_character_,
        qf_desc        = NA_character_,
        source_url     = src_url,
        vertical_datum = NA_character_
      )
    },
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  # ---------------------------------------------------------------------------
  # Iterate over stations (chunked) with progress bar
  # ---------------------------------------------------------------------------

  batches <- chunk_vec(ids, 5L)
  pb <- progress::progress_bar$new(total = length(batches))

  out <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, one_station))
  })

  res <- dplyr::bind_rows(out)
  if (!nrow(res)) empty else res
}
