# ==== South Africa (DWS Verified Hydrology) adapter ==========================
# Stations:
#   - DWS "Verified" Hydrology station catalogue:
#       https://www.dws.gov.za/hydrology/Verified/hymain.aspx
#     -> "Station Catalogue" (HyCatalogue.aspx)
#     -> WMA-specific PDFs (e.g. WMA1_Limpopo-Olifants_River.pdf)
#     Each "River" PDF lists river gauges with:
#       Station, Description, Latitude (dd:mm:ss), Longitude (dd:mm:ss),
#       Drainage region code, Catchment area (km^2), etc.
#
# Time series:
#   - Daily mean water level (stage), from primary "Point" data:
#       https://www.dws.gov.za/hydrology/Verified/HyData.aspx
#       DataType = "Point", SiteType = "RIV"
#     We download sub-daily COR_LEVEL values and aggregate to daily mean.
#
#   - Daily mean discharge:
#       DataType = "Daily", using D_AVG_FR.
#
# Notes:
#   - Adapter exposes parameters:
#       * "water_level"     (DataType = Point, COR_LEVEL daily mean)
#       * "water_discharge" (DataType = Daily, D_AVG_FR)
#   - Station metadata are scraped from the "River" PDFs; we treat all
#     as river gauges.
#   - Coordinates are given as dd:mm:ss with *no hemisphere*; we assume:
#       lat < 0 (southern hemisphere), lon > 0 (eastern hemisphere).
#   - Daily data are returned with `timestamp` at midnight UTC.


# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @export
register_ZA_DWS <- function() {
  register_service(
    provider_id   = "ZA_DWS",
    provider_name = "South Africa – Department of Water and Sanitation (Verified)",
    country       = "ZA",
    base_url      = "https://www.dws.gov.za/hydrology/Verified",
    rate_cfg      = list(n = 1L, period = 1),  # 1 request / second
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_ZA_DWS <- function(x, ...) {
  # daily level + discharge
  c("water_discharge", "water_level")
}


# -----------------------------------------------------------------------------
# Station list (from DWS "Verified" station catalogue PDFs)
# -----------------------------------------------------------------------------

.za_dws_river_pdf_urls <- function() {
  page_url <- "https://www.dws.gov.za/hydrology/Verified/HyCatalogue.aspx"

  req <- httr2::request(page_url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort("ZA_DWS: failed to fetch HyCatalogue page for station list.")
  }

  html <- try(
    xml2::read_html(httr2::resp_body_string(resp)),
    silent = TRUE
  )
  if (inherits(html, "try-error")) {
    rlang::abort("ZA_DWS: could not parse HyCatalogue HTML.")
  }

  links <- rvest::html_elements(html, "a")
  hrefs <- trimws(rvest::html_attr(links, "href"))
  hrefs <- hrefs[!is.na(hrefs)]

  # Only the River PDFs (surface water flow gauges)
  cand <- hrefs[grepl("_River\\.pdf$", hrefs, ignore.case = TRUE)]
  if (!length(cand)) {
    rlang::abort("ZA_DWS: no *_River.pdf station catalogue PDFs found.")
  }

  urls <- vapply(cand, function(u) {
    if (!grepl("^https?://", u)) {
      xml2::url_absolute(u, page_url)
    } else {
      u
    }
  }, character(1))

  unique(urls)
}

# Parse dd:mm:ss to decimal degrees
# - lat: always negative (southern hemisphere)
# - lon: positive (eastern hemisphere)
.za_dws_dms_to_dd <- function(x, is_lon = FALSE) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  res <- rep(NA_real_, length(x))
  ok  <- !is.na(x)

  if (!any(ok)) return(res)

  idx_ok <- which(ok)
  parts  <- strsplit(x[ok], ":", fixed = TRUE)

  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (length(p) < 3) next

    d <- suppressWarnings(as.numeric(p[1]))
    m <- suppressWarnings(as.numeric(p[2]))
    s <- suppressWarnings(as.numeric(p[3]))
    if (any(is.na(c(d, m, s)))) next

    val <- d + m / 60 + s / 3600
    if (!is_lon) {
      val <- -val  # south
    }
    res[idx_ok[i]] <- val
  }

  res
}

# Parse a single WMA*_River.pdf into a small station tibble
.za_dws_parse_station_pdf <- function(pdf_url) {
  tmp <- tempfile(fileext = ".pdf")

  req <- httr2::request(pdf_url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    )

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::warn(paste("ZA_DWS: failed to download", pdf_url))
    return(tibble::tibble())
  }

  bin <- httr2::resp_body_raw(resp)
  writeBin(bin, tmp)

  # Extract text from all pages
  pages <- try(pdftools::pdf_text(tmp), silent = TRUE)
  if (inherits(pages, "try-error")) {
    rlang::warn(paste("ZA_DWS: pdftools::pdf_text() failed for", pdf_url))
    return(tibble::tibble())
  }

  lines <- unlist(strsplit(pages, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  # Pattern for station rows, e.g.:
  # A2H023 Jukskei River @ Nietgedacht 25:57:16 27:57:45 A21C 686
  pat <- paste0(
    "^([A-Z0-9]{3,})\\s+",
    "(.+?)\\s+",
    "(\\d{2}:\\d{2}:\\d{2})\\s+",
    "(\\d{2}:\\d{2}:\\d{2})\\s+",
    "([A-Z0-9]{3,})\\s+",
    "([0-9]+(?:\\.[0-9]+)?)\\b"
  )

  m   <- regexec(pat, lines)
  out <- regmatches(lines, m)

  rows <- lapply(out, function(x) {
    if (length(x) != 7) return(NULL)
    data.frame(
      station_id = x[2],
      desc       = x[3],
      lat_dms    = x[4],
      lon_dms    = x[5],
      drainage   = x[6],
      area       = x[7],
      stringsAsFactors = FALSE
    )
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) return(tibble::tibble())

  df <- do.call(rbind, rows)

  df$area <- suppressWarnings(as.numeric(df$area))

  # WMA name from filename, e.g. "WMA1_Limpopo-Olifants_River.pdf"
  wma <- basename(pdf_url)
  wma <- sub("\\.pdf$", "", wma, ignore.case = TRUE)
  df$wma <- wma

  tibble::as_tibble(df)
}

.za_dws_read_station_catalogue <- function() {
  urls <- .za_dws_river_pdf_urls()

  pieces <- lapply(urls, function(u) {
    try(.za_dws_parse_station_pdf(u), silent = TRUE)
  })

  ok <- !vapply(pieces, function(p) {
    inherits(p, "try-error") || !NROW(p)
  }, logical(1))

  pieces <- pieces[ok]

  if (!length(pieces)) {
    rlang::abort("ZA_DWS: failed to parse any station metadata from River PDFs.")
  }

  out <- dplyr::bind_rows(pieces)

  # De-duplicate by station_id
  out <- out[!is.na(out$station_id) & nzchar(out$station_id), , drop = FALSE]
  out <- out[!duplicated(out$station_id), , drop = FALSE]

  out
}

#' @export
stations.hydro_service_ZA_DWS <- function(x, ...) {
  raw_tbl <- .za_dws_read_station_catalogue()

  if (!NROW(raw_tbl)) {
    return(tibble::tibble(
      country               = x$country,
      provider_id           = x$provider_id,
      provider_name         = x$provider_name,
      station_id            = character(),
      station_name_original = character(),
      station_name          = character(),
      river                 = character(),
      lat                   = numeric(),
      lon                   = numeric(),
      area                  = numeric(),
      altitude              = numeric()
    ))
  }

  desc <- normalize_utf8(trimws(raw_tbl$desc))

  # Split "River @ Location" where present
  has_at <- grepl("@", desc, fixed = TRUE)

  station_name <- desc
  station_name[has_at] <- trimws(sub("^.*@", "", desc[has_at]))

  river <- ifelse(
    has_at,
    trimws(sub("@.*$", "", desc)),
    NA_character_
  )

  lat_dd <- .za_dws_dms_to_dd(raw_tbl$lat_dms, is_lon = FALSE)
  lon_dd <- .za_dws_dms_to_dd(raw_tbl$lon_dms, is_lon = TRUE)

  out <- tibble::tibble(
    country               = x$country,
    provider_id           = x$provider_id,
    provider_name         = x$provider_name,
    station_id            = raw_tbl$station_id,
    station_name_original = desc,
    station_name          = station_name,
    river                 = river,
    lat                   = lat_dd,
    lon                   = lon_dd,
    area                  = raw_tbl$area,
    altitude              = NA_real_
  )

  out
}


# -----------------------------------------------------------------------------
# Internal helpers for time series (HyData.aspx)
# -----------------------------------------------------------------------------

# Low-level raw fetch (text table) for one gauge + variable
.za_dws_download_raw <- function(site,
                                 variable,
                                 start_date,
                                 end_date) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)

  if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
    return(tibble::tibble())
  }

  start_year <- as.integer(format(start_date, "%Y"))
  end_year   <- as.integer(format(end_date, "%Y"))
  years      <- seq.int(start_year, end_year, by = 1L)

  # Stage (water level) from primary "Point" data; discharge from "Daily"
  if (identical(variable, "stage")) {
    chunk_size <- 1L
    data_type  <- "Point"
    header     <- c(
      "DATE", "TIME", "COR_LEVEL",
      "COR_LEVEL_QUAL", "COR_FLOW", "COR_FLOW_QUAL"
    )
  } else {
    chunk_size <- 20L
    data_type  <- "Daily"
    header     <- c("DATE", "D_AVG_FR", "QUAL")
  }

  n_cols   <- length(header)
  n_years  <- length(years)
  n_chunks <- ceiling(n_years / chunk_size)

  data_list <- vector("list", n_chunks)
  have_any  <- FALSE

  for (i in seq_len(n_chunks)) {
    idx_start <- (i - 1L) * chunk_size + 1L
    idx_end   <- min(n_years, i * chunk_size)

    chunk_start_year <- years[idx_start]
    chunk_end_year   <- years[idx_end]

    chunk_start_date <- as.Date(sprintf("%04d-01-01", chunk_start_year))
    chunk_end_date   <- as.Date(sprintf("%04d-12-31", chunk_end_year))

    chunk_start_date <- max(chunk_start_date, start_date)
    chunk_end_date   <- min(chunk_end_date,   end_date)

    if (chunk_start_date > chunk_end_date) next

    endpoint <- paste0(
      "https://www.dws.gov.za/hydrology/Verified/HyData.aspx?",
      "Station=", site, "100.00",
      "&DataType=", data_type,
      "&StartDT=", format(chunk_start_date, "%Y-%m-%d"),
      "&EndDT=",   format(chunk_end_date,   "%Y-%m-%d"),
      "&SiteType=RIV"
    )

    req <- httr2::request(endpoint) |>
      httr2::req_user_agent(
        "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
      )

    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
      rlang::warn(paste0("ZA_DWS: request failed for ", endpoint))
      next
    }

    txt <- try(httr2::resp_body_string(resp), silent = TRUE)
    if (inherits(txt, "try-error") || !nzchar(txt)) next

    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    if (!length(lines)) next

    # Header line "DATE ..." and data lines "YYYYMMDD ..."
    header_row <- grep("^DATE",         lines)
    data_rows  <- grep("^[0-9]{8}\\s", lines)

    if (!length(header_row) || !length(data_rows)) next

    raw_lines <- trimws(lines[data_rows])

    split_list <- strsplit(raw_lines, "[[:space:]]+")
    rows <- lapply(split_list, function(tokens) {
      tokens <- tokens[tokens != ""]
      if (!length(tokens)) {
        rep(NA_character_, n_cols)
      } else if (length(tokens) > n_cols) {
        tokens[seq_len(n_cols)]
      } else if (length(tokens) < n_cols) {
        c(tokens[1], rep(NA_character_, n_cols - 1L))
      } else {
        tokens
      }
    })

    mat <- do.call(rbind, rows)
    colnames(mat) <- header

    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    data_list[[i]] <- df
    have_any <- TRUE
  }

  if (!have_any) {
    return(tibble::tibble())
  }

  original_data <- do.call(rbind, data_list)
  tibble::as_tibble(original_data)
}

.za_dws_fetch_series <- function(x,
                                 station_id,
                                 parameter,
                                 rng,
                                 mode) {
  site <- trimws(as.character(station_id))
  if (!nzchar(site)) return(tibble::tibble())

  variable <- if (identical(parameter, "water_level")) "stage" else "discharge"

  # Default "complete" range if resolve_dates didn't set explicit bounds
  start_date <- rng$start_date %||% as.Date("1900-01-01")
  end_date   <- rng$end_date   %||% Sys.Date()

  ts_raw <- try(
    .za_dws_download_raw(
      site       = site,
      variable   = variable,
      start_date = start_date,
      end_date   = end_date
    ),
    silent = TRUE
  )

  if (inherits(ts_raw, "try-error") || !NROW(ts_raw)) {
    rlang::warn(paste0("ZA_DWS: download failed or empty for station ", site))
    return(tibble::tibble())
  }

  # Common date parsing
  ts_raw$DATE <- suppressWarnings(
    as.Date(ts_raw$DATE, format = "%Y%m%d")
  )
  ts_raw <- ts_raw[!is.na(ts_raw$DATE), , drop = FALSE]

  if (!NROW(ts_raw)) return(tibble::tibble())

  if (identical(parameter, "water_level")) {
    # Daily mean of COR_LEVEL from sub-daily "Point" data
    ts_raw$COR_LEVEL <- suppressWarnings(
      as.numeric(ts_raw$COR_LEVEL)
    )

    if (!any(!is.na(ts_raw$COR_LEVEL))) {
      return(tibble::tibble())
    }

    daily <- ts_raw |>
      dplyr::group_by(.data$DATE) |>
      dplyr::summarise(
        value = mean(.data$COR_LEVEL, na.rm = TRUE),
        .groups = "drop"
      )

    # mean(NA, na.rm=TRUE) -> NaN; normalise to NA
    daily$value[!is.finite(daily$value)] <- NA_real_

  } else {
    # Daily discharge from D_AVG_FR
    ts_raw$D_AVG_FR <- suppressWarnings(
      as.numeric(ts_raw$D_AVG_FR)
    )

    daily <- ts_raw[, c("DATE", "D_AVG_FR"), drop = FALSE]
    names(daily) <- c("DATE", "value")
    daily <- daily[!is.na(daily$value), , drop = FALSE]
  }

  if (!NROW(daily)) return(tibble::tibble())

  # Range filter (if requested)
  if (identical(mode, "range")) {
    keep <- !is.na(daily$DATE) &
      daily$DATE >= rng$start_date &
      daily$DATE <= rng$end_date
    daily <- daily[keep, , drop = FALSE]
  }

  if (!NROW(daily)) return(tibble::tibble())

  ts_final <- as.POSIXct(daily$DATE, tz = "UTC")
  unit     <- if (identical(parameter, "water_discharge")) "m^3/s" else "m"

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = site,
    parameter     = parameter,
    timestamp     = ts_final,
    value         = daily$value,
    unit          = unit,
    quality_code  = NA_character_,
    source_url    = paste0(x$base_url, "/HyData.aspx")
  )
}


# -----------------------------------------------------------------------------
# Public time series interface
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_ZA_DWS <- function(x,
                                            parameter = c("water_level",
                                                          "water_discharge"),
                                            stations      = NULL,
                                            start_date    = NULL,
                                            end_date      = NULL,
                                            mode          = c("complete",
                                                              "range"),
                                            exclude_quality = NULL,
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)

  rng <- resolve_dates(mode, start_date, end_date)

  # --------------------------------------------------------------------------
  # station_id vector
  # --------------------------------------------------------------------------
  if (is.null(stations)) {
    st <- stations.hydro_service_ZA_DWS(x)
    # Only keep valid station_ids
    st <- st[!is.na(st$station_id) & nzchar(st$station_id), , drop = FALSE]
    station_vec <- st$station_id
  } else {
    station_vec <- stations
  }

  station_vec <- unique(trimws(as.character(station_vec)))
  if (!length(station_vec)) {
    return(tibble::tibble())
  }

  # batching + rate limit -----------------------------------------------------

  batches <- chunk_vec(station_vec, 20L)

  pb <- progress::progress_bar$new(
    total  = length(batches),
    format = "ZA_DWS [:bar] :current/:total (:percent) eta: :eta"
  )

  fetch_one <- function(st_id) {
    .za_dws_fetch_series(
      x          = x,
      station_id = st_id,
      parameter  = parameter,
      rng        = rng,
      mode       = mode
    )
  }

  limited <- ratelimitr::limit_rate(
    fetch_one,
    rate = ratelimitr::rate(
      n      = x$rate_cfg$n %||% 1L,
      period = x$rate_cfg$period %||% 1
    )
  )

  out <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, limited))
  })

  dplyr::bind_rows(out)
}
