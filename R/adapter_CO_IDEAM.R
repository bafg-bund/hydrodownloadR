# ==== Colombia (IDEAM AQUARIUS WebPortal) adapter ===========================
#
# Scope
#   - Parameters exposed by hydrodownloadR:
#       water_discharge (daily mean discharge)
#       water_level     (daily mean water level)
#   - AQUARIUS labels accepted:
#       CAUDAL.HIS_Q_MEDIA_D@<station_id>
#       CAUDAL.Q_MEDIA_D@<station_id>
#       NIVEL.HIS_NV_MEDIA_D@<station_id>
#       NIVEL.NV_MEDIA_D@<station_id>
#   - Hourly (_H), monthly (_M), and annual (_A) series are deliberately
#     excluded.
#
# Discovery workflow
#   1. POST /Data/List/ and obtain the numeric AQUARIUS parameter id whose
#      stable data-code is "CAUDAL" or "NIVEL".
#   2. POST /Data/Data_List, paginating over all datasets for that parameter.
#   3. Retain only the supported daily-mean labels listed above.
#
# Time-series workflow
#   - Values are read from /Data/DatasetGrid using the numeric DatasetId.
#   - This JSON endpoint avoids the locale-dependent ZIP/CSV export and its
#     short-lived token.
#   - If a station contains both a historical and an operational daily series,
#     they are combined. The operational series takes precedence only where
#     timestamps overlap; the HIS_* series supplies the remaining history.
#   - AQUARIUS timestamps are requested in Colombia local time (UTC-05:00) and
#     converted to UTC in the returned `timestamp` column.
#
# Station metadata
#   - Locations with at least one supported daily-discharge or daily-level
#     dataset are returned.
#   - Location/name/coordinates come from the AQUARIUS dataset index.
#   - Elevation and fallback name/coordinates are taken, when available, from
#     IDEAM's National Station Catalogue on datos.gov.co.
#   - Catchment area and river name are not supplied by these endpoints and are
#     therefore returned as NA.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @keywords internal
#' @noRd
register_CO_IDEAM <- function() {
  register_service_usage(
    provider_id   = "CO_IDEAM",
    provider_name = "IDEAM AQUARIUS WebPortal",
    country       = "Colombia",
    base_url      = "https://aquariuswebportal.ideam.gov.co",
    rate_cfg      = list(n = 1L, period = 1),
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_CO_IDEAM <- function(x, ...) {
  c("water_discharge", "water_level")
}

# -----------------------------------------------------------------------------
# Constants and small utilities
# -----------------------------------------------------------------------------

.co_ideam_catalog_url <- function() {
  "https://www.datos.gov.co/resource/hp9r-jxuu.json"
}

.co_ideam_supported_parameters <- function() {
  c("water_discharge", "water_level")
}

.co_ideam_parameter_spec <- function(parameter) {
  parameter <- match.arg(parameter, .co_ideam_supported_parameters())

  switch(
    parameter,
    water_discharge = list(
      portal_code = "CAUDAL",
      labels      = c("Q_MEDIA_D", "HIS_Q_MEDIA_D"),
      unit        = "m^3/s",
      description = "daily-mean discharge"
    ),
    water_level = list(
      portal_code = "NIVEL",
      labels      = c("NV_MEDIA_D", "HIS_NV_MEDIA_D"),
      unit        = "cm",
      description = "daily-mean water-level"
    )
  )
}

.co_ideam_add_headers <- function(req, accept = "application/json") {
  httr2::req_headers(
    req,
    Accept = accept,
    `Accept-Language` = "es-CO,es;q=0.9,en;q=0.8",
    `User-Agent` = "hydrodownloadR"
  )
}

.co_ideam_id_chr <- function(x) {
  if (is.numeric(x)) {
    return(format(x, scientific = FALSE, trim = TRUE))
  }
  trimws(as.character(x))
}

.co_ideam_station_key <- function(x) {
  x <- .co_ideam_id_chr(x)
  out <- sub("^0+", "", x)
  out[!nzchar(out)] <- "0"
  out
}

.co_ideam_num <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))
}

.co_ideam_optional_col <- function(df, candidates, default = NA_character_) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) {
    return(rep(default, NROW(df)))
  }
  df[[hit[1]]]
}

.co_ideam_required_col <- function(df, candidates, what) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) {
    rlang::abort(
      paste0(
        "CO_IDEAM: AQUARIUS response does not contain ", what,
        ". Available columns: ", paste(names(df), collapse = ", ")
      )
    )
  }
  df[[hit[1]]]
}

.co_ideam_as_tibble <- function(x) {
  if (is.null(x) || !length(x)) {
    return(tibble::tibble())
  }
  if (is.data.frame(x)) {
    return(tibble::as_tibble(x))
  }
  out <- try(tibble::as_tibble(x), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(tibble::tibble())
  }
  out
}

.co_ideam_empty_points <- function() {
  tibble::tibble(
    timestamp     = as.POSIXct(character(), tz = "UTC"),
    value         = numeric(),
    quality_code  = character(),
    quality_name  = character(),
    quality_desc  = character()
  )
}

# -----------------------------------------------------------------------------
# AQUARIUS parameter and dataset discovery
# -----------------------------------------------------------------------------

.co_ideam_parameter_id <- function(x, parameter_code = "CAUDAL") {
  url <- paste0(x$base_url, "/Data/List/")

  req <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_raw("") |>
    .co_ideam_add_headers(accept = "text/html,application/xhtml+xml")

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::abort(
      paste0("CO_IDEAM: failed to read AQUARIUS parameter list from ", url, ".")
    )
  }

  doc <- try(xml2::read_html(httr2::resp_body_string(resp)), silent = TRUE)
  if (inherits(doc, "try-error")) {
    rlang::abort("CO_IDEAM: AQUARIUS parameter page could not be parsed as HTML.")
  }

  options <- rvest::html_elements(doc, "option[data-code]")
  codes   <- toupper(trimws(rvest::html_attr(options, "data-code")))
  values  <- trimws(rvest::html_attr(options, "value"))

  hit <- which(codes == toupper(parameter_code))
  if (!length(hit)) {
    available <- unique(codes[!is.na(codes) & nzchar(codes)])
    rlang::abort(
      paste0(
        "CO_IDEAM: parameter code '", parameter_code,
        "' was not found. Available codes include: ",
        paste(utils::head(available, 20L), collapse = ", ")
      )
    )
  }

  id <- suppressWarnings(as.integer(values[hit[1]]))
  if (is.na(id)) {
    rlang::abort(
      paste0(
        "CO_IDEAM: the ", toupper(parameter_code),
        " parameter id is not numeric."
      )
    )
  }
  id
}

.co_ideam_parameter_datasets <- function(x,
                                         parameter_code = "CAUDAL",
                                         page_size = 5000L) {
  parameter_id <- .co_ideam_parameter_id(x, parameter_code)
  url <- paste0(x$base_url, "/Data/Data_List")

  page_no <- 1L
  total   <- NA_integer_
  pages   <- list()
  n_read  <- 0L

  repeat {
    req <- httr2::request(url) |>
      httr2::req_url_query(
        page = page_no,
        pageSize = page_size,
        `parameters[0]` = parameter_id
      ) |>
      httr2::req_method("POST") |>
      httr2::req_body_form(
        page = page_no,
        pageSize = page_size,
        `parameters[0]` = parameter_id
      ) |>
      .co_ideam_add_headers()

    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
      rlang::abort(
        paste0("CO_IDEAM: failed to read AQUARIUS dataset-list page ", page_no, ".")
      )
    }

    payload <- try(
      httr2::resp_body_json(resp, simplifyVector = TRUE),
      silent = TRUE
    )
    if (inherits(payload, "try-error") || is.null(payload[["Data"]])) {
      rlang::abort("CO_IDEAM: AQUARIUS dataset-list response is not valid JSON.")
    }

    page_tbl <- .co_ideam_as_tibble(payload[["Data"]])
    if (!NROW(page_tbl)) {
      break
    }

    pages[[length(pages) + 1L]] <- page_tbl
    n_read <- n_read + NROW(page_tbl)

    if (is.na(total) && !is.null(payload[["Total"]])) {
      total <- suppressWarnings(as.integer(payload[["Total"]]))
    }

    if ((!is.na(total) && n_read >= total) ||
        (is.na(total) && NROW(page_tbl) < page_size)) {
      break
    }

    page_no <- page_no + 1L
    if (page_no > 100L) {
      rlang::abort("CO_IDEAM: stopped after 100 AQUARIUS dataset-list pages.")
    }
  }

  dplyr::bind_rows(pages)
}

.co_ideam_daily_index <- function(
    x,
    parameter = .co_ideam_supported_parameters()
) {
  parameter <- match.arg(parameter, .co_ideam_supported_parameters())
  spec <- .co_ideam_parameter_spec(parameter)

  raw <- .co_ideam_parameter_datasets(x, spec$portal_code)
  if (!NROW(raw)) {
    return(tibble::tibble())
  }

  dataset_identifier <- .co_ideam_id_chr(
    .co_ideam_required_col(
      raw,
      c("DatasetIdentifier", "datasetIdentifier"),
      "DatasetIdentifier"
    )
  )

  left  <- toupper(sub("@.*$", "", dataset_identifier))
  param <- sub("\\..*$", "", left)
  label <- sub("^[^.]*\\.", "", left)

  keep <- param == spec$portal_code & label %in% spec$labels
  raw  <- raw[keep, , drop = FALSE]
  if (!NROW(raw)) {
    return(tibble::tibble())
  }

  dataset_identifier <- dataset_identifier[keep]
  label              <- label[keep]

  station_id <- .co_ideam_id_chr(
    .co_ideam_required_col(
      raw,
      c("LocationIdentifier", "locationIdentifier"),
      "LocationIdentifier"
    )
  )

  station_name <- normalize_utf8(trimws(as.character(
    .co_ideam_required_col(raw, c("Location", "location"), "Location")
  )))

  out <- tibble::tibble(
    station_id        = station_id,
    station_key       = .co_ideam_station_key(station_id),
    station_name      = station_name,
    parameter         = parameter,
    unit              = spec$unit,
    lon               = .co_ideam_num(.co_ideam_optional_col(raw, c("LocX", "locX"), NA_real_)),
    lat               = .co_ideam_num(.co_ideam_optional_col(raw, c("LocY", "locY"), NA_real_)),
    dataset_id        = .co_ideam_id_chr(
      .co_ideam_required_col(raw, c("DatasetId", "datasetId"), "DatasetId")
    ),
    dataset_identifier = dataset_identifier,
    dataset_label      = label,
    dataset_start      = as.character(
      .co_ideam_optional_col(raw, c("StartOfRecord", "startOfRecord"))
    ),
    dataset_end        = as.character(
      .co_ideam_optional_col(raw, c("EndOfRecord", "endOfRecord"))
    ),
    dataset_priority   = match(label, spec$labels)
  )

  out <- out[
    !is.na(out$station_id) & nzchar(out$station_id) &
      !is.na(out$dataset_id) & nzchar(out$dataset_id),
    ,
    drop = FALSE
  ]

  out |>
    dplyr::arrange(station_key, dataset_priority, dataset_id) |>
    dplyr::distinct(dataset_id, .keep_all = TRUE)
}

.co_ideam_all_daily_index <- function(x) {
  parameters <- .co_ideam_supported_parameters()
  pieces <- lapply(
    parameters,
    function(parameter) .co_ideam_daily_index(x, parameter)
  )

  out <- dplyr::bind_rows(pieces)
  if (!NROW(out)) {
    return(tibble::tibble())
  }

  out$parameter_priority <- match(out$parameter, parameters)
  out |>
    dplyr::arrange(
      station_key,
      parameter_priority,
      dataset_priority,
      dataset_id
    )
}

# -----------------------------------------------------------------------------
# Optional National Station Catalogue enrichment
# -----------------------------------------------------------------------------

.co_ideam_station_catalog <- function() {
  url <- .co_ideam_catalog_url()

  req <- httr2::request(url) |>
    httr2::req_url_query(`$limit` = 50000L) |>
    .co_ideam_add_headers()

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::warn(
      "CO_IDEAM: National Station Catalogue could not be read; AQUARIUS metadata will still be returned."
    )
    return(tibble::tibble())
  }

  payload <- try(
    httr2::resp_body_json(resp, simplifyVector = TRUE),
    silent = TRUE
  )
  raw <- .co_ideam_as_tibble(payload)
  if (inherits(payload, "try-error") || !NROW(raw) || !"codigo" %in% names(raw)) {
    rlang::warn(
      "CO_IDEAM: National Station Catalogue response could not be parsed; AQUARIUS metadata will still be returned."
    )
    return(tibble::tibble())
  }

  catalog_id <- .co_ideam_id_chr(raw[["codigo"]])
  name <- as.character(.co_ideam_optional_col(raw, "nombre"))
  name <- normalize_utf8(trimws(sub("\\s*\\[[^]]+\\]\\s*$", "", name)))

  tibble::tibble(
    station_key      = .co_ideam_station_key(catalog_id),
    catalog_id       = catalog_id,
    catalog_name     = name,
    catalog_lon      = .co_ideam_num(.co_ideam_optional_col(raw, "longitud", NA_real_)),
    catalog_lat      = .co_ideam_num(.co_ideam_optional_col(raw, "latitud", NA_real_)),
    catalog_altitude = .co_ideam_num(.co_ideam_optional_col(raw, "altitud", NA_real_))
  ) |>
    dplyr::distinct(station_key, .keep_all = TRUE)
}

# -----------------------------------------------------------------------------
# Public station method
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_CO_IDEAM <- function(x, ...) {
  idx <- .co_ideam_all_daily_index(x)

  if (!NROW(idx)) {
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

  # Dataset-list location metadata are identical across daily datasets. Prefer
  # discharge over level, then operational over historical metadata, when a
  # location occurs in more than one supported series.
  loc <- idx[!duplicated(idx$station_key), , drop = FALSE]
  catalog <- .co_ideam_station_catalog()

  if (NROW(catalog)) {
    m <- match(loc$station_key, catalog$station_key)
    catalog_name <- catalog$catalog_name[m]
    catalog_lon  <- catalog$catalog_lon[m]
    catalog_lat  <- catalog$catalog_lat[m]
    altitude     <- catalog$catalog_altitude[m]
  } else {
    catalog_name <- rep(NA_character_, NROW(loc))
    catalog_lon  <- rep(NA_real_, NROW(loc))
    catalog_lat  <- rep(NA_real_, NROW(loc))
    altitude     <- rep(NA_real_, NROW(loc))
  }

  use_catalog_name <- !is.na(catalog_name) & nzchar(trimws(catalog_name))
  station_name <- loc$station_name
  station_name[use_catalog_name] <- catalog_name[use_catalog_name]
  station_name <- normalize_utf8(station_name)

  lon <- loc$lon
  lat <- loc$lat
  lon[is.na(lon)] <- catalog_lon[is.na(lon)]
  lat[is.na(lat)] <- catalog_lat[is.na(lat)]

  tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = loc$station_id,
    station_name       = station_name,
    station_name_ascii = to_ascii(station_name),
    river              = NA_character_,
    river_ascii        = NA_character_,
    lat                = lat,
    lon                = lon,
    area               = NA_real_,
    altitude           = altitude
  ) |>
    dplyr::arrange(station_id)
}

# -----------------------------------------------------------------------------
# Direct JSON time-series download
# -----------------------------------------------------------------------------

.co_ideam_parse_with_formats <- function(x, formats) {
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")

  for (fmt in formats) {
    todo <- which(is.na(out) & !is.na(x) & nzchar(x))
    if (!length(todo)) {
      break
    }

    parsed <- suppressWarnings(as.POSIXct(strptime(x[todo], format = fmt, tz = "UTC")))
    good <- !is.na(parsed)
    if (any(good)) {
      out[todo[good]] <- parsed[good]
    }
  }
  out
}

.co_ideam_parse_timestamp <- function(x) {
  x <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")

  # Older ASP.NET deployments may return /Date(<milliseconds><offset>)/.
  is_ms <- !is.na(x) & grepl("^/Date\\(-?[0-9]+", x)
  if (any(is_ms, na.rm = TRUE)) {
    ms <- suppressWarnings(as.numeric(sub("^/Date\\((-?[0-9]+).*$", "\\1", x[is_ms])))
    out[is_ms] <- as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC")
  }

  todo <- which(is.na(out) & !is.na(x) & nzchar(x))
  if (!length(todo)) {
    return(out)
  }

  z <- x[todo]
  z <- sub("Z$", "+0000", z)
  z <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", z)

  has_offset <- grepl("[+-][0-9]{4}$", z)

  parsed <- as.POSIXct(rep(NA_real_, length(z)), origin = "1970-01-01", tz = "UTC")
  if (any(has_offset)) {
    parsed[has_offset] <- .co_ideam_parse_with_formats(
      z[has_offset],
      c(
        "%Y-%m-%dT%H:%M:%OS%z",
        "%Y-%m-%d %H:%M:%OS%z"
      )
    )
  }

  # DatasetGrid is requested with timezone=-300. When the returned string does
  # not include an offset, attach Colombia's fixed UTC-05:00 offset explicitly.
  if (any(!has_offset)) {
    local <- paste(z[!has_offset], "-0500")
    parsed[!has_offset] <- .co_ideam_parse_with_formats(
      local,
      c(
        "%Y-%m-%dT%H:%M:%OS %z",
        "%Y-%m-%d %H:%M:%OS %z",
        "%Y-%m-%d %z"
      )
    )
  }

  out[todo] <- parsed
  out
}

.co_ideam_parse_dataset_page <- function(page_tbl) {
  if (!NROW(page_tbl)) {
    return(.co_ideam_empty_points())
  }

  timestamp_raw <- .co_ideam_required_col(
    page_tbl,
    c("TimeStamp", "Timestamp", "timeStamp", "timestamp"),
    "TimeStamp"
  )
  value_raw <- .co_ideam_required_col(
    page_tbl,
    c("Value", "value"),
    "Value"
  )

  timestamp <- .co_ideam_parse_timestamp(timestamp_raw)
  value     <- .co_ideam_num(value_raw)

  quality_code <- as.character(
    .co_ideam_optional_col(page_tbl, c("Grade", "GradeCode", "grade"))
  )
  quality_name <- as.character(
    .co_ideam_optional_col(page_tbl, c("ApprovalLevel", "Approval", "approval"))
  )
  quality_desc <- as.character(
    .co_ideam_optional_col(page_tbl, c("Qualifiers", "Qualifier", "qualifier"))
  )

  quality_code[quality_code %in% c("NA", "NULL", "null")] <- NA_character_
  quality_name[quality_name %in% c("NA", "NULL", "null")] <- NA_character_
  quality_desc[quality_desc %in% c("NA", "NULL", "null")] <- NA_character_

  keep <- !is.na(timestamp) & is.finite(value)
  tibble::tibble(
    timestamp    = timestamp[keep],
    value        = value[keep],
    quality_code = quality_code[keep],
    quality_name = quality_name[keep],
    quality_desc = quality_desc[keep]
  )
}

.co_ideam_download_dataset <- function(x,
                                       dataset_id,
                                       page_size = 50000L) {
  url <- paste0(x$base_url, "/Data/DatasetGrid")

  page_no <- 1L
  total   <- NA_integer_
  n_read  <- 0L
  pages   <- list()

  repeat {
    req <- httr2::request(url) |>
      httr2::req_url_query(
        dataset = dataset_id,
        sort = "TimeStamp-asc",
        page = page_no,
        pageSize = page_size,
        interval = "Latest",
        timezone = -300,
        alldata = "true",
        virtual = "true"
      ) |>
      .co_ideam_add_headers()

    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
      rlang::warn(
        paste0("CO_IDEAM: download failed for AQUARIUS dataset ", dataset_id, ".")
      )
      return(.co_ideam_empty_points())
    }

    payload <- try(
      httr2::resp_body_json(resp, simplifyVector = TRUE),
      silent = TRUE
    )
    if (inherits(payload, "try-error") || is.null(payload[["Data"]])) {
      rlang::warn(
        paste0("CO_IDEAM: invalid JSON for AQUARIUS dataset ", dataset_id, ".")
      )
      return(.co_ideam_empty_points())
    }

    page_tbl <- .co_ideam_as_tibble(payload[["Data"]])
    if (!NROW(page_tbl)) {
      break
    }

    pages[[length(pages) + 1L]] <- .co_ideam_parse_dataset_page(page_tbl)
    n_read <- n_read + NROW(page_tbl)

    if (is.na(total) && !is.null(payload[["Total"]])) {
      total <- suppressWarnings(as.integer(payload[["Total"]]))
    }

    if ((!is.na(total) && n_read >= total) ||
        (is.na(total) && NROW(page_tbl) < page_size)) {
      break
    }

    page_no <- page_no + 1L
    if (page_no > 100L) {
      rlang::warn(
        paste0("CO_IDEAM: stopped dataset ", dataset_id, " after 100 pages.")
      )
      break
    }
  }

  if (!length(pages)) {
    return(.co_ideam_empty_points())
  }

  dplyr::bind_rows(pages) |>
    dplyr::arrange(timestamp)
}

.co_ideam_format_missing_ids <- function(x, n = 10L) {
  x <- unique(as.character(x))
  shown <- utils::head(x, n)
  suffix <- if (length(x) > n) paste0(" (and ", length(x) - n, " more)") else ""
  paste0(paste(shown, collapse = ", "), suffix)
}

# -----------------------------------------------------------------------------
# Public time-series method
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_CO_IDEAM <- function(x,
                                              parameter = c(
                                                "water_discharge",
                                                "water_level"
                                              ),
                                              stations = NULL,
                                              start_date = NULL,
                                              end_date = NULL,
                                              mode = c("complete", "range"),
                                              exclude_quality = NULL,
                                              ...) {
  parameter <- match.arg(parameter, .co_ideam_supported_parameters())
  mode      <- match.arg(mode)
  spec      <- .co_ideam_parameter_spec(parameter)

  rng <- resolve_dates(mode, start_date, end_date)
  idx <- .co_ideam_daily_index(x, parameter)

  if (!NROW(idx)) {
    return(tibble::tibble())
  }

  if (is.null(stations)) {
    station_keys <- unique(idx$station_key)
  } else {
    requested_ids  <- unique(.co_ideam_id_chr(stations))
    requested_keys <- .co_ideam_station_key(requested_ids)
    found           <- requested_keys %in% idx$station_key

    if (any(!found)) {
      rlang::warn(
        paste0(
          "CO_IDEAM: no supported ", spec$description,
          " dataset found for station(s): ",
          .co_ideam_format_missing_ids(requested_ids[!found])
        )
      )
    }
    station_keys <- unique(requested_keys[found])
  }

  if (!length(station_keys)) {
    return(tibble::tibble())
  }

  download_one <- function(dataset_id) {
    .co_ideam_download_dataset(x, dataset_id)
  }

  limited_download <- ratelimitr::limit_rate(
    download_one,
    rate = ratelimitr::rate(
      n = x$rate_cfg$n %||% 1L,
      period = x$rate_cfg$period %||% 1
    )
  )

  pb <- progress::progress_bar$new(
    total = length(station_keys),
    format = "CO_IDEAM [:bar] :current/:total (:percent) eta: :eta"
  )

  fetch_station <- function(station_key) {
    rows <- idx[idx$station_key == station_key, , drop = FALSE]
    if (!NROW(rows)) {
      return(tibble::tibble())
    }

    series <- lapply(seq_len(NROW(rows)), function(i) {
      dat <- limited_download(rows$dataset_id[i])
      if (!NROW(dat)) {
        return(tibble::tibble())
      }
      dat$dataset_priority   <- rows$dataset_priority[i]
      dat$dataset_identifier <- rows$dataset_identifier[i]
      dat
    })

    dat <- dplyr::bind_rows(series)
    if (!NROW(dat)) {
      return(tibble::tibble())
    }

    # The operational label has priority 1 and therefore wins only at duplicate
    # timestamps; the historical label supplies the remaining record.
    dat <- dat |>
      dplyr::arrange(timestamp, dataset_priority) |>
      dplyr::distinct(timestamp, .keep_all = TRUE)

    if (identical(mode, "range")) {
      local_date <- as.Date(dat$timestamp, tz = "America/Bogota")
      keep <- local_date >= rng$start_date & local_date <= rng$end_date
      dat <- dat[keep, , drop = FALSE]
    }

    if (!NROW(dat)) {
      return(tibble::tibble())
    }

    tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = rows$station_id[1],
      parameter     = parameter,
      timestamp     = dat$timestamp,
      value         = dat$value,
      unit          = spec$unit,
      quality_code  = dat$quality_code,
      quality_name  = dat$quality_name,
      quality_desc  = dat$quality_desc,
      source_url    = x$base_url
    )
  }

  out <- lapply(station_keys, function(station_key) {
    pb$tick()
    fetch_station(station_key)
  })

  res <- dplyr::bind_rows(out)
  if (!NROW(res)) {
    return(res)
  }

  dplyr::arrange(res, station_id, timestamp)
}
