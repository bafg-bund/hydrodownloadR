# ==== Rwanda (RWB Water Portal) adapter ======================================
# Base: https://waterportal.rwb.rw
#
# Data access:
#   - Station list (HTML table):
#       https://waterportal.rwb.rw/data/surface_water
#     Columns (as of 2025-11): "Location name", "Identifier", "Location type",
#     "Longitude", "Latitude", "SRID".
#
#   - Per-station page with short, irregular field-visit data:
#       https://waterportal.rwb.rw/location_ng_info/{Identifier}
#     The field-visit table has columns like:
#       Date | Parameter | Unit | Value
#     With Parameter values "Discharge" (m^3/s) and "Stage" (m), etc. :contentReference[oaicite:1]{index=1}
#
# Notes:
#   - This adapter ONLY scrapes the field-visit table.
#     It ignores the "Stage - Historical" / "Stage - Observers" download links.
#   - Time stamps are sub-daily instants; we keep them as instants
#     (no daily aggregation).
#   - HTML layout is brittle; if the portal changes, this may break.

# -----------------------------------------------------------------------------
# Registration
# -----------------------------------------------------------------------------

#' @export
register_RW_RWB <- function() {
  register_service(
    provider_id   = "RW_RWB",
    provider_name = "Rwanda – Rwanda Water Resources Board (Water Portal)",
    country       = "RW",
    base_url      = "https://waterportal.rwb.rw",
    rate_cfg      = list(n = 1, period = 1),  # be polite: 1 request / second
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_RW_RWB <- function(x, ...) {
  c(
    "water_discharge",
    "water_level",
    "water_temperature",
    "electrical_conductivity",
    "salinity",
    "total_dissolved_solids",
    "ph",
    "dissolved_oxygen_saturation",
    "total_suspended_solids",
    "turbidity"
  )
}
# -----------------------------------------------------------------------------
# Parameter mapping
# -----------------------------------------------------------------------------

.rw_param_map <- function(parameter) {
  # Map hydrodownloadR "parameter" -> field-visit "Parameter" + unit(s).
  #
  # pm$field_params is a character vector of acceptable Parameter strings
  # pm$unit can be a single string or a character vector of allowed units
  # (matching is done case-insensitively).

  switch(
    parameter,

    # existing ones -----------------------------------------------------------
    water_discharge = list(
      field_params = c("Discharge", "Flow"),
      unit         = "m^3/s"
    ),

    water_level = list(
      field_params = c("Stage", "Water level", "Water Level"),
      unit         = "m"
    ),

    water_temperature = list(
      field_params = c("Water Temp", "Water temperature", "Temperature"),
      unit         = c("degC", "°C")
    ),

    # new ones ---------------------------------------------------------------
    electrical_conductivity = list(
      # 2  Cond                uS/cm
      field_params = c("Cond", "Conductivity", "Cond.", "Electric Cond"),
      unit         = c("uS/cm", "µS/cm", "us/cm")
    ),

    salinity = list(
      # 3  Salinity            mg/l
      field_params = c("Salinity"),
      unit         = c("mg/l")
    ),

    total_dissolved_solids = list(
      # 4  TDS                 mg/l
      field_params = c("TDS", "Total Dissolved Solids"),
      unit         = c("mg/l")
    ),

    ph = list(
      # 5  pH                  pH Units
      field_params = c("pH", "pH value"),
      unit         = c("pH Units", "pH")
    ),

    dissolved_oxygen_saturation = list(
      # 6  Dis Oxygen Sat      %
      field_params = c("Dis Oxygen Sat", "Dissolved Oxygen Sat", "DO Sat"),
      unit         = c("%")
    ),

    total_suspended_solids = list(
      # 7  TSS                 mg/l
      field_params = c("TSS", "Total Suspended Solids"),
      unit         = c("mg/l")
    ),

    turbidity = list(
      # 8  Turbidity, Nephelom _NTU
      field_params = c("Turbidity, Nephelom", "Turbidity"),
      unit         = c("_NTU", "NTU")
    ),

    # fallback ---------------------------------------------------------------
    rlang::abort(
      paste(
        "RW_RWB supports",
        "'water_discharge', 'water_level', 'water_temperature',",
        "'electrical_conductivity', 'salinity', 'total_dissolved_solids',",
        "'ph', 'dissolved_oxygen_saturation',",
        "'total_suspended_solids', 'turbidity'."
      )
    )
  )
}


# -----------------------------------------------------------------------------
# Stations (S3 method)
# -----------------------------------------------------------------------------

#' @export
stations.hydro_service_RW_RWB <- function(x, ...) {
  req <- build_request(x, path = "/data/surface_water")
  resp <- perform_request(req)

  html <- xml2::read_html(httr2::resp_body_string(resp))

  tables <- rvest::html_elements(html, "table")
  if (!length(tables)) {
    rlang::abort("RW_RWB: no table found on /data/surface_water (layout change?).")
  }

  raw_tbl <- rvest::html_table(tables[[1]], fill = TRUE)
  names(raw_tbl) <- trimws(names(raw_tbl))

  # Safe extraction with fallbacks
  loc_name   <- col_or_null(raw_tbl, "Location name") %||%
    col_or_null(raw_tbl, "Location") %||%
    col_or_null(raw_tbl, "Name")

  identifier <- col_or_null(raw_tbl, "Identifier") %||%
    col_or_null(raw_tbl, "ID") %||%
    col_or_null(raw_tbl, "Code")


  lon    <- col_or_null(raw_tbl, "Longitude")

  lat    <- col_or_null(raw_tbl, "Latitude")


  loc_name_clean <- normalize_utf8(loc_name)

  lon_num <- suppressWarnings(as.numeric(lon))
  lat_num <- suppressWarnings(as.numeric(lat))

  out <- tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = as.character(identifier),
    station_name       = loc_name_clean,
    river              = NA_character_,
    lat                = lat_num,
    lon                = lon_num,
    area               = NA_real_,
    altitude           = NA_real_
  )

  out <- out[!is.na(out$lat),]
}

# -----------------------------------------------------------------------------
# Internal helpers for time series
# -----------------------------------------------------------------------------

.rw_station_path <- function(station_id) {
  # Normalize and URL-encode station id for use as a path segment.
  id_clean <- trimws(as.character(station_id))
  id_clean <- gsub("\\s+", " ", id_clean)
  id_enc   <- curl::curl_escape(id_clean)
  paste0("location_ng_info/", id_enc)
}

.rw_pick_fieldvisit_table <- function(html) {
  # Find the table that looks like the field-visit table
  # (has "Date" + "Parameter"/"Unit"/"Value" headers).
  nodes <- rvest::html_elements(html, "table")
  if (!length(nodes)) return(NULL)

  tbls <- lapply(nodes, function(nd) {
    out <- try(rvest::html_table(nd, fill = TRUE), silent = TRUE)
    if (inherits(out, "try-error")) return(NULL)
    if (!NROW(out)) return(NULL)
    out
  })
  tbls <- Filter(Negate(is.null), tbls)
  if (!length(tbls)) return(NULL)

  idx <- which(vapply(tbls, function(df) {
    nm <- trimws(names(df))
    "Date" %in% nm &&
      ("Parameter" %in% nm || "Unit" %in% nm || "Value" %in% nm)
  }, logical(1)))

  if (!length(idx)) {
    # fallback: any table containing a "Date" column
    idx <- which(vapply(tbls, function(df) {
      "Date" %in% trimws(names(df))
    }, logical(1)))
  }
  if (!length(idx)) return(NULL)

  tbl <- tbls[[idx[1]]]
  names(tbl) <- trimws(names(tbl))
  tbl
}

.rw_fetch_field_visits <- function(x,
                                   station_id,
                                   parameter,
                                   pm,
                                   rng,
                                   mode) {
  path <- .rw_station_path(station_id)

  req <- httr2::request(x$base_url) |>
    httr2::req_user_agent(
      "hydrodownloadR (+https://github.com/your-org/hydrodownloadR)"
    ) |>
    httr2::req_url_path_append(path)

  resp <- try(perform_request(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    rlang::warn(paste0("RW_RWB: request failed for station ", station_id))
    return(tibble::tibble())
  }

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    if (status %in% c(404L, 410L)) {
      return(tibble::tibble())
    }
    rlang::warn(paste0(
      "RW_RWB: HTTP ", status, " for station ", station_id,
      " – skipping."
    ))
    return(tibble::tibble())
  }

  html <- try(
    xml2::read_html(httr2::resp_body_string(resp)),
    silent = TRUE
  )
  if (inherits(html, "try-error")) {
    rlang::warn(paste0("RW_RWB: HTML parse failed for station ", station_id))
    return(tibble::tibble())
  }

  tbl <- .rw_pick_fieldvisit_table(html)
  if (is.null(tbl)) {
    # No field-visit table; nothing to return.
    return(tibble::tibble())
  }

  date_raw   <- col_or_null(tbl, "Date")
  param_raw  <- col_or_null(tbl, "Parameter")
  unit_raw   <- col_or_null(tbl, "Unit")
  value_raw  <- col_or_null(tbl, "Value")

  if (is.null(date_raw) || is.null(value_raw)) {
    return(tibble::tibble())
  }

  n <- length(date_raw)
  keep <- rep(TRUE, n)

  # Filter by parameter strings (if we have that column)
  if (!is.null(param_raw)) {
    prm_norm <- tolower(trimws(as.character(param_raw)))
    prm_ok   <- prm_norm %in% tolower(pm$field_params)
    keep <- keep & prm_ok
  }

  # Filter by unit (if present)
  if (!is.null(pm$unit) && !is.null(unit_raw)) {
    unit_norm <- tolower(trimws(as.character(unit_raw)))
    wanted    <- tolower(trimws(as.character(pm$unit)))
    u_ok      <- unit_norm %in% wanted
    keep <- keep & u_ok
  }


  if (!any(keep)) {
    return(tibble::tibble())
  }

  date_use  <- date_raw[keep]
  value_use <- value_raw[keep]

  ts_parsed <- suppressWarnings(lubridate::as_datetime(date_use))

  # Date-range filter (if requested)
  keep2 <- !is.na(ts_parsed)
  if (mode == "range") {
    tmin <- as.POSIXct(rng$start_date)
    tmax <- as.POSIXct(rng$end_date) + 86399
    keep2 <- keep2 & ts_parsed >= tmin & ts_parsed <= tmax
  }

  if (!any(keep2)) {
    return(tibble::tibble())
  }

  ts_final   <- ts_parsed[keep2]
  val_num    <- suppressWarnings(as.numeric(value_use[keep2]))

  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = as.character(station_id),
    parameter     = parameter,
    timestamp     = ts_final,
    value         = val_num,
    unit          = pm$unit,
    quality_code  = NA_character_,
    source_url    = paste0(x$base_url, "/", path)
  )
}

# -----------------------------------------------------------------------------
# Time series (S3 method)
# -----------------------------------------------------------------------------

#' @export
timeseries.hydro_service_RW_RWB <- function(x,
                                            parameter = c(
                                              "water_discharge",
                                              "water_level",
                                              "water_temperature",
                                              "electrical_conductivity",
                                              "salinity",
                                              "total_dissolved_solids",
                                              "ph",
                                              "dissolved_oxygen_saturation",
                                              "total_suspended_solids",
                                              "turbidity"
                                            ),
                                            stations    = NULL,
                                            start_date  = NULL,
                                            end_date    = NULL,
                                            mode        = c("complete", "range"),
                                            exclude_quality = NULL,
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .rw_param_map(parameter)

  # station_id vector
  station_vec <- if (is.null(stations)) {
    st <- stations.hydro_service_RW_RWB(x)
    st$station_id
  } else {
    stations
  }

  station_vec <- unique(as.character(station_vec))
  if (!length(station_vec)) return(tibble::tibble())

  batches <- chunk_vec(station_vec, 20L)

  pb <- progress::progress_bar$new(
    total  = length(batches),
    format = "RW_RWB [:bar] :current/:total (:percent) eta: :eta"
  )

  fetch_one <- function(st_id) {
    .rw_fetch_field_visits(
      x          = x,
      station_id = st_id,
      parameter  = parameter,
      pm         = pm,
      rng        = rng,
      mode       = mode
    )
  }

  limited <- ratelimitr::limit_rate(
    fetch_one,
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  out <- lapply(batches, function(batch) {
    pb$tick()
    dplyr::bind_rows(lapply(batch, limited))
  })

  dplyr::bind_rows(out)
}
