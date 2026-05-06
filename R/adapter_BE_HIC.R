# R/BE_HIC.R

# ---- Registration ------------------------------------------------------------

#' @keywords internal
#' @noRd
register_BE_HIC <- function() {
  register_service_usage(
    provider_id   = "BE_HIC",
    provider_name = "Hydrological Information Centre - HIC (Flanders)",
    country       = "Belgium",
    base_url      = "https://hicws.vlaanderen.be/KiWIS/KiWIS",
    rate_cfg      = list(n = 1, period = 5),
    auth          = list(type = "none")
  )
}

# What parameters this service exposes to users of the package
#' @export
timeseries_parameters.hydro_service_BE_HIC <- function(x, ...) {
  c(
    "water_discharge", "water_level", "water_temperature",
    "chlorophyll", "conductivity", "salinity", "turbidity",
    "oxygen_concentration", "oxygen_saturation", "ph"
  )
}


# Internal: map our canonical parameter names to HIC/KiWIS fields/units
.be_param_map <- function(parameter) {
  switch(
    parameter,
    water_discharge      = list(par = "Q",       unit = "m^3/s", group_id = "156169"),
    water_level          = list(par = "H",       unit = "m",     group_id = "156162"),

    # High-resolution water quality (as provided)
    chlorophyll          = list(par = "Chl",     unit = "\u00B5g/L",  group_id = "156172"),
    conductivity         = list(par = "EC",      unit = "\u00B5S/cm", group_id = "156173"),
    salinity             = list(par = "Salinity",unit = "PSU",   group_id = "421208"),
    turbidity            = list(par = "Turbidity",unit= "FNU",   group_id = "156202"),
    water_temperature    = list(par = "TWater",  unit = "\u00B0C",    group_id = "156200"),
    oxygen_concentration = list(par = "DO",      unit = "mg/L",  group_id = "156207"),
    oxygen_saturation    = list(par = "DO_sat",  unit = "%",     group_id = "156208"),
    ph                   = list(par = "pH",      unit = "",      group_id = "156197"),

    rlang::abort(
      "BE_HIC supports: water_discharge, water_level, chlorophyll, conductivity, salinity, turbidity, water_temperature, oxygen_concentration, oxygen_saturation, ph."
    )
  )
}


# ---- Stations ---------------------------------------------------------------

# NOTE:
# - Pull all stations via KiWIS getStationList (JSON).
# - Exclude any stations that only belong to virtual group 260592
#   ("Calculated discharges at important locations waterways").
#   We identify those by querying getTimeseriesList for that group and
#   dropping any matching station_nos.

.be_parse_kisters_table <- function(js) {
  # columns + data form
  if (!is.null(js$columns) && !is.null(js$data)) {
    # get matrix
    mat <- if (is.list(js$data) && length(js$data) == 1 && is.matrix(js$data[[1]])) {
      js$data[[1]]
    } else if (is.matrix(js$data)) {
      js$data
    } else {
      as.matrix(js$data)
    }
    if (is.null(dim(mat)) || nrow(mat) == 0) return(tibble::tibble())
    df <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)

    # split single string "A,B,C" into vector
    cols <- js$columns
    if (is.character(cols) && length(cols) == 1L) {
      cols <- strsplit(cols, "\\s*,\\s*")[[1]]
    }
    if (is.character(cols) && length(cols) == ncol(df)) {
      names(df) <- cols
    } else {
      names(df) <- paste0("V", seq_len(ncol(df)))
    }
    return(tibble::as_tibble(df, .name_repair = "minimal"))
  }

  # header-row matrix form
  if (!is.null(dim(js))) {
    if (nrow(js) < 2) return(tibble::tibble())
    hdr <- as.character(js[1, , drop = TRUE])
    dat <- js[-1, , drop = FALSE]
    out <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
    names(out) <- hdr
    return(tibble::as_tibble(out))
  }

  tibble::as_tibble(js)
}

#' @export
stations.hydro_service_BE_HIC <- function(x, ...) {
  # Pull station core + custom attrs (ca_sta) including station_gauge_datum (+ Catchment)
  q1 <- list(
    service                = "kisters",
    type                   = "queryServices",
    request                = "getStationList",
    datasource             = 4,
    format                 = "json",
    returnfields           = "station_no,station_name,station_no,station_latitude,station_longitude,site_name,ca_sta",
    # leave empty to return all available station custom attributes
    ca_sta_returnfields    = ""
  )
  req1  <- build_request(x, path = "", query = q1)
  resp1 <- perform_request(req1)
  js1   <- httr2::resp_body_json(resp1, simplifyVector = TRUE)
  hdr <- as.character(js1[1, , drop = TRUE])
  dat <- js1[-1, , drop = FALSE]
  out <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- hdr
  df <- tibble::as_tibble(out)
  # df    <- .be_parse_kisters_table(js1)

  # defensive aliasing
  st_id       <- col_or_null(df, "station_no") %||% col_or_null(df, "station_id")
  st_name_raw <- normalize_utf8(col_or_null(df, "station_name"))
  lat         <- suppressWarnings(as.numeric(col_or_null(df, "station_latitude")))
  lon         <- suppressWarnings(as.numeric(col_or_null(df, "station_longitude")))

  # split "Station/River"
  has_slash    <- !is.na(st_name_raw) & grepl("/", st_name_raw, fixed = TRUE)
  station_part <- ifelse(has_slash, sub("/.*$", "", st_name_raw), st_name_raw)
  river_part   <- ifelse(has_slash, sub("^[^/]*/", "", st_name_raw), NA_character_)
  station_part <- normalize_utf8(trimws(station_part))
  river_part   <- normalize_utf8(trimws(river_part))

  # custom-attribute fields (names differ across KiWIS setups; try several)
  vdatum_raw <- col_or_null(df, "station_gauge_datum_postfix")

  catch_raw <- col_or_null(df, "CATCHMENT_SIZE")
  area_km2  <- parse_area_km2(catch_raw)
  area_km2[!is.na(area_km2) & abs(area_km2 - 1) < 1e-9] <- NA_real_
  alt  <- col_or_null(df, "ALTITUDE") %||% NA_real_

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = as.character(st_id),
    station_name  = station_part,
    river         = river_part,
    lat           = lat,
    lon           = lon,
    area          = area_km2,
    altitude      = alt,
    vertical_datum= vdatum_raw
  )

  # drop rows without coordinates
  out <- out[!is.na(out$lat) & !is.na(out$lon), , drop = FALSE]

  # Exclude VIRTUAL group 260592
  qv <- list(service="kisters", type="queryServices", datasource=4,
             request="getTimeseriesList", timeseriesgroup_id="260592",
             format="json")
  reqv  <- build_request(x, path = "", query = qv)
  respv <- perform_request(reqv)
  jsv   <- httr2::resp_body_json(respv, simplifyVector = TRUE)
  hdrv  <- as.character(jsv[1, , drop = TRUE])
  datv  <- jsv[-1, , drop = FALSE]
  outv  <- as.data.frame(datv, stringsAsFactors = FALSE, check.names = FALSE)
  names(outv) <- hdrv
  # dv    <- .be_parse_kisters_table(jsv)

  if (nrow(outv)) {
    v_stations <- as.character(col_or_null(outv, "station_no") %||% col_or_null(outv, "station_id"))
    v_stations <- unique(stats::na.omit(v_stations))
    if (length(v_stations)) {
      out <- dplyr::filter(out, !.data$station_id %in% v_stations)
    }
  }

  out
}


# --- small helpers ------------------------------------------------------------

.be_empty_ts <- function(x, parameter, unit) {
  tibble::tibble(
    country        = x$country,
    provider_id    = x$provider_id,
    provider_name  = x$provider_name,
    station_id     = character(0),
    parameter      = character(0),
    timestamp      = as.POSIXct(character(0), tz = "UTC"),
    value          = numeric(0),
    unit           = character(0),
    vertical_datum = character(0),
    quality_code   = character(0),
    quality_name   = character(0),
    quality_desc   = character(0),
    source_url     = character(0)
  )
}

# Parse KiWIS datasource=4 responses (first row = headers)
.be_parse_header_table <- function(js) {
  if (is.null(js) || length(js) == 0) return(tibble::tibble())
  hdr <- as.character(js[1, , drop = TRUE])
  dat <- js[-1, , drop = FALSE]
  if (NROW(dat) == 0) return(tibble::tibble())
  out <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE)
  names(out) <- hdr
  tibble::as_tibble(out)
}

# Discover daily series for requested parameter using GRDC groups
# Discover series for requested parameter using its group_id
.be_discover_ts_map <- function(x, stations, parameter) {
  pm <- .be_param_map(parameter)

  q <- list(
    service            = "kisters",
    type               = "queryServices",
    request            = "getTimeseriesList",
    timeseriesgroup_id = pm$group_id,
    datasource         = 4,
    format             = "json",
    returnfields       = "station_no,ts_id"
  )

  req  <- build_request(x, path = "", query = q)
  resp <- perform_request(req)
  js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  # datasource=4 header-row parsing
  hdr <- as.character(js[1, , drop = TRUE])
  dat <- js[-1, , drop = FALSE]
  if (NROW(dat) == 0) {
    return(tibble::tibble(station_id = character(), ts_id = character()))
  }
  df <- tibble::as_tibble(`colnames<-`(
    as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE),
    hdr
  ))

  station_col <- col_or_null(df, "station_no")
  ts_col      <- col_or_null(df, "ts_id")

  out <- tibble::tibble(
    station_id = as.character(station_col),
    ts_id      = as.character(ts_col)
  )

  out <- out[!is.na(out$station_id) & !is.na(out$ts_id) & nzchar(out$ts_id), , drop = FALSE]

  if (!is.null(stations) && length(stations)) {
    want <- unique(as.character(stations))
    out  <- dplyr::filter(out, .data$station_id %in% want)
  }

  dplyr::distinct(out, .data$station_id, .data$ts_id, .keep_all = TRUE)
}





# Put this near your other helpers in R/BE_HIC.R

.be_perform_with_retry <- function(req) {
  # Polite retries for 429/5xx with exponential backoff + small jitter
  httr2::req_retry(
    req,
    max_tries   = 6,
    backoff     = ~ min(120, 2^.x) + stats::runif(1, 0, 0.5),
    is_transient = function(resp) {
      s <- httr2::resp_status(resp)
      s == 429L || (s >= 500L && s < 600L)
    }
  ) |>
    httr2::req_perform()
}




# --- main timeseries (AU_BOM-style) ------------------------------------------
#' @export
timeseries.hydro_service_BE_HIC <- function(x,
                                            parameter = c("water_discharge", "water_level",
                                                          "chlorophyll", "conductivity",
                                                          "salinity", "turbidity",
                                                          "water_temperature", "oxygen_concentration",
                                                          "oxygen_saturation", "ph"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            exclude_quality = NULL,
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .be_param_map(parameter)

  start_utc <- if (inherits(rng$start, "POSIXt")) rng$start else as.POSIXct(rng$start, tz = "UTC")
  end_utc   <- if (inherits(rng$end,   "POSIXt")) rng$end   else as.POSIXct(rng$end,   tz = "UTC")
  end_utc   <- end_utc + 24*3600 - 1  # include the full end day


  # ---- resolve stations ------------------------------------------------------
  if (is.null(stations) || !length(stations)) {
    st  <- stations.hydro_service_BE_HIC(x)
    stations <- unique(stats::na.omit(as.character(st$station_id)))
  } else {
    stations <- unique(as.character(stations))
  }

  # ---- discover series via group --------------------------------------------
  map_df <- .be_discover_ts_map(x, stations, parameter)
  df_valid <- dplyr::filter(map_df, !is.na(.data$ts_id), nzchar(.data$ts_id))
  if (!nrow(df_valid)) return(.be_empty_ts(x, parameter, pm$unit))

  # lookups
  st_by_ts <- df_valid$station_id; names(st_by_ts) <- df_valid$ts_id
  ts_ids   <- unname(df_valid$ts_id)

  # station metadata (already includes vertical_datum via ca_sta)
  st_meta <- stations.hydro_service_BE_HIC(x)
  vd_map  <- st_meta$vertical_datum; names(vd_map) <- st_meta$station_id

  # ---- single-values fetcher (datasource=4 + retry) -------------------------
  fetch_vals <- function(tsid) {
    st_id <- st_by_ts[[tsid]] %||% NA_character_

    q <- list(
      service      = "kisters",
      type         = "queryServices",
      request      = "getTimeseriesValues",
      format       = "json",
      datasource   = 4,
      ts_id        = tsid,
      from         = format(start_utc, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      to           = format(end_utc,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      returnfields = "Timestamp,Value,Quality Code,Quality Code Name,Quality Code Description"
    )


    req  <- build_request(x, path = "", query = q)
    resp <- .be_perform_with_retry(req)
    js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    if (is.null(js$data) || length(js$data) == 0) return(.be_empty_ts(x, parameter, pm$unit))

    mat <- if (is.list(js$data) && length(js$data) == 1 && is.matrix(js$data[[1]])) js$data[[1]]
    else if (is.matrix(js$data)) js$data else as.matrix(js$data)
    if (is.null(dim(mat)) || nrow(mat) == 0) return(.be_empty_ts(x, parameter, pm$unit))

    df <- as.data.frame(mat, stringsAsFactors = FALSE, optional = TRUE)
    cols <- js$columns; if (is.character(cols) && length(cols) == 1L) cols <- strsplit(cols, "\\s*,\\s*")[[1]]
    names(df) <- if (is.character(cols) && length(cols) == ncol(df)) cols else paste0("V", seq_len(ncol(df)))
    df <- tibble::as_tibble(df, .name_repair = "minimal")

    # pick fields
    ts_raw <- col_or_null(df, "Timestamp") %||% col_or_null(df, "timestamp") %||% df[[1]]
    val    <- col_or_null(df, "Value")     %||% col_or_null(df, "value")     %||% df[[2]]
    qc     <- col_or_null(df, "Quality Code")
    qn     <- col_or_null(df, "Quality Code Name")
    qd     <- col_or_null(df, "Quality Code Description")

    # normalize timestamps (UTC)
    ts_chr <- gsub("T", " ", as.character(ts_raw), fixed = TRUE)
    ts_chr <- sub("\\..*$", "", ts_chr)
    ts_chr <- sub(" [+-]\\d{2}:?\\d{2}$", "", ts_chr)
    ts_posix <- suppressWarnings(as.POSIXct(ts_chr, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

    val_num <- suppressWarnings(as.numeric(val))
    qc_chr  <- if (!is.null(qc)) as.character(qc) else rep(NA_character_, length(ts_posix))
    qn_chr  <- if (!is.null(qn)) as.character(qn) else rep(NA_character_, length(ts_posix))
    qd_chr  <- if (!is.null(qd)) as.character(qd) else rep(NA_character_, length(ts_posix))

    # filter by rng + optional quality exclusion
    keep <- !is.na(ts_posix) & ts_posix >= start_utc & ts_posix <= end_utc
    if (!is.null(exclude_quality)) keep <- keep & !(qc_chr %in% exclude_quality)

    if (!any(keep)) return(.be_empty_ts(x, parameter, pm$unit))

    tibble::tibble(
      country        = x$country,
      provider_id    = x$provider_id,
      provider_name  = x$provider_name,
      station_id     = st_id,
      parameter      = parameter,
      timestamp      = ts_posix[keep],
      value          = val_num[keep],
      unit           = pm$unit,
      vertical_datum = dplyr::if_else(parameter == "water_level", vd_map[[st_id]], NA_character_),
      quality_code   = qc_chr[keep],
      quality_name   = qn_chr[keep],
      quality_desc   = qd_chr[keep],
      source_url     = paste0(
        x$base_url,
        "?service=kisters&type=queryServices&request=getTimeseriesValues",
        "&ts_id=", utils::URLencode(tsid),
        "&format=json&datasource=4",
        "&returnfields=", utils::URLencode("Timestamp,Value,Quality Code,Quality Code Name,Quality Code Description"),
        "&from=", utils::URLencode(q$from),
        "&to=",   utils::URLencode(q$to)
      )
    )
  }

  # ---- batching + rate: 1 call every ~5s ------------------------------------
  idx     <- seq_along(ts_ids)
  pb      <- progress::progress_bar$new(
    format = "values    [:bar] :current/:total (:percent) eta: :eta",
    total  = length(idx)
  )
  rl_vals <- ratelimitr::limit_rate(
    function(i) {
      on.exit(Sys.sleep(5 + stats::runif(1, 0, 0.5)), add = TRUE)  # 5.0-5.5s
      fetch_vals(ts_ids[[i]])
    },
    rate = ratelimitr::rate(n = 1, period = 5)
  )

  res <- lapply(idx, function(i) { pb$tick(); rl_vals(i) }) |> dplyr::bind_rows()

  if (!nrow(res)) return(.be_empty_ts(x, parameter, pm$unit))
  dplyr::arrange(res, .data$station_id, .data$timestamp)
}
