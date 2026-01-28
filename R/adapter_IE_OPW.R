# R/adapter_IE_OPW.R
# Office of Public Works - waterlevel.ie

# -- Registration --------------------------------------------------------------

#' @keywords internal
#' @noRd
register_IE_OPW <- function() {
  register_service_usage(
    provider_id   = "IE_OPW",
    provider_name = "Office of Public Works",
    country       = "Ireland",
    base_url      = "https://waterlevel.ie",          # Hydro-Data JSON
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

# Keep this alongside registration (same style as your LT function)
#' @export
timeseries_parameters.hydro_service_IE_OPW <- function(x, ...) {
  c("water_discharge", "water_level", "water_temperature")
}

# -- Parameter mapping ---------------------------------------------------------

.ie_param_map <- function(parameter) {
  switch(tolower(parameter),
         water_level        = list(code = "S",      unit = "m"),
         water_discharge    = list(code = "Q",      unit = "m^3/s"),
         water_temperature  = list(code = "TWater", unit = "\u00B0C"),
         rlang::abort("IE_OPW supports 'water_level', 'water_discharge', or 'water_temperature'.")
  )
}

.ie_is_republishable <- function(id_int) {
  is.finite(id_int) && id_int >= 1L && id_int <= 41000L
}

.ie_pad5 <- function(id_chr) {
  id_num <- suppressWarnings(as.integer(id_chr))
  ifelse(is.na(id_num), id_chr, sprintf("%05d", id_num))
}

# Build Hydro-Data archive path (used with build_request(x, path = ...))
.ie_archive_path <- function(station_id_padded, code, period = c("year.json")) {
  period <- match.arg(period)
  sprintf("/hydro-data/data/internet/stations/0/%s/%s/%s", as.character(station_id_padded), code, period)
}

# Flexible payload parser: {values:[{t,v,q}]} or list of {t,v,q} or [t,v,q]
.ie_parse_payload <- function(txt) {
  empty <- list(
    data = data.frame(
      timestamp = as.POSIXct(character(), tz = "UTC"),
      value  = numeric(),
      quality_code = integer(),
      stringsAsFactors = FALSE
    ),
    unit = NULL
  )
  if (is.null(txt) || !nzchar(txt)) return(empty)

  j <- try(jsonlite::fromJSON(txt, simplifyVector = FALSE), silent = TRUE)
  if (inherits(j, "try-error")) return(empty)

  # helper: coerce any input to UNIX seconds (numeric), then caller casts once
  to_unix_seconds <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_real_)
    xn <- suppressWarnings(as.numeric(x))
    if (is.finite(xn)) {
      if (xn >= 1e11) xn/1000 else xn  # treat very large as milliseconds
    } else {
      as.numeric(suppressWarnings(as.POSIXct(x, tz = "UTC")))
    }
  }

  # ---- Case A: OPW Hydro-Data array with columns+data ----
  if (is.list(j) && length(j) >= 1 && is.list(j[[1]]) && !is.null(j[[1]]$data)) {
    obj  <- j[[1]]
    dat  <- obj$data %||% list()
    unit <- obj$ts_unitsymbol %||% obj$unit %||% NULL
    if (!length(dat)) return(empty)

    ts_num <- vapply(dat, function(row) to_unix_seconds(row[[1]] %||% NA), numeric(1))
    v      <- vapply(dat, function(row) suppressWarnings(as.numeric(row[[2]] %||% NA)), numeric(1))
    q      <- vapply(dat, function(row) suppressWarnings(as.integer(row[[3]] %||% NA)), integer(1))

    df <- data.frame(
      timestamp = as.POSIXct(ts_num, origin = "1970-01-01", tz = "UTC"),
      value = v, quality_code = q,
      stringsAsFactors = FALSE
    )
    return(list(data = df, unit = unit))
  }

  # ---- Case B: { values: [ {t,v,q} ] } or similar generic shapes ----
  values <- if (is.list(j) && !is.null(j$values)) j$values else j
  unit   <- if (is.list(j)) j$unit %||% NULL else NULL
  if (is.null(values) || length(values) == 0) return(empty)

  norm <- lapply(values, function(x) {
    if (is.list(x)) {
      list(
        t = x$t %||% x$time %||% x$timestamp %||% x[[1]] %||% NA,
        v = suppressWarnings(as.numeric(x$v %||% x$value %||% x[[2]] %||% NA)),
        q = suppressWarnings(as.integer(x$q %||% x$quality %||% x[[3]] %||% NA))
      )
    } else if (is.atomic(x) && length(x) >= 2) {
      list(
        t = x[[1]],
        v = suppressWarnings(as.numeric(x[[2]])),
        q = suppressWarnings(as.integer(x[[3]] %||% NA))
      )
    } else list(t = NA, v = NA_real_, q = NA_integer_)
  })

  ts_num <- vapply(norm, function(z) to_unix_seconds(z$t), numeric(1))
  v      <- vapply(norm, function(z) z$v, numeric(1))
  q      <- vapply(norm, function(z) z$q, integer(1))

  df <- data.frame(
    timestamp = as.POSIXct(ts_num, origin = "1970-01-01", tz = "UTC"),
    value = v, quality_code = q,
    stringsAsFactors = FALSE
  )
  list(data = df, unit = unit)
}


# -- Stations (S3 method) -----------------------------------------------------

#' @export
stations.hydro_service_IE_OPW <- function(x, ...) {
  # stations.json (full metadata)
  path <- "/hydro-data/data/internet/stations/stations.json"
  req  <- build_request(x, path = path)
  resp <- perform_request(req)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    rlang::abort(paste0("IE_OPW stations(): failed to fetch stations.json (status ", status, ")"))
  }

  js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  if (is.null(js) || !length(js)) {
    return(tibble::tibble(
      country="IE", provider_id="IE_OPW", provider_name="Office of Public Works",
      station_id=character(), org_id=character(), station_name=character(),
      river=character(), lat=numeric(), lon=numeric(),
      area=as.numeric(NULL), altitude=as.numeric(NULL)
    ))
  }
  df <- tibble::as_tibble(js)

  # Helpers
  get_chr <- function(col, ...) {
    col_or_null(df, col, ...) %||% NA_character_
  }
  get_num <- function(col, ...) {
    suppressWarnings(as.numeric(col_or_null(df, col, ...)))
  }

  # Source fields
  st_id_chr   <- get_chr("station_no")
  st_name     <- normalize_utf8(get_chr("station_name"))
  lat         <- get_num("station_latitude")
  lon         <- get_num("station_longitude")

  # River name: prefer WTO_OBJECT (waterbody), fallback catchment_name
  river0      <- normalize_utf8(get_chr("WTO_OBJECT"))
  river_fallback <- normalize_utf8(get_chr("catchment_name"))
  river       <- ifelse(!is.na(river0) & nzchar(river0), river0,
                        ifelse(!is.na(river_fallback) & nzchar(river_fallback), river_fallback, NA_character_))

  # Catchment area like "76.82 km" -> numeric km
  area_txt    <- get_chr("CATCHMENT_SIZE")
  area_num    <- suppressWarnings(as.numeric(sub("\\s.*$", "", area_txt)))

  # Altitude (station_gauge_datum if present; may be blank) as numeric
  altitude_txt <- get_chr("station_gauge_datum")
  altitude_num <- suppressWarnings(as.numeric(altitude_txt))
  # If you would rather fall back to GAUGE_DATUM as an "elevation" proxy, uncomment:
  # altitude_num <- ifelse(is.na(altitude_num), suppressWarnings(as.numeric(get_chr("GAUGE_DATUM"))), altitude_num)

  # org_id: zero-padded 5-digit from station_id
  org_id <- ifelse(is.na(st_id_chr), NA_character_, sprintf("%05d", as.integer(st_id_chr)))

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,  # "Office of Public Works"
    station_id    = st_id_chr,        # stripped id from JSON
    org_id        = org_id,           # zero-padded for timeseries paths
    station_name  = st_name,
    river         = river,
    lat           = lat,
    lon           = lon,
    area          = area_num,
    altitude      = altitude_num
  )

  # Republication filter 1..41000
  id_int <- suppressWarnings(as.integer(out$station_id))
  keep <- vapply(id_int, function(z) is.finite(z) && z >= 1L && z <= 41000L, logical(1))
  dropped <- sum(!keep & !is.na(keep))
  if (dropped > 0) {
    rlang::warn(paste0("IE_OPW: excluding ", dropped, " station(s) outside 1..41000 (republication constraint)."))
  }
  out <- out[keep | is.na(keep), , drop = FALSE]

  out
}

# Fetch once; returns a named character vector:
.ie_datum_unit_lookup <- function(x) {
  path <- "/hydro-data/data/internet/stations/stations.json"
  resp <- perform_request(build_request(x, path = path))
  if (httr2::resp_status(resp) >= 400) return(character())

  df <- tibble::as_tibble(httr2::resp_body_json(resp, simplifyVector = TRUE))

  ids   <- as.character(col_or_null(df, "station_no") %||% character())
  units <- normalize_utf8(col_or_null(df, "station_gauge_datum_unit") %||% character())

  # strip any leading zeros just in case, and coerce to plain character keys
  ids_stripped <- sub("^0+", "", ids)
  keep <- nzchar(ids_stripped)
  ids_stripped <- ids_stripped[keep]
  units        <- units[keep]

  # ensure one-to-one (keep first occurrence)
  # (if dupes exist, tapply could be used; keeping first is fine here)
  idx_first <- !duplicated(ids_stripped)
  stats::setNames(as.character(units[idx_first]), ids_stripped[idx_first])
}

.ie_get_datum_unit <- function(datum_lu, station_id_stripped) {
  if (length(datum_lu) == 0 || !nzchar(station_id_stripped)) return(NA_character_)
  v <- unname(datum_lu[as.character(station_id_stripped)])
  if (length(v) == 0 || is.na(v)) NA_character_ else as.character(v)
}

# OPW quality code descriptions (long form)
.ie_quality_desc_Q <- c(
  `31`  = "Flow from rating curve (good quality) with inspected WL. May contain some error; acceptable for general use.",
  `32`  = "As 31, but using water level data of Code 32.",
  `36`  = "Flow from rating curve (fair quality) with inspected/corrected WL. May contain a fair degree of error; use with caution.",
  `46`  = "Flow from rating curve (poor quality) with inspected/corrected WL. May contain significant error; indicative use only.",
  `56`  = "Flow from extrapolated rating curve. Reliability unknown; treat with caution.",
  `96`  = "Flow from provisional rating curve; subject to revision after retrospective assessment.",
  `101` = "Flow estimated using unreliable water level data; suspected erroneous; use with caution.",
  `151` = "Unusable data: dry channel or logger malfunction.",
  `254` = "Flow estimated using unchecked water level data; provisional; use with caution.",
  `255` = "Missing; no data available."
)

.ie_quality_desc_S <- c(
  `31`  = "Inspected water level; may contain some error; approved for general use.",
  `32`  = "As 31, but original WL has been corrected.",
  `41`  = "Poor measured water level.",
  `42`  = "As 41, but original WL has been corrected.",
  `101` = "Unreliable water level; suspected erroneous or artificially affected; use with caution.",
  `151` = "Unusable data: dry channel or logger malfunction.",
  `254` = "Unchecked imported water level; provisional; use with caution.",
  `255` = "Missing; no data available."
)

.ie_quality_desc_TWater <- c(
  `254` = "Unchecked imported water temperature data; provisional; use with caution.",
  `255` = "Missing; no data available."
)


.ie_temp_quality_recommend <- function(qc) {
  q <- suppressWarnings(as.integer(qc))
  out <- ifelse(is.na(q), NA_integer_, 254L)
  out[q == 1L] <- 254L
  out[q == -1L] <- 255L
  out
}

# -- Time series (S3 method) --------------------------------------------------
#' @export
timeseries.hydro_service_IE_OPW <- function(x,
                                            parameter = c("water_level","water_discharge","water_temperature"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .ie_param_map(parameter)

  # Build datum-unit lookup once (only needed for water_level)
  datum_lu <- if (parameter == "water_level") .ie_datum_unit_lookup(x) else character()

  # Station ids (accept stripped or padded from caller)
  if (is.null(stations) || !length(stations)) {
    st <- stations.hydro_service_IE_OPW(x)
    ids_stripped <- st$station_id
  } else {
    ids_stripped <- sub("^0+", "", as.character(stations))
    ids_stripped[ids_stripped == ""] <- "0"
  }
  ids_stripped <- unique(ids_stripped)

  # Republication constraint
  ok <- vapply(suppressWarnings(as.integer(ids_stripped)), .ie_is_republishable, logical(1))
  if (any(!ok, na.rm = TRUE)) {
    rlang::warn(paste0("IE_OPW: skipping ", sum(!ok, na.rm = TRUE), " station id(s) outside 1..41000."))
  }
  ids_stripped <- ids_stripped[ok | is.na(ok)]

  # Early empty return (schema depends on parameter)
  if (!length(ids_stripped)) {
    if (parameter == "water_level") {
      return(tibble::tibble(
        country=character(), provider_id=character(), provider_name=character(),
        station_id=character(), parameter=character(), timestamp=as.POSIXct(character()),
        value=numeric(), unit=character(), value_datum_unit=character(),
        quality_code=integer(), quality_desc=character(), source_url=character()
      ))
    } else if (parameter == "water_temperature") {
      return(tibble::tibble(
        country=character(), provider_id=character(), provider_name=character(),
        station_id=character(), parameter=character(), timestamp=as.POSIXct(character()),
        value=numeric(), unit=character(),
        quality_code=integer(), quality_code_recommended=integer(),
        quality_desc=character(), source_url=character()
      ))
    } else {
      return(tibble::tibble(
        country=character(), provider_id=character(), provider_name=character(),
        station_id=character(), parameter=character(), timestamp=as.POSIXct(character()),
        value=numeric(), unit=character(),
        quality_code=integer(), quality_desc=character(), source_url=character()
      ))
    }
  }

  # One fetch per station (year.json returns full archive); filter client-side
  batches <- chunk_vec(ids_stripped, 50)
  pb <- progress::progress_bar$new(total = length(batches))
  not_found_ids <- new.env(parent = emptyenv())  # collect 404/empty

  fetch_station_all <- function(st_id_stripped) {
    st_id_padded <- .ie_pad5(st_id_stripped)
    code <- pm$code

    p_all  <- .ie_archive_path(st_id_padded, code, "year.json")
    req    <- build_request(x, path = p_all)

    # robust perform: catch transport errors and 404s
    resp <- try(perform_request(req), silent = TRUE)
    if (inherits(resp, "try-error")) {
      not_found_ids[[st_id_stripped]] <- TRUE
      return(tibble::tibble())
    }
    status <- httr2::resp_status(resp)
    if (status == 404 || status >= 400) {
      not_found_ids[[st_id_stripped]] <- TRUE
      return(tibble::tibble())
    }

    payload <- httr2::resp_body_string(resp)
    if (!nzchar(payload)) {
      not_found_ids[[st_id_stripped]] <- TRUE
      return(tibble::tibble())
    }

    parsed <- .ie_parse_payload(payload)
    d <- parsed$data
    if (is.null(d) || !nrow(d)) return(tibble::tibble())

    # Date filter
    if (!is.null(rng$start)) d <- d[d$timestamp >= rng$start, , drop = FALSE]
    if (!is.null(rng$end))   d <- d[d$timestamp <= rng$end,   , drop = FALSE]
    if (!nrow(d)) return(tibble::tibble())

    # Base output (pad station_id to 5 digits in the result)
    out <- tibble::tibble(
      country       = x$country,
      provider_id   = x$provider_id,
      provider_name = x$provider_name,
      station_id    = .ie_pad5(st_id_stripped),
      parameter     = parameter,
      timestamp     = d$timestamp,
      value         = as.numeric(d$value),
      unit          = pm$unit,
      quality_code  = suppressWarnings(as.integer(d$quality_code)),
      source_url    = paste0(x$base_url, p_all)
    )

    # Add value_datum_unit ONLY for water_level (right after 'unit')
    if (parameter == "water_level") {
      du <- .ie_get_datum_unit(datum_lu, st_id_stripped)
      out$value_datum_unit <- rep(du, nrow(out))
      out <- out[, c("country","provider_id","provider_name","station_id","parameter",
                     "timestamp","value","unit","value_datum_unit","quality_code","source_url")]
    }

    # Temperature-only: add recommended quality column (do not change raw)
    if (parameter == "water_temperature") {
      out$quality_code_recommended <- .ie_temp_quality_recommend(out$quality_code)
      # place right after quality_code
      out <- out[, c("country","provider_id","provider_name","station_id","parameter",
                     "timestamp","value","unit",
                     "quality_code","quality_code_recommended","source_url")]
    }

    # Append human-readable quality descriptions (Q/S/TWater)
    # Append human-readable quality descriptions (Q/S/TWater)
    qmap <- switch(pm$code,
                   "Q"      = .ie_quality_desc_Q,
                   "S"      = .ie_quality_desc_S,
                   "TWater" = .ie_quality_desc_TWater,
                   NULL)

    if (!is.null(qmap)) {
      if (pm$code == "TWater") {
        # map description using the recommended temp codes
        qc_rec <- if ("quality_code_recommended" %in% names(out)) {
          out$quality_code_recommended
        } else {
          .ie_temp_quality_recommend(out$quality_code)
        }
        out$quality_desc <- unname(qmap[as.character(qc_rec)])
      } else {
        # map description using raw codes for Q/S
        out$quality_desc <- unname(qmap[as.character(out$quality_code)])
      }

      # place quality_desc after quality_code (+ after datum/recommended if present)
      cols <- names(out)
      if ("value_datum_unit" %in% cols && "quality_code_recommended" %in% cols) {
        out <- out[, c("country","provider_id","provider_name","station_id","parameter",
                       "timestamp","value","unit","value_datum_unit",
                       "quality_code","quality_code_recommended","quality_desc","source_url")]
      } else if ("value_datum_unit" %in% cols) {
        out <- out[, c("country","provider_id","provider_name","station_id","parameter",
                       "timestamp","value","unit","value_datum_unit",
                       "quality_code","quality_desc","source_url")]
      } else if ("quality_code_recommended" %in% cols) {
        out <- out[, c("country","provider_id","provider_name","station_id","parameter",
                       "timestamp","value","unit",
                       "quality_code","quality_code_recommended","quality_desc","source_url")]
      } else {
        out <- out[, c("country","provider_id","provider_name","station_id","parameter",
                       "timestamp","value","unit","quality_code","quality_desc","source_url")]
      }
    }
    out
  }

  res <- lapply(batches, function(b) {
    pb$tick()
    rl <- ratelimitr::limit_rate(fetch_station_all,
                                 rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))
    dplyr::bind_rows(lapply(b, rl))
  })

  out <- dplyr::bind_rows(res)

  # Single consolidated warning for any 404/empty stations
  nf <- ls(not_found_ids)
  if (length(nf)) {
    rlang::warn(paste0(
      "IE_OPW: ", length(nf), " station(s) returned 404/empty for parameter '", parameter,
      "'. Likely no series or discontinued. Skipped: ",
      paste(utils::head(nf, 10), collapse = ", "),
      if (length(nf) > 10) paste0(" ... +", length(nf) - 10, " more") else ""
    ))
  }

  out
}
