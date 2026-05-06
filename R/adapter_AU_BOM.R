# ---- Registration ------------------------------------------------------------

#' @keywords internal
#' @noRd
register_AU_BOM <- function() {
  register_service_usage(
    provider_id   = "AU_BOM",
    provider_name = "Bureau of Meteorology",
    country       = "Australia",
    base_url      = "https://www.bom.gov.au/waterdata/services",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

# Expose supported canonical parameters
#' @export
timeseries_parameters.hydro_service_AU_BOM <- function(x, ...) {
  c("water_discharge", "water_level", "water_temperature")
}

# ---- Local helpers -----------------------------------------------------------

# Canonical -> KiWIS parameter mapping (par ~ parametertype/short code)
.au_param_map <- function(parameter) {
  switch(parameter,
         water_discharge   = list(par = "Q",  unit = "m^3/s"),
         water_level       = list(par = "H",  unit = "m"),
         water_temperature = list(par = "TW", unit = "\u00B0C"),
         rlang::abort("AU_BOM supports 'water_discharge', 'water_level', 'water_temperature'.")
  )
}

# best-effort parse of KiWIS timestamps (ISO8601 or epoch milliseconds)
.au_parse_time <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA))
  suppressWarnings({
    # numeric epoch milliseconds?
    if (is.numeric(x)) {
      # some APIs deliver seconds, some ms; detect scale
      if (max(x, na.rm = TRUE) > 1e11) {
        as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC")
      } else {
        as.POSIXct(x, origin = "1970-01-01", tz = "UTC")
      }
    } else {
      # character ISO8601 (with or without timezone)
      # try ymd_hms first, then ymd, etc.
      ts <- tryCatch(lubridate::ymd_hms(x, tz = "UTC"), error = function(e) NA)
      if (all(is.na(ts))) {
        ts <- tryCatch(lubridate::ymd_hm(x, tz = "UTC"), error = function(e) NA)
      }
      if (all(is.na(ts))) {
        ts <- tryCatch(lubridate::ymd(x, tz = "UTC"), error = function(e) NA)
      }
      ts
    }
  })
}

# ---- Small utilities ---------------------------------------------------------

.au_unquote <- function(x) {
  # remove a single pair of leading/trailing quotes if present
  ifelse(grepl('^".*"$', x), substring(x, 2, nchar(x) - 1), x)
}

# If KiWIS returns a 2D array with the first row as headers, promote them.
.au_tableize <- function(js) {
  df <- tibble::as_tibble(js)
  # If first row looks like headers (all non-empty, no spaces for typical keys),
  # promote them. Works for your head(df) example.
  hdr <- as.character(unlist(df[1, , drop = TRUE]))
  if (all(nzchar(hdr))) {
    names(df) <- hdr
    df <- dplyr::slice(df, -1)
  }
  df
}

# ---- Stations (filtered + safe altitude) ------------------------------------

#' @export
stations.hydro_service_AU_BOM <- function(x, ...) {
  q <- list(
    service = "KISTERS",
    type    = "queryServices",
    request = "getStationList",
    format  = "json"
  )
  req  <- build_request(x, path = "", query = q)
  resp <- perform_request(req)
  js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  df <- .au_tableize(js)

  st_id   <- col_or_null(df, "station_no")   %||% col_or_null(df, "station_id")
  st_name <- normalize_utf8(col_or_null(df, "station_name") %||% col_or_null(df, "name"))
  st_name <- to_ascii(.au_unquote(st_name))

  # lat/lon may be "", make them numeric NAs cleanly
  lat_chr <- col_or_null(df, "station_latitude")  %||% col_or_null(df, "latitude")
  lon_chr <- col_or_null(df, "station_longitude") %||% col_or_null(df, "longitude")
  lat_num <- suppressWarnings(as.numeric(lat_chr))
  lon_num <- suppressWarnings(as.numeric(lon_chr))

  # altitude: ensure length == nrow(df) even when the column is missing
  alt_src <- col_or_null(df, "station_elevation") %||%
    col_or_null(df, "elevation") %||%
    col_or_null(df, "altitude")
  alt_num <- if (is.null(alt_src)) rep(NA_real_, nrow(df)) else suppressWarnings(as.numeric(alt_src))

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = as.character(st_id),
    station_name  = st_name,
    river         = NA_character_,
    lat           = lat_num,
    lon           = lon_num,
    area          = NA_real_,
    altitude      = alt_num
  )

  # keep only rows with valid coordinates (EPSG:4326)
  out |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lon)) |>
    dplyr::distinct()
}

# ---- Preferred BOM daily series name by parameter ---------------------------
.au_preferred_ts_name <- function(parameter) {
  if (parameter %in% c("water_level", "water_discharge")) {
    "DMQaQc.Merged.DailyMean.09HR"
    # "DMQaQc.Merged.DailyMean.24HR"
  } else {
    "DMQaQc.Merged.DailyMean.24HR"
  }
}


# ---- Timeseries discovery (select exact ts_name) ----------------------------
.bom_discover_ts <- function(x, station_ids, pm, parameter) {
  preferred_name <- .au_preferred_ts_name(parameter)

  fetch_one <- function(st_id) {
    q <- list(
      service    = "KISTERS",
      type       = "queryServices",
      request    = "getTimeseriesList",
      format     = "json",
      station_no = st_id
    )
    req  <- build_request(x, path = "", query = q)
    resp <- perform_request(req)
    js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    rows <- .au_tableize(js)
    if (!nrow(rows)) {
      return(tibble::tibble(
        station_id = st_id,
        ts_id      = NA_character_,
        ts_path    = NA_character_
      ))
    }

    # normalise common id/path/name fields
    rows <- dplyr::mutate(
      rows,
      ts_id   = as.character(col_or_null(rows, "ts_id")   %||% col_or_null(rows, "tsid")),
      ts_path = as.character(col_or_null(rows, "ts_path") %||% col_or_null(rows, "path") %||% NA_character_),
      ts_name = as.character(col_or_null(rows, "ts_name") %||% col_or_null(rows, "name") %||% NA_character_)
    )

    # exact selection by agreed ts_name
    hit <- rows[!is.na(rows$ts_name) & rows$ts_name == preferred_name, , drop = FALSE]
    if (!nrow(hit)) {
      return(tibble::tibble(
        station_id = st_id,
        ts_id      = NA_character_,
        ts_path    = NA_character_
      ))
    }

    tibble::tibble(
      station_id = st_id,
      ts_id      = hit$ts_id[[1]]   %||% NA_character_,
      ts_path    = hit$ts_path[[1]] %||% NA_character_
    )
  }

  # batching + progress
  idx <- seq_along(station_ids)
  batches <- split(idx, ceiling(seq_along(idx) / 50))  # 50 stations per batch
  pb <- progress::progress_bar$new(
    format = "discovery [:bar] :current/:total (:percent) eta: :eta",
    total  = length(idx)
  )

  rl <- ratelimitr::limit_rate(
    fetch_one,
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  out <- lapply(batches, function(batch_idx) {
    rows <- lapply(batch_idx, function(i) {
      pb$tick()
      rl(station_ids[[i]])
    })
    dplyr::bind_rows(rows)
  })

  dplyr::bind_rows(out)
}


# Expand KiWIS getTimeseriesValues JSON (columns + data) into a tibble
.au_values_df <- function(js) {
  # handle either a single object or a list with one object
  if (is.list(js) && !is.null(js$columns) && !is.null(js$data)) {
    items <- list(js)
  } else if (is.list(js) && length(js) && !is.null(js[[1]]$columns)) {
    items <- js
  } else {
    return(tibble::tibble())
  }

  # concatenate across items (rare, but safe)
  out <- lapply(items, function(it) {
    cols <- unlist(strsplit(as.character(it$columns %||% ""), "\\s*,\\s*"))
    if (!length(cols)) return(tibble::tibble())
    # each entry in it$data is a row (often nested list/matrix); flatten to vector
    rows_list <- lapply(it$data %||% list(), function(r) as.character(unlist(r, use.names = FALSE)))
    if (!length(rows_list)) return(tibble::tibble())
    # make all rows the same length as cols (pad with NA if needed)
    rows_list <- lapply(rows_list, function(v) { length(v) <- length(cols); v })
    m <- do.call(rbind, rows_list)
    df <- as.data.frame(m, stringsAsFactors = FALSE, optional = TRUE)
    names(df) <- cols
    tibble::as_tibble(df)
  })

  dplyr::bind_rows(out)
}



# stable empty schema
.au_empty_ts <- function(x, parameter, unit = NA_character_) {
  tibble::tibble(
    country       = character(),
    provider_id   = character(),
    provider_name = character(),
    station_id    = character(),
    parameter     = character(),
    timestamp     = as.POSIXct(character()),
    value         = numeric(),
    unit          = character(),
    quality_code  = character(),
    quality_name  = character(),
    quality_desc  = character(),
    source_url    = character()
  )
}

#' @export
timeseries.hydro_service_AU_BOM <- function(x,
                                            parameter = c("water_discharge","water_level","water_temperature"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            exclude_quality = NULL,
                                            station_cap = 10000,
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .au_param_map(parameter)

  # ---- resolve stations with cap --------------------------------------------
  if (is.null(stations) || !length(stations)) {
    st <- stations.hydro_service_AU_BOM(x)
    ids <- unique(stats::na.omit(as.character(st$station_id)))
    if (!length(ids)) return(.au_empty_ts(x, parameter, pm$unit))
    if (length(ids) > station_cap) {
      rlang::warn(sprintf("AU_BOM: %s stations available; limiting to the first %s for this call.",
                          format(length(ids), big.mark=","), format(station_cap, big.mark=",")))
      rlang::inform("Tip: pass 'stations' explicitly and iterate in batches to cover all stations.")
      ids <- ids[seq_len(station_cap)]
    }
    stations <- ids
  } else {
    stations <- unique(as.character(stations))
    if (length(stations) > station_cap) {
      rlang::abort(sprintf("AU_BOM: Too many stations in one call (%s). Split into batches of up to %s.",
                           format(length(stations), big.mark=","), format(station_cap, big.mark=",")))
    }
  }

  # ---- discovery via your helper (respects preferred ts_name) ---------------
  map_df <- .bom_discover_ts(x, stations, pm, parameter)

  # iterate only over valid ts_id
  df_valid <- dplyr::filter(map_df, !is.na(.data$ts_id), nzchar(.data$ts_id))
  if (!nrow(df_valid)) {
    rlang::warn("AU_BOM: no stations have a valid ts_id for the preferred series.")
    return(.au_empty_ts(x, parameter, pm$unit))
  }

  # lookup: station_id by ts_id
  st_by_ts <- df_valid$station_id
  names(st_by_ts) <- df_valid$ts_id
  ts_ids <- unname(df_valid$ts_id)

  # ---- values worker (by ts_id; read js$data directly) ----------------------
  fetch_vals <- function(tsid) {



    st_id <- st_by_ts[[tsid]] %||% NA_character_


    q <- list(
      service      = "KISTERS",
      type         = "queryServices",
      request      = "getTimeseriesValues",
      format       = "json",
      ts_id        = tsid,
      returnfields = "Timestamp,Value,Quality Code,Quality Code Name,Quality Code Description",
      from         = format(rng$start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      to           = format(rng$end,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )

    req  <- build_request(x, path = "", query = q)
    resp <- perform_request(req)
    js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # js$data[[1]] is an RxC character matrix; js$columns gives header names
    if (is.null(js$data) || !length(js$data)) return(.au_empty_ts(x, parameter, pm$unit))
    mat <- js$data[[1]]
    if (is.null(dim(mat)) || nrow(mat) == 0)   return(.au_empty_ts(x, parameter, pm$unit))

    # Build data.frame then name columns BEFORE converting to tibble
    df <- as.data.frame(mat, stringsAsFactors = FALSE, optional = TRUE)
    cols <- unlist(strsplit(as.character(js$columns %||% ""), "\\s*,\\s*"))
    if (length(cols) == ncol(df)) {
      names(df) <- cols
    } else {
      names(df) <- paste0("V", seq_len(ncol(df)))  # defensive fallback
    }
    df <- tibble::as_tibble(df, .name_repair = "minimal")

    # ---- field picking (exact names from returnfields) -------------------------
    ts_raw  <- df[["Timestamp"]] %||% df[["time"]] %||% df[[1]]
    val     <- df[["Value"]]     %||% df[["value"]] %||% df[[2]]
    qc      <- df[["Quality Code"]] %||% df[["quality_code"]] %||% df[["Quality"]]
    qcname  <- df[["Quality Code Name"]] %||% df[["quality_name"]] %||% df[["qualifier"]]
    qcdesc  <- df[["Quality Code Description"]] %||% df[["quality_description"]]

    # ---- timestamp formatting you wanted --------------------------------------
    # "1990-01-02T09:00:00.000+10:00" -> "1990-01-02 09:00:00"
    ts_chr <- as.character(ts_raw)
    ts_chr <- gsub("T", " ", ts_chr, fixed = TRUE)
    ts_chr <- sub("\\..*$", "", ts_chr)               # drop fractional + timezone
    ts_chr <- sub(" [+-]\\d{2}:?\\d{2}$", "", ts_chr) # safety if no fractional part
    ts_posix <- suppressWarnings(as.POSIXct(ts_chr, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

    val_num <- suppressWarnings(as.numeric(val))
    qc_chr  <- if (!is.null(qc)) as.character(qc) else rep(NA_character_, length(ts_posix))
    qn_chr  <- if (!is.null(qcname)) as.character(qcname) else rep(NA_character_, length(ts_posix))
    qd_chr  <- if (!is.null(qcdesc)) as.character(qcdesc) else rep(NA_character_, length(ts_posix))

    # ---- filtering -------------------------------------------------------------
    keep <- !is.na(ts_posix)
    if (identical(mode, "range")) {
      keep <- keep & ts_posix >= rng$start & ts_posix <= (rng$end + 86399)
    }
    if (!is.null(exclude_quality)) {
      keep <- keep & !(qc_chr %in% exclude_quality)
    }
    if (!any(keep)) return(.au_empty_ts(x, parameter, pm$unit))

    tibble::tibble(
      country        = x$country,
      provider_id    = x$provider_id,
      provider_name  = x$provider_name,
      station_id     = st_id,
      parameter      = parameter,
      timestamp      = ts_posix[keep],
      value          = val_num[keep],
      unit           = pm$unit,
      vertical_datum = dplyr::if_else(parameter == "water_level", "AHD", NA_character_),
      quality_code   = qc_chr[keep],
      quality_name   = qn_chr[keep],
      quality_desc   = qd_chr[keep],
      source_url = paste0(
        x$base_url,
        "?service=KISTERS",
        "&type=queryServices",
        "&request=getTimeseriesValues",
        "&ts_id=", utils::URLencode(tsid),
        "&format=json",
        "&returnfields=", utils::URLencode("Timestamp,Value,Quality Code,Quality Code Name,Quality Code Description"),
        "&from=", utils::URLencode(q$from),
        "&to=",   utils::URLencode(q$to)
      )
    )
  }


  # ---- batching + progress over ts_id ---------------------------------------
  idx  <- seq_along(ts_ids)
  batches <- split(idx, ceiling(seq_along(idx) / 30))  # 30 series per batch
  pb <- progress::progress_bar$new(
    format = "values    [:bar] :current/:total (:percent) eta: :eta",
    total  = length(idx)
  )
  rl_vals <- ratelimitr::limit_rate(
    function(i) fetch_vals(ts_ids[[i]]),
    rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period)
  )

  out <- lapply(batches, function(bi) {
    rows <- lapply(bi, function(i) { pb$tick(); rl_vals(i) })
    dplyr::bind_rows(rows)
  })
  res <- dplyr::bind_rows(out)

  if (!nrow(res)) return(.au_empty_ts(x, parameter, pm$unit))
  dplyr::arrange(res, .data$station_id, .data$timestamp)
}
