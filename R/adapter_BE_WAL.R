# ==== Belgium (Wallonia, SPW Hydrométrie — KiWIS) adapter =====================
# Provider: BE_WAL
# Base URL: https://hydrometrie.wallonie.be/services/KiWIS/KiWIS
# Scope: water_discharge (daily), water_level (daily)
# Groups: Q = 7256919, H = 7255151
# Notes:
# - datasource=4 (tabular JSON: columns + data matrix)
# - vertical_datum: "NA" for water_level (no clear datum exposed)
# - polite rate limit: ~1 req / 5 s, with retry/backoff for 429/5xx
# - batching + progress for value fetches
# - helpers expected from core: build_request(), perform_request(), col_or_null(),
#   normalize_utf8(), resolve_dates(), `%||%`

# ---- registration ------------------------------------------------------------

#' @export
register_BE_WAL <- function() {
  register_service(
    provider_id   = "BE_WAL",
    provider_name = "Belgium – SPW Hydrométrie (Wallonia, KiWIS)",
    country       = "BE",
    base_url      = "https://hydrometrie.wallonie.be/services/KiWIS/KiWIS",
    rate_cfg      = list(n = 1, period = 5),  # polite: 1 req / 5 s
    auth          = list(type = "none")
  )
}

#' @export
timeseries_parameters.hydro_service_BE_WAL <- function(x, ...) {
  c("water_discharge","water_level")
}

# ---- parameter mapping -------------------------------------------------------

.wal_param_map <- function(parameter) {
  switch(
    parameter,
    water_discharge = list(par = "Q", unit = "m^3/s", group_id = "7256919"),
    water_level     = list(par = "H", unit = "m",     group_id = "7255151"),
    rlang::abort("BE_WAL supports only 'water_discharge' and 'water_level'.")
  )
}


.wal_parse_header_matrix0 <- function(js) {
  # js is a 2D array where the first row contains column names
  if (is.null(dim(js)) || nrow(js) < 2) return(tibble::tibble())
  hdr <- as.character(js[1, , drop = TRUE])
  dat <- js[-1, , drop = FALSE]
  df  <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  names(df) <- hdr
  tibble::as_tibble(df, .name_repair = "minimal")
}

#' @export
stations.hydro_service_BE_WAL <- function(x, ...) {
  # Ask KIWIS for stations via datasource=0; include ca_sta & ca_sta_returnfields
  q <- list(
    service              = "kisters",
    type                 = "queryServices",
    request              = "getStationList",
    datasource           = 0,
    format               = "json",
    returnfields           = "station_no,station_name,station_no,station_latitude,station_longitude,site_name,ca_sta",
    # leave empty to return all available station custom attributes
    ca_sta_returnfields    = "")

  req  <- build_request(x, path = "", query = q)
  resp <- perform_request(req)

  # datasource=0 returns a header-first 2D array/matrix
  js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  df <- .wal_parse_header_matrix0(js)

  # Extract fields (be tolerant to slight naming diffs)
  st_id_raw <- col_or_null(df, "station_no") %||% col_or_null(df, "station_id")
  st_name   <- normalize_utf8(col_or_null(df, "station_name"))
  riv       <- col_or_null(df, "river_name")
  lat_chr   <- col_or_null(df, "station_latitude")
  lon_chr   <- col_or_null(df, "station_longitude")
  area_km2  <- .be_parse_km2_numeric(col_or_null(df, "CATCHMENT_SIZE"))
  alt_chr   <- col_or_null(df, "station_gauge_datum")
  vdatum_raw <- col_or_null(df, "station_gauge_datum_unit")

  # Empty strings -> NA, then numeric
  to_num <- function(z) suppressWarnings(as.numeric(ifelse(is.na(z) | z == "", NA, z)))
  lat <- to_num(lat_chr)
  lon <- to_num(lon_chr)
  alt <- to_num(alt_chr)

  out <- tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = as.character(st_id_raw),
    station_name  = st_name,
    station_name_ascii  = to_ascii(st_name),
    river         = riv,
    river_ascii   = to_ascii(riv),
    lat           = lat,  # EPSG:4326
    lon           = lon,  # EPSG:4326
    area          = area_km2,
    altitude      = alt,
    vertical_datum = vdatum_raw
  )

  # Throw out stations without coordinates
  dplyr::filter(out, !is.na(.data$lat), !is.na(.data$lon), !is.na(.data$river), nzchar(.data$river))
}


# ---- retry + discovery helpers ----------------------------------------------

.wal_perform_with_retry <- function(req) {
  httr2::req_retry(
    req,
    max_tries    = 6,
    backoff      = ~ min(120, 2^.x) + stats::runif(1, 0, 0.5),
    is_transient = function(resp) {
      s <- httr2::resp_status(resp)
      s == 429L || (s >= 500L && s < 600L)
    }
  ) |>
    httr2::req_perform()
}

.wal_discover_ts_map <- function(x, stations, parameter) {
  pm <- .wal_param_map(parameter)
  q <- list(service="kisters", type="queryServices", request="getTimeseriesList",
            timeseriesgroup_id=pm$group_id, datasource=0, format="json",
            returnfields="station_no,ts_id")
  req  <- build_request(x, path = "", query = q)
  resp <- perform_request(req)
  js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  hdr <- as.character(js[1, , drop = TRUE]); dat <- js[-1, , drop = FALSE]
  if (NROW(dat) == 0) return(tibble::tibble(station_id=character(), ts_id=character()))
  df <- tibble::as_tibble(`colnames<-`(as.data.frame(dat, stringsAsFactors=FALSE, check.names=FALSE), hdr))

  out <- tibble::tibble(
    station_id = as.character(col_or_null(df, "station_no")),
    ts_id      = as.character(col_or_null(df, "ts_id"))
  )
  out <- out[!is.na(out$station_id) & !is.na(out$ts_id) & nzchar(out$ts_id), , drop=FALSE]

  if (!is.null(stations) && length(stations)) {
    out <- dplyr::filter(out, .data$station_id %in% unique(as.character(stations)))
  }
  dplyr::distinct(out, .data$station_id, .data$ts_id, .keep_all = TRUE)
}

.wal_empty_ts <- function(x, parameter, unit) {
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
    quality_description = character(0),
    source_url     = character(0)
  )
}

# ---- timeseries() ------------------------------------------------------------

#' @export
timeseries.hydro_service_BE_WAL <- function(x,
                                            parameter = c("water_discharge","water_level"),
                                            stations = NULL,
                                            start_date = NULL, end_date = NULL,
                                            mode = c("complete","range"),
                                            exclude_quality = NULL,
                                            ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)
  pm        <- .wal_param_map(parameter)

  # 1) pull stations (for vd_map and default station universe)
  st_all <- stations.hydro_service_BE_WAL(x)

  # if user passed stations, restrict to them (and keep only those we know)
  if (is.null(stations) || !length(stations)) {
    st <- st_all
  } else {
    stations <- unique(as.character(stations))
    st <- dplyr::filter(st_all, .data$station_id %in% stations)
  }

  # safety: if nothing left, return empty
  if (!nrow(st)) return(.wal_empty_ts(x, parameter, pm$unit))

  # per-station vertical datum lookup (named vector)
  vd_map <- rlang::set_names(st$vertical_datum, st$station_id)

  # 2) discover ts_ids for chosen parameter
  map_df   <- .wal_discover_ts_map(x, unique(st$station_id), parameter)
  df_valid <- dplyr::filter(map_df, !is.na(.data$ts_id), nzchar(.data$ts_id))
  if (!nrow(df_valid)) return(.wal_empty_ts(x, parameter, pm$unit))

  st_by_ts <- df_valid$station_id; names(st_by_ts) <- df_valid$ts_id
  ts_ids   <- unname(df_valid$ts_id)

  # 3) worker to fetch a single ts_id
  fetch_vals <- function(tsid) {
    st_id <- st_by_ts[[tsid]] %||% NA_character_
    q <- list(
      service      = "kisters",
      type         = "queryServices",
      request      = "getTimeseriesValues",
      datasource   = 0,
      format       = "json",
      ts_id        = tsid,
      from         = format(rng$start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      to           = format(rng$end,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      returnfields = "Timestamp,Value,Quality Code,Quality Code Name,Quality Code Description"
    )
    req  <- build_request(x, path = "", query = q)
    resp <- .wal_perform_with_retry(req)
    resp <- .wal_perform_with_retry(req)
    js   <- httr2::resp_body_json(resp, simplifyVector = TRUE)

    # ---- Parse values robustly (datasource=0 or columns+data) ----------------
    parse_values <- function(js) {
      # Case A: KiWIS columns+data
      if (!is.null(js$columns) && !is.null(js$data)) {
        mat <- if (is.list(js$data) && length(js$data) == 1 && is.matrix(js$data[[1]])) js$data[[1]]
        else if (is.matrix(js$data)) js$data else as.matrix(js$data)
        if (is.null(dim(mat)) || nrow(mat) == 0) return(tibble::tibble())
        df <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
        cols <- js$columns
        if (is.character(cols) && length(cols) == 1L) cols <- strsplit(cols, "\\s*,\\s*")[[1]]
        names(df) <- if (is.character(cols) && length(cols) == ncol(df)) cols else paste0("V", seq_len(ncol(df)))
        return(tibble::as_tibble(df, .name_repair = "minimal"))
      }

      # Case B: datasource=0 — header-first 2D array/matrix
      if (!is.null(dim(js)) && nrow(js) >= 2) {
        hdr <- as.character(js[1, , drop = TRUE])
        dat <- js[-1, , drop = FALSE]
        df  <- as.data.frame(dat, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
        names(df) <- hdr
        return(tibble::as_tibble(df, .name_repair = "minimal"))
      }

      tibble::tibble()
    }

    df <- parse_values(js)
    if (!nrow(df)) return(.wal_empty_ts(x, parameter, pm$unit))

    # Column name helpers (case/space tolerant)
    pick_col <- function(df, candidates) {
      # candidates: vector of preferred names (ordered)
      nms <- names(df)
      for (cand in candidates) {
        hit <- which(tolower(trimws(nms)) == tolower(trimws(cand)))
        if (length(hit)) return(nms[hit[1]])
      }
      NULL
    }

    # 1) Timestamp
    ts_col <- pick_col(df, c("Timestamp","Time","Date","Datetime","Date Time"))
    if (is.null(ts_col)) {
      # If no timestamp column, nothing useful to return
      return(.wal_empty_ts(x, parameter, pm$unit))
    }
    ts_raw <- df[[ts_col]]

    # 2) Value — prefer explicit "Value", else first non-timestamp numeric-ish column
    val_col <- pick_col(df, c("Value","value"))
    if (is.null(val_col)) {
      # find first column that is not the timestamp and looks numeric
      other_cols <- setdiff(names(df), ts_col)
      # try to coerce each candidate and pick the first with at least one numeric
      val_col <- NULL
      for (cname in other_cols) {
        vv <- suppressWarnings(as.numeric(df[[cname]]))
        if (any(!is.na(vv))) { val_col <- cname; break }
      }
    }

    # If still no value column or only 1 column total, bail safely
    if (is.null(val_col) || ncol(df) < 2) return(.wal_empty_ts(x, parameter, pm$unit))

    # 3) Quality fields (optional)
    qc_col <- pick_col(df, c("Quality Code","quality","Quality"))
    qn_col <- pick_col(df, c("Quality Code Name","quality_name"))
    qd_col <- pick_col(df, c("Quality Code Description","quality_description"))

    # normalize + filter window
    ts_chr   <- gsub("T"," ", as.character(ts_raw), fixed = TRUE)
    ts_chr   <- sub("\\..*$", "", ts_chr)
    ts_chr   <- sub(" [+-]\\d{2}:?\\d{2}$", "", ts_chr)
    ts_posix <- suppressWarnings(as.POSIXct(ts_chr, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

    val_num <- suppressWarnings(as.numeric(df[[val_col]]))
    qc_chr  <- if (!is.null(qc_col)) as.character(df[[qc_col]]) else rep(NA_character_, length(ts_posix))
    qn_chr  <- if (!is.null(qn_col)) as.character(df[[qn_col]]) else rep(NA_character_, length(ts_posix))
    qd_chr  <- if (!is.null(qd_col)) as.character(df[[qd_col]]) else rep(NA_character_, length(ts_posix))

    keep <- !is.na(ts_posix) & ts_posix >= rng$start & ts_posix <= (rng$end + 86399)
    if (!is.null(exclude_quality)) keep <- keep & !(qc_chr %in% exclude_quality)
    if (!any(keep)) return(.wal_empty_ts(x, parameter, pm$unit))

    # NEW: station-specific vertical datum mapping
    vdatum <- dplyr::if_else(parameter == "water_level", vd_map[[st_id]], NA_character_)

    tibble::tibble(
      country        = x$country,
      provider_id    = x$provider_id,
      provider_name  = x$provider_name,
      station_id     = st_id,
      parameter      = parameter,
      timestamp      = ts_posix[keep],
      value          = val_num[keep],
      unit           = pm$unit,
      vertical_datum = vdatum,
      quality_code   = qc_chr[keep],
      quality_name   = qn_chr[keep],
      quality_description = qd_chr[keep],
      source_url     = paste0(
        x$base_url,
        "?service=kisters&type=queryServices&request=getTimeseriesValues",
        "&ts_id=", utils::URLencode(tsid),
        "&format=json&datasource=0",
        "&returnfields=", utils::URLencode("Timestamp,Value,Quality Code,Quality Code Name,Quality Code Description"),
        "&from=", utils::URLencode(q$from),
        "&to=",   utils::URLencode(q$to)
      )
    )
  }

  # batching with progress + polite rate limiting
  idx <- seq_along(ts_ids)
  pb  <- progress::progress_bar$new(format="values    [:bar] :current/:total (:percent) eta: :eta",
                                    total=length(idx))
  rl_vals  <- ratelimitr::limit_rate(
    function(i){ on.exit(Sys.sleep(5 + stats::runif(1,0,0.5)), add=TRUE); fetch_vals(ts_ids[[i]]) },
    rate = ratelimitr::rate(n=1, period=5)
  )

  res <- lapply(idx, function(i){ pb$tick(); rl_vals(i) }) |> dplyr::bind_rows()
  if (!nrow(res)) return(.wal_empty_ts(x, parameter, pm$unit))
  dplyr::arrange(res, .data$station_id, .data$timestamp)
}
