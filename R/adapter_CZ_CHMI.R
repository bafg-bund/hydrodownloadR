# R/adapter_CZ_CHMI.R
# Czech Republic - CMHU Hydroportal (OpenData) adapter
# Historical daily data via:
# - Metadata: https://opendata.chmi.cz/hydrology/historical/metadata/meta1.json
# - Daily TS (per station/year): https://opendata.chmi.cz/hydrology/historical/data/daily/H_{id}_DQ_{year}.json
#   (NOTE: file contains tsList for HD, QD, TD; we filter by tsConID)
#
# Dependencies: httr2, jsonlite, dplyr, tidyr, purrr, stringr, lubridate, readr, tibble, cli

# ---- Registration ------------------------------------------------------------

#' @keywords internal
#' @noRd
register_CZ_CHMI <- function() {
  register_service_usage(
    provider_id   = "CZ_CHMI",
    provider_name = "Czech Hydrometeorological Institute",
    country       = "Czech Republic",
    base_url      = "https://opendata.chmi.cz",
    rate_cfg      = list(n = 3, period = 1),
    auth          = list(type = "none")
  )
}

# keep parameter list discoverable
#' @export
timeseries_parameters.hydro_service_CZ_CHMI <- function(x, ...) {
  c("water_discharge","water_level","water_temperature")
}

# ---- Constants (private) -----------------------------------------------------

# .czm_meta_url  <- "https://opendata.chmi.cz/hydrology/historical/metadata/meta1.json"
# Even though the path says DQ, each file contains tsList entries for HD/QD/TD; we filter later.
.czm_daily_tpl <- "https://opendata.chmi.cz/hydrology/historical/data/daily/H_%s_DQ_%d.json"

# ---- Parameter mapping (private) --------------------------------------------

.cz_param_map <- function(parameter) {
  switch(parameter,
         water_level = list(
           unit     = "cm",
           tsConID  = "HD",
           to_canon = function(v) v,          # already CM
           canon    = "cm"
         ),
         water_discharge = list(
           unit     = "m^3/s",
           tsConID  = "QD",
           to_canon = function(v) v,          # already M3_S
           canon    = "m^3/s"
         ),
         water_temperature = list(
           unit     = "\u00B0C",              # display as \u00B0C
           tsConID  = "TD",
           to_canon = function(v) v,          # already 0C
           canon    = "\u00B0C"
         ),
         stop("Unsupported parameter: ", parameter)
  )
}

# ---- Helpers (private) -------------------------------------------------------

.cz_empty_ts <- function(x, parameter, unit) {
  tibble::tibble(
    country       = x$country,
    provider_id   = x$provider_id,
    provider_name = x$provider_name,
    station_id    = character(),
    ts_id         = character(),
    parameter     = rep(parameter, 0),
    timestamp     = as.POSIXct(character(), tz = "UTC"),
    value         = numeric(),
    unit          = rep(unit, 0),
    quality_code  = character(),
    source_url    = character()
  )
}

# Parse the metadata structure:
#   $Daten$Daten$header : "objID,DBC,STATION_NAME,..."
#   $Daten$Daten$Werte  : list of vectors in the same order as header
.cz_meta_parse <- function(meta_json) {
  dat   <- meta_json[["data"]][["data"]]
  hdr   <- dat[["header"]]
  vals  <- dat[["values"]]

  cols  <- strsplit(hdr, "\\s*,\\s*")[[1]]
  # Turn list(list(...), list(...)) into a data.frame with given column names
  df <- tibble::as_tibble(do.call(rbind, vals), .name_repair = "minimal")
  if (ncol(df) != length(cols)) {
    cli::cli_abort("Metadata column mismatch: expected {length(cols)}, got {ncol(df)}.")
  }
  names(df) <- cols
  df
}

.cz_make_daily_url <- function(station_id, year) {
  sprintf(.czm_daily_tpl, station_id, as.integer(year))
}

# Pull and parse a station-year JSON; returns tibble(timestamp,value,unit,tsConID)
.cz_fetch_station_year <- function(station_id, year) {
  url  <- .cz_make_daily_url(station_id, year)
  req  <- httr2::request(url)
  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)
  if (httr2::resp_status(resp) >= 400) return(NULL)

  js <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  ts_list <- js[["tsList"]]
  if (is.null(ts_list) || !length(ts_list)) return(NULL)

  purrr::map(ts_list, function(one) {
    id   <- one[["tsConID"]]
    unit <- one[["unit"]]
    dcol <- one[["tsData"]][["data"]]
    hdr  <- dcol[["header"]] %||% "DT,VAL"
    vals <- dcol[["values"]] %||% list()

    if (!length(vals)) return(NULL)

    # Expect header "DT,VAL"
    tib <- tibble::as_tibble(do.call(rbind, vals), .name_repair = "minimal")
    names(tib) <- strsplit(hdr, "\\s*,\\s*")[[1]]
    # Ensure proper types
    tibble::tibble(
      timestamp = lubridate::ymd_hms(tib$DT, tz = "UTC"),
      value     = readr::parse_number(tib$VAL),
      unit      = unit,
      tsConID   = id
    )
  }) |> purrr::list_c() # combine tibbles
}

# Map CHMI units to canonical label we output
.cz_unit_to_canon <- function(u) {
  switch(toupper(u),
         "CM"   = "cm",
         "M3_S" = "m^3/s",
         "0C"   = "\u00B0C",
         u
  )
}

# ---- Stations ---------------------------------------------------------------

#' @export
stations.hydro_service_CZ_CHMI <- function(x, ...) {
  md_path <- "/hydrology/historical/metadata/meta1.json"
  req  <- build_request(x, path = md_path)
  resp <- perform_request(req)
  dat  <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  data  <- dat[["data"]][["data"]]
  if (is.null(data)) cli::cli_abort("Unexpected CHMI metadata schema: missing $.data.data")

  hdr  <- data$header
  vals <- as.data.frame(data$values)
  if (is.null(hdr) || is.null(vals)) {
    cli::cli_abort("Unexpected CHMI metadata schema: missing 'header' or 'values'.")
  }

  cols <- strsplit(hdr, "\\s*,\\s*")[[1]]

  if (is.data.frame(vals)) {
    row_list <- split(vals, seq_len(nrow(vals)))
    vals_list <- purrr::map(row_list, ~ as.list(.x[1, , drop = TRUE]))
  } else if (is.list(vals)) {
    vals_list <- vals
  } else {
    cli::cli_abort("Unexpected type for $.data.data$values (got {class(vals)}).")
  }

  vals_list <- purrr::map(vals_list, function(row) {
    row <- as.list(row)
    row[lengths(row) == 0] <- NA
    row <- c(row, rep(NA, length(cols) - length(row)))[seq_len(length(cols))]
    stats::setNames(row, cols)
  })

  df <- dplyr::bind_rows(vals_list)

  tibble::tibble(
    country            = x$country,
    provider_id        = x$provider_id,
    provider_name      = x$provider_name,
    station_id         = as.character(df$DBC),
    ts_id              = as.character(df$objID),
    station_name       = as.character(df$STATION_NAME),
    station_name_ascii = to_ascii(df$STATION_NAME),
    river              = as.character(df$STREAM_NAME),
    river_ascii        = to_ascii(as.character(df$STREAM_NAME)),
    lat                = suppressWarnings(as.numeric(df$GEOGR1)),
    lon                = suppressWarnings(as.numeric(df$GEOGR2)),
    area               = suppressWarnings(as.numeric(df$PLO_STA)),
    altitude           = NA_real_
  )
}



# ---- Timeseries -------------------------------------------------------------

#' @export
timeseries.hydro_service_CZ_CHMI <- function(x,
                                             parameter = c("water_discharge","water_level","water_temperature"),
                                             stations = NULL,
                                             start_date = NULL, end_date = NULL,
                                             mode = c("complete","range"),
                                             exclude_quality = NULL,
                                             ...) {
  parameter <- match.arg(parameter)
  mode      <- match.arg(mode)
  rng       <- resolve_dates(mode, start_date, end_date)

  pm     <- .cz_param_map(parameter)
  st_all <- stations.hydro_service_CZ_CHMI(x)

  # ts_id <-> station_id map
  id_map <- unique(st_all[, c("ts_id", "station_id")])
  id_map <- id_map[!is.na(id_map$ts_id) & nzchar(id_map$ts_id), , drop = FALSE]

  # normalize arbitrary 'stations' input to character vector
  .normalize_station_ids <- function(stations) {
    if (is.null(stations) || length(stations) == 0L) return(character())
    ids <- stations
    if (is.data.frame(ids)) {
      if ("ts_id" %in% names(ids))      ids <- ids$ts_id
      else if ("station_id" %in% names(ids)) ids <- ids$station_id
    } else if (is.list(ids) && !is.atomic(ids)) {
      ids <- unlist(ids, use.names = FALSE)
    }
    ids <- suppressWarnings(as.character(ids))
    ids[!is.na(ids) & nzchar(ids)]
  }

  user_raw <- .normalize_station_ids(stations)

  # Accept either ts_id or station_id from caller; map to ts_id for fetching
  to_ts_ids <- function(v) {
    v <- unique(v)
    ts_hits <- v[v %in% id_map$ts_id]
    st_hits <- v[v %in% id_map$station_id]
    if (length(st_hits)) {
      ts_from_st <- id_map$ts_id[match(st_hits, id_map$station_id)]
      ts_hits <- unique(c(ts_hits, ts_from_st))
    }
    ts_hits[!is.na(ts_hits) & nzchar(ts_hits)]
  }

  user_ts   <- to_ts_ids(user_raw)
  known_ts  <- unique(id_map$ts_id)
  station_vec <- if (length(user_ts) == 0L) known_ts else intersect(known_ts, user_ts)

  if (length(station_vec) == 0L) {
    return(.cz_empty_ts(parameter = parameter, unit = pm$canon))
  }

  batches <- chunk_vec(station_vec, 10)
  pb <- progress::progress_bar$new(total = length(batches))

  # helper: get ts entry robustly (tsList may be flattened or not)
  get_ts_entry <- function(js, target_id) {
    tsl <- js$tsList
    if (is.null(tsl) || length(tsl) == 0L) return(NULL)

    if (is.data.frame(tsl)) {
      if (!"tsConID" %in% names(tsl)) return(NULL)
      row <- tsl[tsl$tsConID == target_id, , drop = FALSE]
      if (nrow(row) == 0L) return(NULL)
      list(
        unit = row$unit[[1]] %||% row$unit[1],
        header = row$tsData$data$header ,
        values = row$tsData$data$values[[1]]
      )
    } else {
      hit <- NULL
      for (k in seq_along(tsl)) if (identical(tsl[[k]]$tsConID, target_id)) { hit <- tsl[[k]]; break }
      if (is.null(hit)) return(NULL)
      list(
        unit   = hit$unit,
        header = hit$tsData$data$header %||% "DT,VAL",
        values = hit$tsData$data$values
      )
    }
  }


  # coerce DT/VAL into a tibble
  coerce_dt_val <- function(vals, hdr) {
    cols <- strsplit(hdr, "\\s*,\\s*")[[1]]
    if (is.matrix(vals) && ncol(vals) >= 2) {
      return(tibble::tibble(DT = vals[,1], VAL = vals[,2]))
    }
    if (is.list(vals) && length(vals) == 1L && is.matrix(vals[[1]]) && ncol(vals[[1]]) >= 2) {
      m <- vals[[1]]; return(tibble::tibble(DT = m[,1], VAL = m[,2]))
    }
    if (is.data.frame(vals)) {
      df <- tibble::as_tibble(vals)
      if (!all(c("DT","VAL") %in% names(df)) && length(cols) == ncol(df)) names(df) <- cols
      if (all(c("DT","VAL") %in% names(df))) return(df[, c("DT","VAL")])
    }
    if (is.list(vals) && length(vals) > 0L) {
      rows <- lapply(vals, function(v) { v <- as.list(v); if (length(v) < 2) return(NULL); list(DT = v[[1]], VAL = v[[2]]) })
      rows <- Filter(Negate(is.null), rows)
      if (length(rows)) return(dplyr::bind_rows(rows))
    }
    tibble::tibble()
  }

  out <- lapply(batches, function(batch) {
    pb$tick()
    one_station <- ratelimitr::limit_rate(function(st_id) {
      y_start <- lubridate::year(as.Date(rng$start_date))
      y_end   <- lubridate::year(as.Date(rng$end_date))
      years   <- seq.int(y_start, y_end)

      fetch_year <- function(yy) {
        path <- sprintf("/hydrology/historical/data/daily/H_%s_DQ_%d.json", st_id, as.integer(yy))
        req  <- build_request(x, path = path, ...)
        resp <- try(perform_request(req), silent = TRUE)
        if (inherits(resp, "try-error")) return(tibble::tibble())
        status <- httr2::resp_status(resp)
        if (status == 404 || status >= 400) return(tibble::tibble())

        js <- httr2::resp_body_json(resp, simplifyVector = TRUE)
        ts_entry <- get_ts_entry(js, pm$tsConID)
        if (is.null(ts_entry)) return(tibble::tibble())

        df <- coerce_dt_val(ts_entry$values, ts_entry$header %||% "DT,VAL")
        if (nrow(df) == 0L) return(tibble::tibble())

        ts_parsed <- suppressWarnings(lubridate::ymd_hms(df$DT, tz = "UTC"))
        val_num   <- suppressWarnings(readr::parse_number(df$VAL))

        keep <- !is.na(ts_parsed) &
          ts_parsed >= as.POSIXct(rng$start_date, tz = "UTC") &
          ts_parsed <= as.POSIXct(rng$end_date,   tz = "UTC") + 86399

        if (!any(keep, na.rm = TRUE)) return(tibble::tibble())

        # ts_id is the one we are iterating (used in URL)
        ts_id <- st_id
        station_id_val <- id_map$station_id[match(ts_id, id_map$ts_id)]

        tibble::tibble(
          country       = x$country,
          provider_id   = x$provider_id,
          provider_name = x$provider_name,
          station_id    = station_id_val,
          ts_id         = ts_id,
          parameter     = parameter,
          timestamp     = ts_parsed[keep],
          value         = pm$to_canon(val_num[keep]),
          unit          = pm$canon,
          quality_code  = NA_character_,
          source_url    = paste0(x$base_url, path)
        )

      }

      dplyr::bind_rows(lapply(years, fetch_year))
    }, rate = ratelimitr::rate(n = x$rate_cfg$n, period = x$rate_cfg$period))

    dplyr::bind_rows(lapply(batch, one_station))
  })

  res <- dplyr::bind_rows(out)

  if (!is.null(exclude_quality) && length(exclude_quality) && nrow(res)) {
    res <- dplyr::filter(res, is.na(.data$quality_code) | !(.data$quality_code %in% exclude_quality))
  }
  if (nrow(res)) res <- dplyr::arrange(res, .data$station_id, .data$timestamp)

  res
}
